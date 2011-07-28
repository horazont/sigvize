unit SiglyzeFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AuFiltergraph, AuDriverClasses, Sources, GTRingBuffer,
  AuTypes, Math, InputProcessor, OutputProcessor, GTNodes, DataTypeStatus,
  ProcessingOvermind, syncobjs, GTStreamUtils, SyncProcessor, GTDebug;

type
  // Read data from source and pass it through a GTRingBuffer to the siglyze
  // thread

  TOnSiglyzeFrameStatus = procedure (Sender: TObject;
    const AStatus: PStatusRecord);
  TOnSiglyzeFrameSamples = procedure (Sender: TObject;
    AIndex: TGTNodePortNumber; const ASamples: PDouble; ACount: SizeUInt);
  TOnSiglyzeFrameFFT = procedure (Sender: TObject;
    AIndex: TGTNodePortNumber; const AFFTData: PDouble; ACount: SizeUInt);

  { TAuFilterStream }

  TAuFilterStream = class (TStream)
  public
    constructor Create(const AFilter: TAuSourceFilter);
    destructor Destroy; override;
  private
    FFilter: TAuSourceFilter;
    FDamper: TGTRingBuffer;
    FDataBuffer: Pointer;
    FDataBufferSize: SizeUInt;
  protected
    procedure ReadData;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  { TAuSiglyzeFilter }

  TAuSiglyzeFilter = class (TAuPassthroughFilter)
  public
    constructor Create(const AOvermind: TProcessingOvermind);
    destructor Destroy; override;
  private
    FOnSiglyzeSetupConnections: TNotifyEvent;
    FOnSiglyzeSetupNodes: TNotifyEvent;
    FSource: TAuFilter;
    FToStream: TStream;
    FFromStream: TGTRingBuffer;
    FSourceStream: TslSourceStream;

    FInterleavedBuffer: Pointer;
    FSiglyzeBuffer: Pointer;

    FInput: TInputProcessor;
    FInputNode: TGTNode;
    FOutput: TOutputProcessor;
    FOutputNode: TGTNode;
    FSync: TSyncProcessor;
    FSyncNode: TGTNode;
    FOvermind: TProcessingOvermind;

    FInternalBuffer: TGTRingBuffer;
  protected
    function DoAddSource(AFilter: TAuFilter): Boolean; override;
    function DoCheckFilter: Boolean; override;
    procedure DoFinalize; override;
    procedure DoFlush; override;
    function DoInit(const AParameters: TAuAudioParameters): Boolean; override;
    function DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
       override;
    procedure DoSiglyzeSetupConnections;
    procedure DoSiglyzeSetupNodes;
    procedure RecieveFromSiglyze;
  public
    property Input: TInputProcessor read FInput;
    property InputNode: TGTNode read FInputNode;
    property OnSiglyzeSetupConnections: TNotifyEvent read FOnSiglyzeSetupConnections write FOnSiglyzeSetupConnections;
    property OnSiglyzeSetupNodes: TNotifyEvent read FOnSiglyzeSetupNodes write FOnSiglyzeSetupNodes;
    property Sync: TSyncProcessor read FSync;
    property SyncNode: TGTNode read FSyncNode;
  end;

implementation

{ TAuFilterStream }

constructor TAuFilterStream.Create(const AFilter: TAuSourceFilter);
begin
  FFilter := AFilter;
  FDamper := TGTRingBuffer.Create(1048576);
  FDataBufferSize := 100 * SizeOf(Single);
  FDataBuffer := GetMem(FDataBufferSize);
end;

destructor TAuFilterStream.Destroy;
begin
  FreeMem(FDataBuffer);
  FDamper.Free;
  inherited Destroy;
end;

procedure TAuFilterStream.ReadData;
var
  ReadCount: Cardinal;
begin
  ReadCount := FFilter.ReadCallback(PSingle(FDataBuffer), FDataBufferSize);
  FDamper.Write(FDataBuffer^, ReadCount);
end;

function TAuFilterStream.Read(var Buffer; Count: Longint): Longint;
begin
  Count -= Count mod SizeOf(Single);
  Result := 0;
  while Result < Count do
  begin
    if FDamper.Available <= FDataBufferSize * 2 then
      ReadData;
    Result += FDamper.Read((Pointer(@Buffer) + Result)^, Min((Count - Result), FDamper.Available));
  end;
end;

{ TAuSiglyzeFilter }

constructor TAuSiglyzeFilter.Create(const AOvermind: TProcessingOvermind);
begin
  inherited Create;
  FOvermind := AOvermind;
  FInputNode := AOvermind.NewNode(TInputProcessor);
  FInput := FInputNode.ProcessorThread as TInputProcessor;
  FOutputNode := AOvermind.NewNode(TOutputProcessor);
  FOutput := FOutputNode.ProcessorThread as TOutputProcessor;
  FSyncNode := AOvermind.NewNode(TSyncProcessor);
  FSync := FSyncNode.ProcessorThread as TSyncProcessor;
  FFromStream := TGTRingBuffer.Create(1048576); // 1MB of buffer, to avoid too many syncs
  FFromStream.Debug := True;
  FOutput.OutStream := FFromStream;
  FInterleavedBuffer := nil;
  Set8087CW($133F);
end;

destructor TAuSiglyzeFilter.Destroy;
begin
  FreeAndNil(FInternalBuffer);
  FInputNode.Free;
  FOutputNode.Free;
  FFromStream.Free;
  inherited Destroy;
end;

function TAuSiglyzeFilter.DoAddSource(AFilter: TAuFilter): Boolean;
begin
  Result := (Sources.Count = 0) and (AFilter is TAuSourceFilter);
end;

function TAuSiglyzeFilter.DoCheckFilter: Boolean;
begin
  Result := Sources.Count = 1;
end;

procedure TAuSiglyzeFilter.DoFinalize;
begin
  FreeAndNil(FSourceStream);
  FreeAndNil(FToStream);
  FreeAndNil(FFromStream);
  inherited DoFinalize;
  FOvermind.Unlock;
end;

procedure TAuSiglyzeFilter.DoFlush;
begin
  inherited DoFlush;
  WriteLn('DoFlush');
end;

function TAuSiglyzeFilter.DoInit(const AParameters: TAuAudioParameters
  ): Boolean;
var
  I: Integer;
begin
  FOvermind.ForceState(osUnlocked);
  inherited DoInit(AParameters);
  FSource := Sources[0];
  FreeAndNil(FSourceStream);
  FreeAndNil(FToStream);
  FToStream := TAuFilterStream.Create(FSource as TAuSourceFilter);
  FSourceStream := TslSourceStream.Create(FToStream, AParameters.Frequency, AParameters.Channels, @TranscodeF32, SizeOf(Single), False, False);

  FInput.SourceStream := FSourceStream;
  FOutput.PCMChannelCount := AParameters.Channels;
  FOutput.DataSet := [deStatus, dePCM];
  FOutput.FFTCount := 0;
  FSync.InPortCount := 1;
  DoSiglyzeSetupNodes;
  FOvermind.Init;
  FOutputNode.InPort[0].Source := FSyncNode.Port[0];
  FSyncNode.InPort[0].Source := FInputNode.Port[0];
  DoSiglyzeSetupConnections;
  for I := 1 to AParameters.Channels do
    FOutputNode.InPort[I].Source := FInputNode.Port[I];
  FInternalBuffer := TGTRingBuffer.Create(FInput.SamplesPerBlock * FInput.SourceStream.ChannelCount * SizeOf(Double) * 2);
  FInternalBuffer.Debug := False;
  FOvermind.Lock;
  Result := True;
end;

function TAuSiglyzeFilter.DoReadCallback(ABuf: PSingle; ASize: Cardinal
  ): Cardinal;
var
  ReadThisTime: Longint;
begin
  try
    WriteLn('Audorra requests data');
    Result := 0;
    while Result < ASize do
    begin
      ReadThisTime := FInternalBuffer.Read(ABuf^, Min(FInternalBuffer.Available, ASize - Result));
      Pointer(ABuf) := Pointer(ABuf) + ReadThisTime;
      Result += ReadThisTime;
      if FInternalBuffer.Available <= 1024 then
        RecieveFromSiglyze;
    end;
    WriteLn('Fulfilled audorras wish for data');
  except
    on E: Exception do
    begin
      WriteLn('AUDORRA CONTROL THREAD IS GOING TO DIE ============================== ');
      WriteLn(E.Message);
      ReadLn;
      raise;
    end;
  end;
end;

procedure TAuSiglyzeFilter.DoSiglyzeSetupConnections;
begin
  if FOnSiglyzeSetupConnections <> nil then
    FOnSiglyzeSetupConnections(Self);
end;

procedure TAuSiglyzeFilter.DoSiglyzeSetupNodes;
begin
  if FOnSiglyzeSetupNodes <> nil then
    FOnSiglyzeSetupNodes(Self);
end;

procedure TAuSiglyzeFilter.RecieveFromSiglyze;
var
  Header: TSiglyzeFrameHeader;
  I, J: Integer;
  Source: PDouble;
  TargetPtr: PSingle;
  CCount: Integer;
  SCount: Cardinal;
  FrameSize: SizeUInt;
begin
  DebugMsg('Receive from siglyze (%d available in FFromStream (size=%d), (%d/%d) available/free in FInternalBuffer)', [FFromStream.Available, FFromStream.Size, FInternalBuffer.Available, FInternalBuffer.Size - FInternalBuffer.Available]);
  FrameSize := SizeOf(Single) * FInput.SamplesPerBlock * FInput.SourceStream.ChannelCount;

  DebugMsg('Receiving header', []);
  CheckRead(FFromStream, Header, SizeOf(TSiglyzeFrameHeader));
  CCount := Parameters.Channels;
  ReAllocMem(FInterleavedBuffer, FrameSize);
  for I := 0 to Header.PCMChannelCount - 1 do
  begin
    DebugMsg('Receiving channel %d', [I]);
    CheckRead(FFromStream, SCount, SizeOf(Cardinal));
    DebugMsg('Channel %d: Received sample count (%d)', [I, SCount]);
    ReAllocMem(FSiglyzeBuffer, SCount * SizeOf(Double));
    DebugMsg('Channel %d: Got memory', [I]);
    CheckRead(FFromStream, FSiglyzeBuffer^, SCount * SizeOf(Double));
    DebugMsg('Channel %d: Received samples', [I]);

    TargetPtr := PSingle(FInterleavedBuffer);
    Source := PDouble(FSiglyzeBuffer);

    DebugMsg('Channel %d: Converting samples', [I]);
    Inc(TargetPtr, I);
    for J := 0 to SCount - 1 do
    begin
      TargetPtr^ := Source^;
      Inc(Source);
      Inc(TargetPtr, CCount);
    end;
    DebugMsg('Channel %d received', [I]);
  end;
  DebugMsg('Attempt to write %d bytes into internal buffer (%d free)', [FrameSize, FInternalBuffer.Size - FInternalBuffer.Available]);
  FInternalBuffer.Write(FInterleavedBuffer^, FrameSize);
end;

end.

