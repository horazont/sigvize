unit SiglyzeFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AuFiltergraph, AuDriverClasses, Sources, GTRingBuffer,
  AuTypes, Math, InputProcessor, GTNodes, DataTypeStatus,
  ProcessingOvermind, syncobjs, GTStreamUtils, POTSyncProcessor, GTDebug;

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
    FChannelStreams: array of TGTSubchanneledRingbuffer;
    FSourceStream: TslSourceStream;

    FInterleavedBuffer: Pointer;
    FInterleavedBufferSize: SizeUInt;
    FChannelBuffer: Pointer;
    FChannelBufferSize: SizeUInt;

    FInput: TInputProcessor;
    FInputNode: TGTNode;
    FSync: TPOTSyncProcessor;
    FSyncNode: TGTNode;
    FOvermind: TProcessingOvermind;

    FMaxBlockSize: SizeUInt;

    FInternalBuffer: TGTRingBuffer;
  protected
    function DetectAvailableByteCount: SizeUInt;
    function DoAddSource(AFilter: TAuFilter): Boolean; override;
    function DoCheckFilter: Boolean; override;
    procedure DoFinalize; override;
    procedure DoFlush; override;
    function DoInit(const AParameters: TAuAudioParameters): Boolean; override;
    function DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
       override;
    procedure DoSiglyzeSetupConnections;
    procedure DoSiglyzeSetupNodes;
    procedure EnforceBuffers(const ASampleCount: SizeUInt; const Persistent: Boolean = False);
    procedure EnforceChannelBuffer(const ASize: SizeUInt; const Persistent: Boolean = False);
    procedure EnforceInterleavedBuffer(const ASize: SizeUInt; const Persistent: Boolean = False);
    procedure RecieveFromSiglyze;
  public
    property Input: TInputProcessor read FInput;
    property InputNode: TGTNode read FInputNode;
    property OnSiglyzeSetupConnections: TNotifyEvent read FOnSiglyzeSetupConnections write FOnSiglyzeSetupConnections;
    property OnSiglyzeSetupNodes: TNotifyEvent read FOnSiglyzeSetupNodes write FOnSiglyzeSetupNodes;
    property Sync: TPOTSyncProcessor read FSync;
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
  FSyncNode := AOvermind.NewNode(TPOTSyncProcessor);
  FSync := FSyncNode.ProcessorThread as TPOTSyncProcessor;
  FInterleavedBuffer := nil;
  FInterleavedBufferSize := 0;
  FChannelBuffer := nil;
  FChannelBufferSize := 0;
  Set8087CW($133F);
end;

destructor TAuSiglyzeFilter.Destroy;
begin
  FreeAndNil(FInternalBuffer);
  FreeAndNil(FChannelBuffer);
  FInputNode.Free;
  inherited Destroy;
end;

function TAuSiglyzeFilter.DetectAvailableByteCount: SizeUInt;
var
  I: Integer;
  CurrAvailable: SizeUInt;
begin
  Result := $FFFFFFFFFFFFFFF;
  for I := 0 to High(FChannelStreams) do
  begin
    CurrAvailable := FChannelStreams[I].Available;
    if CurrAvailable < Result then
      Result := CurrAvailable;
  end;
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
var
  I: Integer;
begin
  for I := 0 to High(FChannelStreams) do
    FChannelStreams[I].Free;
  FreeAndNil(FSourceStream);
  FreeAndNil(FToStream);
  inherited DoFinalize;
  FOvermind.Unlock;
end;

procedure TAuSiglyzeFilter.DoFlush;
begin
  inherited DoFlush;
end;

function TAuSiglyzeFilter.DoInit(const AParameters: TAuAudioParameters
  ): Boolean;
var
  I, SI: Integer;
begin
  FOvermind.ForceState(osUnlocked);
  inherited DoInit(AParameters);
  FSource := Sources[0];
  FreeAndNil(FSourceStream);
  FreeAndNil(FToStream);
  FToStream := TAuFilterStream.Create(FSource as TAuSourceFilter);
  FSourceStream := TslSourceStream.Create(FToStream, AParameters.Frequency, AParameters.Channels, @TranscodeF32, SizeOf(Single), False, False);

  SetLength(FChannelStreams, FSourceStream.ChannelCount);
  for I := 0 to High(FChannelStreams) do
    FChannelStreams[I] := TGTSubchanneledRingbuffer.Create(FSourceStream.SampleRate * SizeOf(Double));
  FChannelStreams[0].Debug := False;

  FInput.SourceStream := FSourceStream;
  FInput.SamplesPerBlock := 2048;
  FSync.InputFFTCount := 0;
  FSync.InputSamplesCount := FSourceStream.ChannelCount;
  DoSiglyzeSetupNodes;
  FOvermind.Init;
  for I := 0 to FSourceStream.ChannelCount - 1 do
  begin
    SI := I + FSync.InputFFTCount;
    FSyncNode.InPort[SI].Source := FInputNode.Port[I+1];
    FSyncNode.Port[SI].OutPipe.Targets.Add(FChannelStreams[I]);
  end;
  DoSiglyzeSetupConnections;
  FMaxBlockSize := 2048 * SizeOf(Double);
  FInternalBuffer := TGTRingBuffer.Create(FSourceStream.SampleRate * FSourceStream.ChannelCount * SizeOf(Single) * 2); // 2s of buffer
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
      if FInternalBuffer.Available <= FMaxBlockSize then
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

procedure TAuSiglyzeFilter.EnforceChannelBuffer(const ASize: SizeUInt;
  const Persistent: Boolean);
begin
  if FChannelBufferSize > ASize then
    Exit;
  if Persistent then
  begin
    ReAllocMem(FChannelBuffer, FChannelBufferSize);
  end
  else
  begin
    FreeMem(FChannelBuffer);
    FChannelBuffer := GetMem(ASize);
  end;
  FChannelBufferSize := ASize;
end;

procedure TAuSiglyzeFilter.EnforceBuffers(const ASampleCount: SizeUInt;
  const Persistent: Boolean);
var
  InterleavedSize: SizeUInt;
  ChannelSize: SizeUInt;
begin
  ChannelSize := ASampleCount * SizeOf(Double);
  InterleavedSize := ChannelSize * FSourceStream.ChannelCount;
  EnforceInterleavedBuffer(InterleavedSize, Persistent);
  EnforceChannelBuffer(ChannelSize, Persistent);
end;

procedure TAuSiglyzeFilter.EnforceInterleavedBuffer(const ASize: SizeUInt;
  const Persistent: Boolean);
begin
  if FInterleavedBufferSize > ASize then
    Exit;
  if Persistent then
  begin
    ReAllocMem(FInterleavedBuffer, FInterleavedBufferSize);
  end
  else
  begin
    FreeMem(FInterleavedBuffer);
    FInterleavedBuffer := GetMem(ASize);
  end;
  FInterleavedBufferSize := ASize;
end;

procedure TAuSiglyzeFilter.RecieveFromSiglyze;
var
  I, J: Integer;
  SampleCount, Count: SizeUInt;
  TargetPtr: PSingle;
  Source: PDouble;
begin
  Count := DetectAvailableByteCount;
  if Count > FMaxBlockSize then
    Count := FMaxBlockSize;
  if Count = 0 then
  begin
    ThreadSwitch;
    Exit;
  end;
  SampleCount := Count div SizeOf(Double);
  Count := SampleCount * SizeOf(Double); // round to full samples
  EnforceBuffers(SampleCount);
  for I := 0 to High(FChannelStreams) do
  begin
    DebugMsg('Channel %d: Reading %d samples (%d bytes)', [I, SampleCount, Count], Self);
    CheckRead(FChannelStreams[I], FChannelBuffer^, Count);

    TargetPtr := PSingle(FInterleavedBuffer);
    Source := PDouble(FChannelBuffer);

    DebugMsg('Channel %d: Converting samples (%d bytes)', [I, SampleCount * SizeOf(Double)], Self);
    Inc(TargetPtr, I);
    for J := 0 to SampleCount - 1 do
    begin
      TargetPtr^ := Source^;
      Inc(Source);
      Inc(TargetPtr, Length(FChannelStreams));
    end;
    DebugMsg('Channel %d received', [I], Self);
  end;
  DebugMsg('Attempt to write %d bytes into internal buffer (%d free)', [Count * Length(FChannelStreams), FInternalBuffer.Size - FInternalBuffer.Available], Self);
  FInternalBuffer.Write(FInterleavedBuffer^, SampleCount * SizeOf(Single) * Length(FChannelStreams));
end;

end.

