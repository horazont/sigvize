unit SiglyzeFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AuFiltergraph, AuDriverClasses, Sources, GTRingBuffer,
  AuTypes, Math, InputProcessor, OutputProcessor, GTNodes, DataTypeStatus,
  ProcessingOvermind, syncobjs, GTStreamUtils, SyncProcessor;

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
  private
    FFilter: TAuSourceFilter;
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
    FFromStream: TStream;
    FSourceStream: TslSourceStream;

    FBuffer: Pointer;
    FBufferSize: SizeUInt;

    FInput: TInputProcessor;
    FInputNode: TGTNode;
    FOutput: TOutputProcessor;
    FOutputNode: TGTNode;
    FSync: TSyncProcessor;
    FSyncNode: TGTNode;
    FOvermind: TProcessingOvermind;
  protected
    function DoAddSource(AFilter: TAuFilter): Boolean; override;
    function DoCheckFilter: Boolean; override;
    procedure DoFinalize; override;
    function DoInit(const AParameters: TAuAudioParameters): Boolean; override;
    function DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
       override;
    procedure DoSiglyzeSetupConnections;
    procedure DoSiglyzeSetupNodes;
  public
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
end;

function TAuFilterStream.Read(var Buffer; Count: Longint): Longint;
begin
  Count -= Count mod SizeOf(Single);
  Result := FFilter.ReadCallback(PSingle(@Buffer), Count);
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
  FOutput.OutStream := FFromStream;
  FBufferSize := 0;
  FBuffer := nil;
end;

destructor TAuSiglyzeFilter.Destroy;
begin
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

function TAuSiglyzeFilter.DoInit(const AParameters: TAuAudioParameters
  ): Boolean;
var
  BufferSize: SizeUInt;
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
  FOutput.FFTCount := 0;
  FSync.InPortCount := 1;
  DoSiglyzeSetupNodes;
  FOvermind.Init;
  FOutputNode.InPort[0].Source := FSyncNode.Port[0];
  FSyncNode.InPort[0].Source := FInputNode.Port[0];
  DoSiglyzeSetupConnections;
  for I := 1 to AParameters.Channels do
    FOutputNode.InPort[I].Source := FInputNode.Port[I];
  FOvermind.Lock;
end;

function TAuSiglyzeFilter.DoReadCallback(ABuf: PSingle; ASize: Cardinal
  ): Cardinal;
var
  Header: TSiglyzeFrameHeader;
  I, J: Integer;
  Source: PDouble;
  TargetPtr: PSingle;
  CCount: Integer;
  SCount: Cardinal;
begin
  CheckRead(FFromStream, Header, SizeOf(TSiglyzeFrameHeader));
  CCount := Parameters.Channels;
  TargetPtr := ABuf;
  for I := 0 to Header.PCMChannelCount - 1 do
  begin
    CheckRead(FFromStream, SCount, SizeOf(Cardinal));
    if SCount * SizeOf(Double) > FBufferSize then
    begin
      FBufferSize := SCount * SizeOf(Double);
      ReAllocMem(FBuffer, FBufferSize);
    end;
    CheckRead(FFromStream, FBuffer^, SCount * SizeOf(Double));
    TargetPtr := ABuf;
    Source := PDouble(FBuffer);
    Inc(TargetPtr, I);
    for J := 0 to SCount - 1 do
    begin
      TargetPtr^ := Source^;
      Inc(Source);
      Inc(TargetPtr, CCount);
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

end.

