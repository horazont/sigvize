unit AudorraInputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, ProcessingOvermind, DataTypeSamples,
  DataTypeStatus, AuFiltergraph;

type

  { TAudorraInputProcessor }

  TAudorraInputProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FFilter: TAuOutputFilter;

    FSamplesType: TDataTypeSamples;
    FStatusType: TDataTypeStatus;
  protected
    procedure Burn; override;
    procedure Init; override;
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
       const AOutputData: TGTNodeDataSet): Boolean; override;
    procedure SetupIO; override;
  end;

implementation

{ TAudorraInputProcessor }

constructor TAudorraInputProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FSamplesType := TDataTypeSamples.Create;
  FStatusType := TDataTypeStatus.Create;
end;

destructor TAudorraInputProcessor.Destroy;
begin
  FStatusType.Free;
  FSamplesType.Free;
  inherited Destroy;
end;

procedure TAudorraInputProcessor.Burn;
begin
  inherited Burn;
  FStatusType.Burn;
  FSamplesType.Burn;
end;

procedure TAudorraInputProcessor.Init;
begin
  FStatusType.Init;
  FSamplesType.Init;
  inherited Init;
end;

function TAudorraInputProcessor.ProcessDataSet(
  const AInputData: TGTNodeDataSet; const AOutputData: TGTNodeDataSet
  ): Boolean;
begin

end;

procedure TAudorraInputProcessor.SetupIO;
begin

  inherited SetupIO;
end;

end.

