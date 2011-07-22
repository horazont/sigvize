unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, OpenGLContext,
  AuVorbis, AuAudio, AuTypes, SiglyzeFilter, AuFiltergraph, AuDriverClasses,
  ProcessingOvermind, FFTProcessor, GTNodes, MixProcessor, DataTypeSamples;

type

  { TMainForm }

  TMainForm = class(TForm)
    HelpAbout: TAction;
    Actions: TActionList;
    Images: TImageList;
    OpenGLControl1: TOpenGLControl;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    procedure HelpAboutExecute(Sender: TObject);
  private
    FAuAudio: TAuAudio;
    FAuPlayer: TAuPlayer;
    FAuOutput: TAuDriverOutput;
    FAuSiglyze: TAuSiglyzeFilter;

    FSlOvermind: TProcessingOvermind;
    FSlFFT: TFFTProcessor;
    FSlFFTNode: TGTNode;
    FSlMixer: TMixProcessor;
    FSlMixerNode: TGTNode;

    FInitialized: Boolean;
    FGLInitialized: Boolean;
  protected
    procedure SiglyzeSetupConnections(Sender: TObject);
    procedure SiglyzeSetupNodes(Sender: TObject);
  public
    procedure Burn;
    procedure BurnAudorra;
    procedure BurnOpenGL;
    procedure BurnSiglyze;
    procedure Init;
    procedure InitAudorra;
    procedure InitOpenGL;
    procedure InitSiglyze;
  end; 

var
  MainForm: TMainForm;

implementation

uses
  AboutFrm;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.HelpAboutExecute(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.SiglyzeSetupConnections(Sender: TObject);
var
  I: Integer;
begin
  if FAuSiglyze.InputNode.PortCount > 2 then
  begin
    for I := 0 to FAuSiglyze.InputNode.PortCount - 2 do
      FSlMixerNode.InPort[I].Source := FAuSiglyze.InputNode.Port[I-1];
    FSlFFTNode.InPort[0].Source := FSlMixerNode.Port[0];
  end
  else
  begin
    FSlFFTNode.InPort[0].Source := FAuSiglyze.InputNode.Port[1];
  end;
end;

procedure TMainForm.SiglyzeSetupNodes(Sender: TObject);
begin
  FAuSiglyze.Input.SamplesPerBlock := 2048;
  FSlMixer.InputChannelCount := FAuSiglyze.Input.SourceStream.ChannelCount;
  FSlMixer.OutputChannelType := pcMixed;
end;

procedure TMainForm.Burn;
begin
  if FGLInitialized then
  begin
    BurnOpenGL;
  end;
  if FInitialized then
  begin
    BurnAudorra;
    BurnSiglyze;
  end;
end;

procedure TMainForm.BurnAudorra;
begin
  FAuPlayer.Free;
  FAuSiglyze.Free;
  FAuOutput.Free;
  FAuAudio.Free;
end;

procedure TMainForm.BurnOpenGL;
begin

end;

procedure TMainForm.BurnSiglyze;
begin
  FSlOvermind.Free;
end;

procedure TMainForm.Init;
begin
  if not FInitialized then
  begin
    InitSiglyze;
    InitAudorra;
  end;
end;

procedure TMainForm.InitAudorra;
var
  StreamDriver: TAuStreamDriver;
begin
  FAuAudio := TAuAudio.Create;
  FAuAudio.AutoChooseDriver;
  FAuAudio.AutoChooseDevice;
  StreamDriver := FAuAudio.Driver.CreateStreamDriver(FAuAudio.StandardDeviceID);

  FAuOutput := TAuDriverOutput.Create(StreamDriver, 16);
  FAuSiglyze := TAuSiglyzeFilter.Create(FSlOvermind);
  FAuPlayer := TAuPlayer.Create(nil, FAuSiglyze);
  FAuOutput.AddSource(FAuSiglyze);

  FAuSiglyze.OnSiglyzeSetupConnections := @SiglyzeSetupConnections;
  FAuSiglyze.OnSiglyzeSetupNodes := @SiglyzeSetupNodes;
end;

procedure TMainForm.InitOpenGL;
begin

end;

procedure TMainForm.InitSiglyze;
begin
  FSlOvermind := TProcessingOvermind.Create;
  FSlFFTNode := FSlOvermind.NewNode(TFFTProcessor);
  FSlFFT := FSlFFTNode.ProcessorThread as TFFTProcessor;
  FSlMixerNode := FSlOvermind.NewNode(TMixProcessor);
  FSlMixer := FSlMixerNode.ProcessorThread as TMixProcessor;
end;

end.

