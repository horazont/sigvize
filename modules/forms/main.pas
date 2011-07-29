unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, OpenGLContext, GTURIAutoRegister,
  AuALSA,
  AuVorbis, AuAudio, AuTypes, SiglyzeFilter, AuFiltergraph, AuDriverClasses,
  ProcessingOvermind, FFTProcessor, GTNodes, MixProcessor, DataTypeSamples,
  ioConfig, GLShader, GLFramebuffer, GLBase, dglOpenGL, GTVFS, GTVFSXDG,
  GTProtocolVFS;

type

  { TFFTInputTex }

  TFFTInputTex = class(TGLObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FConvertBuffer: array of Single;
  public
    procedure Bind; override;
    procedure SetDataCount(const ADataCount: Integer);
    procedure Unbind; override;
    procedure WriteData(const AData: PDouble);
    procedure WriteSingleData(const AData: PSingle);
  end;

  { TMainForm }

  TMainForm = class(TForm)
    HelpAbout: TAction;
    Actions: TActionList;
    Images: TImageList;
    GL: TOpenGLControl;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

    PrepareShader, WaterfallWriteShader, CopyShader: TGLShader;
    WaterfallShader, SpectrumShader: array [Boolean] of TGLShader;
    FFTInputTex, AvgFFTInputTex: TFFTInputTex;
    PrepareBuffer: TGLFramebuffer;
    PrepareBufferTex: TGLAttachmentRawTexture;
    PrepareBufferTexAvg: TGLAttachmentRawTexture;
    WaterfallBuffer: TGLFramebuffer;
    WaterfallBufferTex: TGLAttachmentRawTexture;
    WaterfallOffsetTex: TGLAttachmentRawTexture;

    SpectrumHeight: Integer;
    AvgSpectrumHeight: Integer;

    FGLBoxX, FGLBoxY: Integer;
    FMouseGLX, FMouseGLY: Integer;
    WaterfallScrollBarBox: TRect;
    WaterfallScrollBarThumb: TRect;
    WaterfallBox: TRect;
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
    procedure InitGL;
    procedure InitGLFramebuffers;
    procedure InitGLShaders;
    procedure InitGLTextures;
    procedure InitSiglyze;
    procedure SetupActualViewport;
    procedure UpdateRenderArea;
  end; 

var
  MainForm: TMainForm;

implementation

uses
  AboutFrm;

{ TFFTInputTex }

constructor TFFTInputTex.Create;
begin
  inherited Create;
  glGenTextures(1, @FGLID);
end;

destructor TFFTInputTex.Destroy;
begin
  glDeleteTextures(1, @FGLID);
  inherited Destroy;
end;

procedure TFFTInputTex.Bind;
begin
  glBindTexture(GL_TEXTURE_1D, FGLID);
end;

procedure TFFTInputTex.SetDataCount(const ADataCount: Integer);
begin
  Bind;
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_LUMINANCE32F_ARB, ADataCount, 0, GL_LUMINANCE, GL_FLOAT, nil);
  SetLength(FConvertBuffer, ADataCount);
  RaiseLastGLError;
end;

procedure TFFTInputTex.Unbind;
begin
  glBindTexture(GL_TEXTURE_1D, 0);
end;

procedure TFFTInputTex.WriteData(const AData: PDouble);
var
  I: Integer;
begin
  for I := 0 to High(FConvertBuffer) do
    FConvertBuffer[I] := AData[I];
  WriteSingleData(@FConvertBuffer[0]);
end;

procedure TFFTInputTex.WriteSingleData(const AData: PSingle);
begin
  Bind;
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, Length(FConvertBuffer), GL_LUMINANCE, GL_FLOAT, AData);
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.HelpAboutExecute(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FInitialized := False;
  FGLInitialized := False;
  Init;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  InitGL;
end;

procedure TMainForm.SiglyzeSetupConnections(Sender: TObject);
var
  I: Integer;
begin
  if FAuSiglyze.InputNode.PortCount > 2 then
  begin
    for I := 0 to FAuSiglyze.InputNode.PortCount - 2 do
      FSlMixerNode.InPort[I].Source := FAuSiglyze.InputNode.Port[I+1];
    FSlFFTNode.InPort[0].Source := FSlMixerNode.Port[0];
  end
  else
  begin
    FSlFFTNode.InPort[0].Source := FAuSiglyze.InputNode.Port[1];
  end;
  FAuSiglyze.SyncNode.InPort[0].Source := FSlFFTNode.Port[0];
end;

procedure TMainForm.SiglyzeSetupNodes(Sender: TObject);
begin
  FSlFFT.FFTSize := Config.FFT.Samples div 2;
  FAuSiglyze.Input.SamplesPerBlock := 2048;
  FSlMixer.InputChannelCount := FAuSiglyze.Input.SourceStream.ChannelCount;
  FSlMixer.OutputChannelType := pcMixed;
  FAuSiglyze.Sync.InputFFTCount := 1;
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
  TGTProtocolVFS.VFS.DumpMounts;
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
  if not FAuAudio.Initialize then
    raise Exception.Create('Could not initialize audio subsystem.');
  FAuAudio.AutoChooseDevice;
  StreamDriver := FAuAudio.Driver.CreateStreamDriver(FAuAudio.StandardDeviceID);

  FAuOutput := TAuDriverOutput.Create(StreamDriver, 16);
  FAuSiglyze := TAuSiglyzeFilter.Create(FSlOvermind);
  FAuPlayer := TAuPlayer.Create(nil, FAuSiglyze);
  FAuSiglyze.Target := FAuOutput;

  FAuSiglyze.OnSiglyzeSetupConnections := @SiglyzeSetupConnections;
  FAuSiglyze.OnSiglyzeSetupNodes := @SiglyzeSetupNodes;

  //FAuPlayer.LoadFromFile('/home/horazont/Music/Johnny Massacre/Johnny Massacre - Ultrasound.mp3');
  //if not FAuPlayer.Open then
  //  raise Exception.Create('Open failed');
  //FAuPlayer.Play;
end;

procedure TMainForm.InitGL;
begin
  if FGLInitialized then
    Exit;
  FGLInitialized := True;

  InitOpenGL;
  GL.MakeCurrent();
  ReadOpenGLCore;
  ReadExtensions;

  SetupActualViewport;
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glClearColor(0.0, 0.0, 0.0, 1.0);

  InitGLTextures;
  InitGLFramebuffers;
  InitGLShaders;

  UpdateRenderArea;
end;

procedure TMainForm.InitGLFramebuffers;
begin
  PrepareBuffer := TGLFramebuffer.Create;
  PrepareBufferTex := TGLAttachmentRawTexture.Create(GL_RGBA16F, Config.FFT.Samples div 2, 1);
  PrepareBufferTex.Autofree := False;
  PrepareBufferTex.ForceBlankRGBA;
  PrepareBufferTexAvg := TGLAttachmentRawTexture.Create(GL_RGBA16F, Config.FFT.Samples div 2, 1);
  PrepareBufferTexAvg.Autofree := False;
  PrepareBufferTexAvg.ForceBlankRGBA;
  PrepareBuffer.ColorAttachment[0] := PrepareBufferTex;
  PrepareBuffer.Bind;
  PrepareBuffer.Unbind;

  WaterfallBuffer := TGLFramebuffer.Create;
  WaterfallBufferTex := TGLAttachmentRawTexture.Create(GL_RGBA16F, 1024, 512);
  WaterfallBufferTex.Autofree := False;
  WaterfallBufferTex.ForceBlankRGBA;
  WaterfallBuffer.ColorAttachment[0] := WaterfallBufferTex;
  WaterfallBuffer.Bind;
  WaterfallBuffer.Unbind;
end;

procedure TMainForm.InitGLShaders;
begin

  PrepareShader := TGLShader.CreateFromURL('vfs:///data/shaders/fft/prepare.vs', 'vfs:///data/shaders/fft/prepare.fs');
  PrepareShader.Bind;
  glUniform1i(PrepareShader.GetUniformLocation('fftData'), 0);
  //glUniform1f(PrepareShader.GetUniformLocation('fftScale'), 1.0);
  //glUniform1f(PrepareShader.GetUniformLocation('startFreq'), 0.0);
  //glUniform1f(PrepareShader.GetUniformLocation('endFreq'), 1.0);
  PrepareShader.Unbind;

  WaterfallShader[False] := TGLShader.CreateFromURL('vfs:///data/shaders/fft/waterfall.vs', 'vfs:///data/shaders/fft/waterfall.fs');
  with WaterfallShader[False] do
  begin
    Bind;
    glUniform1i(GetUniformLocation('fftData'), 0);
    glUniform1f(GetUniformLocation('scale'), -4.0);
    glUniform1f(GetUniformLocation('bias'), 4.0);
    Unbind;
  end;

  WaterfallShader[True] := TGLShader.CreateFromURL('vfs:///data/shaders/fft/waterfall.vs', 'vfs:///data/shaders/fft/waterfall-db.fs');
  with WaterfallShader[True] do
  begin
    Bind;
    glUniform1i(GetUniformLocation('fftData'), 0);
    glUniform1f(GetUniformLocation('scale'), -4.0);
    glUniform1f(GetUniformLocation('bias'), 4.0);
    Unbind;
  end;

  SpectrumShader[False] := TGLShader.CreateFromURL('vfs:///data/shaders/fft/spectrum.vs', 'vfs:///data/shaders/fft/spectrum.fs');
  with SpectrumShader[False] do
  begin
    Bind;
    glUniform1i(GetUniformLocation('fftData'), 0);
    Unbind;
  end;

  SpectrumShader[True] := TGLShader.CreateFromURL('vfs:///data/shaders/fft/spectrum.vs', 'vfs:///data/shaders/fft/spectrum-db.fs');
  with SpectrumShader[True] do
  begin
    Bind;
    glUniform1i(GetUniformLocation('fftData'), 0);
    Unbind;
  end;

  CopyShader := TGLShader.CreateFromURL('vfs:///data/shaders/fft/copy.vs', 'vfs:///data/shaders/fft/copy.fs');
  CopyShader.Bind;
  //glUniform1i(CopyShader.GetUniformLocation('input'), 0);
  CopyShader.Unbind;

  WaterfallWriteShader := TGLShader.CreateFromURL('vfs:///data/shaders/fft/waterfall-write.vs', 'vfs:///data/shaders/fft/waterfall-write.fs');
  WaterfallWriteShader.Bind;
  glUniform1i(WaterfallWriteShader.GetUniformLocation('fftData'), 0);
  WaterfallWriteShader.Unbind;
end;

procedure TMainForm.InitGLTextures;
begin
  FFTInputTex := TFFTInputTex.Create;
  FFTInputTex.SetDataCount(Config.FFT.Samples div 2);
  FFTInputTex.Unbind;

  AvgFFTInputTex := TFFTInputTex.Create;
  AvgFFTInputTex.SetDataCount(Config.FFT.Samples div 2);
  AvgFFTInputTex.Unbind;

  WaterfallOffsetTex := TGLAttachmentRawTexture.Create(GL_LUMINANCE16F_ARB, 1024, 512);
  glGetError;
end;

procedure TMainForm.InitSiglyze;
begin
  FSlOvermind := TProcessingOvermind.Create;
  FSlFFTNode := FSlOvermind.NewNode(TFFTProcessor);
  FSlFFT := FSlFFTNode.ProcessorThread as TFFTProcessor;
  FSlMixerNode := FSlOvermind.NewNode(TMixProcessor);
  FSlMixer := FSlMixerNode.ProcessorThread as TMixProcessor;
end;

procedure TMainForm.SetupActualViewport;
begin
  glViewport(0, 0, GL.Width, GL.Height);
  glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, GL.Width, GL.Height, 0, -10.0, 10.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TMainForm.UpdateRenderArea;
begin
  SpectrumHeight := GL.Height - (512+3+3+17+24+3);
  if SpectrumHeight >= 384 then
  begin
    AvgSpectrumHeight := (SpectrumHeight div 2) - 3;
    SpectrumHeight := (SpectrumHeight - AvgSpectrumHeight) - 3;
  end
  else
    AvgSpectrumHeight := 0;
  WaterfallScrollBarBox := Rect(1051, 1, 1059, 513);
  WaterfallBox := Rect(1, 1, 1025, 513);
end;

end.

