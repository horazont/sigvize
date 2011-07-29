unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, ExtCtrls, OpenGLContext, GTURIAutoRegister,
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
    RenderTimer: TTimer;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
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

    FMaxFrequency: Cardinal;

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
    procedure glBox(const X, Y, W, H: Integer);
    procedure glClearBox;
    procedure Init;
    procedure InitAudorra;
    procedure InitGL;
    procedure InitGLFramebuffers;
    procedure InitGLShaders;
    procedure InitGLTextures;
    procedure InitSiglyze;
    procedure PerFrameData;
    procedure Render;
    procedure RenderAvgSpectrum;
    procedure RenderFrequencyMeter;
    procedure RenderSpectrum;
    procedure RenderWaterfall;
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

procedure TMainForm.RenderTimerTimer(Sender: TObject);
begin
  PerFrameData;
  Render;
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
  FMaxFrequency := FAuSiglyze.Input.SourceStream.SampleRate div 2;
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

procedure TMainForm.glBox(const X, Y, W, H: Integer);
begin
  glTranslatef(X, Y, 0.0);
  FGLBoxX := X;
  FGLBoxY := Y;
  FMouseGLX -= X;
  FMouseGLY -= Y;
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
    glVertex2f(-1, -1);
    glVertex2f(-1, H+1);
    glVertex2f(W+1, H+1);
    glVertex2f(W+1, -1);
  glEnd;
  glEnable(GL_SCISSOR_TEST);
  glScissor(X, GL.Height - (Y + H), W, H);
  glColor3f(0, 0, 0);
  glBegin(GL_QUADS);
    glVertex2f(0, 0);
    glVertex2f(0, H);
    glVertex2f(W, H);
    glVertex2f(W, 0);
  glEnd;
end;

procedure TMainForm.glClearBox;
begin
  FMouseGLX += FGLBoxX;
  FMouseGLY += FGLBoxY;
  glDisable(GL_SCISSOR_TEST);
  glLoadIdentity;
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

procedure TMainForm.PerFrameData;
begin

end;

procedure TMainForm.Render;
begin
  glClear(GL_COLOR_BUFFER_BIT);

  glBox(1, 1, 1058, 512);
  RenderWaterfall;
  glClearBox;

  glBox(1, 516, 1024, 24);
  RenderFrequencyMeter;
  glClearBox;

  glBox(1, 543, 1024, SpectrumHeight+17);
  RenderSpectrum;
  glClearBox;

  if AvgSpectrumHeight > 0 then
  begin
    glBox(1, 543+SpectrumHeight+20, 1024, AvgSpectrumHeight);
    RenderAvgSpectrum;
    glClearBox;
  end;

  GL.SwapBuffers;
  RaiseLastGLError;
end;

procedure TMainForm.RenderAvgSpectrum;
var
  Scale: Double;
  CurrentShader: TGLShader;
begin
  {if Config.Spectrum.AutoScale then
  begin
    if Config.Spectrum.UsedBScale then
      Scale := Analyzer.ClampDecibel(Analyzer.PeakDetector.Maximum)
    else
      Scale := DecibelToValue(Analyzer.PeakDetector.Maximum);
    if Scale <> 0.0 then
      Scale := 1/Scale
    else
      Scale := 1.0;
  end
  else}
    Scale := Config.Spectrum.Scale;

  CurrentShader := SpectrumShader[Config.Spectrum.UsedBScale];
  CurrentShader.Bind;
  glUniform1f(CurrentShader.GetUniformLocation('fftScale'), Scale);
  PrepareBufferTexAvg.Bind;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1.0);
    glVertex2f(0, 0);
    glTexCoord2f(1, 1.0);
    glVertex2f(1023, 0);
    glTexCoord2f(1, 0);
    glVertex2f(1023, AvgSpectrumHeight);
    glTexCoord2f(0, 0);
    glVertex2f(0, AvgSpectrumHeight);
  glEnd;
  PrepareBufferTexAvg.Unbind;
  CurrentShader.Unbind;
end;

procedure TMainForm.RenderFrequencyMeter;
var
  I, Count: Integer;
  X, Step: Single;
  H: Single;
  LargeTicks: Integer;
  MedTicks: Integer;
begin
  if FMaxFrequency > 50000 then
  begin
    Count := FMaxFrequency div 1000;
    Step := (1000 / FMaxFrequency) * 1024;
    LargeTicks := 10;
    MedTicks := 5;
  end
  else if FMaxFrequency > 10000 then
  begin
    Count := FMaxFrequency div 500;
    Step := (500 / FMaxFrequency) * 1024;
    LargeTicks := 20;
    MedTicks := 10;
  end
  else if FMaxFrequency > 5000 then
  begin
    Count := FMaxFrequency div 100;
    Step := (100 / FMaxFrequency) * 1024;
    LargeTicks := 10;
    MedTicks := 5;
  end
  else
  begin
    Count := FMaxFrequency div 50;
    Step := (50 / FMaxFrequency) * 1024;
    LargeTicks := 20;
    MedTicks := 10;
  end;

  X := 0;
  glColor4f(1.0, 1.0, 1.0, 1.0);
  glBegin(GL_LINES);
    for I := 0 to Count - 1 do
    begin
      if I mod LargeTicks = 0 then
        H := 6.0
      else if I mod MedTicks = 0 then
        H := 5.0
      else
        H := 4.0;
      glVertex2f(X, 0.0);
      glVertex2f(X, H);

      glVertex2f(X, 24.0-H);
      glVertex2f(X, 24.0);
      X += Step;
    end;
  glEnd;

  glTranslatef(0, 16, 0);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_QUADS);
    glColor4f(0.0, 1.0, 0.0, 1.0);
    glVertex2f(0.0, -16.0);
    glVertex2f(0.0, 0.0);
    glVertex2f(100.0, 0.0);
    glVertex2f(100.0, -16.0);
  glEnd;
  //glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  {tsTextColor4f(1.0, 1.0, 1.0, 1.0);
  glColor4f(1.0, 1.0, 1.0, 1.0);
  RaiseLastTSError;
  tsFontBind(FTSFont);
  RaiseLastTSError;
  tsTextOutA('Hello World');
  RaiseLastTSError;       }
  glTranslatef(0, -16, 0);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TMainForm.RenderSpectrum;
const
  Y = 8;
var
  AvgY, MaxY, PSY, ValidY, Scale: Double;
//  PeakData: TExtendedFFTData;
  I: Integer;
  X, XStep: Double;
  CurrentShader: TGLShader;
  SpectrumBoxHeight: Integer;
begin
  SpectrumBoxHeight := SpectrumHeight+17;
  {if Config.Spectrum.AutoScale then
  begin
    if Config.Spectrum.UsedBScale then
      Scale := Analyzer.ClampDecibel(Analyzer.PeakDetector.Maximum)
    else
      Scale := DecibelToValue(Analyzer.PeakDetector.Maximum);
    if Scale <> 0.0 then
      Scale := 1/Scale
    else
      Scale := 1.0;
  end
  else
    }Scale := Config.Spectrum.Scale;

  CurrentShader := SpectrumShader[Config.Spectrum.UsedBScale];
  CurrentShader.Bind;
  {if Config.Spectrum.UsedBScale then
  begin
    AvgY := Analyzer.ClampDecibel(Analyzer.PeakDetector.Average);
    MaxY := Analyzer.ClampDecibel(Analyzer.PeakDetector.Maximum);
    PSY := Analyzer.ClampDecibel(Analyzer.PeakDetector.PeakThreshold);
    ValidY := Analyzer.ClampDecibel(Analyzer.PeakDetector.ValidThreshold);
  end
  else
  begin
    AvgY := DecibelToValue(Analyzer.PeakDetector.Average);
    MaxY := DecibelToValue(Analyzer.PeakDetector.Maximum);
    PSY := DecibelToValue(Analyzer.PeakDetector.PeakThreshold);
    ValidY := DecibelToValue(Analyzer.PeakDetector.ValidThreshold);
  end;}
  AvgY := 0;
  MaxY := 0;
  PSY := 0;
  ValidY := 0;
  glUniform1f(CurrentShader.GetUniformLocation('fftScale'), Scale);
  PrepareBufferTex.Bind;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1.0);
    glVertex2f(0, 17);
    glTexCoord2f(1, 1.0);
    glVertex2f(1023, 17);
    glTexCoord2f(1, 0);
    glVertex2f(1023, SpectrumBoxHeight);
    glTexCoord2f(0, 0);
    glVertex2f(0, SpectrumBoxHeight);
  glEnd;
  FFTInputTex.Unbind;
  CurrentShader.Unbind;

  AvgY := SpectrumBoxHeight - AvgY * Scale * SpectrumHeight;
  MaxY := SpectrumBoxHeight - MaxY * Scale * SpectrumHeight;
  PSY := SpectrumBoxHeight - PSY * Scale * SpectrumHeight;
  ValidY := SpectrumBoxHeight - ValidY * Scale * SpectrumHeight;
  glBegin(GL_LINES);
    glColor4f(0.5, 1.0, 0.5, 1.0);
    glVertex2f(0, AvgY);
    glVertex2f(1024, AvgY);

    glColor4f(1.0, 0.5, 0.5, 1.0);
    glVertex2f(0, MaxY);
    glVertex2f(1024, MaxY);

    glColor4f(0.5, 0.5, 1.0, 1.0);
    glVertex2f(0, PSY);
    glVertex2f(1024, PSY);

    glColor4f(1.0, 1.0, 0.5, 1.0);
    glVertex2f(0, ValidY);
    glVertex2f(1024, ValidY);
  glEnd;

  {glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4f(0.75, 0.75, 0.75, 0.25);
  glBegin(GL_QUADS);
    glVertex2f(0, SpectrumBoxHeight);
    glVertex2f(0, 17);
    glVertex2f(Analyzer.DataMin / (Analyzer.InputSize-1) * 1024, 17);
    glVertex2f(Analyzer.DataMin / (Analyzer.InputSize-1) * 1024, SpectrumBoxHeight);

    glVertex2f(1024, SpectrumBoxHeight);
    glVertex2f(1024, 17);
    glVertex2f(Analyzer.DataMax / (Analyzer.InputSize-1) * 1024, 17);
    glVertex2f(Analyzer.DataMax / (Analyzer.InputSize-1) * 1024, SpectrumBoxHeight);
  glEnd;
  glDisable(GL_BLEND);}

  {PeakData := Analyzer.GetFullData;
  if not Analyzer.PeakDetector.NoisedOut then
  begin
    X := 0;
    XStep := 1024 / Length(PeakData);
    glBegin(GL_QUADS);
      for I := 0 to High(PeakData) do with PeakData[I].Peak do
      begin
        if IsPeak or IsPersistentPeak then
        begin
          if IsPeak then
            glColor4f(1.0, 1.0, 1.0, 1.0)
          else
            glColor4f(1.0, 1.0, 0.0, 1.0);
          glVertex2f(X-5.0, Y);
          glVertex2f(X, Y+5.0);
          glVertex2f(X+5.0, Y);
          glVertex2f(X, Y-5.0);
          if IsPersistentPeak then
            glColor4f(0.0, 1.0, 0.0, 1.0)
          else
            glColor4f(0.0, 0.0, 0.0, 1.0);
          glVertex2f(X-4.0, Y);
          glVertex2f(X, Y+4.0);
          glVertex2f(X+4.0, Y);
          glVertex2f(X, Y-4.0);
        end;
        X += XStep;
      end;
    glEnd;
  end
  else
  begin
    glColor4f(0.5, 0.5, 0.5, 1.0);
    glBegin(GL_TRIANGLE_STRIP);
      glVertex2f(0.0, Y);
      glVertex2f(4.0, Y-4.0);
      glVertex2f(4.0, Y+4.0);
      glVertex2f(1020, Y-4.0);
      glVertex2f(1020, Y+4.0);
      glVertex2f(1024, Y);
    glEnd;
  end;}
end;

procedure TMainForm.RenderWaterfall;
var
  CurrentShader: TGLShader;
  ViewPosY1, ViewPosY2: Single;
begin
  CurrentShader := WaterfallShader[Config.Spectrum.UsedBScale];
  CurrentShader.Bind;
  glUniform1f(CurrentShader.GetUniformLocation('fftScale'), Config.Spectrum.Scale);
  WaterfallBufferTex.Bind;
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0.0, 0.0);
    glTexCoord2f(0.0, 1.0);
    glVertex2f(0.0, 512.0);
    glTexCoord2f(1.0, 1.0);
    glVertex2f(1024.0, 512.0);
    glTexCoord2f(1.0, 0.0);
    glVertex2f(1024.0, 0.0);
  glEnd;
  WaterfallBufferTex.Unbind;
  CurrentShader.Unbind;

  {glTranslatef(1050, 0, 0);
  ViewPosY1 := 512-(FWaterfallBuffer.ReadPos/FWaterfallBuffer.Size)*512;
  ViewPosY2 := ViewPosY1-(512/FWaterfallBuffer.Size)*512;
  glBegin(GL_QUADS);
    glColor4f(1, 1, 1, 1);
    glVertex2f(0, -1);
    glVertex2f(0, 513);
    glVertex2f(10, 513);
    glVertex2f(10, -1);

    glColor4f(0, 0, 0, 1);
    glVertex2f(1, 0);
    glVertex2f(1, 512);
    glVertex2f(9, 512);
    glVertex2f(9, 0);

    glColor4f(0.25, 0.25, 0.25, 1.0);
    glVertex2f(1, ViewPosY2);
    glVertex2f(1, ViewPosY1);
    glVertex2f(9, ViewPosY1);
    glVertex2f(9, ViewPosY2);
  glEnd;
  glTranslatef(-1050, 0, 0);

  with WaterfallScrollBarBox do
  begin
    WaterfallScrollBarThumb := Rect(Left, Top + Round(ViewPosY2), Right, Bottom + Round(ViewPosY1));
  end;}
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

