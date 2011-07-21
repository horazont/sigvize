unit ioConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTConfig, GTXDG, GTBase;

type

  { TAuPaths }

  TAuPaths = class (TGTXDGPaths)
  public
    class function GetAppName: String; override;
  end;

  TDoubleRange = class (TGTBaseObject)
  protected
    FMin, FMax: Double;
  published
    property Min: Double read FMin write FMin;
    property Max: Double read FMax write FMax;
  end;

  { TFFTConfig }

  TFFTConfig = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FSamples: Word;
  published
    property Samples: Word read FSamples write FSamples default 2048;
  end;

  { TPlaybackConfig }

  TPlaybackConfig = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FMuted: Boolean;
    FVolume: Word;
  published
    property Muted: Boolean read FMuted write FMuted default False;
    property Volume: Word read FVolume write FVolume default (65535 div 2);
  end;

  { TSpectrumConfig }

  TSpectrumConfig = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FAutoScale: Boolean;
    FScale: Double;
    FUsedBScale: Boolean;
    FdbRange: TDoubleRange;
  published
    property dbRange: TDoubleRange read FdbRange;
    property AutoScale: Boolean read FAutoScale write FAutoScale;
    property Scale: Double read FScale write FScale;
    property UsedBScale: Boolean read FUsedBScale write FUsedBScale;
  end;

  { TPeakAnalysisConfig }

  TPeakAnalysisConfig = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FLowLevelClipDistance: Double;
    FPeakThreshold: Double;
  published
    property LowLevelClipDistance: Double read FLowLevelClipDistance write FLowLevelClipDistance;
    property PeakThreshold: Double read FPeakThreshold write FPeakThreshold;
  end;

  { TAnalysisConfig }

  TAnalysisConfig = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FPeak: TPeakAnalysisConfig;
    FFreq: TDoubleRange;
  published
    property Peak: TPeakAnalysisConfig read FPeak;
    property Freq: TDoubleRange read FFreq;
  end;

  { TSigAnalyze }

  TSigAnalyze = class (TGTConfig)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FAnalysis: TAnalysisConfig;
    FFFT: TFFTConfig;
    FHistoryList: TStringList;
    FPlayback: TPlaybackConfig;
    FSpectrum: TSpectrumConfig;
  private
    function GetHistory: String;
    procedure SetHistory(const AValue: String);
  protected
    function DefaultNodeName: String; override;
  public
    property HistoryList: TStringList read FHistoryList;
  published
    property Analysis: TAnalysisConfig read FAnalysis;
    property FFT: TFFTConfig read FFFT;
    property History: String read GetHistory write SetHistory;
    property Playback: TPlaybackConfig read FPlayback;
    property Spectrum: TSpectrumConfig read FSpectrum;
  end;

var
  Config: TSigAnalyze;

implementation

{ TFFTConfig }

constructor TFFTConfig.Create;
begin
  inherited Create;
  FSamples := 2048;
end;

{ TPlaybackConfig }

constructor TPlaybackConfig.Create;
begin
  inherited Create;
  FMuted := False;
  FVolume := 65535 div 2;
end;

{ TSpectrumConfig }

constructor TSpectrumConfig.Create;
begin
  inherited Create;
  FScale := 1.0;
  FAutoScale := False;
  FUsedBScale := False;
  FdbRange := TDoubleRange.Create;
  FdbRange.Max := 0;
  FdbRange.Min := -60;
end;

destructor TSpectrumConfig.Destroy;
begin
  FdbRange.Free;
  inherited Destroy;
end;

{ TPeakAnalysisConfig }

constructor TPeakAnalysisConfig.Create;
begin
  inherited Create;
  FLowLevelClipDistance := 20.0;
  FPeakThreshold := 0.5;
end;

{ TAnalysisConfig }

constructor TAnalysisConfig.Create;
begin
  inherited Create;
  FPeak := TPeakAnalysisConfig.Create;
  FFreq := TDoubleRange.Create;
  FFreq.Min := 0.0;
  FFreq.Max := 22050;
end;

destructor TAnalysisConfig.Destroy;
begin
  FFreq.Free;
  FPeak.Free;
  inherited Destroy;
end;

{ TSigAnalyze }

constructor TSigAnalyze.Create;
begin
  inherited Create;
  FAnalysis := TAnalysisConfig.Create;
  FFFT := TFFTConfig.Create;
  FPlayback := TPlaybackConfig.Create;
  FHistoryList := TStringList.Create;
  FSpectrum := TSpectrumConfig.Create;
end;

destructor TSigAnalyze.Destroy;
begin
  FSpectrum.Free;
  FHistoryList.Free;
  FPlayback.Free;
  FFFT.Free;
  FAnalysis.Free;
  inherited Destroy;
end;

function TSigAnalyze.GetHistory: String;
begin
  Result := FHistoryList.Text;
end;

procedure TSigAnalyze.SetHistory(const AValue: String);
begin
  FHistoryList.Text := AValue;
end;

function TSigAnalyze.DefaultNodeName: String;
begin
  Result := 'siganalyze';
end;

{ TAuPaths }

class function TAuPaths.GetAppName: String;
begin
  Result := 'siganalyze';
end;

initialization
XDG := TAuPaths;
Config := TSigAnalyze.Create;
Config.Load;

finalization
Config.Save;
Config.Free;

end.

