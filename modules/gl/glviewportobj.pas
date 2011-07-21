unit GLViewportObj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, math;

type

  { TGLViewport }

  TGLViewport = class (TObject)
  public
    constructor Create;
  private
    FBottom: Integer;
    FLeft: Integer;
    FOnChange: TNotifyEvent;
    FRight: Integer;
    FTop: Integer;
    procedure SetBottom(const AValue: Integer);
    procedure SetLeft(const AValue: Integer);
    procedure SetRight(const AValue: Integer);
    procedure SetTop(const AValue: Integer);
  protected
    procedure DoChange;
  public
    procedure Assign(Source: TGLViewport);
    procedure SetAll(const ATop, ALeft, ABottom, ARight: Integer);
  public
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Right: Integer read FRight write SetRight;
    property Bottom: Integer read FBottom write SetBottom;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TGLViewport }

constructor TGLViewport.Create;
begin
  FOnChange := nil;
end;

procedure TGLViewport.SetBottom(const AValue: Integer);
begin
  if FBottom=AValue then exit;
  FBottom:=AValue;
  DoChange;
end;

procedure TGLViewport.SetLeft(const AValue: Integer);
begin
  if FLeft=AValue then exit;
  FLeft:=AValue;
  DoChange;
end;

procedure TGLViewport.SetRight(const AValue: Integer);
begin
  if FRight=AValue then exit;
  FRight:=AValue;
  DoChange;
end;

procedure TGLViewport.SetTop(const AValue: Integer);
begin
  if FTop=AValue then exit;
  FTop:=AValue;
  DoChange;
end;

procedure TGLViewport.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGLViewport.Assign(Source: TGLViewport);
begin
  SetAll(Source.Top, Source.Left, Source.Bottom, Source.Right);
end;

procedure TGLViewport.SetAll(const ATop, ALeft, ABottom, ARight: Integer);
begin
  if (FTop = ATop) and (FLeft = ALeft) and (FRight = ARight) and (FBottom = ABottom) then
    Exit;
  FTop := ATop;
  FLeft := ALeft;
  FRight := ARight;
  FBottom := ABottom;
  DoChange;
end;

end.

