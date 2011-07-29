program sigvize;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, audorra, gametoolsetpkg, dglsdk, lazcontrols, lazopenglcontext, Main,
  AboutFrm, SiglyzeFilter, glbase, glframebuffer, glshader, glviewportobj,
  ioconfig
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

