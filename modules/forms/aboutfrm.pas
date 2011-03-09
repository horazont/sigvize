unit AboutFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ButtonPanel;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Buttons: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Lsigvize: TLabel;
    LVersion: TLabel;
    LLogo: TLabel;
    InfoPages: TPageControl;
    CreditsTab: TTabSheet;
    LicenseTab: TTabSheet;
    Credits: TMemo;
    License: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

end.

