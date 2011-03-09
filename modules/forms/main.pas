unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    HelpAbout: TAction;
    Actions: TActionList;
    Images: TImageList;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    procedure HelpAboutExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
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

end.

