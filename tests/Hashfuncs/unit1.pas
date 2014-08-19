unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Convenience,
  EngineStrings;

type

  { TForm1 }

  TForm1 = class(TForm)
  Button1: TButton;
  Edit1: TEdit;
  Memo1: TMemo;
  procedure Button1Click(Sender: TObject);
  procedure Memo1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var tmpstring: TEngineString;
begin
  tmpstring := EngineString(Edit1.Text);
  Memo1.Lines.Add(IntToStr(tmpstring.xHash)+ ';' + IntToStr(tmpstring.dHash));
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

end.

