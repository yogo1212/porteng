unit Unit1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
	EngineResource;

type

	{ TForm1 }

	TForm1 = class(TForm)
Button1: TButton;
		Memo1: TMemo;
procedure Button1Click(Sender: TObject);
  procedure FormResize(Sender: TObject);
	private
		{ private declarations }
	public
		{ public declarations }
	end;

var
	Form1: TForm1;

implementation

var
	resourceTree: TGameResource = nil;

{$R *.lfm}

procedure AddResource(rtype: TGameResourceType;
	loader: TGameResourceLoader; var prefPath: string);
var
	tmp: TGameResource;
begin
	tmp := resourceTree.CreateNode(prefPath);
	FreeAndNil(tmp.Content);
  tmp.loader := loader;
	tmp.Content := nil;
	tmp.resourceType := rtype;
end;

{ TForm1 }

procedure TForm1.FormResize(Sender: TObject);
begin
	with Memo1 do
	begin
		Top := 0;
		Left := 0;
		Width := Form1.ClientWidth;
		Height := Form1.ClientHeight;
	end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Hide;
  Memo1.Show;
	resourceTree := TGameResource.Create('', grtNode, nil);
  Application.ProcessMessages;

  AddResource();
end;

end.

