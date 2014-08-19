unit Unit1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
	StdCtrls, EngineMemory,
	EngineStrings, Convenience;

type

	{ TForm1 }

	TForm1 = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Edit1: TEdit;
		Edit2: TEdit;
		Memo1: TMemo;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Edit1Change(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure UpdateMemo;
	private
		{ private declarations }
	public
		{ public declarations }
	end;

var
	Form1: TForm1;
	memmgr: TSortedMemoryManager;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Edit1Change(Sender: TObject);
var
	tmpstring: TEngineString;
begin
	tmpstring := EngineString(Edit1.Text);
	Edit2.Text := IntToStr(tmpstring.xHash) + ';' + IntToStr(tmpstring.dHash);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	memmgr := TSortedMemoryManager.Create(SizeOf(TEngineString), 10, 5,
		TComparer(@CompareEngineStrings));
end;

procedure TForm1.UpdateMemo;
var
	zhlr: integer;
begin
	Memo1.Lines.Clear;
	for zhlr := memmgr.Count - 1 downto 0 do
	begin
		Memo1.Lines.Add(BinToHexStr(PEngineString(memmgr.Items[zhlr]),
			SizeOf(TEngineString)));
	end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
	zhlr: integer;
	new: TEngineString;
begin
	new := EngineString(Edit1.Text);
	for zhlr := memmgr.Count - 1 downto 0 do
	begin
		if (PEngineString(memmgr.Items[zhlr]))^ = new then
			exit;
	end;
	memmgr.Request;
	memmgr.Items[memmgr.Count - 1] := @new;
	UpdateMemo;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
	zhlr: integer;
	new: TEngineString;
begin
	new := EngineString(Edit1.Text);
	for zhlr := memmgr.Count - 1 downto 0 do
	begin
		if (PEngineString(memmgr.Items[zhlr]))^ = new then
		begin
			memmgr.Items[zhlr] := memmgr.Items[memmgr.Count - 1];
			memmgr.Release;
			break;
		end;
	end;
	UpdateMemo;
end;

end.
