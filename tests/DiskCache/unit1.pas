unit Unit1;

{$mode objfpc}{$H+}

interface
{$define ENGINEDEBUG}

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
	EngineDiskCache, Convenience, EngineMemory, EngineStrings;

type

	{ TForm1 }

	TForm1 = class(TForm)
		AddButton: TButton;
		Button1: TButton;
		GetButton: TButton;
		DelButton: TButton;
		Edit1: TEdit;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Memo1: TMemo;
		MetaMemo: TMemo;
		Name1Edit: TEdit;
		x1Edit: TEdit;
		d1Edit: TEdit;
		Label1: TLabel;
		Label2: TLabel;
		procedure AddButtonClick(Sender: TObject);
		procedure Button1Click(Sender: TObject);
		procedure DelButtonClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure GetButtonClick(Sender: TObject);
		procedure Name1EditChange(Sender: TObject);
	private
		{ private declarations }
	public
		{ public declarations }
	end;

var
	Form1: TForm1;
	cache: TEngineDiskCache;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Name1EditChange(Sender: TObject);
var
	tmpxor: TXorHash;
	tmpdecx: TDeczHash;
begin
	tmpxor := HashXor(Name1Edit.Text);
	tmpdecx := HashDecz(Name1Edit.Text);
	x1Edit.Text := BinToHexStr(@tmpxor, SizeOf(tmpxor));
	d1Edit.Text := BinToHexStr(@tmpdecx, SizeOf(tmpdecx));
end;

procedure TForm1.AddButtonClick(Sender: TObject);
var
	tmpstring: TEngineString;
begin
	tmpstring := EngineString(Name1Edit.Text);
	cache.Store(EngineDiskCacheEntry(tmpstring), @Edit1.Text[1], Length(Edit1.Text));
	MetaMemo.Lines.Add(Name1Edit.Text + ': ' + #9 + IntToStr(tmpstring.xHash) +
		': ' + #9 + IntToStr(tmpstring.dHash) + LineEnding +
		BinToHexStr(@tmpstring, SizeOf(tmpstring)));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
	cnt: integer;
	tmpentry: PEngineDiskCacheEntry;
	tmpstr: string;
	tmppnt: Pointer;
	tmpsize: cardinal;
  entries : TSortedMemoryManager;
begin                     {
	Memo1.Lines.Clear;
  memo1.Lines.add(cache.printentries);
  entries := cache.getEntries;
	for cnt := 0 to entries.Count - 1 do
	begin
		tmpentry := entries.Items[cnt];
		tmpsize := cache.Load(tmpentry^.id, tmppnt);
		SetLength(tmpstr, tmpsize);
		Move(tmppnt^, tmpstr[1], tmpsize);
		Memo1.Lines.Add(GetRealValue(tmpentry^.id) + ':' + tmpstr);
		SetLength(tmpstr, 0);
		Freemem(tmppnt, tmpsize);
	end;   }
end;

procedure TForm1.DelButtonClick(Sender: TObject);
begin
	cache.Delete(EngineString(Name1Edit.Text));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	cache := TEngineDiskCache.Create('./cache/');
	MetaMemo.Clear;
end;

procedure TForm1.GetButtonClick(Sender: TObject);
var
	tmppnt: Pointer;
	tmpsize: cardinal;
	tmpstr: string;
begin
	tmpsize := cache.Load(EngineString(Name1Edit.Text), tmppnt);
	SetLength(tmpstr, tmpsize);
	Move(tmppnt^, tmpstr[1], tmpsize);
	ShowMessage(Name1Edit.Text + ' has the value "' + tmpstr + '"');
	SetLength(tmpstr, 0);
	Freemem(tmppnt, tmpsize);
end;

end.
