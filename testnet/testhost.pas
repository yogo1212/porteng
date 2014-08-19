unit TestHost;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
	ExtCtrls, dynstructs;

type

	{ TForm1 }

	TForm1 = class(TForm)
		UpdateBtn: TButton;
		NodeSubscriptionRadiusEdit: TEdit;
		NodeSubscriptionRadiusLbl: TLabel;
		ControlPanel: TPanel;
		NodePanel: TPanel;
		procedure FormCreate(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure NodePanelMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: integer);
		procedure UpdateBtnClick(Sender: TObject);
	private
		{ private declarations }
	public
		{ public declarations }
	end;

var
	Form1: TForm1;
	nodeSubRadius: word;
  nodeTree: TBinTree;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
	DoubleBuffered := True;
	ClientHeight := 400;
	ClientWidth := 400;

  nodeTree:= TBinTree.Create(nil);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	NodeSubscriptionRadiusEdit.Top := NodeSubscriptionRadiusLbl.Height;
	ControlPanel.Height := NodeSubscriptionRadiusEdit.Top +
		NodeSubscriptionRadiusEdit.Height;
	ControlPanel.Width := ClientWidth;
	UpdateBtn.Left := ClientWidth - UpdateBtn.Width;

	NodePanel.Width := ClientWidth;
	NodePanel.Top := ControlPanel.Height;
	NodePanel.Height := ClientHeight - ControlPanel.Height;
end;

procedure TForm1.NodePanelMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: integer);
begin
	if Button = mbLeft then
	begin

	end
	else if Button = mbRight then
	begin

	end;
end;

procedure TForm1.UpdateBtnClick(Sender: TObject);
begin
	nodeSubRadius := StrToInt(NodeSubscriptionRadiusEdit.Text);
end;

end.
