unit MainForm;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
	ExtCtrls, SortedList, NetTypes;

type

	TWPoint = record
		X, Y: word;
	end;

	{ TForm1 }

	TForm1 = class(TForm)
		procedure PaintStuff;
		procedure QuickSort(leftd, rightd: integer);
		procedure FormClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormResize(Sender: TObject);
	private
		wRect: TRect;
		nodes: TSortedList;
		Buffer: TImage;
	public
		{ public declarations }
	end;

var
	Form1: TForm1;

implementation

{$R *.lfm}

//procedure TForm1.QuickSort(leftd, rightd: integer);

//procedure Swap(p, q: word);
//  var
//    tmppoint: TWPoint;
//  begin
//    tmppoint := points[p];
//    points[p] := points[q];
//    points[q] := tmppoint;
//  end;

//var
//  tmpindex, zhlr: integer;
//begin
//  tmpindex := leftd;
//  if left < rightd then
//  begin
//    for zhlr := leftd to rightd - 1 do
//    begin
//      if points[zhlr].X < points[rightd].X then
//      begin
//        swap(zhlr, tmpindex);
//        Inc(tmpindex);
//      end;
//    end;
//    swap(tmpindex, rightd);
//    QuickSort(leftd, tmpindex - 1);
//    QuickSort(tmpindex + 1, rightd);
//  end;
//end;

{ TForm1 }

procedure TForm1.PaintStuff;
var
	it: TNetNode;
begin
	Buffer.Canvas.Clear;
	while it <> nil do
	begin
		Buffer.Canvas.EllipseC(points[zhlr].X, points[zhlr].Y, 3, 3);

    it := it.ne
	end;
	Canvas.CopyRect(wRect, Buffer.Canvas, wRect);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	stage := 0;
	Canvas.Font.Color := clRed;
	Buffer := TImage.Create(self);
	Buffer.SetBounds(0, 0, ClientWidth, ClientHeight);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	Buffer.SetBounds(0, 0, ClientWidth, ClientHeight);
	wRect := Rect(0, 0, ClientWidth, ClientHeight);
	Buffer.SetBounds(0, 0, ClientWidth, ClientHeight);
end;

end.
