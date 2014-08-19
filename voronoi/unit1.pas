unit Unit1;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

	TNode = record
		X, Y: word;
		cuts: byte;
	end;

	{ TForm1 }

	TForm1 = class(TForm)
		procedure fortune;
		procedure PaintFortune;
		procedure QuickSort(leftd, rightd: integer);
		procedure FormClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormResize(Sender: TObject);
	private
		pindex: integer;
		stage: byte;
		wRect: TRect;
		points: array[0..100] of TNode;
		sweepline: word;
		Buffer: TImage;
	public
		{ public declarations }
	end;

var
	Form1: TForm1;

implementation

{$R *.lfm}
procedure TForm1.QuickSort(leftd, rightd: integer);

	procedure Swap(p, q: word);
	var
		tmppoint: TNode;
	begin
		tmppoint := points[p];
		points[p] := points[q];
		points[q] := tmppoint;
	end;

var
	tmpindex, zhlr: integer;
begin
	tmpindex := leftd;
	if leftd < rightd then
	begin
		for zhlr := leftd to rightd - 1 do
		begin
			if points[zhlr].X < points[rightd].X then
			begin
				swap(zhlr, tmpindex);
				Inc(tmpindex);
			end;
		end;
		swap(tmpindex, rightd);
		QuickSort(leftd, tmpindex - 1);
		QuickSort(tmpindex + 1, rightd);
	end;
end;

{ TForm1 }

procedure TForm1.fortune;
begin
	sweepline := ClientWidth;
	while sweepline > 0 do
	begin
		// ((y-b)^2 - l^2 + a^2)/(2(l-a))


		PaintFortune;
		Sleep(200);

		Dec(sweepline);
	end;
end;

procedure TForm1.PaintFortune;
var
	zhlr, miau: integer;
begin
	Buffer.Canvas.Clear;
	Buffer.Canvas.Pen.Color := clBlack;
	Buffer.Canvas.Pen.Width := 2;
	Buffer.Canvas.Line(sweepline, 0, sweepline, ClientHeight);

	Buffer.Canvas.Pen.Color := clRed;
	Buffer.Canvas.EllipseC(points[pindex].X, points[pindex].Y, 4, 4);

	Buffer.Canvas.Pen.Color := clBlack;
	Buffer.Canvas.Pen.Width := 2;
	Buffer.Canvas.Line(sweepline, 0, sweepline, ClientHeight);

	for zhlr := 0 to Length(points) - 1 do
	begin
		Buffer.Canvas.EllipseC(points[zhlr].X, points[zhlr].Y, 3, 3);
	end;

	if sweepline = points[pindex].X then
	begin
		Buffer.Canvas.Pen.Color := clRed;
		Buffer.Canvas.Pen.Width := 4;
		Buffer.Canvas.Line(sweepline, points[pindex].Y, ClientWidth, points[pindex].Y);
	end
	else
	if sweepline < points[pindex].X then
	begin
		for zhlr := 0 to ClientHeight do
		begin
			//(a² - l² + (b - y)²)/(2(a-l)
			miau := ((zhlr - points[pindex].Y) * (zhlr - points[pindex].Y) -
				sweepline * sweepline + points[pindex].X * points[pindex].X) div
				(2 * (points[pindex].X - sweepline));
			if (miau > 0) and (miau < ClientHeight) then
				Buffer.Canvas.Pixels[miau, zhlr] := clRed;
		end;
	end;
	Canvas.CopyRect(wRect, Buffer.Canvas, wRect);
end;

procedure TForm1.FormClick(Sender: TObject);
var
	zhlr: word;
begin
	case stage of
		0:
		begin
			Canvas.Clear;				// Generate and draw 100 random points
			Randomize;
			for zhlr := 0 to Length(points) - 1 do
			begin
				points[zhlr].X := Random(ClientWidth);
				points[zhlr].Y := Random(ClientHeight);
				points[zhlr].cuts := 0;
				Canvas.EllipseC(points[zhlr].X, points[zhlr].Y, 3, 3);
			end;			// Now sort them by X
			QuickSort(0, 99);
		end;
		1:
		begin
{ for zhlr := 0 to 99 do      begin        Canvas.TextRect(Rect(points[zhlr].X - 18, points[zhlr].Y -
          18, points[zhlr].X + 2, points[zhlr].Y + 2), points[zhlr].X + 2
          , points[zhlr].Y + 2, IntToStr(zhlr));      end;  }			fortune;
		end;
		2:
		begin
		end;
	end;
	stage := (stage + 1) mod 3;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	stage := 0;
	Canvas.Font.Color := clRed;
	Buffer := TImage.Create(self);
	Buffer.SetBounds(0, 0, ClientWidth, ClientHeight);

	pindex := 80;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	if stage = 1 then
	begin
		Application.ProcessMessages;
		FormCreate(Self);
		FormClick(Self);
	end;

	wRect := Rect(0, 0, ClientWidth, ClientHeight);
	Buffer.SetBounds(0, 0, ClientWidth, ClientHeight);
end;

end.
