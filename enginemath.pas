unit EngineMath;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, Math, EngineTypes;

type
	TGlMatrix = record
		case integer of
			1: (n00, n10, n20, n30, n01, n11, n21, n31, n02, n12, n22, n32, n03, n13, n23, n33: GLfloat);
			2: (colums: array[0..3] of TVec4);
			3: (n: array[0..15] of GLfloat);
	end;

function BuildPerspProjMat(fov, aspect, znear, zfar: GLfloat): TGlMatrix;

procedure Limit(var input: TVec3; const length: GLfloat);
function LengthSquare(const input: TVec3): GLfloat;
function LengthSquare(const input: TVec3i): GLfloat; overload;
function Normalize(input: TVec3): TVec3;

// this brakes for number = 0!!
function Q_rsqrt(number: GLfloat): GLfloat;

implementation

function BuildPerspProjMat(fov, aspect, znear, zfar: GLfloat): TGlMatrix;
var
	ymax, ymin, xmax, xmin, Width, Height, depth, q, qn, w, h: GLfloat;
begin
	ymax := znear * tan(DegToRad(fov));
	ymin := -ymax;
	xmax := ymax * aspect;
	xmin := ymin * aspect;

	Width := xmax - xmin;
	Height := ymax - ymin;

	depth := zfar - znear;
	q := -(zfar + znear) / depth;
	qn := -2 * (zfar * znear) / depth;

	w := 2 * znear / Width;
	w := w / aspect;
	h := 2 * znear / Height;

	Result.n[0] := w;
	Result.n[1] := 0;
	Result.n[2] := 0;
	Result.n[3] := 0;

	Result.n[4] := 0;
	Result.n[5] := h;
	Result.n[6] := 0;
	Result.n[7] := 0;

	Result.n[8] := 0;
	Result.n[9] := 0;
	Result.n[10] := q;
	Result.n[11] := -1;

	Result.n[12] := 0;
	Result.n[13] := 0;
	Result.n[14] := qn;
	Result.n[15] := 0;
end;

{$IF sizeof(glfloat) = 4}
function Q_rsqrt(number: GLfloat): GLfloat;
	// Result ~= 1/ sqrt(number)
var
	i: TIntFloat;
	x2: GLfloat;
const
	threehalfs: GLfloat = 1.5;
begin
	i.floatval := number;
	x2 := number * 0.5;
	i.intval := $5f375a86 - (i.intval shr 1);
	Result := i.floatval * (threehalfs - (x2 * i.floatval * i.floatval));
	//y  = y * ( threehalfs - ( x2 * y * y ) ); // 2nd iteration
end;
{$ENDIF}

// TODO implement those in asm:

function LengthSquare(const input: TVec3): GLfloat;
begin
	Result := input.X * input.X + input.Y * input.Y + input.Z * input.Z;
end;

function LengthSquare(const input: TVec3i): GLfloat;
begin
  Result := input.X * input.X + input.Y * input.Y + input.Z * input.Z;
end;

function Normalize(input: TVec3): TVec3;
begin
	Result := input * Q_rsqrt(LengthSquare(input));
end;


procedure LimitDoNothing(var {%H-}input: TVec3; const {%H-}length: GLfloat); register;
begin
end;

procedure DoLimit(var input: TVec3; const length: GLfloat); register;
begin
	input := input * (Q_rsqrt(LengthSquare(input)) * length);
end;

type
	TLimitFunc = procedure(var input: TVec3; const length: GLfloat); register;

const
	limitLookup: array[boolean] of TLimitFunc = (@LimitDoNothing, @DoLimit);


procedure Limit(var input: TVec3; const length: GLfloat);
begin
	limitLookup[(length * length) < LengthSquare(input)](input, length);
end;

end.
