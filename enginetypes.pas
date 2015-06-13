unit EngineTypes;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, Math;

type

{$IF sizeof(glfloat) = sizeof(gluint)}
  Tglfloatsizedint = gluint;
{$ELSE}
{$ERROR glfloatsize not uint!}
{$ENDIF}

  Pglfloatsizeduint = ^Tglfloatsizedint;

	TIntFloat = record
		case boolean of
			False: (intval: Tglfloatsizedint);
			True: (floatval: GLfloat);
	end;

const
  glfloatsignbitindex = ((sizeof(GLfloat) shl 3) - 1);
  // TODO this gives 'range check error while evaluating constant'
  glfloatsignbit: Tglfloatsizedint = 1 shl glfloatsignbitindex;

  worldChunkSize = 512;

type
	TGameObject = class
		procedure passtime(seconds: GLfloat); virtual; abstract;
	end;

	// 16 bits are xz-rota, 16 bit y-rota; rotated in that order
	// relative to (0,-1,1)
	TWordNormal = longword;

	//Assume screen size < SizeOf(Word)
	TWindowPosition = packed record
		x, y: word;
	end;

	TVec2 = packed record
		X, Y: GLfloat;
	end;

	PVec2 = ^TVec2;

	TVec3 = packed record
		X, Y, Z: GLfloat;
	end;

	PVec3 = ^TVec3;

  TCol3 = packed record
    R, G, B: GLfloat;
	end;

  PCol3 = ^TCol3;

	TDVec3 = packed record
		X, Y, Z: GLdouble;
	end;

  TVec3ub = packed record
    x, y, z: GLubyte;
  end;

  PVec3ub = ^TVec3ub;

	TVec3i = packed record
		X, Y, Z: glint;
	end;

  TCol3b = packed record
    R, G, B: GLubyte;
	end;

  PCol3b = ^TCol3b;

	PVec3i = ^TVec3i;

  TVec3ui = packed record
    X,Y,Z: GLuint;
  end;

  PVec3ui = ^TVec3ui;

	TVec3s = packed record
		X, Y, Z: GLshort;
	end;

  PVec3s = ^TVec3s;

	TVec3us = packed record
		X, Y, Z: GLushort;
	end;

  PVec3us = ^TVec3us;

	TVec4 = packed record
		X, Y, Z, W: GLfloat;
	end;

  PVec4 = ^TVec4;

  TCol4 = packed record
    R, G, B, A: GLfloat;
	end;

  PCol4 = ^TCol4;

  TVec4ub = packed record
    X, Y, Z, W: GLubyte;
  end;

  PVec4ub = ^TVec4ub;

  TCol4b = packed record
    R, G, B, A: GLubyte;
	end;

  PCol4b = ^TCol4b;

	TVec3Coord2 = packed record
		vec: TVec3;
		coord: TVec2;
	end;

  PVec3Coord2 = ^TVec3Coord2;

	TVec2Coord2 = packed record
		vec, coord: TVec2;
	end;

  PVec2Coord2 = ^TVec2Coord2;

	TVec3Col3 = packed record
		vec: TVec3;
    col: TCol3;
	end;

  PVec3Col3 = ^TVec3Col3;

	TVec3Col3b = packed record
		vec: TVec3;
    col: TCol3b;
	end;

  PVec3Col3b = ^TVec3Col3b;

	TVec3Col4 = packed record
		vec: TVec3;
		col: TCol4;
	end;

  PVec3Col4 = ^TVec3Col4;

	TVec3Col4b = packed record
		vec: TVec3;
    col: TCol4b;
	end;

  PVec3Col4b = ^TVec3Col4b;

  TChunkPosOffset = TVec3;
  TChunkPosition = TVec3i;
	TGamePosition = packed record
    offset: TChunkPosOffset;
		worldPos: TChunkPosition;
	end;

	PGamePosition = ^TGamePosition;

	TVoxelInfo = record
		position: TVec3;
		colour: TCol4b;
		hsize: GLfloat;
	end;

	PVoxelInfo = ^TVoxelInfo;

	{ TRotation }

	TRotation = packed record
		angle, X, Y, Z, rad: GLfloat;
	end;

	TGameRotationSet = packed record
		xzRot, yRot: TRotation;
	end;

  TGameRotation = packed record
    xzangle, yangle: GLfloat;
  end;

	PGameRotationSet = ^TGameRotationSet;

  PGameRotation = ^TGameRotation;

  GLindex = GLuint;

operator + (const a, b: TVec3): TVec3; inline;
operator + (const a, b: TVec3i): TVec3i; inline;
operator + (const a, b: TVec4ub): TVec4ub; inline;
operator + (const a, b: TCol4b): TCol4b; inline;
operator - (const a, b: TVec3): TVec3; inline;
operator - (const a, b: TVec3i): TVec3i; inline;
operator - (const a, b: TVec4ub): TVec4ub; inline;
operator * (const a: TVec3; const b: GLfloat): TVec3; inline;
operator * (const a: TVec3i; const b: GLshort): TVec3i; inline;
operator * (const a: TVec3ub; const b: Byte): TVec3ub; inline;
operator * (const a: TCol3b; const b: Byte): TCol3b; inline;
operator * (const a: TVec4ub; const b: Byte): TVec4ub; inline;
operator * (const a: TCol4b; const b: Byte): TCol4b; inline;
operator / (const a: TVec3; const b: GLfloat): TVec3; inline;
operator div (const a: TVec3ub; const b: Byte): TVec3ub; inline;
operator div (const a: TCol3b; const b: Byte): TCol3b; inline;
function Dot(const a, b: TVec3): GLfloat; inline;
function Cross(const a, b: TVec3): TVec3; inline;
operator = (const a, b: TVec3): Boolean; inline;
operator = (const a, b: TVec3i): Boolean; inline;
operator = (const a, b: TVec4): Boolean; inline;

operator - (const a, b: TGamePosition): TVec3; inline;

function GamePosition(const x, y, z: GLfloat; const worldX, worldY, worldZ: GLint): TGamePosition;
function WindowPoint(const X, Y: word): TWindowPosition;
function Vec2(const x, y: GLfloat): TVec2; inline;
function Vec3(const x, y, z: GLfloat): TVec3; inline;
function DVec3(const x, y, z: GLdouble): TDVec3;
function Vec3(const vec3i: TVec3i): TVec3; overload;
function Vec3(const vec3s: TVec3s): TVec3; overload;
function Vec3i(const x, y, z: glInt): TVec3i; inline;
function Vec3ui(const x, y, z: gluInt): TVec3ui;
function Vec3us(const x, y, z: GLushort): TVec3us;
function Vec4ub(const x, y, z, w: Byte): TVec4ub;
function Vec4(const x, y, z, w: GLfloat): TVec4;
function Col3(const vec: TVec3): TCol3;
function Col3(const r, g, b: GLfloat): TCol3; overload;
function Col3(const col: TCol3b): TCol3; overload;
function Col3b(const vec: TVec3): TCol3b;
function Col3b(const r, g, b: GLubyte): TCol3b;
function Col4b(const r, g, b, a: Byte): TCol4b;

function Floor(const vec3: TVec3): TVec3i; inline;
function DegToRad(const Value: GLfloat): GLfloat; inline;
function RadToDeg(const Value: GLfloat): GLfloat; inline;

procedure Interleave(v1: PVec3; v2: PCol3; buffa: PVec3Col3; count: Cardinal);
procedure Interleave(v1: PVec3; v2: PVec2; buffa: PVec3Coord2; count: Cardinal); overload;
procedure Interleave(v1, v2: PVec2; buffa: PVec2Coord2; count: Cardinal); overload;
procedure Interleave(v1: PVec3; v2: PCol3b; buffa: PVec3Col3b; count: Cardinal); overload;

function VecToStr(input: TVec2): String;
function VecToStr(input: TVec3): String; overload;
function VecToStr(input: TVec3ub): String; overload;
function VecToStr(input: TVec4): String; overload;
function VecToStr(input: TVec4ub): String; overload;

function ColToStr(input: TCol3): String;
function ColToStr(input: TCol3b): String; overload;
function ColToStr(input: TCol4): String; overload;
function ColToStr(input: TCol4b): String; overload;

function Rotation(const angle, x, y, z: GLfloat): TRotation;
function XYZRotation(const xzangle, yangle: GLfloat): TGameRotation;
procedure Rotate(const amount: GLfloat; rota: PGameRotation); register; inline;
procedure RotateClamped(const amount: GLfloat; rota: PGameRotation); register; inline;

implementation

type
	TGameAlterProc = procedure(amount: GLfloat; rota: PGameRotation); register;

procedure DoNothing({%H-}amount: GLfloat; {%H-}rota: PGameRotation);
begin

end;

procedure actuallyRotateXZ(amount: GLfloat; rota: PGameRotation);
const twopi = 2 * pi;
begin
	rota^.xzangle += DegToRad(amount);
	if rota^.xzangle < 0 then
		rota^.xzangle += twopi
	else if rota^.xzangle > twopi then
		rota^.xzangle -= twopi;
end;

procedure actuallyRotateY(amount: GLfloat; rota: PGameRotation);
const halfpi = 0.5 * pi;
begin
	// This will be restricted to 90° up and down
	rota^.yangle += DegToRad(amount);
	if rota^.yangle < -halfpi then
		rota^.yangle := -halfpi + 0.0001
	else if rota^.yangle > halfpi then
		rota^.yangle := halfpi - 0.0001;
end;

var
	rotateTable: array[boolean] of TGameAlterProc = (@DoNothing, @actuallyRotateXZ);
	rotateClampedTable: array[boolean] of TGameAlterProc = (@DoNothing, @actuallyRotateY);


procedure Rotate(const amount: GLfloat; rota: PGameRotation); register;
begin
  rotateTable[amount <> 0](amount, rota);
end;

procedure RotateClamped(const amount: GLfloat; rota: PGameRotation); register;
begin
  rotateClampedTable[amount <> 0](amount, rota);
end;

// TODO do those with mmx/sse4

operator + (const a, b: TVec3): TVec3;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
end;

operator = (const a, b: TVec3i): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.Z = b.Z);
end;

operator + (const a, b: TVec3i): TVec3i;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
  Result.Z := a.Z + b.Z;
end;

operator - (const a, b: TVec3i): TVec3i;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
  Result.Z := a.Z - b.Z;
end;

operator + (const a, b: TVec4ub): TVec4ub;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
  Result.w := a.w + b.w;
end;

operator - (const a, b: TVec4ub): TVec4ub;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
  Result.w := a.w - b.w;
end;

operator * (const a: TCol3b; const b: Byte): TCol3b;
begin
  Result.R := a.R * b;
  Result.G := a.G * b;
  Result.B := a.B * b;
end;

operator * (const a: TVec4ub; const b: Byte): TVec4ub;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
  Result.w := a.w * b;
end;

operator div (const a: TVec3ub; const b: Byte): TVec3ub;
begin
  Result.x := a.x div b;
  Result.y := a.y div b;
  Result.z := a.z div b;
end;

operator * (const a: TVec3ub; const b: Byte): TVec3ub;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
end;

operator = (const a, b: TVec3): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.Z = b.Z);
end;

operator - (const a, b: TGamePosition): TVec3;
begin
  Result.X := (a.worldPos.X - b.worldPos.X) * worldChunkSize + (a.offset.X - b.offset.X);
  Result.Y := (a.worldPos.Y - b.worldPos.Y) * worldChunkSize + (a.offset.Y - b.offset.Y);
  Result.Z := (a.worldPos.Z - b.worldPos.Z) * worldChunkSize + (a.offset.Z - b.offset.Z);
end;

operator * (const a: TVec3i; const b: GLshort): TVec3i;
begin
  Result.X := a.X * b;
  Result.Y := a.Y * b;
  Result.Z := a.Z * b;
end;

operator = (const a, b: TVec4): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y) and (a.Z = b.Z) and (a.W = b.W);
end;

operator + (const a, b: TCol4b): TCol4b;
begin
  Result.R := a.R + b.R;
  Result.G := a.G + b.G;
  Result.B := a.B + b.B;
  Result.A := a.A + b.A;
end;

operator - (const a, b: TVec3): TVec3;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
  Result.Z := a.Z - b.Z;
end;

operator div (const a: TCol3b; const b: Byte): TCol3b;
begin
  Result.R := a.R div b;
  Result.G := a.G div b;
  Result.B := a.B div b;
end;

function Dot (const a, b: TVec3): GLfloat;
begin
  Result := a.X * b.X + a.Y * b.Y + a.Z * b.Z;
end;

function Cross (const a, b: TVec3): TVec3;
begin
  Result.X := a.Y * b.Z - a.Z - b.Y;
  Result.Y := a.Z * b.X - a.X - b.Z;
  Result.Z := a.X * b.Y - a.Y - b.X;
end;

operator * (const a: TVec3; const b: GLfloat): TVec3;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
end;

operator * (const a: TCol4b; const b: Byte): TCol4b;
begin
  Result.R := a.R * b;
  Result.G := a.G * b;
  Result.B := a.B * b;
  Result.A := a.A * b;
end;

operator / (const a: TVec3; const b: GLfloat): TVec3;
begin
  Result.x := a.x / b;
  Result.y := a.y / b;
  Result.z := a.z / b;
end;

function DegToRad(const Value: GLfloat): GLfloat;
const piding: GLfloat = pi / 180;
begin
	Result := piding * Value;
end;

function RadToDeg(const Value: GLfloat): GLfloat;
const piding: GLfloat = pi / 180;
begin
	Result := Value / piding;
end;

procedure Interleave(v1: PVec3; v2: PCol3; buffa: PVec3Col3; count: Cardinal);
begin
	while count > 0 do
	begin
		buffa^.vec := v1^;
		buffa^.col := v2^;
    Dec(count);
    Inc(buffa);
		Inc(v1);
		Inc(v2);
	end;
end;

procedure Interleave(v1: PVec3; v2: PVec2; buffa: PVec3Coord2; count: Cardinal);
begin
  while count > 0 do
  begin
  	buffa^.vec := v1^;
  	buffa^.coord := v2^;
    Dec(count);
    Inc(buffa);
  	Inc(v1);
  	Inc(v2);
  end;
end;

procedure Interleave(v1, v2: PVec2; buffa: PVec2Coord2; count: Cardinal);
begin
  while count > 0 do
  begin
  	buffa^.vec := v1^;
  	buffa^.coord := v2^;
    Dec(count);
    Inc(buffa);
  	Inc(v1);
  	Inc(v2);
  end;
end;

procedure Interleave(v1: PVec3; v2: PCol3b; buffa: PVec3Col3b; count: Cardinal);
begin
  while count > 0 do
  begin
  	buffa^.vec := v1^;
  	buffa^.col := v2^;
    Dec(count);
    Inc(buffa);
  	Inc(v1);
  	Inc(v2);
  end;
end;

function VecToStr(input: TVec2): String;
begin
  Result := 'X: ' + FloatToStr(input.X) + ' '#9'Y: ' + FloatToStr(input.Y);
end;

function VecToStr(input: TVec3): String;
begin
  Result := 'X: ' + FloatToStr(input.X) + ' '#9'Y: ' + FloatToStr(input.Y) + ' '#9'Z: '
    + FloatToStr(input.Z);
end;

function VecToStr(input: TVec3ub): String;
begin
  Result := 'X: ' + IntToStr(input.X) + ' '#9'Y: ' + IntToStr(input.Y) + ' '#9'Z: '
    + IntToStr(input.Z);
end;

function VecToStr(input: TVec4): String;
begin
  Result := 'X: ' + FloatToStr(input.X) + ' '#9'Y: ' + FloatToStr(input.Y) + ' '#9'Z: '
    + FloatToStr(input.Z) + ' '#9'W: ' + FloatToStr(input.W);
end;

function VecToStr(input: TVec4ub): String;
begin
  Result := 'X: ' + IntToStr(input.X) + ' '#9'Y: ' + IntToStr(input.Y) + ' '#9'Z: '
    + IntToStr(input.Z) + ' '#9'W: ' + IntToStr(input.W);
end;

function ColToStr(input: TCol3): String;
begin
  Result := 'R: ' + FloatToStr(input.R) + ' '#9'G: ' + FloatToStr(input.G) + ' '#9'B: '
    + FloatToStr(input.B);
end;

function ColToStr(input: TCol3b): String;
begin
  Result := 'R: ' + IntToStr(input.R) + ' '#9'G: ' + IntToStr(input.G) + ' '#9'B: '
    + IntToStr(input.B);
end;

function ColToStr(input: TCol4): String;
begin
  Result := 'R: ' + FloatToStr(input.R) + ' '#9'G: ' + FloatToStr(input.G) + ' '#9'B: '
    + FloatToStr(input.B) + ' '#9'A: ' + FloatToStr(input.A);
end;

function ColToStr(input: TCol4b): String;
begin
  Result := 'R: ' + IntToStr(input.R) + ' '#9'G: ' + IntToStr(input.G) + ' '#9'B: '
    + IntToStr(input.B) + ' '#9'A: ' + IntToStr(input.A);
end;

function WindowPoint(const X, Y: word): TWindowPosition;
begin
	Result.X := X;
	Result.Y := Y;
end;

function GamePosition(const x, y, z: GLfloat; const worldX, worldY, worldZ: GLint): TGamePosition;
begin
	Result.offset.X := x;
	Result.offset.Y := y;
	Result.offset.Z := z;

	Result.worldPos.X := worldX;
	Result.worldPos.Y := worldY;
	Result.worldPos.Z := worldZ;
end;

function Vec2(const x, y: GLfloat): TVec2;
begin
	Result.x := x;
	Result.y := y;
end;

function Vec3(const x, y, z: GLfloat): TVec3;
begin
	Result.x := x;
	Result.y := y;
	Result.z := z;
end;

function Vec3(const vec3s: TVec3s): TVec3;
begin
  Result.X := vec3s.X;
  Result.Y := vec3s.Y;
  Result.Z := vec3s.Z;
end;

function Vec3i(const x, y, z: glInt): TVec3i;
begin
  Result.X := x;
  Result.Y := Y;
  Result.Z := Z;
end;

function Floor(const vec3: TVec3): TVec3i;
begin
  Result.X := Math.Floor(vec3.X);
  Result.Y := Math.Floor(vec3.Y);
  Result.Z := Math.Floor(vec3.Z);
end;

function Vec3ui(const x, y, z: gluInt): TVec3ui;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function Vec3us(const x, y, z: GLushort): TVec3us;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function DVec3(const x, y, z: GLdouble): TDVec3;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function Vec3(const vec3i: TVec3i): TVec3;
begin
  Result.X := vec3i.X;
  Result.Y := vec3i.Y;
  Result.Z := vec3i.Z;
end;

function Vec4ub(const x, y, z, w: Byte): TVec4ub;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function Vec4(const x, y, z, w: GLfloat): TVec4;
begin
	Result.x := x;
	Result.y := y;
	Result.z := z;
	Result.w := w;
end;

function Col3(const vec: TVec3): TCol3;
begin
  Result.R := vec.X;
  Result.G := vec.Y;
  Result.B := vec.Z;
end;

function Col3(const r, g, b: GLfloat): TCol3;
begin
  Result.R := r;
  Result.G := g;
  Result.B := b;
end;

function Col3(const col: TCol3b): TCol3;
begin
  Result.R := col.R / 255;
  Result.G := col.G / 255;
  Result.B := col.B / 255;
end;

function Col3b(const vec: TVec3): TCol3b;
begin
  Result.R := round(vec.X * 255);
  Result.G := round(vec.Y * 255);
  Result.B := round(vec.Z * 255);
end;

function Col3b(const r, g, b: GLubyte): TCol3b;
begin
  Result.R := r;
  Result.G := g;
  Result.B := b;
end;

function Col4b(const r, g, b, a: Byte): TCol4b;
begin
  Result.R := r;
  Result.G := g;
  Result.B := b;
  Result.A := a;
end;

function Rotation(const angle, x, y, z: GLfloat): TRotation;
begin
	Result.angle := angle;
	Result.x := x;
	Result.y := y;
	Result.z := z;
	Result.rad := DegToRad(angle);
end;

function XYZRotation(const xzangle, yangle: GLfloat): TGameRotation;
begin
	//Result.xzrot := Rotation(xzangle, 0, -1, 0);
	//Result.yrot := Rotation(yangle, 1, 0, 0);
  Result.xzangle := xzangle;
  Result.yangle := yangle;
end;

end.
