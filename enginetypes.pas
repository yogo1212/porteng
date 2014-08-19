unit EngineTypes;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, Math;

type

{$IF sizeof(glfloat) = sizeof(glint)}
  Tglfloatsizedint = glint;
{$ELSEIF }
  ERROR!
{$ENDIF}

  Pglfloatsizedint = ^Tglfloatsizedint;

	TIntFloat = record
		case boolean of
			False: (intval: Tglfloatsizedint);
			True: (floatval: GLfloat);
	end;

const
  glfloatsignbitindex = ((sizeof(GLfloat) shl 3) - 1);
  glfloatsignbit: Tglfloatsizedint = 1 shl glfloatsignbitindex;

type
	TGameObject = class
		procedure passtime(seconds: GLfloat); virtual; abstract;
	end;

	// 16 bits are xz-rota, 16 bit y-rota; rotated in that order
	// relative to (0,-1,1)
	TWordNormal = longword;

	//Assume screen size < SizeOf(Word)
	TWindowPosition = record
		x, y: word;
	end;

	TVec2 = record
		X, Y: GLfloat;
	end;

	PVec2 = ^TVec2;

	TVec3 = record
		X, Y, Z: GLfloat;
	end;

	TDVec3 = record
		X, Y, Z: GLdouble;
	end;

	PVec3 = ^TVec3;

  TVec3ub = record
    x, y, z: GLubyte;
  end;

  PVec3ub = ^TVec3ub;

	TVec3i = record
		X, Y, Z: glint;
	end;

	PVec3i = ^TVec3i;

  TVec3ui = record
    X,Y,Z: GLuint;
  end;

  PVec3ui = ^TVec3ui;

	TVec3s = record
		X, Y, Z: GLshort ;
	end;

  PVec3s = ^TVec3s;

	TVec3us = record
		X, Y, Z: GLushort ;
	end;

  PVec3us = ^TVec3us;

	TGamePosition = TVec3;
  TRelWorldPosition = TVec3i;

	TVec4 = record
		X, Y, Z, W: GLfloat;
	end;

  PVec4 = ^TVec4;

  TVec4ub = record
    x, y, z, w: GLubyte;
  end;

  PVec4ub = ^TVec4ub;

	TVec3Coord2 = record
		vec: TVec3;
		coord: TVec2;
	end;

  TCol3b = TVec3ub;
  PCol3b = ^TCol3b;
  TCol4b = TVec4ub;
  PCol4b = ^TCol4b;

  PVec3Coord2 = ^TVec3Coord2;

	TVec2Coord2 = record
		vec, coord: TVec2;
	end;

  PVec2Coord2 = ^TVec2Coord2;

	TVec3Col3 = record
		vec, col: TVec3;
	end;

  PVec3Col3 = ^TVec3Col3;

	TVec3Col4 = record
		vec: TVec3;
		col: TVec4;
	end;


  PVec3Col4 = ^TVec3Col4;

	PGamePosition = ^TGamePosition;

	{ TRotation }

	TRotation = record
		angle, X, Y, Z, rad: GLfloat;
	end;

	TGameRotationSet = record
		xzRot, yRot: TRotation;
	end;

  TGameRotation = record
    xzangle, yangle: GLfloat;
  end;

	PGameRotationSet = ^TGameRotationSet;

  PGameRotation = ^TGameRotation;

operator + (const a, b: TVec3): TVec3; inline;
operator - (const a, b: TVec3): TVec3; inline;
function Dot(const a, b: TVec3): GLfloat; inline;
function Cross(const a, b: TVec3): TVec3; inline;
operator * (const a: TVec3; const b: GLfloat): TVec3; inline;
operator / (const a: TVec3; const b: GLfloat): TVec3; inline;
operator = (const a, b: TVec3): Boolean; inline;
operator = (const a, b: TVec3i): Boolean; inline;
operator + (const a, b: TVec3i): TVec3i; inline;
operator - (const a, b: TVec3i): TVec3i; inline;
operator * (const a: TVec3i; const b: GLshort): TVec3i; inline;
operator = (const a, b: TVec4): Boolean; inline;
operator + (const a, b: TVec4ub): TVec4ub; inline;
operator - (const a, b: TVec4ub): TVec4ub; inline;
operator * (const a: TVec4ub; const b: Byte): TVec4ub; inline;
operator * (const a: TVec3ub; const b: Byte): TVec3ub; inline;
operator div (const a: TVec3ub; const b: Byte): TVec3ub; inline;

function Position(x, y, z: GLfloat): TGamePosition;
function WindowPoint(X, Y: word): TWindowPosition;
function Vec2(x, y: GLfloat): TVec2; inline;
function Vec3(x, y, z: GLfloat): TVec3; inline;
function DVec3(x, y, z: GLdouble): TDVec3;
function Vec3(vec3i: TVec3i): TVec3; overload;
function Vec3(vec3s: TVec3s): TVec3; overload;
function Vec3i(x, y, z: glInt): TVec3i; inline;
function Vec3ui(x, y, z: gluInt): TVec3ui;
function Vec3us(x, y, z: GLushort): TVec3us;
function Vec4ub(x,y,z,w: Byte): TVec4ub;
function Vec4(x, y, z, w: GLfloat): TVec4;

function Floor(vec3: TVec3): TVec3i; inline;
function DegToRad(Value: GLfloat): GLfloat; inline;
function RadToDeg(Value: GLfloat): GLfloat; inline;

procedure Interleave(v1, v2: PVec3; buffa: PVec3Col3; count: Cardinal);
procedure Interleave(v1: PVec3; v2: PVec2; buffa: PVec3Coord2; count: Cardinal); overload;
procedure Interleave(v1, v2: PVec2; buffa: PVec2Coord2; count: Cardinal); overload;

function VecToStr(input: TVec2): String;
function VecToStr(input: TVec3): String; overload;
function VecToStr(input: TVec4): String; overload;

function Rotation(angle, x, y, z: GLfloat): TRotation;
function XYZRotation(xzangle, yangle: GLfloat): TGameRotation;
procedure Rotate(amount: GLfloat; rota: PGameRotation); register; inline;
procedure RotateClamped(amount: GLfloat; rota: PGameRotation); register; inline;

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


procedure Rotate(amount: GLfloat; rota: PGameRotation); register;
begin
  rotateTable[amount <> 0](amount, rota);
end;

procedure RotateClamped(amount: GLfloat; rota: PGameRotation); register;
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

operator - (const a, b: TVec3): TVec3;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
  Result.Z := a.Z - b.Z;
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

operator / (const a: TVec3; const b: GLfloat): TVec3;
begin
  Result.x := a.x / b;
  Result.y := a.y / b;
  Result.z := a.z / b;
end;

function DegToRad(Value: GLfloat): GLfloat;
const piding: GLfloat = pi / 180;
begin
	Result := piding * Value;
end;

function RadToDeg(Value: GLfloat): GLfloat;
const piding: GLfloat = pi / 180;
begin
	Result := Value / piding;
end;

procedure Interleave(v1, v2: PVec3; buffa: PVec3Col3; count: Cardinal);
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

function VecToStr(input: TVec2): String;
begin
  Result := 'X: ' + FloatToStr(input.X) + 'Y: ' + FloatToStr(input.Y);
end;

function VecToStr(input: TVec3): String;
begin
  Result := 'X: ' + FloatToStr(input.X) + 'Y: ' + FloatToStr(input.Y) + 'Z: '
    + FloatToStr(input.Z);
end;

function VecToStr(input: TVec4): String;
begin
  Result := 'X: ' + FloatToStr(input.X) + 'Y: ' + FloatToStr(input.Y) + 'Z: '
    + FloatToStr(input.Z) + 'W: ' + FloatToStr(input.W);
end;

function WindowPoint(X, Y: word): TWindowPosition;
begin
	Result.X := X;
	Result.Y := Y;
end;

function Position(x, y, z: GLfloat): TGamePosition;
begin
	Result.x := x;
	Result.y := y;
	Result.z := z;
end;

function Vec2(x, y: GLfloat): TVec2;
begin
	Result.x := x;
	Result.y := y;
end;

function Vec3(x, y, z: GLfloat): TVec3;
begin
	Result.x := x;
	Result.y := y;
	Result.z := z;
end;

function Vec3(vec3s: TVec3s): TVec3;
begin
  Result.X := vec3s.X;
  Result.Y := vec3s.Y;
  Result.Z := vec3s.Z;
end;

function Vec3i(x, y, z: glInt): TVec3i;
begin
  Result.X := x;
  Result.Y := Y;
  Result.Z := Z;
end;

function Floor(vec3: TVec3): TVec3i;
begin
  Result.X := Math.Floor(vec3.X);
  Result.Y := Math.Floor(vec3.Y);
  Result.Z := Math.Floor(vec3.Z);
end;

function Vec3ui(x, y, z: gluInt): TVec3ui;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function Vec3us(x, y, z: GLushort): TVec3us;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function DVec3(x, y, z: GLdouble): TDVec3;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function Vec3(vec3i: TVec3i): TVec3;
begin
  Result.X := vec3i.X;
  Result.Y := vec3i.Y;
  Result.Z := vec3i.Z;
end;

function Vec4ub(x, y, z, w: Byte): TVec4ub;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function Vec4(x, y, z, w: GLfloat): TVec4;
begin
	Result.x := x;
	Result.y := y;
	Result.z := z;
	Result.w := w;
end;

function Rotation(angle, x, y, z: GLfloat): TRotation;
begin
	Result.angle := angle;
	Result.x := x;
	Result.y := y;
	Result.z := z;
	Result.rad := DegToRad(angle);
end;

function XYZRotation(xzangle, yangle: GLfloat): TGameRotation;
begin
	//Result.xzrot := Rotation(xzangle, 0, -1, 0);
	//Result.yrot := Rotation(yangle, 1, 0, 0);
  Result.xzangle := xzangle;
  Result.yangle := yangle;
end;

end.
