unit EngineResourceColor;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineTypes, EngineObject, EngineShader,
	EngineResourceLoader, EngineStrings;

function ColouredPyramidModelLoader(scale: GLfloat;
	Name: TEngineString): TResourceBuilderType;

implementation

function ColModelLoader(cache: Pointer; cachesize: cardinal;
	out conttype: TGameResourceType): TObject;
var
	bytesize: longword;
	vertexarray, vertexbuffer: GLuint;
	drawtype: GLenum;
	vertexcount: cardinal;
begin
	vertexcount := PCardinal(cache)^;
	drawtype := PGLenum(cache + SizeOf(cardinal))^;
	bytesize := 2 * vertexcount * sizeof(TVec3);

	glGenVertexArrays(1, @vertexarray);
	glGenBuffers(1, @vertexbuffer);

	glBindVertexArray(vertexarray);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, bytesize,
		Pointer(cache + SizeOf(cardinal) + SizeOf(GLenum)), GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Col3), nil);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Col3),
		Pointer(Sizeof(TVec3)));

	conttype := grtModel;

	Result := TGameModel.Create(spColour, vertexarray, vertexbuffer, 0,
		0, drawtype, 0, vertexcount);
end;

function ColouredPyramidModelLoader(scale: GLfloat;
	Name: TEngineString): TResourceBuilderType;
var
	fTriangleColor, fTriangle: array[0..7] of TVec3;
	Data: PByte;
	tmpword: cardinal;
begin
	Result := grbColModel;
	// Setup triangle vertices
	fTriangle[0] := Vec3(scale, 0, scale); //1
	fTriangle[1] := Vec3(0, 2 * scale, 0); //22
	fTriangle[2] := Vec3(-scale, 0, scale);//311
	fTriangle[3] := Vec3(-scale, 0, -scale);//322
	fTriangle[4] := Vec3(scale, 0, scale);	 //311
	fTriangle[5] := Vec3(scale, 0, -scale);		//322
	fTriangle[6] := Vec3(0, 2 * scale, 0);		 //31
	fTriangle[7] := Vec3(-scale, 0, -scale);		//3

	fTriangleColor[0] := Vec3(1, 0, 0);
	fTriangleColor[1] := Vec3(1, 1, 1);
	fTriangleColor[2] := Vec3(0, 1, 0);
	fTriangleColor[3] := Vec3(0, 0, 1);
	fTriangleColor[4] := Vec3(1, 0, 0);
	fTriangleColor[5] := Vec3(0, 1, 0);
	fTriangleColor[6] := Vec3(1, 1, 1);
	fTriangleColor[7] := Vec3(0, 0, 1);

	tmpword := Length(fTriangle) * SizeOf(TVec3Col3) + SizeOf(cardinal) + SizeOf(GLenum);
	GetMem(Data, tmpword);
	PCardinal(Data)^ := Length(fTriangle);
	PGLenum(Data + SizeOf(cardinal))^ := GL_TRIANGLE_STRIP;
	Interleave(@fTriangle[0], @fTriangleColor[0],
		PVec3Col3(Data + SizeOf(cardinal) + SizeOf(GLenum)), Length(fTriangle));
	Store(Name, Data, tmpword);
	Freemem(Data, tmpword);
end;

initialization
	SetLoader(grbColModel, @ColModelLoader);
end.
