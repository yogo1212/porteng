unit EngineResourceColour;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineTypes, EngineObject, EngineShader,
	EngineResourceLoader, EngineStrings, EngineShape, EngineResource;

procedure ColouredPyramidModelLoader(const scale: GLfloat; const Name: TEngineString);
procedure ColouredBallModelLoader(const radius: GLfloat; detail: word;
	const offset: TVec3; const Name: TEngineString);

implementation

{$ifdef ENGINEDEBUG}
procedure PrintVertexList(list: PVec3Col3; Count: cardinal);
begin
	while Count > 0 do
	begin
		WriteLn(VecToStr(list^.vec) + ' ' + ColToStr(list^.col));
		Inc(list);
		Dec(Count);
	end;
end;

procedure PrintIndexList(indices: PGLint; Count: cardinal);
begin
	while Count > 0 do
	begin
		WriteLn(IntToStr(indices^));
		Inc(indices);
		Dec(Count);
	end;
end;

{$endif}

type

	{ TgrbColHandler }

	TgrbColHandler = class(TEngineResourceHandler)
		function LoadFromCache(const cache: Pointer; const cachesize: cardinal;
			out conttype: TGameResourceType): TObject; override;
		procedure FreeLoaded(loaded: Pointer; const rtype: TGameResourceType); override;
	end;

{ TgrbColHandler }

function TgrbColHandler.LoadFromCache(const cache: Pointer;
	const cachesize: cardinal; out conttype: TGameResourceType): TObject;
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
	glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Col3),
		@PVec3Col3(nil)^.vec);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Col3),
		@PVec3Col3(nil)^.col);

	conttype := grtModel;

	Result := TGameModel.Create(spColour, vertexarray, vertexbuffer, 0,
		0, drawtype, 0, vertexcount);
end;

procedure TgrbColHandler.FreeLoaded(loaded: Pointer; const rtype: TGameResourceType);
begin
	FreeAndNil(TGameModel(loaded));
end;

type

	{ TgrbIndexedColHandler }

	TgrbIndexedColHandler = class(TEngineResourceHandler)
		function LoadFromCache(const cache: Pointer; const cachesize: cardinal;
			out conttype: TGameResourceType): TObject; override;
		procedure FreeLoaded(loaded: Pointer; const rtype: TGameResourceType); override;
	end;

{ TgrbIndexedColHandler }

function TgrbIndexedColHandler.LoadFromCache(const cache: Pointer;
	const cachesize: cardinal; out conttype: TGameResourceType): TObject;
var
	bytesize: longword;
	vertexarray, vertexbuffer, elementbuffer: GLuint;
	drawtype: GLenum;
	vertexcount, indexcount: cardinal;
	iterator: Pointer;
begin
	iterator := cache;
	ExtractVar(iterator, drawtype, Sizeof(drawtype));
	ExtractVar(iterator, vertexcount, Sizeof(vertexcount));
	ExtractVar(iterator, indexcount, Sizeof(indexcount));
	bytesize := vertexcount * sizeof(TVec3Col3);

	//PrintVertexList(PVec3Col3(iterator), vertexcount);

	glGenVertexArrays(1, @vertexarray);
	glGenBuffers(1, @vertexbuffer);

	glBindVertexArray(vertexarray);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, bytesize, iterator, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Col3),
		@PVec3Col3(nil)^.vec);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, bytebool(GL_FALSE),
		SizeOf(TVec3Col3), @PVec3Col3(nil)^.col);

	iterator += bytesize;

	//PrintIndexList(PGLint(iterator), indexcount);

	glGenBuffers(1, @elementbuffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, indexcount * SizeOf(GLuint),
		iterator, GL_STATIC_DRAW);


	conttype := grtModel;

	Result := TGameModel.Create(spColour, vertexarray, vertexbuffer,
		elementbuffer, 0, drawtype, indexcount, vertexcount);
end;

procedure TgrbIndexedColHandler.FreeLoaded(loaded: Pointer;
	const rtype: TGameResourceType);
begin
	FreeAndNil(TGameModel(loaded));
end;

procedure ColouredPyramidModelLoader(const scale: GLfloat; const Name: TEngineString);
var
	fTriangleColor, fTriangle: array[0..7] of TVec3;
	Data: PByte;
	bytesize: cardinal;
begin
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

	bytesize := Length(fTriangle) * SizeOf(TVec3Col3) + SizeOf(cardinal) + SizeOf(GLenum);
	GetMem(Data, bytesize);
	PCardinal(Data)^ := Length(fTriangle);
	PGLenum(Data + SizeOf(cardinal))^ := GL_TRIANGLE_STRIP;
	Interleave(@fTriangle[0], @fTriangleColor[0],
		PVec3Col3(Data + SizeOf(cardinal) + SizeOf(GLenum)), Length(fTriangle));
	GameResourceAdd(Name, grbColModel, Data, bytesize);
	Freemem(Data, bytesize);
end;

procedure ColouredBallModelLoader(const radius: GLfloat; detail: word;
	const offset: TVec3; const Name: TEngineString);
var
	vertices: array of TVec3;
	colours: array of TCol3;
	faces: array of TTriangleFace;

	Data: PByte;
	bytesize: cardinal;

	iterator: Pointer;
	i: integer;
begin
	IcoSphere(vertices, faces, detail);
	SetLength(colours, Length(vertices));
	for i := 0 to Length(vertices) - 1 do
	begin
		colours[i] := Col3((vertices[i] + Vec3(1, 1, 1)) / 2);
		//colours[i] := Col3(1, 1, 1);
		vertices[i] := vertices[i] * radius + offset;
	end;

	bytesize := SizeOf(GLenum) + 2 * SizeOf(cardinal) + Length(vertices) *
		SizeOf(TVec3Col3) + Length(faces) * Sizeof(TTriangleFace);
	GetMem(Data, bytesize);
	iterator := Data;
	PGLenum(iterator)^ := GL_TRIANGLES;
	iterator += SizeOf(GLenum);
	PCardinal(iterator)^ := Length(vertices);
	iterator += SizeOf(cardinal);
	PCardinal(iterator)^ := 3 * Length(faces);
	iterator += SizeOf(cardinal);
	Interleave(@vertices[0], @colours[0], PVec3Col3(iterator), Length(vertices));
	iterator += Length(vertices) * SizeOf(TVec3Col3);
	StoreVar(iterator, faces[0], Sizeof(TTriangleFace) * Length(faces));
	GameResourceAdd(Name, grbIndexedColModel, Data, bytesize);



	Freemem(Data, bytesize);
end;

initialization
	SetHandler(grbColModel, TgrbColHandler.Create);
	SetHandler(grbIndexedColModel, TgrbIndexedColHandler.Create);
end.
