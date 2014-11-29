unit EngineResourceTexture;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineResource, dglOpenGL, EngineTypes, EngineObject, EngineShader,
	EngineResourceLoader, EngineStrings, EngineGLContext;

type

	TGameBoundry = (gbMiddle, gbBottomMiddle, gbTopLeft);

	PGameTextureRepr = ^TGameTextureRepr;

	TGameTextureRepr = record
		handle: GLuint;
	end;

	{ TGameTexture }

	TGameTexture = class
		handle: GLuint;
		function GetRepresentation: TGameTextureRepr;
		destructor Destroy; override;
	end;

	{ TGameTexModPair }

	TGameTexModPair = class(TGameModel)
		model, texture: TEngineString;
		gtexture: TGameTextureRepr;
		gmodel: TGameModelRepr;
		constructor Create(nmodel, ntexture: TEngineString);
		destructor Destroy; override;
	end;

function GameTextureLoader(const Name: TEngineString; const Width, Height: cardinal;
	const internalformat: GLint; const pixeltype: GLenum; const format: GLint;
	const pixels: Pointer): TResourceBuilderType;
function GameTexMesh3Loader(Name: TEngineString; vertexcount: cardinal;
	vertexes: PVec3; texcoords: PVec2; drawtype: GLenum; indexcount: cardinal;
	indexlist: PGLuint): TResourceBuilderType;
function GameTexMesh2Loader(Name: TEngineString; vertexcount: cardinal;
	vertexes: PVec2; texcoords: PVec2; drawtype: GLenum; indexcount: cardinal;
	indexlist: PGLuint): TResourceBuilderType;
function GameTexModLoader(Name, model, texture: TEngineString): TResourceBuilderType;
function GameTextureLoader(Name: TEngineString; Width, Height: cardinal;
	format, internalFormat: GLint; pixeltype: GLenum; filename: string;
	pixeloffset: cardinal; topdown: boolean): TResourceBuilderType; overload;

procedure Create2DRect(Name: TEngineString; boundry: TGameBoundry;
	Width, Height: GlFloat);
procedure CreateCube(Name: TEngineString; boundry: TGameBoundry; Size: GLfloat);
procedure Create2DGUIRect(Name: TEngineString; boundry: TGameBoundry;
	x, y, Width, Height: GlFloat);

implementation

type
	TTextureMeta = record
		Width, Height: cardinal;
		format, internalFormat: GLint;
		pixeltype: GLenum;
	end;

	PTextureMeta = ^TTextureMeta;

	TTexModelMeta = record
		vertexcount: cardinal;
		drawtype: GLenum;
		indexcount: cardinal;
	end;

	PTexModelMeta = ^TTexModelMeta;

constructor TGameTexModPair.Create(nmodel, ntexture: TEngineString);
var
	tmpmodel: TGameModel;
begin
	gtexture := TGameTexture(GameResourceUse(ntexture, grtTexture)).GetRepresentation;
	tmpmodel := TGameModel(GameResourceUse(nmodel, grtModel));
	gmodel := tmpmodel.GetRepresentation;
	inherited Create(gmodel.programtype, gmodel.vertexarray, tmpmodel.vertexbuffer,
		gmodel.indexlist, gtexture.handle, gmodel.drawType, gmodel.indexcount,
		gmodel.vertexcount);
	model := nmodel;
	texture := ntexture;
end;

destructor TGameTexModPair.Destroy;
begin
	GameResourceUnUnse(texture);
	GameResourceUnUnse(model);
	inherited Destroy;
end;

function GameTexModLoader(Name, model, texture: TEngineString): TResourceBuilderType;
var
	tmppnt: Pointer;
	tmpsize: cardinal;
begin
	tmpsize := SizeOf(TEngineString) * 2;
	GetMem(tmppnt, tmpsize);
	Result := grbTexModPair;

	Move(model, tmppnt^, SizeOf(TEngineString));
	Move(texture, Pointer(tmppnt + SizeOf(TEngineString))^, SizeOf(TEngineString));

	Store(Name, tmppnt, tmpsize);
	FreeMem(tmppnt, tmpsize);
end;

function LoadTexModFromCache(const cache: Pointer; const cachesize: cardinal;
	out conttype: TGameResourceType): TObject;
begin
	Result := TGameTexModPair.Create(PEngineString(cache)^,
		PEngineString(cache + SizeOf(TEngineString))^);
	conttype := grtModel;
end;

function GameTextureLoader(Name: TEngineString; Width, Height: cardinal;
	format, internalFormat: GLint; pixeltype: GLenum; filename: string;
	pixeloffset: cardinal; topdown: boolean): TResourceBuilderType;
var
	tmpmeta: TTextureMeta;
	tmppnt: Pointer;
	tmpsize: cardinal;
	filenamelength: word;
begin
	filenamelength := Length(filename);
	tmpsize := filenamelength + SizeOf(tmpmeta) + SizeOf(pixeloffset) +
		SizeOf(filenamelength) + SizeOf(topdown);

	tmpmeta.format := format;
	tmpmeta.internalFormat := internalFormat;
	tmpmeta.Height := Height;
	tmpmeta.Width := Width;
	tmpmeta.pixeltype := pixeltype;
	GetMem(tmppnt, tmpsize);

	Move(tmpmeta, tmppnt^, SizeOf(tmpmeta));
	Move(filenamelength, Pointer(tmppnt + SizeOf(tmpmeta))^, SizeOf(filenamelength));
	Move(filename[1], Pointer(tmppnt + SizeOf(tmpmeta) + SizeOf(filenamelength))^,
		filenamelength);
	PCardinal(tmppnt + SizeOf(tmpmeta) + SizeOf(filenamelength) + filenamelength)^ :=
		pixeloffset;
	PBoolean(tmppnt + SizeOf(tmpmeta) + SizeOf(filenamelength) +
		filenamelength + SizeOf(pixeloffset))^ := topdown;

	Store(Name, tmppnt, tmpsize);

	FreeMem(tmppnt, tmpsize);
	Result := grbTextureFile;
end;

function LoadTextureFileFromCache(const cache: Pointer; const cachesize: cardinal;
	out conttype: TGameResourceType): TObject;
var
	texmeta: TTextureMeta;
	filenamelength: word;
	filename: string;
	handle: GLuint;
	iterator, tmppnt: Pointer;
	cnt: cardinal;
	tmpfile: TFileStream;
	offset, linebytesize: cardinal;
	topdown: boolean;
begin
	iterator := cache;
	ExtractVar(iterator, texmeta, SizeOf(texmeta));
	ExtractVar(iterator, filenamelength, SizeOf(filenamelength));
	SetLength(filename, filenamelength);
	ExtractVar(iterator, filename[1], filenamelength);
	ExtractVar(iterator, offset, SizeOf(offset));
	ExtractVar(iterator, topdown, SizeOf(topdown));

	glGenTextures(1, @handle);
	glBindTexture(GL_TEXTURE_2D, handle);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	// Optionaly, set the border color
	//float color[] = { 1.0f, 0.0f, 0.0f, 1.0f };
	//glTexParameterfv( GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, color );

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);

	if checkGlVersion(4, 2) then
		glTexStorage2D(GL_TEXTURE_2D, 1, texmeta.internalFormat, texmeta.Width,
			texmeta.Height)
	else
		glTexImage2D(GL_TEXTURE_2D, 0, texmeta.internalFormat, texmeta.Width, texmeta.Height,
			0, texmeta.format, texmeta.pixeltype, nil);

	tmpfile := TFileStream.Create(filename, fmOpenRead);
	tmpfile.Seek(offset, soBeginning);

	if texmeta.format = GL_RGBA then
		linebytesize := SizeOf(byte) * 4 * texmeta.Width
	else if texmeta.format = GL_RGB then
		linebytesize := SizeOf(byte) * 3 * texmeta.Width
	else
		raise Exception.Create('textureloader unknown pixelformat');

	Getmem(tmppnt, linebytesize);
	cnt := 0;

	while texmeta.Height > cnt do
	begin
		tmpfile.ReadBuffer(tmppnt^, linebytesize);

		glBindTexture(GL_TEXTURE_2D, handle);

		if topdown then
			glTexSubImage2D(GL_TEXTURE_2D, 0, 0, cnt, texmeta.Width, 1, texmeta.format,
				texmeta.pixeltype, tmppnt)
		else
			glTexSubImage2D(GL_TEXTURE_2D, 0, 0, texmeta.Height - 1 - cnt, texmeta.Width, 1,
				texmeta.format, texmeta.pixeltype, tmppnt);

		Inc(cnt);
	end;

	FreeAndNil(tmpfile);

	FreeMem(tmppnt, linebytesize);

	Result := TGameTexture.Create;
	conttype := grtTexture;
	TGameTexture(Result).handle := handle;
end;

{ TGameTexture }

function TGameTexture.GetRepresentation: TGameTextureRepr;
begin
	Result.handle := handle;
end;

destructor TGameTexture.Destroy;
begin
	glDeleteTextures(1, @handle);
	inherited Destroy;
end;

function GameTextureLoader(const Name: TEngineString; const Width, Height: cardinal;
	const internalformat: GLint; const pixeltype: GLenum; const format: GLint;
	const pixels: Pointer): TResourceBuilderType;
var
	tmpmeta: TTextureMeta;
	tmppnt: Pointer;
	pixelsize, tmpword: cardinal;
begin
	if format = GL_RGBA then
		pixelsize := SizeOf(byte) * 4 * Width * Height
	else if format = GL_RGB then
		pixelsize := SizeOf(byte) * 3 * Width * Height
	else
		raise Exception.Create('textureloader unknown pixelformat');

	tmpword := pixelsize + SizeOf(tmpmeta);
	GetMem(tmppnt, tmpword);

	tmpmeta.format := format;
	tmpmeta.internalFormat := internalformat;
	tmpmeta.Width := Width;
	tmpmeta.Height := Height;
	tmpmeta.pixeltype := pixeltype;

	Move(tmpmeta, tmppnt^, SizeOf(tmpmeta));
	Move(pixels^, Pointer(tmppnt + SizeOf(tmpmeta))^, pixelsize);

	Result := grbTexture;

	Store(Name, tmppnt, tmpword);

	FreeMem(tmppnt, tmpword);
end;

function LoadTextureFromCache(const cache: Pointer; const cachesize: cardinal;
	out conttype: TGameResourceType): TObject;
var
	texmeta: TTextureMeta;
	handle: GLuint;
begin
	texmeta := PTextureMeta(cache)^;

	glGenTextures(1, @handle);
	glBindTexture(GL_TEXTURE_2D, handle);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	// Optionaly, set the border color
	//float color[] = { 1.0f, 0.0f, 0.0f, 1.0f };
	//glTexParameterfv( GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, color );

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
	//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);

	if checkGlVersion(4, 2) then
	begin
		glTexStorage2D(GL_TEXTURE_2D, 1, texmeta.internalFormat, texmeta.Width,
			texmeta.Height);
		glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, texmeta.Width, texmeta.Height,
			texmeta.format, GL_UNSIGNED_BYTE, Pointer(cache + SizeOf(TTextureMeta)));
	end
	else
		glTexImage2D(GL_TEXTURE_2D, 0, texmeta.internalFormat, texmeta.Width, texmeta.Height,
			0, texmeta.format, texmeta.pixeltype, Pointer(cache + SizeOf(TTextureMeta)));


	Result := TGameTexture.Create;
	conttype := grtTexture;
	TGameTexture(Result).handle := handle;
end;

function GameTexMesh3Loader(Name: TEngineString; vertexcount: cardinal;
	vertexes: PVec3; texcoords: PVec2; drawtype: GLenum; indexcount: cardinal;
	indexlist: PGLuint): TResourceBuilderType;
var
	tmpmeta: TTexModelMeta;
	tmppnt: Pointer;
	tmpword: cardinal;
begin
	tmpword := vertexcount * (SizeOf(TVec3Coord2)) + SizeOf(tmpmeta) +
		indexcount * SizeOf(indexlist^);
	GetMem(tmppnt, tmpword);

	tmpmeta.drawtype := drawtype;
	tmpmeta.vertexcount := vertexcount;
	tmpmeta.indexcount := indexcount;
	Move(tmpmeta, tmppnt^, SizeOf(tmpmeta));

	interleave(vertexes, texcoords, Pointer(tmppnt + SizeOf(tmpmeta)), vertexcount);

	if indexcount > 0 then
		Move(indexlist^, Pointer(tmppnt + vertexcount * (SizeOf(TVec3Coord2)) +
			SizeOf(tmpmeta))^, indexcount * SizeOf(indexlist^));

	Result := grbTexModel3;
	Store(Name, tmppnt, tmpword);
	FreeMem(tmppnt, tmpword);
end;

function GameTexMesh2Loader(Name: TEngineString; vertexcount: cardinal;
	vertexes, texcoords: PVec2; drawtype: GLenum; indexcount: cardinal;
	indexlist: PGLuint): TResourceBuilderType;
var
	tmpmeta: TTexModelMeta;
	tmppnt: Pointer;
	tmpword: cardinal;
begin
	tmpword := vertexcount * (SizeOf(TVec2Coord2)) + SizeOf(tmpmeta);
	GetMem(tmppnt, tmpword);

	tmpmeta.drawtype := drawtype;
	tmpmeta.vertexcount := vertexcount;
	tmpmeta.indexcount := 0;
	Move(tmpmeta, tmppnt^, SizeOf(tmpmeta));

	interleave(vertexes, texcoords, Pointer(tmppnt + SizeOf(tmpmeta)), vertexcount);

	if indexcount > 0 then
		Move(indexlist^, Pointer(tmppnt + vertexcount * (SizeOf(TVec3Coord2)) +
			SizeOf(tmpmeta))^, indexcount * SizeOf(indexlist^));

	Store(Name, tmppnt, tmpword);
	Result := grbTexModel2;
	FreeMem(tmppnt, tmpword);
end;

function LoadTexMesh3FromCache(const cache: Pointer; const cachesize: cardinal;
	out conttype: TGameResourceType): TObject;
var
	vertexarray, vertexbuffer, indexlist: GLuint;
	vertexbytesize: cardinal;
	modMeta: TTexModelMeta;
begin
	modMeta := PTexModelMeta(cache)^;

	vertexbytesize := modMeta.vertexcount * sizeof(TVec3Coord2);

	glGenVertexArrays(1, @vertexarray);
	glGenBuffers(1, @vertexbuffer);

	glBindVertexArray(vertexarray);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, vertexbytesize,
		Pointer(cache + SizeOf(TTexModelMeta)), GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Coord2), nil);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3Coord2),
		Pointer(Sizeof(TVec3)));

	conttype := grtModel;

	if modMeta.indexcount > 0 then
	begin
		glGenBuffers(1, @indexlist);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexlist);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, modMeta.indexcount * SizeOf(GLuint),
			Pointer(cache + vertexbytesize + SizeOf(TTexModelMeta)), GL_STATIC_DRAW);
	end
	else
		indexlist := 0;

	Result := TGameModel.Create(spTexture, vertexarray, vertexbuffer,
		indexlist, 0, modMeta.drawtype, modMeta.indexcount, modMeta.vertexcount);
end;

function LoadTexMesh2FromCache(const cache: Pointer; const cachesize: cardinal;
	out conttype: TGameResourceType): TObject;
var
	bytesize: longword;
	vertexarray, vertexbuffer, indexlist: GLuint;
	modMeta: TTexModelMeta;
begin
	modMeta := PTexModelMeta(cache)^;

	bytesize := 2 * modmeta.vertexcount * sizeof(TVec2);

	glGenVertexArrays(1, @vertexarray);
	glGenBuffers(1, @vertexbuffer);

	glBindVertexArray(vertexarray);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, bytesize, Pointer(cache + SizeOf(TTexModelMeta)),
		GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec2Coord2), nil);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec2Coord2),
		Pointer(Sizeof(TVec2)));

	if modMeta.indexcount > 0 then
	begin
		glGenBuffers(1, @indexlist);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexlist);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, modMeta.indexcount * SizeOf(GLuint),
			Pointer(cache + bytesize + SizeOf(TTexModelMeta)), GL_STATIC_DRAW);
	end
	else
		indexlist := 0;

	Result := TGameModel.Create(spTexture, vertexarray, vertexbuffer,
		indexlist, 0, modMeta.drawtype, modmeta.indexcount, modMeta.vertexcount);
	conttype := grtModel;
end;

procedure Create2DRect(Name: TEngineString; boundry: TGameBoundry;
	Width, Height: GlFloat);
const
	ftexcoords: array[0..3] of TVec2 =
		((X: 0; Y: 0), (X: 0; Y: 1), (X: 1; Y: 0), (X: 1; Y: 1));
var
	fobj: array[0..3] of TVec3;
begin
	case boundry of
		gbMiddle:
		begin
			fobj[0] := Vec3(-Width / 2, Height / 2, 0);	 //1
			fobj[1] := Vec3(-Width / 2, -Height / 2, 0); //22
			fobj[2] := Vec3(Width / 2, Height / 2, 0);	 //31
			fobj[3] := Vec3(Width / 2, -Height / 2, 0);	 // 2
		end;
		gbBottomMiddle:
		begin
			fobj[0] := Vec3(-Width / 2, Height, 0); //1
			fobj[1] := Vec3(-Width / 2, 0, 0);			//22
			fobj[2] := Vec3(Width / 2, Height, 0);	//31
			fobj[3] := Vec3(Width / 2, 0, 0);				// 2
		end;
		gbTopLeft:
		begin
			fobj[0] := Vec3(0, 0, 0);					//1
			fobj[1] := Vec3(0, -Height, 0);		//22
			fobj[2] := Vec3(Width, 0, 0);			//31
			fobj[3] := Vec3(Width, -Height, 0);// 2
		end
		else
			raise Exception.Create('Invalid boundry for TexObjects ' + IntToStr(Ord(boundry)));
	end;

	GameResourceAdd(GameTexMesh3Loader(Name, Length(fobj), @fobj[0],
		@ftexcoords[0], GL_TRIANGLE_STRIP, 0, nil), Name);
end;

procedure CreateCube(Name: TEngineString; boundry: TGameBoundry; Size: GLfloat);
const
	ftexcoords: array[0..13] of TVec2 =
		((X: 1 / 3; Y: 0), (X: 0; Y: 0), (X: 1 / 3; Y: 1 / 2), (X: 0; Y: 1 / 2),
		(X: 0; Y: 1 / 2), (X: 2 / 3; Y: 1 / 2), (X: 1 / 3; Y: 1), (X: 0;
		Y: 1),
		(X: 1 / 3; Y: 0), (X: 1 / 3; Y: 1 / 2), (X: 0; Y: 0), (X: 0; Y: 1 / 2),
		(X: 1 / 3; Y: 0), (X: 1 / 3; Y: 1 / 2));
var
	fobj: array[0..13] of TVec3;
begin
	case boundry of
		gbMiddle:
		begin
			// Draw three sides facing outwards
			fobj[0] := Vec3(-Size / 2, Size / 2, Size / 2); //1
			fobj[1] := Vec3(-Size / 2, -Size / 2, Size / 2);
			fobj[2] := Vec3(Size / 2, Size / 2, Size / 2);
			fobj[3] := Vec3(Size / 2, -Size / 2, Size / 2);
			// Draw the bottom
			fobj[4] := Vec3(Size / 2, -Size / 2, -Size / 2); //1
			fobj[5] := Vec3(-Size / 2, -Size / 2, Size / 2);
			fobj[6] := Vec3(-Size / 2, -Size / 2, -Size / 2);
			// Draw .. urgh whatever
			fobj[7] := Vec3(-Size / 2, Size / 2, Size / 2);	//2
			fobj[8] := Vec3(-Size / 2, Size / 2, -Size / 2);
			fobj[9] := Vec3(Size / 2, Size / 2, Size / 2);
			fobj[10] := Vec3(Size / 2, Size / 2, -Size / 2);
			fobj[11] := Vec3(Size / 2, -Size / 2, -Size / 2);
			fobj[12] := Vec3(-Size / 2, Size / 2, -Size / 2);
			fobj[13] := Vec3(-Size / 2, -Size / 2, -Size / 2);

			// Side-strip:
			//fobj[0] := Vec3(-Size / 2, Size / 2, Size / 2); //1
			//fobj[1] := Vec3(-Size / 2, -Size / 2, Size / 2);//22
			//fobj[2] := Vec3(Size / 2, Size / 2, Size / 2);  //311
			//fobj[3] := Vec3(Size / 2, -Size / 2, Size / 2);   //322
			//fobj[4] := Vec3(Size / 2, Size / 2, -Size / 2);    //311
			//fobj[5] := Vec3(Size / 2, -Size / 2, -Size / 2);   //322
			//fobj[6] := Vec3(-Size / 2, Size / 2, -Size / 2);    //311
			//fobj[7] := Vec3(-Size / 2, -Size / 2, -Size / 2);     //322
			//fobj[8] := Vec3(-Size / 2, Size / 2, Size / 2);        //311
			//fobj[9] := Vec3(-Size / 2, -Size / 2, Size / 2);       //32
		end;
		else
			raise Exception.Create('Unsupported boundry for TexObjects ' +
				IntToStr(Ord(boundry)));
	end;

	GameResourceAdd(GameTexMesh3Loader(Name, Length(fobj), @fobj[0],
		@ftexcoords[0], GL_TRIANGLE_STRIP, 0, nil), Name);
end;

procedure Create2DGUIRect(Name: TEngineString; boundry: TGameBoundry;
	x, y, Width, Height: GlFloat);
const
	ftexcoords: array[0..3] of TVec2 =
		((X: 0; Y: 0), (X: 0; Y: 1), (X: 1; Y: 0), (X: 1; Y: 1));
var
	fobj: array[0..3] of TVec2;
begin
	case boundry of
		gbMiddle:
		begin
			fobj[0] := Vec2(x + Width / 2, y - Height / 2); //1
			fobj[1] := Vec2(x + Width / 2, y + Height / 2); //22
			fobj[2] := Vec2(x - Width / 2, y - Height / 2); //31
			fobj[3] := Vec2(x - Width / 2, y + Height / 2);	// 2
		end;
		gbBottomMiddle:
		begin
			fobj[0] := Vec2(x - Width / 2, y + Height); //1
			fobj[1] := Vec2(x - Width / 2, y);					//22
			fobj[2] := Vec2(x + Width / 2, y + Height);	//31
			fobj[3] := Vec2(x + Width / 2, y);					// 2
		end;
		gbTopLeft:
		begin
			fobj[0] := Vec2(x, y + Height);									//1
			fobj[1] := Vec2(x, y);					//22
			fobj[2] := Vec2(x + Width, y + Height);					//31
			fobj[3] := Vec2(x + Width, y);	// 2
		end
		else
			raise Exception.Create('Invalid boundry for TexObjects ' + IntToStr(Ord(boundry)));
	end;

	GameResourceAdd(GameTexMesh2Loader(Name, Length(fobj), @fobj[0],
		@ftexcoords[0], GL_TRIANGLE_STRIP, 0, nil), Name);
end;

initialization
	SetLoader(grbTexture, @LoadTextureFromCache);
	SetLoader(grbTexModPair, @LoadTexModFromCache);
	SetLoader(grbTexModel3, @LoadTexMesh3FromCache);
	SetLoader(grbTexModel2, @LoadTexMesh2FromCache);
	SetLoader(grbTextureFile, @LoadTextureFileFromCache);
end.
