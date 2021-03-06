unit EngineObject;

{$H+}

interface

uses
	Classes, SysUtils, EngineTypes, dglOpenGL, EngineResource, EngineShader,
	EngineResourceLoader, EngineStrings;

type

	PGameModelRepr = ^TGameModelRepr;

	TGameModelRepr = record
		vertexarray, indexlist, textureHandle: GLuint;
		drawType: GLenum;
		indexcount, vertexcount: word;
		programtype: TShaderProgramType;
	end;

	{ TGameModel }

	TGameModel = class(TEngineResourceUser)
		programtype: TShaderProgramType;
		vertexarray, vertexbuffer, indexlist, textureHandle: GLuint;
		drawType: GLenum;
		indexcount, vertexcount: word;
		constructor Create;
		constructor Create(nprogramtype: TShaderProgramType;
			nvertexarray, nvertexbuffer, nindexlist, ntextureHandle: GLuint;
			ndrawType: GLenum; nindexcount, nvertexcount: word);
		destructor Destroy; override;
		function GetRepresentation: TGameModelRepr;
	end;

	{ TDrawableObject }

	TDrawableObject = object
		gMesh: TGameModelRepr;
		rota: TGameRotation;
		pos: TGamePosition;

		constructor Create(model: TGameModelRepr; rotation: TGameRotation;
			position: TGamePosition);
		procedure Draw;
		procedure rotateXZ(amount: GLfloat); virtual;
		procedure rotateY(amount: GLfloat); virtual;
	end;


procedure PrepareShader(repr: PGameModelRepr); register; inline;
procedure DrawModel(repr: PGameModelRepr); register; inline;

function ModelName(const s: string): TResourceName;

implementation

function ModelName(const s: string): TResourceName;
begin
	Result := EngineString('model.' + s);
end;

type
	TModelReprProc = procedure(repr: PGameModelRepr);

procedure HasNoTexture({%H-}repr: PGameModelRepr);
begin

end;

procedure HasTexture(repr: PGameModelRepr);
begin
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, repr^.textureHandle);
	activeProgram.SetTexture(0);
end;

procedure NonIndexedModelDraw(repr: PGameModelRepr);
begin
	glBindVertexArray(repr^.vertexarray);
	glDrawArrays(repr^.drawType, 0, repr^.vertexcount);
end;

procedure IndexedModelDraw(repr: PGameModelRepr);
begin
	glBindVertexArray(repr^.vertexarray);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, repr^.indexlist);
	glDrawElements(repr^.drawType, repr^.indexcount, GL_UNSIGNED_INT, nil);
end;

const
	BindTextureLookup: array[boolean] of TModelReprProc = (@HasNoTexture, @HasTexture);
	RenderLookup: array[boolean] of
		TModelReprProc = (@NonIndexedModelDraw, @IndexedModelDraw);

procedure PrepareShader(repr: PGameModelRepr);
begin
	GetProgram(repr^.programtype).Use;
	BindTextureLookup[repr^.textureHandle <> 0](repr);
end;

procedure DrawModel(repr: PGameModelRepr);
begin
	RenderLookup[repr^.indexcount <> 0](repr);
end;

{ TGameModel }

constructor TGameModel.Create;
begin
	inherited;
end;

constructor TGameModel.Create(nprogramtype: TShaderProgramType;
	nvertexarray, nvertexbuffer, nindexlist, ntextureHandle: GLuint;
	ndrawType: GLenum; nindexcount, nvertexcount: word);
begin
	inherited Create;
	programtype := nprogramtype;
	vertexarray := nvertexarray;
	vertexbuffer := nvertexbuffer;
	indexlist := nindexlist;
	textureHandle := ntextureHandle;
	drawType := ndrawType;
	indexcount := nindexcount;
	vertexcount := nvertexcount;
end;

destructor TGameModel.Destroy;
begin
	if vertexbuffer <> 0 then
		glDeleteBuffers(1, @vertexbuffer);
	glDeleteVertexArrays(1, @vertexarray);

	inherited Destroy;
end;

function TGameModel.GetRepresentation: TGameModelRepr;
begin
	Result.drawType := drawType;
	Result.vertexarray := vertexarray;
	Result.vertexcount := vertexcount;
	Result.indexcount := indexcount;
	Result.indexlist := indexlist;
	Result.textureHandle := textureHandle;
	Result.programtype := programtype;
end;

{ TDrawableObject }

procedure TDrawableObject.Draw;
begin
	glPushMatrix;
	PrepareShader(@gMesh);
	glTranslatef(pos.offset.X, pos.offset.Y, pos.offset.Z);
	glRotatef(RadToDeg(rota.xzangle), 0, -1, 0);
	glRotatef(RadToDeg(rota.yangle), 1, 0, 0);
	EngineShader.SetRenderOffset(pos.worldPos);
	DrawModel(@gMesh);
	glPopMatrix;
end;

constructor TDrawableObject.Create(model: TGameModelRepr; rotation: TGameRotation;
	position: TGamePosition);
begin
	gMesh := model;
	rota := rotation;
	pos := position;
end;

procedure TDrawableObject.rotateXZ(amount: GLfloat);
begin
	EngineTypes.Rotate(amount, @rota);
end;

procedure TDrawableObject.rotateY(amount: GLfloat);
begin
	EngineTypes.RotateClamped(amount, @rota);
end;

end.
