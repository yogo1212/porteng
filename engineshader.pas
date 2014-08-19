unit EngineShader;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

{$UNDEF USEDEPTHACCU}
{$DEFINE DISCARDNOALPHA}

interface

//{$UNDEF ENGINEDEBUG}
uses
	Classes, SysUtils, dglOpenGL, EngineFacilities, EngineShaderBuilder, EngineTypes;

type

	TShaderProgramType = (spColour, spTexture, spColourGUI, spTextureGUI, spMap);

	{ TGameShader }

	// Shader-build funcs

	TGameShader = object
		id: GLHandle;
		compiled: boolean;
		shadertype: TEngineShaderType;
		error: string;
		constructor Create(shaderName: string; builder: TShaderBuilder);
		destructor Destroy;
	end;

	{ TGameProgram }

	TGameProgram = class
	protected
		id: GLHandle;
		shaders: array[TEngineShaderType] of TGameShader;
		used: array[TEngineShaderType] of boolean;
		Name: string;
	public
		// Texture-aware Programs override this:
		procedure SetTexture(nId: GLint); virtual; abstract;

		procedure Use;

		procedure Link; virtual;
		procedure AddShader(shader: TGameShader);
		constructor Create(programName: string);
		destructor Destroy; override;
	end;

	{ TGameTextureProgram }

	TGameTextureProgram = class(TGameProgram)
	protected
		texpos, texid: GLint;
	public
		procedure Link; override;
		procedure SetTexture(nId: GLint); override;
	end;

var
	activeProgram: TGameProgram = TGameProgram(nil);

procedure GameColorShaderInit;
procedure GameTextureShaderInit;
procedure GameTextureGUIShaderInit;
procedure GameColourGUIShaderInit;
procedure GameMapShaderInit;

function GetProgram(programtype: TShaderProgramType): TGameProgram; inline;

procedure SetRenderOffset(offs: TVec3i);
procedure SetViewOffset(offs: TVec3i);

implementation

var
	initialised: boolean = False;
	programs: array[TShaderProgramType] of TGameProgram = (nil, nil, nil, nil, nil);

function createVertexShaderColour: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stVertex);

	builder.AddUniformBlockDec('OffsetBlock', (ShaderVarDecl('ivec4', 'relWorldOffset')));

	builder.AddInputDecl('vec4', 'shit');
	builder.AddInputDecl('vec4', 'incol');

	builder.AddOutputDecl('vec4', 'colour');

	builder.AddMainInstruction('colour = incol;');
	builder.AddMainInstruction('gl_Position = gl_ModelViewProjectionMatrix');
	builder.AddMainInstruction(' * (shit + relWorldOffset);');
{$IFDEF DISCARDNOALPHA}
	builder.AddMainInstruction('if (incol.w == 0.0)');
	builder.AddMainInstruction('gl_Position.w = 0.0;');
{$ENDIF}

	Result.Create('VertexShaderColour', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

function createVertexShaderMap: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stVertex);

	builder.AddUniformBlockDec('OffsetBlock', (ShaderVarDecl('ivec4', 'relWorldOffset')));

	builder.AddInputDecl('vec3', 'shit');
	builder.AddInputDecl('vec3', 'position');
	builder.AddInputDecl('vec4', 'incol');
	builder.AddInputDecl('float', 'size');

	builder.AddOutputDecl('vec4', 'colour');

	builder.AddMainInstruction('gl_Position = gl_ModelViewProjectionMatrix');
	builder.AddMainInstruction(' * (vec4(shit * size + position, 1) + relWorldOffset);');

	builder.AddMainInstruction('colour = incol;');

{$IFDEF DISCARDNOALPHA}
	builder.AddMainInstruction('if (incol.w == 0.0)');
	builder.AddMainInstruction('gl_Position.w = 0.0;');
{$ENDIF}

	ReadExtensions;
	Result.Create('VertexShader', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

function createVertexShaderColourGui: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stVertex);

	builder.AddInputDecl('vec2', 'shit');
	builder.AddInputDecl('vec4', 'col');

	builder.AddOutputDecl('vec4', 'colour');

	builder.AddMainInstruction('colour = col;');
	builder.AddMainInstruction('gl_Position = vec4(shit, 0, 1);');

{$IFDEF DISCARDNOALPHA}
	builder.AddMainInstruction('if (col.w == 0.0)');
	builder.AddMainInstruction('gl_Position.w = 0.0;');
{$ENDIF}

	Result.Create('VertexShaderColourGUI', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

function createVertexShaderTextureGui: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stVertex);

	builder.AddInputDecl('vec2', 'shit');
	builder.AddInputDecl('vec2', 'coord');

	builder.AddOutputDecl('vec2', 'texcoord');

	builder.AddMainInstruction('texcoord = coord;');
	builder.AddMainInstruction('gl_Position = vec4(shit, 0, 1);');

	Result.Create('VertexShaderTextureGUI', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

function createVertexShaderTexture: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stVertex);

	builder.AddUniformBlockDec('OffsetBlock', (ShaderVarDecl('ivec4', 'relWorldOffset')));

	builder.AddInputDecl('vec4', 'shit');
	builder.AddInputDecl('vec2', 'coord');

	builder.AddOutputDecl('vec2', 'texcoord');

	builder.AddMainInstruction('texcoord = coord;');
	builder.AddMainInstruction('gl_Position = gl_ModelViewProjectionMatrix');
	builder.AddMainInstruction(' * (shit + relWorldOffset);');

	Result.Create('VertexShaderTexture', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

{$IFDEF USEDEPTHACCU}
procedure AddFragmentOutputDecls(builder: TShaderBuilder);
begin
	builder.AddOutputDecl('vec4', 'col1', '0');
	builder.AddOutputDecl('vec4', 'col2', '1');
	builder.AddOutputDecl('vec4', 'col3', '2');
	builder.AddOutputDecl('vec4', 'col4', '3');
end;
{$ENDIF}

function createFragmentShaderColour: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stFragment);
	builder.AddInputDecl('vec4', 'colour');
	// there was a 'smooth' here

{$IFDEF USEDEPTHACCU}
	AddFragmentOutputDecls(builder);
{$ENDIF}

	builder.AddMainInstruction('gl_FragColor = colour;');

	Result.Create('FragmentShaderColour', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

function createFragmentShaderTexture: TGameShader;
var
	builder: TShaderBuilder;
begin
	builder := TShaderBuilder.Create(stFragment);
	builder.AddUniformDec('sampler2D', 'tex');
	builder.AddInputDecl('vec2', 'texcoord');
{$IFDEF USEDEPTHACCU}
	AddFragmentOutputDecls(builder);
{$ENDIF}

	builder.AddMainInstruction('gl_FragColor = texture2D(tex, texcoord);');

{$IFDEF DISCARDNOALPHA}
	builder.AddMainInstruction('if (gl_FragColor.w == 0.0)');
	builder.AddMainInstruction('discard;');
{$ENDIF}

	Result.Create('FragmentShaderTexture', builder);

	if Result.compiled <> True then
		raise Exception.Create(Result.error);

	FreeAndNil(builder);
end;

const
	offsetBindingPoint = 1;

// this is for translating between sections of the virtual world:
var
	renderOffset, viewOffset: TVec3i;
	offsetBuffer: GLint;

procedure UpdateOffset;
var
	diff: TVec3i;
begin
	diff := (renderOffset - viewOffset) * 512;
	glBindBuffer(GL_UNIFORM_BUFFER, offsetBuffer);
	glBufferSubData(GL_UNIFORM_BUFFER, 0, SizeOf(diff), @diff);
end;

procedure SetRenderOffset(offs: TVec3i);
begin
	if offs <> renderOffset then
	begin
		renderOffset := offs;
		UpdateOffset;
	end;
end;

procedure SetViewOffset(offs: TVec3i);
begin
	if offs <> viewOffset then
	begin
		viewOffset := offs;
		UpdateOffset;
	end;
end;

procedure GameShaderFree;
begin
	if initialised then
	begin
		if programs[spColour] <> nil then
			FreeAndNil(programs[spColour]);
		if programs[spTexture] <> nil then
			FreeAndNil(programs[spTexture]);
		if programs[spColourGUI] <> nil then
			FreeAndNil(programs[spColourGUI]);
		if programs[spTextureGUI] <> nil then
			FreeAndNil(programs[spTextureGUI]);
		if programs[spMap] <> nil then
			FreeAndNil(programs[spMap]);
	end;
end;

procedure GameShaderInit;
var
	tmpvec4: TVec4;
begin
	if not initialised then
	begin
		initialised := True;

		AddFreeRoutine(@GameShaderFree);

		glGenBuffers(1, @offsetBuffer);
		glBindBuffer(GL_UNIFORM_BUFFER, offsetBuffer);

		tmpvec4 := Vec4(0, 0, 0, 0);
		glBufferData(GL_UNIFORM_BUFFER, sizeof(tmpvec4), @tmpvec4, GL_DYNAMIC_DRAW);
		glBindBufferBase(GL_UNIFORM_BUFFER, offsetBindingPoint, offsetBuffer);
	end;
end;

procedure GameColourGUIShaderInit;
begin
	if programs[spMap] = nil then
	begin
		if not initialised then
		begin
			GameShaderInit;
		end;
		programs[spColourGUI] := TGameProgram.Create('Coloured Map GlProgram');
		programs[spColourGUI].AddShader(createVertexShaderColourGui);
		programs[spColourGUI].AddShader(createFragmentShaderColour);
		programs[spColourGUI].Link;
	end;
end;

procedure GameMapShaderInit;
begin
	if programs[spMap] = nil then
	begin
		if not initialised then
		begin
			GameShaderInit;
		end;
		programs[spMap] := TGameProgram.Create('Coloured Map GlProgram');
		programs[spMap].AddShader(createVertexShaderMap);
		programs[spMap].AddShader(createFragmentShaderColour);
		programs[spMap].Link;
	end;
end;

procedure GameColorShaderInit;
begin
	if programs[spColour] = nil then
	begin
		if not initialised then
		begin
			GameShaderInit;
		end;
		programs[spColour] := TGameProgram.Create('Colored GlProgram');
		programs[spColour].AddShader(createVertexShaderColour);
		programs[spColour].AddShader(createFragmentShaderColour);
		programs[spColour].Link;
	end;
end;

procedure GameTextureShaderInit;
begin
	if programs[spTexture] = nil then
	begin
		if not initialised then
		begin
			GameShaderInit;
		end;
		programs[spTexture] := TGameTextureProgram.Create('Textured GlProgram');
		programs[spTexture].AddShader(createVertexShaderTexture);
		programs[spTexture].AddShader(createFragmentShaderTexture);
		programs[spTexture].Link;
	end;
end;

procedure GameTextureGUIShaderInit;
begin
	if programs[spTextureGUI] = nil then
	begin
		if not initialised then
		begin
			GameShaderInit;
		end;
		programs[spTextureGUI] := TGameTextureProgram.Create('GUI GlProgram');
		programs[spTextureGUI].AddShader(createVertexShaderTextureGui);
		programs[spTextureGUI].AddShader(createFragmentShaderTexture);
		programs[spTextureGUI].Link;
	end;
end;

function GetProgram(programtype: TShaderProgramType): TGameProgram;
begin
	Result := programs[programtype];
end;

procedure ProgramIsNotActive(glProgram: TGameProgram);
begin
	glUseProgram(glProgram.id);
	activeProgram := glProgram;
end;

procedure ProgramIsActive({%H-}glProgram: TGameProgram);
begin
	// Nothing to do
end;

type
	TProgramProc = procedure(glProgram: TGameProgram);

const
	actProgLookupTbl: array[boolean] of TProgramProc =
		(@ProgramIsActive, @ProgramIsNotActive);

procedure enforceProgram(glProgram: TGameProgram); inline;
begin
	actProgLookupTbl[activeProgram <> glProgram](glProgram);
end;

{ TGameTextureProgram }

procedure TGameTextureProgram.SetTexture(nId: GLint);
begin
	if texid <> nId then
	begin
		glUniform1i(texpos, nId);
		texid := nId;
	end;
end;

procedure TGameTextureProgram.Link;
begin
	inherited Link;
	texpos := glGetUniformLocation(id, PChar('tex'));
	glUniform1i(texpos, 0);
	texid := 0;
end;

{ TGameProgram }

procedure TGameProgram.AddShader(shader: TGameShader);
begin
	if used[shader.shadertype] = False then
	begin
		if shader.compiled then
		begin
			used[shader.shadertype] := True;
			shaders[shader.shadertype] := shader;
			glAttachShader(id, shader.id);
		end
		else
			raise Exception.Create('Trying to add uncompiled shader to ' + Name);
	end
	else
		raise Exception.Create(Name + ' cant hold more shaders');
end;

procedure TGameProgram.Link;
var
	status: GLint;
	errorstr: array of char;
	errorlen: integer;
begin
	glLinkProgram(id);
	glGetProgramiv(id, GL_LINK_STATUS, @status);
	if status <> GL_TRUE then
	begin
		glGetProgramiv(id, GL_INFO_LOG_LENGTH, @errorlen);
		SetLength(errorstr, errorlen);
		glGetProgramInfoLog(id, Length(errorstr), @errorlen, @errorstr[0]);

		raise Exception.Create('Error in glProgramm ' + Name + LineEnding + PChar(errorstr));
	end;
	Use;
	if glGetUniformBlockIndex(id, 'OffsetBlock') <> GL_INVALID_INDEX then
		glUniformBlockBinding(id, glGetUniformBlockIndex(id, 'OffsetBlock'),
			offsetBindingPoint);
end;

procedure TGameProgram.Use;
begin
	enforceProgram(Self);
end;

constructor TGameProgram.Create(programName: string);
var
	iterator: TEngineShaderType;
begin
	inherited Create;
	id := glCreateProgram();
	if id <> 0 then
	begin
		Name := programName;
		for	iterator in TEngineShaderType do
			used[iterator] := False;
	end
	else
		raise Exception.Create('Failed to create glProgram (usually context was not' +
			'created properly or not made current)');
end;

destructor TGameProgram.Destroy;
var
	iterator: TEngineShaderType;
begin
	glDeleteProgram(id);
	for	iterator in TEngineShaderType do
	begin
		if used[iterator] then
		begin
			glDetachShader(id, shaders[iterator].id);
			shaders[iterator].Destroy;
		end;
	end;
	inherited Destroy;
end;

{ TGameShader }

constructor TGameShader.Create(shaderName: string; builder: TShaderBuilder);

	function GetShaderCode: string;
	var
		linecounter: integer;
		tmpstr: string;
	begin
		Result := '';
		linecounter := 0;
		while linecounter < (Length(builder.Lines) - 1) do
		begin
			SetLength(tmpstr, builder.Lengths[linecounter]);
			Move((builder.Lines[linecounter][0]), (@tmpstr[1])^, builder.Lengths[linecounter]);
			Inc(linecounter);
			Result := Result + tmpstr;

		end;
		SetLength(tmpstr, 0);
	end;

var
	res: integer;
	errorStr: TLine;
	more: boolean;
begin
	// Create shader
	id := glCreateShader(builder.GlTypeEnum);
	if id <> 0 then
	begin
		shadertype := builder.shType;
		more := True;
		repeat
			more := builder.Build;
			// The arrays will be copied
			glShaderSource(id, Length(builder.Lines), Pointer(builder.Lines),
				Pointer(builder.Lengths));
			glCompileShader(id);
			compiled := True;

			glGetShaderiv(id, GL_COMPILE_STATUS, @res);
			if res = GL_FALSE then
			begin
				compiled := False;
				glGetShaderiv(id, GL_INFO_LOG_LENGTH, @res);
				SetLength(errorstr, res);
				glGetShaderInfoLog(id, res, res, @errorStr[0]);
				error := 'Shader "' + shaderName + '" did not compile: ' +
					LineEnding + string(PChar(@errorStr[0])) + LineEnding + GetShaderCode;
				SetLength(errorStr, 0);
			end;
		until compiled or not more;
	end
	else
	begin
		compiled := False;
		error := IntToHex(glGetError(), 8);
		glGetShaderiv(id, GL_INFO_LOG_LENGTH, @res);
		SetLength(errorstr, res);
		glGetShaderInfoLog(id, res, res, @errorStr[0]);
		error := 'Shader "' + shaderName + '" did not compile: ' + error +
			LineEnding + string(PChar(@errorStr[0])) + LineEnding + GetShaderCode;
		SetLength(errorStr, 0);
	end;
end;

destructor TGameShader.Destroy;
begin
	glDeleteShader(id);
end;

end.
