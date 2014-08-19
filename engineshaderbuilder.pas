unit EngineShaderBuilder;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, typinfo, dglOpenGL;

type
	EGameShaderCreationException = Exception;

	TLine = array of char;
	TLineArray = array of TLine;
	TLineLengthArray = array of GLint;

	TEngineShaderType = (stVertex, stFragment, stGeometry, stTessellation);

	TShaderVarDecl = record
		typ, Name, lmod: string;
	end;

	TUniformBlock = record
		Name: string;
		vars: array of TShaderVarDecl;
	end;

	{ TShaderBuilder }

	TShaderBuilder = class
	private
		version: word;
		built: boolean;
		outdecls, indecls, unidecls, globaldecls: array of TShaderVarDecl;
		instrs: TStringList;
		uniBlocks: array of TUniformBlock;
		shadertype: TEngineShaderType;
		PLengths: TLineLengthArray;
		PLines: TLineArray;
		procedure FreeCurrentBuild;
	public
		constructor Create(nshadertype: TEngineShaderType);
		destructor Destroy; override;
		function Build: boolean;
		procedure AddInputDecl(typ, Name: string; layout: String = '');
		procedure AddOutputDecl(typ, Name: string; layout: String = '');
		procedure AddGlobalDec(typ, Name: string);
		procedure AddUniformDec(typ, Name: string);
		procedure AddUniformBlockDec(Name: string; defs: array of TShaderVarDecl);
		procedure AddMainInstruction(instr: string);
		function InPrefix: string;
		function OutPrefix: string;
		function GlTypeEnum: GLenum;
		property Lines: TLineArray read PLines;
		property Lengths: TLineLengthArray read PLengths;
		property shType: TEngineShaderType read shadertype;
	end;

function ShaderVarDecl(typ, Name: string): TShaderVarDecl;

implementation

function ShaderVarDecl(typ, Name: string): TShaderVarDecl;
begin
	Result.Name := Name;
	Result.typ := Typ;
end;

{ TShaderBuilder }

procedure TShaderBuilder.FreeCurrentBuild;
var
	zhlr: integer;
begin
	if built then
	begin
		for zhlr := 0 to Length(PLines) - 1 do
			SetLength(PLines[zhlr], 0);
		SetLength(PLines, 0);
		SetLength(PLengths, 0);
		built := False;
	end;
end;

constructor TShaderBuilder.Create(nshadertype: TEngineShaderType);
begin
	inherited Create;
	built := False;
	shadertype := nshadertype;
	PLines := nil;
	PLengths := nil;
	version := 140;

	SetLength(indecls, 0);
	SetLength(outdecls, 0);
	SetLength(unidecls, 0);
	SetLength(uniBlocks, 0);
	SetLength(globaldecls, 0);

	instrs := TStringList.Create;
end;

destructor TShaderBuilder.Destroy;
var
	tmpblock: TUniformBlock;
begin
	FreeCurrentBuild;
	for tmpblock in uniBlocks do
		SetLength(tmpblock.vars, 0);

	SetLength(indecls, 0);
	SetLength(outdecls, 0);
	SetLength(unidecls, 0);
	SetLength(uniBlocks, 0);
	SetLength(globaldecls, 0);
	FreeAndNil(instrs);
	inherited Destroy;
end;

function TShaderBuilder.Build: boolean;
var
	tmplist: TStringList;
	tmpstr: string;
	tmpdecl: TShaderVarDecl;
	tmpblock: TUniformBlock;
	zhlr: integer;
begin
	if built then
		FreeCurrentBuild;

	tmplist := TStringList.Create;

	tmplist.Add('#version ' + IntToStr(version));

  if (Length(uniBlocks) > 0) and (version < 140) then
    tmplist.Add('#extension GL_ARB_uniform_buffer_object : enable');

	for tmpblock in uniBlocks do
	begin
		tmplist.Add('uniform ' + tmpblock.Name + ' {');
		for tmpdecl in tmpblock.vars do
			tmplist.Add(tmpdecl.typ + ' ' + tmpdecl.Name + ';');
		tmplist.Add('};');
	end;


	for tmpdecl in indecls do
		tmplist.Add(tmpdecl.lmod + ' ' + InPrefix + ' ' + tmpdecl.typ + ' ' + tmpdecl.Name
      + ';');

	for tmpdecl in outdecls do
		tmplist.Add(tmpdecl.lmod + ' ' + OutPrefix + ' ' + tmpdecl.typ + ' ' + tmpdecl.Name
      + ';');

	for tmpdecl in unidecls do
		tmplist.Add('uniform  ' + tmpdecl.typ + ' ' + tmpdecl.Name + ';');

	for tmpdecl in globaldecls do
		tmplist.Add(tmpdecl.typ + ' ' + tmpdecl.Name + ';');


	tmplist.Add('void main()');
	tmplist.Add('{');

	for tmpstr in instrs do
		tmplist.add(tmpstr);

	tmplist.Add('}');

	// Init arrays for lines and lengths
	SetLength(PLines, tmplist.Count);
	SetLength(PLengths, tmplist.Count);
	for zhlr := 0 to tmplist.Count - 1 do
	begin
		tmpstr := tmplist[zhlr] + #10;
		// Set length in array
		PLengths[zhlr] := Length(tmpstr);
		SetLength(PLines[zhlr], PLengths[zhlr]);
		// Copy chars from StringList to array
		strlcopy(@(PLines[zhlr][0]), PChar(tmpstr), PLengths[zhlr]);
	end;

	built := True;

	Result := True;
	if version = 140 then
		version := 130
	else if version = 130 then
		version := 120
	else
		Result := False;
end;

procedure TShaderBuilder.AddInputDecl(typ, Name: string; layout: String);
begin
	SetLength(indecls, Length(indecls) + 1);
	indecls[Length(indecls) - 1].Name := Name;
	indecls[Length(indecls) - 1].typ := typ;
  if layout <> '' then
    indecls[Length(indecls) - 1].lmod := 'layout(location = ' + layout + ')'
  else
    indecls[Length(indecls) - 1].lmod := '';
end;

procedure TShaderBuilder.AddOutputDecl(typ, Name: string; layout: String);
begin
	SetLength(outdecls, Length(outdecls) + 1);
	outdecls[Length(outdecls) - 1].Name := Name;
	outdecls[Length(outdecls) - 1].typ := typ;
  if layout <> '' then
    outdecls[Length(outdecls) - 1].lmod := 'layout(location = ' + layout + ')'
  else
    outdecls[Length(outdecls) - 1].lmod := '';
end;

procedure TShaderBuilder.AddGlobalDec(typ, Name: string);
begin
	SetLength(globaldecls, Length(globaldecls) + 1);
	globaldecls[Length(globaldecls) - 1].Name := Name;
	globaldecls[Length(globaldecls) - 1].typ := typ;
end;

procedure TShaderBuilder.AddUniformDec(typ, Name: string);
begin
	SetLength(unidecls, Length(unidecls) + 1);
	unidecls[Length(unidecls) - 1].Name := Name;
	unidecls[Length(unidecls) - 1].typ := typ;
end;

procedure TShaderBuilder.AddUniformBlockDec(Name: string; defs: array of TShaderVarDecl);
begin
	SetLength(uniBlocks, Length(uniBlocks) + 1);
	uniBlocks[Length(uniBlocks) - 1].Name := Name;
	SetLength(uniBlocks[Length(uniBlocks) - 1].vars, Length(defs));
	Move((@defs[0])^, (@uniBlocks[Length(uniBlocks) - 1].vars[0])^,
		Length(defs) * SizeOf(TShaderVarDecl));
end;

procedure TShaderBuilder.AddMainInstruction(instr: string);
begin
	instrs.Add(instr);
end;

function TShaderBuilder.InPrefix: string;
begin
	if version > 120 then
		Result := 'in'
	else
	begin
		if shadertype = stVertex then
			Result := 'attribute'
		else if shadertype = stFragment then
			Result := 'varying'
		else
			raise EGameShaderCreationException.Create('Ivalid Shader type ' +
				GetEnumName(TypeInfo(TEngineShaderType), integer(shadertype)) +
				' with ''in'' decl');
	end;
end;

function TShaderBuilder.OutPrefix: string;
begin
	if version > 120 then
		Result := 'out'
	else
	begin
		if shadertype = stVertex then
			Result := 'varying'
		else
			raise EGameShaderCreationException.Create('Ivalid Shader type ' +
				GetEnumName(TypeInfo(TEngineShaderType), integer(shadertype)) +
				' with ''out'' decl');
	end;
end;

function TShaderBuilder.GlTypeEnum: GLenum;
begin
	// Pick glenum for shadertype
	case shadertype of
		stFragment: Result := GL_FRAGMENT_SHADER;
		stVertex: Result := GL_VERTEX_SHADER;
		stGeometry: Result := GL_GEOMETRY_SHADER;
		stTessellation: Result := GL_TESS_CONTROL_SHADER;
		else
			raise EGameShaderCreationException.Create('Unknown shader type' +
				GetEnumName(TypeInfo(TEngineShaderType), integer(shadertype)));
	end;
end;

end.
