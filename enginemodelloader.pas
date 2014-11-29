unit EngineModelLoader;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineTypes, EngineResourceTexture,
    EngineResourceLoader;

implementation

function LoadModel(filepath: String; var rpath: string): TResourceBuilderType; forward;
function LoadModelFromObj(filepath: String; var rPath: string): TResourceBuilderType;
  forward;

function LoadModel(filepath: String; var rpath: string): TResourceBuilderType;
var
	done: boolean;
begin
	done := False;
  raise Exception.Create('unimpl');
	// Take a look at the header first

	// Then guess the content type from the file name
	if (not done) and (Length(filepath) > 3) then
	begin
		if Copy(filepath, Length(filepath) - 3, 3) = 'obj' then
			Result := LoadModelFromObj(filepath, rpath)
		else
			raise Exception.Create('Unknown model-filetype "' + filepath + '"');
	end
	else
		raise Exception.Create('Unknown model-filetype for "' + filepath + '"');
end;

function LoadModelFromObj(filepath: String; var rPath: string): TResourceBuilderType;
const
	MAXLEN = 5600;
var
	dafile: TextFile;
	tmppos: integer;
	line, tmp1, tmp2, tmp3: string;
	vectorbuffer: array of TVec3;
	normalbuffer: array of TVec3;
	texcoordbuffer: array of TVec2;
	vIbuffer: array of word;
	vtIbuffer: array of word;
	vnIbuffer: array of word;
	outcnt, indexcnt, veccnt, normcnt, txccnt, zhlr, outsize: word;

	outbuffer: array of byte;

	usesnormals, usestxccrd: boolean;

	procedure AddIndex(bum: string);
	begin
		tmppos := Pos('/', bum);
		if tmppos = 0 then
		begin
			usestxccrd := False;
			vIbuffer[indexcnt] := StrToInt(bum) - 1;
		end
		else
		begin
			vIbuffer[indexcnt] := StrToInt(Copy(bum, 1, tmppos)) - 1;
			Delete(bum, 1, tmppos);
			tmppos := Pos('/', bum);
			if tmppos = 0 then
			begin
				usesnormals := False;
				vtIbuffer[indexcnt] := StrToInt(bum) - 1;
			end
			else if tmppos = 1 then
			begin
				usestxccrd := False;
				Delete(bum, 1, 1);
				vnIbuffer[indexcnt] := StrToInt(bum) - 1;
			end
			else
			begin
				vtIbuffer[indexcnt] := StrToInt(Copy(bum, 1, tmppos)) - 1;
				Delete(bum, 1, tmppos);
				vnIbuffer[indexcnt] := StrToInt(bum) - 1;
			end;
		end;
		Inc(indexcnt);
	end;

begin
  raise Exception.Create('unimpl');
	Assign(dafile, filepath);
	Reset(dafile);

	veccnt := 0;
	normcnt := 0;
	txccnt := 0;
	indexcnt := 0;
	outcnt := 0;

	SetLength(vectorbuffer, MAXLEN);
	SetLength(normalbuffer, MAXLEN);
	SetLength(texcoordbuffer, MAXLEN);
	SetLength(vIbuffer, MAXLEN);
	SetLength(vtIbuffer, MAXLEN);
	SetLength(vnIbuffer, MAXLEN);

	usesnormals := True;
	usestxccrd := True;

	while not EOF(dafile) do
	begin
		Readln(dafile, line);

		case line[1] of
			'v':
			begin
				case line[2] of
					' ':
					begin
						Delete(line, 1, 2);
						tmppos := Pos(' ', line);
						vectorbuffer[veccnt].X := StrToFloat(Copy(line, 1, tmppos));
						Delete(line, 1, tmppos);
						tmppos := Pos(' ', line);
						vectorbuffer[veccnt].Y := StrToFloat(Copy(line, 1, tmppos));
						Delete(line, 1, tmppos);
						vectorbuffer[veccnt].Z := StrToFloat(line);
						Inc(veccnt);
						if veccnt = MAXLEN then
							raise Exception.Create('vecBuffer too small');
					end;
					't':
					begin
						Delete(line, 1, 3);
						tmppos := Pos(' ', line);
						texcoordbuffer[txccnt].X := StrToFloat(Copy(line, 1, tmppos));
						Delete(line, 1, tmppos);
						texcoordbuffer[txccnt].Y := StrToFloat(line);
						Inc(txccnt);
						if txccnt = MAXLEN then
							raise Exception.Create('txcBuffer too small');

					end;
					'n':
					begin
						Delete(line, 1, 3);
						tmppos := Pos(' ', line);
						normalbuffer[normcnt].X := StrToFloat(Copy(line, 1, tmppos));
						Delete(line, 1, tmppos);
						tmppos := Pos(' ', line);
						normalbuffer[normcnt].Y := StrToFloat(Copy(line, 1, tmppos));
						Delete(line, 1, tmppos);
						normalbuffer[normcnt].Z := StrToFloat(line);
						Inc(normcnt);
						if normcnt = MAXLEN then
							raise Exception.Create('normBuffer too small');
					end
					else;
				end;
			end;

			'f':
			begin
				Delete(line, 1, 2);
				tmppos := Pos(' ', line);
				tmp1 := Copy(line, 1, tmppos - 1);
				Delete(line, 1, tmppos);
				tmppos := Pos(' ', line);
				tmp2 := Copy(line, 1, tmppos - 1);
				Delete(line, 1, tmppos);
				tmppos := Pos(' ', line);
				if tmppos = 0 then
				begin
					tmp3 := line;
				end
				else
				begin
					tmp3 := Copy(line, 1, tmppos);
				end;

				AddIndex(tmp1);
				AddIndex(tmp2);
				AddIndex(tmp3);
				//unsigned int vertexIndex[3], uvIndex[3], normalIndex[3];
				//int matches = fscanf(file, "%d/%d/%d %d/%d/%d %d/%d/%d\n",
				//&vertexIndex[0], &uvIndex[0], &normalIndex[0],
				//&vertexIndex[1], &uvIndex[1], &normalIndex[1],
				//&vertexIndex[2], &uvIndex[2], &normalIndex[2] );
				//vertexIndices.push_back(vertexIndex[0]);
				//vertexIndices.push_back(vertexIndex[1]);
				//vertexIndices.push_back(vertexIndex[2]);
				//uvIndices.push_back(uvIndex[0]);
				//uvIndices.push_back(uvIndex[1]);
				//uvIndices.push_back(uvIndex[2]);
				//normalIndices.push_back(normalIndex[0]);
				//normalIndices.push_back(normalIndex[1]);
				//normalIndices.push_back(normalIndex[2]);
			end
			else;

		end;
	end;

	outsize := Sizeof(TVec3);
	if usesnormals then
		outsize += SizeOf(TVec3);
	if usestxccrd then
		outsize += SizeOf(TVec3);

	SetLength(outbuffer, outsize * indexcnt);

	// For each vertex of each triangle
	for zhlr := 0 to indexcnt - 1 do
	begin

		// Get the indices of its attributes
		//unsigned int vertexIndex = vertexIndices[i];
		//unsigned int uvIndex = uvIndices[i];
		//unsigned int normalIndex = normalIndices[i];

		// Get the attributes thanks to the index
		//glm::vec3 vertex = temp_vertices[ vertexIndex-1 ];
		//glm::vec2 uv = temp_uvs[ uvIndex-1 ];
		//glm::vec3 normal = temp_normals[ normalIndex-1 ];

		// Put the attributes in buffers
		//out_vertices.push_back(vertex);
		//out_uvs.push_back(uv);
		//out_normals.push_back(normal);

	end;

	// return true;
  raise Exception.Create('model-loader is implemented yet');

	SetLength(vectorbuffer, 0);
	SetLength(normalbuffer, 0);
	SetLength(texcoordbuffer, 0);
	SetLength(vIbuffer, 0);
	SetLength(vtIbuffer, 0);
	SetLength(vnIbuffer, 0);

	CloseFile(dafile);

  Result := grbTexModel3;
end;

end.
