unit EngineTextureLoader;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineResourceTexture, EngineResource, EngineStrings,
	EngineResourceLoader;

procedure LoadTexFromBmp(filepath: string; Name: TResourceName;
	targetFormat: GLenum);

function TextureName(const s: string): TResourceName;

implementation

procedure LoadTexFromBmp(filepath: string; Name: TEngineString; targetFormat: GLenum);
var
	startaddress, headersize, filesize, bmpsize, compression: longword;
	Width, Height: longint;
	bpp: word; // Bits per pixel
	filestream: TFileStream;
	headerbuffer: array[byte] of byte;
	topdown: boolean;
	glformat: GLint;
begin
	filestream := TFileStream.Create(filepath, fmOpenRead);
	headerbuffer[0] := 0;
	filestream.Read(headerbuffer, 10);
	if (Chr(headerbuffer[0]) <> 'B') or (Chr(headerbuffer[1]) <> 'M') then
		raise Exception.Create(filepath + ' is not valid Bitmap-File');

	filesize := PLongWord(@headerbuffer[2])^;

	//0A
	filestream.Read(headerbuffer, 8);
	startaddress := PLongWord(@headerbuffer[0])^;
	headersize := PLongWord(@headerbuffer[4])^;


	if headersize - 4 <= 256 then
	begin
		filestream.Read(headerbuffer, headersize - 4);
		if headersize >= 40 then
		begin
			// Assume MS Bitmap thing.
			Width := PInteger(@headerbuffer[0])^;
			Height := PInteger(@headerbuffer[4])^;
			bpp := PWord(@headerbuffer[10])^;

			compression := PWord(@headerbuffer[12])^;

			bmpsize := PLongWord(@headerbuffer[16])^;

			if compression <> 0 then
				raise Exception.Create('Unsupported compression type in: ' + filepath);

		end
		else if headersize = 12 then
		begin
			Width := PWord(@headerbuffer[0])^;
			Height := PWord(@headerbuffer[2])^;
			bpp := PWord(@headerbuffer[6])^;

			bmpsize := filesize - startaddress;
		end
		else
			raise Exception.Create('Unsupported header type in ' + filepath);

	end
	else
		raise Exception.Create('Header too large..');

	if Height < 0 then
	begin
		topdown := True;
		Height := -Height;
	end
	else
		topdown := False;

	if bpp = 32 then
		glformat := GL_RGBA
	else if bpp = 24 then
		glformat := GL_RGB
	else
		raise Exception.Create('Unrecognized pixelformat on ' + filepath);

	GameTextureLoader(Name, Width, Height, glformat, targetFormat,
		GL_UNSIGNED_BYTE, filepath, filestream.Position, topdown);

	FreeAndNil(filestream);
end;

function TextureName(const s: string): TResourceName;
begin
	Result := EngineString('texture.' + s);
end;

end.
