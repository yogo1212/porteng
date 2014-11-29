unit EngineShape;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineTypes, EngineMath;

type
	TTriangleFace = packed record
		v1, v2, v3: GLindex;
	end;

	TVec3Array = array of TVec3;
	TFaceArray = array of TTriangleFace;

procedure IcoSphere(out vertices: TVec3Array; out faces: TFaceArray; lod: word);
function TriangleFace(v1, v2, v3: GLindex): TTriangleFace;
function getMiddlePoint(v1, v2: TVec3): TVec3;

implementation

procedure GenerateIcosahedron(out vertices: TVec3Array; out faces: TFaceArray);
const
	t: GLfloat = (1 + sqrt(5.0)) / 2;
begin
	SetLength(vertices, 12);

	vertices[0] := Normalize(Vec3(-1, t, 0));
	vertices[1] := Normalize(Vec3(1, t, 0));
	vertices[2] := Normalize(Vec3(-1, -t, 0));
	vertices[3] := Normalize(Vec3(1, -t, 0));

	vertices[4] := Normalize(Vec3(0, -1, t));
	vertices[5] := Normalize(Vec3(0, 1, t));
	vertices[6] := Normalize(Vec3(0, -1, -t));
	vertices[7] := Normalize(Vec3(0, 1, -t));

	vertices[8] := Normalize(Vec3(t, 0, -1));
	vertices[9] := Normalize(Vec3(t, 0, 1));
	vertices[10] := Normalize(Vec3(-t, 0, -1));
	vertices[11] := Normalize(Vec3(-t, 0, 1));

	SetLength(faces, 20);

	faces[0] := TriangleFace(0, 11, 5);
	faces[1] := TriangleFace(0, 5, 1);
	faces[2] := TriangleFace(0, 1, 7);
	faces[3] := TriangleFace(0, 7, 10);
	faces[4] := TriangleFace(0, 10, 11);

	faces[5] := TriangleFace(1, 5, 9);
	faces[6] := TriangleFace(5, 11, 4);
	faces[7] := TriangleFace(11, 10, 2);
	faces[8] := TriangleFace(10, 7, 6);
	faces[9] := TriangleFace(7, 1, 8);

	faces[10] := TriangleFace(3, 9, 4);
	faces[11] := TriangleFace(3, 4, 2);
	faces[12] := TriangleFace(3, 2, 6);
	faces[13] := TriangleFace(3, 6, 8);
	faces[14] := TriangleFace(3, 8, 9);

	faces[15] := TriangleFace(4, 9, 5);
	faces[16] := TriangleFace(2, 4, 11);
	faces[17] := TriangleFace(6, 2, 10);
	faces[18] := TriangleFace(8, 6, 7);
	faces[19] := TriangleFace(9, 8, 1);
end;

procedure RefineSphere(var vertices: TVec3Array; var faces: TFaceArray);
var
	oldfaces: TFaceArray;
	face: TTriangleFace;
	cnt, vertexindex: GLindex;
begin
	// refine triangles
	oldfaces := faces;
	SetLength(faces, Length(oldfaces) * 4);
  vertexindex := Length(vertices);
	SetLength(vertices, Length(vertices) + Length(oldfaces) * 3);
	//for face in oldfaces do
	for cnt := 0 to Length(oldfaces) - 1 do
	begin
    face := oldfaces[cnt];

		vertices[vertexindex] :=
			Normalize(getMiddlePoint(vertices[face.v1], vertices[face.v2]));
		vertices[vertexindex + 1] :=
			Normalize(getMiddlePoint(vertices[face.v2], vertices[face.v3]));
		vertices[vertexindex + 2] :=
			Normalize(getMiddlePoint(vertices[face.v3], vertices[face.v1]));

		faces[cnt * 4] := TriangleFace(face.v1, vertexindex, vertexindex + 2);
		faces[cnt * 4 + 1] := TriangleFace(face.v2, vertexindex + 1, vertexindex);
		faces[cnt * 4 + 2] := TriangleFace(face.v3, vertexindex + 2, vertexindex + 1);
		faces[cnt * 4 + 3] := TriangleFace(vertexindex, vertexindex + 1, vertexindex + 2);

    Inc(vertexindex, 3);
	end;
	SetLength(oldfaces, 0);
end;

procedure IcoSphere(out vertices: TVec3Array; out faces: TFaceArray; lod: word);
begin
	GenerateIcosahedron(vertices, faces);
	while lod > 0 do
  begin
    Dec(lod);
		RefineSphere(vertices, faces);
	end;
end;

function TriangleFace(v1, v2, v3: GLindex): TTriangleFace;
begin
	Result.v1 := v1;
	Result.v2 := v2;
	Result.v3 := v3;
end;

function getMiddlePoint(v1, v2: TVec3): TVec3;
begin
	Result := (v1 + v2) / 2;
end;

end.
