unit EngineCamera;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Convenience, dglOpenGL, EngineTypes;

type
	TGameCameraMode = (gcFree, gcFixed);
	{ TGameCamera }

	TGameCamera = object
		camMode: TGameCameraMode;
		runTR: array[TGameCameraMode] of TObjProc;
		pos: PGamePosition;
		dist: GLfloat; // this is only for fixed mode
		rota: PGameRotation;
		procedure modDistance(amount: GLfloat);
		constructor Create(position: PGamePosition; distance: GLfloat;
			rotation: PGameRotation; mode: TGameCameraMode);
		procedure TranslateRotate;
		procedure FreeTR;
		procedure FixedTR;
	end;

implementation

{ TGameCamera }

procedure TGameCamera.modDistance(amount: GLfloat);
begin
	dist += amount;
	if (dist <= 0) then
		dist -= amount;
end;

procedure TGameCamera.TranslateRotate;
begin
	runTR[camMode]();
end;

procedure TGameCamera.FreeTR;
begin
	gluLookAt(pos^.offset.x, pos^.offset.y, pos^.offset.z, pos^.offset.X + (sin(rota^.xzangle) * cos(rota^.yangle)),
		pos^.offset.Y + sin(rota^.yangle), pos^.offset.Z - (cos(rota^.xzangle) *
		cos(rota^.yangle)), 0, 1, 0);
end;

procedure TGameCamera.FixedTR;
begin
	gluLookAt(pos^.offset.X + (sin(rota^.xzangle) * cos(rota^.yangle)) * dist,
		pos^.offset.Y + sin(rota^.yangle) * dist, pos^.offset.Z -
		(cos(rota^.xzangle) * cos(rota^.yangle)) * dist,
		pos^.offset.x, pos^.offset.y, pos^.offset.z, 0, 1, 0);
end;

constructor TGameCamera.Create(position: PGamePosition; distance: GLfloat;
	rotation: PGameRotation; mode: TGameCameraMode);
begin
	camMode := mode;

	rota := rotation;
	dist := distance;
	pos := position;

	runTR[gcFree] := @FreeTR;
	runTR[gcFixed] := @FixedTR;
end;

end.
