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
	//Das hier geht geht:
	gluLookAt(pos^.x, pos^.y, pos^.z, pos^.X + (sin(rota^.xzangle) * cos(rota^.yangle)),
		pos^.Y + sin(rota^.yangle), pos^.Z - (cos(rota^.xzangle) *
		cos(rota^.yangle)), 0, 1, 0);
end;

procedure TGameCamera.FixedTR;
begin
	gluLookAt(pos^.X + (sin(rota^.xzangle) * cos(rota^.yangle)) * dist,
		pos^.Y + sin(rota^.yangle) * dist, pos^.Z -
		(cos(rota^.xzangle) * cos(rota^.yangle)) * dist,
		pos^.x, pos^.y, pos^.z, 0, 1, 0);
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
