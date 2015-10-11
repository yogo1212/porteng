unit EnginePort;

{$mode objfpc}{$H+}

{$UNDEF USEDEPTHACCU}
interface

uses
	Classes, SysUtils, EngineDebug, EngineInput, EngineDevice, EngineStrings,
	EngineGUI, dglOpenGL, EngineUnit, EngineMath, EngineFacilities, EngineShader, GameGUI,
	EngineTypes, EngineWorld, EngineCamera, Convenience, EngineMouseKeyboard,
	Math;

type
	TEventValueProc = procedure(Value: double) of object; register;

	{ TEnginePort }

	TEnginePort = class(TGameObject)
	private
		states: array[TEngineLogicRelatedElement] of shortint;
		tmp: GLfloat;
		// x,y,z-indexed array
		MapCache: array[0..2, 0..2, 0..2] of TLoadedChunk;
		mapoffset: byte;
		OldRenderPos: TGamePosition;

		rotaUnitXZproc, rotaUnitYproc, rotaCamXZproc, rotaCamYproc: TEventValueProc;
		procedure DoRotaUnitXZ(Value: double);
		procedure DoRotaUnitY(Value: double);
		procedure DoRotaCamXZ(Value: double);
		procedure DoRotaCamY(Value: double);
		procedure DoRotaCamAndUnitXZ(Value: double);
		procedure DoRotaCamAndUnitY(Value: double);

		procedure LockRota;
		procedure UnlockRota;

		procedure DoRender;
		procedure DoInit;
	public
		input: TInput;
		camera: TGameCamera;
		HUD: TEngineUIElement;
		// this should be somewhere else:
		camRota: TGameRotation;
		mainUnit: PEngineUnit;

		sizeX, sizeY: word;
		scale: GLfloat;
		viewPortX, viewPortY, viewPortW, viewPortH: word;
		Render: TObjProc;

		//end tmp
		procedure HandleStateUpdate(const evt: TEngineInputElement; const Value: shortint);
		procedure HandleEvent(const evt: TEngineInputElement; const Value: shortint);
		procedure passtime(seconds: GLfloat); override;
		procedure RenderWorld;
		procedure UpdateMapCache;
		procedure TryUpdatePos;
		constructor Create(inp: TInput);
		destructor Destroy; override;
	end;

	TPortModificationCallback = procedure(port: TEnginePort);
	TWordRenderCallback = procedure;


procedure EnginePortsInit;
procedure PassPortTime(seconds: GLfloat);
procedure RenderPorts;
procedure definePortSpace(x, y: longword);
procedure SetPortCreationCallback(cb: TPortModificationCallback);
procedure SetPortDeletionCallback(cb: TPortModificationCallback);
procedure SetWorldRenderCallback(cb: TWordRenderCallback);
procedure SetPortListener;

implementation

type
	TPassTimeProc = procedure(seconds: GLfloat) of object;

	{ TTimeDummy }

	TTimeDummy = class
		// Hide warning
		procedure passtime({%H-}seconds: GLfloat);
	end;

var
	initialised: boolean = False;
	timedummy: TTimeDummy;
	ports: array[0..8] of TEnginePort;
	timeprocs: array[0..8] of TPassTimeProc;
	portcnt: byte;
	creationCB, deletionCB: TPortModificationCallback;
	worldRenderCB: TWordRenderCallback;

	renderHeight, renderWidth: longword;

procedure PassPortTime(seconds: GLfloat);
begin
	timeprocs[0](seconds);
	timeprocs[1](seconds);
	timeprocs[2](seconds);
	timeprocs[3](seconds);
	timeprocs[4](seconds);
	timeprocs[5](seconds);
	timeprocs[6](seconds);
	timeprocs[7](seconds);
	timeprocs[8](seconds);
end;

procedure RenderPorts;
var
	zhlr: byte;
begin
	// SCHEISSE HIER:
	for zhlr := 1 to portcnt do
		ports[zhlr - 1].Render;
end;

procedure definePortSpace(x, y: longword);
begin
	renderHeight := y;
	renderWidth := x;
end;

procedure dividePortSpace;
begin
	// TODO spread space on ports
	if (portcnt = 1) then
	begin
		ports[0].viewPortX := 0;
		ports[0].viewPortY := 0;
		ports[0].viewPortW := renderWidth;
		ports[0].viewPortH := renderHeight;
		ports[0].sizeX := renderWidth;
		ports[0].sizeY := renderHeight;
		ports[0].UpdateMapCache;
	end
	else if (portcnt = 2) then
	begin
		ports[0].viewPortX := 0;
		ports[0].viewPortY := 0;
		ports[0].viewPortW := (renderWidth + 1) div 2;
		ports[0].viewPortH := renderHeight;
		ports[0].sizeX := (renderWidth + 1) div 2;
		ports[0].sizeY := renderHeight;
		ports[0].UpdateMapCache;

		ports[1].viewPortX := (renderWidth + 1) div 2;
		ports[1].viewPortY := 0;
		ports[1].viewPortW := renderWidth div 2;
		ports[1].viewPortH := renderHeight;
		ports[1].sizeX := renderWidth div 2;
		ports[1].sizeY := renderHeight;
		ports[1].UpdateMapCache;
	end;
end;

procedure addPort(input: TInput);
begin
	if portcnt < Length(ports) then
	begin
		ports[portcnt] := TEnginePort.Create(input);
		Inc(portcnt);
	end;
end;

procedure removePort(port: TEnginePort);
var
	found: boolean = False;
	zhlr: byte;
begin
	for zhlr := 0 to portcnt - 1 do
	begin
		if found then
		begin
			ports[zhlr - 1] := ports[zhlr];
			timeprocs[zhlr - 1] := timeprocs[zhlr];
		end
		else
			found := port = ports[zhlr];
	end;

	if found then
		timeprocs[portcnt - 1] := @timedummy.passtime;

	Dec(portcnt);

	FreeAndNil(port);
	dividePortSpace;
end;

procedure compatAddPort(dev: TGameDevice);
begin
	addPort(TInput.Create(dev));
	logTrace('Port added for unused device ' + dev.GetName);
end;

procedure EnginePortsCleanup;
var
	zhlr: integer;
begin
	for zhlr := 0 to Length(timeprocs) do
		timeprocs[zhlr] := nil;

	FreeAndNil(timedummy);
end;

procedure EnginePortsInit;
begin
	if not initialised then
	begin
		initialised := True;

		creationCB := nil;
		deletionCB := nil;
		worldRenderCB := nil;

		AddFreeRoutine(@EnginePortsCleanup);

		timedummy := TTimeDummy.Create;
		timeprocs[0] := @timedummy.passtime;
		timeprocs[1] := @timedummy.passtime;
		timeprocs[2] := @timedummy.passtime;
		timeprocs[3] := @timedummy.passtime;
		timeprocs[4] := @timedummy.passtime;
		timeprocs[5] := @timedummy.passtime;
		timeprocs[6] := @timedummy.passtime;
		timeprocs[7] := @timedummy.passtime;
		timeprocs[8] := @timedummy.passtime;

		// TODO this needs to be in gamecontext
		InitGuiTextures;

		GameUnitInit;
	end;
end;

procedure SetPortCreationCallback(cb: TPortModificationCallback);
begin
	creationCB := cb;
end;

procedure SetPortDeletionCallback(cb: TPortModificationCallback);
begin
	deletionCB := cb;
end;

procedure SetWorldRenderCallback(cb: TWordRenderCallback);
begin
	worldRenderCB := cb;
end;

procedure SetPortListener;
begin
	SetUnboundNotify(@compatAddPort);
end;

{ TTimeDummy }

procedure TTimeDummy.passtime(seconds: GLfloat);
begin
	//Do muffin
end;

{ TEnginePort }

procedure TEnginePort.DoRender;
begin
	glViewport(viewPortX, viewPortY, viewPortW, viewPortH);
	glLoadIdentity;

{$IFDEF ENGINEDEBUG}
	frameLogTrace('x ' + FloatToStr(camera.pos^.offset.X) + 'y ' +
		FloatToStr(camera.pos^.offset.y) + 'z ' + FloatToStr(camera.pos^.offset.z) +
		' xzrota: ' + FloatToStr(camera.rota^.xzangle) + ' yrota: ' +
		FloatToStr(camera.rota^.yangle));
{$ENDIF}

	camera.TranslateRotate;
	SetViewOffset(mainUnit^.pos.worldPos);

	DrawAllUnits;

{$IFDEF USEDEPTHACCU}
{$ENDIF}

	RenderWorld;

	worldRenderCB;

	glDisable(GL_DEPTH_TEST);
	HUD.DrawSelf;
	glEnable(GL_DEPTH_TEST);

	// this must be here, because the opengl-calls must be made from the main thread
	TryUpdatePos;
end;

procedure TEnginePort.DoRotaUnitXZ(Value: double);
begin
	mainUnit^.rotateYclamped(Value, 0);
end;

procedure TEnginePort.DoRotaUnitY(Value: double);
begin
	mainUnit^.rotateYclamped(0, Value);
end;

procedure TEnginePort.DoRotaCamXZ(Value: double);
begin
	Rotate(Value, @camRota);
end;

procedure TEnginePort.DoRotaCamY(Value: double);
begin
	RotateClamped(Value, @camRota);
end;

procedure TEnginePort.DoRotaCamAndUnitXZ(Value: double);
begin
	mainUnit^.rotateYclamped(Value, 0);
	Rotate(Value, @camRota);
end;

procedure TEnginePort.DoRotaCamAndUnitY(Value: double);
begin
	mainUnit^.rotateYclamped(0, Value);
	RotateClamped(Value, @camRota);
end;

procedure TEnginePort.LockRota;
begin
	rotaUnitXZproc := @DoRotaCamAndUnitXZ;
	rotaUnitYproc := @DoRotaCamAndUnitY;
	rotaCamXZproc := @DoRotaCamAndUnitXZ;
	rotaCamYproc := @DoRotaCamAndUnitY;

	mainUnit^.SetRota(camRota);
end;

procedure TEnginePort.UnlockRota;
begin
	rotaUnitXZproc := @DoRotaUnitXZ;
	rotaUnitYproc := @DoRotaUnitY;
	rotaCamXZproc := @DoRotaCamXZ;
	rotaCamYproc := @DoRotaCamY;
end;

procedure TEnginePort.DoInit;

	function getownindex: byte;
	begin
		Result := 0;
		while Result < Length(ports) do
		begin
			if ports[Result] <> Self then
				Inc(Result)
			else
				break;
		end;
	end;

var
	xit, yit, zit: byte;
begin
	if creationCB <> nil then
		creationCB(Self);
	if mainUnit = nil then
		mainUnit := CreateUnit(0, EngineString('colBall'), GamePosition(0, 12, 0, 0, 0, 0),
			XYZRotation(0, 0), 5);
	if HUD = nil then
		// TODO need engine default HUD
		HUD := TGameInterfaceMaster.Create;

	camera.Create(@mainUnit^.pos, Vec3(0, 1, 0), 4, @camRota, gcFixed);

	mapoffset := $15;

	for xit := 0 to 2 do
		for yit := 0 to 2 do
			for zit := 0 to 2 do
				MapCache[xit, yit, zit].Create(mainUnit^.pos.worldPos +
					Vec3i(xit - (mapoffset and $3), yit - ((mapoffset shr 2) and $3),
					zit - ((mapoffset shr 4) and $3)));

	dividePortSpace;
	timeprocs[getownindex] := @passtime;

	Render := @DoRender;
	DoRender;
end;

procedure TEnginePort.HandleStateUpdate(const evt: TEngineInputElement;
	const Value: shortint);

	procedure UpdateVelocity;
	begin
		mainUnit^.SetVelocity(Vec3(states[ieMoveRelX], states[ieMoveRelY],
			states[ieMoveRelZ]));
	end;

begin
	states[evt] := Value;

	if (evt = ieMoveRelX) or (evt = ieMoveRelY) or (evt = ieMoveRelZ) then
		UpdateVelocity;
	//dbgTrace(IntToStr(states[ieMoveRelZ]) + ' aus ' + IntToStr(states[ieMoveRelZp]) +
	//  ' und ' + IntToStr(states[ieMoveRelZn]));
end;

procedure TEnginePort.HandleEvent(const evt: TEngineInputElement; const Value: shortint);
begin
	states[evt] := Value;

	case evt of
		ieZoomTickY:
			camera.dist -= Value / 4;
		ieJump:
			if Value <> 0 then
				mainUnit^.Jump;
		ieTurnRelTickXZ:
			rotaUnitXZproc(Value);
		ieTurnRelTickY:
			rotaUnitYproc(Value);
		ieLookRelTickXZ:
			rotaCamXZproc(Value / 10);
		ieLookRelTickY:
			rotaCamYproc(Value / 10);
		ieActionLeft, ieActionMiddle, ieActionRight, ieActionX1, ieActionX2: ;
		ieAlignCam:
			if Value <> 0 then
				mainUnit^.SetRota(camRota);
		ieCursorTickX, ieCursorTickY,
		ieZoomTickX: ;
		ieNoclip:
			if mainUnit^.state = psNoclip then
				mainUnit^.state := psGrounded
			else
				mainUnit^.state := psNoclip;

    ieAbility1: ;
    ieAbility2: ;
    ieAbility3: ;
    ieAbility4: ;
    ieAbility5: ;
    ieAbility6: ;
	end;
end;

procedure TEnginePort.passtime(seconds: GLfloat);
begin
	tmp := seconds * (60 / 127);

	rotaUnitXZproc(states[ieTurnRelXZ] * tmp);
	rotaUnitYproc(states[ieTurnRelY] * tmp);
	rotaCamXZproc(states[ieLookRelXZ] * tmp);
	rotaCamYproc(states[ieLookRelY] * tmp);

	tmp := seconds / 127;
	camera.dist += states[ieZoom] * tmp;
end;

procedure TEnginePort.RenderWorld;
begin
	GetProgram(spMap).Use;
	MapCache[0, 0, 0].draw();
	MapCache[0, 0, 1].draw();
	MapCache[0, 0, 2].draw();
	MapCache[0, 1, 0].draw();
	MapCache[0, 1, 1].draw();
	MapCache[0, 1, 2].draw();
	MapCache[0, 2, 0].draw();
	MapCache[0, 2, 1].draw();
	MapCache[0, 2, 2].draw();
	MapCache[1, 0, 0].draw();
	MapCache[1, 0, 1].draw();
	MapCache[1, 0, 2].draw();
	MapCache[1, 1, 0].draw();
	MapCache[1, 1, 1].draw();
	MapCache[1, 1, 2].draw();
	MapCache[1, 2, 0].draw();
	MapCache[1, 2, 1].draw();
	MapCache[1, 2, 2].draw();
	MapCache[2, 0, 0].draw();
	MapCache[2, 0, 1].draw();
	MapCache[2, 0, 2].draw();
	MapCache[2, 1, 0].draw();
	MapCache[2, 1, 1].draw();
	MapCache[2, 1, 2].draw();
	MapCache[2, 2, 0].draw();
	MapCache[2, 2, 1].draw();
	MapCache[2, 2, 2].draw();
end;

procedure TEnginePort.UpdateMapCache;
begin
	MapCache[0, 0, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 0, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 0, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 1, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 1, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 1, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 2, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 2, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[0, 2, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 0, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 0, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 0, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 1, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 1, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 1, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 2, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 2, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[1, 2, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 0, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 0, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 0, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 1, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 1, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 1, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 2, 0].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 2, 1].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));
	MapCache[2, 2, 2].UpdateRenderData(OldRenderPos, Min(sizeX, sizeY));

	OldRenderPos := mainUnit^.pos;
end;

procedure TEnginePort.TryUpdatePos;
begin
	genericLookup(LengthSquare(OldRenderPos - mainUnit^.pos) > 100, @UpdateMapCache);
end;

constructor TEnginePort.Create(inp: TInput);
begin
	inherited Create;
	input := inp;
	scale := 1;

	UnlockRota;

	FillByte(states, SizeOf(states[Low(TEngineLogicRelatedElement)]) * Length(states), 0);

	inp.SetStatesCallback(@HandleStateUpdate);
	inp.SetEventsCallback(@HandleEvent);
	inp.SetLockRotaProc(@LockRota);
	inp.SetUnlockRotaProc(@UnlockRota);

	Render := @DoInit;
end;

destructor TEnginePort.Destroy;
begin
	if Assigned(deletionCB) then
		deletionCB(Self);
	FreeAndNil(input);
	inherited Destroy;
end;

end.
