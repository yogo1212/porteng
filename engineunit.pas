unit EngineUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineTypes, EngineObject, dglOpenGL, EngineStrings,
	EngineMemory, EngineFacilities, EngineMath, EngineWorld, Convenience;

type

	TGameUnitPhysState = (psFalling, psGrounded, psFlying, psNoclip);

	{ TEngineUnit }

	TEngineUnit = object(TGameGraphicsObj)
	protected
		//tvelo,
		velo, relamov, fallVelo: TVec3;
		tpos: TGamePosition;
		id: longword;

		constructor Create(newid: longword; model: TEngineString;
			position: TGamePosition; rotation: TGameRotation; nspeed: GLfloat);
		procedure UpdateVelocity;
	public
    deltapos: TVec3;
		speed: GLfloat;
		state: TGameUnitPhysState;

		procedure rotateYclamped(amountxz, amounty: GLfloat);
		procedure Jump;
		function PlanMove(time: GLfloat): TVec3;
		procedure passTime(seconds: GLfloat);
		procedure SetVelocity(relativeMovement: TVec3);
    procedure SetRota(nrota: TGameRotation);
		destructor Destroy;
		property UnitId: longword read id;
	end;

	PEngineUnit = ^TEngineUnit;

procedure PassAllUnitsTime(seconds: GLfloat);
procedure DrawAllUnits;
procedure GameUnitInit;
function CreateUnit(newid: longword; Model: TEngineString; position: TGamePosition;
	rotation: TGameRotation; nspeed: GLfloat): PEngineUnit;

implementation

const
	gravitationalAcc: GLfloat = 20;
	gravitationalMaxSpeed: GLfloat = 80;

procedure moveFalling(unit_: PEngineUnit; seconds: GLfloat);
begin
	unit_^.fallVelo.Y := unit_^.fallVelo.Y - seconds * gravitationalAcc;
	Limit(unit_^.fallVelo, gravitationalMaxSpeed);
	unit_^.tpos := unit_^.pos;
	// TODO lookup
	if CheckWorldCollision(unit_^.tpos, unit_^.fallVelo * seconds) then
	begin
		unit_^.state := psGrounded;
		unit_^.fallVelo.Y := 0;
	end;
	unit_^.pos := unit_^.tpos;
end;

procedure moveGrounded(unit_: PEngineUnit; seconds: GLfloat);
begin
	unit_^.tpos.offset.y := unit_^.pos.offset.y + unit_^.Speed / 2;
	// TODO loooooookup
	CheckWorldCollision(unit_^.tpos, unit_^.velo * seconds);
	if not CheckWorldCollision(unit_^.tpos,
		Vec3(0, -unit_^.Speed - 0.5 * seconds * seconds * gravitationalAcc, 0)) then
	begin
		unit_^.state := psFalling;
		unit_^.fallVelo := unit_^.velo;
	end;
	unit_^.pos.offset := unit_^.tpos.offset;
end;

procedure moveFlying(unit_: PEngineUnit; seconds: GLfloat);
begin
	unit_^.tpos := unit_^.pos;
	CheckWorldCollision(unit_^.tpos, unit_^.velo * seconds);
	unit_^.pos := unit_^.tpos;
end;

procedure moveNoclip(unit_: PEngineUnit; seconds: GLfloat);
begin
	unit_^.tpos.offset += unit_^.velo * seconds;
  TryNormalise(unit_^.tpos);
end;

type

	TGameUpdateProc = procedure(notif: TObjProc);
	TGameUnitTimeProc = procedure(unit_: PEngineUnit; seconds: GLfloat);

	{ TGameUnitComparer }

	TGameUnitComparer = class(_TPointerComparer)
		function compare(o1, o2: Pointer): shortint; override;
		function greater(o1, o2: Pointer): boolean; override;
		function equal(o1, o2: Pointer): boolean; override;
		function less(o1, o2: Pointer): boolean; override;
	end;

var
	move: array[TGameUnitPhysState] of
	TGameUnitTimeProc = (@moveFalling, @moveGrounded, @moveFlying, @moveNoclip);

procedure DoUpdate(notif: TObjProc);
begin
	notif();
end;

procedure DontUpdate({%H-}notif: TObjProc);
begin

end;

var
	gameUnitComparer: TGameUnitComparer;
	units: TSortedMemoryManager;
	initialized: boolean = False;

	updateTable: array[boolean] of TGameUpdateProc = (@DontUpdate, @DoUpdate);

procedure GameUnitFree;
var
	zhlr: cardinal;
begin
	if initialized then
	begin
		initialized := False;
		zhlr := units.Count;
		while zhlr > 0 do
		begin
			Dec(zhlr);
			PEngineUnit(units.Traverse[zhlr])^.Destroy;
		end;
		FreeAndNil(units);
		FreeAndNil(gameUnitComparer);
	end;
end;

procedure DrawAllUnits;
var
	zhlr: cardinal;
begin
	zhlr := units.Count;
	while zhlr > 0 do
	begin
		Dec(zhlr);
		PEngineUnit(units.Traverse[zhlr])^.Draw;
	end;
end;

procedure GameUnitInit;
begin
	if not initialized then
	begin
		initialized := True;
		gameUnitComparer := TGameUnitComparer.Create;
		units := TSortedMemoryManager.Create(gameUnitComparer,
			TFragmentedMemoryManager.Create(SizeOf(TEngineUnit), 50));
		AddFreeRoutine(@GameUnitFree);
	end;
end;

function CreateUnit(newid: longword; Model: TEngineString; position: TGamePosition;
	rotation: TGameRotation; nspeed: GLfloat): PEngineUnit;
var
	tmpunit: TEngineUnit;
begin
	tmpunit.Create(newid, model, position, rotation, nspeed);
	Result := units.Store(@tmpunit);
end;

procedure RemoveUnit(gUnit: TEngineUnit);
begin
	units.Delete(@gUnit);
end;

procedure PassAllUnitsTime(seconds: GLfloat);
var
	zhlr: cardinal;
begin
	zhlr := units.Count;
	while zhlr > 0 do
	begin
		Dec(zhlr);
		PEngineUnit(units.Traverse[zhlr])^.passTime(seconds);
	end;
end;

{ TGameUnitComparer }

function TGameUnitComparer.compare(o1, o2: Pointer): shortint;
begin
	if PEngineUnit(o1)^.id < PEngineUnit(o2)^.id then
		Result := -1
	else if PEngineUnit(o1)^.id = PEngineUnit(o2)^.id then
		Result := 0
	else
		Result := 1;
end;

function TGameUnitComparer.greater(o1, o2: Pointer): boolean;
begin
	Result := PEngineUnit(o1)^.id > PEngineUnit(o2)^.id;
end;

function TGameUnitComparer.equal(o1, o2: Pointer): boolean;
begin
	Result := PEngineUnit(o1)^.id = PEngineUnit(o2)^.id;
end;

function TGameUnitComparer.less(o1, o2: Pointer): boolean;
begin
	Result := PEngineUnit(o1)^.id < PEngineUnit(o2)^.id;
end;

{ TEngineUnit }

procedure TEngineUnit.SetVelocity(relativeMovement: TVec3);
begin
	Limit(relativeMovement, 127);
	relativeMovement := relativeMovement / 127;
	// TODO lookuptable?
	if relamov <> relativeMovement then
	begin
		relamov := relativeMovement;

		UpdateVelocity;
	end;
end;

procedure TEngineUnit.SetRota(nrota: TGameRotation);
begin
  rota := nrota;
  if state <> psFlying then
    rota.yangle := 0;
end;

function TEngineUnit.PlanMove(time: GLfloat): TVec3;
begin
	Result := velo * time;
end;

procedure TEngineUnit.passTime(seconds: GLfloat);
begin
	//TODO
	//Check for contact with ground ()

	// TODO add second pass in second thread!
	// In the meantime:

	move[state](@Self, seconds);
end;

procedure TEngineUnit.UpdateVelocity;
begin
	if state = psFlying then
	begin
		velo.X := (relamov.Z * sin(rota.xzangle) * cos(rota.yangle) -
			relamov.X * cos(rota.xzangle)) * Speed;
		velo.Y := (relamov.Z * sin(rota.yangle)) * Speed;
		velo.Z := (-relamov.Z * cos(rota.xzangle) * cos(rota.yangle) -
			relamov.X * sin(rota.xzangle)) * Speed;
	end
	else // if state = psGrounded then
	begin
		velo.X := (relamov.Z * sin(rota.xzangle) - relamov.X *
			cos(rota.xzangle)) * Speed;
		velo.Y := 0;// (relamov.Z * sin(rota.yRot.rad)) * Speed;
		velo.Z := (-relamov.Z * cos(rota.xzangle) - relamov.X *
			sin(rota.xzangle)) * Speed;
	end;
end;

constructor TEngineUnit.Create(newid: longword; model: TEngineString;
	position: TGamePosition; rotation: TGameRotation; nspeed: GLfloat);
begin
	inherited Create(Model, rotation, position);
	id := newid;
	Speed := nspeed;
	velo := Vec3(0, 0, 0);
	relamov := Vec3(0, 0, 0);
	state := psGrounded;
end;

procedure TEngineUnit.rotateYclamped(amountxz, amounty: GLfloat);
begin
  RotateXZ(amountxz);
  // TODO
  if state = psFlying then
    RotateY(amounty);
  updateTable[(amountxz <> 0) or (amounty <> 0)](@UpdateVelocity);
end;

procedure TEngineUnit.Jump;
begin
	if state = psGrounded then
	begin
		state := psFalling;
		fallVelo := velo;
		fallVelo.Y += 10;
	end;
end;

destructor TEngineUnit.Destroy;
begin
	RemoveUnit(Self);

	inherited Destroy;
end;

end.
