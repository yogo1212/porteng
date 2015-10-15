unit GamePort;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineFacilities, EnginePort, EngineStrings, EngineTypes, GameUnit,
	EngineUnit, GameGUI;

type
	{ TLinkedAbility }

	TLinkedAbility = class
		subject: TGameUnit;
		id: PAbility;
	end;

type
	TGamePort = class
		port: TEnginePort;
		u: TGameUnit;
	end;

procedure GamePortInit;

implementation

var
	ports: TFPList;

procedure portCreationCB(port: TEnginePort);
var
	tmpport: TGamePort;
begin
	tmpport := TGamePort.Create;
	tmpport.port := port;
	ports.Add(Pointer(tmpport));
	port.mainUnit := CreateUnit(0, EngineString('colBall'),
		GamePosition(0, 12, 0, 0, 0, 0), XYZRotation(0, 0), 5);
	port.HUD := TGameInterfaceMaster.Create;
end;

procedure portDeletionCB(port: TEnginePort);
var
	pnt: Pointer;
begin
	for pnt in ports do
	begin
		if TGamePort(pnt).port = port then
			break;
	end;
	port.mainUnit^.Destroy;
	FreeAndNil(port.HUD);
end;

var
	initialised: boolean = False;

procedure GamePortFree;
var
	pnt: Pointer;
begin
	if not initialised then
	begin
		initialised := False;

		for pnt in ports do
		begin
			// TODO this means the engine-ports have not been freed
		end;
		FreeAndNil(ports);
	end;
end;

procedure GamePortInit;
begin
	if not initialised then
	begin
		initialised := True;
		EnginePortsInit;
		ports := TFPList.Create;

		SetPortCreationCallback(@portCreationCB);
		SetPortDeletionCallback(@portDeletionCB);

		AddFreeRoutine(@GamePortFree);
	end;
end;

end.
