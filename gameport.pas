unit GamePort;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineFacilities, EnginePort, EngineStrings, EngineResourceLoader,
	EngineTypes, GameUnit, EngineUnit, EngineResource, GameGUI, EngineObject;

type
	{ TLinkedAbility }

	TLinkedAbility = class
		subject: TGameUnit;
		id: PAbility;
	end;

type

	{ TGamePort }

	TGamePort = class(TEngineResourceUser)
		port: TEnginePort;
		u: TGameUnit;
		procedure ActivateAbility(index: word);
		constructor Create(nport: TEnginePort; pos: TGamePosition; rota: TGameRotation);
	end;

procedure GamePortInit;

implementation

var
	ports: TFPList;

procedure portCreationCB(port: TEnginePort);
var
	tmpport: TGamePort;
begin
	tmpport := TGamePort.Create(port, GamePosition(0, 0, 0, 0, 0, 0), XYZRotation(0, 0));
	ports.Add(Pointer(tmpport));
	port.mainUnit := tmpport.u.physunit;
	port.HUD := TGameInterfaceMaster.Create;
	port.SetAbilityCb(@tmpport.ActivateAbility);
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
			FreeAndNil(TGamePort(pnt));
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

{ TGamePort }

procedure TGamePort.ActivateAbility(index: word);
begin
	//TODO
end;

constructor TGamePort.Create(nport: TEnginePort; pos: TGamePosition;
	rota: TGameRotation);
var
	model: TGameModel;
begin
	inherited Create;
	port := nport;
	model := TGameModel(GetResource(ModelName('colBall'), grtModel));
	u.Init(CreateUnit(0, model.GetRepresentation, pos, rota, 0), 5);
end;

end.
