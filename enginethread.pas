unit EngineThread;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, PortEngProj, EnginePort, EngineInput, EngineTimer, EngineFacilities,
	EngineUnit, dglOpenGL;

type

	{ TGameThread }

	TGameThread = class(TThread)
		procedure Execute; override;
		constructor Create;
		destructor Destroy; override;
	end;

var
	gameThr: TGameThread;

procedure GameThreadInit;

implementation

var
	initialised: boolean = False;

procedure GameThreadCleanup;
begin
	if initialised then
	begin
		gameThr.Terminate;

		FreeAndNil(gameThr);
	end;
end;

procedure GameThreadInit;
begin
	if not initialised then
	begin
		Initialised := True;

		GameInputInit(portengproject.mousekeyboard, portengproject.gamepad);
		GameTimeInit;

		EnginePortsInit;

		GameUnitInit;

		gameThr := TGameThread.Create;

		AddFreeRoutine(@GameThreadCleanup);
	end;
end;

{ TGameThread }

procedure TGameThread.Execute;
var
	gameThreadTimer: TGameTimer;
	akku: UInt64;
	time: GLfloat;
begin
	gameThreadTimer.Create(30);
	akku := 0;
	while not Terminated do
	begin
		akku := gameThreadTimer.waitFor;

		//translate from milliSeconds to seconds
		time := akku / 1000;

		PassPortTime(time);
		PassAllUnitsTime(time);
	end;
end;

constructor TGameThread.Create;
begin
	inherited Create(False);
end;

destructor TGameThread.Destroy;
begin
	WaitForThreadTerminate(ThreadID, 0);
	inherited Destroy;
end;

end.
