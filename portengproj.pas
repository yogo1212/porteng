unit PortEngProj;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineDebug;

type
	TPortEngProject = record
		Name, Author: string;
		logToFile: boolean;
		debugLevel: TGameDebugLevel;
		mousekeyboard, gamepad: boolean;
	end;

const
	portengproject: TPortEngProject = (Name: 'DING'; Author: 'banyoghurt';
		logToFile: False; debugLevel: gdTrace; mousekeyboard: True;
		gamepad: True);

implementation

end.