unit GamePort;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EnginePort, GameUnit;

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


implementation

end.
