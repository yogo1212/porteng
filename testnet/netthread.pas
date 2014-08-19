unit NetThread;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dynstructs;

type

	{ TNetThread }

	TNetThread = class(TThread)
		id: longword;
		constructor Create(nId: longword);
		procedure Execute; override;
    function GetKey: longword;
	end;

implementation

{ TNetThread }

constructor TNetThread.Create(nId: longword);
begin
	inherited Create(False);
	id := nId;
end;

procedure TNetThread.Execute;
begin
	while not Terminated do
	begin

	end;
end;

function TNetThread.GetKey: longword;
begin
  Result := id;
end;

end.
