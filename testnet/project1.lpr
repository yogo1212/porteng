program project1;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
	cthreads, cmem,
{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms,
	TestHost,
	NetThread,
	TestTypes, dynstructs;

{$R *.res}

begin
	RequireDerivedFormResource := True;
	Application.Initialize;
	Application.CreateForm(TForm1, Form1);
	Application.Run;
end.
