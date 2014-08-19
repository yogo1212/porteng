unit EngineDebug;

{$mode objfpc}{$H+}

// Show Form for debuggin

interface

uses
	Classes, SysUtils, EngineFacilities, dglOpenGL;

type
	TGameDebugLevel = (gdTrace, gdError, gdNone);

var
	errorenum: glenum;

procedure GameDebugInit(lvl: TGameDebugLevel; appname: String);
procedure dbgError(input: string);
procedure dbgTrace(message: string);
procedure SetDebugCallback;

{$IFDEF ENGINEDEBUG}
procedure frameDbgError(input: string);
procedure frameDbgTrace(message: string);
procedure dbgClearFrameLines;
{$ENDIF}

implementation

var
	LogFilePath: String;
	debuglvl: TGameDebugLevel;
	logfile: TextFile;
	Initialised: boolean = False;

procedure debug(lvl: TGameDebugLevel; message: string);
begin
	if lvl >= debuglvl then
	begin
	  writeln(logfile, message);
{$IFDEF Linux}
    writeln(message);
{$ENDIF}
	end;
end;

procedure dbgError(input: string);
begin
	debug(gdError, input);
end;

procedure dbgTrace(message: string);
begin
	debug(gdTrace, message);
end;

{$IFDEF ENGINEDEBUG}
procedure framedebug(lvl: TGameDebugLevel; message: string);
begin
	if lvl >= debuglvl then
	begin
    //TODO
	end;
end;

procedure frameDbgError(input: string);
begin
	framedebug(gdError, input);
end;

procedure frameDbgTrace(message: string);
begin
	framedebug(gdTrace, message);
end;

var
	glLines: byte;

procedure glPrint(message: string);
begin
	//ShowText(message, 0, glLines * 12);
	Inc(glLines);
end;

procedure glResetLineCount;
begin
	glLines := 0;
end;

procedure dbgClearFrameLines;
begin

end;
{$ENDIF}

procedure GameDebugCleanup;
begin
	if initialised then
	begin
		initialised := False;
{$IFDEF ENGINEDEBUG}
		//dbgForm.Close;
{$ENDIF}
		if debuglvl <> gdNone	then
			CloseFile(logfile);
	end;
end;

procedure glCallbackProc(Source, type_: GLEnum; id: GLUInt; severity: GLenum;
	length: GLsizei; const message_: PGLCHar; userParam: PGLvoid);
	{$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
var
	tmpstr, msgbuf: string;
begin
	tmpstr := 'glErrCb ';
	case (severity) of
		GL_DEBUG_SEVERITY_LOW:
			tmpstr += 'LOW';
		GL_DEBUG_SEVERITY_MEDIUM:
			tmpstr += 'MEDIUM';
		GL_DEBUG_SEVERITY_HIGH:
			tmpstr += 'HIGH';
		else
			tmpstr += '[nknwn]';
	end;

	tmpstr += #9;

	case (type_) of
		GL_DEBUG_TYPE_ERROR:
			tmpstr += 'ERROR';
		GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR:
			tmpstr += 'DEPRECATED_BEHAVIOR';
		GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:
			tmpstr += 'UNDEFINED_BEHAVIOR';
		GL_DEBUG_TYPE_PORTABILITY:
			tmpstr += 'PORTABILITY';
		GL_DEBUG_TYPE_PERFORMANCE:
			tmpstr += 'PERFORMANCE';
		GL_DEBUG_TYPE_OTHER:
			tmpstr += 'OTHER';
		else
			tmpstr += '[nknwn]';
	end;

	SetLength(msgbuf, length);
	Move(message_^, msgbuf[1], length);

	tmpstr += ': "' + msgbuf + Copy(tmpstr, 1, system.Length(tmpstr) - 1);
  SetLength(msgbuf, 0);
	{$IFDEF Linux}
	writeln(tmpstr);
	{$ENDIF}
	writeln(logfile, tmpstr);
end;

procedure SetDebugCallback;
begin
	errorenum := glGetError();
	if errorenum <> 0 then
		raise Exception.Create('There were already errors when initializing the ' +
			'error-callback: ' + IntToStr(errorenum));
	if dglCheckExtension('GL_ARB_debug_output') then
	begin
		glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
		errorenum := glGetError();
		if errorenum <> 0 then
			raise Exception.Create('glEnable(DEBUG_OUTPUT_SYNCHRONOUS_ARB): ' +
				IntToStr(errorenum));
		glDebugMessageCallbackARB(@glCallbackProc, nil);
		errorenum := glGetError();
		if errorenum <> 0 then
			raise Exception.Create('glDebugMessageCallbackARB: ' + IntToStr(errorenum));
		glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil,
			bytebool(GL_TRUE));
		errorenum := glGetError();
		if errorenum <> 0 then
			raise Exception.Create('glDebugMessageControlARB: ' + IntToStr(errorenum));
	end
	else
	begin
		if glDebugMessageCallback <> nil then
		begin
			glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
			errorenum := glGetError();
			if errorenum <> 0 then
				raise Exception.Create('glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS): ' +
					IntToStr(errorenum));
			glEnable(GL_DEBUG_OUTPUT);
			errorenum := glGetError();
			if errorenum <> 0 then
				raise Exception.Create('glEnable(GL_DEBUG_OUTPUT): ' + IntToStr(errorenum));
			glDebugMessageCallback(@glCallbackProc, nil);
			errorenum := glGetError();
			if errorenum <> 0 then
				raise Exception.Create('glDebugMessageCallback: ' + IntToStr(errorenum));
			glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil,
				bytebool(GL_TRUE));
			errorenum := glGetError();
			if errorenum <> 0 then
				raise Exception.Create('glDebugMessageControlError: ' + IntToStr(errorenum));
		end;
	end;
end;

procedure GameDebugInit(lvl: TGameDebugLevel; appname: String);
begin
	if not initialised then
	begin
		initialised := True;
    LogFilePath := GetTempDir(false) + appname +
{$IFDEF Windows}
    StringReplace(DateTimeToStr(now), ':', '-', [rfReplaceAll]);
{$ELSE}
    DateTimeToStr(now);
{$ENDIF}
		AddFreeRoutine(@GameDebugCleanup);
		debuglvl := lvl;
		if lvl <> gdNone then
		begin
			AssignFile(logfile, LogFilePath);
			Rewrite(logfile);
		end;
	end;
end;

end.
