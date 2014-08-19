unit EngineGLContext;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineDebug;

procedure InitGlContext;
function checkGlVersion(major, minor: GLint): Boolean;

implementation

var
	initialised: boolean = False;

procedure InitGlContext;
var
	tmpint: integer;
begin
	if not initialised then
	begin
		initialised := True;

		//if not InitOpenGL then
		//	raise Exception.Create('OpenGlInitError');

		ReadExtensions;

		glCullFace(GL_BACK);
		glEnable(GL_CULL_FACE);

		glDisable(GL_DITHER);

    glDepthFunc(GL_LEQUAL);
		glDepthMask(bytebool(GL_TRUE));
		glEnable(GL_DEPTH_TEST);

		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glEnable(GL_BLEND);

		glClearColor(0.50, 0, 0.50, 1);
		glClearDepth(1);

		dbgTrace('Created GlContext with version ' + glGetString(GL_VERSION));

		glGetIntegerv(GL_DEPTH_BITS, @tmpint);
		dbgTrace('Depth-channel is ' + IntToStr(tmpint) + ' bits wide');
		dbgTrace('Extensions: ' + glGetString(GL_EXTENSIONS));
	end;
end;

function checkGlVersion(major, minor: GLint): Boolean;
const maj: Glint = 1; min: GLint = 1;
var tmp: glenum;
begin
  glGetIntegerv(GL_MAJOR_VERSION, @maj);
  tmp := glGetError();
  if tmp <> 0 then
    dbgError('Error checking version: ' + IntToStr(tmp));
  glGetIntegerv(GL_MINOR_VERSION, @min);
  Result := (maj >= major) or ((maj = major) and (min >= minor));
end;

end.
