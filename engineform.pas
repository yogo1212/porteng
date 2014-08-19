unit EngineForm;

{$mode objfpc}{$H+}


{$UNDEF USEDEPTHACCU}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineText, EngineMouseKeyboard, PortEngProj, EnginePort,
	EngineFacilities, EngineDebug, SDL2, EngineGLContext, Convenience;

type

	PSDL_WindowEvent = ^TSDL_WindowEvent;

	{ TMainWindow }

	TMainWindow = object
	private
		//Whether the window is windowed or not
		windowed,
		//Whether the window is fine
		running: boolean;

		window: PSDL_Window; // Our main screen
		context: SDL_GLContext;

{$IFDEF USEDEPTHACCU}
		depthFramebuffer: GLuint;
		depthTextures: array[0..3] of GLuint;
{$ENDIF}

		wWidth, wHeight, fWidth, fHeight, actualWidth, actualHeight: cardinal;
		procedure DohandleWindowEvent(evt: PSDL_WindowEvent);
	public
		constructor Create(nwidth, nheight: cardinal; ntitle: string);
		destructor Destroy;
		procedure Resize;
		procedure Paint;
		//Turn fullscreen on/off
		procedure toggle_fullscreen;
		procedure mainLoop;

		procedure Close;
	end;

var
	mainWindow: TMainWindow;

procedure InitWindow;

implementation

var
	initialised: boolean = False;

procedure FreeWindow;
begin
	if initialised then
	begin
		initialised := False;
		mainwindow.Destroy;
		SDL_QuitSubSystem(SDL_INIT_VIDEO); // close the subsystem
	end;
end;

function SelectDisplay: longint;
begin

end;

procedure InitWindow;
var
	display: longint;
	disprect: TSDL_Rect;
begin
	if not initialised then
	begin
		initialised := True;
		// Initialize the video SDL subsystem
		if SDL_InitSubSystem(SDL_INIT_VIDEO) = -1 then
			raise Exception.Create('Error with SDL_Init(SDL_INIT_VIDEO)');

		//find screen size
		//SDL_GetDesktopDisplayMode();
		display := SelectDisplay;
		if SDL_GetDisplayBounds(display, @disprect) = 0 then
		begin
			mainWindow.Create(round(disprect.w * 0.8), round(disprect.h * 0.8),
				portengproject.Name);

			AddFreeRoutine(@FreeWindow);

			InitGlContext;
		end
		else
		begin
			dbgError('SDL_GetDisplayBounds failed: ' + SDL_GetError());
		end;
	end;
end;

{ TMainWindow }

constructor TMainWindow.Create(nwidth, nheight: cardinal; ntitle: string);
var
	display: longint;
	dispcnt: word;
	dm: TSDL_DisplayMode;
begin
	// TODO for now we always run on :0
	display := 0;

	dispcnt := SDL_GetNumVideoDisplays;
	if display >= dispcnt then
		display := 0;

	SDL_GetDesktopDisplayMode(display, @dm);
	fwidth := dm.w;
	fheight := dm.h;

	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

	window := SDL_CreateWindow(PChar(ntitle), SDL_WINDOWPOS_UNDEFINED or
		display, SDL_WINDOWPOS_UNDEFINED or display, nwidth, nheight,
		SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE);

	if window = nil then
		raise Exception.Create('Error on window-initialisation!');

	wWidth := nwidth;
	wHeight := nheight;

	windowed := True;

	//SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	//SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);

	context := SDL_GL_CreateContext(window);

	if context = nil then
		raise Exception.Create('Error on context-initialisation!');

	SDL_GL_MakeCurrent(window, context);
	SDL_GL_SetSwapInterval(1);


	InitGlContext;

{$IFDEF USEDEPTHACCU}
	glGenFramebuffers(1, @depthFramebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, depthFramebuffer);

	glGenTextures(4, @depthTextures[0]);
{$ENDIF}

	Resize;

	running := True;
end;

destructor TMainWindow.Destroy;
begin
{$IFDEF USEDEPTHACCU}
	glDeleteFramebuffers(1, @depthFramebuffer);
{$ENDIF}

	SDL_GL_DeleteContext(context);
	// Close and destroy the window
	SDL_DestroyWindow(window);
end;

procedure TMainWindow.Resize;
{$IFDEF USEDEPTHACCU}
var
	cnt: byte;
{$ENDIF}
begin
	if windowed then
	begin
		actualHeight := wheight;
		actualWidth := wwidth;
	end
	else
	begin
		actualHeight := fheight;
		actualWidth := fwidth;
	end;

{$IFDEF USEDEPTHACCU}
	for cnt := 3 downto 0 do
	begin
		glBindTexture(GL_TEXTURE_2D, depthTextures[cnt]);

		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, actualWidth, actualHeight, 0, GL_RGBA,
			GL_UNSIGNED_BYTE, nil);
		// Poor filtering. Needed !
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

		// does this need to happen on every resize or just once?
		glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + cnt,
			GL_TEXTURE_2D, depthTextures[cnt], 0);
	end;
{$ENDIF}

	if actualHeight = 0 then
		actualHeight := 1;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
	//glOrtho(0, ClientWidth, ClientHeight, 0, -1, 1);
	gluPerspective(90, actualWidth / actualHeight, 0.2, 768);
	glMatrixMode(GL_MODELVIEW);

	definePortSpace(actualWidth, actualHeight);
end;

procedure TMainWindow.DohandleWindowEvent(evt: PSDL_WindowEvent);
begin
	if evt^.type_ = 512 then
		exit;
	// these - most-likely - wont be called too often, so no lookup necessary
	if evt^.type_ = SDL_WINDOWEVENT_CLOSE then
	begin
		Close;
	end
	else if evt^.type_ = SDL_WINDOWEVENT_RESIZED then
	begin
		wWidth := evt^.data1;
		wHeight := evt^.data2;
		if windowed then
			Resize;
	end
	else
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, 'Window_event',
			PChar('unhandled window event: ' + IntToHex(evt^.type_, SizeOf(evt^.type_) * 2)),
			window);
end;

procedure TMainWindow.Paint;
{$IFDEF USEDEPTHACCU}
const
	drawbuffers: array[0..3] of GLuint = (GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1,
		GL_COLOR_ATTACHMENT2, GL_COLOR_ATTACHMENT3);
{$ENDIF}
begin
{$IFDEF ENGINEDEBUG}
	dbgClearFrameLines;
{$ENDIF}

{$IFDEF USEDEPTHACCU}
	glBindFramebuffer(GL_FRAMEBUFFER, depthFramebuffer);
{$ENDIF}

	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

{$IFDEF USEDEPTHACCU}
	glDrawBuffers(4, @drawbuffers);
{$ENDIF}

	renderPorts;

{$IFDEF USEDEPTHACCU}
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glViewport(0, 0, actualWidth, actualHeight);
{$ENDIF}

	SDL_GL_SwapWindow(window);
end;

procedure TMainWindow.toggle_fullscreen;
begin
	//  SDL_WINDOW_FULLSCREEN does real frame
	if windowed then
	begin
		SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
		windowed := False;
		actualWidth := fWidth;
		actualHeight := fHeight;
	end
	else
	begin
		SDL_SetWindowFullscreen(window, 0);
		windowed := True;
		actualWidth := wWidth;
		actualHeight := wHeight;
	end;
	Resize;
end;

var
	evt: TSDL_Event;

procedure TMainWindow.mainLoop;
type
	TSDLWindowEventHandler = procedure(evt: PSDL_WindowEvent) of object; register;
var
	handleWindowEvent: array[boolean] of TSDLWindowEventHandler;
begin
	PPointer(@handleWindowEvent[False])^ := Pointer(@DoNothing);
	handleWindowEvent[True] := @DohandleWindowEvent;

	while Running do
	begin
		while SDL_PollEvent(@evt) <> 0 do
		begin
			KeyDown(evt.type_ = SDL_KEYDOWN, @evt.key);
			KeyUp(evt.type_ = SDL_KEYUP, @evt.key);
			handleWindowEvent[evt.type_ = SDL_WINDOWEVENT](@evt.window);
			MouseMotion(evt.type_ = SDL_MOUSEMOTION, @evt.motion);
			MouseBtnDown(evt.type_ = SDL_MOUSEBUTTONDOWN, @evt.button);
			MouseBtnUp(evt.type_ = SDL_MOUSEBUTTONUP, @evt.button);
			MouseWheel(evt.type_ = SDL_MOUSEWHEEL, @evt.wheel);
			genericLookup(evt.type_ = SDL_QUITEV, @Close);
		end;
		Paint;
	end;
end;

procedure TMainWindow.Close;
begin
	// TODO this doesn't look good:wq :-p
	running := False;
end;

end.
