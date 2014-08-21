program DING;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
	cthreads, {$ENDIF} {$ENDIF}

	convenience,
	dglOpenGL,
	diyftgl,
	PortEngProj,
	EngineCamera,
	EngineDataTypes,
	EngineDebug,
	EngineDevice,
	EngineDiskCache,
	EngineFacilities,
	EngineFileUtils,
	EngineForm,
	EngineGLContext,
	EngineGUI,
	EngineInput,
	EngineMath,
	EngineMemory,
	EngineModelLoader,
	EngineMouseKeyboard,
	EngineObject,
	EnginePort,
	EngineResource,
	EngineResourceColor,
	EngineResourceLoader,
	EngineResourceTexture,
	EngineShader,
	EngineShaderBuilder,
	EngineStringComparer,
	EngineStrings,
	EngineText,
	EngineTextureLoader,
	EngineThread,
	EngineTimer,
	EngineTypes,
	EngineUnit,
	EngineWorld,
	GameContext,
	GameGUI,
  GameSpell,
  GameStats,
  GameUnit,
	SDL2,
	SysUtils;

{$R *.res}

begin
	GameLogInit(portengproject.debugLevel, portengproject.Name);

	SDL_Init(0);
	if not InitOpenGL then
		raise Exception.Create('OpenGlInitError');
	InitWindow;

	GameTextInit;
	GameThreadInit;

{$ifdef ENGINEDEBUG}
	SetGlDebugCallback;
{$endif}

	SetPortListener;

	//EngineForm.mainWindow.toggle_fullscreen;
	EngineForm.mainWindow.mainLoop;
	GameClose;
	SDL_Quit;

end.
