program DING;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
	cthreads, {$ENDIF} {$ENDIF}
	SysUtils,
	dglOpenGL,
	SDL2,
	convenience,
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
	EngineOctree,
	EnginePort,
	EngineResource,
	EngineResourceColour,
	EngineResourceLoader,
	EngineResourceTexture,
	EngineShader,
	EngineShaderBuilder,
	EngineShape,
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
	GamePort,
	GameSpell,
	GameStats,
	GameUnit;

{$R *.res}

begin
	try
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
	except
		on E: Exception do
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, PChar('Unhandled exception'),
				PChar(E.Message), nil);
	end;
end.
