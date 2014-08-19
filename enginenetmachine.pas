unit EngineNetMachine;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, SDL2_net, SDL2;

type

	TUdpPacketHandler = procedure(p: PUDPpacket);

	{ TEngineNetMachine }

	TEngineNetMachine = object
		udpsock: PUDPsocket;
		localipaddr: TIPaddress;
		thread: PSDL_Thread;
		cb: TUdpPacketHandler;
		active: boolean;
		function Init(localport: UInt16;
			callback: TUdpPacketHandler): boolean;
		procedure Close;
	end;

	PEngineNetMachine = ^TEngineNetMachine;

const
	//max. udp-datalength over IP. is 65,507. the whole packet can be 65,515 bytes in size.
	//considering IP-fragmentation, the payload should only be 1472 bytes.
	//so the whole frame should be 1480 at max.
	//(over pppoe thats 8 bytes less)
	//dont send more than this:
	maxPacketSize = 1480;

	//for the min. MTU (576) the frame can be 516 (60 bytes IP-header) bytes long.
	//taking pppoe into consideration:
	safePacketSize = 508;

implementation

function NetThreadFunc(Data: PEngineNetMachine): longint; cdecl;
var
	buf: PUDPpacket;
begin
	buf := SDLNet_AllocPacket(maxPacketSize);

	while Data^.active do
	begin
		SDLNet_UDP_Recv(Data^.udpsock, buf);
		Data^.cb(buf);
    SDL_Delay(12);
	end;

	SDLNet_FreePacket(buf);
end;

{ TEngineNetMachine }

function TEngineNetMachine.Init(localport: UInt16; callback: TUdpPacketHandler): boolean;
begin
	udpsock := SDLNet_UDP_Open(localport);
	active := udpsock <> nil;
	Result := active;
	if active then
	begin
		thread := SDL_CreateThread(TSDL_ThreadFunction(@NetThreadFunc),
			PChar(IntToStr(localport) + 'UDP'), @Self);
		cb := callback;
	end
	else
	begin
		udpsock := nil;
		writeln('ouch! (lookup): ' + SDLNet_GetError);
	end;

	//if (SDLNet_ResolveHost(@remoteipaddr, PChar(remotehostname), remoteport) <> 0) and
	//  (SDLNet_ResolveHost(@localipaddr, nil, localport) <> 0) then;
	//Result := (remoteipaddr.host <> INADDR_NONE) and (localipaddr.host = INADDR_ANY);
end;

procedure TEngineNetMachine.Close;
begin
	if active then
		SDL_WaitThread(thread, @Result);
end;

end.

