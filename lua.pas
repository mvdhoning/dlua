(*
** $Id: lua.h,v 1.154 2002/08/12 17:23:12 roberto Exp $
** Lua - An Extensible Extension Language
** TeCGraf: Computer Graphics Technology Group, PUC-Rio, Brazil
** http://www.lua.org   mailto:info@lua.org
** See Copyright Notice at the end of this file
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
**    - lua_upvalueindex constant was transformed to function
**    - Warning : You must init lualib if you want to use ref compatiblity
**        macros.
** Bugs report :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)
(*
** Converted pascal unit from static linking to dynamic loading by M van der Honing
*)
unit lua;

interface

// defines to configure freepascal
{$IFDEF FPC}
  {$MODE Delphi}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

const
{$IFDEF LINUX}
  LUA_NAME = 'liblua.so';
  LUA_LIB_NAME = 'liblualib.so';
{$ELSE}
  //LUA_NAME = 'lua.dll';
  //LUA_LIB_NAME = 'lualib.dll';
  LUA_NAME = 'lua5.3.0.dll';
  LUA_LIB_NAME = 'lua5.3.0.dll';
{$ENDIF}

function LoadLua: Boolean;
function LoadLuaFrom(FileName: string): Boolean;
procedure UnLoadLua;
function LuaLoaded: Boolean;

function LoadLuaLib: Boolean;
function LoadLuaLibFrom(FileName: string): Boolean;
procedure UnLoadLuaLib;
function LuaLibLoaded: Boolean;

(*==============================================================================
    LUA.PAS
==============================================================================*)
type
  size_t = Cardinal;
  Psize_t = ^size_t;
  Pinteger = ^integer;
  PPchar = ^pchar;

const
  LUA_VERSION = 'Lua 5.0 (alpha)';
  LUA_COPYRIGHT = 'Copyright (C) 1994-2002 TeCGraf, PUC-Rio';
  LUA_AUTHORS = 'W. Celes, R. Ierusalimschy & L. H. de Figueiredo';

(* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;

(* maximum stack available.
 * CHANGE it if you need a different limit. This limit is arbitrary;
 * its only purpose is to stop Lua to consume unlimited stack
 * space (and to reserve some numbers for pseudo-indices).
 *)
{$IFDEF CPU64}
LUAI_MAXSTACK         = 1000000;
{$ELSE}
LUAI_MAXSTACK         = 1000000; //C value : 15000
{$ENDIF}

LUAI_FIRSTPSEUDOIDX   = (-LUAI_MAXSTACK - 1000);

(* pseudo-indices *)
LUA_REGISTRYINDEX     = LUAI_FIRSTPSEUDOIDX;

  //LUA_REGISTRYINDEX = -10000;
  LUA_GLOBALSINDEX = -10001;

function lua_upvalueindex(I: Integer): Integer;

const
(* error codes for `lua_load' and `lua_pcall' *)
  LUA_ERRRUN    = 1;
  LUA_ERRFILE   = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRERR    = 5;
  LUA_ERRTHROW  = 6;

type
  Plua_State = Pointer;

  lua_CFunction = function(L: Plua_State): Integer; cdecl;

(*
** functions that read blocks when loading Lua chunk
*)
type
  lua_Chunkreader = function(L: Plua_State; ud: Pointer; size: PInteger): PChar;

(*
** basic types
*)
const
  LUA_TNONE          = -1;

  LUA_TNIL           = 0;
  LUA_TNUMBER        = 1;
  LUA_TSTRING        = 2;
  LUA_TBOOLEAN       = 3;
  LUA_TTABLE         = 4;
  LUA_TFUNCTION      = 5;
  LUA_TUSERDATA      = 6;
  LUA_TLIGHTUSERDATA = 7;

(* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

type
(* Type of Numbers in Lua *)
  lua_Number = Double;

  function lua_open: Plua_State;

var
(*
** state manipulation
*)
  //lua_open: function: Plua_state; cdecl;
  lua_close: procedure(L: Plua_State); cdecl;
  lua_newthread: function(L: Plua_State): Plua_State; cdecl;
  lua_closethread: procedure(L: Plua_State; thread: Plua_State); cdecl;

  lua_atpanic: function(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

(*
** basic stack manipulation
*)
  lua_gettop: function(L: Plua_State): Integer; cdecl;
  lua_settop: procedure(L: Plua_State; index: Integer); cdecl;
  lua_pushvalue: procedure(L: Plua_State; Index: Integer); cdecl;
  lua_remove: procedure(L: Plua_State; index: Integer); cdecl;
  lua_insert: procedure(L: Plua_State; index: Integer); cdecl;
  lua_replace: procedure(L: Plua_State; index: Integer); cdecl;
  lua_checkstack: function(L: Plua_State; size: Integer): LongBool; cdecl;

(*
** access functions (stack -> C)
*)
  lua_isnumber: function(L: Plua_State; index: Integer): LongBool; cdecl;
  lua_isstring: function(L: Plua_State; index: Integer): LongBool; cdecl;
  lua_iscfunction: function(L: Plua_State; index: Integer): LongBool; cdecl;
  lua_type: function(L: Plua_State; index: Integer): Integer; cdecl;
  lua_typename: function(L: Plua_State; ltype: Integer): PChar; cdecl;

  lua_equal: function(L: Plua_State; index1, index2: Integer): LongBool; cdecl;
  lua_rawequal: function(L: Plua_State; index1, index2: Integer): LongBool; cdecl;
  lua_lessthan: function(L: Plua_State; index1, index2: Integer): LongBool; cdecl;

  //lua_tonumber: function(L: Plua_State; index: Integer): lua_Number; cdecl;
  lua_toboolean: function(L: Plua_State; index: Integer): LongBool; cdecl;
  //lua_tostring: function(L: Plua_State; index: Integer): PChar; cdecl;
  lua_strlen: function(L: Plua_State; index: Integer): size_t; cdecl;
  lua_tocfunction: function(L: Plua_State; index: Integer): lua_CFunction; cdecl;
  lua_touserdata: function(L: Plua_State; index: Integer): Pointer; cdecl;
  lua_topointer: function(L: Plua_State; index: Integer): Pointer; cdecl;

(*
** push functions (C -> stack)
*)
  lua_pushnil: procedure(L: Plua_State); cdecl;
  lua_pushnumber: procedure(L: Plua_State; n: lua_Number); cdecl;
  lua_pushlstring: procedure(L: Plua_State; const s: PChar; len: size_t); cdecl;
  lua_pushstring: procedure(L: Plua_State; const s: PChar); cdecl;
  lua_pushvfstring: function(L: Plua_State; const fmt: PChar; argp: Pointer): PChar; cdecl;
  lua_pushfstring: function(L: Plua_State; const fmt: PChar; arg: array of Pointer): PChar; cdecl;
  lua_pushcclosure: procedure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
  lua_pushboolean: procedure(L: Plua_State; b: LongBool); cdecl;
  lua_pushlightuserdata: procedure(L: Plua_State; p: Pointer); cdecl;

(*
** get functions (Lua -> stack)
*)
  lua_gettable: procedure(L: Plua_State; index: Integer); cdecl;
  lua_rawget: procedure(L: Plua_State; index: Integer); cdecl;
  lua_rawgeti: procedure(L: Plua_State; index, n: Integer); cdecl;
  lua_newtable: procedure(L: Plua_State); cdecl;
  lua_getmetatable: function(L: Plua_State; objindex: Integer): Integer; cdecl;
  lua_getmode: function(L: Plua_State; index: Integer): PChar; cdecl;
  lua_getglobals: procedure(L: Plua_State; index: Integer); cdecl;

(*
** set functions (stack -> Lua)
*)
  lua_settable: procedure(L: Plua_State; index: Integer); cdecl;
  lua_rawset: procedure(L: Plua_State; index: Integer); cdecl;
  lua_rawseti: procedure(L: Plua_State; index, n: Integer); cdecl;
  lua_setmode: procedure(L: Plua_State; index: Integer; const mode: PChar); cdecl;
  lua_setmetatable: procedure(L: Plua_State; objindex: Integer); cdecl;
  lua_setglobals: procedure(L: Plua_State; index: Integer); cdecl;

(*
** `load' and `call' functions (load and run Lua code)
*)
  lua_call: procedure(L: Plua_State; nargs, nresults: Integer); cdecl;
  //lua_pcall: function(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
  lua_pcall       : function (L : Plua_State; nargs, nresults, errfunc : integer;
                                 ctx : integer = 0; k : lua_CFunction = nil) : integer; cdecl;
  lua_load: function(L: Plua_State; reader: lua_Chunkreader; data: Pointer; const chunkname: PChar): Integer; cdecl;

(*
** coroutine functions
*)
  lua_cobegin: procedure(L: Plua_State; args: Integer); cdecl;
  lua_yield: function(L: Plua_State; nresults: Integer): Integer; cdecl;
  lua_resume: function(L: Plua_State; co: Plua_State): Integer; cdecl;

(*
** Garbage-collection functions
*)
  lua_getgcthreshold: function(L: Plua_State): Integer; cdecl;
  lua_getgccount: function(L: Plua_State): Integer; cdecl;
  lua_setgcthreshold: procedure(L: Plua_State; newthreshold: Integer); cdecl;

(*
** miscellaneous functions
*)
  lua_error: function(L: Plua_State): Integer; cdecl;

  lua_next: function(L: Plua_State; index: Integer): Integer; cdecl;

  lua_concat: procedure(L: Plua_State; n: Integer); cdecl;

  lua_newuserdata: function(L: Plua_State; size: size_t): Pointer; cdecl;

  //5.0
   lua_setfield: procedure(L: Plua_State; idx: Integer; k: PChar); cdecl;

   lua_setglobal: procedure(L : Plua_State; var_ : PChar); cdecl;
   lua_getglobal: function(L: Plua_State; const name: PChar): Integer; cdecl;

   luaL_tolstring    : function (L : Plua_State; idx : integer; var len : size_t) : PAnsiChar; cdecl;
   lua_tolstring   : function (L : Plua_State; idx : integer; len : size_t) : PAnsiChar; cdecl;
   lua_getfield    : procedure(L : Plua_State; idx : integer; k : PAnsiChar); cdecl;
   lua_tonumberx   : function (L : Plua_State; idx : integer; isnum : PInteger) : lua_Number; cdecl;

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

procedure lua_boxpointer(L: Plua_State; u: Pointer);

function lua_unboxpointer(L: Plua_State; i: Integer): Pointer;

procedure lua_pop(L: Plua_State; n: Integer);

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);

function lua_isfunction(L: Plua_State; n: Integer): Boolean;
function lua_istable(L: Plua_State; n: Integer): Boolean;
function lua_isuserdata(L: Plua_State; n: Integer): Boolean;
function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
function lua_isnil(L: Plua_State; n: Integer): Boolean;
function lua_isboolean(L: Plua_State; n: Integer): Boolean;
function lua_isnone(L: Plua_State; n: Integer): Boolean;
function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;

procedure lua_pushliteral(L: Plua_State; s: PChar);


(*
** compatibility macros and functions
*)
var
  lua_pushupvalues: procedure(L: Plua_State); cdecl;

procedure lua_getregistry(L: Plua_State);
//procedure lua_setglobal(L: Plua_State; const s: PChar);
//procedure lua_getglobal(L: Plua_State; const s: PChar);

function isnull(L: Plua_State; n: Integer): Boolean;

(* compatibility with ref system*)

(* pre-defined references *)
const
  LUA_NOREF = -2;
  LUA_REFNIL = -1;

function lua_ref(L: Plua_State; lock: Integer): Integer;
procedure lua_unref(L: Plua_State; ref: Integer);
procedure lua_getref(L: Plua_State; ref: Integer);

(*
** {======================================================================
** Debug API
** =======================================================================
*)
type
  lua_Hookevent = (LUA_HOOKCALL, LUA_HOOKRET, LUA_HOOKLINE, LUA_HOOKCOUNT);

const
  LUA_MASKCALL = 2 shl Ord(LUA_HOOKCALL);
  LUA_MASKRET  = 2 shl Ord(LUA_HOOKRET);
  LUA_MASKLINE = 2 shl Ord(LUA_HOOKLINE);

function LUA_MASKCOUNT(count: LongWord): LongWord;
function lua_getmaskcount(mask: LongWord): LongWord;

const
  LUA_MAXCOUNT = (1 shl 24) - 1;

  LUA_IDSIZE = 60;

type
  lua_Debug = record           (* activation record *)
    event: lua_Hookevent;
    name: PChar;               (* (n) *)
    namewhat: PChar;           (* (n) `global', `local', `field', `method' *)
    what: PChar;               (* (S) `Lua' function, `C' function, Lua `main' *)
    source: PChar;             (* (S) *)
    currentline: Integer;      (* (l) *)
    nups: Integer;             (* (u) number of upvalues *)
    linedefined: Integer;      (* (S) *)
    short_src: array[0..LUA_IDSIZE - 1] of Char; (* (S) *)
    (* private part *)
    i_ci: Integer;              (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

var
  lua_getstack: function(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
  lua_getinfo: function(L: Plua_State; const what: PChar; ar: Plua_Debug): Integer; cdecl;
  lua_getlocal: function(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl;
  lua_setlocal: function(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl;

  lua_sethook: function(L: Plua_State; func: lua_Hook; mask: LongWord): Integer; cdecl;
  lua_gethook: function(L: Plua_State): lua_hook; cdecl;
  lua_gethookmask: function(L: Plua_State): LongWord; cdecl;

(*==============================================================================
    LUALIB.PAS
==============================================================================*)
const
  LUA_COLIBNAME = 'coroutine';

var
  luaopen_base: function(L: Plua_State): LongBool; cdecl;

const
  LUA_TABLIBNAME = 'table';

var
  luaopen_table: function(L: Plua_State): LongBool; cdecl;

const
  LUA_IOLIBNAME = 'io';
  LUA_OSLIBNAME = 'os';

var
  luaopen_io: function(L: Plua_State): LongBool; cdecl;

const
  LUA_STRLINAME = 'string';

var
  luaopen_string: function(L: Plua_State): LongBool; cdecl;

const
  LUA_MATHLIBNAME = 'math';

var
  luaopen_math: function(L: Plua_State): LongBool; cdecl;

const
  LUA_DBLIBNAME = 'debug';

var
  luaopen_debug: function(L: Plua_State): LongBool; cdecl;

(*==============================================================================
    LAUXLIB.PAS
==============================================================================*)
type
  luaL_reg = record
    name: PChar;
    func: lua_CFunction;
  end;
  PluaL_reg = ^luaL_reg;

  procedure luaL_openlib(L: Plua_State; const n: PChar; lr: PluaL_reg; nup: Integer); inline;

var
  //luaL_openlib: procedure(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer); cdecl;




  //luaL_openlib: procedure(L: Plua_State; const lr: PluaL_reg; nup: Integer); cdecl;
  //luaL_opennamedlib: procedure(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer); cdecl;
  luaL_callmeta: function(L: Plua_State; obj: Integer; const event: PChar): Integer; cdecl;
  luaL_typerror: function(L: Plua_State; narg: Integer; const tname: PChar): Integer; cdecl;
  luaL_argerror: function(L: Plua_State; numarg: Integer; const extramsg: PChar): Integer; cdecl;
  luaL_check_lstr: function(L: Plua_State; numArg: Integer; len: Psize_t): PChar; cdecl;
  luaL_opt_lstr: function(L: Plua_State; numArg: Integer; const def: PChar; len: Psize_t): PChar; cdecl;
  luaL_check_number: function(L: Plua_State; numArg: Integer): lua_Number; cdecl;
  luaL_opt_number: function(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl;

  luaL_check_stack: procedure(L: Plua_State; space: Integer; const msg: PChar); cdecl;
  luaL_check_type: procedure(L: Plua_State; narg, t: Integer); cdecl;
  luaL_check_any: procedure(L: Plua_State; narg: Integer); cdecl;

  luaL_where: procedure(L: Plua_State; level: Integer); cdecl;
  luaL_error: function(L: Plua_State; const fmt: PChar; argsup: array of pointer): Integer; cdecl;

  luaL_findstring: function(name: PChar; const list: PPChar): Integer; cdecl;

  luaL_ref: function(L: Plua_State; t: Integer): Integer; cdecl;
  luaL_unref: procedure(L: Plua_State; t, ref: Integer); cdecl;

  luaL_loadfile: function(L: Plua_State; const filename: PChar): Integer; cdecl;

  luaL_loadbuffer   : function (L : Plua_State; buff : PAnsiChar; size : size_t;
                                   name : PAnsiChar; mode : PAnsiChar = nil) : integer; cdecl;
  luaL_loadbufferx  : function (L : Plua_State; buff : PAnsiChar; size : size_t;
                                   name : PAnsiChar; mode : PAnsiChar) : integer; cdecl;
  //luaL_loadbuffer: function(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl;

  //5.0
  luaL_setfuncs: procedure(L: Plua_State; lr: PluaL_Reg; nup: Integer); cdecl;

  luaL_newstate: function: Plua_State; cdecl;
  luaL_newmetatable: function(L: Plua_State; const tname: PChar): Integer; cdecl;
  luaL_setmetatable: procedure(L: Plua_State; const tname: PChar); cdecl;

  procedure luaL_getmetatable(L : Plua_State; n : PAnsiChar); inline;
   procedure luaL_register(L : Plua_State; n : PChar; lib : PLuaL_Reg); inline;
   function lua_tostring(L : Plua_State; i : integer) : PAnsiChar; inline;
  function lua_tonumber(L : Plua_State; i : integer) : lua_Number; inline;


(*
** ===============================================================
** some useful macros
** ===============================================================
*)

procedure luaL_arg_check(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
function luaL_check_string(L: Plua_State; n: Integer): PChar;
function luaL_opt_string(L: Plua_State; n: Integer; d: PChar): PChar;
function luaL_check_int(L: Plua_State; n: Integer): Integer;
function luaL_check_long(L: Plua_State; n: Integer): LongInt;
function luaL_opt_int(L: Plua_State; n: Integer; d: Double): Integer;
function luaL_opt_long(L: Plua_State; n: Integer; d: Double): LongInt;

(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

const
  LUAL_BUFFERSIZE = 4096;

type
  luaL_Buffer = record
    p: PChar;
    level: Integer;
    L: Plua_State;
    buffer: array [0..LUAL_BUFFERSIZE - 1] of Char;
  end;
  PluaL_Buffer = ^luaL_Buffer;

procedure luaL_putchar(B: PluaL_Buffer; c: Char);

procedure luaL_addsize(B: PluaL_Buffer; n: Integer);

var
  luaL_buffinit: procedure(L: Plua_State ; B: PluaL_Buffer); cdecl;
  luaL_prepbuffer: function(B: PluaL_Buffer): PChar; cdecl;
  luaL_addlstring: procedure(B: PluaL_Buffer; const s: PChar; l: size_t); cdecl;
  luaL_addstring: procedure(B: PluaL_Buffer; const s: PChar); cdecl;
  luaL_addvalue: procedure(B: PluaL_Buffer); cdecl;
  luaL_pushresult: procedure(B: PluaL_Buffer); cdecl;

(*
** Compatibility macros
*)

procedure luaL_checktype(L: Plua_State; narg, t: Integer);
procedure luaL_checkany(L: Plua_State; narg: Integer);

var
  lua_dofile: function(L: Plua_State; const filename: PChar): Integer; cdecl;
  lua_dostring: function(L: Plua_State; const str: PChar): Integer; cdecl;
  lua_dobuffer: function(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl;

implementation

uses
{$IFDEF LINUX}
  LibC,
{$ELSE}
  Windows;
{$ENDIF}

{$IFDEF LINUX}
const
  INVALID_MODULE_HANDLE = nil;

type
  LibHandle = Pointer;

function OpenLib(Name: PChar): LibHandle;
begin
  Result := dlopen(Name);
end;
{$ELSE}
const
  INVALID_MODULE_HANDLE = 0;

type
  LibHandle = HINST;

function OpenLib(Name: PChar): LibHandle;
begin
  Result := LoadLibrary(Name);
end;
{$ENDIF}

var
  LuaHandle: LibHandle = INVALID_MODULE_HANDLE;
  LuaLibHandle: LibHandle = INVALID_MODULE_HANDLE;

  function lua_open: Plua_State;
  begin
       Result :=luaL_newstate;
  end;

procedure ClearLuaProc;
begin
  //lua_open := nil;
  lua_close := nil;
  lua_newthread := nil;
  lua_closethread := nil;
  lua_atpanic := nil;
  lua_gettop := nil;
  lua_settop := nil;
  lua_pushvalue := nil;
  lua_remove := nil;
  lua_insert := nil;
  lua_replace := nil;
  lua_checkstack := nil;
  lua_isnumber := nil;
  lua_isstring := nil;
  lua_iscfunction := nil;
  lua_type := nil;
  lua_typename := nil;
  lua_equal := nil;
  lua_rawequal := nil;
  lua_lessthan := nil;
  lua_tonumberx := nil;
  lua_toboolean := nil;

  lua_strlen := nil;
  lua_tocfunction := nil;
  lua_touserdata := nil;
  lua_topointer := nil;
  lua_pushnil := nil;
  lua_pushnumber := nil;
  lua_pushlstring := nil;
  lua_pushstring := nil;
  lua_pushvfstring := nil;
  lua_pushfstring := nil;
  lua_pushcclosure := nil;
  lua_pushboolean := nil;
  lua_pushlightuserdata := nil;
  lua_gettable := nil;
  lua_rawget := nil;
  lua_rawgeti := nil;
  lua_newtable := nil;
  lua_getmetatable := nil;
  lua_getmode := nil;
  lua_getglobals := nil;
  lua_settable := nil;
  lua_rawset := nil;
  lua_rawseti := nil;
  lua_setmode := nil;
  lua_setmetatable := nil;
  lua_setglobals := nil;
  lua_call := nil;
  lua_pcall := nil;
  lua_load := nil;
  lua_cobegin := nil;
  lua_yield := nil;
  lua_resume := nil;
  lua_getgcthreshold := nil;
  lua_getgccount := nil;
  lua_setgcthreshold := nil;
  lua_error := nil;
  lua_next := nil;
  lua_concat := nil;
  lua_newuserdata := nil;
  lua_pushupvalues := nil;
  lua_getstack := nil;
  lua_getinfo := nil;
  lua_getlocal := nil;
  lua_setlocal := nil;
  lua_gethook := nil;
  lua_sethook := nil;
  lua_gethookmask := nil;

  lua_setfield := nil;
  lua_getfield := nil;
  lua_setglobal := nil;
  lua_getglobal := nil;

  lua_tolstring := nil;
end;

procedure LoadLuaProc;
begin
  if LuaHandle <> INVALID_MODULE_HANDLE then
  begin
    //lua_open := GetProcAddress(LuaHandle, 'lua_open');
    lua_close := GetProcAddress(LuaHandle, 'lua_close');
    lua_newthread := GetProcAddress(LuaHandle, 'lua_newthread');
    lua_closethread := GetProcAddress(LuaHandle, 'lua_closethread');
    lua_atpanic := GetProcAddress(LuaHandle, 'lua_atpanic');
    lua_gettop := GetProcAddress(LuaHandle, 'lua_gettop');
    lua_settop := GetProcAddress(LuaHandle, 'lua_settop');
    lua_pushvalue := GetProcAddress(LuaHandle, 'lua_pushvalue');
    lua_remove := GetProcAddress(LuaHandle, 'lua_remove');
    lua_insert := GetProcAddress(LuaHandle, 'lua_insert');
    lua_replace := GetProcAddress(LuaHandle, 'lua_replace');
    lua_checkstack := GetProcAddress(LuaHandle, 'lua_checkstack');
    lua_isnumber := GetProcAddress(LuaHandle, 'lua_isnumber');
    lua_isstring := GetProcAddress(LuaHandle, 'lua_isstring');
    lua_iscfunction := GetProcAddress(LuaHandle, 'lua_iscfunction');
    lua_type := GetProcAddress(LuaHandle, 'lua_type');
    lua_typename := GetProcAddress(LuaHandle, 'lua_typename');
    lua_equal := GetProcAddress(LuaHandle, 'lua_equal');
    lua_rawequal := GetProcAddress(LuaHandle, 'lua_rawequal');
    lua_lessthan := GetProcAddress(LuaHandle, 'lua_lessthan');
    lua_tonumberx := GetProcAddress(LuaHandle, 'lua_tonumberx');
    lua_toboolean := GetProcAddress(LuaHandle, 'lua_toboolean');

    lua_strlen := GetProcAddress(LuaHandle, 'lua_strlen');
    lua_tocfunction := GetProcAddress(LuaHandle, 'lua_tocfunction');
    lua_touserdata := GetProcAddress(LuaHandle, 'lua_touserdata');
    lua_topointer := GetProcAddress(LuaHandle, 'lua_topointer');
    lua_pushnil := GetProcAddress(LuaHandle, 'lua_pushnil');
    lua_pushnumber := GetProcAddress(LuaHandle, 'lua_pushnumber');
    lua_pushlstring := GetProcAddress(LuaHandle, 'lua_pushlstring');
    lua_pushstring := GetProcAddress(LuaHandle, 'lua_pushstring');
    lua_pushvfstring := GetProcAddress(LuaHandle, 'lua_pushvfstring');
    lua_pushfstring := GetProcAddress(LuaHandle, 'lua_pushfstring');
    lua_pushcclosure := GetProcAddress(LuaHandle, 'lua_pushcclosure');
    lua_pushboolean := GetProcAddress(LuaHandle, 'lua_pushboolean');
    lua_pushlightuserdata := GetProcAddress(LuaHandle, 'lua_pushlightuserdata');
    lua_gettable := GetProcAddress(LuaHandle, 'lua_gettable');
    lua_rawget := GetProcAddress(LuaHandle, 'lua_rawget');
    lua_rawgeti := GetProcAddress(LuaHandle, 'lua_rawgeti');
    lua_newtable := GetProcAddress(LuaHandle, 'lua_newtable');
    lua_getmetatable := GetProcAddress(LuaHandle, 'lua_getmetatable');
    lua_getmode := GetProcAddress(LuaHandle, 'lua_getmode');
    //lua_getglobals := GetProcAddress(LuaHandle, 'lua_getglobals');
    lua_settable := GetProcAddress(LuaHandle, 'lua_settable');
    lua_rawset := GetProcAddress(LuaHandle, 'lua_rawset');
    lua_rawseti := GetProcAddress(LuaHandle, 'lua_rawseti');
    lua_setmode := GetProcAddress(LuaHandle, 'lua_setmode');
    lua_setmetatable := GetProcAddress(LuaHandle, 'lua_setmetatable');
    lua_setglobals := GetProcAddress(LuaHandle, 'lua_setglobals');
    lua_call := GetProcAddress(LuaHandle, 'lua_call');
    lua_pcall := GetProcAddress(LuaHandle, 'lua_pcallk'); //subtle rename
    lua_load := GetProcAddress(LuaHandle, 'lua_load');
    lua_cobegin := GetProcAddress(LuaHandle, 'lua_cobegin');
    lua_yield := GetProcAddress(LuaHandle, 'lua_yield');
    lua_resume := GetProcAddress(LuaHandle, 'lua_resume');
    lua_getgcthreshold := GetProcAddress(LuaHandle, 'lua_getgcthreshold');
    lua_getgccount := GetProcAddress(LuaHandle, 'lua_getgccount');
    lua_setgcthreshold := GetProcAddress(LuaHandle, 'lua_setgcthreshold');
    lua_error := GetProcAddress(LuaHandle, 'lua_error');
    lua_next := GetProcAddress(LuaHandle, 'lua_next');
    lua_concat := GetProcAddress(LuaHandle, 'lua_concat');
    lua_newuserdata := GetProcAddress(LuaHandle, 'lua_newuserdata');
    lua_pushupvalues := GetProcAddress(LuaHandle, 'lua_pushupvalues');
    lua_getstack := GetProcAddress(LuaHandle, 'lua_getstack');
    lua_getinfo := GetProcAddress(LuaHandle, 'lua_getinfo');
    lua_getlocal := GetProcAddress(LuaHandle, 'lua_getlocal');
    lua_setlocal := GetProcAddress(LuaHandle, 'lua_setlocal');
    lua_gethook := GetProcAddress(LuaHandle, 'lua_gethook');
    lua_sethook := GetProcAddress(LuaHandle, 'lua_sethook');
    lua_gethookmask := GetProcAddress(LuaHandle, 'lua_gethookmask');

    //5.0
    lua_setfield := GetProcAddress(LuaHandle, 'lua_setfield');
    lua_getfield := GetProcAddress(LuaHandle, 'lua_getfield');
    lua_setglobal := GetProcAddress(LuaHandle, 'lua_setglobal');
    lua_getglobal := GetProcAddress(LuaHandle, 'lua_getglobal');

    lua_tolstring := GetProcAddress(LuaHandle, 'lua_tolstring');
  end;
end;

function LoadLua: Boolean;
begin
  if LuaHandle = INVALID_MODULE_HANDLE then
    Result := LoadLuaFrom(LUA_NAME)
  else
    Result := True
end;

function LoadLuaFrom(FileName: string): Boolean;
begin
  ClearLuaProc;
  LuaHandle := OpenLib(PChar(FileName));
  if LuaHandle <> INVALID_MODULE_HANDLE then
  begin
    LoadLuaProc;
    Result := True
  end
  else
    Result := False
end;

procedure UnLoadLua;
begin
  if LuaHandle <> INVALID_MODULE_HANDLE then
    FreeLibrary(LuaHandle);
  LuaHandle := INVALID_MODULE_HANDLE;
  ClearLuaProc;
end;

function LuaLoaded: Boolean;
begin
  Result := LuaHandle <> INVALID_MODULE_HANDLE
end;

procedure ClearLuaLibProc;
begin
  luaopen_base := nil;
  luaopen_table := nil;
  luaopen_io := nil;
  luaopen_string := nil;
  luaopen_math := nil;
  luaopen_debug := nil;
  //luaL_openlib := nil;

//  luaL_opennamedlib := nil;
  luaL_callmeta := nil;
  luaL_typerror := nil;
  luaL_argerror := nil;
  luaL_check_lstr := nil;
  luaL_opt_lstr := nil;
  luaL_check_number := nil;
  luaL_opt_number := nil;
  luaL_check_stack := nil;
  luaL_check_type := nil;
  luaL_check_any := nil;
  luaL_where := nil;
  luaL_error := nil;
  luaL_findstring := nil;
  luaL_ref := nil;
  luaL_unref := nil;
  luaL_loadfile := nil;
  luaL_loadbuffer := nil;
  luaL_loadbufferx := nil;
  luaL_buffinit := nil;
  luaL_prepbuffer := nil;
  luaL_addlstring := nil;
  luaL_addstring := nil;
  luaL_addvalue := nil;
  luaL_pushresult := nil;
  lua_dofile := nil;
  lua_dostring := nil;
  lua_dobuffer := nil;

  luaL_newstate:= nil;
  luaL_newmetatable := nil;
  luaL_setmetatable := nil;
  luaL_tolstring := nil;

  luaL_setfuncs := nil;
end;

procedure LoadLuaLibProc;
begin
  if LuaLibHandle <> INVALID_MODULE_HANDLE then
  begin
    luaopen_base := GetProcAddress(LuaLibHandle, 'luaopen_base');
    luaopen_table := GetProcAddress(LuaLibHandle, 'luaopen_table');
    luaopen_io := GetProcAddress(LuaLibHandle, 'luaopen_io');
    luaopen_string := GetProcAddress(LuaLibHandle, 'luaopen_string');
    luaopen_math := GetProcAddress(LuaLibHandle, 'luaopen_math');
    luaopen_debug := GetProcAddress(LuaLibHandle, 'luaopen_debug');
    //luaL_openlib := GetProcAddress(LuaLibHandle, 'luaL_openlib');

//    luaL_opennamedlib := GetProcAddress(LuaLibHandle, 'luaL_opennamedlib');
    luaL_callmeta := GetProcAddress(LuaLibHandle, 'luaL_callmeta');
    luaL_typerror := GetProcAddress(LuaLibHandle, 'luaL_typerror');
    luaL_argerror := GetProcAddress(LuaLibHandle, 'luaL_argerror');
    luaL_check_lstr := GetProcAddress(LuaLibHandle, 'luaL_check_lstr');
    luaL_opt_lstr := GetProcAddress(LuaLibHandle, 'luaL_opt_lstr');
    luaL_check_number := GetProcAddress(LuaLibHandle, 'luaL_check_number');
    luaL_opt_number := GetProcAddress(LuaLibHandle, 'luaL_opt_number');
    luaL_check_stack := GetProcAddress(LuaLibHandle, 'luaL_check_stack');
    luaL_check_type := GetProcAddress(LuaLibHandle, 'luaL_check_type');
    luaL_check_any := GetProcAddress(LuaLibHandle, 'luaL_check_any');
    luaL_where := GetProcAddress(LuaLibHandle, 'luaL_where');
    luaL_error := GetProcAddress(LuaLibHandle, 'luaL_error');
    luaL_findstring := GetProcAddress(LuaLibHandle, 'luaL_findstring');
    luaL_ref := GetProcAddress(LuaLibHandle, 'luaL_ref');
    luaL_unref := GetProcAddress(LuaLibHandle, 'luaL_unref');
    luaL_loadfile := GetProcAddress(LuaLibHandle, 'luaL_loadfile');
    luaL_loadbuffer         := GetProcAddress(LuaLibHandle, 'luaL_loadbufferx');
    luaL_loadbufferx        := luaL_loadbuffer;

    luaL_buffinit := GetProcAddress(LuaLibHandle, 'luaL_buffinit');
    luaL_prepbuffer := GetProcAddress(LuaLibHandle, 'luaL_prepbuffer');
    luaL_addlstring := GetProcAddress(LuaLibHandle, 'luaL_addlstring');
    luaL_addstring := GetProcAddress(LuaLibHandle, 'luaL_addstring');
    luaL_addvalue := GetProcAddress(LuaLibHandle, 'luaL_addvalue');
    luaL_pushresult := GetProcAddress(LuaLibHandle, 'luaL_pushresult');
    lua_dofile := GetProcAddress(LuaLibHandle, 'lua_dofile');
    lua_dostring := GetProcAddress(LuaLibHandle, 'lua_dostring');
    lua_dobuffer := GetProcAddress(LuaLibHandle, 'lua_dobuffer');

    writeln('x');
    //5.0
    luaL_newstate:= GetProcAddress(LuaLibHandle, 'luaL_newstate');
    luaL_newmetatable := GetProcAddress(LuaLibHandle, 'luaL_newmetatable');

    luaL_setmetatable := GetProcAddress(LuaLibHandle, 'luaL_setmetatable');

    luaL_tolstring := GetProcAddress(LuaLibHandle,'luaL_tolstring');

    luaL_setfuncs := GetProcAddress(LuaLibHandle,'luaL_setfuncs');

    writeln('y');
  end;
end;

procedure luaL_register(L : Plua_State; n : PChar; lib : PLuaL_Reg); inline;
begin
   luaL_openlib(L,n,lib,0);
end;

procedure luaL_getmetatable(L : Plua_State; n : PAnsiChar); inline;
begin
  write('x1');
  if (@lua_getfield = nil) then write('pfff');
     lua_getfield(L, LUA_REGISTRYINDEX, n);
     write('x2')
end;

function lua_tostring(L : Plua_State; i : integer) : PAnsiChar; inline;
begin
     Result := lua_tolstring(L, i, 0);
end;

function lua_tonumber(L : Plua_State; i : integer) : lua_Number; inline;
begin
     Result := lua_tonumberx(L,i,nil);
end;

procedure luaL_openlib(L: Plua_State; const n: PChar; lr: PluaL_reg; nup: Integer); inline;
begin

  writeln('ol1');
  (*
  if n <> nil then begin //if empty then dont call setglobal
  lua_getglobal(L, n);
  writeln('ol2');
  if (lua_isnil(L, -1)) then begin
    writeln('ol2.1');
    lua_pop(L, 1);
    writeln('ol2.2');
    lua_newtable(L);
    writeln('ol2.3');
  end;
  writeln('ol3');
  end;
  *)
  luaL_setfuncs(L, lr, 0);
  writeln('ol4');
  if n <> nil then //if empty then dont call setglobal
  lua_setglobal(L, n);
  writeln('ol5');
end;

function LoadLuaLib: Boolean;
begin
  if LuaLibHandle = INVALID_MODULE_HANDLE then
    Result := LoadLuaLibFrom(LUA_LIB_NAME)
  else
    Result := True
end;

function LoadLuaLibFrom(FileName: string): Boolean;
begin
  ClearLuaLibProc;
  LuaLibHandle := OpenLib(PChar(FileName));
  if LuaLibHandle <> INVALID_MODULE_HANDLE then
  begin
    LoadLuaLibProc;
    Result := True
  end
  else
    Result := False
end;

procedure UnLoadLuaLib;
begin
  if LuaLibHandle <> INVALID_MODULE_HANDLE then
    FreeLibrary(LuaLibHandle);
  LuaLibHandle := INVALID_MODULE_HANDLE;
  ClearLuaLibProc
end;

function LuaLibLoaded: boolean;
begin
  Result := LuaLibHandle <> INVALID_MODULE_HANDLE;
end;

(*==============================================================================
    LUA.PAS
==============================================================================*)
function lua_upvalueindex(I: Integer): Integer;
begin
  Result := LUA_GLOBALSINDEX - i;
end;

procedure lua_boxpointer(L: Plua_State; u: Pointer);
type
  PPointer = ^Pointer;
begin
  PPointer(lua_newuserdata(L, SizeOf(Pointer)))^ := u;
end;

function lua_unboxpointer(L: Plua_State; i: Integer): Pointer;
type
  PPointer = ^Pointer;
begin
  Result := PPointer(lua_touserdata(L, i))^;
end;

procedure lua_pop(L: Plua_State; n: Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
begin
  //lua_pushstring(L, n);
  //lua_pushcfunction(L, f);
  //lua_settable(L, LUA_GLOBALSINDEX);
  lua_pushcfunction(L,f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_isfunction(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_isuserdata(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) >= LUA_TUSERDATA;
end;

function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isnone(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L: Plua_State; s: PChar);
begin
  lua_pushlstring(L, s, (SizeOf(s) div SizeOf(Char)) - 1);
end;

procedure lua_getregistry(L: Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

//procedure lua_setglobal(L: Plua_State; const s: PChar);
//begin
//  lua_pushstring(L, s);
//  lua_insert(L, -2);
//  lua_settable(L, LUA_GLOBALSINDEX);
//end;

//procedure lua_getglobal(L: Plua_State; const s: PChar);
//begin
//  lua_pushstring(L, s);
//  lua_gettable(L, LUA_GLOBALSINDEX);
//end;

function isnull(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_isnone(L, n);
end;

function lua_ref(L: Plua_State; lock: Integer): Integer;
begin
  Result := 0;
  if lock <> 0 then
  begin
    Result := luaL_ref(L, LUA_REGISTRYINDEX);
  end
  else
  begin
    lua_pushstring(L, PChar('unlocked references are obsolete'));
    lua_error(L);
  end;
end;

procedure lua_unref(L: Plua_State; ref: Integer);
begin
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_getref(L: Plua_State; ref: Integer);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
end;

(*
** {======================================================================
** Debug API
** =======================================================================
*)

function LUA_MASKCOUNT(count: LongWord): LongWord;
begin
  Result := count shl 8;
end;

function lua_getmaskcount(mask: LongWord): LongWord;
begin
  Result := mask shr 8;
end;

(*==============================================================================
    LAUXLIB.PAS
==============================================================================*)
procedure luaL_arg_check(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
begin
  if not cond then
    luaL_argerror(L, numarg, extramsg)
end;

function luaL_check_string(L: Plua_State; n: Integer): PChar;
begin
  Result := luaL_check_lstr(L, n, nil)
end;

function luaL_opt_string(L: Plua_State; n: Integer; d: PChar): PChar;
begin
  Result := luaL_opt_lstr(L, n, d, nil)
end;

function luaL_check_int(L: Plua_State; n: Integer): Integer;
begin
  Result := Integer(Trunc(luaL_check_number(L, n)))
end;

function luaL_check_long(L: Plua_State; n: Integer): LongInt;
begin
  Result := LongInt(Trunc(luaL_check_number(L, n)))
end;

function luaL_opt_int(L: Plua_State; n: Integer; d: Double): Integer;
begin
  Result := Integer(Trunc(luaL_opt_number(L, n, d)))
end;

function luaL_opt_long(L: Plua_State; n: Integer; d: Double): LongInt;
begin
  Result := LongInt(Trunc(luaL_opt_number(L, n, d)))
end;

procedure luaL_putchar(B: PluaL_Buffer; c: Char);
begin
  if Cardinal(@(B^.p)) < (Cardinal(@(B^.buffer[0])) + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B^.p[1] := c;
  B^.p := B^.p + 1;
end;

procedure luaL_addsize(B: PluaL_Buffer; n: Integer);
begin
  B^.p := B^.p + n;
end;

procedure luaL_checktype(L: Plua_State; narg, t: Integer);
begin
  luaL_checktype(L, narg, t);
end;

procedure luaL_checkany(L: Plua_State; narg: Integer);
begin
  luaL_check_any(L, narg);
end;

(******************************************************************************
* Copyright (C) 2002 Tecgraf, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
end.
