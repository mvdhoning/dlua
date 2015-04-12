(*
** $Id: lua.h,v 1.325 2014/12/26 17:24:27 roberto Exp $
** Lua - A Scripting Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*)

//Pascal unit with dynamic loading of lua lib by M van der Honing

unit lua;

interface

//Defines to configure freepascal
{$IFDEF FPC}
  {$MODE Delphi}
  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

const
{$IFDEF LINUX}
  LUA_NAME = 'liblua.so.5.3.0';
{$ELSE}
  LUA_NAME = 'lua5.3.0.dll';

{$ENDIF}

//Dynamic loading and unloading of Lua libs
function loadLua: boolean;
function loadLuaFrom(FileName: string): boolean;
procedure unLoadLua;
function isLuaLoaded: boolean;

//LuaConf

type
  size_t = cardinal;
  Psize_t = ^size_t;

const
  //maximum stack available.
  //CHANGE it if you need a different limit. This limit is arbitrary;
  //its only purpose is to stop Lua to consume unlimited stack
  //space (and to reserve some numbers for pseudo-indices)
  {$IFDEF CPU64}
  LUAI_MAXSTACK = 1000000;
  {$ELSE}
  LUAI_MAXSTACK = 1000000; //C value : 15000
  {$ENDIF}

  LUAI_FIRSTPSEUDOIDX = (-LUAI_MAXSTACK - 1000);

  //LUA_IDSIZE gives the maximum size for the description of the source
  //of a function in debug information.
  //CHANGE it if you want a different size.
  LUA_IDSIZE = 60;

  LUA_VERSION_NUM = 503;

//Lua

//option for multiple returns in 'lua_pcall' and 'lua_call'
const
  LUA_MULTRET = -1;

  //pseudo-indices
  LUA_REGISTRYINDEX = LUAI_FIRSTPSEUDOIDX;

function lua_upvalueindex(I: integer): integer; inline;

//thread status
const
  LUA_OK = 0;
  LUA_YIELD_ = 1; // LUA_YIELD was suffixed by '_' for avoiding name collision
  LUA_ERRRUN = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM = 4;
  LUA_ERRGCMM = 5;
  LUA_ERRERR = 6;

type
  Plua_State = Pointer; //TODO: base on type instead of pointer

//basic types
const
  LUA_TNONE = (-1);

  LUA_TNIL = 0;
  LUA_TBOOLEAN = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER = 3;
  LUA_TSTRING = 4;
  LUA_TTABLE = 5;
  LUA_TFUNCTION = 6;
  LUA_TUSERDATA = 7;
  LUA_TTHREAD = 8;

  LUA_NUMTAGS = 9;

  //minimum Lua stack available to a C function
  LUA_MINSTACK = 20;

  //predefined values in the registry
  LUA_RIDX_MAINTHREAD = 1;
  LUA_RIDX_GLOBALS = 2;
  LUA_RIDX_LAST = LUA_RIDX_GLOBALS;

type

  //type of numbers in Lua
  lua_Number = double;
  Plua_Number = ^lua_Number;

  //type for integer functions
  lua_Integer = int64;

  //unsigned integer type
  lua_Unsigned = UInt64;

  //type for continuation-function contexts
  lua_KContext = integer;

  //Type for C functions registered with Lua
  lua_CFunction = function(L: Plua_State): integer; cdecl;

  //Type for continuation functions
  lua_KFunction = function(L: Plua_State; status: integer; ctx: integer): integer; cdecl;

  //Type for functions that read/write blocks when loading/dumping Lua chunks
  lua_Reader = function(L: Plua_State; ud: Pointer; size: Psize_t): PChar; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): integer; cdecl;

  //Type for memory-allocation functions
  lua_Alloc = function(ud: Pointer; ptr: Pointer; osize: size_t; nsize: size_t): Pointer; cdecl;

//Comparison and arithmetic functions
const
  LUA_OPADD = 0; // ORDER TM, ORDER OP
  LUA_OPSUB = 1;
  LUA_OPMUL = 2;
  LUA_OPMOD = 3;
  LUA_OPPOW = 4;
  LUA_OPDIV = 5;
  LUA_OPIDIV = 6;
  LUA_OPBAND = 7;
  LUA_OPBOR = 8;
  LUA_OPBXOR = 9;
  LUA_OPSHL = 10;
  LUA_OPSHR = 11;
  LUA_OPUNM = 12;
  LUA_OPBNOT = 13;

  //Comparison functions
  LUA_OPEQ = 0;
  LUA_OPLT = 1;
  LUA_OPLE = 2;

  //garbage-collection function and options
  LUA_GCSTOP = 0;
  LUA_GCRESTART = 1;
  LUA_GCCOLLECT = 2;
  LUA_GCCOUNT = 3;
  LUA_GCCOUNTB = 4;
  LUA_GCSTEP = 5;
  LUA_GCSETPAUSE = 6;
  LUA_GCSETSTEPMUL = 7;
  LUA_GCISRUNNING = 9;

//lua api functions
var
  //state manipulation
  lua_newstate: function(f: lua_Alloc; ud: Pointer): Plua_State; cdecl;
  lua_close: procedure(L: Plua_State); cdecl;
  lua_newthread: function(L: Plua_State): Plua_State; cdecl;

  lua_atpanic: function(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

  lua_version: function(L: Plua_State): Plua_Number; cdecl;

  //basic stack manipulation
  lua_absindex: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_gettop: function(L: Plua_State): integer; cdecl;
  lua_settop: procedure(L: Plua_State; idx: integer); cdecl;
  lua_pushvalue: procedure(L: Plua_State; idx: integer); cdecl;
  lua_rotate: procedure(L: Plua_State; idx: integer; n: integer); cdecl;
  lua_copy: procedure(L: Plua_State; fromidx: integer; toidx: integer); cdecl;
  lua_checkstack: function(L: Plua_State; n: integer): integer; cdecl;

  lua_xmove: procedure(from: Plua_State; to_: Plua_State; n: integer); cdecl;

  //access functions (stack -> C)
  lua_isnumber: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_isstring: function(L: Plua_State; idx: integer): boolean; cdecl;
  lua_iscfunction: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_isinteger: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_isuserdata: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_type: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_typename: function(L: Plua_State; tp: integer): PChar; cdecl;

  lua_tonumberx: function(L: Plua_State; idx: integer; isnum: PInteger): lua_Number; cdecl;
  lua_tointegerx: function(L: Plua_State; idx: integer; isnum: Pinteger): lua_Integer; cdecl;
  lua_toboolean: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_tolstring: function(L: Plua_State; idx: integer; len: psize_t): PChar; cdecl;
  lua_rawlen: function(L: Plua_State; idx: integer): size_t; cdecl;
  lua_tocfunction: function(L: Plua_State; idx: integer): lua_CFunction; cdecl;
  lua_touserdata: function(L: Plua_State; idx: integer): Pointer; cdecl;
  lua_tothread: function(L: Plua_State; idx: integer): Plua_State; cdecl;
  lua_topointer: function(L: Plua_State; idx: integer): Pointer; cdecl;

  //Comparison and arithmetic functions
  lua_arith: procedure(L: Plua_State; op: integer); cdecl;
  lua_rawequal: function(L: Plua_State; idx1: integer; idx2: integer): integer; cdecl;
  lua_compare: function(L: Plua_State; idx1: integer; idx2: integer; op: integer): integer; cdecl;

  //push functions (C -> stack)
  lua_pushnil: procedure(L: Plua_State); cdecl;
  lua_pushnumber: procedure(L: Plua_State; n: lua_Number); cdecl;
  lua_pushinteger: procedure(L: Plua_State; n: lua_Integer); cdecl;
  lua_pushlstring: function(L: Plua_State; const s: PChar; len: size_t): PChar; cdecl;
  lua_pushstring: function(L: Plua_State; const s: PChar): PChar; cdecl;
  lua_pushvfstring: function(L: Plua_State; const fmt: PChar; argp: pointer): PChar; cdecl;
  lua_pushfstring: function(L: Plua_State; const fmt: PChar; arg: array of Pointer): PChar; cdecl;
  lua_pushcclosure: procedure(L: Plua_State; fn: lua_CFunction; n: integer); cdecl;
  lua_pushboolean: procedure(L: Plua_State; b: integer); cdecl;
  lua_pushlightuserdata: procedure(L: Plua_State; p: pointer); cdecl;
  lua_pushthread: function(L: Plua_State): integer; cdecl;

  //get functions (Lua -> stack)
  lua_getglobal: function(L: Plua_State; const Name: PChar): integer; cdecl;
  lua_gettable: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_getfield: function(L: Plua_State; idx: integer; const k: PChar): integer; cdecl;
  lua_geti: function(L: Plua_State; idx: integer; n: lua_Integer): integer; cdecl;
  lua_rawget: function(L: Plua_State; idx: integer): integer; cdecl;
  lua_rawgeti: function(L: Plua_State; idx: integer; n: lua_Integer): integer; cdecl;
  lua_rawgetp: function(L: Plua_State; idx: integer; const p: Pointer): integer; cdecl;

  lua_createtable: procedure(L: Plua_State; narr: integer; nrec: integer); cdecl;
  lua_newuserdata: function(L: Plua_State; sz: size_t): Pointer; cdecl;
  lua_getmetatable: function(L: Plua_State; objindex: integer): integer; cdecl;
  lua_getuservalue: function(L: Plua_State; idx: integer): integer; cdecl;

  //set functions (stack -> Lua)
  lua_setglobal: procedure(L: Plua_State; const Name: PChar); cdecl;
  lua_settable: procedure(L: Plua_State; idx: integer); cdecl;
  lua_setfield: procedure(L: Plua_State; idx: integer; const k: PChar); cdecl;
  lua_seti: procedure(L: Plua_State; idx: integer; n: lua_Integer); cdecl;
  lua_rawset: procedure(L: Plua_State; idx: integer); cdecl;
  lua_rawseti: procedure(L: Plua_State; idx: integer; n: lua_Integer); cdecl;
  lua_rawsetp: procedure(L: Plua_State; idx: integer; const p: Pointer); cdecl;
  lua_setmetatable: function(L: Plua_State; objindex: integer): integer; cdecl;
  lua_setuservalue: procedure(L: Plua_State; idx: integer); cdecl;

  //'load' and 'call' functions (load and run Lua code)
  lua_callk: procedure(L: Plua_State; nargs: integer; nresults: integer; ctx: integer; k: lua_KFunction); cdecl;

procedure lua_call(L: Plua_State; n: integer; r: integer); inline;

var
  lua_pcallk: function(L: Plua_State; nargs: integer; nresults: integer; errfunc: integer; ctx: integer; k: lua_KFunction): integer; cdecl;

function lua_pcall(L: Plua_State; n: integer; r: integer; f: integer): integer; inline;

var
  lua_load: function(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PChar; const mode: PChar): integer; cdecl;
  lua_dump: function(L: Plua_State; writer: lua_Writer; Data: Pointer; strip: integer): integer; cdecl;

  //coroutine functions
  lua_yieldk: function(L: Plua_State; nresults: integer; ctx: integer; k: lua_KFunction): integer; cdecl;
  lua_resume: function(L: Plua_State; from: Plua_State; narg: integer): integer; cdecl;
  lua_status: function(L: Plua_State): integer; cdecl;
  lua_isyieldable: function(L: Plua_State): integer; cdecl;

function lua_yield(L: Plua_State; n: integer): integer; inline;

var
  //garbage-collection function and options
  lua_gc: function(L: Plua_State; what: integer; Data: integer): integer; cdecl;

  //miscellaneous functions
  lua_error: function(L: Plua_State): integer; cdecl;

  lua_next: function(L: Plua_State; idx: integer): integer; cdecl;

  lua_concat: procedure(L: Plua_State; n: integer); cdecl;
  lua_len: procedure(L: Plua_State; idx: integer); cdecl;

  lua_stringtonumber: function(L: Plua_State; const s: PChar): size_t; cdecl;

  lua_getallocf: function(L: Plua_State; ud: PPointer): lua_Alloc; cdecl;
  lua_setallocf: procedure(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl;

//some useful macros

function lua_getextraspace(L: Plua_State): Pointer; inline;

function lua_tonumber(L: Plua_State; i: integer): lua_Number; inline;
function lua_tointeger(L: Plua_State; i: integer): lua_Integer; inline;

procedure lua_pop(L: Plua_State; n: integer); inline;

procedure lua_newtable(L: Plua_State); inline;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction); inline;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction); inline;

function lua_isfunction(L: Plua_State; n: integer): boolean; inline;
function lua_istable(L: Plua_State; n: integer): boolean; inline;
function lua_islightuserdata(L: Plua_State; n: integer): boolean; inline;
function lua_isnil(L: Plua_State; n: integer): boolean; inline;
function lua_isboolean(L: Plua_State; n: integer): boolean; inline;
function lua_isthread(L: Plua_State; n: integer): boolean; inline;
function lua_isnone(L: Plua_State; n: integer): boolean; inline;
function lua_isnoneornil(L: Plua_State; n: integer): boolean; inline;

procedure lua_pushliteral(L: Plua_State; s: PChar); inline;

procedure lua_pushglobaltable(L: Plua_State); inline;

function lua_tostring(L: Plua_State; i: integer): PChar; inline;

procedure lua_insert(L: Plua_State; idx: integer); inline;

procedure lua_remove(L: Plua_State; idx: integer); inline;

procedure lua_replace(L: Plua_State; idx: integer); inline;

//depracted
function lua_open: Plua_State; inline;

//Debug API
const
  // Event codes
  LUA_HOOKCALL = 0;
  LUA_HOOKRET = 1;
  LUA_HOOKLINE = 2;
  LUA_HOOKCOUNT = 3;
  LUA_HOOKTAILCALL = 4;

  //Event masks
  LUA_MASKCALL = 1 shl Ord(LUA_HOOKCALL);
  LUA_MASKRET = 1 shl Ord(LUA_HOOKRET);
  LUA_MASKLINE = 1 shl Ord(LUA_HOOKLINE);
  LUA_MASKCOUNT = 1 shl Ord(LUA_HOOKCOUNT);

type
  lua_Debug = record           //activation record
    event: integer;
    Name: PChar;               // (n)
    namewhat: PChar;           // (n) 'global', 'local', 'field', 'method'
    what: PChar;               // (S) 'Lua' function, 'C' function, Lua 'main'
    Source: PChar;             // (S)
    currentline: integer;      // (l)
    linedefined: integer;      // (S)
    lastlinedefined: integer;  // (S)
    nups: byte;                // (u) number of upvalues
    nparams: byte;             // (u) number of parameters
    isvararg: bytebool;        // (u)
    istailcall: bytebool;      // (t)
    short_src: packed array[0..LUA_IDSIZE - 1] of char; // (S)
    //private part
    i_ci: Pointer;             // active function
  end;
  Plua_Debug = ^lua_Debug;

  //Functions to be called by the debugger in specific events
  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

var
  lua_getstack: function(L: Plua_State; level: integer; ar: Plua_Debug): integer; cdecl;
  lua_getinfo: function(L: Plua_State; const what: PChar; ar: Plua_Debug): integer; cdecl;
  lua_getlocal: function(L: Plua_State; const ar: Plua_Debug; n: integer): PChar; cdecl;
  lua_setlocal: function(L: Plua_State; const ar: Plua_Debug; n: integer): PChar; cdecl;
  lua_getupvalue: function(L: Plua_State; funcindex: integer; n: integer): PChar; cdecl;
  lua_setupvalue: function(L: Plua_State; funcindex: integer; n: integer): PChar; cdecl;

  lua_upvalueid: function(L: Plua_State; fidx: integer; n: integer): Pointer; cdecl;
  lua_upvaluejoin: procedure(L: Plua_State; fidx1: integer; n1: integer; fidx2: integer; n2: integer); cdecl;

  lua_sethook: procedure(L: Plua_State; func: lua_Hook; mask: integer; Count: integer); cdecl;
  lua_gethook: function(L: Plua_State): lua_Hook; cdecl;
  lua_gethookmask: function(L: Plua_State): integer; cdecl;
  lua_gethookcount: function(L: Plua_State): integer; cdecl;

//LuaLib
var
  luaopen_base: function(L: Plua_State): integer; cdecl;

const
  LUA_COLIBNAME = 'coroutine';

var
  luaopen_coroutine: function(L: Plua_State): integer; cdecl;

const
  LUA_TABLIBNAME = 'table';

var
  luaopen_table: function(L: Plua_State): integer; cdecl;

const
  LUA_IOLIBNAME = 'io';

var
  luaopen_io: function(L: Plua_State): integer; cdecl;

const
  LUA_OSLIBNAME = 'os';

var
  luaopen_os: function(L: Plua_State): integer; cdecl;

const
  LUA_STRLIBNAME = 'string';

var
  luaopen_string: function(L: Plua_State): integer; cdecl;

const
  LUA_UTF8LIBNAME = 'utf8';

var
  luaopen_utf8: function(L: Plua_State): integer; cdecl;

const
  LUA_BITLIBNAME = 'bit32';

var
  luaopen_bit32: function(L: Plua_State): integer; cdecl;

const
  LUA_MATHLIBNAME = 'math';

var
  luaopen_math: function(L: Plua_State): integer; cdecl;

const
  LUA_DBLIBNAME = 'debug';

var
  luaopen_debug: function(L: Plua_State): integer; cdecl;

const
  LUA_LOADLIBNAME = 'package';

var
  luaopen_package: function(L: Plua_State): integer; cdecl;

  //open all previous libraries
  luaL_openlibs: procedure(L: Plua_State); cdecl;

//LuaXLib
const
  LUA_ERRFILE = LUA_ERRERR + 1;   // extra error code for 'luaL_load'

type
  luaL_reg = record
    Name: PChar;
    func: lua_CFunction;
  end;
  PluaL_reg = ^luaL_reg;

const
  LUAL_NUMSIZES = sizeof(lua_Integer) * 16 + sizeof(lua_Number);
  // pre-defined references
  LUA_NOREF = -2;
  LUA_REFNIL = -1;

var
  luaL_checkversion_: procedure(L: Plua_State; ver: lua_Number; sz: size_t); cdecl;

procedure luaL_checkversion(L: PLua_State);

var
  luaL_getmetafield: function(L: Plua_State; obj: integer; const e: PChar): integer; cdecl;
  luaL_callmeta: function(L: Plua_State; obj: integer; const event: PChar): integer; cdecl;
  luaL_tolstring: function(L: Plua_State; idx: integer; len: size_t): PChar; cdecl;
  luaL_argerror: function(L: Plua_State; arg: integer; const extramsg: PChar): integer; cdecl;

  luaL_checklstring: function(L: Plua_State; arg: integer; length: Psize_t): PChar; cdecl;
  luaL_optlstring: function(L: Plua_State; arg: integer; const def: PChar; length: Psize_t): PChar; cdecl;

  luaL_checknumber: function(L: Plua_State; arg: integer): lua_Number; cdecl;
  luaL_optnumber: function(L: Plua_State; arg: integer; def: lua_Number): lua_Number; cdecl;

  luaL_checkinteger: function(L: Plua_State; arg: integer): lua_Integer; cdecl;
  luaL_optinteger: function(L: Plua_State; arg: integer; def: lua_Integer): lua_Integer; cdecl;

  luaL_check_stack: procedure(L: Plua_State; sz: integer; const msg: PChar); cdecl;
  luaL_check_type: procedure(L: Plua_State; arg: integer; t: integer); cdecl;
  luaL_check_any: procedure(L: Plua_State; arg: integer); cdecl;

  luaL_newmetatable: function(L: Plua_State; const tname: PChar): integer; cdecl;
  luaL_setmetatable: procedure(L: Plua_State; const tname: PChar); cdecl;
  luaL_testudata: function(L: Plua_State; ud: integer; const tname: PChar): Pointer; cdecl;
  luaL_checkudata: function(L: Plua_State; ud: integer; const tname: PChar): Pointer; cdecl;

  luaL_where: procedure(L: Plua_State; lvl: integer); cdecl;
  luaL_error: function(L: Plua_State; const fmt: PChar; args: array of const): integer; cdecl;

  luaL_checkoption: function(L: Plua_State; arg: integer; const cdef: PChar; const lst: PPChar): integer; cdecl;

  luaL_fileresult: function(L: Plua_State; stat: integer; const fname: PChar): integer; cdecl;
  luaL_execresult: function(L: Plua_State; stat: integer): integer; cdecl;

  luaL_ref: function(L: Plua_State; t: integer): integer; cdecl;
  luaL_unref: procedure(L: Plua_State; t, ref: integer); cdecl;

  luaL_loadfilex: function(L: Plua_State; const filename: PChar; const mode: PChar): integer; cdecl;

function luaL_loadfile(L: Plua_State; f: PChar): integer;

var
  luaL_loadbufferx: function(L: Plua_State; buff: PChar; sz: size_t; Name: PChar; mode: PChar): integer; cdecl;

  luaL_loadstring: function(L: Plua_State; const s: PChar): integer; cdecl;

  luaL_newstate: function(): Plua_state; cdecl;

  luaL_len: function(L: Plua_State; idx: integer): lua_Integer; cdecl;

  luaL_gsub: function(L: Plua_State; const s: PChar; const p: PChar; const r: PChar): PChar; cdecl;

  luaL_setfuncs: procedure(L: Plua_State; lr: PluaL_Reg; nup: integer); cdecl;

  luaL_getsubtable: function(L: Plua_State; idx: integer; const fname: PChar): integer; cdecl;

  luaL_traceback: procedure(L: Plua_State; L1: Plua_State; msg: PChar; level: integer); cdecl;

  luaL_requiref: procedure(L: Plua_State; const modname: PChar; openf: lua_CFunction; glb: integer); cdecl;

//some useful macros
procedure luaL_newlibtable(L: Plua_State; lr: array of luaL_Reg); inline;

procedure luaL_newlib(L: Plua_State; lr: array of luaL_Reg); inline;

procedure luaL_arg_check(L: Plua_State; cond: boolean; numarg: integer; extramsg: PChar); inline;

function luaL_check_string(L: Plua_State; n: integer): PChar; inline;
function luaL_opt_string(L: Plua_State; n: integer; d: PChar): PChar; inline;

function luaL_typename(L: Plua_State; i: integer): PChar; inline;

function luaL_dofile(L: Plua_State; fn: PChar): integer; inline;

function luaL_dostring(L: Plua_State; s: PChar): integer; inline;

function luaL_getmetatable(L: Plua_State; n: PChar): integer; inline;

function luaL_loadbuffer(L: Plua_State; buff: PChar; sz: size_t; const Name: PChar): integer; inline;

//depracted
procedure luaL_openlib(L: Plua_State; const n: PChar; lr: PluaL_reg; nup: integer); inline;

//depracted
procedure luaL_register(L: Plua_State; n: PChar; lib: PLuaL_Reg); inline;


implementation

uses
{$IFDEF LINUX}
  LibC,
{$ELSE}
  Windows;

{$ENDIF}

//Dynamic loading and unloading of Lua libs
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

procedure ClearLuaProc;
begin
  //Lua
  lua_newstate := nil;
  lua_close := nil;
  lua_newthread := nil;
  lua_atpanic := nil;
  lua_version := nil;
  lua_absindex := nil;
  lua_gettop := nil;
  lua_settop := nil;
  lua_pushvalue := nil;
  lua_rotate := nil;
  lua_copy := nil;
  lua_checkstack := nil;
  lua_xmove := nil;
  lua_isnumber := nil;
  lua_isstring := nil;
  lua_iscfunction := nil;
  lua_isinteger := nil;
  lua_isuserdata := nil;
  lua_type := nil;
  lua_typename := nil;
  lua_tonumberx := nil;
  lua_tointegerx := nil;
  lua_toboolean := nil;
  lua_tolstring := nil;
  lua_rawlen := nil;
  lua_tocfunction := nil;
  lua_touserdata := nil;
  lua_tothread := nil;
  lua_topointer := nil;
  lua_arith := nil;
  lua_rawequal := nil;
  lua_compare := nil;
  lua_pushnil := nil;
  lua_pushnumber := nil;
  lua_pushinteger := nil;
  lua_pushlstring := nil;
  lua_pushstring := nil;
  lua_pushvfstring := nil;
  lua_pushfstring := nil;
  lua_pushcclosure := nil;
  lua_pushboolean := nil;
  lua_pushlightuserdata := nil;
  lua_pushthread := nil;
  lua_getglobal := nil;
  lua_gettable := nil;
  lua_getfield := nil;
  lua_geti := nil;
  lua_rawget := nil;
  lua_rawgeti := nil;
  lua_rawgetp := nil;
  lua_createtable := nil;
  lua_newuserdata := nil;
  lua_getmetatable := nil;
  lua_getuservalue := nil;
  lua_setglobal := nil;
  lua_settable := nil;
  lua_setfield := nil;
  lua_seti := nil;
  lua_rawset := nil;
  lua_rawseti := nil;
  lua_rawsetp := nil;
  lua_setmetatable := nil;
  lua_setuservalue := nil;
  lua_callk := nil;
  lua_pcallk := nil;
  lua_load := nil;
  lua_dump := nil;
  lua_yieldk := nil;
  lua_resume := nil;
  lua_status := nil;
  lua_isyieldable := nil;
  lua_gc := nil;
  lua_error := nil;
  lua_next := nil;
  lua_concat := nil;
  lua_len := nil;
  lua_stringtonumber := nil;
  lua_getallocf := nil;
  lua_setallocf := nil;
  lua_getstack := nil;
  lua_getinfo := nil;
  lua_getlocal := nil;
  lua_setlocal := nil;
  lua_getupvalue := nil;
  lua_setupvalue := nil;
  lua_upvalueid := nil;
  lua_upvaluejoin := nil;
  lua_sethook := nil;
  lua_gethook := nil;
  lua_gethookmask := nil;
  lua_gethookcount := nil;
  //LuaLib
  luaopen_base := nil;
  luaopen_coroutine := nil;
  luaopen_table := nil;
  luaopen_io := nil;
  luaopen_os := nil;
  luaopen_string := nil;
  luaopen_utf8 := nil;
  luaopen_bit32 := nil;
  luaopen_math := nil;
  luaopen_debug := nil;
  luaopen_package := nil;
  luaL_openlibs := nil;
  //LuaXLib
  luaL_checkversion_ := nil;
  luaL_getmetafield := nil;
  luaL_callmeta := nil;
  luaL_tolstring := nil;
  luaL_argerror := nil;
  luaL_checklstring := nil;
  luaL_optlstring := nil;
  luaL_checknumber := nil;
  luaL_optnumber := nil;
  luaL_checkinteger := nil;
  luaL_optinteger := nil;
  luaL_check_stack := nil;
  luaL_check_type := nil;
  luaL_check_any := nil;
  luaL_newmetatable := nil;
  luaL_setmetatable := nil;
  luaL_testudata := nil;
  luaL_checkudata := nil;
  luaL_where := nil;
  luaL_error := nil;
  luaL_checkoption := nil;
  luaL_fileresult := nil;
  luaL_execresult := nil;
  luaL_ref := nil;
  luaL_unref := nil;
  luaL_loadfilex := nil;
  luaL_loadbufferx := nil;
  luaL_loadstring := nil;
  luaL_newstate := nil;
  luaL_len := nil;
  luaL_gsub := nil;
  luaL_setfuncs := nil;
  luaL_getsubtable := nil;
  luaL_traceback := nil;
  luaL_requiref := nil;
end;

procedure LoadLuaProc;
begin
  if LuaHandle <> INVALID_MODULE_HANDLE then
  begin
    //Lua
    lua_newstate := GetProcAddress(LuaHandle, 'lua_newstate');
    lua_close := GetProcAddress(LuaHandle, 'lua_close');
    lua_newthread := GetProcAddress(LuaHandle, 'lua_newthread');
    lua_atpanic := GetProcAddress(LuaHandle, 'lua_atpanic');
    lua_version := GetProcAddress(LuaHandle, 'lua_version');
    lua_absindex := GetProcAddress(LuaHandle, 'lua_absindex');
    lua_gettop := GetProcAddress(LuaHandle, 'lua_gettop');
    lua_settop := GetProcAddress(LuaHandle, 'lua_settop');
    lua_pushvalue := GetProcAddress(LuaHandle, 'lua_pushvalue');
    lua_rotate := GetProcAddress(LuaHandle, 'lua_rotate');
    lua_copy := GetProcAddress(LuaHandle, 'lua_copy');
    lua_checkstack := GetProcAddress(LuaHandle, 'lua_checkstack');
    lua_xmove := GetProcAddress(LuaHandle, 'lua_xmove');
    lua_isnumber := GetProcAddress(LuaHandle, 'lua_isnumber');
    lua_isstring := GetProcAddress(LuaHandle, 'lua_isstring');
    lua_iscfunction := GetProcAddress(LuaHandle, 'lua_iscfunction');
    lua_isinteger := GetProcAddress(LuaHandle, 'lua_isinteger');
    lua_isuserdata := GetProcAddress(LuaHandle, 'lua_isuserdata');
    lua_type := GetProcAddress(LuaHandle, 'lua_type');
    lua_typename := GetProcAddress(LuaHandle, 'lua_typename');
    lua_tonumberx := GetProcAddress(LuaHandle, 'lua_tonumberx');
    lua_tointegerx := GetProcAddress(LuaHandle, 'lua_tointegerx');
    lua_toboolean := GetProcAddress(LuaHandle, 'lua_toboolean');
    lua_tolstring := GetProcAddress(LuaHandle, 'lua_tolstring');
    lua_rawlen := GetProcAddress(LuaHandle, 'lua_rawlen');
    lua_tocfunction := GetProcAddress(LuaHandle, 'lua_tocfunction');
    lua_touserdata := GetProcAddress(LuaHandle, 'lua_touserdata');
    lua_tothread := GetProcAddress(LuaHandle, 'lua_tothread');
    lua_topointer := GetProcAddress(LuaHandle, 'lua_topointer');
    lua_arith := GetProcAddress(LuaHandle, 'lua_arith');
    lua_rawequal := GetProcAddress(LuaHandle, 'lua_rawequal');
    lua_compare := GetProcAddress(LuaHandle, 'lua_compare');
    lua_pushnil := GetProcAddress(LuaHandle, 'lua_pushnil');
    lua_pushnumber := GetProcAddress(LuaHandle, 'lua_pushnumber');
    lua_pushinteger := GetProcAddress(LuaHandle, 'lua_pushinteger');
    lua_pushlstring := GetProcAddress(LuaHandle, 'lua_pushlstring');
    lua_pushstring := GetProcAddress(LuaHandle, 'lua_pushstring');
    lua_pushvfstring := GetProcAddress(LuaHandle, 'lua_pushvfstring');
    lua_pushfstring := GetProcAddress(LuaHandle, 'lua_pushfstring');
    lua_pushcclosure := GetProcAddress(LuaHandle, 'lua_pushcclosure');
    lua_pushboolean := GetProcAddress(LuaHandle, 'lua_pushboolean');
    lua_pushlightuserdata := GetProcAddress(LuaHandle, 'lua_pushlightuserdata');
    lua_pushthread := GetProcAddress(LuaHandle, 'lua_pushthread');
    lua_getglobal := GetProcAddress(LuaHandle, 'lua_getglobal');
    lua_gettable := GetProcAddress(LuaHandle, 'lua_gettable');
    lua_getfield := GetProcAddress(LuaHandle, 'lua_getfield');
    lua_geti := GetProcAddress(LuaHandle, 'lua_geti');
    lua_rawget := GetProcAddress(LuaHandle, 'lua_rawget');
    lua_rawgeti := GetProcAddress(LuaHandle, 'lua_rawgeti');
    lua_rawgetp := GetProcAddress(LuaHandle, 'lua_rawgetp');
    lua_createtable := GetProcAddress(LuaHandle, 'lua_createtable');
    lua_newuserdata := GetProcAddress(LuaHandle, 'lua_newuserdata');
    lua_getmetatable := GetProcAddress(LuaHandle, 'lua_getmetatable');
    lua_getuservalue := GetProcAddress(LuaHandle, 'lua_getuservalue');
    lua_setglobal := GetProcAddress(LuaHandle, 'lua_setglobal');
    lua_settable := GetProcAddress(LuaHandle, 'lua_settable');
    lua_setfield := GetProcAddress(LuaHandle, 'lua_setfield');
    lua_seti := GetProcAddress(LuaHandle, 'lua_seti');
    lua_rawset := GetProcAddress(LuaHandle, 'lua_rawset');
    lua_rawseti := GetProcAddress(LuaHandle, 'lua_rawseti');
    lua_rawsetp := GetProcAddress(LuaHandle, 'lua_rawsetp');
    lua_setmetatable := GetProcAddress(LuaHandle, 'lua_setmetatable');
    lua_setuservalue := GetProcAddress(LuaHandle, 'lua_setuservalue');
    lua_callk := GetProcAddress(LuaHandle, 'lua_callk');
    lua_pcallk := GetProcAddress(LuaHandle, 'lua_pcallk');
    lua_load := GetProcAddress(LuaHandle, 'lua_load');
    lua_dump := GetProcAddress(LuaHandle, 'lua_dump');
    lua_yieldk := GetProcAddress(LuaHandle, 'lua_yieldk');
    lua_resume := GetProcAddress(LuaHandle, 'lua_resume');
    lua_status := GetProcAddress(LuaHandle, 'lua_status');
    lua_isyieldable := GetProcAddress(LuaHandle, 'lua_isyieldable');
    lua_gc := GetProcAddress(LuaHandle, 'lua_gc');
    lua_error := GetProcAddress(LuaHandle, 'lua_error');
    lua_next := GetProcAddress(LuaHandle, 'lua_next');
    lua_concat := GetProcAddress(LuaHandle, 'lua_concat');
    lua_len := GetProcAddress(LuaHandle, 'lua_len');
    lua_stringtonumber := GetProcAddress(LuaHandle, 'lua_stringtonumber');
    lua_getallocf := GetProcAddress(LuaHandle, 'lua_getallocf');
    lua_setallocf := GetProcAddress(LuaHandle, 'lua_setallocf');
    lua_getstack := GetProcAddress(LuaHandle, 'lua_getstack');
    lua_getinfo := GetProcAddress(LuaHandle, 'lua_getinfo');
    lua_getlocal := GetProcAddress(LuaHandle, 'lua_getlocal');
    lua_setlocal := GetProcAddress(LuaHandle, 'lua_setlocal');
    lua_getupvalue := GetProcAddress(LuaHandle, 'lua_getupvalue');
    lua_setupvalue := GetProcAddress(LuaHandle, 'lua_setupvalue');
    lua_upvalueid := GetProcAddress(LuaHandle, 'lua_upvalueid');
    lua_upvaluejoin := GetProcAddress(LuaHandle, 'lua_upvaluejoin');
    lua_sethook := GetProcAddress(LuaHandle, 'lua_sethook');
    lua_gethook := GetProcAddress(LuaHandle, 'lua_gethook');
    lua_gethookmask := GetProcAddress(LuaHandle, 'lua_gethookmask');
    lua_gethookcount := GetProcAddress(LuaHandle, 'lua_gethookcount');
    //LuaLibs
    luaopen_base := GetProcAddress(LuaHandle, 'luaopen_base');
    luaopen_coroutine := GetProcAddress(LuaHandle, 'luaopen_coroutine');
    luaopen_table := GetProcAddress(LuaHandle, 'luaopen_table');
    luaopen_io := GetProcAddress(LuaHandle, 'luaopen_io');
    luaopen_os := GetProcAddress(LuaHandle, 'luaopen_os');
    luaopen_string := GetProcAddress(LuaHandle, 'luaopen_string');
    luaopen_utf8 := GetProcAddress(LuaHandle, 'luaopen_utf8');
    luaopen_bit32 := GetProcAddress(LuaHandle, 'luaopen_bit32');
    luaopen_math := GetProcAddress(LuaHandle, 'luaopen_math');
    luaopen_debug := GetProcAddress(LuaHandle, 'luaopen_debug');
    luaopen_package := GetProcAddress(LuaHandle, 'luaopen_package');
    luaL_openlibs := GetProcAddress(LuaHandle, 'luaL_openlibs');
    //LuaXLib
    luaL_checkversion_ := GetProcAddress(LuaHandle, 'luaL_checkversion_');
    luaL_getmetafield := GetProcAddress(LuaHandle, 'luaL_getmetafield');
    luaL_callmeta := GetProcAddress(LuaHandle, 'luaL_callmeta');
    luaL_tolstring := GetProcAddress(LuaHandle, 'luaL_tolstring');
    luaL_argerror := GetProcAddress(LuaHandle, 'luaL_argerror');
    luaL_checklstring := GetProcAddress(LuaHandle, 'luaL_checklstring');
    luaL_optlstring := GetProcAddress(LuaHandle, 'luaL_optlstring');
    luaL_checknumber := GetProcAddress(LuaHandle, 'luaL_checknumber');
    luaL_optnumber := GetProcAddress(LuaHandle, 'luaL_optnumber');
    luaL_checkinteger := GetProcAddress(LuaHandle, 'luaL_checkinteger');
    luaL_optinteger := GetProcAddress(LuaHandle, 'luaL_optinteger');
    luaL_check_stack := GetProcAddress(LuaHandle, 'luaL_check_stack');
    luaL_check_type := GetProcAddress(LuaHandle, 'luaL_check_type');
    luaL_check_any := GetProcAddress(LuaHandle, 'luaL_check_any');
    luaL_newmetatable := GetProcAddress(LuaHandle, 'luaL_newmetatable');
    luaL_setmetatable := GetProcAddress(LuaHandle, 'luaL_setmetatable');
    luaL_testudata := GetProcAddress(LuaHandle, 'luaL_testudata');
    luaL_checkudata := GetProcAddress(LuaHandle, 'luaL_checkudata');
    luaL_where := GetProcAddress(LuaHandle, 'luaL_where');
    luaL_error := GetProcAddress(LuaHandle, 'luaL_error');
    luaL_checkoption := GetProcAddress(LuaHandle, 'luaL_checkoption');
    luaL_fileresult := GetProcAddress(LuaHandle, 'luaL_fileresult');
    luaL_execresult := GetProcAddress(LuaHandle, 'luaL_execresult');
    luaL_ref := GetProcAddress(LuaHandle, 'luaL_ref');
    luaL_unref := GetProcAddress(LuaHandle, 'luaL_unref');
    luaL_loadfilex := GetProcAddress(LuaHandle, 'luaL_loadfilex');
    luaL_loadbufferx := GetProcAddress(LuaHandle, 'luaL_loadbufferx');
    luaL_loadstring := GetProcAddress(LuaHandle, 'luaL_loadstring');
    luaL_newstate := GetProcAddress(LuaHandle, 'luaL_newstate');
    luaL_len := GetProcAddress(LuaHandle, 'luaL_len');
    luaL_gsub := GetProcAddress(LuaHandle, 'luaL_gsub');
    luaL_setfuncs := GetProcAddress(LuaHandle, 'luaL_setfuncs');
    luaL_getsubtable := GetProcAddress(LuaHandle, 'luaL_getsubtable');
    luaL_traceback := GetProcAddress(LuaHandle, 'luaL_traceback');
    luaL_requiref := GetProcAddress(LuaHandle, 'luaL_requiref');
  end;
end;

function loadLua: boolean;
begin
  if LuaHandle = INVALID_MODULE_HANDLE then
    Result := LoadLuaFrom(LUA_NAME)
  else
    Result := True;
end;

function loadLuaFrom(FileName: string): boolean;
begin
  ClearLuaProc;
  LuaHandle := OpenLib(PChar(FileName));
  if LuaHandle <> INVALID_MODULE_HANDLE then
  begin
    LoadLuaProc;
    Result := True;
  end
  else
    Result := False;
end;

procedure unLoadLua;
begin
  if LuaHandle <> INVALID_MODULE_HANDLE then
    FreeLibrary(LuaHandle);
  LuaHandle := INVALID_MODULE_HANDLE;
  ClearLuaProc;
end;

function isLuaLoaded: boolean;
begin
  Result := LuaHandle <> INVALID_MODULE_HANDLE;
end;

//Lua

//lua_open is depracted this is a replacement
function lua_open: Plua_State;
begin
  Result := luaL_newstate;
end;

//pseudo-indices
function lua_upvalueindex(I: integer): integer;
begin
  Result := LUA_REGISTRYINDEX - i;
end;

//'load' and 'call' functions (load and run Lua code)
procedure lua_call(L: Plua_State; n: integer; r: integer);
begin
  lua_callk(L, n, r, 0, nil);
end;

function lua_pcall(L: Plua_State; n: integer; r: integer; f: integer): integer;
begin
  Result := lua_pcallk(L, n, r, f, 0, nil);
end;

//coroutine functions
function lua_yield(L: Plua_State; n: integer): integer;
begin
  Result := lua_yieldk(L, n, 0, nil);
end;

//some useful macros

function lua_getextraspace(L: Plua_State): Pointer;
const
  LUA_EXTRASPACE = sizeof(Pointer);
begin
  Result := L - LUA_EXTRASPACE; //hmm how does this magic work in c
end;

function lua_tonumber(L: Plua_State; i: integer): lua_Number;
begin
  Result := lua_tonumberx(L, i, nil);
end;

function lua_tointeger(L: Plua_State; i: integer): lua_Integer;
begin
  Result := lua_tointegerx(L, i, nil);
end;

procedure lua_pop(L: Plua_State; n: integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L: Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_isfunction(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: integer): boolean;
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L: Plua_State; s: PChar);
begin
  lua_pushlstring(L, s, (SizeOf(s) div SizeOf(char)) - 1);
end;

procedure lua_pushglobaltable(L: Plua_State);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;

function lua_tostring(L: Plua_State; i: integer): PChar;
begin
  Result := lua_tolstring(L, i, nil);
end;

procedure lua_insert(L: Plua_State; idx: integer);
begin
  lua_rotate(L, (idx), 1);
end;

procedure lua_remove(L: Plua_State; idx: integer);
begin
  lua_rotate(L, (idx), -1);
  lua_pop(L, 1);
end;

procedure lua_replace(L: Plua_State; idx: integer);
begin
  lua_copy(L, -1, (idx));
  lua_pop(L, 1);
end;

//LuaXLib

procedure luaL_checkversion(L: PLua_State);
begin
  luaL_checkversion_(L, LUA_VERSION_NUM, LUAL_NUMSIZES);
end;

function luaL_loadfile(L: Plua_State; f: PChar): integer;
begin
  Result := luaL_Loadfilex(L, f, nil);
end;

//Some usefull macros
procedure luaL_newlibtable(L: Plua_State; lr: array of luaL_Reg);
begin
  lua_createtable(L, 0, High(lr));
end;

procedure luaL_newlib(L: Plua_State; lr: array of luaL_Reg);
begin
  luaL_checkversion(L);
  luaL_newlibtable(L, lr);
  luaL_setfuncs(L, @lr, 0);
end;

procedure luaL_arg_check(L: Plua_State; cond: boolean; numarg: integer; extramsg: PChar);
begin
  if not cond then
    luaL_argerror(L, numarg, extramsg);
end;

function luaL_check_string(L: Plua_State; n: integer): PChar;
begin
  Result := luaL_checklstring(L, n, nil);
end;

function luaL_opt_string(L: Plua_State; n: integer; d: PChar): PChar;
begin
  Result := luaL_optlstring(L, n, d, nil);
end;

function luaL_typename(L: Plua_State; i: integer): PChar;
begin
  Result := lua_typename(L, lua_type(L, (i)));
end;

function luaL_dofile(L: Plua_State; fn: PChar): integer;
begin
  Result := luaL_loadfile(L, fn);
  if Result = 0 then
    Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

function luaL_dostring(L: Plua_State; s: PChar): integer;
begin
  Result := luaL_loadstring(L, s);
  if Result = 0 then
    Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

function luaL_getmetatable(L: Plua_State; n: PChar): integer;
begin
  Result := lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

function luaL_loadbuffer(L: Plua_State; buff: PChar; sz: size_t; const Name: PChar): integer;
begin
  Result := luaL_loadbufferx(L, buff, sz, Name, nil);
end;

//depracted
procedure luaL_openlib(L: Plua_State; const n: PChar; lr: PluaL_reg; nup: integer); inline;
begin
  luaL_setfuncs(L, lr, 0);
  if n <> nil then //if empty then dont call setglobal
    lua_setglobal(L, n);
end;

//depracted
procedure luaL_register(L: Plua_State; n: PChar; lib: PLuaL_Reg); inline;
begin
  luaL_openlib(L, n, lib, 0);
end;

  (******************************************************************************
  * Copyright (C) 1994-2015 Lua.org, PUC-Rio.
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
