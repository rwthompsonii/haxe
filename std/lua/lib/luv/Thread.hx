package lua.lib.luv;

@:luaRequire("luv")
extern class Thread {
  static function new_thread() : Timer;
  @:native("new_thread") function new() : Void;
  function equal() : Bool;
  function self() : Thread;
  function join() : Bool;
  function sleep() : Void;
}
