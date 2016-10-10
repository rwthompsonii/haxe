package lib.lua.luv;

@:luaRequire("luv")
extern class Process extends Handle {
  static function disable_stdio_inheritance() : Void;
  function spawn(path : String, options : ProcessOptions, cb : Int->Signal->Void ) : Int;
  function kill(sig:String) : Int;
}

typedef ProcessOptions = {
  args : Table<Int,String>,
  stdio : Table<Int,String>
}
