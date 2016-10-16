package lua.lib.lpath;
@:luaRequire("path.fs")
extern class Fs {
	public static function setenv(name : String, value : String) : Void;
}
