/*
 * Copyright (C)2005-2016 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package sys;

import lua.Io;
import lua.Os;
import lua.Lib;
import lua.Table;
import haxe.io.Path;

class FileSystem {
	public static function exists( path : String ) : Bool {
		if (path == null) return false;
		else{
			var f = Io.open(path);
			if (f == null) return false;
			else {
				f.close();
				return true;
			}
		}
	}

	public inline static function rename( path : String, newPath : String ) : Void {
		return  Os.rename(path, newPath);
	}

	public inline static function stat( path : String ) : FileStat {
		var l =  lua.lib.luv.fs.FileSystem.stat(path);
		return {
			gid   : l.gid,
			uid   : l.uid,
			rdev  : l.rdev,
			size  : l.size,
			nlink : l.nlink,
			mtime : Date.fromTime(l.mtime.sec + l.mtime.nsec/10000000),
			mode  : l.mode,
			ino   : l.ino,
			dev   : l.dev,
			ctime : Date.fromTime(l.ctime.sec + l.ctime.nsec/1000000),
			atime : Date.fromTime(l.atime.sec + l.atime.nsec/1000000)
		};
	}

	public inline static function fullPath( relPath : String ) : String {
		return Path.normalize(absolutePath(relPath));
	}

	public inline static function absolutePath( relPath : String ) : String {
		var pwd = lua.lib.luv.Misc.cwd();
		if (pwd == null) return relPath;
		return Path.join([pwd, relPath]);
	}

	public inline static function deleteFile( path : String ) : Void {
		lua.Os.remove(path);
	}

	public inline static function readDirectory( path : String ) : Array<String> {
		var scandir = lua.lib.luv.fs.FileSystem.scandir(path);

		var itr = function(){
			var k = lua.lib.luv.fs.FileSystem.scandir_next(scandir).name;
			return k;
		}
		return lua.Lib.fillArray(itr);
	}

	public inline static function isDirectory( path : String ) : Bool {
		return  lua.lib.luv.fs.FileSystem.stat(path).type ==  "directory";
	}

	public inline static function deleteDirectory( path : String ) : Void {
		lua.lib.luv.fs.FileSystem.rmdir(path);
	}

	public inline static function createDirectory( path : String ) : Void {
		lua.lib.luv.fs.FileSystem.mkdir(path, 511);
	}
}
