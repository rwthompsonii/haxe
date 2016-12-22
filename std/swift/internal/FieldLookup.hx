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
package swift.internal;

@:native('haxe.lang.FieldLookup')
@:keep
@:static private class FieldLookup
{

	@:functionCode('
	')
	public static function hash(s:String):Int
	{
		return 0;
	}

	public static function findHash(hash:String, hashs:swift.NativeArray<String>, length:Int):Int
	{
		return 0;
    }

	static function removeString(a:swift.NativeArray<String>, length:Int, pos:Int) {
    }

	static function removeFloat(a:swift.NativeArray<Float>, length:Int, pos:Int) {
	}

	static function removeDynamic(a:swift.NativeArray<Dynamic>, length:Int, pos:Int) {
	}

	@:extern
	static inline function __insert<T>(a:swift.NativeArray<T>, length:Int, pos:Int, x:T):swift.NativeArray<T>
	{
        return null;
	}

	static function insertString(a:swift.NativeArray<String>, length:Int, pos:Int, x:String):swift.NativeArray<String> return __insert(a, length, pos, x);
	static function insertFloat(a:swift.NativeArray<Float>, length:Int, pos:Int, x:Float):swift.NativeArray<Float> return __insert(a, length, pos, x);
	static function insertDynamic(a:swift.NativeArray<Dynamic>, length:Int, pos:Int, x:Dynamic):swift.NativeArray<Dynamic> return __insert(a, length, pos, x);
}
