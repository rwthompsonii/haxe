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

/**
 This class is meant for internal compiler use only. It provides the Haxe runtime
 compatibility to the host language. Do not access it directly.
**/

@:native('haxe.lang.Runtime')
@:nativeGen
@:keep class Runtime
{
	public static var undefined:Dynamic = { };

	@:functionCode('
	')
	public static function closure(obj:Dynamic, field:String):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function eq(v1:Dynamic, v2:Dynamic):Bool
	{
		return false;
	}

	@:functionCode('
	')
	public static function refEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}

	@:functionCode('
	')
	public static function valEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}

	@:functionCode('
	')
	public static function toDouble(obj:Dynamic):Float
	{
		return 0.0;
	}

	@:functionCode('
	')
	public static function toBool(obj:Dynamic):Bool
	{
		return false;
	}

	@:functionCode('
	')
	public static function toInt(obj:Dynamic):Int
	{
		return 0;
	}

	public static function toLong(obj:Dynamic):StdTypes.Int64
	{
		return new StdTypes.Int64();
	}

	@:functionCode('
	')
	public static function isDouble(obj:Dynamic):Bool
	{
		return false;
	}

	@:overload public static function isInt(obj:Dynamic):Bool
	{
	    return false;
    }

	@:functionCode('
	')
	public static function slowHasField(o:Dynamic, field:String):Bool
	{
		return false;
	}

	@:functionCode('')
	public static function compare(v1:Dynamic, v2:Dynamic):Int
	{
		return 0;
	}

	@:functionCode('
	')
	public static function plus(v1:Dynamic, v2:Dynamic):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function slowGetField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function slowSetField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function slowCallField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function callField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function getField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function getField_f(obj:Dynamic, field:String, throwErrors:Bool):Float
	{
		return 0.0;
	}

	@:functionCode('
	')
	public static function setField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		return null;
	}

	@:functionCode('
	')
	public static function setField_f(obj:Dynamic, field:String, value:Float):Float
	{
		return 0.0;
	}

	public static function toString(obj:Dynamic):String
	{
		return ""; 
	}

	public static function isFinite(v:Float):Bool
	{
		return false;
	}
}

@:keep @:native("haxe.lang.EmptyObject") enum EmptyObject
{
	EMPTY;
}
