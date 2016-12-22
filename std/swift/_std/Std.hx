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

import Reflect;

@:keep @:coreApi @:nativeGen class Std {

	@:functionCode('return (v is t)')
    public static function is( v : Dynamic, t : Dynamic ) : Bool
	{
        return false;
	}

    @:functionCode('return (s is String) ? s as! String : String(s)')
	public static function string( s : Dynamic ) : String {
	    return null;
    }

    @:functionCode('return Int(x)')
	public static function int( x : Float ) : Int {
		return 0;
	}

	@:functionCode('return (x != nil) ? Int(x) : nil')
	public static function parseInt( x : String ) : Null<Int> {
		return null;
	}

    @:functionCode('return (x != nil) ? Float(x) : nil')
	public static function parseFloat( x : String ) : Float {
	    return 0.0;
    }

    @:functionCode('return c != nil ? (value != nil ? value as? c : nil) : nil')
	inline public static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
	    return null;
    }

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return Std.int(Math.random() * x);
	}

}
