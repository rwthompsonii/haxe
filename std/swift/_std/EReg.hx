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

@:keep @:coreApi class EReg {

	public function new( r : String, opt : String ) {
	}

	private static function convert(r:String):String
	{
		return r;
	}

	public function match( s : String ) : Bool {
	    return false;
    }

	public function matched( n : Int ) : String
	{
	    return "";
    }

	public function matchedLeft() : String
	{
	    return "";
    }

	public function matchedRight() : String
	{
		return "";
	}

	public function matchedPos() : { pos : Int, len : Int } 
    {
		return { pos : 0, len : 0 };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool
    {
		return false;
	}

	public function split( s : String ) : Array<String>
	{
		return [s];
	}

	inline function start(group:Int) : Int
	{
		return 0;
	}

	inline function len(group:Int) : Int
	{
		return 0;
	}

	public function replace( s : String, by : String ) : String
	{
        return "";
	}

	public function map( s : String, f : EReg -> String ) : String 
    {
		return "";
	}
}
