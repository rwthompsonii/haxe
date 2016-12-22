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

//TODO: note that many of these functions require either `import Darwin` (on iOS/OSX) or `import Glibc` (on Linux).  The compiler needs to make sure these are imported in the generated code.

@:keep @:coreApi @:nativeGen 
class Math
{
	public static var PI(default, null) : Float;

    @:functionCode('return Double.pi')
    private static function get_PI() : Float 
    {
        return 0.0;
    }

    public static var NaN(default, null) : Float;

    @:functionCode('return Double.nan')
    private static function get_NaN() : Float
    {
        return 0.0;
    }
	
    public static var NEGATIVE_INFINITY(default, null) : Float;
	
    @:functionCode('return -Float.infinity')
    private static function get_NEGATIVE_INFINITY() : Float
    {
        return 0.0;
    }

    public static var POSITIVE_INFINITY(default, null) : Float;

    @:functionCode('return Float.infinity')
    private static function get_POSITIVE_INFINITY() : Float
    {
        return 0.0;
    }

    @:functionCode('return Swift.abs(v)')
	public static function abs(v : Float) : Float
    {
        return 0.0;
    }
                      
    @:functionCode('return Swift.min(a, b)')
	public static function min(a : Float, b : Float) : Float
    {
        return 0.0;
    }
	
    @:functionCode('return Swift.max(a, b)')
    public static function max(a : Float, b : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return sin(v)')
	public static function sin(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return cos(v)')
	public static function cos(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return atan2(y, x)')
	public static function atan2(y : Float, x : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return tan(v)')
	public static function tan(v : Float) : Float
    {
        return 0.0;
    }
   
    @:functionCode('return exp(v)')
    public static function exp(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return log(v)')	
    public static function log(v : Float) : Float
    {
        return 0.0;
    }
	
    @:functionCode('return sqrt(v)')	
    public static function sqrt(v : Float) : Float
    {
        return 0.0;
    }
	
    @:functionCode('return Int(round(v))')	
    public static function round(v : Float) : Int
    {
        return 0;
    }
	
    @:functionCode('return Int(floor(v))')	
    public static function floor(v : Float) : Int
    {
        return 0;
    }
	
    @:functionCode('return Int(ceil(v))')	
    public static function ceil(v : Float) : Int
    {
        return 0;
    }

    @:functionCode('return atan(v)')	
	public static function atan(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return round(v)')	
	public static function fround(v : Float) : Float 
    {
		return 0.0;
	}

    @:functionCode('return floor(v)')	
	public static function ffloor(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return ceil(v)')	
	public static function fceil(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return asin(v)')	
	public static function asin(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return acos(v)')	
	public static function acos(v : Float) : Float
    {
        return 0.0;
    }

    @:functionCode('return pow(v, exp)')	
	public static function pow(v : Float, exp : Float) : Float
    {
        return 0.0;
    }

    /**
      * TODO: note that this function requires a srandom(UInt32(time(nil))) call 
      * before otherwise it will always return the *same* random numbers.
      * the compiler should probably generate it anytime it detects this class
      * being imported, or maybe this function being called.
      * the logic here is because the random() call goes from 0 to 2^32 - 1.
      * and the caller expects to see a Float between 0 and 1, so we normalize.
      */
    @:functionCode('return (Float(random()) / Float(Int32.max))')	
	public static function random() : Float
    {
        return 0.0;
    }

    @:functionCode('return f.isFinite')	
	public static function isFinite( f : Float ) : Bool
    {
        return false;
    }
	
    @:functionCode('return f.isNan')	
    public static function isNaN( f : Float ) : Bool
    {
        return false;
    }
}


