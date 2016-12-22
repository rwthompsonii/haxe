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
package swift;

/**
	Represents a swift fixed-size Array (`T[]`)
**/
@:nativeGen class NativeArray<T> implements ArrayAccess<T>
{
	/**
		Creates a new array with the specified elements.

		Usage:
		```haxe
		var elements = NativeArray.make(1,2,3,4,5,6);
		```
	 **/
    @:functionCode('var ret Array<T> = Array<T>.self.init()
                    for e in elements {
                        ret.append(e)
                    }
                    return ret')
	public static function make<T>():NativeArray<T>
    {
        return null;
    }

	/**
		The length of the array
	 **/
	public var length(get, null):Int;
    
    @:functionCode('return self.count')
    private function get_length() : Int 
    {
        return 0;
    }
    
	/**
		Allocates a new array with size `len`
	 **/
    @:functionCode(' self.init(len)')
	public function new(len:Int):Void
    {
    }
}
