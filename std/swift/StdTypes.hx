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

@:notNull @:runtimeValue @:coreType extern abstract Int from Int to Int
{
    @:overload(function(i : Int) : Void {} )
    public function new() : Void;

    @:op(A+B) public static function add(lhs:Int, rhs:Int):Int;
	@:op(A*B) public static function mul(lhs:Int, rhs:Int):Int;
	@:op(A%B) public static function mod(lhs:Int, rhs:Int):Int;
	@:op(A-B) public static function sub(lhs:Int, rhs:Int):Int;
	@:op(A/B) public static function div(lhs:Int, rhs:Int):Int;
	@:op(A|B) public static function or(lhs:Int, rhs:Int):Int;
	@:op(A^B) public static function xor(lhs:Int, rhs:Int):Int;
	@:op(A&B) public static function and(lhs:Int, rhs:Int):Int;
	@:op(A<<B) public static function shl(lhs:Int, rhs:Int):Int;
	@:op(A>>B) public static function shr(lhs:Int, rhs:Int):Int;

	@:op(A>B) public static function gt(lhs:Int, rhs:Int):Bool;
	@:op(A>=B) public static function gte(lhs:Int, rhs:Int):Bool;
	@:op(A<B) public static function lt(lhs:Int, rhs:Int):Bool;
	@:op(A<=B) public static function lte(lhs:Int, rhs:Int):Bool;

	@:op(~A) public static function bneg(t:Int):Int;
	@:op(-A) public static function neg(t:Int):Int;

	@:op(++A) public static function preIncrement(t:Int):Int;
	@:op(A++) public static function postIncrement(t:Int):Int;
	@:op(--A) public static function preDecrement(t:Int):Int;
	@:op(A--) public static function postDecrement(t:Int):Int;
}

@:notNull @:runtimeValue @:coreType extern abstract Int64 from Int64 to Int64
{  
    @:overload(function(i : Int) : Void {} )
    public function new() : Void;

    @:op(A+B) public static function add(lhs:Int64, rhs:Int64):Int64;
	@:op(A*B) public static function mul(lhs:Int64, rhs:Int64):Int64;
	@:op(A%B) public static function mod(lhs:Int64, rhs:Int64):Int64;
	@:op(A-B) public static function sub(lhs:Int64, rhs:Int64):Int64;
	@:op(A/B) public static function div(lhs:Int64, rhs:Int64):Int64;
	@:op(A|B) public static function or(lhs:Int64, rhs:Int64):Int64;
	@:op(A^B) public static function xor(lhs:Int64, rhs:Int64):Int64;
	@:op(A&B) public static function and(lhs:Int64, rhs:Int64):Int64;
	@:op(A<<B) public static function shl(lhs:Int64, rhs:Int64):Int64;
	@:op(A>>B) public static function shr(lhs:Int64, rhs:Int64):Int64;

	@:op(A>B) public static function gt(lhs:Int64, rhs:Int64):Bool;
	@:op(A>=B) public static function gte(lhs:Int64, rhs:Int64):Bool;
	@:op(A<B) public static function lt(lhs:Int64, rhs:Int64):Bool;
	@:op(A<=B) public static function lte(lhs:Int64, rhs:Int64):Bool;

	@:op(~A) public static function bneg(t:Int64):Int64;
	@:op(-A) public static function neg(t:Int64):Int64;

	@:op(++A) public static function preIncrement(t:Int64):Int64;
	@:op(A++) public static function postIncrement(t:Int64):Int64;
	@:op(--A) public static function preDecrement(t:Int64):Int64;
	@:op(A--) public static function postDecrement(t:Int64):Int64;
}

@:notNull @:runtimeValue @:coreType extern abstract Int32 from Int32 to Int32
{
    @:overload(function(i : Int) : Void {} )
    public function new() : Void;

    @:op(A+B) public static function add(lhs:Int32, rhs:Int32):Int32;
	@:op(A*B) public static function mul(lhs:Int32, rhs:Int32):Int32;
	@:op(A%B) public static function mod(lhs:Int32, rhs:Int32):Int32;
	@:op(A-B) public static function sub(lhs:Int32, rhs:Int32):Int32;
	@:op(A/B) public static function div(lhs:Int32, rhs:Int32):Int32;
	@:op(A|B) public static function or(lhs:Int32, rhs:Int32):Int32;
	@:op(A^B) public static function xor(lhs:Int32, rhs:Int32):Int32;
	@:op(A&B) public static function and(lhs:Int32, rhs:Int32):Int32;
	@:op(A<<B) public static function shl(lhs:Int32, rhs:Int32):Int32;
	@:op(A>>B) public static function shr(lhs:Int32, rhs:Int32):Int32;

	@:op(A>B) public static function gt(lhs:Int32, rhs:Int32):Bool;
	@:op(A>=B) public static function gte(lhs:Int32, rhs:Int32):Bool;
	@:op(A<B) public static function lt(lhs:Int32, rhs:Int32):Bool;
	@:op(A<=B) public static function lte(lhs:Int32, rhs:Int32):Bool;

	@:op(~A) public static function bneg(t:Int32):Int32;
	@:op(-A) public static function neg(t:Int32):Int32;

	@:op(++A) public static function preIncrement(t:Int32):Int32;
	@:op(A++) public static function postIncrement(t:Int32):Int32;
	@:op(--A) public static function preDecrement(t:Int32):Int32;
	@:op(A--) public static function postDecrement(t:Int32):Int32;
}

@:notNull @:runtimeValue @:coreType extern abstract Int16 from Int16 to Int16
{
    @:overload(function(i : Int) : Void {} )
    public function new() : Void;

    @:op(A+B) public static function add(lhs:Int16, rhs:Int16):Int16;
	@:op(A*B) public static function mul(lhs:Int16, rhs:Int16):Int16;
	@:op(A%B) public static function mod(lhs:Int16, rhs:Int16):Int16;
	@:op(A-B) public static function sub(lhs:Int16, rhs:Int16):Int16;
	@:op(A/B) public static function div(lhs:Int16, rhs:Int16):Int16;
	@:op(A|B) public static function or(lhs:Int16, rhs:Int16):Int16;
	@:op(A^B) public static function xor(lhs:Int16, rhs:Int16):Int16;
	@:op(A&B) public static function and(lhs:Int16, rhs:Int16):Int16;
	@:op(A<<B) public static function shl(lhs:Int16, rhs:Int16):Int16;
	@:op(A>>B) public static function shr(lhs:Int16, rhs:Int16):Int16;

	@:op(A>B) public static function gt(lhs:Int16, rhs:Int16):Bool;
	@:op(A>=B) public static function gte(lhs:Int16, rhs:Int16):Bool;
	@:op(A<B) public static function lt(lhs:Int16, rhs:Int16):Bool;
	@:op(A<=B) public static function lte(lhs:Int16, rhs:Int16):Bool;

	@:op(~A) public static function bneg(t:Int16):Int16;
	@:op(-A) public static function neg(t:Int16):Int16;

	@:op(++A) public static function preIncrement(t:Int16):Int16;
	@:op(A++) public static function postIncrement(t:Int16):Int16;
	@:op(--A) public static function preDecrement(t:Int16):Int16;
	@:op(A--) public static function postDecrement(t:Int16):Int16;
}

@:notNull @:runtimeValue @:coreType extern abstract Int8 from Int8 to Int8
{
    @:overload(function(i : Int) : Void {} )
    public function new() : Void;

    @:op(A+B) public static function add(lhs:Int8, rhs:Int8):Int8;
	@:op(A*B) public static function mul(lhs:Int8, rhs:Int8):Int8;
	@:op(A%B) public static function mod(lhs:Int8, rhs:Int8):Int8;
	@:op(A-B) public static function sub(lhs:Int8, rhs:Int8):Int8;
	@:op(A/B) public static function div(lhs:Int8, rhs:Int8):Int8;
	@:op(A|B) public static function or(lhs:Int8, rhs:Int8):Int8;
	@:op(A^B) public static function xor(lhs:Int8, rhs:Int8):Int8;
	@:op(A&B) public static function and(lhs:Int8, rhs:Int8):Int8;
	@:op(A<<B) public static function shl(lhs:Int8, rhs:Int8):Int8;
	@:op(A>>B) public static function shr(lhs:Int8, rhs:Int8):Int8;

	@:op(A>B) public static function gt(lhs:Int8, rhs:Int8):Bool;
	@:op(A>=B) public static function gte(lhs:Int8, rhs:Int8):Bool;
	@:op(A<B) public static function lt(lhs:Int8, rhs:Int8):Bool;
	@:op(A<=B) public static function lte(lhs:Int8, rhs:Int8):Bool;

	@:op(~A) public static function bneg(t:Int8):Int8;
	@:op(-A) public static function neg(t:Int8):Int8;

	@:op(++A) public static function preIncrement(t:Int8):Int8;
	@:op(A++) public static function postIncrement(t:Int8):Int8;
	@:op(--A) public static function preDecrement(t:Int8):Int8;
	@:op(A--) public static function postDecrement(t:Int8):Int8;
}
