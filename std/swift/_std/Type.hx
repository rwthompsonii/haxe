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

using StringTools;

enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

@:keep @:coreApi class Type {

	public static function getClass<T>( o : T ) : Class<T>
	{
        return null;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic>
	{
		return null;
	}

	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic>
	{
		return null;
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
	    return "";
    }

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return "";
	}

	public static function resolveClass( name : String ) : Class<Dynamic>
	{
        return null;
	}


	@:functionCode('
		
	')
	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped
	{
	    return null;
	}

	@:functionCode('
			
	')
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped
	{
		return null;
	}

	// cache empty constructor arguments so we don't allocate it on each createEmptyInstance call
	//@:protected @:readOnly static var __createEmptyInstance_EMPTY_TYPES = swift.NativeArray.make(swift.Lib.toNativeEnum(swift.internal.Runtime.EmptyObject));
	//@:protected @:readOnly static var __createEmptyInstance_EMPTY_ARGS = swift.NativeArray.make(swift.internal.Runtime.EmptyObject.EMPTY);

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		return null;
	}

	@:functionCode('
		
	')
	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T
	{
		return null;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var constr = getEnumConstructs(e);
		return createEnum(e, constr[index], params);
	}

	@:functionCode('
		
	')
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	@:functionCode('
		
	')
	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
        return null;	
	}

	@:functionCode('
	
	')
	public static function typeof( v : Dynamic ) : ValueType untyped
	{
		return null;
	}

	@:functionCode('
			
	')
	public static function enumEq<T>( a : T, b : T ) : Bool untyped
	{
		return false;
	}

	@:functionCode('
	
	')
	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return null;
	}

	@:functionCode('
	
	')
	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return null;
	}

	@:functionCode('
	')
	public static function enumIndex( e : EnumValue ) : Int untyped
	{
		return 0;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T>
	{
		return null;
	}

}
