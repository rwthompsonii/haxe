// Generated by Haxe 3.3.0
package haxe.root;

import haxe.root.*;

public class Type : haxe.lang.HxObject
{
	public class func getClass <T>(o : T) ->  Any?
	{
		//line 41 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
	public class func getEnum(o : Any?) ->  Any?
	{
		//line 46 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
	public class func getSuperClass(c : Any?) ->  Any?
	{
		//line 51 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
	public class func getClassName(c : Any?) ->  String?
	{
		//line 55 "/home/ron/haxe/std/swift/_std/Type.hx"
		return ""
	}
	
	
	public class func getEnumName(e : Any?) ->  String?
	{
		//line 59 "/home/ron/haxe/std/swift/_std/Type.hx"
		return ""
	}
	
	
	public class func resolveClass(name : String?) ->  Any?
	{
		//line 64 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
	public class func resolveEnum(name : String?) ->  Any?
	{
		
		
	
	}
	
	
	public class func createInstance <T>(cl : Any?, args : haxe.root.Array) ->  T
	{
		
			
	
	}
	
	
	public class func createEmptyInstance <T>(cl : Any?) ->  T
	{
		//line 89 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
	public class func createEnum <T>(e : Any?, constr : String?, params : haxe.root.Array) ->  T
	{
		
		
	
	}
	
	
	public class func createEnumIndex <T>(e : Any?, index : Int, params : haxe.root.Array) ->  T
	{
		//line 102 "/home/ron/haxe/std/swift/_std/Type.hx"
		return haxe.root.Type.createEnum(e, haxe.root.Type.getEnumConstructs(e)!.__get(index), params)
	}
	
	
	public class func getInstanceFields(c : Any?) ->  haxe.root.Array<String??>
	{
		
		
	
	}
	
	
	public class func getClassFields(c : Any?) ->  haxe.root.Array<String??>
	{
		
		
	
	}
	
	
	public class func getEnumConstructs(e : Any?) ->  haxe.root.Array<String??>
	{
		//line 120 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
	public class func typeof(v : Any?) ->  haxe.root.ValueType
	{
		
	
	
	}
	
	
	public class func enumEq <T>(a : T, b : T) ->  Bool
	{
		
			
	
	}
	
	
	public class func enumConstructor(e : Any?) ->  String?
	{
		
	
	
	}
	
	
	public class func enumParameters(e : Any?) ->  haxe.root.Array
	{
		
	
	
	}
	
	
	public class func enumIndex(e : Any?) ->  Int
	{
		
	
	}
	
	
	public class func allEnums <T>(e : Any?) ->  haxe.root.Array<T?>
	{
		//line 164 "/home/ron/haxe/std/swift/_std/Type.hx"
		return nil
	}
	
	
}


