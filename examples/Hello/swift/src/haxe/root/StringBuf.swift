// Generated by Haxe 3.3.0
package haxe.root;

import haxe.root.*;

public class StringBuf : haxe.lang.HxObject
{
	public init() ->  Void
	{
	}
	
	
	
	
	public func add <T>(x : T) ->  Void
	{
	}
	
	
	public func toString() ->  String?
	{
		//line 45 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
		return nil
	}
	
	
	public override func __hx_getField(field : String?, throwErrors : Bool, isCheck : Bool, handleProperties : Bool) ->  Any?
	{
		//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
		switch (field)
		{
			case "toString":
			{
				//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
				return this!.toString
			}
			
			
			case "add":
			{
				//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
				return this!.add
			}
			
			
			default:
			{
				//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
				return super!.__hx_getField(field, throwErrors, isCheck, handleProperties)
			}
			
		}
		
	}
	
	
	public override func __hx_invokeField(field : String?, dynargs : haxe.root.Array) ->  Any?
	{
		//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
		switch (field)
		{
			case "toString":
			{
				//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
				return this!.toString()
			}
			
			
			case "add":
			{
				//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
				this!.add(dynargs!.__get(0))
			}
			
			
			default:
			{
				//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
				return super!.__hx_invokeField(field, dynargs)
			}
			
		}
		
		//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
		return nil
	}
	
	
	public override func __hx_getFields(baseArr : haxe.root.Array<String??>) ->  Void
	{
		//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
		baseArr!.push("length")
		//line 24 "/home/ron/haxe/std/swift/_std/StringBuf.hx"
		super!.__hx_getFields(baseArr)
	}
	
	
}


