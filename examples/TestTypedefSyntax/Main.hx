

class Main 
{
    public static function main() : Void 
    {
                
        var myTuple : MyTuple = {
            myInt1 : 2, myInt2 : 2, add : function (a, b) { return a + b; }
        };
        
        trace(myTuple.add(myTuple.myInt1, myTuple.myInt2));   
    }
}

typedef MyTuple = {
    var myInt1 : Int;
    var myInt2 : Int;
    public dynamic function add(a: Int, b: Int) : Int;
}
