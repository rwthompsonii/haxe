//no copyright

using Lambda;

class ArrayDedup 
{
    public static function test() : Void 
    {
        var arr = [1, 2, 3, 5, 6, 2, 3, 4, 5, 6, 7];
        var uniqArr = ArrayExtension.unique(arr);
        trace(uniqArr);
    }
}

class ArrayExtension 
{
    public static function unique<T>(arr: Array<T>/*, 
                                     equals: function(el : T, el2 : T) -> Bool*/) : Array<T>
    {
        if(arr == null /*|| equals == null*/)
        {
            return new Array<T>();
        }
        if(arr.length == 0 || arr.length == 1 || arr[0] != arr[1])//optimizing common use cases
        {
            return arr;
        }

        //first we map up all the elements in the array.
        var map = new Map <T, Int>();
        
        var iterFunc = function(el){
            map.set(el, 0);
        };

        //note that map.set will *replace* existing elements. This function does not preserve
        //the order of the original array, so be warned.  
        //The last element will be the one you end up with.
        arr.iter(iterFunc);

        //since the map is already formed here and by definition only contains unique elements,
        //all we have to do is put it back into an array and we're done.
        var accumFunc = function(el1 : T, retArr : Array<T>) { 
            return retArr.push(el1);
        }

        //putting it all together.  
        return Lambda.fold(map, accumFunc, new Array<T>());

    }
}


