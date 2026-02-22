from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispExceptions import LispRuntimeFuncError
from pythonslisp.LispInterpreter import LispInterpreter


def register(primitive) -> None:

   @primitive( 'map', '(<key1> <val1>) (<key2> <val2>) ...', specialForm=True )
   def LP_map( env: Environment, *args ) -> Any:
      """Constructs and returns a map of key-value pairs."""
      theMapping = dict()
      requiredKeyType = None
      for entryNum,key_expr_pair in enumerate(args):
         try:
            key,expr = key_expr_pair
         except (ValueError, TypeError):
            raise LispRuntimeFuncError( LP_map, f'Entry {entryNum + 1} does not contain a (key value) pair.' )

         if isinstance( key, LSymbol ):
            key = key.strval

         if isinstance( key, (int,float,str) ):
            if requiredKeyType is None:
               requiredKeyType = type(key)
            elif type(key) != requiredKeyType:
               raise LispRuntimeFuncError( LP_map,
                  f'All keys in a map must be the same type. '
                  f'Entry {entryNum + 1} is {type(key).__name__}'
                  f', expected {requiredKeyType.__name__}.' )
            theMapping[ key ] = LispInterpreter._lEval( env, expr )
         else:
            raise LispRuntimeFuncError( LP_map, f'Entry {entryNum+1} has an invalid <key> type.' )
      return theMapping

   @primitive( 'car', '<list>' )
   def LP_car( env: Environment, *args ) -> Any:
      """Returns the first item in a list."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_car, '1 argument expected.' )
      theList = args[0]

      if not isinstance(theList, list):
         raise LispRuntimeFuncError( LP_car, '1st argument expected to be a list.' )

      try:
         return theList[0]
      except IndexError:
         return L_NIL

   @primitive( 'cdr', '<list>' )
   def LP_cdr( env: Environment, *args ) -> Any:
      """Returns a copy of the list minus the first element."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_cdr, '1 argument expected.' )
      theList = args[0]

      if not isinstance(theList, list):
         raise LispRuntimeFuncError( LP_cdr, '1st argument expected to be a list.' )

      return theList[1:]

   @primitive( 'cons', '<obj> <list>' )
   def LP_cons( env: Environment, *args ) -> Any:
      """Returns a copy of list with obj inserted into the front of the copy."""
      try:
         obj,consList = args
      except ValueError:
         raise LispRuntimeFuncError( LP_cons, '2 arguments expected.' )

      if not isinstance(consList, list):
         raise LispRuntimeFuncError( LP_cons, '2nd argument expected to be a list.' )

      return [ obj, *consList ]

   @primitive( 'push!', '<list> <value>' )
   def LP_push( env: Environment, *args ) -> Any:
      """Pushes a value onto the back of a list."""
      try:
         alist, value = args
      except ValueError:
         raise LispRuntimeFuncError( LP_push, '2 arguments expected.' )

      if not isinstance(alist, list):
         raise LispRuntimeFuncError( LP_push, '1st argument expected to be a list.' )
      alist.append( value )
      return alist

   @primitive( 'pop!', '<list>' )
   def LP_pop( env: Environment, *args ) -> Any:
      """Pops and returns the last value of a list."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_pop, '1 argument expected.' )
      alist = args[0]

      if not isinstance(alist, list):
         raise LispRuntimeFuncError( LP_pop, '1st argument expected to be a list.' )

      try:
         value = alist.pop()
      except IndexError:
         raise LispRuntimeFuncError( LP_pop, 'Invalid argument.' )
      return value

   @primitive( 'at', '<keyOrIndex> <mapListOrStr>' )
   def LP_at( env: Environment, *args ) -> Any:
      """Returns the value at a specified index of a list or string,
      or specified key of a map."""
      try:
         key,keyed = args
      except ValueError:
         raise LispRuntimeFuncError( LP_at, '2 arguments expected.' )

      if not isinstance(keyed, (list, dict, str) ):
         raise LispRuntimeFuncError( LP_at, 'Invalid argument.  List, Map, or String expected.' )

      if isinstance( key, LSymbol ):
         key = key.strval

      try:
         return keyed[ key ]
      except ( KeyError, IndexError, TypeError ):
         raise LispRuntimeFuncError( LP_at, 'Invalid argument key/index.' )

   @primitive( 'at-delete', '<keyOrIndex> <mapOrList>' )
   def LP_atDelete( env: Environment, *args ) -> bool:
      """Deletes the key-value pair from a map or list specified by keyOrIndex."""
      try:
         key, keyed = args
      except ValueError:
         raise LispRuntimeFuncError( LP_atDelete, "Exactly 2 arguments expected." )

      if not isinstance( keyed, (list, dict) ):
         raise LispRuntimeFuncError( LP_atDelete, "Argument 2 expected to be a list or map." )

      try:
         del keyed[key]
      except ( IndexError, KeyError, TypeError ):
         raise LispRuntimeFuncError( LP_atDelete, "Bad index or key into collection." )

      return L_T

   @primitive( 'at-insert', '<index> <list> <newItem>' )
   def LP_atInsert( env: Environment, *args ) -> bool:
      """Inserts newItem into list at the position specified by index.  Returns newItem."""
      try:
         index, lst, newItem = args
      except ValueError:
         raise LispRuntimeFuncError( LP_atInsert, "Exactly 3 arguments expected." )

      if not isinstance(index, int):
         raise LispRuntimeFuncError( LP_atInsert, "Argument 1 expected to be an integer index." )

      if not isinstance( lst, list ):
         raise LispRuntimeFuncError( LP_atInsert, "Argument 2 expected to be a list." )

      lst.insert( index, newItem )
      return newItem

   @primitive( 'append', '<list1> <list2> ...' )
   def LP_append( env: Environment, *args ) -> Any:
      """Returns a new list with the contents of the argument lists merged.  Order is retained."""
      if len(args) < 2:
         raise LispRuntimeFuncError( LP_append, 'At least 2 arguments expected.' )

      resultList = list( )
      for lst in args:
         if not isinstance( lst,  list ):
            raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )
         for item in lst:
            resultList.append( item )
      return resultList

   @primitive( 'hasValue?', '<listOrMap> <value>' )
   def LP_hasValue( env: Environment, *args ) -> Any:
      """Returns t if the list/map contains value otherwise nil."""
      try:
         keyed,aVal = args
      except ValueError:
         raise LispRuntimeFuncError( LP_hasValue, '2 arguments expected.' )

      if isinstance(keyed, list):
         pass
      elif isinstance(keyed, dict):
         keyed = keyed.values()
      else:
         raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.  Argument 1 expected to be a list or map.')

      return L_T if aVal in keyed else L_NIL

   @primitive( 'update!', '<map1> <map2>' )
   def LP_update( env: Environment, *args ) -> Any:
      """Updates map1's data with map2's."""
      try:
         map1,map2 = args
      except ValueError:
         raise LispRuntimeFuncError( LP_update, '2 arguments expected.' )

      if not isinstance( map1, dict ):
         raise LispRuntimeFuncError( LP_update, 'Argument 1 expected to be a map.' )

      if not isinstance( map2, dict ):
         raise LispRuntimeFuncError( LP_update, 'Argument 2 expected to be a map.' )

      map1.update( map2 )
      return map1

   @primitive( 'hasKey?', '<map> <key>' )
   def LP_hasKey( env: Environment, *args ) -> Any:
      """Returns t if the key is in the map otherwise nil."""
      try:
         aMap,aKey = args
      except ValueError:
         raise LispRuntimeFuncError( LP_hasKey, '2 arguments expected.' )

      if not isinstance(aMap, dict):
         raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument 1.  Map expected.')

      if isinstance( aKey, LSymbol ):
         aKey = aKey.strval

      return L_T if aKey in aMap else L_NIL

   @primitive( 'sort', '<list>' )
   def LP_sorted( env: Environment, *args ) -> Any:
      """Returns a copy of the list sorted."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_sorted, "Exactly 1 argument expected." )

      theList = args[0]
      if not isinstance(theList, list):
         raise LispRuntimeFuncError( LP_sorted, "Argument 1 expected to be a list." )

      try:
         return sorted( theList )
      except TypeError:
         raise LispRuntimeFuncError( LP_sorted, 'Cannot sort a list with incomparable types.' )

   @primitive( 'length', '<sequence>' )
   def LP_length( env: Environment, *args ) -> Any:
      """Returns the number of elements in a list, string, or map."""
      if len(args) != 1:
         raise LispRuntimeFuncError( LP_length, '1 argument expected.' )
      arg = args[0]
      if isinstance(arg, (list, str, dict)):
         return len(arg)
      raise LispRuntimeFuncError( LP_length, 'Argument 1 must be a List, String, or Map.' )
