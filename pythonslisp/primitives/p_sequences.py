from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispContext import LispContext
from pythonslisp.LispExceptions import LispRuntimeFuncError


def register(primitive) -> None:

   @primitive( 'make-dict', '(<key1> <val1>) (<key2> <val2>) ...', specialForm=True )
   def LP_make_dict( ctx: LispContext, env: Environment, *args ) -> Any:
      """Constructs and returns a dict of key-value pairs."""
      theMapping = dict()
      requiredKeyType = None
      for entryNum,key_expr_pair in enumerate(args):
         try:
            key,expr = key_expr_pair
         except (ValueError, TypeError):
            raise LispRuntimeFuncError( LP_make_dict, f'Entry {entryNum + 1} does not contain a (key value) pair.' )

         if isinstance( key, LSymbol ):
            key = key.strval

         if isinstance( key, (int,float,str) ):
            if requiredKeyType is None:
               requiredKeyType = type(key)
            elif type(key) != requiredKeyType:
               raise LispRuntimeFuncError( LP_make_dict,
                  f'All keys in a map must be the same type. '
                  f'Entry {entryNum + 1} is {type(key).__name__}'
                  f', expected {requiredKeyType.__name__}.' )
            theMapping[ key ] = ctx.lEval( env, expr )
         else:
            raise LispRuntimeFuncError( LP_make_dict, f'Entry {entryNum+1} has an invalid <key> type.' )
      return theMapping

   @primitive( 'car', '<list>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_car( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the first item in a list."""
      theList = args[0]

      if not isinstance(theList, list):
         raise LispRuntimeFuncError( LP_car, '1st argument expected to be a list.' )

      try:
         return theList[0]
      except IndexError:
         return L_NIL

   @primitive( 'cdr', '<list>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_cdr( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of the list minus the first element."""
      theList = args[0]

      if not isinstance(theList, list):
         raise LispRuntimeFuncError( LP_cdr, '1st argument expected to be a list.' )

      return theList[1:]

   @primitive( 'cons', '<obj> <list>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_cons( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of list with obj inserted into the front of the copy."""
      obj, consList = args

      if not isinstance(consList, list):
         raise LispRuntimeFuncError( LP_cons, '2nd argument expected to be a list.' )

      return [ obj, *consList ]

   @primitive( 'push!', '<list> <value>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_push( ctx: LispContext, env: Environment, *args ) -> Any:
      """Pushes a value onto the back of a list."""
      alist, value = args

      if not isinstance(alist, list):
         raise LispRuntimeFuncError( LP_push, '1st argument expected to be a list.' )
      alist.append( value )
      return alist

   @primitive( 'pop!', '<list>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_pop( ctx: LispContext, env: Environment, *args ) -> Any:
      """Pops and returns the last value of a list."""
      alist = args[0]

      if not isinstance(alist, list):
         raise LispRuntimeFuncError( LP_pop, '1st argument expected to be a list.' )

      try:
         value = alist.pop()
      except IndexError:
         raise LispRuntimeFuncError( LP_pop, 'Invalid argument.' )
      return value

   @primitive( 'at', '<keyOrIndex> <mapListOrStr>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_at( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the value at a specified index of a list or string,
      or specified key of a map."""
      key, keyed = args

      if not isinstance(keyed, (list, dict, str) ):
         raise LispRuntimeFuncError( LP_at, 'Invalid argument.  List, Map, or String expected.' )

      if isinstance( key, LSymbol ):
         key = key.strval

      try:
         return keyed[ key ]
      except ( KeyError, IndexError, TypeError ):
         raise LispRuntimeFuncError( LP_at, 'Invalid argument key/index.' )

   @primitive( 'at-set', '<keyOrIndex> <mapListOrStr> <newValue>',
               min_args=3, max_args=3, arity_msg='Exactly 3 arguments expected.' )
   def LP_atSet( ctx: LispContext, env: Environment, *args ) -> Any:
      """Sets the value at a specified index of a list,
      or specified key of a map.  Returns newValue."""
      key, keyed, newValue = args

      if not isinstance(keyed, (list, dict)):
         raise LispRuntimeFuncError( LP_atSet, 'Invalid argument.  List or Map expected.' )

      if isinstance( key, LSymbol ):
         key = key.strval

      try:
         keyed[ key ] = newValue
      except ( KeyError, IndexError, TypeError ):
         raise LispRuntimeFuncError( LP_atSet, 'Invalid argument key/index.' )
      
      return newValue

   @primitive( 'at-delete', '<keyOrIndex> <mapOrList>',
               min_args=2, max_args=2, arity_msg='Exactly 2 arguments expected.' )
   def LP_atDelete( ctx: LispContext, env: Environment, *args ) -> bool:
      """Deletes the key-value pair from a map or list specified by keyOrIndex."""
      key, keyed = args

      if not isinstance( keyed, (list, dict) ):
         raise LispRuntimeFuncError( LP_atDelete, "Argument 2 expected to be a list or map." )

      try:
         del keyed[key]
      except ( IndexError, KeyError, TypeError ):
         raise LispRuntimeFuncError( LP_atDelete, "Bad index or key into collection." )

      return L_T

   @primitive( 'at-insert', '<index> <list> <newItem>',
               min_args=3, max_args=3, arity_msg='Exactly 3 arguments expected.' )
   def LP_atInsert( ctx: LispContext, env: Environment, *args ) -> bool:
      """Inserts newItem into list at the position specified by index.  Returns newItem."""
      index, lst, newItem = args

      if not isinstance(index, int):
         raise LispRuntimeFuncError( LP_atInsert, "Argument 1 expected to be an integer index." )

      if not isinstance( lst, list ):
         raise LispRuntimeFuncError( LP_atInsert, "Argument 2 expected to be a list." )

      lst.insert( index, newItem )
      return newItem

   @primitive( 'append', '<list1> <list2> ...',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_append( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a new list with the contents of the argument lists merged.  Order is retained."""

      resultList = list( )
      for lst in args:
         if not isinstance( lst,  list ):
            raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )
         for item in lst:
            resultList.append( item )
      return resultList

   @primitive( 'hasValue?', '<listOrMap> <value>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_hasValue( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the list/map contains value otherwise nil."""
      keyed, aVal = args

      if isinstance(keyed, list):
         pass
      elif isinstance(keyed, dict):
         keyed = keyed.values()
      else:
         raise LispRuntimeFuncError( LP_hasValue, 'Invalid argument.  Argument 1 expected to be a list or map.')

      return L_T if aVal in keyed else L_NIL

   @primitive( 'update!', '<map1> <map2>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_update( ctx: LispContext, env: Environment, *args ) -> Any:
      """Updates map1's data with map2's."""
      map1, map2 = args

      if not isinstance( map1, dict ):
         raise LispRuntimeFuncError( LP_update, 'Argument 1 expected to be a map.' )

      if not isinstance( map2, dict ):
         raise LispRuntimeFuncError( LP_update, 'Argument 2 expected to be a map.' )

      map1.update( map2 )
      return map1

   @primitive( 'hasKey?', '<map> <key>',
               min_args=2, max_args=2, arity_msg='2 arguments expected.' )
   def LP_hasKey( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns t if the key is in the map otherwise nil."""
      aMap, aKey = args

      if not isinstance(aMap, dict):
         raise LispRuntimeFuncError( LP_hasKey, 'Invalid argument 1.  Map expected.')

      if isinstance( aKey, LSymbol ):
         aKey = aKey.strval

      return L_T if aKey in aMap else L_NIL

   @primitive( 'sort', '<list>',
               min_args=1, max_args=1, arity_msg='Exactly 1 argument expected.' )
   def LP_sorted( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of the list sorted."""
      theList = args[0]
      if not isinstance(theList, list):
         raise LispRuntimeFuncError( LP_sorted, "Argument 1 expected to be a list." )

      try:
         return sorted( theList )
      except TypeError:
         raise LispRuntimeFuncError( LP_sorted, 'Cannot sort a list with incomparable types.' )

   @primitive( 'length', '<sequence>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_length( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the number of elements in a list, string, or map."""
      arg = args[0]
      if isinstance(arg, (list, str, dict)):
         return len(arg)
      raise LispRuntimeFuncError( LP_length, 'Argument 1 must be a List, String, or Map.' )
