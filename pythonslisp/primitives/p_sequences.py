import functools
from typing import Any, Callable

from pythonslisp.Environment import Environment
from pythonslisp.LispAST import LSymbol
from pythonslisp.LispAST import L_T, L_NIL
from pythonslisp.LispContext import LispContext
from pythonslisp.LispEnvironment import LispEnvironment
from pythonslisp.LispExceptions import LispRuntimeFuncError, LispArgBindingError


# ── Shared keyword-argument helpers ──────────────────────────────────────────

def _is_nil_val( val ) -> bool:
   """True iff val is the Lisp NIL (empty list)."""
   return isinstance( val, list ) and not val


def _bind_kw( ll: list, args: tuple, ctx: Any, env: Environment, fn: Any ) -> LispEnvironment:
   """Create a child LispEnvironment, bind args against ll, wrap errors."""
   kw_env = LispEnvironment( parent=env )
   try:
      kw_env.bindArguments( ll, args, ctx.lEval )
   except LispArgBindingError as e:
      raise LispRuntimeFuncError( fn, str(e) )
   return kw_env


def _extract_key( ctx: Any, env: Environment, key_fn: Any, item: Any ) -> Any:
   """Apply the :key function to item, or return item unchanged if key_fn is NIL."""
   if _is_nil_val( key_fn ):
      return item
   return ctx.lApply( env, key_fn, [item] )


def _apply_test( ctx: Any, env: Environment, test_fn: Any, item: Any, cell_key: Any ) -> bool:
   """Call test_fn(item, cell_key); return True iff the result is truthy (non-NIL)."""
   return ctx.lApply( env, test_fn, [item, cell_key] ) != L_NIL


def _validate_bounds( start: Any, end: Any, seqlen: int, fn: Any ) -> tuple[int, int]:
   """Validate :start and :end; return (start_n, end_n) as concrete ints."""
   if not isinstance( start, int ) or isinstance( start, bool ):
      raise LispRuntimeFuncError( fn, ':start must be a non-negative integer.' )
   if start < 0:
      raise LispRuntimeFuncError( fn, ':start must be non-negative.' )
   if _is_nil_val( end ):
      return start, seqlen
   if not isinstance( end, int ) or isinstance( end, bool ):
      raise LispRuntimeFuncError( fn, ':end must be a non-negative integer or NIL.' )
   if end < 0 or end > seqlen:
      raise LispRuntimeFuncError( fn, f':end {end} out of range for sequence of length {seqlen}.' )
   if end < start:
      raise LispRuntimeFuncError( fn, f':end {end} must be >= :start {start}.' )
   return start, end


def _validate_count( count: Any, fn: Any ):
   """Validate :count; return None for unlimited, else the int limit."""
   if _is_nil_val( count ):
      return None
   if not isinstance( count, int ) or isinstance( count, bool ):
      raise LispRuntimeFuncError( fn, ':count must be a non-negative integer or NIL.' )
   if count < 0:
      raise LispRuntimeFuncError( fn, ':count must be non-negative.' )
   return count


# ── Primitive registration ────────────────────────────────────────────────────

def register( primitive, parseLispString: Callable ) -> None:

   def _ll( s: str ) -> list:
      """Parse a lambda-list string; strip the PROGN wrapper parseLispString adds."""
      return parseLispString( s )[1]

   # Pre-parsed lambda lists for the sort and CL sequence functions.
   # Stored as closures so each primitive body can reference its own list.
   _SORT_LL      = _ll( '(sequence predicate &key (key nil))' )
   _MEMBER_LL    = _ll( '(item list &key (test eql) (key nil))' )
   _ASSOC_LL     = _ll( '(item alist &key (test eql) (key nil))' )
   _FIND_LL      = _ll( '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil))' )
   _FIND_IF_LL   = _ll( '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil))' )
   _POS_LL       = _ll( '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil))' )
   _POS_IF_LL    = _ll( '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil))' )
   _COUNT_LL     = _ll( '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil))' )
   _COUNT_IF_LL  = _ll( '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil))' )
   _REMOVE_LL    = _ll( '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil) (count nil))' )
   _REM_IF_LL    = _ll( '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil))' )
   _REM_IFNOT_LL = _ll( '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil))' )
   _SUBST_LL     = _ll( '(new old sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil) (count nil))' )
   _SUBST_IF_LL  = _ll( '(new pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil))' )

   # ── Existing non-keyword primitives ────────────────────────────────────────

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

   @primitive( 'append', '&rest <lists>' )
   def LP_append( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a new list with the contents of the argument lists merged.  Order is retained.
(append) = NIL; (append lst) = lst; 2+ args: all must be proper lists."""
      if len(args) == 0:
         return L_NIL
      if len(args) == 1:
         return args[0]
      resultList = list( )
      for lst in args:
         if not isinstance( lst, list ):
            raise LispRuntimeFuncError( LP_append, 'Invalid argument.' )
         resultList.extend( lst )
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

   @primitive( 'sort', 'sequence predicate &key (key nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_sort( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of the list sorted by predicate (a two-arg less-than test).
The optional :key function extracts the comparison key from each element."""
      kw       = _bind_kw( _SORT_LL, args, ctx, env, LP_sort )
      seq      = kw.lookup( 'SEQUENCE' )
      pred     = kw.lookup( 'PREDICATE' )
      key_fn   = kw.lookup( 'KEY' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_sort, 'Argument 1 expected to be a list.' )

      def _cmp( a, b ):
         ka = _extract_key( ctx, env, key_fn, a )
         kb = _extract_key( ctx, env, key_fn, b )
         if ctx.lApply( env, pred, [ka, kb] ) != L_NIL:
            return -1
         if ctx.lApply( env, pred, [kb, ka] ) != L_NIL:
            return 1
         return 0

      try:
         return sorted( seq, key=functools.cmp_to_key(_cmp) )
      except TypeError:
         raise LispRuntimeFuncError( LP_sort, 'Cannot sort a list with incomparable types.' )

   @primitive( 'length', '<sequence>',
               min_args=1, max_args=1, arity_msg='1 argument expected.' )
   def LP_length( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the number of elements in a list, string, or map."""
      arg = args[0]
      if isinstance(arg, (list, str, dict)):
         return len(arg)
      raise LispRuntimeFuncError( LP_length, 'Argument 1 must be a List, String, or Map.' )

   @primitive( 'subseq', '<sequence> <start> &optional <end>',
               min_args=2, max_args=3, arity_msg='2 or 3 arguments expected.' )
   def LP_subseq( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a subsequence of a list or string from start (inclusive) to end (exclusive).
If end is not provided, returns from start to the end of the sequence."""
      seq = args[0]
      start = args[1]
      end = args[2] if len(args) > 2 else None

      if not isinstance(seq, (list, str)):
         raise LispRuntimeFuncError( LP_subseq, '1st argument must be a list or string.' )
      if not isinstance(start, int) or isinstance(start, bool):
         raise LispRuntimeFuncError( LP_subseq, '2nd argument must be an integer.' )
      if end is not None and (not isinstance(end, int) or isinstance(end, bool)):
         raise LispRuntimeFuncError( LP_subseq, '3rd argument must be an integer.' )

      seqLen = len(seq)
      if start < 0:
         raise LispRuntimeFuncError( LP_subseq, 'Start index must be non-negative.' )
      if start > seqLen:
         raise LispRuntimeFuncError( LP_subseq, 'Start index out of bounds.' )
      if end is not None:
         if end < 0:
            raise LispRuntimeFuncError( LP_subseq, 'End index must be non-negative.' )
         if end > seqLen:
            raise LispRuntimeFuncError( LP_subseq, 'End index out of bounds.' )
         if end < start:
            raise LispRuntimeFuncError( LP_subseq, 'End index must be >= start index.' )
         return seq[start:end]

      return seq[start:]

   # ── CL sequence functions with full keyword-argument support ───────────────

   @primitive( 'member', 'item list &key (test eql) (key nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_member( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the tail of list beginning with the first element whose :key
satisfies :test when compared to item.  Returns NIL if no match is found.
Default :test is eql.  Default :key is identity (NIL)."""
      kw      = _bind_kw( _MEMBER_LL, args, ctx, env, LP_member )
      item    = kw.lookup( 'ITEM' )
      lst     = kw.lookup( 'LIST' )
      test_fn = kw.lookup( 'TEST' )
      key_fn  = kw.lookup( 'KEY' )
      if not isinstance( lst, list ):
         raise LispRuntimeFuncError( LP_member, '2nd argument must be a list.' )
      for i in range( len(lst) ):
         if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, lst[i] ) ):
            return lst[i:]
      return L_NIL

   @primitive( 'assoc', 'item alist &key (test eql) (key nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_assoc( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the first pair in alist whose car (optionally extracted via :key)
satisfies :test when compared to item.  Non-cons elements in alist are skipped.
Returns NIL if no match is found.  Default :test is eql.  Default :key is identity."""
      kw      = _bind_kw( _ASSOC_LL, args, ctx, env, LP_assoc )
      item    = kw.lookup( 'ITEM' )
      alist   = kw.lookup( 'ALIST' )
      test_fn = kw.lookup( 'TEST' )
      key_fn  = kw.lookup( 'KEY' )
      if not isinstance( alist, list ):
         raise LispRuntimeFuncError( LP_assoc, '2nd argument must be a list.' )
      for pair in alist:
         if isinstance( pair, list ) and pair:
            if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, pair[0] ) ):
               return pair
      return L_NIL

   @primitive( 'find', 'item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_find( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the first element of sequence (bounded by :start/:end) whose :key
satisfies :test when compared to item.  If :from-end is true, searches right
to left and returns the rightmost match.  Returns NIL if not found."""
      kw       = _bind_kw( _FIND_LL, args, ctx, env, LP_find )
      item     = kw.lookup( 'ITEM' )
      seq      = kw.lookup( 'SEQUENCE' )
      test_fn  = kw.lookup( 'TEST' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_find, '2nd argument must be a list.' )
      start_n, end_n = _validate_bounds( start, end, len(seq), LP_find )
      indices = range( start_n, end_n )
      if not _is_nil_val( from_end ):
         indices = reversed( range( start_n, end_n ) )
      for i in indices:
         if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[i] ) ):
            return seq[i]
      return L_NIL

   @primitive( 'find-if', 'pred sequence &key (key nil) (from-end nil) (start 0) (end nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_find_if( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the first element of sequence (bounded by :start/:end) for which
pred returns true when applied to the element's :key.  If :from-end is true,
returns the rightmost such element.  Returns NIL if none found."""
      kw       = _bind_kw( _FIND_IF_LL, args, ctx, env, LP_find_if )
      pred     = kw.lookup( 'PRED' )
      seq      = kw.lookup( 'SEQUENCE' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_find_if, '2nd argument must be a list.' )
      start_n, end_n = _validate_bounds( start, end, len(seq), LP_find_if )
      indices = range( start_n, end_n )
      if not _is_nil_val( from_end ):
         indices = reversed( range( start_n, end_n ) )
      for i in indices:
         cell_key = _extract_key( ctx, env, key_fn, seq[i] )
         if ctx.lApply( env, pred, [cell_key] ) != L_NIL:
            return seq[i]
      return L_NIL

   @primitive( 'position', 'item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_position( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the index in sequence of the first element whose :key satisfies
:test when compared to item.  If :from-end is true, returns the index of the
rightmost such element.  Returns NIL if not found."""
      kw       = _bind_kw( _POS_LL, args, ctx, env, LP_position )
      item     = kw.lookup( 'ITEM' )
      seq      = kw.lookup( 'SEQUENCE' )
      test_fn  = kw.lookup( 'TEST' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_position, '2nd argument must be a list.' )
      start_n, end_n = _validate_bounds( start, end, len(seq), LP_position )
      indices = range( start_n, end_n )
      if not _is_nil_val( from_end ):
         indices = reversed( range( start_n, end_n ) )
      for i in indices:
         if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[i] ) ):
            return i
      return L_NIL

   @primitive( 'position-if', 'pred sequence &key (key nil) (from-end nil) (start 0) (end nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_position_if( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the index in sequence of the first element for which pred returns
true when applied to the element's :key.  If :from-end is true, returns the
index of the rightmost such element.  Returns NIL if none found."""
      kw       = _bind_kw( _POS_IF_LL, args, ctx, env, LP_position_if )
      pred     = kw.lookup( 'PRED' )
      seq      = kw.lookup( 'SEQUENCE' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_position_if, '2nd argument must be a list.' )
      start_n, end_n = _validate_bounds( start, end, len(seq), LP_position_if )
      indices = range( start_n, end_n )
      if not _is_nil_val( from_end ):
         indices = reversed( range( start_n, end_n ) )
      for i in indices:
         cell_key = _extract_key( ctx, env, key_fn, seq[i] )
         if ctx.lApply( env, pred, [cell_key] ) != L_NIL:
            return i
      return L_NIL

   @primitive( 'count', 'item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_count( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the number of elements in sequence (bounded by :start/:end) whose
:key satisfies :test when compared to item."""
      kw      = _bind_kw( _COUNT_LL, args, ctx, env, LP_count )
      item    = kw.lookup( 'ITEM' )
      seq     = kw.lookup( 'SEQUENCE' )
      test_fn = kw.lookup( 'TEST' )
      key_fn  = kw.lookup( 'KEY' )
      start   = kw.lookup( 'START' )
      end     = kw.lookup( 'END' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_count, '2nd argument must be a list.' )
      start_n, end_n = _validate_bounds( start, end, len(seq), LP_count )
      n = 0
      for i in range( start_n, end_n ):
         if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[i] ) ):
            n += 1
      return n

   @primitive( 'count-if', 'pred sequence &key (key nil) (from-end nil) (start 0) (end nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_count_if( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the number of elements in sequence (bounded by :start/:end) for
which pred returns true when applied to the element's :key."""
      kw     = _bind_kw( _COUNT_IF_LL, args, ctx, env, LP_count_if )
      pred   = kw.lookup( 'PRED' )
      seq    = kw.lookup( 'SEQUENCE' )
      key_fn = kw.lookup( 'KEY' )
      start  = kw.lookup( 'START' )
      end    = kw.lookup( 'END' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_count_if, '2nd argument must be a list.' )
      start_n, end_n = _validate_bounds( start, end, len(seq), LP_count_if )
      n = 0
      for i in range( start_n, end_n ):
         cell_key = _extract_key( ctx, env, key_fn, seq[i] )
         if ctx.lApply( env, pred, [cell_key] ) != L_NIL:
            n += 1
      return n

   @primitive( 'remove', 'item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil) (count nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_remove( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of sequence with elements matching item removed.  An
element matches if its :key satisfies :test when compared to item.  Only the
bounded region [:start, :end) is considered.  :count limits how many elements
are removed; :from-end causes removal from the right when :count is supplied."""
      kw       = _bind_kw( _REMOVE_LL, args, ctx, env, LP_remove )
      item     = kw.lookup( 'ITEM' )
      seq      = kw.lookup( 'SEQUENCE' )
      test_fn  = kw.lookup( 'TEST' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      count    = kw.lookup( 'COUNT' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_remove, '2nd argument must be a list.' )
      seqlen = len( seq )
      start_n, end_n = _validate_bounds( start, end, seqlen, LP_remove )
      count_n = _validate_count( count, LP_remove )
      indices = list( range( start_n, end_n ) )
      if not _is_nil_val( from_end ):
         indices = list( reversed( indices ) )
      to_remove: set = set()
      n_removed = 0
      for idx in indices:
         if count_n is not None and n_removed >= count_n:
            break
         if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[idx] ) ):
            to_remove.add( idx )
            n_removed += 1
      return [ seq[i] for i in range( seqlen ) if i not in to_remove ]

   @primitive( 'remove-if', 'pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_remove_if( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of sequence with elements removed where pred returns true
for the element's :key.  Only the bounded region [:start, :end) is considered.
:count limits removals; :from-end causes removal from the right."""
      kw       = _bind_kw( _REM_IF_LL, args, ctx, env, LP_remove_if )
      pred     = kw.lookup( 'PRED' )
      seq      = kw.lookup( 'SEQUENCE' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      count    = kw.lookup( 'COUNT' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_remove_if, '2nd argument must be a list.' )
      seqlen = len( seq )
      start_n, end_n = _validate_bounds( start, end, seqlen, LP_remove_if )
      count_n = _validate_count( count, LP_remove_if )
      indices = list( range( start_n, end_n ) )
      if not _is_nil_val( from_end ):
         indices = list( reversed( indices ) )
      to_remove: set = set()
      n_removed = 0
      for idx in indices:
         if count_n is not None and n_removed >= count_n:
            break
         cell_key = _extract_key( ctx, env, key_fn, seq[idx] )
         if ctx.lApply( env, pred, [cell_key] ) != L_NIL:
            to_remove.add( idx )
            n_removed += 1
      return [ seq[i] for i in range( seqlen ) if i not in to_remove ]

   @primitive( 'remove-if-not', 'pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil)',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_remove_if_not( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of sequence keeping only elements where pred returns true
for the element's :key.  Only the bounded region [:start, :end) is considered.
:count limits how many elements are removed; :from-end removes from the right."""
      kw       = _bind_kw( _REM_IFNOT_LL, args, ctx, env, LP_remove_if_not )
      pred     = kw.lookup( 'PRED' )
      seq      = kw.lookup( 'SEQUENCE' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      count    = kw.lookup( 'COUNT' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_remove_if_not, '2nd argument must be a list.' )
      seqlen = len( seq )
      start_n, end_n = _validate_bounds( start, end, seqlen, LP_remove_if_not )
      count_n = _validate_count( count, LP_remove_if_not )
      indices = list( range( start_n, end_n ) )
      if not _is_nil_val( from_end ):
         indices = list( reversed( indices ) )
      to_remove: set = set()
      n_removed = 0
      for idx in indices:
         if count_n is not None and n_removed >= count_n:
            break
         cell_key = _extract_key( ctx, env, key_fn, seq[idx] )
         if ctx.lApply( env, pred, [cell_key] ) == L_NIL:   # remove where pred is FALSE
            to_remove.add( idx )
            n_removed += 1
      return [ seq[i] for i in range( seqlen ) if i not in to_remove ]

   @primitive( 'substitute', 'new old sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil) (count nil)',
               min_args=3, arity_msg='At least 3 arguments expected.' )
   def LP_substitute( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of sequence with occurrences of old replaced by new.  An
element matches old if its :key satisfies :test when compared to old.  Only
the bounded region [:start, :end) is considered.  :count limits replacements;
:from-end replaces from the right when :count is supplied."""
      kw       = _bind_kw( _SUBST_LL, args, ctx, env, LP_substitute )
      new      = kw.lookup( 'NEW' )
      old      = kw.lookup( 'OLD' )
      seq      = kw.lookup( 'SEQUENCE' )
      test_fn  = kw.lookup( 'TEST' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      count    = kw.lookup( 'COUNT' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_substitute, '3rd argument must be a list.' )
      seqlen = len( seq )
      start_n, end_n = _validate_bounds( start, end, seqlen, LP_substitute )
      count_n = _validate_count( count, LP_substitute )
      result = list( seq )
      indices = list( range( start_n, end_n ) )
      if not _is_nil_val( from_end ):
         indices = list( reversed( indices ) )
      n_done = 0
      for idx in indices:
         if count_n is not None and n_done >= count_n:
            break
         if _apply_test( ctx, env, test_fn, old, _extract_key( ctx, env, key_fn, seq[idx] ) ):
            result[idx] = new
            n_done += 1
      return result

   @primitive( 'substitute-if', 'new pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil)',
               min_args=3, arity_msg='At least 3 arguments expected.' )
   def LP_substitute_if( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns a copy of sequence with elements replaced by new where pred returns
true for the element's :key.  Only the bounded region [:start, :end) is
considered.  :count limits replacements; :from-end replaces from the right."""
      kw       = _bind_kw( _SUBST_IF_LL, args, ctx, env, LP_substitute_if )
      new      = kw.lookup( 'NEW' )
      pred     = kw.lookup( 'PRED' )
      seq      = kw.lookup( 'SEQUENCE' )
      key_fn   = kw.lookup( 'KEY' )
      from_end = kw.lookup( 'FROM-END' )
      start    = kw.lookup( 'START' )
      end      = kw.lookup( 'END' )
      count    = kw.lookup( 'COUNT' )
      if not isinstance( seq, list ):
         raise LispRuntimeFuncError( LP_substitute_if, '3rd argument must be a list.' )
      seqlen = len( seq )
      start_n, end_n = _validate_bounds( start, end, seqlen, LP_substitute_if )
      count_n = _validate_count( count, LP_substitute_if )
      result = list( seq )
      indices = list( range( start_n, end_n ) )
      if not _is_nil_val( from_end ):
         indices = list( reversed( indices ) )
      n_done = 0
      for idx in indices:
         if count_n is not None and n_done >= count_n:
            break
         cell_key = _extract_key( ctx, env, key_fn, seq[idx] )
         if ctx.lApply( env, pred, [cell_key] ) != L_NIL:
            result[idx] = new
            n_done += 1
      return result

   # ── Multi-sequence mapping functions ──────────────────────────────────────

   @primitive( 'mapcar', 'fn &rest sequences',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_mapcar( ctx: LispContext, env: Environment, *args ) -> Any:
      """Applies fn element-wise across one or more sequences (lists) and returns
a list of the results.  Stops at the shortest sequence."""
      fn   = args[0]
      seqs = args[1:]
      for i, s in enumerate(seqs):
         if not isinstance( s, list ):
            raise LispRuntimeFuncError( LP_mapcar, f'Argument {i + 2} must be a list.' )
      result = []
      for elts in zip( *seqs ):
         result.append( ctx.lApply( env, fn, list(elts) ) )
      return result

   @primitive( 'every', 'pred &rest sequences',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_every( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns T if pred returns true for every element-wise group across sequences.
Returns NIL at the first false result.  Returns T for empty sequences."""
      pred = args[0]
      seqs = args[1:]
      for elts in zip( *seqs ):
         result = ctx.lApply( env, pred, list(elts) )
         if _is_nil_val( result ):
            return L_NIL
      return L_T

   @primitive( 'some', 'pred &rest sequences',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_some( ctx: LispContext, env: Environment, *args ) -> Any:
      """Returns the first truthy value pred returns across the sequences.
Returns NIL if pred returns NIL for every element-wise group."""
      pred = args[0]
      seqs = args[1:]
      for elts in zip( *seqs ):
         result = ctx.lApply( env, pred, list(elts) )
         if not _is_nil_val( result ):
            return result
      return L_NIL

   @primitive( 'mapc', 'fn &rest sequences',
               min_args=2, arity_msg='At least 2 arguments expected.' )
   def LP_mapc( ctx: LispContext, env: Environment, *args ) -> Any:
      """Applies fn element-wise across one or more sequences for side effects.
Returns the first sequence."""
      fn       = args[0]
      seqs     = args[1:]
      first_seq = seqs[0]
      for elts in zip( *seqs ):
         ctx.lApply( env, fn, list(elts) )
      return first_seq
