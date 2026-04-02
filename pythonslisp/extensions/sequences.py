from __future__ import annotations
import functools
from typing import Any, Callable

from pythonslisp.Environment import Environment
from pythonslisp.AST import LSymbol, got_str
from pythonslisp.AST import L_T, L_NIL
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimePrimError, LRuntimeUsageError
from pythonslisp.extensions import LambdaListMode, primitive


# ── Shared keyword-argument helpers ──────────────────────────────────────────

def _is_nil_val( val ) -> bool:
   """True iff val is the Lisp NIL (empty list)."""
   return isinstance( val, list ) and not val


def _extract_key( ctx: Any, env: Environment, key_fn: Any, item: Any ) -> Any:
   """Apply the :key function to item, or return item unchanged if key_fn is NIL."""
   if _is_nil_val( key_fn ):
      return item
   return ctx.lApply( ctx, env, key_fn, [item] )


def _apply_test( ctx: Any, env: Environment, test_fn: Any, item: Any, cell_key: Any ) -> bool:
   """Call test_fn(item, cell_key); return True iff the result is truthy (non-NIL)."""
   return ctx.lApply( ctx, env, test_fn, [item, cell_key] ) != L_NIL


def _validate_bounds( start: Any, end: Any, seqlen: int, fn: Any ) -> tuple[int, int]:
   """Validate :start and :end; return (start_n, end_n) as concrete ints."""
   if not isinstance( start, int ) or isinstance( start, bool ):
      raise LRuntimePrimError( fn, f':start must be a non-negative integer{got_str(start)}.')
   if start < 0:
      raise LRuntimePrimError( fn, ':start must be non-negative.')
   if _is_nil_val( end ):
      return start, seqlen
   if not isinstance( end, int ) or isinstance( end, bool ):
      raise LRuntimePrimError( fn, f':end must be a non-negative integer or NIL{got_str(end)}.')
   if end < 0 or end > seqlen:
      raise LRuntimePrimError( fn, f':end {end} out of range for sequence of length {seqlen}.')
   if end < start:
      raise LRuntimePrimError( fn, f':end {end} must be >= :start {start}.')
   return start, end


def _validate_count( count: Any, fn: Any ):
   """Validate :count; return None for unlimited, else the int limit."""
   if _is_nil_val( count ):
      return None
   if not isinstance( count, int ) or isinstance( count, bool ):
      raise LRuntimePrimError( fn, f':count must be a non-negative integer or NIL{got_str(count)}.')
   if count < 0:
      raise LRuntimePrimError( fn, ':count must be non-negative.')
   return count


# ── Existing non-keyword primitives ────────────────────────────────────────

@primitive( 'make-dict', '((key1 val1) (key2 val2) ...)',
            mode=LambdaListMode.DOC_ONLY, special=True )
def LP_make_dict( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Constructs and returns a dict of key-value pairs."""
   raise LRuntimeUsageError( LP_make_dict, 'Handled by CEK machine.' )

@primitive( 'car', '(list)' )
def LP_car( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the first item in a list."""
   theList = args[0]

   if not isinstance(theList, list):
      raise LRuntimeUsageError( LP_car, f'Invalid argument 1. LIST expected{got_str(theList)}.' )

   try:
      return theList[0]
   except IndexError:
      return L_NIL

@primitive( 'cdr', '(list)' )
def LP_cdr( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of the list minus the first element."""
   theList = args[0]

   if not isinstance(theList, list):
      raise LRuntimeUsageError( LP_cdr, f'Invalid argument 1. LIST expected{got_str(theList)}.' )

   return theList[1:]

@primitive( 'cons', '(obj list)' )
def LP_cons( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of list with obj inserted into the front of the copy."""
   obj, consList = args

   if not isinstance(consList, list):
      raise LRuntimeUsageError( LP_cons, f'Invalid argument 2. LIST expected{got_str(consList)}.' )

   return [ obj, *consList ]

@primitive( 'push!', '(list value)' )
def LP_push( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Pushes a value onto the back of a list."""
   alist, value = args

   if not isinstance(alist, list):
      raise LRuntimeUsageError( LP_push, f'Invalid argument 1. LIST expected{got_str(alist)}.' )
   alist.append( value )
   return alist

@primitive( 'pop!', '(list)' )
def LP_pop( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Pops and returns the last value of a list."""
   alist = args[0]

   if not isinstance(alist, list):
      raise LRuntimeUsageError( LP_pop, f'Invalid argument 1. LIST expected{got_str(alist)}.' )

   try:
      value = alist.pop()
   except IndexError:
      raise LRuntimePrimError( LP_pop, 'Invalid argument 1. NON-EMPTY LIST expected.')
   return value

@primitive( 'at', '(keyOrIndex dictListOrStr)' )
def LP_at( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the value at a specified index of a list or string,
      or specified key of a map."""
   key, keyed = args

   if not isinstance(keyed, (list, dict, str) ):
      raise LRuntimeUsageError( LP_at, f'Invalid argument 2. LIST, DICT, or STRING expected{got_str(keyed)}.' )

   try:
      return keyed[ key ]
   except ( KeyError, IndexError, TypeError ):
      raise LRuntimePrimError( LP_at, 'Invalid argument 1. Key/index out of range or wrong type.')

@primitive( 'at-set', '(keyOrIndex dictListOrStr newValue)' )
def LP_atSet( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Sets the value at a specified index of a list,
      or specified key of a map.  Returns newValue."""
   key, keyed, newValue = args

   if not isinstance(keyed, (list, dict)):
      raise LRuntimeUsageError( LP_atSet, f'Invalid argument 2. LIST or DICT expected{got_str(keyed)}.' )

   try:
      keyed[ key ] = newValue
   except ( KeyError, IndexError, TypeError ):
      raise LRuntimePrimError( LP_atSet, 'Invalid argument 1. Key/index out of range or wrong type.')

   return newValue

@primitive( 'at-delete', '(keyOrIndex dictOrList)' )
def LP_atDelete( ctx: Context, env: Environment, args: list[Any] ) -> bool:
   """Deletes the key-value pair from a map or list specified by keyOrIndex."""
   key, keyed = args

   if not isinstance( keyed, (list, dict) ):
      raise LRuntimeUsageError( LP_atDelete, f'Invalid argument 2. LIST or DICT expected{got_str(keyed)}.' )

   try:
      del keyed[key]
   except ( IndexError, KeyError, TypeError ):
      raise LRuntimePrimError( LP_atDelete, 'Invalid argument 1. Valid key or index expected.')

   return L_T

@primitive( 'at-insert', '(index list newItem)' )
def LP_atInsert( ctx: Context, env: Environment, args: list[Any] ) -> bool:
   """Inserts newItem into list at the position specified by index.  Returns newItem."""
   index, lst, newItem = args

   if not isinstance(index, int):
      raise LRuntimeUsageError( LP_atInsert, f'Invalid argument 1. INTEGER expected{got_str(index)}.' )

   if not isinstance( lst, list ):
      raise LRuntimeUsageError( LP_atInsert, f'Invalid argument 2. LIST expected{got_str(lst)}.' )

   lst.insert( index, newItem )
   return newItem

@primitive( 'append', '(&rest lists)' )
def LP_append( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a new list with the contents of the argument lists merged.  Order is retained.
(append) = NIL; (append lst) = lst; 2+ args: all must be proper lists."""
   if len(args) == 0:
      return L_NIL
   if len(args) == 1:
      return args[0]
   resultList = list( )
   for i, lst in enumerate(args, 1):
      if not isinstance( lst, list ):
         raise LRuntimeUsageError( LP_append, f'Invalid argument {i}. LIST expected{got_str(lst)}.' )
      resultList.extend( lst )
   return resultList

@primitive( 'has-value-p', '(value listOrDict)' )
def LP_has_value_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the list/map contains value otherwise nil."""
   aVal, keyed = args

   if isinstance(keyed, list):
      pass
   elif isinstance(keyed, dict):
      keyed = keyed.values()
   else:
      raise LRuntimeUsageError( LP_has_value_p, f'Invalid argument 2. LIST or DICT expected{got_str(keyed)}.' )

   return L_T if aVal in keyed else L_NIL

@primitive( 'update!', '(dict1 dict2)' )
def LP_update( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Updates dict1's data with dict2's."""
   dict1, dict2 = args

   if not isinstance( dict1, dict ):
      raise LRuntimeUsageError( LP_update, f'Invalid argument 1. DICT expected{got_str(dict1)}.' )

   if not isinstance( dict2, dict ):
      raise LRuntimeUsageError( LP_update, f'Invalid argument 2. DICT expected{got_str(dict2)}.' )

   dict1.update( dict2 )
   return dict1

@primitive( 'has-key-p', '(key dict)' )
def LP_has_key_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns t if the key is in the map otherwise nil."""
   aKey, aMap = args

   if not isinstance(aMap, dict):
      raise LRuntimeUsageError( LP_has_key_p, f'Invalid argument 2. DICT expected{got_str(aMap)}.' )

   return L_T if aKey in aMap else L_NIL

@primitive( 'dict-keys', '(dict)' )
def LP_dict_keys( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a list of all keys in the dict, in insertion order."""
   aDict = args[0]
   if not isinstance( aDict, dict ):
      raise LRuntimeUsageError( LP_dict_keys, f'Invalid argument 1. DICT expected{got_str(aDict)}.' )
   return list( aDict.keys() )

@primitive( 'dict-values', '(dict)' )
def LP_dict_values( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a list of all values in the dict, in insertion order."""
   aDict = args[0]
   if not isinstance( aDict, dict ):
      raise LRuntimeUsageError( LP_dict_values, f'Invalid argument 1. DICT expected{got_str(aDict)}.' )
   return list( aDict.values() )

@primitive( 'dict-pairs', '(dict)' )
def LP_dict_pairs( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a list of (key value) pairs from the dict, in insertion order."""
   aDict = args[0]
   if not isinstance( aDict, dict ):
      raise LRuntimeUsageError( LP_dict_pairs, f'Invalid argument 1. DICT expected{got_str(aDict)}.' )
   return [ [k, v] for k, v in aDict.items() ]

@primitive( 'dict-length', '(dict)' )
def LP_dict_length( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the number of key-value pairs in the dict."""
   aDict = args[0]
   if not isinstance( aDict, dict ):
      raise LRuntimeUsageError( LP_dict_length, f'Invalid argument 1. DICT expected{got_str(aDict)}.' )
   return len( aDict )

@primitive( 'sort', '(sequence predicate &key (key nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_sort( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of the list sorted by predicate (a two-arg less-than test).
The optional :key function extracts the comparison key from each element."""
   seq      = env.lookup( 'SEQUENCE' )
   pred     = env.lookup( 'PREDICATE' )
   key_fn   = env.lookup( 'KEY' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_sort, f'Invalid argument 1. LIST expected{got_str(seq)}.' )

   def _cmp( a, b ):
      ka = _extract_key( ctx, env, key_fn, a )
      kb = _extract_key( ctx, env, key_fn, b )
      if ctx.lApply( ctx, env, pred, [ka, kb] ) != L_NIL:
         return -1
      if ctx.lApply( ctx, env, pred, [kb, ka] ) != L_NIL:
         return 1
      return 0

   try:
      return sorted( seq, key=functools.cmp_to_key(_cmp) )
   except TypeError:
      raise LRuntimePrimError( LP_sort, 'Cannot sort a list with incomparable types.')

@primitive( 'length', '(sequence)' )
def LP_length( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the number of elements in a list, string, or map."""
   arg = args[0]
   if isinstance(arg, (list, str, dict)):
      return len(arg)
   raise LRuntimeUsageError( LP_length, f'Invalid argument 1. LIST, STRING, or DICT expected{got_str(arg)}.' )

@primitive( 'subseq', '(sequence start &optional end)' )
def LP_subseq( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a subsequence of a list or string from start (inclusive) to end (exclusive).
If end is not provided, returns from start to the end of the sequence."""
   seq = args[0]
   start = args[1]
   end = args[2] if len(args) > 2 else None

   if not isinstance(seq, (list, str)):
      raise LRuntimeUsageError( LP_subseq, f'Invalid argument 1. LIST or STRING expected{got_str(seq)}.' )
   if not isinstance(start, int) or isinstance(start, bool):
      raise LRuntimeUsageError( LP_subseq, f'Invalid argument 2. INTEGER expected{got_str(start)}.' )
   if end is not None and (not isinstance(end, int) or isinstance(end, bool)):
      raise LRuntimeUsageError( LP_subseq, f'Invalid argument 3. INTEGER expected{got_str(end)}.' )

   seqLen = len(seq)
   if start < 0:
      raise LRuntimePrimError( LP_subseq, 'Start index must be non-negative.')
   if start > seqLen:
      raise LRuntimePrimError( LP_subseq, 'Start index out of bounds.')
   if end is not None:
      if end < 0:
         raise LRuntimePrimError( LP_subseq, 'End index must be non-negative.')
      if end > seqLen:
         raise LRuntimePrimError( LP_subseq, 'End index out of bounds.')
      if end < start:
         raise LRuntimePrimError( LP_subseq, 'End index must be >= start index.')
      return seq[start:end]

   return seq[start:]


# ── CL sequence functions with full keyword-argument support ───────────────

@primitive( 'member', '(item list &key (test eql) (key nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_member( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the tail of list beginning with the first element whose :key
satisfies :test when compared to item.  Returns NIL if no match is found.
Default :test is eql.  Default :key is identity (NIL)."""
   item    = env.lookup( 'ITEM' )
   lst     = env.lookup( 'LIST' )
   test_fn = env.lookup( 'TEST' )
   key_fn  = env.lookup( 'KEY' )
   if not isinstance( lst, list ):
      raise LRuntimeUsageError( LP_member, f'Invalid argument 2. LIST expected{got_str(lst)}.' )
   for i in range( len(lst) ):
      if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, lst[i] ) ):
         return lst[i:]
   return L_NIL

@primitive( 'assoc', '(item alist &key (test eql) (key nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_assoc( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the first pair in alist whose car (optionally extracted via :key)
satisfies :test when compared to item.  Non-cons elements in alist are skipped.
Returns NIL if no match is found.  Default :test is eql.  Default :key is identity."""
   item    = env.lookup( 'ITEM' )
   alist   = env.lookup( 'ALIST' )
   test_fn = env.lookup( 'TEST' )
   key_fn  = env.lookup( 'KEY' )
   if not isinstance( alist, list ):
      raise LRuntimeUsageError( LP_assoc, f'Invalid argument 2. LIST expected{got_str(alist)}.' )
   for pair in alist:
      if isinstance( pair, list ) and pair:
         if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, pair[0] ) ):
            return pair
   return L_NIL

@primitive( 'find', '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_find( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the first element of sequence (bounded by :start/:end) whose :key
satisfies :test when compared to item.  If :from-end is true, searches right
to left and returns the rightmost match.  Returns NIL if not found."""
   item     = env.lookup( 'ITEM' )
   seq      = env.lookup( 'SEQUENCE' )
   test_fn  = env.lookup( 'TEST' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_find, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
   start_n, end_n = _validate_bounds( start, end, len(seq), LP_find )
   indices = range( start_n, end_n )
   if not _is_nil_val( from_end ):
      indices = reversed( range( start_n, end_n ) )
   for i in indices:
      if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[i] ) ):
         return seq[i]
   return L_NIL

@primitive( 'find-if', '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_find_if( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the first element of sequence (bounded by :start/:end) for which
pred returns true when applied to the element's :key.  If :from-end is true,
returns the rightmost such element.  Returns NIL if none found."""
   pred     = env.lookup( 'PRED' )
   seq      = env.lookup( 'SEQUENCE' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_find_if, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
   start_n, end_n = _validate_bounds( start, end, len(seq), LP_find_if )
   indices = range( start_n, end_n )
   if not _is_nil_val( from_end ):
      indices = reversed( range( start_n, end_n ) )
   for i in indices:
      cell_key = _extract_key( ctx, env, key_fn, seq[i] )
      if ctx.lApply( ctx, env, pred, [cell_key] ) != L_NIL:
         return seq[i]
   return L_NIL

@primitive( 'position', '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_position( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the index in sequence of the first element whose :key satisfies
:test when compared to item.  If :from-end is true, returns the index of the
rightmost such element.  Returns NIL if not found."""
   item     = env.lookup( 'ITEM' )
   seq      = env.lookup( 'SEQUENCE' )
   test_fn  = env.lookup( 'TEST' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_position, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
   start_n, end_n = _validate_bounds( start, end, len(seq), LP_position )
   indices = range( start_n, end_n )
   if not _is_nil_val( from_end ):
      indices = reversed( range( start_n, end_n ) )
   for i in indices:
      if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[i] ) ):
         return i
   return L_NIL

@primitive( 'search', '(sequence1 sequence2 &key (test eql) (key nil) (from-end nil) (start1 0) (end1 nil) (start2 0) (end2 nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_search( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Searches sequence2 for a subsequence matching sequence1.  Returns the
starting index in sequence2 of the leftmost match, or NIL if not found.
With :from-end T, returns the rightmost match.  :start1/:end1 bound the
pattern; :start2/:end2 bound the search region.  Works on both strings
and lists.  For strings with the default test (eql), uses a fast native
search."""
   seq1     = env.lookup( 'SEQUENCE1' )
   seq2     = env.lookup( 'SEQUENCE2' )
   test_fn  = env.lookup( 'TEST' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start1   = env.lookup( 'START1' )
   end1     = env.lookup( 'END1' )
   start2   = env.lookup( 'START2' )
   end2     = env.lookup( 'END2' )
   if not isinstance( seq1, (list, str) ):
      raise LRuntimeUsageError( LP_search, f'Invalid argument 1. LIST or STRING expected{got_str(seq1)}.' )
   if not isinstance( seq2, (list, str) ):
      raise LRuntimeUsageError( LP_search, f'Invalid argument 2. LIST or STRING expected{got_str(seq2)}.' )
   if type(seq1) is not type(seq2):
      raise LRuntimeUsageError( LP_search, 'Invalid argument 2. Same type as argument 1 expected.' )
   s1_n, e1_n = _validate_bounds( start1, end1, len(seq1), LP_search )
   s2_n, e2_n = _validate_bounds( start2, end2, len(seq2), LP_search )
   pattern = seq1[s1_n:e1_n]
   pat_len = len( pattern )
   # Fast path: native string search with default eql test and no key
   if ( isinstance( seq1, str )
        and _is_nil_val( key_fn )
        and isinstance( test_fn, LSymbol ) and test_fn.name == 'EQL' ):
      if not _is_nil_val( from_end ):
         idx = seq2.rfind( pattern, s2_n, e2_n )
      else:
         idx = seq2.find( pattern, s2_n, e2_n )
      return idx if idx != -1 else L_NIL
   # General path: sliding window comparison
   search_range = range( s2_n, e2_n - pat_len + 1 )
   if not _is_nil_val( from_end ):
      search_range = reversed( search_range )
   for i in search_range:
      match = True
      for j in range( pat_len ):
         cell1 = _extract_key( ctx, env, key_fn, pattern[j] )
         cell2 = _extract_key( ctx, env, key_fn, seq2[i + j] )
         if not _apply_test( ctx, env, test_fn, cell1, cell2 ):
            match = False
            break
      if match:
         return i
   return L_NIL

@primitive( 'position-if', '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_position_if( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the index in sequence of the first element for which pred returns
true when applied to the element's :key.  If :from-end is true, returns the
index of the rightmost such element.  Returns NIL if none found."""
   pred     = env.lookup( 'PRED' )
   seq      = env.lookup( 'SEQUENCE' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_position_if, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
   start_n, end_n = _validate_bounds( start, end, len(seq), LP_position_if )
   indices = range( start_n, end_n )
   if not _is_nil_val( from_end ):
      indices = reversed( range( start_n, end_n ) )
   for i in indices:
      cell_key = _extract_key( ctx, env, key_fn, seq[i] )
      if ctx.lApply( ctx, env, pred, [cell_key] ) != L_NIL:
         return i
   return L_NIL

@primitive( 'count', '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_count( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the number of elements in sequence (bounded by :start/:end) whose
:key satisfies :test when compared to item."""
   item    = env.lookup( 'ITEM' )
   seq     = env.lookup( 'SEQUENCE' )
   test_fn = env.lookup( 'TEST' )
   key_fn  = env.lookup( 'KEY' )
   start   = env.lookup( 'START' )
   end     = env.lookup( 'END' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_count, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
   start_n, end_n = _validate_bounds( start, end, len(seq), LP_count )
   n = 0
   for i in range( start_n, end_n ):
      if _apply_test( ctx, env, test_fn, item, _extract_key( ctx, env, key_fn, seq[i] ) ):
         n += 1
   return n

@primitive( 'count-if', '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_count_if( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the number of elements in sequence (bounded by :start/:end) for
which pred returns true when applied to the element's :key."""
   pred   = env.lookup( 'PRED' )
   seq    = env.lookup( 'SEQUENCE' )
   key_fn = env.lookup( 'KEY' )
   start  = env.lookup( 'START' )
   end    = env.lookup( 'END' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_count_if, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
   start_n, end_n = _validate_bounds( start, end, len(seq), LP_count_if )
   n = 0
   for i in range( start_n, end_n ):
      cell_key = _extract_key( ctx, env, key_fn, seq[i] )
      if ctx.lApply( ctx, env, pred, [cell_key] ) != L_NIL:
         n += 1
   return n

@primitive( 'remove', '(item sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil) (count nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_remove( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of sequence with elements matching item removed.  An
element matches if its :key satisfies :test when compared to item.  Only the
bounded region [:start, :end) is considered.  :count limits how many elements
are removed; :from-end causes removal from the right when :count is supplied."""
   item     = env.lookup( 'ITEM' )
   seq      = env.lookup( 'SEQUENCE' )
   test_fn  = env.lookup( 'TEST' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   count    = env.lookup( 'COUNT' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_remove, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
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

@primitive( 'remove-if', '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_remove_if( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of sequence with elements removed where pred returns true
for the element's :key.  Only the bounded region [:start, :end) is considered.
:count limits removals; :from-end causes removal from the right."""
   pred     = env.lookup( 'PRED' )
   seq      = env.lookup( 'SEQUENCE' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   count    = env.lookup( 'COUNT' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_remove_if, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
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
      if ctx.lApply( ctx, env, pred, [cell_key] ) != L_NIL:
         to_remove.add( idx )
         n_removed += 1
   return [ seq[i] for i in range( seqlen ) if i not in to_remove ]

@primitive( 'remove-if-not', '(pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_remove_if_not( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of sequence keeping only elements where pred returns true
for the element's :key.  Only the bounded region [:start, :end) is considered.
:count limits how many elements are removed; :from-end removes from the right."""
   pred     = env.lookup( 'PRED' )
   seq      = env.lookup( 'SEQUENCE' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   count    = env.lookup( 'COUNT' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_remove_if_not, f'Invalid argument 2. LIST expected{got_str(seq)}.' )
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
      if ctx.lApply( ctx, env, pred, [cell_key] ) == L_NIL:   # remove where pred is FALSE
         to_remove.add( idx )
         n_removed += 1
   return [ seq[i] for i in range( seqlen ) if i not in to_remove ]

@primitive( 'substitute', '(new old sequence &key (test eql) (key nil) (from-end nil) (start 0) (end nil) (count nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_substitute( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of sequence with occurrences of old replaced by new.  An
element matches old if its :key satisfies :test when compared to old.  Only
the bounded region [:start, :end) is considered.  :count limits replacements;
:from-end replaces from the right when :count is supplied."""
   new      = env.lookup( 'NEW' )
   old      = env.lookup( 'OLD' )
   seq      = env.lookup( 'SEQUENCE' )
   test_fn  = env.lookup( 'TEST' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   count    = env.lookup( 'COUNT' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_substitute, f'Invalid argument 3. LIST expected{got_str(seq)}.' )
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

@primitive( 'substitute-if', '(new pred sequence &key (key nil) (from-end nil) (start 0) (end nil) (count nil))',
            mode=LambdaListMode.FULL_BINDING )
def LP_substitute_if( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a copy of sequence with elements replaced by new where pred returns
true for the element's :key.  Only the bounded region [:start, :end) is
considered.  :count limits replacements; :from-end replaces from the right."""
   new      = env.lookup( 'NEW' )
   pred     = env.lookup( 'PRED' )
   seq      = env.lookup( 'SEQUENCE' )
   key_fn   = env.lookup( 'KEY' )
   from_end = env.lookup( 'FROM-END' )
   start    = env.lookup( 'START' )
   end      = env.lookup( 'END' )
   count    = env.lookup( 'COUNT' )
   if not isinstance( seq, list ):
      raise LRuntimeUsageError( LP_substitute_if, f'Invalid argument 3. LIST expected{got_str(seq)}.' )
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
      if ctx.lApply( ctx, env, pred, [cell_key] ) != L_NIL:
         result[idx] = new
         n_done += 1
   return result


# ── Multi-sequence mapping functions ──────────────────────────────────────

@primitive( 'mapcar', '(fn seq &rest more-seqs)' )
def LP_mapcar( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Applies fn element-wise across one or more sequences (lists) and returns
a list of the results.  Stops at the shortest sequence."""
   fn   = args[0]
   seqs = args[1:]
   for i, s in enumerate(seqs):
      if not isinstance( s, list ):
         raise LRuntimeUsageError( LP_mapcar, f'Invalid argument {i + 2}. LIST expected{got_str(s)}.' )
   result = []
   for elts in zip( *seqs ):
      result.append( ctx.lApply( ctx, env, fn, list(elts) ) )
   return result

@primitive( 'every', '(pred seq &rest more-seqs)' )
def LP_every( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if pred returns true for every element-wise group across sequences.
Returns NIL at the first false result.  Returns T for empty sequences."""
   pred = args[0]
   seqs = args[1:]
   for elts in zip( *seqs ):
      result = ctx.lApply( ctx, env, pred, list(elts) )
      if _is_nil_val( result ):
         return L_NIL
   return L_T

@primitive( 'some', '(pred seq &rest more-seqs)' )
def LP_some( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the first truthy value pred returns across the sequences.
Returns NIL if pred returns NIL for every element-wise group."""
   pred = args[0]
   seqs = args[1:]
   for elts in zip( *seqs ):
      result = ctx.lApply( ctx, env, pred, list(elts) )
      if not _is_nil_val( result ):
         return result
   return L_NIL

@primitive( 'mapc', '(fn seq &rest more-seqs)' )
def LP_mapc( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Applies fn element-wise across one or more sequences for side effects.
Returns the first sequence."""
   fn       = args[0]
   seqs     = args[1:]
   first_seq = seqs[0]
   for elts in zip( *seqs ):
      ctx.lApply( ctx, env, fn, list(elts) )
   return first_seq
