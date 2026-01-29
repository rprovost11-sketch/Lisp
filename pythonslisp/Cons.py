

class LCons( object ):
   __slots__ = ('car', 'cdr', 'marked', 'free')
   
   def __init__( self, initialCar=None, initialCdr=None ):
      self.car = initialCar if initialCar else L_NIL
      self.cdr = initialCdr if initialCdr else L_NIL
      self.marked = False
      self.free = False
   
   def isEmpty( self ):
      return False

   def len( self ):
      ptr = self
      ctr = 0
      while ptr != L_NIL:
         ptr = ptr.cdr
         ctr += 1
      return ctr
   
   def at( self, index ):
      ptr = self
      ctr = 0
      while (ptr != L_NIL) and (ctr < index):
         ptr = ptr.cdr
         ctr += 1
      if ctr == index:
         return ptr.car
      raise IndexError("Index out of range.")
   
   def at_set( self, index, value ):
      ptr = self
      ctr = 0
      while (ptr != L_NIL) and (ctr < index):
         ptr = ptr.cdr
         ctr += 1
      if ctr == index:
         ptr.car = value
         return value
      raise IndexError("Index out of range.")

   def __iter__( self ):
      return LCons_Iterator( self )

   def __str__( self ) -> str:
      if self.isEmpty():
         return 'NIL'

      mbrList = [ prettyPrintSExpr(mbr) for mbr in self ]
      mbrListStr = ' '.join(mbrList)
      resultStr = f'({mbrListStr})'
      return resultStr

   def __repr__( self ) -> str:
      if self.isEmpty():
         return 'NIL'

      mbrList = [ prettyPrintSExpr(mbr) for mbr in self ]
      mbrListStr = ' '.join(mbrList)
      resultStr = f'({mbrListStr})'
      return resultStr

class LNil( LCons ):
   def __init__( self ):
      super().__init__( self, self )
   
   def isEmpty( self ):
      return True

   def len(self):
      return 0

L_NIL = LNil( )

class LCons_Iterator(object):
   def __init__( self, lCons: LCons ):
      self.theCons = lCons
   
   def __iter__( self ):
      return self
   
   def __next__( self ):
      if self.theCons.isEmpty():
         raise StopIteration()
      
      value = self.theCons.car
      self.theCons = self.theCons.cdr
      return value

class LHeap(object):
   def __init__( self ):
      self.heap = { }
      for address in range( 10_000 ):
         self.heap[ address ] = LCons()