with Ada.Finalization;

--  Implements a deque data structure which is suitable for use as a queue. Ada
--  does not provide a type like this in the standard library with suitable
--  performance characteristics. The closest thing is Vector which has O(N)
--  Delete_First. This type supports O(1) append and delete on both sides.
--
--  This deque is implemented as a linked list of arrays (blocks) and does not
--  support random access. When an element is pushed onto the deque on either
--  side, if that element would fall outside of existing storage, a new block
--  is created on that side and linked onto the storage list. Later, if a
--  storage block becomes empty, it is removed from the storage list and moved
--  into a free list. When a new storage block is created, the deque will first
--  try to take a block from the free list. If there are no blocks in the free
--  list, one will be dynamically allocated instead. This design allows the
--  deque use a roughly constant amount of memory if an equal number of pushes
--  and pops happen on opposite sides.

generic
   type Element_Type is private;

   --  The given type will determine how large an individual storage block is.
   type Block_Index is mod <>;
package Advent.Containers.Linked_Deques is

   pragma Pure;

   type Deque is tagged limited private;

   function Length (D : Deque) return Natural;

   function Capacity (D : Deque) return Natural;

   function Empty (D : Deque) return Boolean with
     Post => Empty'Result = (D.Length = 0);

   function Front (D : Deque) return Element_Type with
     Pre => not D.Empty;

   function Back (D : Deque) return Element_Type with
     Pre => not D.Empty;

   procedure Clear (D : in out Deque) with
     Post => D.Empty;

   procedure Push_Front (D : in out Deque; Value : Element_Type) with
     Post => D.Length = D.Length'Old + 1 and D.Front = Value;

   procedure Pop_Front (D : in out Deque; Value : out Element_Type) with
     Pre => not D.Empty, Post => D.Length = D.Length'Old - 1 and Value = D.Front'Old;

   procedure Push_Back (D : in out Deque; Value : Element_Type) with
     Post => D.Length = D.Length'Old + 1 and D.Back = Value;

   procedure Pop_Back (D : in out Deque; Value : out Element_Type) with
     Pre => not D.Empty, Post => D.Length = D.Length'Old - 1 and Value = D.Back'Old;

private

   type Element_Array is array (Block_Index) of Element_Type;

   type Block;

   type Block_Ptr is access Block;

   type Block is record
      Xs         : Element_Array;
      Next, Prev : Block_Ptr;
   end record;

   type Deque is new Ada.Finalization.Limited_Controlled with record
      Left, Right : Block_Ptr   := null;
      Length      : Natural     := 0;
      Left_Index  : Block_Index := 1;
      Right_Index : Block_Index := 0;
      Free_List   : Block_Ptr   := null;
   end record;

   overriding procedure Initialize (D : in out Deque);
   overriding procedure Finalize (D : in out Deque);

end Advent.Containers.Linked_Deques;
