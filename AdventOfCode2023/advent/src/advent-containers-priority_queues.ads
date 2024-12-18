with Ada.Finalization;

--  Implements a reasonably fast, simple, unbounded priority queue using a
--  binary heap. Compared to GNAT's Ada.Containers.Unbounded_Priority_Queues,
--  this implementation does not use protected objects and is backed by an
--  array instead of an RB-tree.

generic
   type Element_Type is private;

   with function "<" (A, B : Element_Type) return Boolean;
package Advent.Containers.Priority_Queues is

   pragma Pure;

   subtype Queue_Index is Natural;

   type Queue is tagged limited private with
     Type_Invariant => Queue.Length <= Queue.Capacity;

   --  Returns the number of elements currently queued.
   function Length (Q : Queue) return Natural;

   --  Returns the number of elements the queue can hold without allocating
   --  more space.
   function Capacity (Q : Queue) return Natural;

   --  Returns the next element in the queue without removing it from the
   --  queue.
   function Peek (Q : Queue) return Element_Type with
     Pre => Q.Length > 0;

   --  Inserts `Value` into the queue `Q`. This function will allocate if
   --  Q.Length = Q.Capacity.
   procedure Enqueue (Q : in out Queue; Value : Element_Type) with
     Post => Q.Length = Q.Length'Old + 1 and Q.Capacity >= Q.Capacity'Old;

   --  Removes the next item with the highest priority from the queue.
   procedure Dequeue (Q : in out Queue; Value : out Element_Type) with
     Pre => Q.Length > 0, Post => Q.Capacity = Q.Capacity'Old and Value = Q.Peek'Old;

   --  Allocates enough space for at least `Requested_Capacity` elements. The
   --  new capacity may be more than requested.
   procedure Reserve (Q : in out Queue; Requested_Capacity : Natural) with
     Post => Q.Capacity >= Requested_Capacity;

   --  Releases allocated memory and returns the queue to an empty state.
   procedure Release (Q : in out Queue) with
     Post => Q.Length = 0 and Q.Capacity = 0;

   --  Returns the queue to an empty state but retains allocated memory.
   procedure Clear (Q : in out Queue) with
     Post => Q.Length = 0 and Q.Capacity = Q.Capacity'Old;

private

   pragma Inline (Length);
   pragma Inline (Capacity);
   pragma Inline (Peek);

   type Element_Array is array (Natural range <>) of Element_Type;

   type Element_Array_Type (Last : Natural) is record
      Xs : Element_Array (1 .. Last);
   end record;

   type Element_Array_Ptr is access Element_Array_Type;

   type Queue is new Ada.Finalization.Limited_Controlled with record
      Elements : Element_Array_Ptr := null;
      Capacity : Natural           := 0;
      Length   : Natural           := 0;
   end record;

   overriding procedure Initialize (Q : in out Queue) is null;
   overriding procedure Finalize (Q : in out Queue) renames Release;

   function Parent (Index : Queue_Index) return Queue_Index is (Index / 2);
   pragma Inline (Parent);

   function Left (Index : Queue_Index) return Queue_Index is (2 * Index);
   pragma Inline (Left);

   function Right (Index : Queue_Index) return Queue_Index is (2 * Index + 1);
   pragma Inline (Right);

   function First (Q : Queue) return Queue_Index is (1);
   pragma Inline (First);

   function Last (Q : Queue) return Queue_Index is (Q.Length);
   pragma Inline (Last);

   procedure Sift_Up (Q : in out Queue; Where : Queue_Index);

   procedure Sift_Down (Q : in out Queue; Where : Queue_Index);

   procedure Swap (A, B : in out Element_Type);
   pragma Inline (Swap);

   procedure Swap (Q : in out Queue; I, J : Queue_Index);
   pragma Inline (Swap);

end Advent.Containers.Priority_Queues;
