with Advent.Containers.Linked_Deques;
with Advent.Testing; use Advent.Testing;

procedure Test_Linked_Deques is

   type Block_Index is mod 2;

   package Int_Deques is new Advent.Containers.Linked_Deques
     (Element_Type => Integer, Block_Index => Block_Index);

   Test_Q : Int_Deques.Deque;
   X      : Integer;

begin

   --  Test around empty container (this creates a block to the left, and
   --  popping frees the original block)
   Assert (Test_Q.Empty);
   Test_Q.Push_Front (123);
   Assert (Test_Q.Length = 1);
   Assert (Test_Q.Front = 123);
   Assert (Test_Q.Back = 123);
   Test_Q.Pop_Back (X);
   Assert (X = 123);
   Assert (Test_Q.Empty);

   declare
      Test_Ints : constant array (Positive range <>) of Integer := (1, 2, 3, 4, 5, 6, 7);
   begin
      --  Pushing/popping the ints in FILO
      for I in Test_Ints'Range loop
         Test_Q.Push_Back (Test_Ints (I));
         Assert (Test_Q.Length = I);
      end loop;
      Assert (Test_Q.Length = Test_Ints'Length);
      Assert (Test_Q.Front = Test_Ints (Test_Ints'First));
      Assert (Test_Q.Back = Test_Ints (Test_Ints'Last));
      for I in reverse Test_Ints'Range loop
         Test_Q.Pop_Back (X);
         Assert (Test_Ints (I) = X);
      end loop;
      Assert (Test_Q.Empty);

      --  Try the same thing but on the other side of the deque
      for I in Test_Ints'Range loop
         Test_Q.Push_Front (Test_Ints (I));
         Assert (Test_Q.Length = I);
      end loop;
      Assert (Test_Q.Length = Test_Ints'Length);
      Assert (Test_Q.Front = Test_Ints (Test_Ints'Last));
      Assert (Test_Q.Back = Test_Ints (Test_Ints'First));
      for I in reverse Test_Ints'Range loop
         Test_Q.Pop_Front (X);
         Assert (Test_Ints (I) = X);
      end loop;
      Assert (Test_Q.Empty);

      --  Pushing/popping in FIFO
      for I in Test_Ints'Range loop
         Test_Q.Push_Back (Test_Ints (I));
         Assert (Test_Q.Length = I);
      end loop;
      Assert (Test_Q.Length = Test_Ints'Length);
      Assert (Test_Q.Front = Test_Ints (Test_Ints'First));
      Assert (Test_Q.Back = Test_Ints (Test_Ints'Last));
      for I in Test_Ints'Range loop
         Test_Q.Pop_Front (X);
         Assert (Test_Ints (I) = X);
      end loop;
      Assert (Test_Q.Empty);

      --  Same; other end
      for I in Test_Ints'Range loop
         Test_Q.Push_Front (Test_Ints (I));
         Assert (Test_Q.Length = I);
      end loop;
      Assert (Test_Q.Length = Test_Ints'Length);
      Assert (Test_Q.Front = Test_Ints (Test_Ints'Last));
      Assert (Test_Q.Back = Test_Ints (Test_Ints'First));
      for I in Test_Ints'Range loop
         Test_Q.Pop_Back (X);
         Assert (Test_Ints (I) = X);
      end loop;
      Assert (Test_Q.Empty);

      --  Grow fast on one end, shrink on the other; this should force block re-use
      for I in Test_Ints'Range loop
         Test_Q.Push_Front (Test_Ints (I));
         Test_Q.Push_Front (Test_Ints (I));
         Test_Q.Push_Front (Test_Ints (I));
         Test_Q.Push_Front (Test_Ints (I));
         Test_Q.Pop_Back (X);
         Test_Q.Pop_Back (X);
         Assert (Test_Q.Length = 2 * I);
      end loop;
      --  Add 1 to Test_Ints'Length because the queue is allowed to waste at
      --  most one block.
      Assert (Test_Q.Capacity <= 2 * (Test_Ints'Length + 1));
   end;

end Test_Linked_Deques;
