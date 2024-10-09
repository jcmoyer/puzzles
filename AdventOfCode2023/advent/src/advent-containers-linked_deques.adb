with Ada.Unchecked_Deallocation;

package body Advent.Containers.Linked_Deques is

   function Length (D : Deque) return Natural is (D.Length);

   function Capacity (D : Deque) return Natural is
      function List_Length (Start : Block_Ptr) return Natural is
         Ptr : Block_Ptr := Start;
      begin
         if Ptr = null then
            return 0;
         end if;

         if Ptr.Prev = null and Ptr.Next = null then
            return 0;
         end if;

         --  This shouldn't be an interior node.
         pragma Assert (Ptr.Prev /= null xor Ptr.Next /= null);

         declare
            Result : Natural := 1;
         begin
            if Ptr.Prev /= null then
               while Ptr /= null loop
                  Result := Result + 1;
                  Ptr    := Ptr.Prev;
               end loop;
               return Result;
            elsif Ptr.Next /= null then
               while Ptr /= null loop
                  Result := Result + 1;
                  Ptr    := Ptr.Next;
               end loop;
               return Result;
            else
               pragma Assert (False);
            end if;
         end;
      end List_Length;
   begin
      --  Maybe we should just store this in the structure, but it's really
      --  only useful as debug info or a performance metric.
      return (1 + Natural (Block_Index'Last)) * (List_Length (D.Left) + List_Length (D.Free_List));
   end Capacity;

   function Empty (D : Deque) return Boolean is (D.Length = 0);

   function Front (D : Deque) return Element_Type is (D.Left.Xs (D.Left_Index));

   function Back (D : Deque) return Element_Type is (D.Right.Xs (D.Right_Index));

   procedure Move_To_Free_List (D : in out Deque; B : Block_Ptr) is
      Next_Free : constant Block_Ptr := D.Free_List;
   begin
      --  Unlink any adjacent nodes
      --  There should only be one since this only happens for outermost nodes
      pragma Assert (B.Prev /= null xor B.Next /= null);
      if B.Next /= null then
         B.Next.Prev := null;
      end if;
      if B.Prev /= null then
         B.Prev.Next := null;
      end if;
      --  Link this node onto the free list
      B.Next      := (if Next_Free /= null then Next_Free else null);
      B.Prev      := null;
      D.Free_List := B;
   end Move_To_Free_List;

   procedure Free_Block (D : in out Deque; B : Block_Ptr) is
   begin
      Move_To_Free_List (D, B);
   end Free_Block;

   function New_Block (D : in out Deque) return Block_Ptr is
   begin
      if D.Free_List = null then
         return new Block'(Xs => <>, Next => null, Prev => null);
      else
         declare
            B : constant Block_Ptr := D.Free_List;
         begin
            D.Free_List := B.Next;
            B.Prev      := null;
            B.Next      := null;
            return B;
         end;
      end if;
   end New_Block;

   overriding procedure Initialize (D : in out Deque) is
   begin
      D.Left        := New_Block (D);
      D.Right       := D.Left;
      D.Left_Index  := 1;
      D.Right_Index := 0;
   end Initialize;

   overriding procedure Finalize (D : in out Deque) is
      procedure Free is new Ada.Unchecked_Deallocation (Block, Block_Ptr);

      procedure Free_List_Elements (First : in out Block_Ptr) is
         P : Block_Ptr := First;
      begin
         while P /= null loop
            declare
               Next : constant Block_Ptr := P.Next;
            begin
               Free (P);
               P := Next;
            end;
         end loop;
         First := null;
      end Free_List_Elements;
   begin
      Free_List_Elements (D.Free_List);
      Free_List_Elements (D.Left);

      pragma Assert (D.Capacity = 0);
   end Finalize;

   procedure Extend_Left (D : in out Deque; B : Block_Ptr) is
   begin
      pragma Assert (B.Prev = null);
      B.Prev      := New_Block (D);
      B.Prev.Next := B;
   end Extend_Left;

   procedure Extend_Right (D : in out Deque; B : Block_Ptr) is
   begin
      pragma Assert (B.Next = null);
      B.Next      := New_Block (D);
      B.Next.Prev := B;
   end Extend_Right;

   procedure Push_Front (D : in out Deque; Value : Element_Type) is
   begin
      D.Left_Index := D.Left_Index - 1;
      if D.Left_Index = Block_Index'Last then
         if D.Left.Prev = D.Right then
            Free_Block (D, D.Left);
            D.Left := D.Right;
         else
            Extend_Left (D, D.Left);
            D.Left := D.Left.Prev;
         end if;
      end if;
      D.Left.Xs (D.Left_Index) := Value;
      D.Length                 := D.Length + 1;
   end Push_Front;

   procedure Pop_Front (D : in out Deque; Value : out Element_Type) is
   begin
      Value        := D.Left.Xs (D.Left_Index);
      D.Left_Index := D.Left_Index + 1;
      if D.Left_Index = Block_Index'First then
         if D.Left = D.Right then
            Extend_Right (D, D.Left);
            D.Left := D.Left.Next;
         else
            declare
               Old_Left : Block_Ptr := D.Left;
            begin
               D.Left := D.Left.Next;
               Free_Block (D, Old_Left);
            end;
         end if;
      end if;
      D.Length := D.Length - 1;
   end Pop_Front;

   procedure Push_Back (D : in out Deque; Value : Element_Type) is
   begin
      D.Right_Index := D.Right_Index + 1;
      if D.Right_Index = Block_Index'First then
         if D.Right.Next = D.Left then
            Free_Block (D, D.Right);
            D.Right := D.Left;
         else
            Extend_Right (D, D.Right);
            D.Right := D.Right.Next;
         end if;
      end if;
      D.Right.Xs (D.Right_Index) := Value;
      D.Length                   := D.Length + 1;
   end Push_Back;

   procedure Pop_Back (D : in out Deque; Value : out Element_Type) is
   begin
      Value         := D.Right.Xs (D.Right_Index);
      D.Right_Index := D.Right_Index - 1;
      if D.Right_Index = Block_Index'Last then
         if D.Left = D.Right then
            Extend_Left (D, D.Right);
            D.Right := D.Right.Prev;
         else
            declare
               Old_Right : Block_Ptr := D.Right;
            begin
               D.Right := D.Right.Prev;
               Free_Block (D, Old_Right);
            end;
         end if;
      end if;
      D.Length := D.Length - 1;
   end Pop_Back;

   procedure Clear (D : in out Deque) is
   begin
      --  Retain all storage, but keep one live node.
      while D.Left /= D.Right loop
         Move_To_Free_List (D, D.Left);
      end loop;
      D.Left_Index  := 1;
      D.Right_Index := 0;
   end Clear;

end Advent.Containers.Linked_Deques;
