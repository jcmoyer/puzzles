with Ada.Unchecked_Deallocation;

package body Advent.Containers.Priority_Queues is

   function Length (Q : Queue) return Natural is (Q.Length);

   function Capacity (Q : Queue) return Natural is (Q.Capacity);

   function Peek (Q : Queue) return Element_Type is (Q.Elements.Xs (Q.First));

   procedure Reserve (Q : in out Queue; Requested_Capacity : Natural) is
      procedure Free is new Ada.Unchecked_Deallocation (Element_Array_Type, Element_Array_Ptr);
   begin
      if Q.Capacity >= Requested_Capacity then
         return;
      end if;

      declare
         New_Capacity : constant Natural := Natural'Max (Requested_Capacity, 2 * Q.Capacity);
         New_Elements : constant Element_Array_Ptr := new Element_Array_Type (New_Capacity);
      begin
         if Q.Elements /= null then
            New_Elements.Xs (First (Q) .. Last (Q)) := Q.Elements.Xs (First (Q) .. Last (Q));
         end if;
         Free (Q.Elements);
         Q.Elements := New_Elements;
         Q.Capacity := New_Capacity;
      end;
   end Reserve;

   procedure Release (Q : in out Queue) is
      procedure Free is new Ada.Unchecked_Deallocation (Element_Array_Type, Element_Array_Ptr);
   begin
      if Q.Elements /= null then
         Free (Q.Elements);
      end if;
      Q.Capacity := 0;
      Q.Length   := 0;
   end Release;

   procedure Swap (A, B : in out Element_Type) is
      Temp : constant Element_Type := A;
   begin
      A := B;
      B := Temp;
   end Swap;

   procedure Swap (Q : in out Queue; I, J : Queue_Index) is
   begin
      Swap (Q.Elements.Xs (I), Q.Elements.Xs (J));
   end Swap;

   procedure Enqueue (Q : in out Queue; Value : Element_Type) is
   begin
      Reserve (Q, Q.Length + 1);
      Q.Length                 := Q.Length + 1;
      Q.Elements.Xs (Last (Q)) := Value;
      Sift_Up (Q, Last (Q));
   end Enqueue;

   procedure Dequeue (Q : in out Queue; Value : out Element_Type) is
   begin
      Value                     := Q.Elements.Xs (First (Q));
      Q.Elements.Xs (First (Q)) := Q.Elements.Xs (Last (Q));
      Q.Length                  := Q.Length - 1;
      Sift_Down (Q, First (Q));
   end Dequeue;

   procedure Sift_Up (Q : in out Queue; Where : Queue_Index) is
      Pos : Queue_Index := Where;
   begin
      while Pos > 1 and then Q.Elements.Xs (Pos) < Q.Elements.Xs (Parent (Pos)) loop
         Swap (Q, Pos, Parent (Pos));
         Pos := Parent (Pos);
      end loop;
   end Sift_Up;

   procedure Sift_Down (Q : in out Queue; Where : Queue_Index) is
      Pos : Queue_Index := Where;
   begin
      loop
         declare
            Left_Child  : constant Queue_Index := Left (Pos);
            Right_Child : constant Queue_Index := Right (Pos);
            Min         : Queue_Index          := Pos;
         begin
            if Left_Child < Q.Length and then (Q.Elements.Xs (Left_Child) < Q.Elements.Xs (Min))
            then
               Min := Left_Child;
            end if;

            if Right_Child < Q.Length and then (Q.Elements.Xs (Right_Child) < Q.Elements.Xs (Min))
            then
               Min := Right_Child;
            end if;

            if Pos /= Min then
               Swap (Q, Pos, Min);
               Pos := Min;
            else
               return;
            end if;
         end;
      end loop;
   end Sift_Down;
end Advent.Containers.Priority_Queues;
