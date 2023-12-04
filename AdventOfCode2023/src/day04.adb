with Advent;         use Advent;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Containers; use Ada.Containers;

procedure Day04 is
   function Hash (Val : Integer) return Hash_Type is (Hash_Type (Val));
   function Equivalent_Elements (X, Y : Integer) return Boolean is (X = Y);

   package Integer_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Integer,
      Hash                => Hash,
      Equivalent_Elements => Equivalent_Elements);
   subtype Integer_Set is Integer_Sets.Set;

   package Integer_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Integer,
      Element_Type    => Integer,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Elements);
   subtype Integer_Map is Integer_Maps.Map;

   --  Bring "=" into scope
   use type Integer_Maps.Cursor;

   function Count_Quantities (M : Integer_Map) return Integer is
      Sum : Integer := 0;
   begin
      for V of M loop
         Sum := Sum + V;
      end loop;
      return Sum;
   end Count_Quantities;

   Lines : constant String_Array := Read_All_Lines ("test/2023-04-input.txt");

   Score           : Integer := 0;
   Card_Quantities : Integer_Map;
begin
   for Line of Lines loop
      declare
         Card_Values : constant String_Array := Split (Line, ": ");
         Card_Info   : constant String_Array :=
           Split (Card_Values (0), " ", Keep_Empty => False);
         Card_Number : constant Integer      := Integer'Value (Card_Info (1));
         Values      : constant String_Array := Split (Card_Values (1), " | ");

         Winning_Strs : constant String_Array :=
           Split (Values (0), " ", Keep_Empty => False);
         Actual_Strs  : constant String_Array :=
           Split (Values (1), " ", Keep_Empty => False);

         Winning_Set : Integer_Set;
         Actual_Set  : Integer_Set;

         This_Quantity : Integer_Maps.Cursor :=
           Card_Quantities.Find (Card_Number);
         Did_Insert    : Boolean             := False;
      begin
         if This_Quantity = Integer_Maps.No_Element then
            Card_Quantities.Insert
              (Card_Number,
               1,
               Position => This_Quantity,
               Inserted => Did_Insert);
         else
            Card_Quantities.Replace_Element
              (This_Quantity, Integer_Maps.Element (This_Quantity) + 1);
         end if;

         for S of Winning_Strs loop
            Winning_Set.Insert (Integer'Value (S));
         end loop;
         for S of Actual_Strs loop
            Actual_Set.Insert (Integer'Value (S));
         end loop;
         Winning_Set.Intersection (Actual_Set);

         if Winning_Set.Length > 0 then
            Score := Score + 2**Natural (Winning_Set.Length - 1);

            for I in 1 .. Winning_Set.Length loop
               declare
                  Ref_Number   : constant Integer := Card_Number + Integer (I);
                  Ref_Quantity : constant Integer_Maps.Cursor :=
                    Card_Quantities.Find (Ref_Number);
               begin
                  if Ref_Quantity = Integer_Maps.No_Element then
                     Card_Quantities.Insert
                       (Ref_Number, Integer_Maps.Element (This_Quantity));
                  else
                     Card_Quantities.Replace_Element
                       (Ref_Quantity,
                        Integer_Maps.Element (Ref_Quantity) +
                        Integer_Maps.Element (This_Quantity));
                  end if;
               end;
            end loop;
         end if;
      end;
   end loop;

   Solution (Score);
   Solution (Count_Quantities (Card_Quantities));
end Day04;
