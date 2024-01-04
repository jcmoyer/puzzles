with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

procedure Day13 is
   package Matrix_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Char_Matrix);

   subtype Matrix_Vector is Matrix_Vectors.Vector;

   function Load_Tilemaps (Lines : String_Array) return Matrix_Vector is
      Result  : Matrix_Vector;
      M_First : Integer := -1;
      M_Last  : Integer := -1;

      procedure Load_Range is
         Row_Count : constant Integer := M_Last - M_First + 1;
         Col_Count : constant Integer := Lines.Element (M_First)'Length;
         M         : Char_Matrix (1 .. Row_Count, 1 .. Col_Count);
      begin
         for I in M'First (1) .. M'Last (1) loop
            for J in M'First (2) .. M'Last (2) loop
               M (I, J) := Lines (M_First + I - M'First (1)) (1 + J - M'First (2));
            end loop;
         end loop;
         Result.Append (M);
      end Load_Range;

   begin
      for I in Lines.First_Index .. Lines.Last_Index loop
         if Lines.Element (I)'Length > 0 then
            --  character data
            if M_First = -1 then
               M_First := I;
            end if;
         else
            --  end of one tilemap
            M_Last := I - 1;
            Load_Range;
            M_First := -1;
            M_Last  := -1;
         end if;
      end loop;
      M_Last := Lines.Last_Index;
      Load_Range;
      return Result;
   end Load_Tilemaps;

   function Equal_Rows (M : Char_Matrix; I, J : Integer) return Boolean is
   begin
      for C in 1 .. Cols (M) loop
         if M (I, C) /= M (J, C) then
            return False;
         end if;
      end loop;
      return True;
   end Equal_Rows;

   No_Reflection : exception;

   type Reflection_Kind is (None, Horizontal, Vertical);

   type Reflection is record
      Kind   : Reflection_Kind;
      Origin : Integer;
   end record;

   package Reflection_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Reflection);

   subtype Reflection_Vector is Reflection_Vectors.Vector;

   function Count_Left (M : Char_Matrix; Col_J : Integer) return Integer is
   begin
      return Integer'Max (0, Col_J - M'First (2) + 1);
   end Count_Left;

   function Count_Above (M : Char_Matrix; Row_I : Integer) return Integer is
   begin
      return Integer'Max (0, Row_I - M'First (1) + 1);
   end Count_Above;

   function Score (M : Char_Matrix; R : Reflection) return Integer is
   begin
      if R.Kind = Horizontal then
         return 100 * Count_Above (M, R.Origin);
      elsif R.Kind = Vertical then
         return Count_Left (M, R.Origin);
      else
         raise No_Reflection;
      end if;
   end Score;

   function Find_Horizontal_Reflection (M : Char_Matrix; From_Row : Integer := 1) return Reflection
   is
      function Check_Reflection (Row_I, Row_J : Integer) return Boolean is
         I : Integer := Row_I;
         J : Integer := Row_J;
      begin
         while I in M'Range (1) and then J in M'Range (1) loop
            if not Equal_Rows (M, I, J) then
               return False;
            end if;
            I := I - 1;
            J := J + 1;
         end loop;
         return True;
      end Check_Reflection;
   begin
      for I in From_Row .. Rows (M) - 1 loop
         if Check_Reflection (I, I + 1) then
            return Reflection'(Kind => Horizontal, Origin => I);
         end if;
      end loop;
      return Reflection'(Kind => None, Origin => 0);
   end Find_Horizontal_Reflection;

   function Find_Vertical_Reflection
     (M : Char_Matrix; From_Column : Integer := 1) return Reflection
   is
      N : constant Char_Matrix := Transpose (M);
      R : Reflection           :=
        Find_Horizontal_Reflection (N, From_Row => M'First (1) + From_Column - M'First (2));
   begin
      if R.Kind /= None then
         R.Kind   := Vertical;
         R.Origin := M'First (2) + R.Origin - M'First (1);
      end if;
      return R;
   end Find_Vertical_Reflection;

   function Find_Reflection (M : Char_Matrix) return Reflection_Vector is
      Results : Reflection_Vector;
      R       : Reflection;
   begin
      R := Find_Horizontal_Reflection (M);
      while R.Kind = Horizontal loop
         Results.Append (R);
         R := Find_Horizontal_Reflection (M, From_Row => R.Origin + 1);
      end loop;
      R := Find_Vertical_Reflection (M);
      while R.Kind = Vertical loop
         Results.Append (R);
         R := Find_Vertical_Reflection (M, From_Column => R.Origin + 1);
      end loop;
      return Results;
   end Find_Reflection;

   Invalid_Character : exception;

   function Find_Desmudged_Reflection (M : Char_Matrix) return Reflection is
      Smudged_Refl : constant Reflection := Find_Reflection (M).First_Element;
      Buffer       : Char_Matrix         := M;

      function Desmudged (C : Character) return Character is
      begin
         case C is
            when '#' =>
               return '.';
            when '.' =>
               return '#';
            when others =>
               raise Invalid_Character;
         end case;
      end Desmudged;
   begin
      for I in 1 .. Rows (M) loop
         for J in 1 .. Cols (M) loop
            Buffer        := M;
            Buffer (I, J) := Desmudged (M (I, J));

            declare
               Desmudged_Refls : constant Reflection_Vector := Find_Reflection (Buffer);
            begin
               for R of Desmudged_Refls loop
                  if R.Kind = Smudged_Refl.Kind and then R.Origin /= Smudged_Refl.Origin then
                     return R;
                  end if;
                  if R.Kind /= Smudged_Refl.Kind then
                     return R;
                  end if;
               end loop;
            end;
         end loop;
      end loop;

      raise No_Reflection;
   end Find_Desmudged_Reflection;

   --  locals
   Lines : constant String_Array  := Read_All_Lines (Ada.Command_Line.Argument (1));
   Maps  : constant Matrix_Vector := Load_Tilemaps (Lines);
   R     : Reflection;

   Count_1 : Integer := 0;
   Count_2 : Integer := 0;

begin

   for M of Maps loop
      R       := Find_Reflection (M).First_Element;
      Count_1 := Count_1 + Score (M, R);

      R       := Find_Desmudged_Reflection (M);
      Count_2 := Count_2 + Score (M, R);
   end loop;

   Solution (Count_1);
   Solution (Count_2);

end Day13;
