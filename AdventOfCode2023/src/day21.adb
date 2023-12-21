with Advent;                     use Advent;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Day21 is

   function Element (M : Char_Matrix; Index : Vec2) return Character is
   begin
      return M (Index (0), Index (1));
   end Element;

   function Element_Wrapping (M : Char_Matrix; Index : Vec2) return Character is
   begin
      return M (1 + ((Index (0) - 1) mod Rows (M)), 1 + ((Index (1) - 1) mod Cols (M)));
   end Element_Wrapping;

   function In_Bounds (M : Char_Matrix; Index : Vec2) return Boolean is
   begin
      return Index (0) in 1 .. Rows (M) and then Index (1) in 1 .. Cols (M);
   end In_Bounds;

   function Element_Count (M : Char_Matrix) return Integer is (Rows (M) * Cols (M));

   function Map_1D (M : Char_Matrix; Index : Vec2) return Integer is
   begin
      return 1 + (Index (0) - 1) * Cols (M) + (Index (1) - 1);
   end Map_1D;

   function Is_Plot (C : Character) return Boolean is (C = '.' or else C = 'S');

   function Is_Start (C : Character) return Boolean is (C = 'S');

   No_Start : exception;

   function Find_Start (M : Char_Matrix) return Vec2 is
   begin
      for I in 1 .. Rows (M) loop
         for J in 1 .. Cols (M) loop
            if Is_Start (M (I, J)) then
               return (I, J);
            end if;
         end loop;
      end loop;
      raise No_Start;
   end Find_Start;

   --  If we revisit a position with the same number of steps, there's no point
   --  re-evaluating it or its neighbors since all paths from that step will
   --  have already been visited
   type Position_State is record
      Position   : Vec2;
      Steps_Left : Integer;
   end record;

   function Hash (S : Position_State) return Hash_Type is
     (Hash (S.Position) xor (Hash_Type (S.Steps_Left) * 2_147_483_647));

   package Position_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Position_State);

   package Score_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Position_State, Element_Type => Boolean, Hash => Hash, Equivalent_Keys => "=");

   function Count_Reachable (M : Char_Matrix; Total_Steps : Integer) return Integer is
      Start : Vec2 := Find_Start (M);

      Explore : Position_Vectors.Vector;
      Current : Position_State;
      Seen    : Score_Maps.Map;

      Result : Integer := 0;
   begin
      Explore.Append ((Start, Total_Steps));

      while Explore.Length > 0 loop
         Current := Explore.Last_Element;
         Explore.Delete_Last;

         if not Seen.Contains (Current) then
            Seen.Insert (Current, True);

            if Current.Steps_Left = 0 then
               Result := Result + 1;
            else
               for D in Direction loop
                  declare
                     Neighbor_Pos : Vec2 := Current.Position + To_Vector (D);
                  begin
                     if In_Bounds (M, Neighbor_Pos) and then Is_Plot (Element (M, Neighbor_Pos))
                     then
                        Explore.Append ((Neighbor_Pos, Current.Steps_Left - 1));
                     end if;
                  end;
               end loop;
            end if;
         end if;

      end loop;

      return Result;

   end Count_Reachable;

   --  locals
   Chars : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));

begin

   Solution (Count_Reachable (Chars, 64));

end Day21;
