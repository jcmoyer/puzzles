with Advent;                     use Advent;
with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Day21 is

   --  Part 1 is nothing special, it's solved with a normal DFS/BFS.
   --
   --  For part 2 the input has no obstacles along the start row or column, so
   --  the elf can walk north/south/east/west in a straight line forever. I
   --  assume this is intentional, because otherwise it would be difficult to
   --  establish an upper bound on steps taken in one direction since you would
   --  have to account for the elf moving around obstacles. Note that the
   --  example DOES NOT HAVE THIS PROPERTY! I spent a long time thinking about
   --  how you would solve this generally before checking the input, which was
   --  a huge mistake.
   --
   --  The puzzle input is 131x131 (presumably the same for all inputs) with
   --  the start in the center at <66,66> (1-based indexing) so the elf must
   --  move 65 steps to reach the end of the current grid, moves into an
   --  adjacent grid at >65 steps, and moves into another grid at >65+131
   --  steps, and so on.
   --
   --  Part 2 can be solved with by first finding the number of reachable plots
   --  on an infinite grid at steps 65, 65 + 131, 65 + 2*131. With these values
   --  we can use the Lagrange interpolating polynomial to find out how many
   --  steps are reachable at 26_501_365 via interpolation. This solution seems
   --  somewhat messy because it involves truncating real values, but it does
   --  work.

   function Element (M : Char_Matrix; Index : Vec2) return Character is
   begin
      return M (Index (X), Index (Y));
   end Element;

   function Element_Wrapping (M : Char_Matrix; Index : Vec2) return Character is
   begin
      --  First we go to zero-based indices, perform the modulus, then go back
      --  to one-based indices.
      return M (1 + ((Index (X) - 1) mod Rows (M)), 1 + ((Index (Y) - 1) mod Cols (M)));
   end Element_Wrapping;

   function In_Bounds (M : Char_Matrix; Index : Vec2) return Boolean is
   begin
      return Index (X) in 1 .. Rows (M) and then Index (Y) in 1 .. Cols (M);
   end In_Bounds;

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

   package Seen_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Position_State, Hash => Hash, Equivalent_Elements => "=");

   generic
      with function Valid_Neighbor (M : Char_Matrix; Index : Vec2) return Boolean;
   function Count_Reachable (M : Char_Matrix; Total_Steps : Integer) return Long_Long_Integer;

   function Count_Reachable (M : Char_Matrix; Total_Steps : Integer) return Long_Long_Integer is
      Start : Vec2 := Find_Start (M);

      Explore : Position_Vectors.Vector;
      Current : Position_State;
      Seen    : Seen_Sets.Set;

      Result : Long_Long_Integer := 0;
   begin
      Explore.Append (Position_State'(Start, Total_Steps));

      while Explore.Length > 0 loop
         Current := Explore.Last_Element;
         Explore.Delete_Last;

         if not Seen.Contains (Current) then
            Seen.Insert (Current);
            if Current.Steps_Left = 0 then
               Result := Result + 1;
            else
               for D in Cardinal_Direction loop
                  declare
                     Neighbor_Pos : constant Vec2 := Current.Position + To_Vector (D);
                  begin
                     if Valid_Neighbor (M, Neighbor_Pos) then
                        Explore.Append (Position_State'(Neighbor_Pos, Current.Steps_Left - 1));
                     end if;
                  end;
               end loop;
            end if;
         end if;
      end loop;

      return Result;
   end Count_Reachable;

   function Is_Walkable_Normal (M : Char_Matrix; Index : Vec2) return Boolean is
     (In_Bounds (M, Index) and then Is_Plot (Element (M, Index)));

   function Count_Reachable_Normal is new Count_Reachable (Valid_Neighbor => Is_Walkable_Normal);

   function Is_Walkable_Wrapping (M : Char_Matrix; Index : Vec2) return Boolean is
     (Is_Plot (Element_Wrapping (M, Index)));

   function Count_Reachable_Wrapping is new Count_Reachable
     (Valid_Neighbor => Is_Walkable_Wrapping);

   --  Reference: https://mathworld.wolfram.com/LagrangeInterpolatingPolynomial.html
   type Long_Array is array (Positive range <>) of Long_Long_Integer;

   function Lagrange (X : Long_Long_Integer; Xn, Yn : Long_Array) return Long_Long_Integer with
     Pre => Xn'Length = Yn'Length and then Xn'First = 1 and then Yn'First = 1
   is
      N      : constant Integer  := Xn'Length;
      Result : Long_Long_Integer := 0;
   begin
      for J in 1 .. N loop
         declare
            Product : Long_Long_Integer := Yn (J);
         begin
            for K in 1 .. N loop
               if K /= J then
                  Product := Product * ((X - Xn (K)) / (Xn (J) - Xn (K)));
               end if;
            end loop;
            Result := Result + Product;
         end;
      end loop;
      return Result;
   end Lagrange;

   --  locals
   Map : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));

begin

   Solution (Count_Reachable_Normal (Map, 64));

   declare
      Steps : constant                   := 26_501_365;
      Size  : constant Integer           := Rows (Map);
      Q     : constant Integer           := Steps / Size;
      R     : constant Integer           := Steps mod Size;
      A     : constant Long_Long_Integer := Count_Reachable_Wrapping (Map, R);
      B     : constant Long_Long_Integer := Count_Reachable_Wrapping (Map, R + Size);
      C     : constant Long_Long_Integer := Count_Reachable_Wrapping (Map, R + 2 * Size);
   begin
      Solution (Lagrange (X => Long_Long_Integer (Q), Xn => (0, 1, 2), Yn => (A, B, C)));
   end;

end Day21;
