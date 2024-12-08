with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;

procedure Day08 is

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);

   --  Positions of each antenna by character
   type Antenna_Table is array (Character) of Vec2_Vectors.Vector;

   --  True if a point (I,J) is an antinode
   type Antinode_Map is array (Integer range <>, Integer range <>) of Boolean;

   function Is_Antenna (C : Character) return Boolean is
     (C in 'a' .. 'z' or else C in 'A' .. 'Z' or else C in '0' .. '9');

   procedure Gather_Antennae (M : Char_Matrix; T : in out Antenna_Table) is
   begin
      for I in M'Range (1) loop
         for J in M'Range (2) loop
            if Is_Antenna (M (I, J)) then
               T (M (I, J)).Append (Vec2'(I, J));
            end if;
         end loop;
      end loop;
   end Gather_Antennae;

   function Count_Antinodes (M : Antinode_Map) return Natural is
      Count : Natural := 0;
   begin
      for I in M'Range (1) loop
         for J in M'Range (2) loop
            if M (I, J) then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return Count;
   end Count_Antinodes;

   procedure Get_Single_Antinodes
     (Map : Char_Matrix; Antennae : Antenna_Table; Result : out Antinode_Map)
   is
   begin
      for A of Antennae loop
         for I in A.First_Index .. A.Last_Index loop
            for J in I + 1 .. A.Last_Index loop
               declare
                  P0 : constant Vec2 := (A.Element (I) * 2) - A.Element (J);
                  P1 : constant Vec2 := (A.Element (J) * 2) - A.Element (I);
               begin
                  if In_Bounds (Map, P0) then
                     Result (P0 (X), P0 (Y)) := True;
                  end if;
                  if In_Bounds (Map, P1) then
                     Result (P1 (X), P1 (Y)) := True;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end Get_Single_Antinodes;

   --  There is a general form of this that can solve part 1 with the same code
   --  but it's annoying to write because it needs to have an upper bound on
   --  iterations that doesn't really apply in part 2, so you have to provide
   --  some bogus number.
   --
   --  Part 1: if I and J are vectors representing the location of two
   --  antennae, the antinodes will appear at I + I - J and J + J - I. This
   --  looks odd, because it's actually a special case of the more generalized
   --  form in part 2.
   --
   --  The actual formula for an antinode at any position away from I or J is
   --  D = I - J      (distance between positions)
   --  I + KD         (position of an antenna offset by some K multiple of D)
   --
   --  Part 1 is as if K = 1: I + 1(I - J) = I + I - J.
   --
   --  Part 2 uses the general form, and goes from K = 0 to infinity. So you
   --  place an antinode at:
   --
   --  I + 0D = I
   --  I + 1D = I + 1I - 1J
   --  I + 2D = I + 2I - 2J = 3I - 2J
   --  I + 3D = I + 3I - 3J = 4I - 4J
   --
   --  ...and so on until both I and J are out of bounds
   procedure Raycast_Antinodes
     (Map : Char_Matrix; Antennae : Antenna_Table; Result : out Antinode_Map)
   is
   begin
      for A of Antennae loop
         for I in A.First_Index .. A.Last_Index loop
            for J in I + 1 .. A.Last_Index loop
               declare
                  D : constant Vec2 := A.Element (I) - A.Element (J);
                  P : Vec2;
               begin
                  --  Raycast twice from one antenna, once in each direction
                  P := A.Element (I);
                  while In_Bounds (Map, P) loop
                     Result (P (X), P (Y)) := True;
                     P                     := P + D;
                  end loop;

                  P := A.Element (I);
                  while In_Bounds (Map, P) loop
                     Result (P (X), P (Y)) := True;
                     P                     := P - D;
                  end loop;
               end;
            end loop;
         end loop;
      end loop;
   end Raycast_Antinodes;

   Map       : constant Char_Matrix := Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));
   Antennae  : Antenna_Table := (others => Vec2_Vectors.Empty_Vector);
   Antinodes : Antinode_Map (Map'Range (1), Map'Range (2)) := (others => (others => False));

begin
   Gather_Antennae (Map, Antennae);
   Get_Single_Antinodes (Map, Antennae, Antinodes);
   Solution (Count_Antinodes (Antinodes));
   Raycast_Antinodes (Map, Antennae, Antinodes);
   Solution (Count_Antinodes (Antinodes));
end Day08;
