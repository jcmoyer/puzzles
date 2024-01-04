with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;    use Ada.Text_IO;

procedure Day22 is

   --  Input observations:
   --
   --  min coordinate is 0, 0, 1
   --  max coordinate is 9, 9, 309
   --  1232 blocks
   --
   --  The entire space is 10x10x309 = 30900 cells, which seems like a really
   --  small thing to simulate by AoC standards.

   type Axis is (X, Y, Z);

   type Vec3 is array (Axis) of Integer;

   --  Credit: Optimized Spatial Hashing for Collision Detection of Deformable Objects
   --  Full citation in src/advent/advent-vector_math.ads
   function Hash (A : Vec3) return Hash_Type is
     ((Ada.Containers.Hash_Type'Mod (A (X)) * 73_856_093) xor
      (Ada.Containers.Hash_Type'Mod (A (Y)) * 19_349_663) xor
      (Ada.Containers.Hash_Type'Mod (A (Z)) * 83_492_791));

   type AABB is record
      Min, Max : Vec3;
   end record;

   --  Turns out this wasn't necessary but [TODO] move it into utils.
   function Hash (Box : AABB) return Hash_Type is (Hash (Box.Min) xor Hash (Box.Max));

   --  Returns the size of the AABB along one axis. Turns out we didn't need
   --  this but I'll keep it and [TODO] move it into utils.
   function Size (Box : AABB; Along : Axis) return Integer is
     (Box.Max (Along) - Box.Min (Along) + 1);

   --  Drops the AABB one unit on the Z axis.
   function Drop (Box : AABB) return AABB is
      Result : AABB := Box;
   begin
      Result.Min (Z) := Result.Min (Z) - 1;
      Result.Max (Z) := Result.Max (Z) - 1;
      return Result;
   end Drop;

   --  Returns true if a single axis `Ax` in the AABB contains `Pos`.
   function Contains (Box : AABB; Ax : Axis; Pos : Integer) return Boolean is
     (Box.Min (Ax) <= Pos and then Pos <= Box.Max (Ax));

   function Is_Adjacent_Z (Top, Bottom : AABB) return Boolean is
   begin
      --  not even worth considering if the Z levels aren't aligned
      if Bottom.Max (Z) + 1 /= Top.Min (Z) then
         return False;
      end if;

      --  There must be an overlap on both X and Y axes
      --!pp off
      return (Contains (Top, X, Bottom.Min (X)) or else
              Contains (Top, X, Bottom.Max (X)) or else
              Contains (Bottom, X, Top.Min (X)) or else
              Contains (Bottom, X, Top.Max (X))) and then
             (Contains (Top, Y, Bottom.Min (Y)) or else
              Contains (Top, Y, Bottom.Max (Y)) or else
              Contains (Bottom, Y, Top.Min (Y)) or else
              Contains (Bottom, Y, Top.Max (Y)));
      --!pp on
   end Is_Adjacent_Z;

   --  AABB to string
   function Image (Box : AABB) return String is
     (Box.Min (X)'Image & "," & Box.Min (Y)'Image & "," & Box.Min (Z)'Image & "~" &
      Box.Max (X)'Image & "," & Box.Max (Y)'Image & "," & Box.Max (Z)'Image);

   --  Parses a string of the form "0,1,6~2,1,6"
   function Parse_AABB (S : String) return AABB is
      Left_Right   : constant String_Array := Split (S, "~");
      Left_Coords  : constant String_Array := Split (Left_Right (0), ",");
      Right_Coords : constant String_Array := Split (Left_Right (1), ",");
      Result       : AABB;
   begin
      Result.Min :=
        (Integer'Value (Left_Coords (0)),
         Integer'Value (Left_Coords (1)),
         Integer'Value (Left_Coords (2)));
      Result.Max :=
        (Integer'Value (Right_Coords (0)),
         Integer'Value (Right_Coords (1)),
         Integer'Value (Right_Coords (2)));
      return Result;
   end Parse_AABB;

   --  Given two AABBs, returns the AABB that contains both of them.
   --  Commutative and associative.
   function Union (Left, Right : AABB) return AABB is
      Result : AABB;
   begin
      for A in Axis loop
         Result.Min (A) := Integer'Min (Left.Min (A), Right.Min (A));
         Result.Max (A) := Integer'Max (Left.Max (A), Right.Max (A));
      end loop;
      return Result;
   end Union;

   --  Brick ids are mostly used for debugging, they're only a unique
   --  identifier and aren't meant to be valid indices.
   type Brick_Id is new Positive;

   type Brick_Index is new Positive;

   function Hash (P : Brick_Index) return Hash_Type is (Hash_Type (P));

   package Brick_Index_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Brick_Index, Hash => Hash, Equivalent_Elements => "=");

   type Brick is record
      Id           : Brick_Id;
      Box          : AABB;
      Below, Above : Brick_Index_Sets.Set;
   end record;

   package Brick_Vectors is new Ada.Containers.Vectors
     (Index_Type => Brick_Index, Element_Type => Brick);

   --  Sorting for bricks along the Z axis so we can drop them from lowest to
   --  highest.
   function Brick_Z_Less_Than (Left, Right : Brick) return Boolean is
     (Left.Box.Min (Z) < Right.Box.Min (Z));

   package Brick_Z_Sorting is new Brick_Vectors.Generic_Sorting ("<" => Brick_Z_Less_Than);

   type World is record
      Bricks : Brick_Vectors.Vector;
   end record;

   function Copy (W : World) return World is
   begin
      --  I think this will shallow copy the sets but who cares we don't need
      --  them for part 2. Might remove them after clean up anyways.
      return World'(Bricks => W.Bricks.Copy);
   end Copy;

   --  Adds a brick to the world given an AABB from the input. Each brick has a
   --  unique ID assigned to it.
   procedure Add_Brick (W : in out World; Box : AABB) is
      New_Id : constant Brick_Id := Brick_Id (W.Bricks.Last_Index + 1);
   begin
      W.Bricks.Append
        (Brick'
           (Id    => New_Id,
            Box   => Box,
            Below => Brick_Index_Sets.Empty_Set,
            Above => Brick_Index_Sets.Empty_Set));
   end Add_Brick;

   --  Finds the extents of the world, mostly useful for debugging
   function Extents (W : World) return AABB is
      Bound : AABB := W.Bricks.First_Element.Box;
   begin
      for I in W.Bricks.First_Index + 1 .. W.Bricks.Last_Index loop
         Bound := Union (Bound, W.Bricks (I).Box);
      end loop;
      return Bound;
   end Extents;

   --  Settles all bricks and returns how many moved.
   function Settle (W : in out World) return Integer is
      Moved : Integer := 0;

      function Drop_Brick (I : Brick_Index) return Boolean is
         Did_Move : Boolean := False;
      begin
         Outer :
         loop
            exit Outer when W.Bricks (I).Box.Min (Z) = 1;

            for J in W.Bricks.First_Index .. I - 1 loop
               if Is_Adjacent_Z (Top => W.Bricks (I).Box, Bottom => W.Bricks (J).Box) then
                  exit Outer;
               end if;
            end loop;

            W.Bricks (I).Box := Drop (W.Bricks (I).Box);
            Did_Move         := True;
         end loop Outer;

         return Did_Move;
      end Drop_Brick;

   begin
      Brick_Z_Sorting.Sort (W.Bricks);

      for I in W.Bricks.First_Index .. W.Bricks.Last_Index loop
         if W.Bricks (I).Box.Min (Z) = 1 then
            -- do nothing, already on floor
            null;
         else
            if Drop_Brick (I) then
               Moved := Moved + 1;
            end if;
         end if;
      end loop;

      --  Now examine each brick and figure out which bricks are adjacent.
      --  Maybe we can do this at the same time as the above loop but I haven't
      --  thought about it yet.
      for I in W.Bricks.First_Index .. W.Bricks.Last_Index loop
         for J in W.Bricks.First_Index .. W.Bricks.Last_Index loop
            if I /= J then
               if Is_Adjacent_Z (Bottom => W.Bricks (I).Box, Top => W.Bricks (J).Box) then
                  W.Bricks (I).Above.Include (J);
                  W.Bricks (J).Below.Include (I);
               end if;
            end if;
         end loop;
      end loop;

      return Moved;
   end Settle;

   --  Counts the number of destroyable blocks. A block is destroyable if it
   --  supports no blocks, or the blocks it supports are all supported by at
   --  least one other block.
   function Count_Destroyable (W : World) return Integer is
      Destroyable : Brick_Index_Sets.Set;
   begin
      for I in W.Bricks.First_Index .. W.Bricks.Last_Index loop
         if W.Bricks (I).Above.Length = 0 then
            Destroyable.Include (I);
         else
            if (for all A of W.Bricks (I).Above => W.Bricks (A).Below.Length > 1) then
               Destroyable.Include (I);
            end if;
         end if;
      end loop;
      return Integer (Destroyable.Length);
   end Count_Destroyable;

   --  Print the world along `Horizontal` and Z axes, useful for debugging
   procedure Print (W : World; Horizontal : Axis) is
      Bounds   : constant AABB := Extents (W);
      Num_Hit  : Integer       := 0;
      Last_Hit : Brick_Index;
      B        : AABB;
   begin
      Put_Line ("==" & Horizontal'Image & "==>");
      for P_Z in reverse Bounds.Min (Z) .. Bounds.Max (Z) loop
         for P_X in Bounds.Min (Horizontal) .. Bounds.Max (Horizontal) loop
            Num_Hit := 0;
            for I in W.Bricks.First_Index .. W.Bricks.Last_Index loop
               B := W.Bricks (I).Box;
               if B.Min (Horizontal) <= P_X and then P_X <= B.Max (Horizontal)
                 and then B.Min (Z) <= P_Z and then P_Z <= B.Max (Z)
               then
                  Num_Hit  := Num_Hit + 1;
                  Last_Hit := I;
               end if;
            end loop;
            if Num_Hit = 1 then
               --  doesn't work for real inputs but it doesn't need to (too
               --  many bricks; runs out of character range)
               Put (Character'Val (Character'Pos ('A') + W.Bricks (Last_Hit).Id - 1));
            elsif Num_Hit = 0 then
               Put ('.');
            else
               Put ('?');
            end if;
         end loop;
         Put_Line (P_Z'Image);
      end loop;
   end Print;

   --  locals
   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   W     : World;

begin

   for Line of Lines loop
      Add_Brick (W, Parse_AABB (Line));
   end loop;

   --  Part 1
   declare
      Moved : Integer := Settle (W);
   begin
      Solution (Count_Destroyable (W));
   end;

   --  Part 2: honestly, simulating is fast enough to just try breaking stuff
   declare
      Sum : Integer := 0;
   begin
      for I in W.Bricks.First_Index .. W.Bricks.Last_Index loop
         declare
            W2 : World := Copy (W);
         begin
            W2.Bricks.Delete (I);
            Sum := Sum + Settle (W2);
         end;
      end loop;
      Solution (Sum);
   end;

end Day22;
