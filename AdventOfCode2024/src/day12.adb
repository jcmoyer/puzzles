with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Directions;          use Advent.Directions;
with Ada.Command_Line;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Containers;

--  This is definitely the incorrect way to solve this problem, but I think
--  it's an interesting one.
--
--  1. Explore all regions; collecting a set of tiles in each region.
--  2. Perform edge detection on those tiles.
--  3. Walk the outermost edge with a simple set of rules to determine how many
--     sides the region has.
--  4. Any tiles we didn't visit walking the edge belong to regions enclosed by
--     the region we're examining.
--  5. Once all regions have been explored, for each region add to its number
--     of sides the sum of all its enclosed regions' sides.
procedure Day12 is

   use type Ada.Containers.Count_Type;

   Invalid_State : exception;

   --  True if we've been to index (I,J).
   type Visited_Map is array (Integer range <>, Integer range <>) of Boolean with
     Default_Component_Value => False;
   type Visited_Map_Ptr is access all Visited_Map;

   --  Holds the number of exterior sides the region containing index (I,J) has.
   type Exterior_Side_Map is array (Integer range <>, Integer range <>) of Natural with
     Default_Component_Value => 0;
   type Exterior_Side_Map_Ptr is access all Exterior_Side_Map;

   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vec2, Hash => Hash, Equivalent_Elements => "=", "=" => "=");

   package Vec2_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vec2);

   function Sort_LTR (A, B : Vec2) return Boolean is (A (Y) < B (Y));

   package LTR_Sorting is new Vec2_Vectors.Generic_Sorting ("<" => Sort_LTR);

   type Region is record
      Name            : Character;
      Area            : Natural;
      --  Part 1
      Tile_Perimeter  : Natural;
      --  Part 2
      Side_Perimeter  : Natural;
      --  All points in this region
      Points          : Vec2_Sets.Set;
      --  Randomly selected points that are within the extent of this region,
      --  but actually belong to interior regions. Each interior region is
      --  guaranteed to be represented once in this set.
      Interior_Points : Vec2_Sets.Set;
   end record;

   package Region_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Region);

   --  Explores outward from a position on the map and collects all the tiles
   --  belonging to that region.
   procedure Extract_Region
     (Map : Char_Matrix; Start : Vec2; Visited : in out Visited_Map; Reg : out Region)
   is
      function Count_Neighboring (Pos : Vec2; What : Character) return Integer is
         Result   : Integer := 0;
         Neighbor : Vec2;
      begin
         for D in Cardinal_Direction loop
            Neighbor := Pos + To_Vector (D);
            if In_Bounds (Map, Neighbor) and then Element (Map, Neighbor) = What then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Neighboring;

      procedure Rec_Visit (Pos : Vec2) is
      begin
         if not In_Bounds (Map, Pos) then
            return;
         end if;

         if Element (Map, Pos) /= Element (Map, Start) then
            return;
         end if;

         if Visited (Pos (X), Pos (Y)) then
            return;
         end if;

         Visited (Pos (X), Pos (Y)) := True;

         Reg.Tile_Perimeter := Reg.Tile_Perimeter + (4 - Count_Neighboring (Pos, Reg.Name));
         Reg.Area           := Reg.Area + 1;

         Reg.Points.Include (Pos);

         for D in Cardinal_Direction loop
            Rec_Visit (Pos + To_Vector (D));
         end loop;
      end Rec_Visit;

   begin
      Reg :=
        (Name            => Element (Map, Start),
         Area            => 0,
         Tile_Perimeter  => 0,
         Side_Perimeter  => 0,
         Points          => Vec2_Sets.Empty_Set,
         Interior_Points => Vec2_Sets.Empty_Set);

      Rec_Visit (Start);
   end Extract_Region;

   procedure Finalize_Perimeter (Reg : in out Region; Sides : Exterior_Side_Map) is
   begin
      for V of Reg.Interior_Points loop
         Reg.Side_Perimeter := Reg.Side_Perimeter + Sides (V (X), V (Y));
      end loop;
   end Finalize_Perimeter;

   --  For each point in `Points`, add the 8 points adjacent to that point.
   --  Result is stored in `Destination`.
   procedure Grow_Points (Points : Vec2_Sets.Set; Destination : in out Vec2_Sets.Set) is
   begin
      Destination.Clear;
      Destination.Reserve_Capacity (Points.Length * 8);
      for V of Points loop
         for D in Direction loop
            Destination.Include (V + To_Vector (D));
         end loop;
      end loop;
   end Grow_Points;

   --  Produce the outline of `Source` and store it in `Destination`.
   procedure Outline (Source : Vec2_Sets.Set; Destination : in out Vec2_Sets.Set) is
   begin
      Grow_Points (Source, Destination);
      Destination.Difference (Source);
   end Outline;

   --  Returns the min/max vectors of a bounding box that fully encloses `Points`.
   --  Only used for debugging.
   procedure Extents (Points : Vec2_Sets.Set; Min, Max : out Vec2) is
      use type Vec2_Sets.Cursor;
      C : Vec2_Sets.Cursor := Points.First;
   begin
      Min := Vec2_Sets.Element (C);
      Max := Min;
      while C /= Vec2_Sets.No_Element loop
         Min (X) := Integer'Min (Min (X), Vec2_Sets.Element (C) (X));
         Min (Y) := Integer'Min (Min (Y), Vec2_Sets.Element (C) (Y));
         Max (X) := Integer'Max (Max (X), Vec2_Sets.Element (C) (X));
         Max (Y) := Integer'Max (Max (Y), Vec2_Sets.Element (C) (Y));
         Vec2_Sets.Next (C);
      end loop;
   end Extents;

   procedure Flood_Visit (Unvisited : in out Vec2_Sets.Set; From : Vec2) is
   begin
      Unvisited.Exclude (From);
      for D in Cardinal_Direction loop
         if Unvisited.Contains (From + To_Vector (D)) then
            Flood_Visit (Unvisited, From + To_Vector (D));
         end if;
      end loop;
   end Flood_Visit;

   function Trace_Perimeter (Points : Vec2_Sets.Set; Interiors : out Vec2_Sets.Set) return Natural
   is
      function Find_Start return Vec2 is
         Points_Vec : Vec2_Vectors.Vector;
      begin
         Points_Vec.Reserve_Capacity (Points.Length);

         --  Pick a spot that hopefully puts us on a path traveling north and
         --  where turning right would bias towards the interior of the shape.
         for V of Points loop
            Points_Vec.Append (V);
         end loop;

         LTR_Sorting.Sort (Points_Vec);

         for V of Points_Vec loop
            --  Concave next to starting point produces an invalid interior
            --  region. Make sure we are on a *vertical only* point.
            if Points.Contains (V + To_Vector (North))
              and then Points.Contains (V + To_Vector (South))
              and then not Points.Contains (V + To_Vector (East))
              and then not Points.Contains (V + To_Vector (West))
            then
               return V;
            end if;
         end loop;

         raise Invalid_State with "no suitable starting location";
      end Find_Start;

      Current_Dir : Cardinal_Direction := North;
      Sides       : Natural            := 0;
      Start       : constant Vec2      := Find_Start;
      Current     : Vec2               := Start;
      Unvisited   : Vec2_Sets.Set      := Points.Copy;

      function Right return Vec2 is (Current + To_Vector (Rotate_CW_90 (Current_Dir)));
      function Forward return Vec2 is (Current + To_Vector (Current_Dir));
      function Left return Vec2 is (Current + To_Vector (Rotate_CCW_90 (Current_Dir)));
      function Backward return Vec2 is (Current + To_Vector (Opposite (Current_Dir)));

   begin
      Unvisited.Exclude (Current);

      --  Rules:
      --  1. If we can turn right, do that. (+1 side)
      --  2. Otherwise, if we can go forward, do that. (+0 side)
      --  3. If we can do neither, try turning left. (+1 side)
      --  4. Try turning left twice to go back the way we came. (+2 side)
      loop
         if Points.Contains (Right) then
            Sides       := Sides + 1;
            Current_Dir := Rotate_CW_90 (Current_Dir);
            Current     := Forward;
         elsif Points.Contains (Forward) then
            Current := Forward;
         elsif Points.Contains (Left) then
            Sides       := Sides + 1;
            Current_Dir := Rotate_CCW_90 (Current_Dir);
            Current     := Forward;
         elsif Points.Contains (Backward) then
            Sides       := Sides + 2;
            Current_Dir := Opposite (Current_Dir);
            Current     := Forward;
         else
            raise Invalid_State with "uh oh we can't move! bad input?";
         end if;

         Unvisited.Exclude (Current);

         --  We made it back to the start
         exit when Current = Start;
      end loop;

      --  Any remaining tiles belong to interior regions.
      while Unvisited.Length > 0 loop
         declare
            --  Pick random unvisited point.
            Interior : constant Vec2 := Vec2_Sets.Element (Unvisited.First);
         begin
            --  We will add this interior's side count to ours later once we've
            --  visited all the regions.
            Interiors.Include (Interior);
            --  Visit every connected tile so we don't double-count.
            Flood_Visit (Unvisited, Interior);
         end;
      end loop;

      return Sides;
   end Trace_Perimeter;

   --  Only used for debugging.
   procedure Print (Points : Vec2_Sets.Set; Min, Max, Mark : Vec2) is
   begin
      for I in Min (Y) .. Max (Y) loop
         for J in Min (X) .. Max (X) loop
            if Mark = (I, J) then
               Put ('X');
            elsif Points.Contains ((I, J)) then
               Put ('#');
            else
               Put ('.');
            end if;
         end loop;
         New_Line;
      end loop;
   end Print;

   procedure Gather_Regions (Map : Char_Matrix; Regions : in out Region_Vectors.Vector) is
      Visited     : constant Visited_Map_Ptr := new Visited_Map (Map'Range (1), Map'Range (2));
      Side_Map    : constant Exterior_Side_Map_Ptr :=
        new Exterior_Side_Map (Map'Range (1), Map'Range (2));
      Outline_Pts : Vec2_Sets.Set;
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            if not Visited (I, J) then
               declare
                  Interiors : Vec2_Sets.Set;
                  Reg       : Region;
               begin
                  Extract_Region (Map, (I, J), Visited.all, Reg);

                  --  Edge detection and tracing
                  Outline (Reg.Points, Outline_Pts);
                  Reg.Side_Perimeter := Trace_Perimeter (Outline_Pts, Interiors);
                  Reg.Interior_Points.Move (Interiors);

                  --  Mark all tiles for this region with our exterior side count
                  for V of Reg.Points loop
                     Side_Map (V (X), V (Y)) := Reg.Side_Perimeter;
                  end loop;

                  Regions.Append (Reg);
               end;
            end if;
         end loop;
      end loop;

      for Reg of Regions loop
         Finalize_Perimeter (Reg, Side_Map.all);
      end loop;
   end Gather_Regions;

   Map : constant Char_Matrix := Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

   All_Regions : Region_Vectors.Vector;

   Part_1 : Integer := 0;
   Part_2 : Integer := 0;

begin
   Gather_Regions (Map, All_Regions);

   for Reg of All_Regions loop
      Part_1 := Part_1 + (Reg.Area * Reg.Tile_Perimeter);
      Part_2 := Part_2 + (Reg.Area * Reg.Side_Perimeter);
   end loop;

   Solution (Part_1);
   Solution (Part_2);
end Day12;
