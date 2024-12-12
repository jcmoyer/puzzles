with Advent.IO;   use Advent.IO;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Directions;          use Advent.Directions;
with Ada.Containers;

procedure Day12 is

   type Visited_Map is array (Integer range <>, Integer range <>) of Boolean;
   type Resolved_Map is array (Integer range <>, Integer range <>) of Integer;

   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vec2, Hash => Hash, Equivalent_Elements => "=", "=" => "=");

   function Measure_Region
     (M       :        Char_Matrix;
      Start   :        Vec2;
      Visited : in out Visited_Map;
      Region  :    out Vec2_Sets.Set)
      return Integer
   is

      Perim : Integer := 0;
      Area  : Integer := 0;

      function Count_Neighboring (Pos : Vec2; What : Character) return Integer is
         Result : Integer := 0;
      begin
         for D in Cardinal_Direction loop
            if In_Bounds (M, Pos + To_Vector (D)) and then Element (M, Pos + To_Vector (D)) = What
            then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Count_Neighboring;

      procedure Rec_Visit (Pos : Vec2) is
      begin
         if not In_Bounds (M, Pos) then
            return;
         end if;

         if Element (M, Pos) /= Element (M, Start) then
            return;
         end if;

         if Visited (Pos (X), Pos (Y)) then
            return;
         end if;

         Visited (Pos (X), Pos (Y)) := True;
         Perim                      := Perim + (4 - Count_Neighboring (Pos, Element (M, Start)));

         Area := Area + 1;

         Region.Include (Pos);

         for D in Cardinal_Direction loop
            Rec_Visit (Pos + To_Vector (D));
         end loop;
      end Rec_Visit;

   begin
      Rec_Visit (Start);
      return Perim * Area;

   end Measure_Region;

   Map : constant Char_Matrix := Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

   Visited : Visited_Map (Map'Range (1), Map'Range (2)) := (others => (others => False));

   Part_1  : Integer := 0;
   Part_2 : Integer := 0;

   package Vec2_Set_Vectors is new Ada.Containers.Vectors
     (Positive, Vec2_Sets.Set, "=" => Vec2_Sets."=");

   use type Ada.Containers.Count_Type;
   R : Vec2_Sets.Set;

   function Grow_Region (Region : Vec2_Sets.Set) return Vec2_Sets.Set is
      Result : Vec2_Sets.Set;
      C      : Vec2_Sets.Cursor := Region.First;
      use type Vec2_Sets.Cursor;
   begin
      while C /= Vec2_Sets.No_Element loop
         for D in Direction loop
            Result.Include (Vec2_Sets.Element (C) + To_Vector (D));
         end loop;
         Vec2_Sets.Next (C);
      end loop;
      return Result;
   end Grow_Region;

   package Vec2_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vec2);

   function Sort_LTR (A, B : Vec2) return Boolean is (A (Y) < B (Y));
   package LTR_Sorting is new Vec2_Vectors.Generic_Sorting ("<" => Sort_LTR);

   procedure Measure (Region : Vec2_Sets.Set; Min, Max : out Vec2) is
      C : Vec2_Sets.Cursor := Region.First;
      use type Vec2_Sets.Cursor;
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
   end Measure;

   function Trace_Perimeter
     (Region : Vec2_Sets.Set; Unresolved_Interiors : out Vec2_Sets.Set) return Integer
   is
      C : Vec2_Sets.Cursor := Region.First;
      use type Vec2_Sets.Cursor;
      Current_Dir : Cardinal_Direction;
      Sides       : Integer       := 0;
      Start       : Vec2;
      Current     : Vec2;
      List        : Vec2_Vectors.Vector;
      Unvisited   : Vec2_Sets.Set := Region.Copy;

      function Right return Vec2 is
      begin
         return Current + To_Vector (Rotate_CW_90 (Current_Dir));
      end Right;

      function Forward return Vec2 is
      begin
         return Current + To_Vector (Current_Dir);
      end Forward;

      function Left return Vec2 is
      begin
         return Current + To_Vector (Rotate_CCW_90 (Current_Dir));
      end Left;

      function Backward return Vec2 is
      begin
         return Current + To_Vector (Opposite (Current_Dir));
      end Backward;

      procedure Flood_Visit (From : Vec2) is
      begin
         Unvisited.Exclude (From);
         for D in Cardinal_Direction loop
            if Unvisited.Contains (From + To_Vector (D)) then
               Flood_Visit (From + To_Vector (D));
            end if;
         end loop;
      end Flood_Visit;

   begin
      --  pick a spot that hopefully puts us on a path traveling north and
      --  where turning right would bias towards the interior of the shape
      while C /= Vec2_Sets.No_Element loop
         List.Append (Vec2_Sets.Element (C));
         C := Vec2_Sets.Next (C);
      end loop;

      LTR_Sorting.Sort (List);
      for V of List loop
         Put_Line (Standard_Error, "Check " & Image (V));
         if Region.Contains (V + To_Vector (North))
           and then Region.Contains (V + To_Vector (South))
            --  Concave next to starting point produces an invalid interior region

           and then not Region.Contains (V + To_Vector (East))
           and then not Region.Contains (V + To_Vector (West))
         then
            Start       := V;
            Current     := V;
            Current_Dir := North;
            Put_Line (Standard_Error, "Start " & Image (Start));
            exit;
         end if;
      end loop;

      Unvisited.Exclude (Current);

      loop
         --  Rules:
         --  1. If we can turn right, do that. (+1 side)
         --  2. Otherwise, if we can go forward, do that. (+0 side)
         --  3. If we can do neither, try turning left. (+1 side)
         --  4. Try turning left twice to go back the way we came. (+2 side)
         if Region.Contains (Right) then
            Sides       := Sides + 1;
            Current_Dir := Rotate_CW_90 (Current_Dir);

            --  We move on the iteration we turn so that we don't get stuck on
            --  corners like:
            --  ##
            --  #
            Current := Current + To_Vector (Current_Dir);
         elsif Region.Contains (Forward) then
            Current := Current + To_Vector (Current_Dir);
         elsif Region.Contains (Left) then
            Sides       := Sides + 1;
            Current_Dir := Rotate_CCW_90 (Current_Dir);

            --  We move on the iteration we turn so that we don't get stuck on
            --  corners like:
            --  ##
            --   #
            Current := Current + To_Vector (Current_Dir);
         elsif Region.Contains (Backward) then
            Sides       := Sides + 2;
            Current_Dir := Opposite (Current_Dir);
            Current     := Current + To_Vector (Current_Dir);
         else
            raise Program_Error with "uh oh we can't move! bad input?";
         end if;

         Put_Line (Standard_Error, Image (Current));
         Unvisited.Exclude (Current);

         if Current = Start then
            while Unvisited.Length > 0 loop
               --  Pick random unvisited
               C := Unvisited.First;

               --  We will score this using the interior's exterior score later
               Unresolved_Interiors.Include (Vec2_Sets.Element (C));

               --  Visit every connected tile so we don't double-count
               Flood_Visit (Vec2_Sets.Element (C));
            end loop;
            exit;
         end if;
      end loop;

      return Sides;
   end Trace_Perimeter;

   procedure Print (Region : Vec2_Sets.Set; Min, Max, Mark : Vec2) is
   begin
      for I in Min (Y) .. Max (Y) loop
         for J in Min (X) .. Max (X) loop
            if Mark = (I, J) then
               Put ('X');
            elsif Region.Contains ((I, J)) then
               Put ('#');
            else
               Put ('.');
            end if;
         end loop;
         New_Line;
      end loop;
   end Print;

   All_Unresolved  : Vec2_Set_Vectors.Vector;
   Resolved_Scores : Resolved_Map (Map'Range (1), Map'Range (2)) := (others => (others => 0));

   type Resolved_Region is record
      Name       : Character;
      Area       : Integer;
      Perimeter  : Integer;
      Unresolved : Vec2_Sets.Set;
   end record;

   package Resolved_Region_Vectors is new Ada.Containers.Vectors (Positive, Resolved_Region);
   All_Resolved : Resolved_Region_Vectors.Vector;

begin

   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         R.Clear;
         Part_1 := Part_1 + (Measure_Region (Map, (I, J), Visited, R));
         if R.Length > 0 then
            declare
               G          : Vec2_Sets.Set := Grow_Region (R);
               Unresolved : Vec2_Sets.Set;
               Perim      : Integer;
               RR         : Resolved_Region;
            begin
               G.Difference (R);
               Put_Line (Standard_Error, "For " & Element (Map, (I, J))'Image);
               Perim := Trace_Perimeter (G, Unresolved);
               Put_Line (Standard_Error, Unresolved.Length'Image & " unresolved");
               All_Unresolved.Append (Unresolved);

               RR.Perimeter  := Perim;
               RR.Unresolved := Unresolved;
               RR.Name       := Map (I, J);
               RR.Area       := Integer (R.Length);
               All_Resolved.Append (RR);

               --  resolve all tiles for this region
               for V of R loop
                  Resolved_Scores (V (X), V (Y)) := Perim;
               end loop;
            end;
         end if;
      end loop;
   end loop;

   Solution (Part_1);

   for RR of All_Resolved loop
      Put_Line (Standard_Error, "Resolving " & RR.Name'Image);
      declare
         Total_Perim : Integer := RR.Perimeter;
      begin
         for UR of RR.Unresolved loop
            Put_Line (Standard_Error, "UR is " & Image (UR));
            Total_Perim := Total_Perim + Resolved_Scores (UR (X), UR (Y));
         end loop;
         Put_Line (Standard_Error, "Total Perim " & Total_Perim'Image);
         Part_2 := Part_2 + (RR.Area * Total_Perim);
      end;
   end loop;

   Solution (Part_2);

end Day12;
