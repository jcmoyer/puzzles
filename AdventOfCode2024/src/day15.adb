with Advent.IO;                  use Advent.IO;
with Advent.Strings;             use Advent.Strings;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Advent.Directions;          use Advent.Directions;

procedure Day15 is

   type Char_Matrix_Ptr is access all Char_Matrix;

   package Direction_Vectors is new Ada.Containers.Vectors (Positive, Cardinal_Direction);
   Directions : Direction_Vectors.Vector;

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);

   procedure Gather_Effectable_Boxes
     (Map   :        Char_Matrix;
      Start :        Vec2;
      Force :        Cardinal_Direction;
      Boxes : in out Vec2_Vectors.Vector)
   is
      Examine : Vec2_Vectors.Vector;
      Next    : Vec2;

      Force_Vec : constant Vec2 := To_Vector (Force);

      function Is_Box (Pos : Vec2) return Boolean is
         E : constant Character := Map (Pos (X), Pos (Y));
      begin
         return E = '[' or else E = ']';
      end Is_Box;

      function Is_Wall (Pos : Vec2) return Boolean is
         E : constant Character := Map (Pos (X), Pos (Y));
      begin
         return E = '#';
      end Is_Wall;

      --  looks at the points behind the box at `Pos` and adds them to the list
      --  of tiles to check
      procedure Examine_Effected_From (Pos : Vec2) is
         E : constant Character := Map (Pos (X), Pos (Y));
      begin
         if Force = North or else Force = South then
            if E = '[' then
               Examine.Append (Pos + Force_Vec);
               Examine.Append ((Pos + To_Vector (East)) + Force_Vec);
            elsif E = ']' then
               Examine.Append (Pos + Force_Vec);
               Examine.Append ((Pos + To_Vector (West)) + Force_Vec);
            end if;
         else
            if Force = West then
               if E = ']' then
                  Examine.Append (Pos + 2 * Force_Vec);
               else
                  raise Program_Error with "west on ]?";
               end if;
            elsif Force = East then
               if E = '[' then
                  Examine.Append (Pos + 2 * Force_Vec);
               else
                  raise Program_Error with "east on [?";
               end if;
            end if;
         end if;
      end Examine_Effected_From;

      function Box_Left (Pos : Vec2) return Vec2 with
        Pre => Is_Box (Pos)
      is
         E : constant Character := Map (Pos (X), Pos (Y));
      begin
         if E = '[' then
            return Pos;
         elsif E = ']' then
            return Pos + To_Vector (West);
         end if;
         raise Program_Error with "precondition: Is_Box (Pos)";
      end Box_Left;

   begin
      Boxes.Clear;
      Examine.Append (Start);
      loop
         exit when Examine.Is_Empty;
         Next := Examine.Last_Element;
         Examine.Delete_Last;

         if Is_Box (Next) then
            Examine_Effected_From (Next);
            Boxes.Append (Box_Left (Next));
         elsif Is_Wall (Next) then
            Boxes.Clear;
            exit;
         end if;
      end loop;
   end Gather_Effectable_Boxes;

   function Order_West_To_East (A, B : Vec2) return Boolean is (A (Y) < B (Y));
   package West_To_East_Sorting is new Vec2_Vectors.Generic_Sorting ("<" => Order_West_To_East);

   function Order_North_To_South (A, B : Vec2) return Boolean is (A (X) < B (X));
   package North_To_South_Sorting is new Vec2_Vectors.Generic_Sorting
     ("<" => Order_North_To_South);

   procedure Move_Boxes
     (Map : in out Char_Matrix; Force : Cardinal_Direction; Boxes : in out Vec2_Vectors.Vector)
   is
   begin
      case Force is
         when West =>
            West_To_East_Sorting.Sort (Boxes);
            for B in Boxes.First_Index .. Boxes.Last_Index loop
               Map (Boxes (B) (X), Boxes (B) (Y) - 1) := '[';
               Map (Boxes (B) (X), Boxes (B) (Y) + 0) := ']';
               Map (Boxes (B) (X), Boxes (B) (Y) + 1) := '.';
            end loop;

         when East =>
            West_To_East_Sorting.Sort (Boxes);
            for B in reverse Boxes.First_Index .. Boxes.Last_Index loop
               Map (Boxes (B) (X), Boxes (B) (Y) + 2) := ']';
               Map (Boxes (B) (X), Boxes (B) (Y) + 1) := '[';
               Map (Boxes (B) (X), Boxes (B) (Y) + 0) := '.';
            end loop;

         when North =>
            North_To_South_Sorting.Sort (Boxes);
            for B in Boxes.First_Index .. Boxes.Last_Index loop
               Map (Boxes (B) (X) - 1, Boxes (B) (Y) + 0) := '[';
               Map (Boxes (B) (X) - 1, Boxes (B) (Y) + 1) := ']';
               Map (Boxes (B) (X) - 0, Boxes (B) (Y) + 0) := '.';
               Map (Boxes (B) (X) - 0, Boxes (B) (Y) + 1) := '.';
            end loop;

         when South =>
            North_To_South_Sorting.Sort (Boxes);
            for B in reverse Boxes.First_Index .. Boxes.Last_Index loop
               Map (Boxes (B) (X) + 1, Boxes (B) (Y) + 0) := '[';
               Map (Boxes (B) (X) + 1, Boxes (B) (Y) + 1) := ']';
               Map (Boxes (B) (X) - 0, Boxes (B) (Y) + 0) := '.';
               Map (Boxes (B) (X) - 0, Boxes (B) (Y) + 1) := '.';
            end loop;
      end case;
   end Move_Boxes;

   function Widen_Map (Original : Char_Matrix; Robot : out Vec2) return Char_Matrix_Ptr is
      Wide : constant Char_Matrix_Ptr :=
        new Char_Matrix (1 .. Original'Length (1), 1 .. 2 * Original'Length (2));
   begin
      for I in Original'Range (1) loop
         for J in Original'Range (2) loop
            if Original (I, J) = '#' then
               Wide (I, J * 2 - 1) := '#';
               Wide (I, J * 2 - 0) := '#';
            elsif Original (I, J) = 'O' then
               Wide (I, J * 2 - 1) := '[';
               Wide (I, J * 2 - 0) := ']';
            elsif Original (I, J) = '.' then
               Wide (I, J * 2 - 1) := '.';
               Wide (I, J * 2 - 0) := '.';
            elsif Original (I, J) = '@' then
               Wide (I, J * 2 - 1) := '@';
               Wide (I, J * 2 - 0) := '.';

               Robot := (I, J * 2 - 1);
            end if;
         end loop;
      end loop;

      return Wide;
   end Widen_Map;

   function Load_Map
     (Lines : String_Array; First, Last, Width : Integer; Robot : out Vec2) return Char_Matrix_Ptr
   is
      Map : constant Char_Matrix_Ptr := new Char_Matrix (1 .. Last, 1 .. Width);
   begin
      for I in First .. Last loop
         for J in 1 .. Width loop
            Map (I, J) := Lines (I) (J);
            if Map (I, J) = '@' then
               Robot := (I, J);
            end if;
         end loop;
      end loop;
      return Map;
   end Load_Map;

   Lines : constant String_Array := Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Map : Char_Matrix_Ptr;

   Part_1 : Integer := 0;
   Part_2 : Integer := 0;

   Tilemap_First               : constant Integer := 1;
   Tilemap_Last, Tilemap_Width : Integer;

   Robot : Vec2;

begin
   for I in Lines.First_Index .. Lines.Last_Index loop
      if Index (Lines (I), "#") > 0 then
         Tilemap_Width := String'(Lines (I))'Length;
      elsif Index (Lines (I), "^") > 0 then
         for C of Lines (I) loop
            Directions.Append (Parse_Direction (C));
         end loop;
      elsif String'(Lines (I))'Length = 0 then
         Tilemap_Last := I - 1;
      end if;
   end loop;

   Map := Load_Map (Lines, Tilemap_First, Tilemap_Last, Tilemap_Width, Robot);
   --  Put_Line (Image (Map.all));

   for D of Directions loop
      declare
         DVec   : constant Vec2 := To_Vector (D);
         Target : constant Vec2 := Robot + To_Vector (D);

         P : Vec2 := Target;
      begin
         if Element (Map.all, Target) = '.' then
            Map (Robot (X), Robot (Y))   := '.';
            Map (Target (X), Target (Y)) := '@';
            Robot                        := Target;
         elsif Element (Map.all, Target) = 'O' then
            loop
               P := P + DVec;
               if Element (Map.all, P) = '.' then
                  Map (Robot (X), Robot (Y))   := '.';
                  Map (Target (X), Target (Y)) := '@';
                  Robot                        := Target;
                  loop
                     exit when P = Target;
                     Map (P (X), P (Y)) := 'O';
                     P                  := P - DVec;
                  end loop;
                  exit;
               elsif Element (Map.all, P) = '#' then
                  exit;
               end if;
            end loop;
         end if;
      end;
      --  Put_Line (Image (Map.all));
   end loop;

   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         if Map (I, J) = 'O' then
            Part_1 := Part_1 + 100 * (I - 1) + (J - 1);
         end if;
      end loop;
   end loop;

   Solution (Part_1);

   --  reload map
   for I in Tilemap_First .. Tilemap_Last loop
      for J in 1 .. Tilemap_Width loop
         Map (I, J) := Lines (I) (J);
         if Map (I, J) = '@' then
            Robot := (I, J);
         end if;
      end loop;
   end loop;

   Map := Widen_Map (Map.all, Robot);

   for D of Directions loop
      declare
         Target     : constant Vec2 := Robot + To_Vector (D);
         Effectable : Vec2_Vectors.Vector;
      begin
         --  Put_Line ("Move " & D'Image);
         if Element (Map.all, Target) = '.' then
            Map (Robot (X), Robot (Y))   := '.';
            Map (Target (X), Target (Y)) := '@';
            Robot                        := Target;
         elsif Element (Map.all, Target) = '[' or else Element (Map.all, Target) = ']' then
            Gather_Effectable_Boxes (Map.all, Target, D, Effectable);
            if not Effectable.Is_Empty then
               Move_Boxes (Map.all, D, Effectable);
               Map (Robot (X), Robot (Y))   := '.';
               Map (Target (X), Target (Y)) := '@';
               Robot                        := Target;
               --  Put_Line ("Move " & Effectable.Length'Image);
            end if;
         end if;
      end;
      --  Put_Line (Image (Map.all));
   end loop;

   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         if Map (I, J) = '[' then
            Part_2 := Part_2 + 100 * (I - 1) + (J - 1);
         end if;
      end loop;
   end loop;

   Solution (Part_2);
end Day15;
