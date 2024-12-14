with Advent.IO;                  use Advent.IO;
with Advent.Strings;             use Advent.Strings;
with Advent.Integer_Parsers;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Day14 is

   package AIP renames Advent.Integer_Parsers;

   type Space is record
      Width, Height : Integer;
   end record;

   Test_Space    : constant Space := (Width => 11, Height => 7);
   Default_Space : constant Space := (Width => 101, Height => 103);

   type Robot_Map is array (Natural range <>, Natural range <>) of Natural with
     Default_Component_Value => 0;

   procedure Inc (Map : in out Robot_Map; Position : Vec2) is
   begin
      Map (Position (X), Position (Y)) := Map (Position (X), Position (Y)) + 1;
   end Inc;

   procedure Dec (Map : in out Robot_Map; Position : Vec2) is
   begin
      Map (Position (X), Position (Y)) := Map (Position (X), Position (Y)) - 1;
   end Dec;

   function Wrap (Index : Vec2; S : Space) return Vec2 is
   begin
      return (Index (X) mod S.Width, Index (Y) mod S.Height);
   end Wrap;

   type Robot is record
      Pos, Vel : Vec2;
   end record;

   package Robot_Vectors is new Ada.Containers.Vectors (Positive, Robot);

   procedure Update (R : in out Robot; Map : in out Robot_Map) is
   begin
      Dec (Map, R.Pos);
      R.Pos := R.Pos + R.Vel;
      R.Pos := Wrap (R.Pos, Default_Space);
      Inc (Map, R.Pos);
   end Update;

   function Score (Map : Robot_Map) return Natural is
      Quads : array (1 .. 4) of Natural := (others => 0);

      Mid_X : constant Natural := Map'First (1) + Map'Length (1) / 2;
      Mid_Y : constant Natural := Map'First (2) + Map'Length (2) / 2;
   begin
      for X in Map'Range (1) loop
         for Y in Map'Range (2) loop
            if X < Mid_X and Y < Mid_Y then
               Quads (1) := Quads (1) + Map (X, Y);
            elsif X > Mid_X and Y < Mid_Y then
               Quads (2) := Quads (2) + Map (X, Y);
            elsif X < Mid_X and Y > Mid_Y then
               Quads (3) := Quads (3) + Map (X, Y);
            elsif X > Mid_X and Y > Mid_Y then
               Quads (4) := Quads (4) + Map (X, Y);
            end if;
         end loop;
      end loop;
      return Quads (1) * Quads (2) * Quads (3) * Quads (4);
   end Score;

   function Has_Tree (Map : Robot_Map) return Boolean is
   begin
      --  Heuristic based on actual tree image.
      return (for all X in 31 .. 61 => Map (X, 38) > 0);
   end Has_Tree;

   procedure Print (Map : Robot_Map) is
   begin
      for Y in Map'Range (2) loop
         for X in Map'Range (1) loop
            if Map (X, Y) > 0 then
               Put (Map (X, Y)'Image (2 .. 2));
            else
               Put ('.');
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print;

   Lines : constant String_Array := Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Part_1 : Integer := 0;
   Part_2 : Integer := 0;

   Robots : Robot_Vectors.Vector;

   Map : Robot_Map (0 .. Default_Space.Width - 1, 0 .. Default_Space.Height - 1);

   Iterations : Natural := 0;

begin
   for Line of Lines loop
      declare
         Ints : AIP.Array_Type (1 .. 4);
         N    : constant Natural := AIP.Extract_Integers (Line, Ints);
      begin
         if 4 /= N then
            raise Program_Error with "expected 4 integers in line";
         end if;

         Robots.Append ((Pos => (Ints (1), Ints (2)), Vel => (Ints (3), Ints (4))));
         Inc (Map, Robots.Last_Element.Pos);
      end;
   end loop;

   loop
      Iterations := Iterations + 1;

      for R of Robots loop
         Update (R, Map);
      end loop;

      if Iterations = 100 then
         Part_1 := Score (Map);
      end if;

      if Has_Tree (Map) then
         Part_2 := Iterations;
         exit;
      end if;
   end loop;

   Solution (Part_1);
   Solution (Part_2);
end Day14;
