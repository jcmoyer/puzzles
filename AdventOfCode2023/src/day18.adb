with Advent;            use Advent;
with Advent.Directions; use Advent.Directions;
with Advent.Vector_Math;
with Ada.Command_Line;
with Ada.Containers;    use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;

procedure Day18 is
   package Long_Vectors is new Advent.Vector_Math (Element_Type => Long_Long_Integer);

   use Long_Vectors;

   type Dig_Order is record
      --  Part 1 comes from first two components
      Dir_1    : Direction;
      Amount_1 : Long_Long_Integer;

      --  Part 2 parsed from color string
      Dir_2    : Direction;
      Amount_2 : Long_Long_Integer;
   end record;

   --  A dig plan is a list of dig orders
   package Dig_Plans is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Dig_Order);

   subtype Dig_Plan is Dig_Plans.Vector;

   Invalid_Character : exception;

   function Parse_LRUD (C : Character) return Direction is
   begin
      case C is
         when 'L' =>
            return West;
         when 'R' =>
            return East;
         when 'U' =>
            return North;
         when 'D' =>
            return South;
         when others =>
            raise Invalid_Character;
      end case;
   end Parse_LRUD;

   function Parse_LRUD (S : String) return Direction is (Parse_LRUD (S (S'First)));

   function Parse_Hex_Direction (C : Character) return Direction is
   begin
      case C is
         when '0' =>
            return East;
         when '1' =>
            return South;
         when '2' =>
            return West;
         when '3' =>
            return North;
         when others =>
            raise Invalid_Character;
      end case;
   end Parse_Hex_Direction;

   function Parse_Hex_Direction (S : String) return Direction is
     (Parse_Hex_Direction (S (S'Last)));

   function Parse_Dig_Order (S : String) return Dig_Order is
      Parts  : constant String_Array := Split_Any (S, " #()", Keep_Empty => False);
      Result : Dig_Order;
      Color  : String (1 .. 6)       := Parts.Element (2);
   begin
      Result.Dir_1    := Parse_LRUD (Parts.Element (0));
      Result.Amount_1 := Long_Long_Integer'Value (Parts.Element (1));
      Result.Dir_2    := Parse_Hex_Direction (Color);
      Result.Amount_2 := Long_Long_Integer'Value ("16#" & Color (1 .. 5) & "#");
      return Result;
   end Parse_Dig_Order;

   package Vec2_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vec2);

   --  References:
   --
   --  https://web.archive.org/web/20100405070507/http://valis.cs.uiuc.edu/~sariel/research/CG/compgeom/msg00831.html
   --  https://en.wikipedia.org/wiki/Shoelace_formula
   function Polygon_Area (Vertices : Vec2_Vectors.Vector) return Long_Long_Integer is
      Area : Long_Long_Integer := 0;
      J    : Integer;

      --  The algorithm doesn't normally use this but the perimeter is
      --  considered part of the solution here.
      Perimeter : Long_Long_Integer := 0;
   begin
      for I in 1 .. Integer (Vertices.Length) loop
         --  I love arbitrary indexing
         J         :=  Vertices.First_Index + ((I + 1 - Vertices.First_Index) rem Vertices.Last_Index);
         Area      := Area + Long_Long_Integer (Vertices (I) (0) * Vertices (J) (1));
         Area      := Area - Long_Long_Integer (Vertices (I) (1) * Vertices (J) (0));
         Perimeter := Perimeter + Manhattan (Vertices (I), Vertices (J));
      end loop;
      --  I don't know why the +1 is necessary here.
      return 1 + ((Perimeter + (abs Area)) / 2);
   end Polygon_Area;

   --  locals
   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Plan  : Dig_Plan;

begin

   for Line of Lines loop
      Plan.Append (Parse_Dig_Order (Line));
   end loop;

   declare
      Pos_1, Pos_2     : Vec2 := (0, 0);
      Verts_1, Verts_2 : Vec2_Vectors.Vector;
   begin
      for Order of Plan loop
         Verts_1.Append (Pos_1);
         Pos_1 := Pos_1 + To_Vector (Order.Dir_1) * Order.Amount_1;

         Verts_2.Append (Pos_2);
         Pos_2 := Pos_2 + To_Vector (Order.Dir_2) * Order.Amount_2;
      end loop;

      Solution (Polygon_Area (Verts_1));
      Solution (Polygon_Area (Verts_2));
   end;

end Day18;
