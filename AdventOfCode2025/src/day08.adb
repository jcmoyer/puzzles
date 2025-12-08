with Advent.IO;           use Advent.IO;
with Ada.Command_Line;
with Advent.Containers.String_Vectors;
with Ada.Containers.Generic_Array_Sort;
with Advent.Long_Parsers; use Advent.Long_Parsers;
with Advent.Vector_Math;

procedure Day08 is
   package Long_Vectors is new Advent.Vector_Math (Long_Long_Integer);
   use Long_Vectors;

   type Box_ID is new Integer;
   Null_Box_ID : constant Box_ID := 0;

   --  Box positions, indexed by box ID
   type Box_Array is array (Box_ID range 1 .. 1000) of Vec3;

   --  Dense set of box IDs
   type Box_Set is array (Box_ID range 1 .. 1000) of Boolean with Default_Component_Value => False;

   --  Connectivity of any two boxes
   type Box_Connectivity_Matrix is
     array (Box_ID range 1 .. 1000, Box_ID range 1 .. 1000) of Boolean
   with Default_Component_Value => False;

   --  Distance between any two boxes
   type Box_Distance_Matrix is
     array (Box_ID range 1 .. 1000, Box_ID range 1 .. 1000) of Long_Long_Integer
   with Default_Component_Value => Long_Long_Integer'Last;

   type Circuit_Sizes is array (Positive range <>) of Natural with Default_Component_Value => 0;

   type Box_Pair is record
      First, Second : Box_ID;
   end record;

   Null_Pair : constant Box_Pair := (First => Null_Box_ID, Second => Null_Box_ID);

   function Sqdist (A, B : Vec3) return Long_Long_Integer is
      Sum_Diff_Sq : Long_Long_Integer := 0;
   begin
      for Axis in Axis3 loop
         Sum_Diff_Sq := Sum_Diff_Sq + (A (Axis) - B (Axis))**2;
      end loop;
      return Sum_Diff_Sq;
   end Sqdist;

   procedure Calc_Distances (Boxes : Box_Array; Distances : in out Box_Distance_Matrix) is
      Dist : Long_Long_Integer;
   begin
      for I in Boxes'First .. Boxes'Last loop
         for J in I + 1 .. Boxes'Last loop
            Dist := Sqdist (Boxes (I), Boxes (J));
            Distances (I, J) := Dist;
            Distances (J, I) := Dist;
         end loop;
      end loop;
   end Calc_Distances;

   function Find_Shortest_Unconnected
     (Boxes : Box_Array; Distances : Box_Distance_Matrix; Conn : Box_Connectivity_Matrix)
      return Box_Pair
   is
      Best      : Long_Long_Integer := Long_Long_Integer'Last;
      Best_Pair : Box_Pair := Null_Pair;
   begin
      for I in 1 .. Boxes'Last loop
         for J in I + 1 .. Boxes'Last loop
            if not Conn (I, J) and then Distances (I, J) < Best then
               Best := Distances (I, J);
               Best_Pair := (First => I, Second => J);
            end if;
         end loop;
      end loop;

      return Best_Pair;
   end Find_Shortest_Unconnected;

   function Find_Largest_Circuits (Conn : Box_Connectivity_Matrix) return Circuit_Sizes is
      procedure Sort_Descending is new
        Ada.Containers.Generic_Array_Sort
          (Index_Type   => Positive,
           Element_Type => Integer,
           Array_Type   => Circuit_Sizes,
           "<"          => ">");

      Sizes   : Circuit_Sizes (1 .. Integer (Box_Array'Last)) := (others => 0);
      Size_ID : Integer := 1;
      Seen    : Box_Set := (others => False);

      function Count_Component_Size (ID : Box_ID) return Integer is
         Size : Integer := 1;
      begin
         Seen (ID) := True;
         for I in 1 .. Box_Array'Last loop
            if not Seen (I) and then Conn (ID, I) then
               Size := Size + Count_Component_Size (I);
            end if;
         end loop;
         return Size;
      end Count_Component_Size;
   begin
      for I in 1 .. Box_Array'Last loop
         if not Seen (I) then
            Sizes (Size_ID) := Count_Component_Size (I);
            Size_ID := Size_ID + 1;
         end if;
      end loop;

      Sort_Descending (Sizes);
      return Sizes;
   end Find_Largest_Circuits;

   ----------------------------------------------------------------------------

   Lines : constant Advent.Containers.String_Vectors.Vector :=
     Read_All_Lines (Ada.Command_Line.Argument (1));

   Ints        : Array_Type (1 .. 3);
   N_Ints      : Integer;
   Last_Box_ID : Box_ID := 0;
   Boxes       : Box_Array;
   Connected   : Box_Connectivity_Matrix;
   Distances   : Box_Distance_Matrix;
   Pair        : Box_Pair;

begin
   for Line of Lines loop
      N_Ints := Extract_Integers (Line, Ints);
      if N_Ints /= 3 then
         raise Program_Error with "expected 3 ints per line";
      end if;

      Last_Box_ID := Last_Box_ID + 1;
      Boxes (Last_Box_ID) := (Ints (1), Ints (2), Ints (3));
   end loop;

   Calc_Distances (Boxes, Distances);

   for I in 1 .. 1000 loop
      Pair := Find_Shortest_Unconnected (Boxes, Distances, Connected);
      Connected (Pair.First, Pair.Second) := True;
      Connected (Pair.Second, Pair.First) := True;
   end loop;

   declare
      Sizes : constant Circuit_Sizes := Find_Largest_Circuits (Connected);
   begin
      Solution (Sizes (Sizes'First) * Sizes (Sizes'First + 1) * Sizes (Sizes'First + 2));
   end;

   loop
      Pair := Find_Shortest_Unconnected (Boxes, Distances, Connected);
      if Pair = Null_Pair then
         raise Program_Error with "no solution for input";
      end if;
      Connected (Pair.First, Pair.Second) := True;
      Connected (Pair.Second, Pair.First) := True;

      declare
         Sizes : constant Circuit_Sizes := Find_Largest_Circuits (Connected);
      begin
         if Sizes (Sizes'First) = Natural (Last_Box_ID) then
            Solution (Boxes (Pair.First) (X) * Boxes (Pair.Second) (X));
            exit;
         end if;
      end;
   end loop;
end Day08;
