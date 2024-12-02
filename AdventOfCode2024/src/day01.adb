with Advent.IO;
with Advent.Strings;
with Advent.Integer_Parsers;
with Advent.Containers.Priority_Queues;
with Ada.Command_Line;
with Ada.Containers.Ordered_Maps;

procedure Day01 is
   package PQ is new
     Advent.Containers.Priority_Queues (Element_Type => Integer, "<" => "<");

   package Count_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Integer,
        Element_Type => Integer,
        "<"          => "<",
        "="          => "=");

   Input_Error : exception;

   --  Looks up a value in a map but falls back to `Default` if it doesn't
   --  exist.
   function Element_Or
     (M : Count_Maps.Map; Key, Default : Integer) return Integer is
   begin
      if M.Contains (Key) then
         return M.Element (Key);
      else
         return Default;
      end if;
   end Element_Or;

   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Occurrences       : Count_Maps.Map;
   Left_PQ, Right_PQ : PQ.Queue;
   Sum_P1, Sum_P2    : Integer := 0;

begin
   for Line of Lines loop
      declare
         Ints : Advent.Integer_Parsers.Array_Type (1 .. 2);
         L    : Integer renames Ints (1);
         R    : Integer renames Ints (2);
      begin
         if 2 /= Advent.Integer_Parsers.Extract_Integers (Line, Ints) then
            raise Input_Error with "number of columns in row /= 2";
         end if;
         Left_PQ.Enqueue (L);
         Right_PQ.Enqueue (R);
         Occurrences.Include (R, 1 + Element_Or (Occurrences, R, 0));
      end;
   end loop;

   while Left_PQ.Length > 0 loop
      declare
         L, R : Integer := 0;
      begin
         Left_PQ.Dequeue (L);
         Right_PQ.Dequeue (R);
         Sum_P1 := Sum_P1 + abs (L - R);
         Sum_P2 := Sum_P2 + L * Element_Or (Occurrences, L, 0);
      end;
   end loop;

   Advent.IO.Solution (Sum_P1);
   Advent.IO.Solution (Sum_P2);
end Day01;
