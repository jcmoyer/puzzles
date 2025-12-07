with Advent.Directions;          use Advent.Directions;
with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Containers.Linked_Deques;
with Ada.Command_Line;

procedure Day07 is

   type Index_Type is mod 64;
   package Vec_Deques is new Advent.Containers.Linked_Deques (Vec2, Index_Type);

   type Count_Map is array (Integer range <>, Integer range <>) of Long_Long_Integer
   with Default_Component_Value => -1;

   function Count_Splits (Map : Char_Matrix; Start : Vec2) return Long_Long_Integer is
      Q      : Vec_Deques.Deque;
      Cur    : Vec2 := Start;
      Splits : Long_Long_Integer := 0;

      --  Draw on a copy of the map so we can keep track of which splitters we
      --  already encountered. When we encounter a splitter we write a '*' into
      --  its position on this map.
      Seen : Char_Matrix := Map;
   begin
      Q.Push_Back (Cur);

      while not Q.Empty loop
         Q.Pop_Back (Cur);

         if In_Bounds (Map, Cur) then
            if Element (Map, Cur) = '^' and then Element (Seen, Cur) /= '*' then
               Q.Push_Back (Cur + To_Vector (West));
               Q.Push_Back (Cur + To_Vector (East));
               Splits := Splits + 1;
               Seen (Cur (X), Cur (Y)) := '*';
            elsif Element (Seen, Cur) /= '*' then
               Q.Push_Back (Cur + To_Vector (South));
            end if;
         end if;
      end loop;

      return Splits;
   end Count_Splits;

   function Count_Paths
     (Map : Char_Matrix; Start : Vec2; Cache : in out Count_Map) return Long_Long_Integer
   is
      Cur : Vec2 := Start;
      Sum : Long_Long_Integer := 1;
   begin
      if Cache (Start (X), Start (Y)) /= -1 then
         return Cache (Start (X), Start (Y));
      end if;

      while In_Bounds (Map, Cur) loop
         if Element (Map, Cur) = '^' then
            Sum :=
              Count_Paths (Map, Cur + To_Vector (West), Cache)
              + Count_Paths (Map, Cur + To_Vector (East), Cache);
            exit;
         else
            Cur := Cur + To_Vector (South);
         end if;
      end loop;

      Cache (Start (X), Start (Y)) := Sum;
      return Sum;
   end Count_Paths;

   ----------------------------------------------------------------------------

   Map   : Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   Cache : Count_Map (Map'Range (1), Map'Range (2));
   Start : Vec2;

begin
   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         if Map (I, J) = 'S' then
            Start := (I, J);
         end if;
      end loop;
   end loop;

   Solution (Count_Splits (Map, Start));
   Solution (Count_Paths (Map, Start, Cache));
end Day07;
