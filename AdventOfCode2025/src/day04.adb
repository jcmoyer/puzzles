with Advent.Directions;          use Advent.Directions;
with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Containers.Linked_Deques;
with Ada.Command_Line;

procedure Day04 is
   type Deque_Index is mod 64;
   package Vec_Deques is new
     Advent.Containers.Linked_Deques (Vec2, Deque_Index);

   procedure Gather_Removable (Map : Char_Matrix; Q : in out Vec_Deques.Deque)
   is
   begin
      Q.Clear;
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            declare
               Adjacent : Integer := 0;
               Pos      : constant Vec2 := (I, J);
            begin
               if Element (Map, Pos) = '@' then
                  for D in Advent.Directions.Direction loop
                     if In_Bounds (Map, Pos + To_Vector (D))
                       and then Element (Map, Pos + To_Vector (D)) = '@'
                     then
                        Adjacent := Adjacent + 1;
                     end if;
                  end loop;

                  if Adjacent < 4 then
                     Q.Push_Back (Pos);
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Gather_Removable;

   procedure Remove_All (Map : in out Char_Matrix; Q : in out Vec_Deques.Deque)
   is
      Pos : Vec2;
   begin
      while not Q.Empty loop
         Q.Pop_Back (Pos);
         Map (Pos (X), Pos (Y)) := '.';
      end loop;
   end Remove_All;

   ----------------------------------------------------------------------------

   Map : Advent.IO.Char_Matrix :=
     Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

   Q  : Vec_Deques.Deque;
   P2 : Integer := 0;
begin
   Gather_Removable (Map, Q);
   Advent.IO.Solution (Q.Length);

   while Q.Length > 0 loop
      P2 := P2 + Q.Length;
      Remove_All (Map, Q);
      Gather_Removable (Map, Q);
   end loop;

   Advent.IO.Solution (P2);
end Day04;
