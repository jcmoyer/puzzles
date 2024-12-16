with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Advent.Directions;          use Advent.Directions;
with Advent.Containers.Priority_Queues;

procedure Day16 is

   Map : Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));

   M_Start, M_End : Vec2;

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);
   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Vec2, Hash => Hash, Equivalent_Elements => "=");

   type Path_State is record
      Score : Integer := 0;
      Pos   : Vec2;
      Dir   : Cardinal_Direction;
      Path  : Vec2_Vectors.Vector;
   end record;

   type Search_Result is record
      Shortest_Path : Integer;
      Total_Tiles   : Integer;
   end record;

   function Order_Score (A, B : Path_State) return Boolean is (A.Score < B.Score);

   package PS_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Path_State, "<" => Order_Score);

   type Score_Map is array (Integer range <>, Integer range <>, Direction range <>) of Integer;

   function Search return Search_Result is
      Q          : PS_Queues.Queue;
      C          : Path_State;
      B          : Integer                                                      := Integer'Last;
      M          : Score_Map (Map'Range (1), Map'Range (2), Cardinal_Direction) :=
        (others => (others => (others => Integer'Last)));
      Best_Tiles : Vec2_Sets.Set;
   begin
      Q.Enqueue
        (Path_State'(Pos => M_Start, Dir => East, Score => 0, Path => Vec2_Vectors.Empty_Vector));

      while Q.Length > 0 loop
         Q.Dequeue (C);
         C.Path.Append (C.Pos);

         if C.Pos = M_End then
            B := C.Score;
            for V of C.Path loop
               Best_Tiles.Include (V);
            end loop;
         end if;

         if C.Pos /= M_End and then C.Score <= M (C.Pos (X), C.Pos (Y), C.Dir)
           and then C.Score <= B
         then
            M (C.Pos (X), C.Pos (Y), C.Dir) := C.Score;

            if Element (Map, C.Pos + To_Vector (C.Dir)) = '.' then
               Q.Enqueue
                 (Path_State'
                    (Pos   => C.Pos + To_Vector (C.Dir),
                     Dir   => C.Dir,
                     Score => C.Score + 1,
                     Path  => C.Path));
            end if;

            if Element (Map, C.Pos + To_Vector (Rotate_CW_90 (C.Dir))) = '.' then
               Q.Enqueue
                 (Path_State'
                    (Pos   => C.Pos,
                     Dir   => Rotate_CW_90 (C.Dir),
                     Score => C.Score + 1_000,
                     Path  => C.Path));
            end if;

            if Element (Map, C.Pos + To_Vector (Rotate_CCW_90 (C.Dir))) = '.' then
               Q.Enqueue
                 (Path_State'
                    (Pos   => C.Pos,
                     Dir   => Rotate_CCW_90 (C.Dir),
                     Score => C.Score + 1_000,
                     Path  => C.Path));
            end if;

         end if;
      end loop;

      return (Shortest_Path => B, Total_Tiles => Integer (Best_Tiles.Length));

   end Search;

begin

   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         if Map (I, J) = 'S' then
            M_Start    := (I, J);
            Map (I, J) := '.';
         end if;
         if Map (I, J) = 'E' then
            M_End      := (I, J);
            Map (I, J) := '.';
         end if;
      end loop;
   end loop;

   declare
      Result : Search_Result := Search;
   begin
      Solution (Result.Shortest_Path);
      Solution (Result.Total_Tiles);
   end;

end Day16;
