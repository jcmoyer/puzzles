with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Advent.Directions;          use Advent.Directions;
with Advent.Containers.Priority_Queues;
with Ada.Containers;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Day16 is

   Map : Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));

   M_Start, M_End : Vec2;

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);
   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Vec2, Hash => Hash, Equivalent_Elements => "=");

   subtype Junction_Id is Natural;

   package Junction_Id_Maps is new Ada.Containers.Hashed_Maps
     (Vec2, Junction_Id, Hash => Hash, Equivalent_Keys => "=");

   type Junction_Out_Edge is record
      Destination : Junction_Id;
      Steps       : Vec2_Vectors.Vector := Vec2_Vectors.Empty_Vector;
      Cost        : Natural;
      Out_Facing  : Cardinal_Direction;
   end record;

   type Junction_Out_Edges is array (Cardinal_Direction) of Junction_Out_Edge;

   type Junction is record
      Id    : Junction_Id;
      Pos   : Vec2;
      Edges : Junction_Out_Edges;
   end record;

   type Path_Record is record
      Pos   : Junction_Id;
      Dir   : Direction;
      Score : Integer;
   end record;

   package Path_Record_Vectors is new Ada.Containers.Vectors (Positive, Path_Record);

   package Junction_Id_Vectors is new Ada.Containers.Vectors (Positive, Junction_Id);

   package Junction_Vectors is new Ada.Containers.Vectors (Positive, Junction);
   Junctions    : Junction_Vectors.Vector;
   Junction_Ids : Junction_Id_Maps.Map;

   type Path_State is record
      Score : Integer := 0;
      Pos   : Junction_Id;
      Dir   : Cardinal_Direction;
      Path  : Path_Record_Vectors.Vector;
   end record;

   type Search_Result is record
      Shortest_Path : Integer;
      Total_Tiles   : Integer;
   end record;

   function Order_Score (A, B : Path_State) return Boolean is (A.Score < B.Score);

   package PS_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Path_State, "<" => Order_Score);

   type Score_Map is array (Junction_Id range <>, Direction range <>) of Integer with
     Default_Component_Value => Integer'Last;

   function Cost (From, To : Junction_Id) return Integer is
   begin
      for D in Direction loop
         if Junctions (From).Edges (D).Destination = To then
            return Junctions (From).Edges (D).Cost;
         end if;
      end loop;
      return -1;
   end Cost;

   procedure Gather_Steps (From, To : Junction_Id; Into : in out Vec2_Sets.Set) is
      --  It's possible there are multiple paths to the same destination
      --  junction from the source junction. We need to pick the shortest one.
      Min_Cost : Integer := Integer'Last;
      Min_Dir  : Cardinal_Direction;
   begin
      Into.Include (Junctions (From).Pos);
      Into.Include (Junctions (To).Pos);
      for D in Cardinal_Direction loop
         if Junctions (From).Edges (D).Destination = To
           and then Junctions (From).Edges (D).Cost < Min_Cost
         then
            Min_Cost := Junctions (From).Edges (D).Cost;
            Min_Dir  := D;
         end if;
      end loop;
      for S of Junctions (From).Edges (Min_Dir).Steps loop
         Into.Include (S);
      end loop;
   end Gather_Steps;

   procedure Print_Marked_Map (Marks : Vec2_Sets.Set) is
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            if Marks.Contains ((I, J)) then
               Put ('O');
            else
               Put (Map (I, J));
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Marked_Map;

   function Search return Search_Result is
      Q          : PS_Queues.Queue;
      C          : Path_State;
      B          : Integer := Integer'Last;
      M          : Score_Map (Junctions.First_Index .. Junctions.Last_Index, Cardinal_Direction);
      Best_Tiles : Vec2_Sets.Set;
   begin
      Q.Enqueue
        (Path_State'
           (Pos   => Junction_Ids.Element (M_Start),
            Dir   => East,
            Score => 0,
            Path  => Path_Record_Vectors.Empty_Vector));

      while Q.Length > 0 loop
         Q.Dequeue (C);
         C.Path.Append (Path_Record'(Pos => C.Pos, Dir => C.Dir, Score => C.Score));

         if C.Score <= B and then C.Pos = Junction_Ids.Element (M_End) then
            B := C.Score;
            for I in C.Path.First_Index .. C.Path.Last_Index - 1 loop
               Gather_Steps (C.Path (I).Pos, C.Path (I + 1).Pos, Best_Tiles);
            end loop;
         end if;

         if C.Pos /= Junction_Ids.Element (M_End) and then C.Score <= M (C.Pos, C.Dir)
           and then C.Score <= B
         then
            if C.Score <= M (C.Pos, C.Dir) then
               M (C.Pos, C.Dir) := C.Score;
            end if;

            if Junctions (C.Pos).Edges (C.Dir).Destination /= 0 then
               Q.Enqueue
                 ((Score => C.Score + Junctions (C.Pos).Edges (C.Dir).Cost,
                   Pos   => Junctions (C.Pos).Edges (C.Dir).Destination,
                   Dir   => Junctions (C.Pos).Edges (C.Dir).Out_Facing,
                   Path  => C.Path));
            end if;

            if Junctions (C.Pos).Edges (Rotate_CW_90 (C.Dir)).Destination /= 0 then
               Q.Enqueue
                 ((Score => C.Score + Junctions (C.Pos).Edges (Rotate_CW_90 (C.Dir)).Cost + 1_000,
                   Pos   => Junctions (C.Pos).Edges (Rotate_CW_90 (C.Dir)).Destination,
                   Dir   => Junctions (C.Pos).Edges (Rotate_CW_90 (C.Dir)).Out_Facing,
                   Path  => C.Path));
            end if;

            if Junctions (C.Pos).Edges (Rotate_CCW_90 (C.Dir)).Destination /= 0 then
               Q.Enqueue
                 ((Score => C.Score + Junctions (C.Pos).Edges (Rotate_CCW_90 (C.Dir)).Cost + 1_000,
                   Pos   => Junctions (C.Pos).Edges (Rotate_CCW_90 (C.Dir)).Destination,
                   Dir   => Junctions (C.Pos).Edges (Rotate_CCW_90 (C.Dir)).Out_Facing,
                   Path  => C.Path));
            end if;

         end if;
      end loop;

      return (Shortest_Path => B, Total_Tiles => Integer (Best_Tiles.Length));

   end Search;

   Junc, N : Integer := 0;

   function Find_Junction_Down_Path
     (Start : Vec2; Dir : Cardinal_Direction) return Junction_Out_Edge
   is
      Cost  : Integer            := 1;
      Pos   : Vec2               := Start;
      CDir  : Cardinal_Direction := Dir;
      Steps : Vec2_Vectors.Vector;
   begin
      loop
         Steps.Append (Pos);
         exit when Junction_Ids.Contains (Pos);

         Steps.Append (Pos);

         if Element (Map, Pos + To_Vector (CDir)) = '.' then
            Pos  := Pos + To_Vector (CDir);
            Cost := Cost + 1;
         elsif Element (Map, Pos + To_Vector (Rotate_CW_90 (CDir))) = '.' then
            Pos  := Pos + To_Vector (Rotate_CW_90 (CDir));
            CDir := Rotate_CW_90 (CDir);
            Cost := Cost + 1_001;
         elsif Element (Map, Pos + To_Vector (Rotate_CCW_90 (CDir))) = '.' then
            Pos  := Pos + To_Vector (Rotate_CCW_90 (CDir));
            CDir := Rotate_CCW_90 (CDir);
            Cost := Cost + 1_001;
         else
            return (Destination => 0, Steps => <>, Cost => 0, Out_Facing => CDir);
         end if;
      end loop;

      return
        (Destination => Junction_Ids.Element (Pos),
         Steps       => Steps,
         Cost        => Cost,
         Out_Facing  => CDir);
   end Find_Junction_Down_Path;

   procedure Connect is

   begin
      for Id in Junctions.First_Index .. Junctions.Last_Index loop
         for D in Cardinal_Direction loop
            if Element (Map, Junctions (Id).Pos + To_Vector (D)) = '.' then
               Junctions (Id).Edges (D) :=
                 Find_Junction_Down_Path (Junctions (Id).Pos + To_Vector (D), D);
            end if;
         end loop;
      end loop;
   end Connect;

begin

   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         if Map (I, J) = '.' then
            N := 0;
            for D in Cardinal_Direction loop
               if Element (Map, To_Vector (D) + (I, J)) = '.' then
                  N := N + 1;
               end if;
            end loop;
            if N >= 3 then
               Junctions.Append
                 (Junction'
                    (Id    => Junctions.Last_Index + 1,
                     Pos   => (I, J),
                     Edges =>
                       (others => (Destination => 0, Steps => <>, Cost => 0, Out_Facing => <>))));
               Junction_Ids.Include ((I, J), Junctions.Last_Index);
            end if;
         end if;
      end loop;
   end loop;

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

   Junctions.Append
     (Junction'
        (Id    => Junctions.Last_Index + 1,
         Pos   => M_Start,
         Edges => (others => (Destination => 0, Steps => <>, Cost => 0, Out_Facing => <>))));
   Junction_Ids.Include (M_Start, Junctions.Last_Index);

   Junctions.Append
     (Junction'
        (Id      => Junctions.Last_Index + 1,
         Pos     => M_End,
           Edges => (others => (Destination => 0, Steps => <>, Cost => 0, Out_Facing => <>))));
   Junction_Ids.Include (M_End, Junctions.Last_Index);

   Connect;

   declare
      Result : Search_Result := Search;
   begin
      Solution (Result.Shortest_Path);
      Solution (Result.Total_Tiles);
   end;

end Day16;
