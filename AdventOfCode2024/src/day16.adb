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

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);
   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Vec2, Hash => Hash, Equivalent_Elements => "=");

   subtype Junction_Id is Natural;

   package Junction_Id_Maps is new Ada.Containers.Hashed_Maps
     (Vec2, Junction_Id, Hash => Hash, Equivalent_Keys => "=");

   J_Start, J_End : Junction_Id;

   type Junction_Out_Edge is record
      Destination : Junction_Id;
      Steps       : Vec2_Vectors.Vector := Vec2_Vectors.Empty_Vector;
      Cost        : Natural;
      Out_Facing  : Cardinal_Direction;
   end record;

   type Junction_Out_Edges is array (Cardinal_Direction) of aliased Junction_Out_Edge;

   --  When we inspect the edges during the search, we use pointers to avoid
   --  copying the edge's tile vector. This has a measurable effect on
   --  performance; about ~10-20% runtime savings depending on optimization
   --  level on official inputs.
   type Junction_Out_Edge_Ptr is access constant Junction_Out_Edge;

   type Junction is record
      Id    : Junction_Id;
      Pos   : Vec2;
      Edges : Junction_Out_Edges;
   end record;

   type Path_Record is record
      Parent : Natural;
      Pos    : Junction_Id;
      Dir    : Cardinal_Direction;
      Score  : Integer;
   end record;

   package Path_Record_Vectors is new Ada.Containers.Vectors (Positive, Path_Record);

   package Junction_Vectors is new Ada.Containers.Vectors (Positive, Junction);
   Junctions    : Junction_Vectors.Vector;
   Junction_Ids : Junction_Id_Maps.Map;

   type Path_State is record
      Score : Integer := 0;
      Pos   : Junction_Id;
      Dir   : Cardinal_Direction;
      Path  : Natural;
   end record;

   type Search_Result is record
      Shortest_Path : Integer;
      Total_Tiles   : Integer;
   end record;

   function Order_Score (A, B : Path_State) return Boolean is (A.Score < B.Score);

   package PS_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Path_State, "<" => Order_Score);

   type Score_Map is array (Junction_Id range <>, Cardinal_Direction range <>) of Integer with
     Default_Component_Value => Integer'Last;

   function Cost (From, To : Junction_Id) return Integer is
   begin
      for D in Cardinal_Direction loop
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

      Records : Path_Record_Vectors.Vector;

      function Fork_Path (State : Path_State) return Positive is
      begin
         Records.Append
           ((Parent => State.Path, Pos => State.Pos, Dir => State.Dir, Score => State.Score));
         return Records.Last_Index;
      end Fork_Path;

      function Parent_Path (Path : Positive) return Natural is
      begin
         return Records (Path).Parent;
      end Parent_Path;

   begin
      Q.Enqueue (Path_State'(Pos => J_Start, Dir => East, Score => 0, Path => 0));

      while Q.Length > 0 loop
         Q.Dequeue (C);
         C.Path := Fork_Path (C);

         if C.Score <= B and then C.Pos = J_End then
            B := C.Score;
            declare
               P : Natural := C.Path;
            begin
               while Parent_Path (P) /= 0 loop
                  Gather_Steps (Records (P).Pos, Records (Parent_Path (P)).Pos, Best_Tiles);
                  P := Parent_Path (P);
               end loop;
            end;
         end if;

         if C.Pos /= J_End and then C.Score <= M (C.Pos, C.Dir) and then C.Score <= B then
            if C.Score <= M (C.Pos, C.Dir) then
               M (C.Pos, C.Dir) := C.Score;
            end if;

            declare
               New_State : Path_State;

               Edge : constant Junction_Out_Edge_Ptr := Junctions (C.Pos).Edges (C.Dir)'Access;
            begin
               if Edge.Destination /= 0 then
                  New_State.Pos   := Edge.Destination;
                  New_State.Dir   := Edge.Out_Facing;
                  New_State.Score := C.Score + Edge.Cost;
                  New_State.Path  := C.Path;
                  Q.Enqueue (New_State);
               end if;
            end;

            declare
               New_State : Path_State;

               Edge : constant Junction_Out_Edge_Ptr :=
                 Junctions (C.Pos).Edges (Rotate_CW_90 (C.Dir))'Access;
            begin
               if Edge.Destination /= 0 then
                  New_State.Pos   := Edge.Destination;
                  New_State.Dir   := Edge.Out_Facing;
                  New_State.Score := C.Score + Edge.Cost + 1_000;
                  New_State.Path  := C.Path;
                  Q.Enqueue (New_State);
               end if;
            end;

            declare
               New_State : Path_State;

               Edge : constant Junction_Out_Edge_Ptr :=
                 Junctions (C.Pos).Edges (Rotate_CCW_90 (C.Dir))'Access;
            begin
               if Edge.Destination /= 0 then
                  New_State.Pos   := Edge.Destination;
                  New_State.Dir   := Edge.Out_Facing;
                  New_State.Score := C.Score + Edge.Cost + 1_000;
                  New_State.Path  := C.Path;
                  Q.Enqueue (New_State);
               end if;
            end;
         end if;
      end loop;

      return (Shortest_Path => B, Total_Tiles => Integer (Best_Tiles.Length));

   end Search;

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

   procedure Connect_Junctions is
   begin
      for Id in Junctions.First_Index .. Junctions.Last_Index loop
         for D in Cardinal_Direction loop
            if Element (Map, Junctions (Id).Pos + To_Vector (D)) = '.' then
               Junctions (Id).Edges (D) :=
                 Find_Junction_Down_Path (Junctions (Id).Pos + To_Vector (D), D);
            end if;
         end loop;
      end loop;
   end Connect_Junctions;

   function Alloc_Junction (Where : Vec2) return Junction_Id is
   begin
      Junctions.Append
        (Junction'
           (Id    => Junctions.Last_Index + 1,
            Pos   => Where,
            Edges => (others => (Destination => 0, Steps => <>, Cost => 0, Out_Facing => <>))));
      Junction_Ids.Include (Where, Junctions.Last_Index);
      return Junctions.Last_Index;
   end Alloc_Junction;

   procedure Find_Junctions is
      N      : Integer := 0;
      Unused : Junction_Id;
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
                  Unused := Alloc_Junction ((I, J));
               end if;
            elsif Map (I, J) = 'S' then
               J_Start    := Alloc_Junction ((I, J));
               --  For the purposes of connecting junctions later, the start
               --  and end tiles need to become empty spaces.
               Map (I, J) := '.';
            elsif Map (I, J) = 'E' then
               J_End      := Alloc_Junction ((I, J));
               Map (I, J) := '.';
            end if;
         end loop;
      end loop;
   end Find_Junctions;

begin
   Find_Junctions;
   Connect_Junctions;

   Put_Line (Standard_Error, "Found " & Junctions.Length'Image & " junctions");

   declare
      Result : constant Search_Result := Search;
   begin
      Solution (Result.Shortest_Path);
      Solution (Result.Total_Tiles);
   end;

end Day16;
