with Advent;                     use Advent;
with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Containers.Priority_Queues;
with Ada.Command_Line;

procedure Day17 is
   --  Digit matrix stores pre-converted characters so we don't have to keep
   --  converting characters to integers in the pathfinding loop.
   type Digit is range 0 .. 9;

   type Digit_Matrix is array (Integer range <>, Integer range <>) of Digit;

   function Char_To_Digit (C : Character) return Digit is
     (Digit (Character'Pos (C) - Character'Pos ('0')));

   function Chars_To_Digits (M : Char_Matrix) return Digit_Matrix is
      Result : Digit_Matrix (M'Range (1), M'Range (2)) := (others => (others => <>));
   begin
      for I in 1 .. Rows (M) loop
         for J in 1 .. Cols (M) loop
            Result (I, J) := Char_To_Digit (M (I, J));
         end loop;
      end loop;
      return Result;
   end Chars_To_Digits;

   type Forward_Step_Count is range 0 .. 10;

   type Path_State is record
      Forward_Steps : Forward_Step_Count;
      Heat_Loss     : Integer;
      Forward       : Direction;
      Position      : Vec2;
   end record;

   --  Should be roughly ~3.34mb for official inputs.
   type Path_Score_Map is
     array
       (Integer range <>,
        Integer range <>,
        Forward_Step_Count range <>,
        Direction range <>) of Integer;

   --  Since the score map will be pretty large, best not to allocate it on the
   --  stack.
   type Path_Score_Map_Ptr is access all Path_Score_Map;

   function In_Bounds (Map : Digit_Matrix; Indices : Vec2) return Boolean is
     (Indices (0) in Map'Range (1) and then Indices (1) in Map'Range (2));

   function Element (Map : Digit_Matrix; Indices : Vec2) return Digit is
     (Map (Indices (0), Indices (1)));

   --  Part 1 rules
   function Can_Forward_Normal (S : Path_State) return Boolean is (S.Forward_Steps < 3);
   function Can_Turn_Normal (S : Path_State) return Boolean is (True);
   function Can_Goal_Normal (S : Path_State) return Boolean is (True);

   --  Part 2 rules
   function Can_Forward_Ultra (S : Path_State) return Boolean is (S.Forward_Steps < 10);
   function Can_Turn_Ultra (S : Path_State) return Boolean is (S.Forward_Steps >= 4);
   function Can_Goal_Ultra (S : Path_State) return Boolean is (S.Forward_Steps >= 4);

   function Priority_Less (A, B : Path_State) return Boolean is (A.Heat_Loss < B.Heat_Loss);

   package Path_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Path_State, "<" => Priority_Less);

   generic
      with function Can_Forward (S : Path_State) return Boolean;
      with function Can_Turn (S : Path_State) return Boolean;
      with function Can_Goal (S : Path_State) return Boolean;
   function Find_Path (Map : Digit_Matrix) return Integer;

   function Find_Path (Map : Digit_Matrix) return Integer is
      Explore         : Path_Queues.Queue;
      Score           : constant Path_Score_Map_Ptr :=
        new Path_Score_Map (Map'Range (1), Map'Range (2), Forward_Step_Count, Direction);
      Goal            : constant Vec2               := (Map'Length (1), Map'Length (2));
      Best_Goal_Score : Integer                     := Integer'Last;

      function Is_Cullable (S : Path_State) return Boolean is
      begin
         --  have we seen this state before at a lower score? if so, there's no
         --  point revisiting it
         if S.Heat_Loss >= Score (S.Position (0), S.Position (1), S.Forward_Steps, S.Forward) then
            return True;
         end if;

         --  if we've already found a solution and it's better than the current
         --  score, there's no point continuing
         if S.Heat_Loss > Best_Goal_Score then
            return True;
         end if;
         return False;
      end Is_Cullable;

      procedure Enqueue (S : Path_State) is
      begin
         if Is_Cullable (S) then
            return;
         end if;

         Explore.Enqueue (S);
      end Enqueue;

      procedure Process_One is
         Current : Path_State;
      begin
         Explore.Dequeue (Current);

         if Is_Cullable (Current) then
            return;
         end if;

         --  otherwise, mark the new best score for this cell
         Score
           (Current.Position (0), Current.Position (1), Current.Forward_Steps, Current.Forward) :=
           Current.Heat_Loss;

         --  if we're at the goal, record the final score and return since
         --  there's nothing else to do
         if Current.Position = Goal and then Can_Goal (Current) then
            Best_Goal_Score := Integer'Min (Best_Goal_Score, Current.Heat_Loss);
            return;
         end if;

         --  enqueue adjacent states
         if Can_Forward (Current) then
            declare
               Forward_Position : constant Vec2 := Current.Position + To_Vector (Current.Forward);
            begin
               if In_Bounds (Map, Forward_Position) then
                  Enqueue
                    (Path_State'
                       (Forward_Steps => Current.Forward_Steps + 1,
                        Heat_Loss => Current.Heat_Loss + Integer (Element (Map, Forward_Position)),
                        Forward       => Current.Forward,
                        Position      => Forward_Position));
               end if;
            end;
         end if;

         if Can_Turn (Current) then
            declare
               Left_Position : constant Vec2 :=
                 Current.Position + To_Vector (Rotate_Left (Current.Forward));
            begin
               if In_Bounds (Map, Left_Position) then
                  Enqueue
                    (Path_State'
                       (Forward_Steps => 1,
                        Heat_Loss => Current.Heat_Loss + Integer (Element (Map, Left_Position)),
                        Forward       => Rotate_Left (Current.Forward),
                        Position      => Left_Position));
               end if;
            end;

            declare
               Right_Position : constant Vec2 :=
                 Current.Position + To_Vector (Rotate_Right (Current.Forward));
            begin
               if In_Bounds (Map, Right_Position) then
                  Enqueue
                    (Path_State'
                       (Forward_Steps => 1,
                        Heat_Loss => Current.Heat_Loss + Integer (Element (Map, Right_Position)),
                        Forward       => Rotate_Right (Current.Forward),
                        Position      => Right_Position));
               end if;
            end;
         end if;
      end Process_One;

   begin
      Score.all := (others => (others => (others => (others => Integer'Last))));

      Explore.Enqueue
        (Path_State'(Forward_Steps => 0, Heat_Loss => 0, Forward => East, Position => (1, 1)));

      Explore.Enqueue
        (Path_State'(Forward_Steps => 0, Heat_Loss => 0, Forward => South, Position => (1, 1)));

      while Explore.Length > 0 loop
         Process_One;
      end loop;

      return Best_Goal_Score;
   end Find_Path;

   function Find_Path_Normal is new Find_Path
     (Can_Forward => Can_Forward_Normal, Can_Turn => Can_Turn_Normal, Can_Goal => Can_Goal_Normal);

   function Find_Path_Ultra is new Find_Path
     (Can_Forward => Can_Forward_Ultra, Can_Turn => Can_Turn_Ultra, Can_Goal => Can_Goal_Ultra);

   --  locals
   Char_Map  : constant Char_Matrix  := Read_Tilemap (Ada.Command_Line.Argument (1));
   Digit_Map : constant Digit_Matrix := Chars_To_Digits (Char_Map);

begin

   Solution (Find_Path_Normal (Digit_Map));
   Solution (Find_Path_Ultra (Digit_Map));

end Day17;
