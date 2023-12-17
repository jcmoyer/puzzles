with Advent;                     use Advent;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day17 is
   --  Digit matrix stores pre-converted characters so we don't have to keep
   --  converting characters to integers in the pathfinding loop.
   subtype Digit is Integer range 0 .. 9;

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
      Forward_Steps : Forward_Step_Count := 0;
      Heat_Loss     : Integer            := 0;
      Forward       : Direction;
      Position      : Vec2;
   end record;

   package Path_State_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Path_State);

   subtype Path_State_Vector is Path_State_Vectors.Vector;

   type Path_Score_Map is
     array
       (Integer range <>,
        Integer range <>,
        Forward_Step_Count range <>,
        Direction range <>) of Integer;

   function In_Bounds (Map : Digit_Matrix; Indices : Vec2) return Boolean is
     (Indices (0) in Map'Range (1) and then Indices (1) in Map'Range (2));

   function Element (Map : Digit_Matrix; Indices : Vec2) return Digit is
     (Map (Indices (0), Indices (1)));

   function Can_Forward_Normal (S : Path_State) return Boolean is (S.Forward_Steps < 3);
   function Can_Turn_Normal (S : Path_State) return Boolean is (True);
   function Can_Goal_Normal (S : Path_State) return Boolean is (True);

   function Can_Forward_Ultra (S : Path_State) return Boolean is (S.Forward_Steps < 10);
   function Can_Turn_Ultra (S : Path_State) return Boolean is (S.Forward_Steps >= 4);
   function Can_Goal_Ultra (S : Path_State) return Boolean is (S.Forward_Steps >= 4);

   generic
      with function Can_Forward (S : Path_State) return Boolean;
      with function Can_Turn (S : Path_State) return Boolean;
      with function Can_Goal (S : Path_State) return Boolean;
   function Find_Path (Map : Digit_Matrix) return Integer;

   function Find_Path (Map : Digit_Matrix) return Integer is
      Explore         : Path_State_Vector;
      Current         : Path_State;
      Score : Path_Score_Map (Map'Range (1), Map'Range (2), Forward_Step_Count, Direction) :=
        (others => (others => (others => (others => Integer'Last))));
      Goal            : constant Vec2 := (Map'Length (1), Map'Length (2));
      Best_Goal_Score : Integer := Integer'Last;
   begin
      Explore.Append
        (Path_State'(Forward_Steps => 0, Heat_Loss => 0, Forward => East, Position => (1, 1)));

      Explore.Append
        (Path_State'(Forward_Steps => 0, Heat_Loss => 0, Forward => South, Position => (1, 1)));

      while Explore.Length > 0 loop
         Current := Explore.Last_Element;
         Explore.Delete_Last;

         --  have we seen this state before at a lower score? if so, there's no
         --  point revisiting it
         if Score
             (Current.Position (0), Current.Position (1), Current.Forward_Steps, Current.Forward) >
           Current.Heat_Loss
           and then Current.Heat_Loss < Best_Goal_Score
         then
            --  otherwise, mark the new best score for this cell
            Score
              (Current.Position (0),
               Current.Position (1),
               Current.Forward_Steps,
               Current.Forward) :=
              Current.Heat_Loss;

            if Current.Position = Goal and then Can_Goal (Current) then
               Best_Goal_Score := Integer'Min (Best_Goal_Score, Current.Heat_Loss);
            else
               if Can_Forward (Current) then
                  declare
                     Forward_Position : Vec2 := Current.Position + To_Vector (Current.Forward);
                  begin
                     if In_Bounds (Map, Forward_Position) then
                        Explore.Append
                          (Path_State'
                             (Forward_Steps => Current.Forward_Steps + 1,
                              Heat_Loss     => Current.Heat_Loss + Element (Map, Forward_Position),
                              Forward       => Current.Forward,
                              Position      => Forward_Position));
                     end if;
                  end;
               end if;

               if Can_Turn (Current) then
                  declare
                     Left_Position  : constant Vec2 :=
                       Current.Position + To_Vector (Rotate_Left (Current.Forward));
                     Right_Position : constant Vec2 :=
                       Current.Position + To_Vector (Rotate_Right (Current.Forward));
                  begin
                     if In_Bounds (Map, Left_Position) then
                        Explore.Append
                          (Path_State'
                             (Forward_Steps => 1,
                              Heat_Loss     => Current.Heat_Loss + Element (Map, Left_Position),
                              Forward       => Rotate_Left (Current.Forward),
                              Position      => Left_Position));
                     end if;
                     if In_Bounds (Map, Right_Position) then
                        Explore.Append
                          (Path_State'
                             (Forward_Steps => 1,
                              Heat_Loss     => Current.Heat_Loss + Element (Map, Right_Position),
                              Forward       => Rotate_Right (Current.Forward),
                              Position      => Right_Position));
                     end if;
                  end;
               end if;
            end if;
         end if;
      end loop;

      return Best_Goal_Score;
   end Find_Path;

   function Find_Path_Normal is new Find_Path
     (Can_Forward => Can_Forward_Normal, Can_Turn => Can_Turn_Normal, Can_Goal => Can_Goal_Normal);

   function Find_Path_Ultra is new Find_Path
     (Can_Forward => Can_Forward_Ultra, Can_Turn => Can_Turn_Ultra, Can_Goal => Can_Goal_Ultra);

   Char_Map  : constant Char_Matrix  := Read_Tilemap (Ada.Command_Line.Argument (1));
   Digit_Map : constant Digit_Matrix := Chars_To_Digits (Char_Map);

begin

   Solution (Find_Path_Normal (Digit_Map));
   Solution (Find_Path_Ultra (Digit_Map));

end Day17;
