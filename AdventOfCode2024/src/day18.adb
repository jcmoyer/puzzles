with Advent.Integer_Parsers;
with Advent.IO;                  use Advent.IO;
with Advent.Strings;             use Advent.Strings;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Directions;          use Advent.Directions;
with Advent.Containers.Priority_Queues;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Strings;
with Ada.Strings.Fixed;
procedure Day18 is

   package AIP renames Advent.Integer_Parsers;

   Input_Error : exception;

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);

   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vec2, Hash => Hash, Equivalent_Elements => "=");

   type Path_State is record
      Pos   : Vec2;
      Steps : Integer;
   end record;

   Start : constant Vec2 := (0, 0);
   Goal  : constant Vec2 := (70, 70);

   function Score_Distance (A, B : Path_State) return Boolean is
     (Manhattan (A.Pos, Goal) < Manhattan (B.Pos, Goal));

   package State_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Path_State, "<" => Score_Distance);

   type Score_Map is array (Integer range <>, Integer range <>) of Integer with
     Default_Component_Value => Integer'Last;

   --  We're just going to simulate a bunch of paths so keep the structures
   --  alive between searches
   type Pathfinder is record
      Explore : State_Queues.Queue;
      Scores  : Score_Map (Start (X) .. Goal (X), Start (Y) .. Goal (Y));
   end record;

   type Find_Path_Result is record
      Completed : Boolean;
      Steps     : Integer;
   end record;

   type Path_Kind is (Any_Path, Shortest_Path);

   function Find_Path
     (P : in out Pathfinder; Walls : Vec2_Sets.Set; Kind : Path_Kind) return Find_Path_Result
   is
      S : Path_State;
      V : Vec2;
   begin
      P.Explore.Clear;
      P.Explore.Enqueue ((Pos => Start, Steps => 0));
      P.Scores := (others => (others => <>));

      while P.Explore.Length > 0 loop
         P.Explore.Dequeue (S);

         if S.Steps < P.Scores (S.Pos (X), S.Pos (Y)) then
            P.Scores (S.Pos (X), S.Pos (Y)) := S.Steps;

            exit when Kind = Any_Path and then S.Pos = Goal;

            for D in Cardinal_Direction loop
               V := S.Pos + To_Vector (D);
               if V (X) in Start (X) .. Goal (X) and then V (Y) in Start (Y) .. Goal (Y)
                 and then not Walls.Contains (V)
               then
                  P.Explore.Enqueue (Path_State'(Pos => V, Steps => S.Steps + 1));
               end if;
            end loop;
         end if;
      end loop;

      return
        Find_Path_Result'
          (Completed => P.Scores (Goal (X), Goal (Y)) /= Integer'Last,
           Steps     => P.Scores (Goal (X), Goal (Y)));
   end Find_Path;

   function Format_Vec2 (V : Vec2) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (V (X)'Image, Left) & ',' & Trim (V (Y)'Image, Left);
   end Format_Vec2;

   function Lower_Bound_Unpathable
     (Pf     : in out Pathfinder;
      Points :        Vec2_Vectors.Vector;
      Walls  : in out Vec2_Sets.Set)
      return Vec2
   is
      Low  : Integer := 1_024;
      High : Integer := Points.Last_Index;
      Mid  : Integer;
      Hit  : Natural := 0;
   begin
      while Low <= High loop
         Mid := Low + (High - Low) / 2;

         Walls.Clear;
         for I in 1 .. Mid loop
            Walls.Include (Points (I));
         end loop;

         if Find_Path (Pf, Walls, Any_Path).Completed then
            Low := Mid + 1;
         else
            Hit  := Mid;
            High := Mid - 1;
         end if;
      end loop;

      return Points (Hit);
   end Lower_Bound_Unpathable;

   Lines  : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Ints   : AIP.Array_Type (1 .. 2);
   Points : Vec2_Vectors.Vector;
   Pf     : Pathfinder;
   Walls  : Vec2_Sets.Set;

begin
   for Line of Lines loop
      if 2 /= AIP.Extract_Integers (Line, Ints) then
         raise Input_Error with "expected 2 integers in line";
      end if;
      --  Flip input from x,y to row,col
      Points.Append (Vec2'(Ints (2), Ints (1)));
   end loop;

   for I in 1 .. 1_024 loop
      Walls.Include (Points (I));
   end loop;

   Solution (Find_Path (Pf, Walls, Shortest_Path).Steps);

   declare
      Point : constant Vec2 := Lower_Bound_Unpathable (Pf, Points, Walls);
   begin
      --  Flip output from row,col back to x,y
      Solution (Format_Vec2 (Vec2'(Point (Y), Point (X))));
   end;
end Day18;
