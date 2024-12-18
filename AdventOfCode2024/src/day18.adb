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

   function Score_Distance (A, B : Path_State) return Boolean is
     (Manhattan (A.Pos, (6, 6)) < Manhattan (B.Pos, (6, 6)));

   package State_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Path_State, "<" => Score_Distance);

   type Score_Map is array (Integer range <>, Integer range <>) of Integer with
     Default_Component_Value => Integer'Last;

   Start : constant Vec2 := (0, 0);
   Goal  : constant Vec2 := (70, 70);

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

   function Find_Path (P : in out Pathfinder; Walls : Vec2_Sets.Set) return Find_Path_Result is
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

            if S.Pos = Goal then
               return Find_Path_Result'(Completed => True, Steps => S.Steps);
            end if;

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

      return Find_Path_Result'(Completed => False, Steps => S.Steps);
   end Find_Path;

   function Format_Vec2 (V : Vec2) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (V (X)'Image, Left) & ',' & Trim (V (Y)'Image, Left);
   end Format_Vec2;

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

   Solution (Find_Path (Pf, Walls).Steps);

   for I in 1_025 .. Points.Last_Index loop
      Walls.Include (Points (I));
      if not Find_Path (Pf, Walls).Completed then
         --  Flip output from row,col back to x,y
         Solution (Format_Vec2 (Vec2'(Points (I) (Y), Points (I) (X))));
         exit;
      end if;
   end loop;
end Day18;
