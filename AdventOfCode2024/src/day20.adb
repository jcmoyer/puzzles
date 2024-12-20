with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Containers.Hashed_Maps;

procedure Day20 is

   use type Ada.Containers.Count_Type;
   use type Ada.Containers.Hash_Type;

   M_Start, M_End : Vec2;

   type Cheat_Location is record
      C_Start, C_End : Vec2;
   end record;

   function Hash (L : Cheat_Location) return Ada.Containers.Hash_Type is
     (Hash (L.C_Start) xor Hash (L.C_End));

   package Cheat_Save_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Cheat_Location, Element_Type => Integer, Hash => Hash, Equivalent_Keys => "=");

   type Distance_Map is array (Integer range <>, Integer range <>) of Integer with
     Default_Component_Value => Integer'Last;

   type Path_State is record
      Pos   : Vec2;
      Steps : Integer;
   end record;

   package Path_State_Vectors is new Ada.Containers.Vectors (Positive, Path_State);

   procedure Build_Distance_Map (Map : Char_Matrix; Distances : in out Distance_Map) is
      Dfs : Path_State_Vectors.Vector;
      C   : Path_State;
      N   : Vec2;
   begin
      Dfs.Append ((Pos => M_End, Steps => 0));

      while Dfs.Length > 0 loop
         C := Dfs.Last_Element;
         Dfs.Delete_Last;

         Distances (C.Pos (X), C.Pos (Y)) := C.Steps;

         for D in Cardinal_Direction loop
            N := C.Pos + To_Vector (D);
            if Distances (N (X), N (Y)) = Integer'Last and then Map (N (X), N (Y)) = '.' then
               Dfs.Append ((Pos => N, Steps => C.Steps + 1));
            end if;
         end loop;
      end loop;
   end Build_Distance_Map;

   function Race
     (Map : Char_Matrix; Distances : Distance_Map; Cheat_Distance : Integer) return Integer
   is
      C : Vec2;
      N : Vec2;

      All_Cheats : Cheat_Save_Maps.Map;
      Sum        : Integer := 0;
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            for DI in -Cheat_Distance .. +Cheat_Distance loop
               for DJ in -Cheat_Distance .. +Cheat_Distance loop
                  if Distances (I, J) /= Integer'Last then
                     C := (I, J);
                     N := C + (DI, DJ);
                     if N (X) in Map'Range (1) and then N (Y) in Map'Range (2)
                       and then Manhattan (N, C) <= Cheat_Distance
                       and then Distances (C (X), C (Y)) > Distances (N (X), N (Y))
                        --  implied pathable; walls are Integer'Last

                     then
                        All_Cheats.Include
                          ((C_Start => C, C_End => N),
                           Distances (C (X), C (Y)) - Distances (N (X), N (Y)) - Manhattan (N, C));
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      for Cheat of All_Cheats loop
         if Cheat >= 100 then
            Sum := Sum + 1;
         end if;
      end loop;
      return Sum;
   end Race;

   Map       : Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   Distances : Distance_Map (Map'Range (1), Map'Range (2));

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

   Build_Distance_Map (Map, Distances);

   Solution (Race (Map, Distances, 2));
   Solution (Race (Map, Distances, 20));
end Day20;
