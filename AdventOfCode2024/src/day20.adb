with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;

procedure Day20 is

   Input_Error : exception;

   type Distance_Map is array (Integer range <>, Integer range <>) of Integer with
     Default_Component_Value => Integer'Last;

   function Is_Walkable (C : Character) return Boolean is
     (C = '.' or else C = 'S' or else C = 'E');

   function Find_Exit (Map : Char_Matrix) return Vec2 is
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            if Map (I, J) = 'E' then
               return (I, J);
            end if;
         end loop;
      end loop;
      raise Input_Error with "no exit found";
   end Find_Exit;

   procedure Build_Distance_Map (Map : Char_Matrix; Distances : in out Distance_Map) is
      M_End : constant Vec2 := Find_Exit (Map);

      Current  : Vec2    := M_End;
      Steps    : Integer := 0;
      Neighbor : Vec2;
      Old      : Vec2;
   begin
      loop
         Distances (Current (X), Current (Y)) := Steps;

         --  Pick whichever adjacent walkable space we haven't visited. The
         --  official input only has straight lines so there should only ever
         --  be one. When we reach 'S' none of the adjacent tiles will meet
         --  these conditions.
         Old := Current;
         for D in Cardinal_Direction loop
            Neighbor := Old + To_Vector (D);
            if Distances (Neighbor (X), Neighbor (Y)) = Integer'Last
              and then Is_Walkable (Map (Neighbor (X), Neighbor (Y)))
            then
               Steps   := Steps + 1;
               Current := Neighbor;
            end if;
         end loop;

         exit when Current = Old;
      end loop;
   end Build_Distance_Map;

   function Race
     (Map : Char_Matrix; Distances : Distance_Map; Cheat_Distance : Integer) return Integer
   is
      Current    : Vec2;
      Neighbor   : Vec2;
      Time_Saves : Integer := 0;

      function End_Distance (Pos : Vec2) return Integer is (Distances (Pos (X), Pos (Y)));
      function In_Bounds (Pos : Vec2) return Boolean is
        (Pos (X) in Map'Range (1) and then Pos (Y) in Map'Range (2));

      --!pp off
      function Is_Time_Save (From, To : Vec2) return Boolean is
        (In_Bounds (To)
           and then Manhattan (From, To) <= Cheat_Distance
           and then End_Distance (From) > End_Distance (To)
           and then End_Distance (From) - End_Distance (To) - Manhattan (To, From) >= 100);
      --!pp on
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            Current := (I, J);
            if End_Distance (Current) /= Integer'Last then
               for DI in -Cheat_Distance .. +Cheat_Distance loop
                  for DJ in -Cheat_Distance .. +Cheat_Distance loop
                     Neighbor := Current + (DI, DJ);
                     if Is_Time_Save (Current, Neighbor) then
                        Time_Saves := Time_Saves + 1;
                     end if;
                  end loop;
               end loop;
            end if;
         end loop;
      end loop;
      return Time_Saves;
   end Race;

   Map       : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   Distances : Distance_Map (Map'Range (1), Map'Range (2));

begin
   Build_Distance_Map (Map, Distances);

   Solution (Race (Map, Distances, 2));
   Solution (Race (Map, Distances, 20));
end Day20;
