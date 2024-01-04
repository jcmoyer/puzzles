with Advent;            use Advent;
with Advent.IO;         use Advent.IO;
with Advent.Strings;    use Advent.Strings;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Command_Line;

procedure Day02 is
   type Color_Name is (Red, Green, Blue);

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   TotalP1 : Integer := 0;
   TotalP2 : Integer := 0;
begin
   --  example input:
   --  Game 2: 1 green, 7 red; 1 green, 9 red, 3 blue; 4 blue, 5 red
   for Line of Lines loop
      declare
         --  <game-info> ": " <rounds>
         Game_Rounds : constant String_Array := Split (Line, ": ");
         --  (<number> <color> ", "?)+ "; " | $
         Rounds      : constant String_Array := Split (Game_Rounds (1), "; ");
         --  "Game " <game-id>
         Game_Info   : constant String_Array := Split (Game_Rounds (0), " ");
         Game_Id     : constant Integer      := Integer'Value (Game_Info (1));

         --  game state
         Is_Game_Valid : Boolean                       := True;
         Max_Colors    : array (Color_Name) of Integer := (others => 0);
      begin
         for R of Rounds loop
            declare
               --  (<number> <color> ", "?)*
               Pairs : constant String_Array         := Split (R, ", ");
               Sum   : array (Color_Name) of Integer := (others => 0);
            begin
               for P of Pairs loop
                  declare
                     Number_Color : constant String_Array := Split (P, " ");
                     Number : constant Integer := Integer'Value (Trim (Number_Color (0), Both));
                     Color_Str    : constant String       := Trim (Number_Color (1), Both);
                     Color        : constant Color_Name   := Color_Name'Value (Color_Str);
                  begin
                     Sum (Color)        := Sum (Color) + Number;
                     Max_Colors (Color) := Integer'Max (Max_Colors (Color), Number);
                  end;
               end loop;
               if Sum (Red) > 12 or Sum (Green) > 13 or Sum (Blue) > 14 then
                  Is_Game_Valid := False;
               end if;
            end;
         end loop;

         if Is_Game_Valid then
            TotalP1 := TotalP1 + Game_Id;
         end if;

         declare
            Game_Power : constant Integer :=
              Max_Colors (Red) * Max_Colors (Green) * Max_Colors (Blue);
         begin
            TotalP2 := TotalP2 + Game_Power;
         end;
      end;
   end loop;

   Solution (TotalP1);
   Solution (TotalP2);
end Day02;
