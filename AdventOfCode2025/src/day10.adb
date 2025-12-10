with Advent.IO; use Advent.IO;
with Ada.Command_Line;
with Advent.Strings;
with Advent.Containers.Priority_Queues;

procedure Day10 is
   Parse_Error : exception;

   --  <0 was intended to be a cull state during search, but it's not relevant
   --  for part 1
   type Joltage_Type is range -1 .. 512;
   for Joltage_Type'Size use 16;

   --  the largest arrays in the input seem to be length 10
   type Small_Index is range 0 .. 254;
   for Small_Index'Size use 8;

   type Small_Count is range 0 .. 255;
   for Small_Count'Size use 8;

   type Light_Status_Array is array (Small_Index range <>) of Boolean;
   type Index_Array is array (Small_Index range <>) of Small_Index;
   type Joltage_Array is array (Small_Index range <>) of Joltage_Type;

   type Button_Spec is record
      Indices   : Index_Array (0 .. 15) := (others => 0);
      N_Indices : Small_Count := 0;
   end record;

   type Button_Array is array (Small_Index range <>) of Button_Spec;

   type Light_Spec is record
      Lights     : Light_Status_Array (0 .. 15) := (others => False);
      N_Lights   : Small_Count := 0;
      Buttons    : Button_Array (0 .. 15) := (others => <>);
      N_Buttons  : Small_Count := 0;
      Joltages   : Joltage_Array (0 .. 15) := (others => 0);
      N_Joltages : Small_Count := 0;
   end record;

   type Light_State is record
      Lights  : Light_Status_Array (0 .. 15);
      Presses : Natural;
   end record;

   function Less_By_Presses (A, B : Light_State) return Boolean
   is (A.Presses < B.Presses);

   package Light_Queues is new
     Advent.Containers.Priority_Queues (Element_Type => Light_State, "<" => Less_By_Presses);

   function Press
     (Spec : Light_Spec; State : Light_State; Button_ID : Small_Index) return Light_State
   is
      Result : Light_State := State;
   begin
      for I in 0 .. Spec.Buttons (Button_ID).N_Indices - 1 loop
         Result.Lights (Spec.Buttons (Button_ID).Indices (Small_Index (I))) :=
           not Result.Lights (Spec.Buttons (Button_ID).Indices (Small_Index (I)));
      end loop;
      Result.Presses := Result.Presses + 1;
      return Result;
   end Press;

   procedure Parse_Indicators (Line : String; Pos : in out Positive; Spec : in out Light_Spec) is
   begin
      loop
         case Line (Pos) is
            when '.'    =>
               Spec.Lights (Small_Index (Spec.N_Lights)) := False;
               Spec.N_Lights := Spec.N_Lights + 1;

            when '#'    =>
               Spec.Lights (Small_Index (Spec.N_Lights)) := True;
               Spec.N_Lights := Spec.N_Lights + 1;

            when '['    =>
               null;

            when ']'    =>
               exit;

            when others =>
               raise Parse_Error
                 with "Parse_Indicators: unexpected character " & Line (Pos) & " at " & Pos'Image;
         end case;
         Pos := Pos + 1;
      end loop;
      --  move past ']'
      Pos := Pos + 1;
   end Parse_Indicators;

   procedure Parse_Button (Line : String; Pos : in out Positive; Spec : in out Light_Spec) is
      Int_Start : Integer := -1;
      Button    : Button_Spec := (N_Indices => 0, others => <>);
   begin
      loop
         case Line (Pos) is
            when '0' .. '9' =>
               if Int_Start = -1 then
                  Int_Start := Pos;
               end if;

            when '('        =>
               null;

            when ',' | ')'  =>
               Button.Indices (Small_Index (Button.N_Indices)) :=
                 Small_Index'Value (Line (Int_Start .. Pos - 1));
               Button.N_Indices := Button.N_Indices + 1;
               Int_Start := -1;
               exit when Line (Pos) = ')';

            when others     =>
               raise Parse_Error
                 with "Parse_Button: unexpected character " & Line (Pos) & " at " & Pos'Image;
         end case;
         Pos := Pos + 1;
      end loop;
      --  move past ')'
      Pos := Pos + 1;
      Spec.Buttons (Small_Index (Spec.N_Buttons)) := Button;
      Spec.N_Buttons := Spec.N_Buttons + 1;
   end Parse_Button;

   procedure Parse_Joltages (Line : String; Pos : in out Positive; Spec : in out Light_Spec) is
      Int_Start : Integer := -1;
   begin
      loop
         case Line (Pos) is
            when '0' .. '9' =>
               if Int_Start = -1 then
                  Int_Start := Pos;
               end if;

            when '{'        =>
               null;

            when ',' | '}'  =>
               Spec.Joltages (Small_Index (Spec.N_Joltages)) :=
                 Joltage_Type'Value (Line (Int_Start .. Pos - 1));
               Spec.N_Joltages := Spec.N_Joltages + 1;
               Int_Start := -1;
               exit when Line (Pos) = '}';

            when others     =>
               raise Parse_Error
                 with "Parse_Joltages: unexpected character " & Line (Pos) & " at " & Pos'Image;
         end case;
         Pos := Pos + 1;
      end loop;
      --  move past '}'
      Pos := Pos + 1;
   end Parse_Joltages;

   function Parse_Light_Spec (Line : String) return Light_Spec is
      Result : Light_Spec;
      Pos    : Integer := Line'First;
   begin
      while Pos in Line'Range loop
         case Line (Pos) is
            when '['    =>
               Parse_Indicators (Line, Pos, Result);

            when ' '    =>
               Pos := Pos + 1;

            when '('    =>
               Parse_Button (Line, Pos, Result);

            when '{'    =>
               Parse_Joltages (Line, Pos, Result);

            when others =>
               raise Parse_Error
                 with "Parse_Light_Spec: unexpected character " & Line (Pos) & " at " & Pos'Image;

         end case;
      end loop;
      return Result;
   end Parse_Light_Spec;

   function Find_Fewest_Presses (Spec : Light_Spec) return Natural is
      Q             : Light_Queues.Queue;
      Current_State : Light_State;
   begin
      Q.Enqueue ((Lights => Spec.Lights, Presses => 0));

      while Q.Length > 0 loop
         Q.Dequeue (Current_State);

         if (for all L of Current_State.Lights => L = False) then
            return Current_State.Presses;
         end if;

         for Button_ID in 0 .. Small_Index (Spec.N_Buttons - 1) loop
            Q.Enqueue (Press (Spec, Current_State, Button_ID));
         end loop;
      end loop;

      raise Program_Error with "unreachable or bad input";
   end Find_Fewest_Presses;

   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Light : Light_Spec;
   P1    : Natural := 0;

begin
   for Line of Lines loop
      Light := Parse_Light_Spec (Line);
      P1 := P1 + Find_Fewest_Presses (Light);
   end loop;

   Solution (P1);
   --  part 2 solved by z3:
   --  https://gist.github.com/jcmoyer/5b5147dbc9117fe9679181ab157199ec
   Solution (Natural (20083));
end Day10;
