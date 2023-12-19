with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Bounded;        use Ada.Strings.Bounded;
with Advent;                     use Advent;
with Ada.Command_Line;

procedure Day01 is
   package Digit_Strings is new Generic_Bounded_Length (Max => 5);
   subtype Digit_String is Digit_Strings.Bounded_String;
   use Digit_Strings;

   Digit_Words : constant array (1 .. 9) of Digit_String :=
     (1 => To_Bounded_String ("one"),
      2 => To_Bounded_String ("two"),
      3 => To_Bounded_String ("three"),
      4 => To_Bounded_String ("four"),
      5 => To_Bounded_String ("five"),
      6 => To_Bounded_String ("six"),
      7 => To_Bounded_String ("seven"),
      8 => To_Bounded_String ("eight"),
      9 => To_Bounded_String ("nine"));

   subtype Calibration_Result is Integer range 11 .. 99;

   function Calibration (S : String) return Calibration_Result is
      I : constant Integer := Index (S, Decimal_Digit_Set, S'First, Going => Forward);
      J : constant Integer := Index (S, Decimal_Digit_Set, S'Last, Going => Backward);
   begin
      return Calibration_Result'Value (S (I) & S (J));
   end Calibration;

   function Is_Digit_Word (S : String; Position : Positive; Value : out Integer) return Boolean is
      --  Number of characters left in S if we were to start searching from Position
      S_Left : constant Integer := S'Last - Position + 1;
   begin
      for R in Digit_Words'Range loop
         declare
            Word_Length : constant Integer := Length (Digit_Words (R));
         begin
            if Word_Length <= S_Left
              and then Digit_Words (R) = S (Position .. Position + Word_Length - 1)
            then
               Value := R;
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_Digit_Word;

   function Calibration_With_Words (S : String) return Calibration_Result is
      --  Zero is not a valid result, so we can use it as a "none" value.
      First_Value : Integer := 0;
      Last_Value  : Integer := 0;
      This_Value  : Integer := 0;

      procedure Handle_Value is
      begin
         if First_Value = 0 then
            First_Value := This_Value;
            Last_Value  := This_Value;
         else
            Last_Value := This_Value;
         end if;
      end Handle_Value;

   begin
      for I in S'Range loop
         if Is_Digit (S (I)) then
            This_Value := Integer'Value (S (I .. I));
            Handle_Value;
         elsif Is_Digit_Word (S, I, This_Value) then
            Handle_Value;
         end if;
      end loop;

      return First_Value * 10 + Last_Value;
   end Calibration_With_Words;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Sum1 : Integer := 0;
   Sum2 : Integer := 0;
begin
   for Line of Lines loop
      Sum1 := Sum1 + Calibration (Line);
      Sum2 := Sum2 + Calibration_With_Words (Line);
   end loop;

   Solution (Sum1);
   Solution (Sum2);
end Day01;
