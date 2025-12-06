with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Advent.Parsers.Integers is

   procedure Iterate_Integers (S : String; Callback : access procedure (Span : String)) is
      I : Positive := S'First;
   begin
      while I <= S'Last loop
         if Is_Digit (S (I)) then
            declare
               Int_Start : constant Positive := I;
            begin
               while I <= S'Last and then Is_Digit (S (I)) loop
                  I := I + 1;
               end loop;
               Callback (S (Int_Start .. I - 1));
            end;
         elsif S (I) = '-' and then I + 1 in S'Range and then Is_Digit (S (I + 1)) then
            --  handle negative numbers
            declare
               Int_Start : constant Positive := I;
            begin
               --  move cursor past sign to first digit
               I := I + 1;
               while I <= S'Last and then Is_Digit (S (I)) loop
                  I := I + 1;
               end loop;
               Callback (S (Int_Start .. I - 1));
            end;
         else
            I := I + 1;
         end if;
      end loop;
   end Iterate_Integers;

   function Extract_Positive_Integers (S : String) return Vector is
      Result : Vector;

      procedure Append_Result (Span : String) is
      begin
         if Span (Span'First) = '-' then
            Result.Append (Element_Type'Value (Span (Span'First + 1 .. Span'Last)));
         else
            Result.Append (Element_Type'Value (Span));
         end if;
      end Append_Result;
   begin
      Iterate_Integers (S, Append_Result'Access);
      return Result;
   end Extract_Positive_Integers;

   function Extract_Integers (S : String) return Vector is
      Result : Vector;

      procedure Append_Result (Span : String) is
      begin
         Result.Append (Element_Type'Value (Span));
      end Append_Result;
   begin
      Iterate_Integers (S, Append_Result'Access);
      return Result;
   end Extract_Integers;

   function Extract_Positive_Integers (S : String; Output : out Array_Type) return Natural is
      Read         : Natural := 0;
      Output_Index : Positive := Output'First;

      procedure Append_Result (Span : String) is
      begin
         if Span (Span'First) = '-' then
            Output (Output_Index) := Element_Type'Value (Span (Span'First + 1 .. Span'Last));
         else
            Output (Output_Index) := Element_Type'Value (Span);
         end if;
         Output_Index := Output_Index + 1;
         Read := Read + 1;
      end Append_Result;
   begin
      Iterate_Integers (S, Append_Result'Access);
      return Read;
   end Extract_Positive_Integers;

   function Extract_Integers (S : String; Output : out Array_Type) return Natural is
      Read         : Natural := 0;
      Output_Index : Positive := Output'First;

      procedure Append_Result (Span : String) is
      begin
         Output (Output_Index) := Element_Type'Value (Span);
         Output_Index := Output_Index + 1;
         Read := Read + 1;
      end Append_Result;
   begin
      Iterate_Integers (S, Append_Result'Access);
      return Read;
   end Extract_Integers;

end Advent.Parsers.Integers;
