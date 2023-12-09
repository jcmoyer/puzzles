with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Advent.Parsers.Integers is

   function Extract_Integers (S : String) return Vector is
      I      : Positive := S'First;
      Result : Vector;
   begin
      while I <= S'Last loop
         if Is_Digit (S (I)) then
            declare
               Int_Start : constant Positive := I;
            begin
               while I <= S'Last and then Is_Digit (S (I)) loop
                  I := I + 1;
               end loop;
               Result.Append (Element_Type'Value (S (Int_Start .. I - 1)));
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
               Result.Append (Element_Type'Value (S (Int_Start .. I - 1)));
            end;
         else
            I := I + 1;
         end if;
      end loop;
      return Result;
   end Extract_Integers;

end Advent.Parsers.Integers;
