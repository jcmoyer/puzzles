with Advent.IO; use Advent.IO;
with Ada.Command_Line;

procedure Day06 is
   type Mode_Type is (Rowwise, Columnwise);

   type Operand_Array is array (Positive range <>) of Long_Long_Integer;

   --  Returns the matrix column of the space right of the numbers such that
   --  result + 1 would be the start of the next number column
   function Find_Right_Edge (Source : Char_Matrix; Col : Positive) return Integer is
      J : Positive := Col;
   begin
      loop
         exit when (for all I in 1 .. 4 => Source (I, J) = ' ');
         J := J + 1;
         exit when J > Source'Last (2);
      end loop;
      return J;
   end Find_Right_Edge;

   function Extract_Operands_Columnwise
     (Source : Char_Matrix; Col : Positive; Right_Edge : Positive) return Operand_Array
   is
      Result : Operand_Array (1 .. Right_Edge - Col) := (others => 0);
   begin
      for J in Col .. Right_Edge - 1 loop
         for I in 1 .. 4 loop
            if Source (I, J) /= ' ' then
               Result (J - Col + 1) :=
                 Result (J - Col + 1) * 10 + Long_Long_Integer'Value ("" & Source (I, J));
            end if;
         end loop;
      end loop;
      return Result;
   end Extract_Operands_Columnwise;

   function Extract_Operands_Rowwise
     (Source : Char_Matrix; Col : Positive; Right_Edge : Positive) return Operand_Array
   is
      Result : Operand_Array (1 .. 4) := (others => 0);
   begin
      for I in 1 .. 4 loop
         for J in Col .. Right_Edge - 1 loop
            if Source (I, J) /= ' ' then
               Result (I) := Result (I) * 10 + Long_Long_Integer'Value ("" & Source (I, J));
            end if;
         end loop;
      end loop;
      return Result;
   end Extract_Operands_Rowwise;

   function Extract_Operands
     (Mode : Mode_Type; Map : Char_Matrix; Col, Right_Edge : Positive) return Operand_Array is
   begin
      case Mode is
         when Rowwise    =>
            return Extract_Operands_Rowwise (Map, Col, Right_Edge);

         when Columnwise =>
            return Extract_Operands_Columnwise (Map, Col, Right_Edge);
      end case;
   end Extract_Operands;

   function Solve (Map : Char_Matrix; Mode : Mode_Type) return Long_Long_Integer is
      Col        : Integer := 1;
      Right_Edge : Integer;
      Next_Col   : Integer;
      Op_Result  : Long_Long_Integer;
      Result     : Long_Long_Integer := 0;
   begin
      while Col in Map'Range (2) loop
         Right_Edge := Find_Right_Edge (Map, Col);
         Next_Col := Right_Edge + 1;

         if Map (5, Col) = '*' then
            Op_Result := 1;
            for N of Extract_Operands (Mode, Map, Col, Right_Edge) loop
               Op_Result := Op_Result * N;
            end loop;
         elsif Map (5, Col) = '+' then
            Op_Result := 0;
            for N of Extract_Operands (Mode, Map, Col, Right_Edge) loop
               Op_Result := Op_Result + N;
            end loop;
         else
            raise Program_Error with "expected * or + under text column " & Col'Image;
         end if;

         Result := Result + Op_Result;
         Col := Next_Col;
      end loop;

      return Result;
   end Solve;

   Map : constant Char_Matrix := Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

begin
   for Mode in Mode_Type loop
      Solution (Solve (Map, Mode));
   end loop;
end Day06;
