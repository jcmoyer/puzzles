with Advent.Integer_Parsers;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

--  All inputs for this puzzle have the same form:
--
--  code | instr | equivalent high level interpretations   |
--  -----+-------+-----------------------------------------+------------------------
--  2 4  | bst 4 | B = A mod 8   ==>  B = A & 7            | B assigned 3 bits every iteration
--  1 2  | bxl 2 | B = B xor 2   ==>  B = B ^ 2            | operand varies with input seed
--  7 5  | cdv 5 | C = A / 2**B  ==>  C = A / 1<<((A&7)^2) | C assigned every iteration
--  1 3  | bxl 3 | B = B xor 3   ==>  B = (A&7)^1          | operand varies with input seed
--  4 3  | bxc 3 | B = B xor C   ==>  B = B ^ C            | B mixed with C every iteration
--  5 5  | out 5 | out B mod 8   ==>  out B & 7            | output discards B bits beyond 3
--  0 3  | adv 3 | A = A / 8     ==>  A = A >> 3           | 3 bits shifted out of A each iteration
--  3 0  | jnz 0 | if A /= 0 then PC := 0                  | repeat program until A is 0
--
--  Since the program must output 16 digits (the length of our program), we can
--  conclude that A contains 46-48 bits because it shifts out groups of 3 bits
--  at a time until all bits have been shifted out. Each group of 3 bits
--  contributes to exactly one digit in the output. The only state that carries
--  on between iterations is `A`.
--
--  We can brute force the solution to part 2 based on this information:
--
--  1. Register A is a 48-bit integer composed of 16 groups of 3 bits.
--  2. Pick a value for the most significant group of 3 bits (we start at this
--     group so that the program outputs 16 digits - there's no point starting
--     lower.)
--  3. If the output digit for that group of 3 bits matches the corresponding
--     code in the program, move onto the next significant group of 3 bits and
--     repeat; otherwise, pick a different value until they match.
--  4. When all digits in the output and program match, we have a value for A.
--     It may not be the lowest though, so we need to check all values that
--     produce corresponding digits.

procedure Day17 is

   package AIP renames Advent.Integer_Parsers;

   type Int_Type is mod 2**64;

   type Int_3 is mod 8;

   type Int_3x16 is array (1 .. 16) of Int_3 with
     Pack => True, Size => 48;

   type Int_48 is range 0 .. 2**48 - 1;

   package Int_3_Vectors is new Ada.Containers.Vectors (Positive, Int_3);

   type Register_Name is (Reg_A, Reg_B, Reg_C);
   type Register_Array is array (Register_Name) of Int_Type;

   type Program is array (Int_Type range <>) of Int_3;

   type Cpu_Type is record
      R  : Register_Array := (others => 0);
      Pc : Int_Type       := 0;
      Out_Handler : access procedure (Value : Int_3) := null;
   end record;

   procedure Reset (Cpu : in out Cpu_Type) is
   begin
      for Reg of Cpu.R loop
         Reg := 0;
      end loop;
      Cpu.Pc := 0;
   end Reset;

   function Combo (Cpu : Cpu_Type; Operand : Int_3) return Int_Type is
   begin
      case Operand is
         when 0 .. 3 =>
            return Int_Type (Operand);

         when 4 =>
            return Cpu.R (Reg_A);

         when 5 =>
            return Cpu.R (Reg_B);

         when 6 =>
            return Cpu.R (Reg_C);

         when others =>
            raise Program_Error;

      end case;
   end Combo;

   procedure Run (Cpu : in out Cpu_Type; Prog : Program) is
   begin
      loop
         exit when Cpu.Pc not in Prog'Range;

         case Prog (Cpu.Pc) is
            when 0 =>
               Cpu.R (Reg_A) := Cpu.R (Reg_A) / (2**Natural (Combo (Cpu, Prog (Cpu.Pc + 1))));
               Cpu.Pc        := Cpu.Pc + 2;

            when 1 =>
               Cpu.R (Reg_B) := Cpu.R (Reg_B) xor Int_Type (Prog (Cpu.Pc + 1));
               Cpu.Pc        := Cpu.Pc + 2;

            when 2 =>
               Cpu.R (Reg_B) := Combo (Cpu, Prog (Cpu.Pc + 1)) rem 8;
               Cpu.Pc        := Cpu.Pc + 2;

            when 3 =>
               if Cpu.R (Reg_A) = 0 then
                  Cpu.Pc := Cpu.Pc + 2;
               else
                  --  Can only jump to 0..7, but program has more than 8 instructions.
                  Cpu.Pc := Int_Type (Prog (Cpu.Pc + 1));
               end if;

            when 4 =>
               Cpu.R (Reg_B) := Cpu.R (Reg_B) xor Cpu.R (Reg_C);
               Cpu.Pc        := Cpu.Pc + 2;

            when 5 =>
               if Cpu.Out_Handler /= null then
                  Cpu.Out_Handler (Int_3 (Combo (Cpu, Prog (Cpu.Pc + 1)) rem 8));
               end if;
               Cpu.Pc := Cpu.Pc + 2;

            when 6 =>
               Cpu.R (Reg_B) := Cpu.R (Reg_A) / (2**Natural (Combo (Cpu, Prog (Cpu.Pc + 1))));
               Cpu.Pc        := Cpu.Pc + 2;

            when 7 =>
               Cpu.R (Reg_C) := Cpu.R (Reg_A) / (2**Natural (Combo (Cpu, Prog (Cpu.Pc + 1))));
               Cpu.Pc        := Cpu.Pc + 2;

         end case;
      end loop;
   end Run;

   function To_String (Vec : Int_3_Vectors.Vector) return String is
      Result : String (1 .. Positive (2 * Vec.Length - 1));
      Write  : Positive := Result'First;
   begin
      for I in Vec.First_Index .. Vec.Last_Index loop
         Result (Write) := Character'Val (Character'Pos ('0') + Integer (Vec.Element (I)));
         Write          := Write + 1;
         if I < Vec.Last_Index then
            Result (Write) := ',';
            Write          := Write + 1;
         end if;
      end loop;
      return Result;
   end To_String;

   function Find_Min_Quine
     (Cpu    : in out Cpu_Type;
      Target :        Program;
      Output : in out Int_3_Vectors.Vector)
      return Int_Type
   is
      Search_Array : Int_3x16 := (others => 0);
      Search_Int   : Int_48;
      for Search_Int'Address use Search_Array'Address;
      pragma Import (Ada, Search_Int);

      Result : Int_Type := Int_Type'Last;

      procedure Search (Position : Natural) is
      begin
         if Position = 0 then
            Result := Int_Type'Min (Result, Int_Type (Search_Int));
            return;
         end if;

         for I in Int_3 loop
            Output.Clear;
            Search_Array (Position) := I;
            Reset (Cpu);
            Cpu.R (Reg_A) := Int_Type (Search_Int);
            Run (Cpu, Target);

            if Output.Length = Target'Length
              and then Output (Position) = Target (Target'First + Int_Type (Position - 1))
            then
               Search (Position - 1);
            end if;
         end loop;
      end Search;
   begin
      Search (Int_3x16'Last);
      return Result;
   end Find_Min_Quine;

   Output : Int_3_Vectors.Vector;

   procedure Output_Value (Value : Int_3) is
   begin
      Output.Append (Value);
   end Output_Value;

   Input_Error : exception;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Cpu    : Cpu_Type;
   Ints   : AIP.Array_Type (1 .. 16);
   N_Ints : Natural;

   Init_A : Int_Type;

begin
   N_Ints := AIP.Extract_Integers (Lines (1), Ints);
   if 1 /= N_Ints then
      raise Input_Error with "more than 1 integer on line 1";
   end if;
   Init_A := Int_Type (Ints (Ints'First));

   N_Ints := AIP.Extract_Integers (Lines (5), Ints);
   declare
      Prog : Program (0 .. Int_Type (N_Ints - 1));
   begin
      for I in 1 .. N_Ints loop
         Prog (Int_Type (I - 1)) := Int_3 (Ints (I));
      end loop;

      Cpu.Out_Handler := Output_Value'Access;

      Reset (Cpu);
      Cpu.R (Reg_A) := Init_A;
      Run (Cpu, Prog);
      Solution (To_String (Output));

      Solution (Long_Long_Integer (Find_Min_Quine (Cpu, Prog, Output)));
   end;
end Day17;
