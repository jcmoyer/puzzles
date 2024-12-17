with Advent.Integer_Parsers;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

--  Part 2 solved with z3: https://gist.github.com/jcmoyer/4147a2dab1ec0a31c8c8404e5059c0b1

procedure Day17 is

   package AIP renames Advent.Integer_Parsers;

   type Int_Type is mod 2**64;
   type Tiny_Int is mod 8;

   package Tiny_Int_Vectors is new Ada.Containers.Vectors (Positive, Tiny_Int);

   type Register_Name is (Reg_A, Reg_B, Reg_C);
   type Register_Array is array (Register_Name) of Int_Type;

   type Program is array (Int_Type range <>) of Tiny_Int;

   type Cpu_Type is record
      R  : Register_Array := (others => 0);
      Pc : Int_Type       := 0;
      Out_Handler : access procedure (Value : Tiny_Int) := null;
   end record;

   procedure Reset (Cpu : in out Cpu_Type) is
   begin
      for Reg of Cpu.R loop
         Reg := 0;
      end loop;
      Cpu.Pc := 0;
   end Reset;

   function Combo (Cpu : Cpu_Type; Operand : Tiny_Int) return Int_Type is
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
                  Cpu.Out_Handler (Tiny_Int (Combo (Cpu, Prog (Cpu.Pc + 1)) rem 8));
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

   Output : Tiny_Int_Vectors.Vector;

   procedure Put_Value (Value : Tiny_Int) is
   begin
      Output.Append (Value);
   end Put_Value;

   function To_String (Vec : Tiny_Int_Vectors.Vector) return String is
      Result : String (1 .. Positive (2 * Output.Length - 1));
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

   Input_Error : exception;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Cpu    : Cpu_Type;
   Ints   : AIP.Array_Type (1 .. 32);
   N_Ints : Natural;

   Init_A : Int_Type;

   Known_Part_2 : constant := 37_221_334_433_268;

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
         Prog (Int_Type (I - 1)) := Tiny_Int (Ints (I));
      end loop;

      Cpu.Out_Handler := Put_Value'Access;

      Reset (Cpu);
      Cpu.R (Reg_A) := Init_A;
      Run (Cpu, Prog);
      Solution (To_String (Output));

      Output.Clear;

      Reset (Cpu);
      Cpu.R (Reg_A) := Known_Part_2;
      Run (Cpu, Prog);

      if
        (for all I in Prog'Range =>
           Prog (I) = Output (Output.First_Index + Natural (I - Prog'First)))
      then
         Solution (Long_Long_Integer (Known_Part_2));
      else
         raise Program_Error with "incorrect solution for part 2";
      end if;
   end;
end Day17;
