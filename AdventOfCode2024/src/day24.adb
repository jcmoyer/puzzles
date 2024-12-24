with Advent.IO;         use Advent.IO;
with Advent.Strings;    use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces;        use Interfaces;

procedure Day24 is

   use type Ada.Containers.Count_Type;
   use type Ada.Containers.Hash_Type;

   type Op is (Op_And, Op_Xor, Op_Or);

   function Parse_Op (S : String) return Op is
   begin
      if S = "AND" then
         return Op_And;
      elsif S = "XOR" then
         return Op_Xor;
      elsif S = "OR" then
         return Op_Or;
      else
         raise Program_Error;
      end if;
   end Parse_Op;

   type Expr_Kind is (Ek_Undef, Ek_Input, Ek_Binop);

   type Input_Source is (In_X, In_Y);

   type Expr_Id is new Natural;

   type Expr (Kind : Expr_Kind := Ek_Undef) is record
      case Kind is
         when Ek_Input =>
            --  this is probably overengineered but might be useful for testing
            --  part 2 since it lets us easily change x/y externally
            In_Source : Input_Source;
            In_Bit    : Natural;
         when Ek_Binop =>
            Bin_Op    : Op;
            Bin_Left  : Expr_Id;
            Bin_Right : Expr_Id;
         when Ek_Undef =>
            null;
      end case;
   end record;

   package Expr_Vectors is new Ada.Containers.Vectors (Expr_Id, Expr);

   package Expr_Name_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Expr_Id, Hash => Ada.Strings.Hash, Equivalent_Keys => "=");

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Circuit_State is record
      X, Y, Z : Unsigned_64 := 0;
   end record;

   Exprs      : Expr_Vectors.Vector;
   Expr_Names : Expr_Name_Maps.Map;
   Z_Names    : String_Vectors.Vector;

   function Get_Or_Create (Name : String) return Expr_Id is
   begin
      if Expr_Names.Contains (Name) then
         return Expr_Names.Element (Name);
      end if;

      Exprs.Append ((Kind => Ek_Undef));
      Expr_Names.Insert (Name, Exprs.Last_Index);
      return Exprs.Last_Index;
   end Get_Or_Create;

   procedure Gather_Z_Names (Names : in out String_Vectors.Vector) is
      use type Expr_Name_Maps.Cursor;
      C : Expr_Name_Maps.Cursor := Expr_Names.First;
   begin
      Names.Clear;
      while C /= Expr_Name_Maps.No_Element loop
         if Starts_With (Expr_Name_Maps.Key (C), "z") then
            Names.Append (Expr_Name_Maps.Key (C));
         end if;
         Expr_Name_Maps.Next (C);
      end loop;
   end Gather_Z_Names;

   procedure Swap_Exprs (X, Y : String) is
   begin
      Exprs.Swap (Expr_Names.Element (X), Expr_Names.Element (Y));
   end Swap_Exprs;

   function Eval (S : in out Circuit_State; E : Expr_Id) return Boolean is
   begin
      case Exprs (E).Kind is
         when Ek_Input =>
            if Exprs (E).In_Source = In_X then
               return Boolean'Val (1 and Shift_Right (S.X, Exprs (E).In_Bit));
            else
               return Boolean'Val (1 and Shift_Right (S.Y, Exprs (E).In_Bit));
            end if;

         when Ek_Binop =>
            case Exprs (E).Bin_Op is
               when Op_And =>
                  return Eval (S, Exprs (E).Bin_Left) and Eval (S, Exprs (E).Bin_Right);
               when Op_Or =>
                  return Eval (S, Exprs (E).Bin_Left) or Eval (S, Exprs (E).Bin_Right);
               when Op_Xor =>
                  return Eval (S, Exprs (E).Bin_Left) xor Eval (S, Exprs (E).Bin_Right);
            end case;

         when Ek_Undef =>
            raise Program_Error;
      end case;
   end Eval;

   function Eval_Z (S : in out Circuit_State) return Unsigned_64 is
   begin
      S.Z := 0;
      for Name of Z_Names loop
         declare
            Bit    : constant Natural := Natural'Value (Name (Name'First + 1 .. Name'Last));
            Result : constant Boolean := Eval (S, Expr_Names.Element (Name));
         begin
            S.Z := S.Z or Shift_Left (Boolean'Pos (Result), Bit);
         end;
      end loop;
      return S.Z;
   end Eval_Z;

   procedure Load_Input (S : out Circuit_State) is
      Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
      Parts : String_Array;
   begin
      for Line of Lines loop
         if Index (Line, ":") /= 0 then
            Parts := Split (Line, ": ");
            declare
               In_Name : constant String  := Parts (1);
               In_Val  : constant String  := Parts (2);
               E       : constant Expr_Id := Get_Or_Create (In_Name);

               Source : constant Input_Source :=
                 (if In_Name (In_Name'First) = 'x' then In_X else In_Y);

               Bit : constant Natural :=
                 Natural'Value (In_Name (In_Name'First + 1 .. In_Name'Last));
            begin
               Exprs.Replace_Element (E, (Kind => Ek_Input, In_Source => Source, In_Bit => Bit));
               if Source = In_X then
                  S.X := S.X or Shift_Left (Unsigned_64'Value (In_Val), Bit);
               else
                  S.Y := S.Y or Shift_Left (Unsigned_64'Value (In_Val), Bit);
               end if;
            end;
         elsif Index (Line, "->") /= 0 then
            Parts := Split (Line, " -> ");
            declare
               Output : constant Expr_Id := Get_Or_Create (Parts (2));
            begin
               -- .Element to get around tamper check
               Parts := Split (Parts.Element (1), " ");
               declare
                  Lhs   : constant Expr_Id := Get_Or_Create (Parts (1));
                  Rhs   : constant Expr_Id := Get_Or_Create (Parts (3));
                  Binop : constant Op      := Parse_Op (Parts (2));
               begin
                  Exprs.Replace_Element
                    (Output,
                     (Kind => Ek_Binop, Bin_Op => Binop, Bin_Left => Lhs, Bin_Right => Rhs));
               end;
            end;
         end if;
      end loop;

      Gather_Z_Names (Z_Names);
   end Load_Input;

   Circuit : Circuit_State;

begin
   Load_Input (Circuit);

   Solution (Long_Long_Integer (Eval_Z (Circuit)));

   --  Solved by hand
   declare
      Corrected_Z : Unsigned_64 := 0;
   begin
      Swap_Exprs ("cdj", "z08");
      Swap_Exprs ("mrb", "z16");
      Swap_Exprs ("gfm", "z32");
      Swap_Exprs ("dhm", "qjd");
      Corrected_Z := Eval_Z (Circuit);
      if Circuit.X + Circuit.Y /= Corrected_Z then
         raise Program_Error with "invalid solution";
      end if;
      Solution ("cdj,dhm,gfm,mrb,qjd,z08,z16,z32");
   end;
end Day24;
