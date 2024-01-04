with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Advent.Long_Parsers;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

procedure Day09 is

   --  types
   subtype Int_Vec is Advent.Long_Parsers.Vector;

   use type Int_Vec;

   package Int_Vec_Vecs is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Int_Vec);

   subtype Int_Vec_Vec is Int_Vec_Vecs.Vector;

   --  sequences
   subtype Derived_Sequence is Int_Vec_Vec;

   function Base_First (S : Derived_Sequence) return Long_Long_Integer is
   begin
      return S.First_Element.First_Element;
   end Base_First;

   function Base_Last (S : Derived_Sequence) return Long_Long_Integer is
   begin
      return S.First_Element.Last_Element;
   end Base_Last;

   function Differentiate (S : Int_Vec) return Int_Vec is
      Result : Int_Vec;
   begin
      Result.Set_Length (S.Length - 1);
      for I in S.First_Index .. S.Last_Index - 1 loop
         Result (I) := S (I + 1) - S (I);
      end loop;
      return Result;
   end Differentiate;

   procedure Differentiate_All (S : in out Derived_Sequence) is
      I : Positive := S.First_Index;
   begin
      while not (for all X of S (I) => X = 0) loop
         declare
            --  Separate the differentiation from the next line because it
            --  raises a cursor tampering error. Not sure why, since I would
            --  expect Differentiate to be completely evaluated before Append.
            D : constant Int_Vec := Differentiate (S (I));
         begin
            S.Append (D);
         end;
         I := I + 1;
      end loop;
   end Differentiate_All;

   procedure Extrapolate (S : in out Derived_Sequence) is
   begin
      S.Reference (S.Last).Append (0);
      S.Reference (S.Last).Prepend (0);

      for I in reverse S.First_Index + 1 .. S.Last_Index loop
         S (I - 1).Append (S (I - 1).Last_Element + S (I).Last_Element);
         S (I - 1).Prepend (S (I - 1).First_Element - S (I).First_Element);
      end loop;
   end Extrapolate;

   function Sequence_From_Base (Xs : Int_Vec) return Derived_Sequence is
   begin
      return S : Derived_Sequence do
         S.Append (Xs);
         Differentiate_All (S);
         Extrapolate (S);
      end return;
   end Sequence_From_Base;

   --  locals
   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Sum1 : Long_Long_Integer := 0;
   Sum2 : Long_Long_Integer := 0;

begin

   for Line of Lines loop
      declare
         S : constant Derived_Sequence :=
           Sequence_From_Base (Advent.Long_Parsers.Extract_Integers (Line));
      begin
         Sum1 := Sum1 + Base_Last (S);
         Sum2 := Sum2 + Base_First (S);
      end;
   end loop;

   Solution (Sum1);
   Solution (Sum2);

end Day09;
