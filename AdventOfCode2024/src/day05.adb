with Advent.IO;         use Advent.IO;
with Advent.Strings;
with Advent.Integer_Parsers;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers;

procedure Day05 is

   package AIP renames Advent.Integer_Parsers;

   subtype Page_Number is Integer range 11 .. 99;

   --  Indexed as From, To.
   --  M(1,2) = True means 1 comes before 2
   type Dependency_Matrix is array (Page_Number, Page_Number) of Boolean;

   --  True if there are any edges coming from any node going to `To`
   function Any_In_Edges
     (D : Dependency_Matrix; To : Page_Number) return Boolean is
   begin
      for From in Page_Number loop
         if D (From, To) then
            return True;
         end if;
      end loop;
      return False;
   end Any_In_Edges;

   --  Filters `D` to only contain the page numbers in `A`
   function Reduce_Dependencies
     (D : Dependency_Matrix; A : AIP.Vector) return Dependency_Matrix
   is
      Result : Dependency_Matrix := D;
   begin
      for Row in Page_Number loop
         if not A.Contains (Row) then
            for Col in Page_Number loop
               Result (Row, Col) := False;
               Result (Col, Row) := False;
            end loop;
         end if;
      end loop;
      return Result;
   end Reduce_Dependencies;

   --  Gathers all the page numbers that have no prior dependencies
   procedure Gather_Roots
     (D : Dependency_Matrix; Filter : AIP.Vector; Roots : out AIP.Vector) is
   begin
      for To in Page_Number loop
         if not Any_In_Edges (D, To) and Filter.Contains (To) then
            Roots.Append (To);
         end if;
      end loop;
   end Gather_Roots;

   --  Gathers all the page numbers that are direct dependencies of `From`
   procedure Gather_Children
     (D : Dependency_Matrix; From : Page_Number; Children : in out AIP.Vector)
   is
   begin
      Children.Clear;
      for To in Page_Number loop
         if D (From, To) then
            Children.Append (To);
         end if;
      end loop;
   end Gather_Children;

   --  Topologically sorts `A` using dependency information in `D`.
   function Sort (A : AIP.Vector; D : Dependency_Matrix) return AIP.Vector is
      Roots    : AIP.Vector;
      Children : AIP.Vector;
      Current  : Page_Number;
      Result   : AIP.Vector;

      --  exclude dependencies involving page numbers outside this list
      Reduced_Dep : Dependency_Matrix := Reduce_Dependencies (D, A);

      use type Ada.Containers.Count_Type;
   begin
      Gather_Roots (Reduced_Dep, A, Roots);

      --  due to the nature of this problem, there can only be one correct sort
      --  which means there can only be one valid starting number
      if Roots.Length /= 1 then
         raise Program_Error with "more than 1 starting root!";
      end if;

      while Roots.Length > 0 loop
         Current := Roots.Last_Element;
         Result.Append (Current);
         Roots.Delete_Last;

         Gather_Children (Reduced_Dep, Current, Children);
         for To of Children loop
            Reduced_Dep (Current, To) := False;
            if not Any_In_Edges (Reduced_Dep, To) then
               Roots.Append (To);
            end if;
         end loop;
      end loop;

      return Result;
   end Sort;

   --  True if `A` is sorted according to the dependency information in `D`
   function Is_Sorted (A : AIP.Vector; D : Dependency_Matrix) return Boolean is
   begin
      for I in reverse A.First_Index .. A.Last_Index loop
         for J in reverse A.First_Index .. I - 1 loop
            if D (A.Element (I), A.Element (J)) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Sorted;

   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Sum_P1 : Integer := 0;
   Sum_P2 : Integer := 0;

   Ints : AIP.Vector;
   Dep  : Dependency_Matrix := (others => (others => False));

begin
   for Line of Lines loop
      if Index (Line, "|") > 0 then
         Ints := AIP.Extract_Integers (Line);
         Dep (Ints.Element (1), Ints.Element (2)) := True;
      elsif Index (Line, ",") > 0 then
         Ints := AIP.Extract_Integers (Line);
         if Is_Sorted (Ints, Dep) then
            Sum_P1 :=
              Sum_P1 + Ints ((Ints.Last_Index - Ints.First_Index) / 2 + 1);
         else
            Ints := Sort (Ints, Dep);
            Sum_P2 :=
              Sum_P2 + Ints ((Ints.Last_Index - Ints.First_Index) / 2 + 1);
         end if;
      end if;
   end loop;

   Solution (Sum_P1);
   Solution (Sum_P2);
end Day05;
