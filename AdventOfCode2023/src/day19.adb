with Advent;            use Advent;
with Advent.Parsers.Integers;
with Ada.Command_Line;
with Ada.Containers;    use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Bounded;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Bounded.Hash;

procedure Day19 is
   package Integer_Parsers is new Advent.Parsers.Integers (Element_Type => Integer);

   package Workflow_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 4);

   subtype Workflow_Name is Workflow_Names.Bounded_String;

   function Hash is new Ada.Strings.Bounded.Hash (Workflow_Names);

   use type Workflow_Name;

   type Part_Field is (Field_X, Field_M, Field_A, Field_S);
   type Part is array (Part_Field) of Integer;

   function Rating (P : Part) return Integer is
      Result : Integer := 0;
   begin
      for F in Part_Field loop
         Result := Result + P (F);
      end loop;
      return Result;
   end Rating;

   package Part_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Part);

   subtype Part_Vector is Part_Vectors.Vector;

   type Comparison is (Less_Than, Greater_Than);

   --  Workflow outputs, e.g. "A", "R", "qqz"
   type Workflow_Output_Kind is (Out_None, Out_Workflow, Out_Accept, Out_Reject);

   type Workflow_Output (Kind : Workflow_Output_Kind := Out_None) is record
      case Kind is
         when Out_Workflow =>
            Out_Name : Workflow_Name;
         when others =>
            null;
      end case;
   end record;

   --  Conditions, e.g. "x>2662"
   type Condition is record
      Left    : Part_Field;
      Right   : Integer;
      Compare : Comparison;
   end record;

   function Compare (Cond : Condition; P : Part) return Boolean is
   begin
      if Cond.Compare = Less_Than then
         return P (Cond.Left) < Cond.Right;
      else
         return P (Cond.Left) > Cond.Right;
      end if;
   end Compare;

   --  Inclusive ranges for each variable 'x', 'm', 'a', 's'
   type Valid_Ranges is record
      Min : Part := (others => 1);
      Max : Part := (others => 4_000);
   end record;

   --  Counts and returns the total accepted combinations for a range. If any
   --  variable has an impossible range, returns 0.
   function Count_Accepted (R : Valid_Ranges) return Long_Long_Integer is
      Accepted : Long_Long_Integer := 1;
   begin
      for F in Part_Field loop
         if R.Max (F) < R.Min (F) then
            return 0;
         else
            Accepted := Accepted * Long_Long_Integer (1 + R.Max (F) - R.Min (F));
         end if;
      end loop;
      return Accepted;
   end Count_Accepted;

   --  For debugging purposes.
   function Image (R : Valid_Ranges) return String is
   begin
      --!pp off
      return
        ("x "  & R.Min (Field_X)'Image & ".." & R.Max (Field_X)'Image &
         " m " & R.Min (Field_M)'Image & ".." & R.Max (Field_M)'Image &
         " a " & R.Min (Field_A)'Image & ".." & R.Max (Field_A)'Image &
         " s " & R.Min (Field_S)'Image & ".." & R.Max (Field_S)'Image);
      --!pp on
   end Image;

   --  Adjusts a range variable based on a condition. If Invert is True, the
   --  comparison operator will behave as if it accepts values for which it
   --  isn't normally true. In particular, this means that < will behave like
   --  >= and > will behave like <=.
   procedure Adjust_Range (R : in out Valid_Ranges; C : Condition; Invert : Boolean) is
   begin
      if not Invert then
         if C.Compare = Less_Than then
            R.Max (C.Left) := Integer'Min (R.Max (C.Left), C.Right - 1);
         elsif C.Compare = Greater_Than then
            R.Min (C.Left) := Integer'Max (R.Min (C.Left), C.Right + 1);
         end if;
      else
         if C.Compare = Less_Than then
            R.Min (C.Left) := Integer'Max (R.Min (C.Left), C.Right);
         elsif C.Compare = Greater_Than then
            R.Max (C.Left) := Integer'Min (R.Max (C.Left), C.Right);
         end if;
      end if;
   end Adjust_Range;

   --  Workflow steps, either an output like 'A' or "lzz" or a condition.
   type Workflow_Step_Kind is (Wf_None, Wf_Condition, Wf_Output);

   type Workflow_Step (Kind : Workflow_Step_Kind := Wf_None) is record
      Ranges : Valid_Ranges;
      Output : Workflow_Output;
      case Kind is
         when Wf_None =>
            null;
         when Wf_Condition =>
            Cond : Condition;
         when Wf_Output =>
            null;
      end case;
   end record;

   type Workflow_Step_Ptr is access all Workflow_Step;

   package Workflow_Step_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Workflow_Step);

   --  Workflow name + step index, which can be used to look up either a
   --  specific workflow or a step within a workflow.
   type Step_Ref is record
      Workflow_Id : Workflow_Name;
      Step_Id     : Positive;
   end record;

   --  A named sequence of steps.
   type Workflow is record
      Name   : Workflow_Name;
      Steps  : Workflow_Step_Vectors.Vector;
      --  When set, this field points to the workflow that flows to this one.
      Parent : Step_Ref;
   end record;

   type Workflow_Ptr is access all Workflow;

   package Workflow_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Workflow_Name,
      Element_Type    => Workflow,
      Hash            => Hash,
      Equivalent_Keys => Workflow_Names."=",
      "="             => "=");

   --  Everything from an input
   type Universe is record
      Flows : Workflow_Maps.Map;
      Parts : Part_Vector;
   end record;

   --  Associates each flow with its parent flow and step index within its
   --  parent flow. After this subprogram is called, Flow Parent fields can be
   --  used to traverse the tree upwards from any flow. This must be done after
   --  loading the entire input since it may be unordered.
   procedure Link_Parents (U : in out Universe) is
      use type Workflow_Maps.Cursor;

      Wf_Cursor : Workflow_Maps.Cursor := U.Flows.First;
   begin
      while Wf_Cursor /= Workflow_Maps.No_Element loop
         for Step_Id in
           Workflow_Maps.Element (Wf_Cursor).Steps.First_Index ..
             Workflow_Maps.Element (Wf_Cursor).Steps.Last_Index
         loop
            declare
               Step : constant Workflow_Step :=
                 Workflow_Maps.Element (Wf_Cursor).Steps.Element (Step_Id);
            begin
               if Step.Output.Kind = Out_Workflow then
                  U.Flows.Reference (Step.Output.Out_Name).Parent :=
                    (Workflow_Maps.Key (Wf_Cursor), Step_Id);
               end if;
            end;
         end loop;
         Workflow_Maps.Next (Wf_Cursor);
      end loop;
   end Link_Parents;

   type Parse_State is (Parse_Workflows, Parse_Parts);

   --  Parse errors
   Invalid_Field : exception;

   function Load_Input (Filename : String) return Universe is
      Lines  : constant String_Array := Read_All_Lines (Filename);
      Result : Universe;

      State : Parse_State := Parse_Workflows;

      --  e.g. 'x', 'm', 'a', 's'
      function Parse_Field (C : Character) return Part_Field is
      begin
         case C is
            when 'x' =>
               return Field_X;
            when 'm' =>
               return Field_M;
            when 'a' =>
               return Field_A;
            when 's' =>
               return Field_S;
            when others =>
               raise Invalid_Field;
         end case;
      end Parse_Field;

      function Parse_Field (S : String) return Part_Field is (Parse_Field (S (S'First)));

      --  e.g. "A", "R", "qqz"
      function Parse_Output (S : String) return Workflow_Output is
      begin
         if S = "A" then
            return Workflow_Output'(Kind => Out_Accept);
         elsif S = "R" then
            return Workflow_Output'(Kind => Out_Reject);
         else
            return
              Workflow_Output'
                (Kind => Out_Workflow, Out_Name => Workflow_Names.To_Bounded_String (S));
         end if;
      end Parse_Output;

      --  One step within braces. e.g. "x<2198:cr", "bn", "A", "R"
      function Parse_Workflow_Step (S : String) return Workflow_Step is
         Operator : constant Natural := Index (S, To_Set ("<>"));
      begin
         if Operator /= 0 then
            declare
               Operator_Kind : constant Comparison :=
                 (if S (Operator) = '<' then Less_Than else Greater_Than);

               Parts : constant String_Array := Split_Any (S, ":<>");

               Part_Kind  : constant Part_Field      := Parse_Field (Parts (0));
               Compare_To : constant Integer         := Integer'Value (Parts (1));
               Output     : constant Workflow_Output := Parse_Output (Parts (2));
            begin
               return
                 (Workflow_Step'
                    (Kind   => Wf_Condition,
                     Ranges => <>,
                     Output => Output,
                     Cond   =>
                       Condition'
                         (Left => Part_Kind, Right => Compare_To, Compare => Operator_Kind)));
            end;
         else
            declare
               Output : constant Workflow_Output := Parse_Output (S);
            begin
               return (Workflow_Step'(Kind => Wf_Output, Ranges => <>, Output => Output));
            end;
         end if;
      end Parse_Workflow_Step;

      --  e.g. "shv{x>3654:sbb,R}"
      function Parse_Workflow (Line : String) return Workflow is
         W     : Workflow;
         Parts : constant String_Array := Split_Any (Line, "{},", Keep_Empty => False);
      begin
         W.Name := Workflow_Names.To_Bounded_String (Parts (0));

         for I in 1 .. Parts.Last_Index loop
            W.Steps.Append (Parse_Workflow_Step (Parts (I)));
         end loop;

         return W;
      end Parse_Workflow;

      --  e.g. "{x=388,m=292,a=82,s=3715}"
      function Parse_Part (Line : String) return Part is
         P    : Part;
         Ints : constant Integer_Parsers.Vector := Integer_Parsers.Extract_Integers (Line);
      begin
         P (Field_X) := Ints.Element (Ints.First_Index + 0);
         P (Field_M) := Ints.Element (Ints.First_Index + 1);
         P (Field_A) := Ints.Element (Ints.First_Index + 2);
         P (Field_S) := Ints.Element (Ints.First_Index + 3);
         return P;
      end Parse_Part;

   begin

      for Line of Lines loop
         if Line'Length = 0 then
            State := Parse_Parts;
         elsif State = Parse_Workflows then
            declare
               W : constant Workflow := Parse_Workflow (Line);
            begin
               Result.Flows.Insert (W.Name, W);
            end;
         elsif State = Parse_Parts then
            Result.Parts.Append (Parse_Part (Line));
         end if;
      end loop;

      Link_Parents (Result);

      return Result;

   end Load_Input;

   --  Returns true if Step will accept P. Outputs always return True, and
   --  conditions return True when their expression returns True.
   function Accepts (Step : Workflow_Step; P : Part) return Boolean is
   begin
      if Step.Kind = Wf_Output then
         return True;
      end if;

      if Step.Kind = Wf_Condition and then Compare (Step.Cond, P) then
         return True;
      end if;

      return False;
   end Accepts;

   function Rate_Part (U : Universe; P : Part) return Integer is
      Current : Workflow_Maps.Cursor := U.Flows.Find (Workflow_Names.To_Bounded_String ("in"));
   begin
      loop
         for Step of Workflow_Maps.Element (Current).Steps loop
            if Accepts (Step, P) then
               case Step.Output.Kind is
                  when Out_Accept =>
                     return Rating (P);

                  when Out_Reject =>
                     return 0;

                  when Out_Workflow =>
                     Current := U.Flows.Find (Step.Output.Out_Name);
                     exit;

                  when Out_None =>
                     raise Program_Error with "output kind is invalid";
               end case;
            end if;
         end loop;
      end loop;
   end Rate_Part;

   --  Various accessors through Step_Ref
   function Get_Flow (U : Universe; Ref : Step_Ref) return Workflow is
   begin
      return U.Flows.Element (Ref.Workflow_Id);
   end Get_Flow;

   function Get_Flow_Ptr (U : in out Universe; Ref : Step_Ref) return Workflow_Ptr is
   begin
      return U.Flows.Reference (Ref.Workflow_Id).Element;
   end Get_Flow_Ptr;

   function Get_Step (U : Universe; Ref : Step_Ref) return Workflow_Step is
      Wf : constant Workflow := U.Flows.Element (Ref.Workflow_Id);
   begin
      return Wf.Steps.Element (Ref.Step_Id);
   end Get_Step;

   function Get_Step_Ptr (U : in out Universe; Ref : Step_Ref) return Workflow_Step_Ptr is
      Wf : constant Workflow_Ptr := U.Flows.Reference (Ref.Workflow_Id).Element;
   begin
      return Wf.Steps.Reference (Ref.Step_Id).Element;
   end Get_Step_Ptr;

   package Step_Ref_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Step_Ref);

   subtype Step_Ref_Vector is Step_Ref_Vectors.Vector;

   --  Returns a list of all steps that output directly to 'A'
   function Find_All_Accept_Steps (U : Universe) return Step_Ref_Vector is
      use type Workflow_Maps.Cursor;

      Wf_Cursor : Workflow_Maps.Cursor := U.Flows.First;
      Result    : Step_Ref_Vector;
   begin
      while Wf_Cursor /= Workflow_Maps.No_Element loop
         for Step_Id in
           Workflow_Maps.Element (Wf_Cursor).Steps.First_Index ..
             Workflow_Maps.Element (Wf_Cursor).Steps.Last_Index
         loop
            declare
               Step : constant Workflow_Step :=
                 Workflow_Maps.Element (Wf_Cursor).Steps.Element (Step_Id);
            begin
               if Step.Output.Kind = Out_Accept then
                  Result.Append
                    ((Workflow_Id => Workflow_Maps.Key (Wf_Cursor), Step_Id => Step_Id));
               end if;
            end;
         end loop;
         Workflow_Maps.Next (Wf_Cursor);
      end loop;

      return Result;
   end Find_All_Accept_Steps;

   --  Starts at S, then walks right-to-left until exhausting the steps left of
   --  S, then go up a level and repeat until we reach past the "in" flow.
   --
   --  At each conditional step, the valid XMAS values for S will be refined:
   --
   --    1. If we have just started or have just gone up a level, the condition
   --       must be true to reach S.
   --    2. If we're at a condition left of S or left of a node we just went up
   --       to, the condition must be false to reach S.
   procedure Compute_Bounds (U : in out Universe; S : Step_Ref) is
      --  Location we're currently examining.
      Current : Step_Ref := S;

      --  We will only adjust the ranges for S, so keep a pointer to it.
      Step_Ptr : constant Workflow_Step_Ptr := Get_Step_Ptr (U, S);

      --  Treat the very first iteration as having gone up a level so it uses
      --  the condition without inverting it.
      Went_Up : Boolean := True;
   begin
      loop
         declare
            Current_Ptr : constant Workflow_Step_Ptr := Get_Step_Ptr (U, Current);
         begin
            --  We only care about conditions.
            if Current_Ptr.Kind = Wf_Condition then
               Adjust_Range (Step_Ptr.Ranges, Current_Ptr.Cond, Invert => not Went_Up);
            end if;
         end;

         Went_Up := False;

         --  Now walk right-to-left. We use 1-based indices so 0 means we're
         --  before the start of this flow.
         if Integer (Current.Step_Id) - 1 = 0 then
            if Get_Flow_Ptr (U, Current).Parent.Workflow_Id = "" then
               --  No parent to climb to; we've reached the end.
               exit;
            else
               --  We go up to the parent flow at the step pointing to this
               --  flow.
               Current := Get_Flow_Ptr (U, Current).Parent;
               Went_Up := True;
            end if;
         else
            --  There are still steps in this flow; keep walking left.
            Current.Step_Id := Current.Step_Id - 1;
         end if;
      end loop;
   end Compute_Bounds;

   --  locals
   U : Universe := Load_Input (Ada.Command_Line.Argument (1));

begin

   --  Part 1
   declare
      Total_Rating : Integer := 0;
   begin
      for P of U.Parts loop
         Total_Rating := Total_Rating + Rate_Part (U, P);
      end loop;
      Solution (Total_Rating);
   end;

   --  Part 2
   declare
      Accepts        : constant Step_Ref_Vector := Find_All_Accept_Steps (U);
      Total_Accepted : Long_Long_Integer        := 0;
   begin
      for A of Accepts loop
         Compute_Bounds (U, A);
         Total_Accepted := Total_Accepted + Count_Accepted (Get_Step (U, A).Ranges);
      end loop;
      Solution (Total_Accepted);
   end;

end Day19;
