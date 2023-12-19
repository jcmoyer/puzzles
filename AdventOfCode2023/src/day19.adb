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

   type Workflow_Output (Kind : Workflow_Output_Kind) is record
      case Kind is
         when Out_Workflow =>
            Out_Name : Workflow_Name;
         when others =>
            null;
      end case;
   end record;

   --  Conditions, e.g. "x>2662:A"
   type Condition (Kind : Workflow_Output_Kind := Out_Workflow) is record
      Left    : Part_Field;
      Right   : Integer;
      Compare : Comparison;
      Output  : Workflow_Output (Kind);
   end record;

   function Compare (Cond : Condition; P : Part) return Boolean is
   begin
      if Cond.Compare = Less_Than then
         return P (Cond.Left) < Cond.Right;
      else
         return P (Cond.Left) > Cond.Right;
      end if;
   end Compare;

   --  P2
   type Valid_Ranges is record
      Min : Part := (others => 1);
      Max : Part := (others => 4_000);
   end record;

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

   function Image (R : Valid_Ranges) return String is
   begin
      return
        ("x " & R.Min (Field_X)'Image & ".." & R.Max (Field_X)'Image & " m " &
         R.Min (Field_M)'Image & ".." & R.Max (Field_M)'Image & " a " & R.Min (Field_A)'Image &
         ".." & R.Max (Field_A)'Image & " s " & R.Min (Field_S)'Image & ".." &
         R.Max (Field_S)'Image);
   end Image;

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

   --  Workflow steps, either an output or a condition.
   type Workflow_Step_Kind is (Wf_None, Wf_Condition, Wf_Output);

   type Workflow_Step
     (Step_Kind : Workflow_Step_Kind := Wf_None; Output_Kind : Workflow_Output_Kind := Out_None)
   is
   record
      Ranges : Valid_Ranges;
      case Step_Kind is
         when Wf_None =>
            null;
         when Wf_Condition =>
            Cond : Condition (Output_Kind);
         when Wf_Output =>
            Output : Workflow_Output (Output_Kind);
      end case;
   end record;

   type Workflow_Step_Ptr is access all Workflow_Step;

   --  TODO: probably should move Workflow_Output out of Condition
   function Get_Output_Name (S : Workflow_Step) return Workflow_Name is
   begin
      case S.Step_Kind is
         when Wf_Condition =>
            return S.Cond.Output.Out_Name;
         when Wf_Output =>
            return S.Output.Out_Name;
         when others =>
            raise Program_Error
              with "attempted to get output name for uninitialized workflow step";
      end case;
   end Get_Output_Name;

   package Workflow_Step_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Workflow_Step);

   --  A named sequence of steps.
   type Workflow;
   type Workflow_Ptr is access all Workflow;

   --  Workflow name + step index
   type Step_Ref is record
      Workflow_Id : Workflow_Name;
      Step_Id     : Positive;
   end record;

   type Workflow is record
      Name   : Workflow_Name;
      Steps  : Workflow_Step_Vectors.Vector;
      Parent : Step_Ref;
   end record;

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

   type Parse_State is (Parse_Workflows, Parse_Parts);

   --  Parse errors
   Invalid_Field : exception;

   function Load_Input (Filename : String) return Universe is
      Lines  : constant String_Array := Read_All_Lines (Filename);
      Result : Universe;

      State : Parse_State := Parse_Workflows;

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

      function Parse_Workflow_Step (S : String) return Workflow_Step is
         Operator : constant Natural := Index (S, To_Set ("<>"));
      begin
         if Operator /= 0 then
            declare
               Operator_Kind : Comparison            :=
                 (if S (Operator) = '<' then Less_Than else Greater_Than);
               Parts         : constant String_Array := Split_Any (S, ":<>");
               Part_Kind     : Part_Field;
               Compare_To    : Integer               := Integer'Value (Parts (1));
               Output        : Workflow_Output       := Parse_Output (Parts (2));
            begin
               if Parts (0) = "x" then
                  Part_Kind := Field_X;
               elsif Parts (0) = "m" then
                  Part_Kind := Field_M;
               elsif Parts (0) = "a" then
                  Part_Kind := Field_A;
               elsif Parts (0) = "s" then
                  Part_Kind := Field_S;
               else
                  raise Invalid_Field;
               end if;

               return
                 (Workflow_Step'
                    (Step_Kind   => Wf_Condition,
                     Output_Kind => Output.Kind,
                     Ranges      => <>,
                     Cond        =>
                       Condition'
                         (Kind    => Output.Kind,
                          Left    => Part_Kind,
                          Right   => Compare_To,
                          Compare => Operator_Kind,
                          Output  => Output)));
            end;
         else
            declare
               Output : Workflow_Output := Parse_Output (S);
            begin
               return
                 (Workflow_Step'
                    (Step_Kind   => Wf_Output,
                     Output_Kind => Output.Kind,
                     Ranges      => <>,
                     Output      => Output));
            end;
         end if;
      end Parse_Workflow_Step;

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
               W : Workflow := Parse_Workflow (Line);
            begin
               Result.Flows.Insert (W.Name, W);
            end;
         elsif State = Parse_Parts then
            Result.Parts.Append (Parse_Part (Line));
         end if;
      end loop;

      return Result;

   end Load_Input;

   function Rate_Part (U : Universe; P : Part) return Integer is
      Result  : Integer              := 0;
      Current : Workflow_Maps.Cursor := U.Flows.Find (Workflow_Names.To_Bounded_String ("in"));
   begin
      loop
         for Step of Workflow_Maps.Element (Current).Steps loop
            case Step.Step_Kind is
               when Wf_Condition =>
                  if Compare (Step.Cond, P) then
                     --  send to this flow
                     case Step.Cond.Output.Kind is
                        when Out_Accept =>
                           return Rating (P);

                        when Out_Reject =>
                           return 0;

                        when Out_Workflow =>
                           Current := U.Flows.Find (Step.Cond.Output.Out_Name);
                           exit;

                        when Out_None =>
                           raise Program_Error with "output kind is invalid";
                     end case;
                  end if;

               when Wf_Output =>
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

               when Wf_None =>
                  raise Program_Error with "workflow has invalid step";
            end case;
         end loop;
      end loop;
   end Rate_Part;

   --  Various accessors through Step_Ref
   function Get_Flow (U : Universe; Ref : Step_Ref) return Workflow is
      Wf : Workflow := U.Flows.Element (Ref.Workflow_Id);
   begin
      return Wf;
   end Get_Flow;

   function Get_Flow_Ptr (U : in out Universe; Ref : Step_Ref) return Workflow_Ptr is
      Wf : Workflow_Ptr := U.Flows.Reference (Ref.Workflow_Id).Element;
   begin
      return Wf;
   end Get_Flow_Ptr;

   function Get_Step (U : Universe; Ref : Step_Ref) return Workflow_Step is
      Wf : Workflow := U.Flows.Element (Ref.Workflow_Id);
   begin
      return Wf.Steps.Element (Ref.Step_Id);
   end Get_Step;

   function Get_Step_Ptr (U : in out Universe; Ref : Step_Ref) return Workflow_Step_Ptr is
      Wf : Workflow_Ptr := U.Flows.Reference (Ref.Workflow_Id).Element;
   begin
      return Wf.Steps.Reference (Ref.Step_Id).Element;
   end Get_Step_Ptr;

   package Step_Ref_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Step_Ref);

   subtype Step_Ref_Vector is Step_Ref_Vectors.Vector;

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
               Step : Workflow_Step := Workflow_Maps.Element (Wf_Cursor).Steps.Element (Step_Id);
            begin
               if Step.Output_Kind = Out_Accept then
                  Result.Append
                    ((Workflow_Id => Workflow_Maps.Key (Wf_Cursor), Step_Id => Step_Id));
               end if;
            end;
         end loop;
         Workflow_Maps.Next (Wf_Cursor);
      end loop;

      return Result;
   end Find_All_Accept_Steps;

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
               Step : Workflow_Step := Workflow_Maps.Element (Wf_Cursor).Steps.Element (Step_Id);
            begin
               if Step.Output_Kind = Out_Workflow then

                  U.Flows.Reference (Get_Output_Name (Step)).Parent :=
                    (Workflow_Maps.Key (Wf_Cursor), Step_Id);
               end if;
            end;
         end loop;
         Workflow_Maps.Next (Wf_Cursor);
      end loop;
   end Link_Parents;

   procedure Compute_Bounds (U : in out Universe; S : Step_Ref) is
      Bounds            : Valid_Ranges;
      Current           : Step_Ref := S;
      --  may be temporarily invalid
      Unsafe_Next_Index : Integer  := 0;
      Went_Up           : Boolean  := False;
   begin
      loop
         declare
            Step_Ptr    : Workflow_Step_Ptr := Get_Step_Ptr (U, S);
            Current_Ptr : Workflow_Step_Ptr := Get_Step_Ptr (U, Current);

         begin
            if Current_Ptr.Step_Kind = Wf_Condition then
               if Current = S then
                  Adjust_Range (Step_Ptr.Ranges, Current_Ptr.Cond, Invert => False);
               else
                  Adjust_Range (Step_Ptr.Ranges, Current_Ptr.Cond, Invert => not Went_Up);
               end if;
            end if;
         end;

         Went_Up := False;

         --  now walk backwards and up
         Unsafe_Next_Index := Current.Step_Id - 1;
         if Unsafe_Next_Index = 0 then
            --  walked beyond first; go up
            if Get_Flow (U, Current).Parent.Workflow_Id = "" then
               exit;
            else
               Went_Up := True;
               Current := Get_Flow (U, Current).Parent;
            end if;
         else
            Current.Step_Id := Unsafe_Next_Index;
         end if;
      end loop;

   end Compute_Bounds;

   U       : Universe        := Load_Input (Ada.Command_Line.Argument (1));
   Accepts : Step_Ref_Vector := Find_All_Accept_Steps (U);
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
   Link_Parents (U);

   declare
      Total_Accepted : Long_Long_Integer := 0;
   begin
      for A of Accepts loop
         Compute_Bounds (U, A);
         Total_Accepted := Total_Accepted + Count_Accepted (Get_Step (U, A).Ranges);
      end loop;
      Solution (Total_Accepted);
   end;

end Day19;
