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

   --  Workflow steps, either an output or a condition.
   type Workflow_Step_Kind is (Wf_None, Wf_Condition, Wf_Output);

   type Workflow_Step
     (Step_Kind : Workflow_Step_Kind := Wf_None; Output_Kind : Workflow_Output_Kind := Out_None)
   is
   record
      case Step_Kind is
         when Wf_None =>
            null;
         when Wf_Condition =>
            Cond : Condition (Output_Kind);
         when Wf_Output =>
            Output : Workflow_Output (Output_Kind);
      end case;
   end record;

   package Workflow_Step_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Workflow_Step);

   --  A named sequence of steps.
   type Workflow is record
      Name  : Workflow_Name;
      Steps : Workflow_Step_Vectors.Vector;
   end record;

   package Workflow_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Workflow_Name,
      Element_Type    => Workflow,
      Hash            => Hash,
      Equivalent_Keys => Workflow_Names."=",
      "="             => "=");

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
                    (Step_Kind => Wf_Output, Output_Kind => Output.Kind, Output => Output));
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

   Total_Rating   : Integer := 0;
   Total_Rejected : Integer := 0;

   procedure Send_Part (U : Universe; P : Part) is
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

                           --  Solution ("Accepted");
                           Total_Rating := Total_Rating + Rating (P);
                           return;

                        when Out_Reject =>

                           --  Solution ("Rejected");
                           Total_Rejected := Total_Rejected + 1;
                           return;

                        when Out_Workflow =>

                           --  Solution
                           --    ("Move to wf " &
                           --     Workflow_Names.To_String (Step.Cond.Output.Out_Name));

                           Current := U.Flows.Find (Step.Cond.Output.Out_Name);

                           exit;

                        when Out_None =>

                           raise Program_Error with "output kind is invalid";

                     end case;

                  end if;

               when Wf_Output =>

                  case Step.Output.Kind is

                     when Out_Accept =>

                        --  Solution ("Accepted");
                        Total_Rating := Total_Rating + Rating (P);
                        return;

                     when Out_Reject =>

                        --  Solution ("Rejected");
                        Total_Rejected := Total_Rejected + 1;
                        return;

                     when Out_Workflow =>

                        --  Solution ("Move to wf " & Workflow_Names.To_String (Step.Output.Out_Name));

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
   end Send_Part;

   U : constant Universe := Load_Input (Ada.Command_Line.Argument (1));

   Flow_In : Workflow_Maps.Cursor;
begin

   Flow_In := U.Flows.Find (Workflow_Names.To_Bounded_String ("in"));
   Solution (Workflow_Names.To_String (Workflow_Maps.Element (Flow_In).Name));
   for Step of Workflow_Maps.Element (Flow_In).Steps loop
      Solution (Step.Step_Kind'Image & " " & Step.Output_Kind'Image);

   end loop;

   for P of U.Parts loop
      Send_Part (U, P);
   end loop;

   Solution (Total_Rating);

end Day19;
