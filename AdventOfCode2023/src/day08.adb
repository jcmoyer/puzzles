with Advent;            use Advent;
with Advent.IO;         use Advent.IO;
with Advent.Strings;    use Advent.Strings;
with Advent.Long_Integer_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers;    use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;

procedure Day08 is
   --  math
   package Integers renames Advent.Long_Integer_Math;

   --  directions
   type Direction is ('L', 'R');

   package Direction_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Direction);

   function Parse_Direction (C : Character) return Direction is
   begin
      case C is
         --!pp off
         when 'L'    => return 'L';
         when 'R'    => return 'R';
         when others => raise Program_Error;
         --!pp on
      end case;
   end Parse_Direction;

   --  node names
   subtype Node_Name is String (1 .. 3);

   package Name_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Node_Name);

   --  graphs
   type Node_Children is array (1 .. 2) of Node_Name;

   type Graph_Node is record
      Name     : Node_Name;
      Children : Node_Children;
   end record;

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Node_Name,
      Element_Type    => Graph_Node,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Graph is record
      Nodes : Node_Maps.Map;
   end record;

   procedure Insert (G : in out Graph; Name, Left, Right : Node_Name) is
   begin
      G.Nodes.Insert (Name, Graph_Node'(Name => Name, Children => (Left, Right)));
   end Insert;

   function Count_Distance_To_Any_Exit
     (G : Graph; Start_Name : Node_Name; Dirs : Direction_Vectors.Vector) return Long_Long_Integer
   is
      Current   : Node_Name         := Start_Name;
      Dir_Index : Positive          := 1;
      Steps     : Long_Long_Integer := 0;
   begin
      while Current (Current'Last) /= 'Z' loop
         if Dirs (Dir_Index) = 'L' then
            Current := G.Nodes (Current).Children (1);
         else
            Current := G.Nodes (Current).Children (2);
         end if;
         Dir_Index := 1 + (Dir_Index rem Positive (Dirs.Length));
         Steps     := Steps + 1;
      end loop;
      return Steps;
   end Count_Distance_To_Any_Exit;

   --  locals
   G          : Graph;
   Lines      : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Directions : Direction_Vectors.Vector;
   Starts     : Name_Vectors.Vector;
   Distances  : Integers.Vector;

begin
   for Line of Lines loop
      if Index (Line, "=") /= 0 then
         declare
            Parent_Children : constant String_Array :=
              Split_Any (Line, "(),= ", Keep_Empty => False);

            Parent_Name : constant Node_Name := Parent_Children.Element (1);
            Child_L     : constant Node_Name := Parent_Children.Element (2);
            Child_R     : constant Node_Name := Parent_Children.Element (3);
         begin
            Insert (G, Parent_Name, Child_L, Child_R);
            if Ends_With (Parent_Name, "A") then
               Starts.Append (Parent_Name);
            end if;
         end;
      elsif Line'Length > 0 then
         for C of Line loop
            Directions.Append (Parse_Direction (C));
         end loop;
      end if;
   end loop;

   Solution (Count_Distance_To_Any_Exit (G, "AAA", Directions));

   for S of Starts loop
      Distances.Append (Count_Distance_To_Any_Exit (G, S, Directions));
   end loop;

   Solution (Integers.Lcm (Distances));
end Day08;
