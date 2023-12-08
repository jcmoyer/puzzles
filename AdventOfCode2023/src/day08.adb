with Advent;            use Advent;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers;    use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day08 is
   type Direction is ('L', 'R');

   package Direction_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Direction);

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Long_Long_Integer);

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

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   type Node_Name is new String (1 .. 3);

   package Name_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Node_Name);

   type Node_Children is array (1 .. 2) of Node_Name;

   type Graph_Node is record
      Name     : Node_Name;
      Children : Node_Children;
   end record;

   Directions : Direction_Vectors.Vector;

   function Hash (N : Node_Name) return Hash_Type is
   begin
      --  idk lol maybe I'll find a real hash function somewhere
      return Character'Pos (N (1)) * 37 + Character'Pos (N (2)) * 17 + Character'Pos (N (3)) * 31;
   end Hash;

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Node_Name, Element_Type => Graph_Node, Hash => Hash, Equivalent_Keys => "=");

   type Graph is record
      Nodes : Node_Maps.Map;
   end record;

   procedure Insert (G : in out Graph; Name, Left, Right : Node_Name) is
   begin
      G.Nodes.Insert (Name, Graph_Node'(Name => Name, Children => (Left, Right)));
   end Insert;

   function Count_Distance
     (G                     : in out Graph;
      Start_Name, Last_Name :        Node_Name;
      Dirs                  :        Direction_Vectors.Vector)
      return Long_Long_Integer
   is
      Current   : Node_Name         := Start_Name;
      Dir_Index : Positive          := 1;
      Steps     : Long_Long_Integer := 0;
   begin
      while Current /= Last_Name loop
         if Dirs (Dir_Index) = 'L' then
            Current := G.Nodes (Current).Children (1);
         else
            Current := G.Nodes (Current).Children (2);
         end if;
         Dir_Index := 1 + (Dir_Index rem Positive (Dirs.Length));
         Steps     := Steps + 1;
         --  proper thing to do would be to check if the node is reachable but
         --  this heuristic works for my input since paths are ~20k steps at most.
         --  TODO: clean this up at some point
         if Steps = 100_000 then
            exit;
         end if;
      end loop;
      return Steps;
   end Count_Distance;

   function Gcd (A, B : Long_Long_Integer) return Long_Long_Integer is
      M : Long_Long_Integer := A;
      N : Long_Long_Integer := B;
      T : Long_Long_Integer;
   begin
      while N /= 0 loop
         T := M;
         M := N;
         N := T rem N;
      end loop;
      return M;
   end Gcd;

   function Lcm (A, B : Long_Long_Integer) return Long_Long_Integer is
   begin
      if A = 0 or else B = 0 then
         return 0;
      end if;
      return abs (A) * (abs (B) / Gcd (A, B));
   end Lcm;

   function Lcm (Xs : Integer_Vectors.Vector) return Long_Long_Integer with
     Pre => Xs.Length > 0
   is
      R : Long_Long_Integer := Xs (1);
   begin
      if Xs.Length = 1 then
         return R;
      end if;

      for I in Xs.First_Index + 1 .. Xs.Last_Index loop
         R := Lcm (R, Xs (I));
      end loop;

      return R;
   end Lcm;

   G : Graph;

   Starts    : Name_Vectors.Vector;
   Ends      : Name_Vectors.Vector;
   Distances : Integer_Vectors.Vector;

begin
   for Line of Lines loop
      if Index (Line, "=") /= 0 then
         declare
            Left_Right : String_Array := Split (Line, " = ");
            Name       : Node_Name    := Node_Name (Left_Right.Element (0));
            Right      : String_Array := Split (Left_Right (1), ", ");
            --  TODO: Finish implementing Split_Any_Of util so we don't have this grossness
            Child_1    : Node_Name    :=
              Node_Name (Right (0) (Right.Element (0)'First + 1 .. Right.Element (0)'Last));
            Child_2    : Node_Name    :=
              Node_Name (Right (1) (Right.Element (1)'First .. Right.Element (1)'Last - 1));
         begin
            Insert (G, Name, Child_1, Child_2);
            if Ends_With (String (Name), "A") then
               Starts.Append (Name);
            elsif Ends_With (String (Name), "Z") then
               Ends.Append (Name);
            end if;
         end;
      elsif Line'Length > 0 then
         -- rlrlrl
         for C of Line loop
            Directions.Append (Parse_Direction (C));
         end loop;
      end if;
   end loop;

   Solution (Count_Distance (G, "AAA", "ZZZ", Directions));

   for S of Starts loop
      for E of Ends loop
         declare
            D : Long_Long_Integer := Count_Distance (G, S, E, Directions);
         begin
            if D /= 100_000 then
               Distances.Append (D);
            end if;
         end;
      end loop;
   end loop;

   Solution (Lcm (Distances));
end Day08;
