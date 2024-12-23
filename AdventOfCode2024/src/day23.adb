with Advent.IO;             use Advent.IO;
with Advent.Strings;        use Advent.Strings;
with Ada.Command_Line;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day23 is

   use type Ada.Containers.Count_Type;
   use type Ada.Containers.Hash_Type;

   type Vertex is new Natural;

   type Vertex_Array is array (Positive range <>) of Vertex;

   subtype Vertex_Triplet is Vertex_Array (1 .. 3);

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type => Positive, Element_Type => Vertex, Array_Type => Vertex_Array);

   function Hash (VA : Vertex_Array) return Ada.Containers.Hash_Type is
      H : Ada.Containers.Hash_Type := 0;
   begin
      for Val of VA loop
         H := H xor Ada.Containers.Hash_Type (Val) * 13;
      end loop;
      return H;
   end Hash;

   package String_To_Int_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Vertex,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   function Hash (X : Vertex) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (X));

   package Int_To_String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Vertex, Element_Type => String, Hash => Hash, Equivalent_Keys => "=");

   package Vertex_Array_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vertex_Triplet, Hash => Hash, Equivalent_Elements => "=");

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);

   package String_Sorting is new String_Vectors.Generic_Sorting;

   Name_To_Vertex : String_To_Int_Maps.Map;
   Vertex_To_Name : Int_To_String_Maps.Map;
   Next_Vertex    : Vertex := 1;
   Last_Vertex    : Vertex := 0;

   type Adjacency_Matrix is array (Vertex range <>, Vertex range <>) of Boolean with
     Default_Component_Value => False;

   type Adjacency_Matrix_Ptr is access all Adjacency_Matrix;

   type Adjacency_Matrix_Array is array (Vertex range <>) of Boolean;

   Graph : Adjacency_Matrix_Ptr;

   function Get_Or_Create (Name : String) return Vertex is
   begin
      if not Name_To_Vertex.Contains (Name) then
         if Next_Vertex not in Graph'Range (1) then
            raise Program_Error with "graph allocation not large enough";
         end if;
         Name_To_Vertex.Insert (Name, Next_Vertex);
         Vertex_To_Name.Insert (Next_Vertex, Name);
         Next_Vertex := Next_Vertex + 1;
         Last_Vertex := Last_Vertex + 1;
      end if;
      return Name_To_Vertex.Element (Name);
   end Get_Or_Create;

   procedure Add_Edge (A, B : String) is
      VA : constant Vertex := Get_Or_Create (A);
      VB : constant Vertex := Get_Or_Create (B);
   begin
      Graph (VA, VB) := True;
      Graph (VB, VA) := True;
   end Add_Edge;

   function Has_Edge (A, B : Vertex) return Boolean is
   begin
      return Graph (A, B);
   end Has_Edge;

   --!pp off
   function Is_Triple_T (A, B, C : Vertex) return Boolean is (
     Has_Edge (A, B) and then Has_Edge (B, C) and then Has_Edge (C, A)
     and then (
       Starts_With (Vertex_To_Name (A), "t") or else
       Starts_With (Vertex_To_Name (B), "t") or else
       Starts_With (Vertex_To_Name (C), "t")
     )
   );
   --!pp on

   procedure Gather_Triple_Ts (All_Triplets : in out Vertex_Array_Sets.Set) is
   begin
      for A in 1 .. Last_Vertex loop
         for B in A + 1 .. Last_Vertex loop
            for C in B + 1 .. Last_Vertex loop
               if Is_Triple_T (A, B, C) then
                  declare
                     New_V : Vertex_Triplet := (A, B, C);
                  begin
                     Sort (New_V);
                     All_Triplets.Include (New_V);
                  end;
               end if;
            end loop;
         end loop;
      end loop;
   end Gather_Triple_Ts;

   procedure Print_Graph is
   begin
      for I in 1 .. Last_Vertex loop
         for J in 1 .. Last_Vertex loop
            Put (Graph (I, J)'Image & " ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Graph;

   procedure Gather_Names (A : Adjacency_Matrix_Array; Names : in out String_Vectors.Vector) is
   begin
      Names.Clear;
      for I in A'Range loop
         if A (I) then
            Names.Append (Vertex_To_Name.Element (I));
         end if;
      end loop;
   end Gather_Names;

   function Find_Largest_Clique return Adjacency_Matrix_Array is
      Current : Adjacency_Matrix_Array (1 .. Last_Vertex) := (others => False);
      Largest : Adjacency_Matrix_Array (1 .. Last_Vertex);

      Largest_Size : Integer := 0;
      Current_Size : Integer := 0;

      function Has_Edge_To_All_Current (J : Vertex) return Boolean is
      begin
         for K in 1 .. Last_Vertex loop
            if Current (K) and then not Has_Edge (J, K) then
               return False;
            end if;
         end loop;
         return True;
      end Has_Edge_To_All_Current;
   begin
      for I in 1 .. Last_Vertex loop
         Current      := (others => False);
         Current (I)  := True;
         Current_Size := 1;

         for J in 1 .. Last_Vertex loop
            if Has_Edge_To_All_Current (J) then
               Current (J)  := True;
               Current_Size := Current_Size + 1;
            end if;
         end loop;

         if Current_Size > Largest_Size then
            Largest      := Current;
            Largest_Size := Current_Size;
         end if;
      end loop;
      return Largest;
   end Find_Largest_Clique;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Triple_Ts      : Vertex_Array_Sets.Set;
   Largest_Clique : Unbounded_String;

begin
   --  Sized for worst case
   Graph := new Adjacency_Matrix (1 .. 2 * Vertex (Lines.Length), 1 .. 2 * Vertex (Lines.Length));

   for Line of Lines loop
      declare
         Parts : String_Array;
      begin
         Parts := Split (Line, "-");
         Add_Edge (Parts (1), Parts (2));
      end;
   end loop;

   Gather_Triple_Ts (Triple_Ts);

   declare
      Clique : constant Adjacency_Matrix_Array := Find_Largest_Clique;
      Names  : String_Vectors.Vector;
   begin
      Gather_Names (Clique, Names);
      String_Sorting.Sort (Names);
      for I in Names.First_Index .. Names.Last_Index loop
         Append (Largest_Clique, Names (I));
         if I /= Names.Last_Index then
            Append (Largest_Clique, (','));
         end if;
      end loop;
   end;

   Solution (Integer (Triple_Ts.Length));
   Solution (To_String (Largest_Clique));
end Day23;
