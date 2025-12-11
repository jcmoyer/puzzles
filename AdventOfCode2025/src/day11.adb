with Advent.IO; use Advent.IO;
with Ada.Command_Line;
with Advent.Containers.String_Vectors;
with Advent.Strings;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Day11 is

   type Node_ID is new Positive;

   No_Count : constant := -1;

   type Path_Count_Map is
     array (Node_ID range <>,
            Node_ID range <>,
            Boolean range <>,
            Boolean range <>)
     of Long_Long_Integer
   with Default_Component_Value => No_Count;

   type Path_Count_Map_Ptr is access all Path_Count_Map;

   package String_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Node_ID,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   type Adjacency_Matrix is array (Node_ID range <>, Node_ID range <>) of Boolean
   with Default_Component_Value => False;

   type Adjacency_Matrix_Ptr is access all Adjacency_Matrix;

   type World_Type is record
      You_Node : Node_ID;
      Out_Node : Node_ID;
      Svr_Node : Node_ID;
      Dac_Node : Node_ID;
      Fft_Node : Node_ID;

      Edges : Adjacency_Matrix_Ptr;
      Cache : Path_Count_Map_Ptr;

      Node_Names   : String_Maps.Map;
      Next_Node_ID : Node_ID := 1;
   end record;

   function Find (World : World_Type; Name : String) return Node_ID is
   begin
      return World.Node_Names.Element (Name);
   end Find;

   function Find_Or_Allocate (World : in out World_Type; Name : String) return Node_ID is
      Result : Node_ID;
   begin
      if World.Node_Names.Contains (Name) then
         Result := World.Node_Names.Element (Name);
      else
         World.Node_Names.Include (Name, World.Next_Node_ID);
         Result := World.Next_Node_ID;
         World.Next_Node_ID := World.Next_Node_ID + 1;
      end if;
      return Result;
   end Find_Or_Allocate;

   function Last_Node_ID (World : World_Type) return Node_ID
   is (World.Next_Node_ID - 1);

   function Count_Paths
     (World : World_Type; Start, Goal : Node_ID; Through_Fft_Dac : Boolean)
      return Long_Long_Integer
   is
      function Impl_Normal (Pos, Goal : Node_ID) return Long_Long_Integer is
         Sum : Long_Long_Integer := 0;
      begin
         if World.Cache (Pos, Goal, False, False) /= No_Count then
            return World.Cache (Pos, Goal, False, False);
         end if;

         for Neighbor in World.Edges'Range (1) loop
            if World.Edges (Pos, Neighbor) then
               if Neighbor = Goal then
                  Sum := Sum + 1;
               else
                  Sum := Sum + Impl_Normal (Neighbor, Goal);
               end if;
            end if;
         end loop;

         World.Cache (Pos, Goal, False, False) := Sum;
         return Sum;
      end Impl_Normal;

      function Impl_Fft_Dac
        (Pos, Goal : Node_ID; Fft, Dac : Boolean := False) return Long_Long_Integer
      is
         Sum : Long_Long_Integer := 0;
      begin
         if World.Cache (Pos, Goal, Fft, Dac) /= No_Count then
            return World.Cache (Pos, Goal, Fft, Dac);
         end if;

         for Neighbor in World.Edges'Range (1) loop
            if World.Edges (Pos, Neighbor) then
               if Neighbor = World.Fft_Node then
                  Sum := Sum + Impl_Fft_Dac (Neighbor, Goal, Fft => True, Dac => Dac);
               elsif Neighbor = World.Dac_Node then
                  Sum := Sum + Impl_Fft_Dac (Neighbor, Goal, Fft => Fft, Dac => True);
               elsif Neighbor = Goal then
                  Sum := Sum + (if Fft and then Dac then 1 else 0);
               else
                  Sum := Sum + Impl_Fft_Dac (Neighbor, Goal, Fft => Fft, Dac => Dac);
               end if;
            end if;
         end loop;

         World.Cache (Pos, Goal, Fft, Dac) := Sum;
         return Sum;
      end Impl_Fft_Dac;
   begin
      --  invalidate cache or else subsequent runs will be affected
      World.Cache.all := (others => (others => (others => (others => No_Count))));
      if Through_Fft_Dac then
         return Impl_Fft_Dac (Start, Goal);
      else
         return Impl_Normal (Start, Goal);
      end if;
   end Count_Paths;

   ----------------------------------------------------------------------------

   Lines : constant Advent.Containers.String_Vectors.Vector :=
     Read_All_Lines (Ada.Command_Line.Argument (1));

   Node_Names : Advent.Containers.String_Vectors.Vector;
   World      : World_Type;

begin
   for Line of Lines loop
      Node_Names := Advent.Strings.Split_Any (Line, ": ");

      for Name of Node_Names loop
         declare
            Unused : Node_ID := Find_Or_Allocate (World, Name);
         begin
            null;
         end;
      end loop;
   end loop;

   World.Edges := new Adjacency_Matrix (1 .. Last_Node_ID (World), 1 .. Last_Node_ID (World));

   World.Cache :=
     new Path_Count_Map
           (1 .. Last_Node_ID (World), 1 .. Last_Node_ID (World), False .. True, False .. True);

   for Line of Lines loop
      Node_Names := Advent.Strings.Split_Any (Line, ": ", Keep_Empty => False);

      for I in Node_Names.First_Index + 1 .. Node_Names.Last_Index loop
         World.Edges
           (Find_Or_Allocate (World, Node_Names.First_Element),
            Find_Or_Allocate (World, Node_Names (I))) :=
           True;
      end loop;
   end loop;

   World.Dac_Node := Find (World, "dac");
   World.Fft_Node := Find (World, "fft");
   World.You_Node := Find (World, "you");
   World.Out_Node := Find (World, "out");
   World.Svr_Node := Find (World, "svr");

   Solution (Count_Paths (World, World.You_Node, World.Out_Node, Through_Fft_Dac => False));
   Solution (Count_Paths (World, World.Svr_Node, World.Out_Node, Through_Fft_Dac => True));
end Day11;
