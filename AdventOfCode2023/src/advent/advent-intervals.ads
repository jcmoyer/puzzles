with Ada.Containers.Vectors;

generic
   type Element_Type is range <>;
package Advent.Intervals is

   pragma Preelaborate;
   ----------------------------------------------------------------------------
   --  Interval
   ----------------------------------------------------------------------------

   type Interval is record
      Min : Element_Type;
      Max : Element_Type;
   end record;

   function "=" (A, B : Interval) return Boolean;

   Empty_Interval : constant Interval := (Min => 0, Max => -1);

   function Image (R : Interval) return String;

   function Singleton (Value : Element_Type) return Interval;

   function First (R : Interval) return Element_Type;
   function Last (R : Interval) return Element_Type;
   function Length (R : Interval) return Element_Type;

   function Contains (R : Interval; Val : Element_Type) return Boolean;

   function Contains (R, Enclosed : Interval) return Boolean;

   function Overlaps (A, B : Interval) return Boolean;

   function Merge (A, B : Interval) return Interval with
     Pre => Overlaps (A, B);

   function Intersect (A, B : Interval) return Interval;

   ----------------------------------------------------------------------------
   --  Multi_Interval
   ----------------------------------------------------------------------------

   type Multi_Interval is private;

   procedure Clear (M : in out Multi_Interval);
   function Copy (M : Multi_Interval) return Multi_Interval;
   procedure Reduce (M : in out Multi_Interval);
   procedure Insert (M : in out Multi_Interval; R : Interval);
   procedure Insert (M : in out Multi_Interval; R : Multi_Interval);
   procedure Delete (M : in out Multi_Interval; R : Interval; Deleted : out Multi_Interval);
   procedure Translate (M : in out Multi_Interval; Amount : Element_Type);
   procedure Translate (M : in out Multi_Interval; Span : Interval; Amount : Element_Type);
   function First (M : Multi_Interval) return Element_Type;

private
   package Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Interval);
   subtype Interval_Vector is Interval_Vectors.Vector;

   type Multi_Interval is record
      Children : Interval_Vector;
   end record;
end Advent.Intervals;
