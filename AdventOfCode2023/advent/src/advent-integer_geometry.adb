package body Advent.Integer_Geometry is

   function Contains (R : Rectangle; P : Point) return Boolean is
   begin
      return P.X >= R.Left and then P.X <= R.Right and then P.Y >= R.Bottom and then P.Y <= R.Top;
   end Contains;

   procedure Inflate (R : in out Rectangle; Delta_X, Delta_Y : Integer) is
   begin
      R.Left   := R.Left - Delta_X;
      R.Right  := R.Right + Delta_X;
      R.Top    := R.Top + Delta_Y;
      R.Bottom := R.Bottom - Delta_Y;
   end Inflate;

end Advent.Integer_Geometry;
