with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Strings;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO;
package body Plane_Tessellation is
   function To_Cartesian (This : tess_point) return cartesian is
      Coordinates : cartesian;
   begin
      Coordinates.Y := 
         ((Sin (0.0,360.0) * This.A) + 
         (Sin (120.0,360.0) * This.B) + 
         (Sin (210.0,360.0) * This.C) ) ;
      Coordinates.X := 
         ((Cos (0.0,360.0) * This.A) + 
         (Cos (120.0,360.0) * This.B) + 
         (Cos (210.0,360.0) * This.C) ) ;
      return Coordinates;
   end To_Cartesian;

   function Is_Vertex (a, b, c : float) return Boolean is
   begin
      return (a + b + c = 0.0); 
   end Is_Vertex;

   function Is_Adjacent (This, Other : tess_point) return Boolean is
   begin
      if This - Other = P_Direction then
         return True;
      elsif This - Other = Q_Direction then
         return True;
      elsif This - Other = R_Direction then
         return True;
      elsif This - Other = S_Direction then
         return True;
      elsif This - Other = T_Direction then
         return True;
      elsif This - Other = U_Direction then
         return True;
      else 
         return False;
      end if;
   end Is_Adjacent;
      

   procedure Set (This : in out tess_point; a, b, c : Float) is
   begin
      This.A := a;
      This.B := b;
      This.C := c;
   end Set;

   procedure Print_Transform (This : tess_point) is
      Cart : cartesian;
   begin
      Cart := To_Cartesian (This);
      Ada.Text_IO.Put_Line ("(" & Float'Image (This.A) & "," & Float'Image (This.B) & "," & Float'Image (This.C) & ") -> (" &  Float'Image (Cart.X) & "," & Float'Image (Cart.Y) & ")");
   end Print_Transform;

   procedure Print_Transform_Image  (This : tess_point) is
      Cart : cartesian;
   begin
      Cart := To_Cartesian (This);
      Ada.Text_IO.Put_Line ("(" & Float'Image (Cart.X) & "," & Float'Image (Cart.Y) & ")");
   end Print_Transform_Image;

   procedure Print_Transform_Image_2  (This : tess_point) is
      Cart : cartesian;
   begin
      Cart := To_Cartesian (This);
      Ada.Text_IO.Put_Line ( Float'Image (Cart.X) & "," & Float'Image (Cart.Y) ) ;
   end Print_Transform_Image_2;

   procedure Print_R_Plot (Points : Point_Vector.Vector) is
      XList : Unbounded_String;
      YList : Unbounded_String;
      Cart : cartesian;
   begin
      for point of Points loop
         Cart := To_Cartesian (point);
         Append ( Xlist, Float'Image (Cart.X)) ;
         Append ( Ylist, Float'Image (Cart.Y)) ;
         if point /= Points.Last_Element then
            Append (XList, ",");
            Append (YList, ",");
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("plot(c(" & To_String (XList) & "), c(" & To_String (YList) & "))");
   end Print_R_Plot;
         
   procedure Print_Point (Point : tess_point) is
   begin 
      Ada.Text_IO.Put_Line ("(" & Float'Image (Point.A) & ", " & Float'Image (Point.B) & ", " & Float'Image (Point.C) & ")");
   end Print_Point;

   function Point_Constructor (a, b, c : Float) return tess_point is
      point : tess_point;
   begin
      Set (point, a, b, c);
      return point;
   end Point_Constructor;

   function Point_Constructor (p : tess_point) return tess_point is
   begin
      return Point_Constructor (p.A, p.B, p.C);
   end Point_Constructor;
      
   function Adjacents (This : tess_point) return Point_Vector.Vector is
      List : Point_Vector.Vector;
   begin
      List.Append (Point_Constructor (This + P_Direction));
      List.Append (Point_Constructor (This + Q_Direction));
      List.Append (Point_Constructor (This + R_Direction));
      List.Append (Point_Constructor (This + S_Direction));
      List.Append (Point_Constructor (This + T_Direction));
      List.Append (Point_Constructor (This + U_Direction));
      return List;
   end Adjacents;

   function Dot_Product (This, Other : tess_point) return Float is
   begin
      return (This.A * Other.A) + (This.B * Other.B) + (This.C * Other.C);
   end Dot_Product;

   function Integer_Product (This : tess_point; Factor : Integer) return tess_point is
   begin
      return Point_Constructor (This.A * Float(Factor), This.B * Float(Factor), This.C * Float(Factor));
   END Integer_Product;

   function Translation (Here, There : tess_point) return tess_point is
   begin
      return There - Here ;
   end Translation;

   --  function Unit_Angle (Left, Right: tess_point) return float is

   function Unit_Rotation (Left, Right: tess_point) return tess_point is
      Buffer1, Buffer2 : tess_point;
   begin
      if Is_Vertex (Left.A, Left.B, Left.c) and Is_Vertex (Right.A, Right.B, Right.C) then
         return Translation (Left, Right) + Translation (Left, P_Direction);
      end if;
   end Unit_Rotation;

   function Unit_Vector_Name (UV : tess_point) return String is
      Name : Unbounded_String;
   begin
      if UV = P_Direction then
         Append (Name, "P");
      elsif UV = Q_Direction then
         Append (Name, "Q");
      elsif UV = R_Direction then
         Append (Name, "R");
      elsif UV = S_Direction then
         Append (Name, "S");
      elsif UV = T_Direction then
         Append (Name, "T");
      elsif UV = U_Direction then
         Append (Name, "U");
      else
         Append (Name, "NaN");
      end if;
      return To_String(Name);
   end Unit_Vector_Name;

function Unit_Vector_Order (V : tess_point) return Natural is
begin
   declare
      Name : constant String := Unit_Vector_Name (V);
   begin
      if Name = "P" then
         return 6; -- is 0 mod 6
      elsif Name = "Q" then
         return 1;
      elsif Name = "R" then
         return 2;
      elsif Name = "S" then
         return 3;
      elsif Name = "T" then
         return 4;
      elsif Name = "U" then
         return 5;
      else
         -- The original Sqrt(-1) would cause a run-time exception.
         -- It's better to explicitly raise an exception or return a special value.
         raise Program_Error with "Input to Unit_Vector_Order is not a unit vector.";
      end if;
   end;
end Unit_Vector_Order;

   function Unit_Angle_Factor (Left, Right : tess_point) return Natural is
   begin
      if Is_Unit_Point (Left) and Is_Unit_Point (Right) then
         return (Unit_Vector_Order (Right) - Unit_Vector_Order (Left) + 6) mod 6;
      else
         raise Program_Error with "Not unit points." ;
      end if;
   end Unit_Angle_Factor;

   function Is_Unit_Point (point : tess_point) return Boolean is
   begin
      if point = P_Direction
         or point = Q_Direction
         or point = R_Direction
         or point = S_Direction
         or point = T_Direction
         or point = U_Direction
      then 
         return True;
      else
         return False;
      end if;
   end Is_Unit_Point;

   function "+" (L, R : tess_point) return tess_point is 
      buf : tess_point;
   begin
      buf.A := L.A + R.A;
      buf.B := L.B + R.B;
      buf.C := L.C + R.C;
      return buf;
   end "+";

   function "-" (L, R : tess_point) return tess_point is 
      buf : tess_point;
   begin
      buf.A := L.A - R.A;
      buf.B := L.B - R.B;
      buf.C := L.C - R.C;
      return buf;
   end "-";
   function "=" (L , R: tess_point) return Boolean is
   begin
      if (L.A = R.A) and (L.B = R.B) and (L.C = R.C) then
         return True;
      else
         return False;
      end if;
   end "=";

end Plane_Tessellation;