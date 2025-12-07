with Ada.Containers;
with Ada.Containers.Vectors;

package Plane_Tessellation is
   --  type tess_point;
   --  type tess_point is access tess_point;
   type tess_point is
      record   
         A, B, C : Float := 0.0;
      end record;
      

   type cartesian is
      record   
         X, Y : Float := 0.0;
      end record;

   function To_Cartesian (This : tess_point) return cartesian;
   procedure Set (This : in out tess_point; a, b, c : Float);
   procedure Print_Transform  (This : tess_point) ;
   procedure Print_Transform_Image  (This : tess_point) ;
   procedure Print_Transform_Image_2  (This : tess_point) ;
   
   function "+" (L, R : tess_point) return tess_point;
   function "-" (L, R : tess_point) return tess_point;
   function "=" (L, R : tess_point) return Boolean;

   package Point_Vector is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => tess_point);

   function Is_Vertex (a, b, c : float) return Boolean;
   function Is_Adjacent (This, Other : tess_point) return Boolean;

   procedure Print_R_Plot (Points : Point_Vector.Vector);
   procedure Print_Point (Point : tess_point);

   function Point_Constructor (a, b, c : Float) return tess_point;
   function Point_Constructor (p : tess_point) return tess_point;
   function Adjacents (This : tess_point) return Point_Vector.Vector;

   function Integer_Product (This : tess_point; Factor : Integer) return tess_point ;
   function Dot_Product (This, Other : tess_point) return Float;
   function Translation (Here, There : tess_point) return tess_point;
   function Unit_Rotation (Left, Right :  tess_point) return tess_point;
   function Unit_Vector_Name (UV : tess_point) return String;
   function Unit_Vector_Order (V : tess_point) return Natural;
   function Unit_Angle_Factor (Left, Right : tess_point) return Natural;
   function Is_Unit_Point (point : tess_point) return Boolean;
 

   Origin : constant tess_point := (0.0,0.0,0.0);


   P_Direction : constant tess_point := (1.0,-1.0,0.0) ;
   R_Direction : constant tess_point := (0.0,1.0,-1.0) ;
   T_Direction : constant tess_point := (-1.0,0.0,1.0) ;

   Q_Direction : constant tess_point := (1.0, 0.0, -1.0) ; 
   S_Direction : constant tess_point := (-1.0, 1.0, 0.0) ;
   U_Direction : constant tess_point := (0.0, -1.0, 1.0) ;
end Plane_Tessellation;