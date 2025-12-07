with Ada.Containers.Bounded_Multiway_Trees;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Wide_Text_IO;
with Plane_Tessellation;
use Plane_Tessellation;

procedure Triangle_Trees is
   package Triangle_Tree is new Ada.Containers.Multiway_Trees
      (Element_Type => tess_point);
   use Triangle_Tree;

   package Cursor_Stack is new Ada.Containers.Vectors 
      ( Index_Type => Natural, Element_Type => Triangle_Tree.Cursor);
   use Cursor_Stack;

   type Displacement is
      record
         Translation : tess_point ;
         Rotation : Natural;
      end record;

   function Test_Value (A_Cursor, B_Cursor : Triangle_Tree.Cursor; Rotation : Natural) return Boolean is
   begin
      if Is_Root (Parent(A_Cursor)) and Is_Root(Parent(B_Cursor)) then
         return True;
      elsif Is_Root (Parent(A_Cursor)) or Is_Root(Parent(B_Cursor)) then
         return False;
      
      elsif Unit_Angle_Factor (Element (A_Cursor) - Element (Parent(A_Cursor)), 
            (Element (B_Cursor) - Element(parent(B_Cursor)))) = Rotation 
      then
         return True;
      else
         return False;
      end if;
   end Test_Value;

   function Sub_Compare (A_Cursor, B_Cursor :Triangle_Tree.Cursor; Rotation : Natural ) return Boolean is
   begin
      if Has_Element(A_Cursor) and Has_Element (B_Cursor) then
         if Test_Value (A_Cursor , B_Cursor, Rotation) 
         then
            if Is_Leaf (A_Cursor) and Is_Leaf (B_Cursor) then
               return True;
            elsif ((Is_Leaf (A_Cursor)) or (Is_Leaf (B_Cursor))) then
               return False;
            elsif (not (Is_Leaf (A_Cursor)) and not (Is_Leaf (B_Cursor))) then

               if First_Child (A_Cursor) = Last_Child (A_Cursor)
                  and First_Child (B_Cursor) = Last_Child (B_Cursor)
               then
                  return Sub_Compare (First_Child (A_Cursor), First_Child(B_Cursor), Rotation);
               elsif Unit_Angle_Factor (Element(A_Cursor) - Element (First_Child(A_Cursor)), Element(A_Cursor) - Element (Last_Child(A_Cursor))) 
                        = Unit_Angle_Factor (Element(B_Cursor) - Element (First_Child(B_Cursor)), Element(B_Cursor) - Element (Last_Child(B_Cursor)))
               then
                  return Sub_Compare (First_Child (A_Cursor), First_Child(B_Cursor), Rotation)
                     and Sub_Compare (Last_Child (A_Cursor), Last_Child(B_Cursor), Rotation) ;  
               else 
                  return False;
               end if;

            end if;
         else
            return False;
         end if;
      elsif Has_Element (A_Cursor) or Has_Element (B_Cursor) then
         return False;
      else 
         return True;
      end if;
   end Sub_Compare;

   function Compare (This, Other : Triangle_Tree.Tree) return Boolean is
      Transform : Displacement;
      Rotation : Natural;
      A_Node_Cursor, B_Node_Cursor : Triangle_Tree.Cursor;
      Bool : Boolean := True;
      --  A_Stack, B_Stack : Cursor_Stack;
   begin
      if (Is_Empty (This) and Is_Empty (Other)) then 
         return True;
      elsif (Is_Empty (This) or Is_Empty (Other)) then
         return False;
      elsif (not Is_Empty (This) and not Is_Empty (Other)) then
         A_Node_Cursor := First_Child(This.Root);
         B_Node_Cursor := First_Child(Other.Root);
         
         --compare
         if Is_Leaf (A_Node_Cursor) and Is_Leaf (B_Node_Cursor) then
            return True;
         elsif Is_Leaf (A_Node_Cursor) or Is_Leaf (B_Node_Cursor) then
            return False;
         elsif 
            (not (Is_Leaf (A_Node_Cursor)) and not (Is_Leaf (B_Node_Cursor)))
         then
            Rotation := Unit_Angle_Factor(Element(First_Child(A_Node_Cursor)) - Element (A_Node_Cursor),
               Element(First_Child(B_Node_Cursor)) - Element(B_Node_Cursor));
             
            return Sub_Compare (A_Node_Cursor, B_Node_Cursor, Rotation) ;
         end if;
      end if;

   end Compare;

   procedure Insert_Point (tree : in out Triangle_Tree.Tree; point : tess_point) is
      Node_Cursor : Triangle_Tree.Cursor;
   begin
      if Is_Vertex (a => point.A, b => point.B, c => point.C) then

         if not Is_Empty (tree) then 
            Node_Cursor := Triangle_Tree.First_Child (tree.Root);
            while True loop
               if Child_Count (Node_Cursor) < 2 then

                  if Is_Adjacent (Triangle_Tree.Element(Node_Cursor), point) then
                     Append_Child (tree, Node_Cursor, point);
                     exit;
                  else
                     Ada.Text_IO.Put_Line ("Not Adjacent");
                     exit;
                  end if ;

               elsif Subtree_Node_Count (First_Child (Node_Cursor))
                  <= Subtree_Node_Count (Last_Child (Node_Cursor))
               then
                  Node_Cursor := First_Child (Node_Cursor);
               else
                  Node_Cursor := Last_Child (Node_Cursor);
               end if;
            end loop;
         else
            Insert_Child (tree, tree.Root, Triangle_Tree.No_Element, point, 1);
         end if;
      else 
         Ada.Text_IO.Put_Line ("Not a Node Point.");
      end if;
   end Insert_Point;
   
   procedure Print_Tree (cursor: Triangle_Tree.Cursor) is
   begin
      if Has_Element(First_Child(cursor)) then
         Print_Tree (First_Child(cursor));
      end if;
      if not Is_Root (cursor) and Has_Element(cursor) then
         Print_Point (Element(cursor));
      end if;
      if Last_Child(cursor) /= First_Child(cursor) 
         and Has_Element(Last_Child(cursor)) then
         Print_Tree (Last_Child(cursor));
      end if;
   end Print_Tree;

   A_Tree, B_Tree, C_Tree : Triangle_Tree.Tree;

   Cursor_A, Cursor_B, Cursor_C : Triangle_Tree.Cursor;

   Unit_Points : Point_Vector.Vector;
begin

   Point_Vector.Append (Unit_Points, P_Direction);
   Point_Vector.Append (Unit_Points, Q_Direction);
   Point_Vector.Append (Unit_Points, R_Direction);
   Point_Vector.Append (Unit_Points, S_Direction);
   Point_Vector.Append (Unit_Points, T_Direction);
   Point_Vector.Append (Unit_Points, U_Direction);

   for point of Unit_Points loop
      Ada.Text_IO.Put_Line( Unit_Vector_Name(point) & ":");
      Print_Point (point - P_Direction);
      Print_Point (point - Q_Direction);
      Print_Point (point - R_Direction);
      Print_Point (point - S_Direction);
      Print_Point (point - T_Direction);
      Print_Point (point - U_Direction);
   end loop;


      

   Cursor_A := A_Tree.Root;
   Insert_Point (A_Tree,Point_Constructor (-2.0, 1.0, 1.0));
   Cursor_A := Triangle_Tree.First_Child(Cursor_A);
   Insert_Point (A_Tree, Element(Cursor_A) + P_Direction);
   Insert_Point (A_Tree, Element(Cursor_A) + Q_Direction);

   Print_Tree (Cursor_A);

   Cursor_B := B_Tree.Root;
   Insert_Point (B_Tree,Point_Constructor (-3.0, 7.0, -4.0));
   Cursor_B := Triangle_Tree.First_Child(Cursor_B);
   Insert_Point (B_Tree, Element(Cursor_B) + Q_Direction);
   Insert_Point (B_Tree, Element(Cursor_B) + R_Direction);
   

   Cursor_C := C_Tree.Root;
   Insert_Point (C_Tree,Point_Constructor(5.0, -2.0, -3.0));
   Cursor_C := Triangle_Tree.First_Child(Cursor_C);
   Insert_Point (C_Tree, Element(Cursor_C) + R_Direction);
   Insert_Point (C_Tree, Element(Cursor_C) + T_Direction);

   if Compare (A_Tree, B_Tree) then 
      Ada.Text_IO.Put_Line ("Okay");
   else
      Ada.Text_IO.Put_Line ("Not Okay");
   end if;

   if Compare (B_Tree, C_Tree) then 
      Ada.Text_IO.Put_Line ("Okay");
   else
      Ada.Text_IO.Put_Line ("Not Okay");
   end if;

   if Compare (C_Tree, A_Tree) then 
      Ada.Text_IO.Put_Line ("Okay");
   else
      Ada.Text_IO.Put_Line ("Not Okay");
   end if;
   
end Triangle_Trees;
