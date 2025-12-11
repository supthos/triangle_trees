with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Wide_Text_IO;
with Plane_Tessellation;
use Plane_Tessellation;
use Plane_Tessellation.Point_Vector;

procedure Triangle_Trees is
   package Triangle_Tree is new Ada.Containers.Multiway_Trees
      (Element_Type => tess_point);
   use Triangle_Tree;

   package Tree_Vector is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Triangle_Tree.Tree);
   use Tree_Vector;

   package Cursor_Vector is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Triangle_Tree.Cursor);
   use Cursor_Vector;

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

   function Get_Point_Vector (This : Triangle_Tree.Cursor) return Point_Vector.Vector is
      PVec : Point_Vector.Vector;
   begin 
      if Has_Element(First_Child(This)) then
         PVec.Append_Vector (Get_Point_Vector(First_Child(This)));
      end if;
      if Has_Element (This) then
         PVec.Append (Element(This));
      end if;
      if Last_Child(This) /= First_Child (This) and Has_Element(Last_Child(This))then
         PVec.Append_Vector (Get_Point_Vector(Last_Child(This)));
      end if;
      return PVec;
   end Get_Point_Vector;

   function Get_Leaf_Vector (This : Triangle_Tree.Cursor) return Cursor_Vector.Vector is
      CVec : Cursor_Vector.Vector;
   begin 
      if Has_Element(First_Child(This)) then
         CVec.Append_Vector (Get_Leaf_Vector(First_Child(This)));
      end if;
      if Has_Element (This) and Is_Leaf (This) then
         CVec.Append (This);
      end if;
      if Last_Child(This) /= First_Child (This) and Has_Element(Last_Child(This))then
         CVec.Append_Vector (Get_Leaf_Vector(Last_Child(This)));
      end if;
      return CVec;
   end Get_Leaf_Vector;

   function Next_Leaf (This : Triangle_Tree.Cursor) return Triangle_Tree.Cursor is
   begin
      if Is_Leaf(This) then
         return This;
      elsif Subtree_Node_Count (Last_Child(This)) < Subtree_Node_Count (First_Child(This)) then
         return Next_Leaf (Last_Child(This));
      else 
         return Next_Leaf (First_Child(This));
      end if;
   end Next_Leaf;

   type Child_Pair is
      record
         L, R : tess_point;
      end record;

   package Child_Pair_Vector is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Child_Pair);
   use Child_Pair_Vector;

   function Process_Leaf (Point : tess_point; P_Vec : Point_Vector.Vector) return Child_Pair_Vector.Vector is
      Buffer :  Child_Pair_Vector.Vector;
      Adj_Vec : Point_Vector.Vector;
      Adj_Vec_Ind_L, Adj_Vec_Ind_R : Point_Vector.Extended_Index;
      Child_Buff : Child_Pair;
   begin
      Adj_Vec := Adjacents (Point);
      Adj_Vec_Ind_L := Point_Vector.First_Index (Adj_Vec);
      while Adj_Vec_Ind_L <= Point_Vector.Last_Index(Adj_Vec) loop
         if Point_Vector.Find(P_Vec, Point_Vector.Element(Adj_Vec, Adj_Vec_Ind_L)) = Point_Vector.No_Element Then
            Child_Buff.L := Point_Vector.Element(Adj_Vec, Adj_Vec_Ind_L);
            Adj_Vec_Ind_R := Point_Vector.First_Index (Adj_Vec);
            while Adj_Vec_Ind_R <= Point_Vector.Last_Index(Adj_Vec) loop
               if Adj_Vec_Ind_R /= Adj_Vec_Ind_L then
                  if Point_Vector.Find(P_Vec, Point_Vector.Element(Adj_Vec, Adj_Vec_Ind_R)) = Point_Vector.No_Element Then
                     Child_Buff.R := Point_Vector.Element (Adj_Vec, Adj_Vec_Ind_R);
                     Append (Buffer, Child_Buff);
                  end if;
               end if;
               
               Adj_Vec_Ind_R := Adj_Vec_Ind_R + 1;
            end loop;
            Adj_Vec_Ind_L := Adj_Vec_Ind_L + 1;
         else
            Adj_Vec_Ind_L := Adj_Vec_Ind_L + 1;
         end if;
      end loop;
      return Buffer;
   end Process_Leaf;

   function Next_Degree (TVec : Tree_Vector.Vector) return Tree_Vector.Vector is

      Point : tess_point;

      Node_Points : Point_Vector.Vector;

      Leaf_Cursors : Cursor_Vector.Vector;
      Leaf_Index : Cursor_Vector.Extended_Index;
      
      Cursor_Buffer: Triangle_Tree.Cursor;
      Buf_Tree_Vec, Tree_Vec, Ret_Vec : Tree_Vector.Vector;


      Buffer_Tree, Buf_T : Triangle_Tree.Tree;

      Child_Buf : Child_Pair_Vector.Vector;


   begin
      
      for In_Tree of TVec loop
         --  Buffer_Tree := Copy (In_Tree);
         Clear (Tree_Vec);
         
         Buf_T := Copy (In_Tree);
         Append (Tree_Vec, Buf_T);


         Leaf_Cursors := Get_Leaf_Vector (Buf_T.Root);
         Leaf_Index := First_Index(Leaf_Cursors);
        

         while Leaf_Index <= Leaf_Cursors.Last_Index loop

            for BTree of Tree_Vec loop
               Point := Element(Element(Leaf_Cursors, Leaf_Index));
               
               Child_Buf := Process_Leaf(Point, Get_Point_Vector (BTree.Root));
               for Pair of Child_Buf loop
                  Buffer_Tree := Copy (BTree);
                  Cursor_Buffer := Triangle_Tree.Find (Buffer_Tree, Point);
                  Append_Child (Container => Buffer_Tree, Parent => Cursor_Buffer, New_Item => Pair.L, Count => 1);
                  Append_Child (Container => Buffer_Tree, Parent => Cursor_Buffer, New_Item => Pair.R, Count => 1);
                  
                  Append(Buf_Tree_Vec, Buffer_Tree);
                  
               end loop;
            end loop;

            Leaf_Index := Leaf_Index + 1 ;

            Tree_Vec := Copy(Buf_Tree_Vec);
            Clear (Buf_Tree_Vec);
         end loop;

         Append_Vector (Ret_Vec, Tree_Vec);
      end loop;
      return Ret_Vec;
   end Next_Degree;

   procedure Reduce (TVec : in out Tree_Vector.Vector) is
      A_Tree, Buf_T : Triangle_Tree.Tree;
      Tree_Index, T_I : Tree_Vector.Extended_Index;
      Out_TVec : Tree_Vector.Vector;
   begin
      if not Tree_Vector.Is_Empty (TVec) then

         T_I := TVec.First_Index;
         while T_I <= TVec.Last_Index loop

            Buf_T := Copy(Tree_Vector.Element(TVec,T_I));
            Tree_Index := T_I;
            while Tree_Index <= TVec.Last_Index loop

               if T_I /= Tree_Index then
                  A_Tree := Copy(Tree_Vector.Element (TVec, Tree_Index)); 

                  if Compare(Buf_T,A_Tree) = True then
                     Tree_Vector.Delete (TVec, Tree_Index, 1);
                  else 
                     Tree_Index := Tree_Index + 1 ;
                  end if;

               else
                  Tree_Index := Tree_Index + 1 ;   
               end if;

            end loop;
            T_I := T_I + 1;
         end loop;
      end if;
   end Reduce;

   A_Tree, B_Tree, C_Tree, D_Tree : Triangle_Tree.Tree;

   Cursor_A, Cursor_B, Cursor_C, Cursor_D : Triangle_Tree.Cursor;

   Unit_Points : Point_Vector.Vector;

   T_Vector : Tree_Vector.Vector;
begin
   Insert_Point (D_Tree, Point_Constructor(0.0,0.0,0.0));

   Tree_Vector.Append(T_Vector, D_Tree);
   T_Vector := Next_Degree (T_Vector);
   --  Reduce (T_Vector);
   Ada.Text_IO.Put_Line ("Trees: " & Integer'Image(Integer(T_Vector.Length)));
   T_Vector := Next_Degree (T_Vector);
   --  Reduce (T_Vector);
   Ada.Text_IO.Put_Line ("Trees: " & Integer'Image(Integer(T_Vector.Length)));
   T_Vector := Next_Degree (T_Vector);
   --  Reduce (T_Vector);
   Ada.Text_IO.Put_Line ("Trees: " & Integer'Image(Integer(T_Vector.Length)));
   T_Vector := Next_Degree (T_Vector);
   --  Reduce (T_Vector);
   Ada.Text_IO.Put_Line ("Trees: " & Integer'Image(Integer(T_Vector.Length)));
   
end Triangle_Trees;
