#pragma once
#include <array>
#include <tuple>
#include <vector>
#include <algorithm>
#include "PlaneTessellation.h"

class TreeNode {
	public:
	tess_point point = directions::O;
	TreeNode* left = nullptr;
	TreeNode* right = nullptr;
	TreeNode() : point(directions::O) {}
	TreeNode(tess_point pos) : point(pos) {}
	~TreeNode() {
		delete left;
		delete right;
	}	

	unsigned long Nodes() const {
		unsigned long count = 1;
		if (left != nullptr) {
			count += left->Nodes();
		}
		if (right != nullptr) {
			count += right->Nodes();
		}
		return count;
	}

	void AppendPoint(tess_point pos) {
		if (left == nullptr)
			left = new TreeNode(pos);
		else if (right == nullptr)
			right = new TreeNode(pos);
		else
			throw std::runtime_error("Both child nodes are already occupied.");
	}

	bool is_leaf() const {
		return left == nullptr && right == nullptr;
	}

	bool is_full() const {
		return left != nullptr && right != nullptr;
	}
	TreeNode* Find(tess_point Point) {
		TreeNode* buf;
		if (Point == point)
			return this;
		if (left != nullptr) {
			buf = left->Find(Point);
			if (buf != nullptr)
				return buf;
		}
		if (right != nullptr) {
			buf = right->Find(Point);
			if (buf != nullptr)
				return buf;
		}
		return nullptr;
	}

	void Delete(){

			delete left;
			left = nullptr;

			delete right;
			right = nullptr;

	}

	void Copy_Tree(TreeNode* const& From) {
		if (!is_leaf())
			Delete();
		if (From != nullptr) {
			point = From->point;
			if (From->left != nullptr) {
				left = new TreeNode();
				left->Copy_Tree(From->left);
			}
			if (From->right != nullptr) {
				right = new TreeNode();
				right->Copy_Tree(From->right);
			}
		}
	}
};

struct ChildPair {
	tess_point left = directions::O;
	tess_point right = directions::O;
};

class TriangleTree {
	public:
	TreeNode* root = nullptr;
	TriangleTree() {
		root = new TreeNode();
	}

	void InsertPoint(tess_point pos) {
		TreeNode* buf_node = root;
		if (is_vertex(pos)) {
			if (root == nullptr){
				root = new TreeNode(pos);
				return;
			}
			while (!buf_node->is_leaf()) {
				if (buf_node->left->Nodes() <= buf_node->right->Nodes()) {
					buf_node = buf_node->left;
				}
				else {
					buf_node = buf_node->right;
				}
			}
			if (is_adjacent(buf_node->point, pos)) {
				buf_node->AppendPoint(pos);
				return;
			}
		}
		else
			throw std::invalid_argument("Only vertex points can be inserted into the TriangleTree.");
	}
	unsigned long Nodes() {
		return root->Nodes();
	}
	std::vector<tess_point> GetPoints(TreeNode* tnp) const {
		std::vector<tess_point> points;
		if (tnp == nullptr) {
			return points;
		}
		if (tnp->left != nullptr) {
			auto left_points = GetPoints(tnp->left);
			points.insert(points.end(), left_points.begin(), left_points.end());
		}
		points.push_back(tnp->point);
		if (tnp->right != nullptr) {
			auto right_points = GetPoints(tnp->right);
			points.insert(points.end(), right_points.begin(), right_points.end());
		}
		return points;
	}

	std::vector<tess_point> GetLeafPoints(TreeNode* tnp) {

		std::vector<tess_point> leaf_points;


		if (tnp->left != nullptr) {
			auto left_leaves = GetLeafPoints(tnp->left);
			leaf_points.insert(leaf_points.end(), left_leaves.begin(), left_leaves.end());
		}
		if (tnp->is_leaf()) {
			leaf_points.push_back(tnp->point);
		}
		if (tnp->right != nullptr) {
			auto right_leaves = GetLeafPoints(tnp->right);
			leaf_points.insert(leaf_points.end(), right_leaves.begin(), right_leaves.end());
		}
		

		return leaf_points;
	}

	TreeNode* Find(tess_point Point) {
		return root->Find(Point);
	}

	// This function checks the adjacent points of a leaf node against the vector
	// of points already in the tree, and returns a vector of possible ChildPairs.
	std::vector<ChildPair> ProcessLeaf(tess_point const& Point, std::vector<tess_point> const& Vector) {
		std::vector<ChildPair> Buffer;
		std::vector<tess_point> Adj_Vec = Adjacents(Point);

		ChildPair ChildBuf ;
		for (const tess_point& L : Adj_Vec) {
			if (std::find(Vector.begin(), Vector.end(), L) == Vector.end()) {
				ChildBuf.left = L;
				for (const tess_point& R : Adj_Vec) {
					if (L != R) {
						if (std::find(Vector.begin(), Vector.end(), R) == Vector.end()) {
							ChildBuf.right = R;
							Buffer.push_back(ChildBuf);
						}
					}
				}
			}
		}
		return Buffer;
	}

	std::vector<ChildPair> ProcessTriangleLeaf(tess_point const& Point, std::vector<tess_point> const& Vector) {
		std::vector<ChildPair> Buffer;
		std::vector<tess_point> Adj_Vec = Adjacents(Point);
		ChildPair ChildBuf;
		for (const tess_point& L : Adj_Vec) {
			if (std::find(Vector.begin(), Vector.end(), L) == Vector.end()) {
				ChildBuf.left = L;
				for (const tess_point& R : Adj_Vec) {
					if (L != R) {
						if (std::find(Vector.begin(), Vector.end(), R) == Vector.end()) {
							if (UnitAngleFactor(L - Point, R - Point) == 1 || UnitAngleFactor(L - Point, R - Point) == 5) {
								ChildBuf.right = R;
								Buffer.push_back(ChildBuf);
							}
						}
					}
				}
			}
		}
		return Buffer;
	}

	std::vector<TriangleTree> NextDegreeLoop(const TriangleTree& InTree) {
		tess_point Point = directions::O;
		std::vector<tess_point> NodePoints;
		std::vector<tess_point> LeafPoints;
		TreeNode* NodeBuffer = nullptr;
		std::vector<TriangleTree> Buf_Tree_Vec, Tree_Vec;
		TriangleTree Buffer_Tree, Buf_T;

		
		std::vector<ChildPair> Child_Buf;


		Tree_Vec.clear();
		Buf_T = InTree;

		Tree_Vec.push_back(Buf_T);

		//LeafPoints.clear();
		LeafPoints = GetLeafPoints(InTree.root);
		unsigned LeafIndex = 0;

		while (LeafIndex < LeafPoints.size()) {
			for (const TriangleTree& BTree : Tree_Vec) {
				Buf_T = BTree;
				Point = LeafPoints[LeafIndex];
				//Child_Buf = ProcessLeaf2(Point, GetPoints(Buf_T.root));
				//Child_Buf = ProcessLeaf(Point, GetPoints(Buf_T.root));
				Child_Buf = ProcessTriangleLeaf(Point, GetPoints(Buf_T.root));
				for (const ChildPair& Pair : Child_Buf) {
					Buffer_Tree = BTree;
					NodeBuffer = Buffer_Tree.Find(Point);
					NodeBuffer->AppendPoint(Pair.left);
					NodeBuffer->AppendPoint(Pair.right);
					Buf_Tree_Vec.push_back(Buffer_Tree);
				}
			}
			LeafIndex++;
			std::swap(Tree_Vec, Buf_Tree_Vec);

			Buf_Tree_Vec.clear();
		}

		return Tree_Vec;

	}

	std::vector<TriangleTree> NextDegree(std::vector<TriangleTree>& TVec) {
		std::vector<TriangleTree> RetVec, bufvec;
		TriangleTree BTree;

		for (TriangleTree tree : TVec) {
			bufvec = NextDegreeLoop(tree);
			RetVec.insert(RetVec.end(), bufvec.begin(), bufvec.end());
		}
		return RetVec;
	}

	void Copy(TriangleTree const& Other) {
		root->Copy_Tree(Other.root);
	}
	~TriangleTree() {
		if (root != nullptr) {
			root->Delete();
			delete root;
			root = nullptr;
		}
	}
	// 1. Copy Constructor: Handles deep copying for vector operations
	TriangleTree(const TriangleTree& other) {
		root = new TreeNode();
		if (other.root) {
			root->Copy_Tree(other.root);
		}
	}

	// 2. Assignment Operator: Handles 'TreeA = TreeB'
	TriangleTree& operator=(const TriangleTree& other) {
		if (this != &other) {
			if (root) root->Delete(); // Clean up existing nodes
			else root = new TreeNode();
			root->Copy_Tree(other.root);
		}
		return *this;
	}

	void ApplySum(TreeNode* tnp, tess_point Value) {
		if (tnp != nullptr) {
			tnp->point = tnp->point + Value;
			ApplySum(tnp->left, Value);
			ApplySum(tnp->right, Value);
		}
	}

	void ApplyRotation(TreeNode* tnp, unsigned short Rotation) {
		if (tnp != nullptr) {
			tnp->point = RotatePoint(tnp->point, Rotation);
			ApplyRotation(tnp->left, Rotation);
			ApplyRotation(tnp->right, Rotation);
		}
	}

	void PrintRPlot() {
		cartesian point, buffer;
		std::vector<double> xList, yList;
		std::vector<std::pair<std::array<double,2>, std::array< double, 2>>> lines;
		//double line1[2], line2[2];
		std::array<double, 2> line1, line2 ;
		std::vector<TreeNode*> nodeStack;

		TreeNode* tnp = nullptr;
		if (tnp != nullptr){}
			nodeStack.push_back(root);
		while (!nodeStack.empty()) {
            tnp = nodeStack.back();
            nodeStack.pop_back();

			point = to_cartesian(tnp->point);
			xList.push_back(point.X);
			yList.push_back(point.Y);


			if (tnp->left != nullptr) {
				buffer = to_cartesian(tnp->left->point);
				xList.push_back(buffer.X);
				yList.push_back(buffer.Y);
				line1[0] = point.X;
				line2[0] = point.Y;
				line1[1] = buffer.X;
				line2[1] = buffer.Y;

				lines.push_back(std::make_pair(line1,line2));
				nodeStack.push_back(tnp->left);
			}
			if (tnp->right != nullptr) {
				buffer = to_cartesian(tnp->right->point);
				xList.push_back(buffer.X);
				yList.push_back(buffer.Y);
				line1[0] = point.X;
				line2[0] = point.Y;
				line1[1] = buffer.X;
				line2[1] = buffer.Y;
				lines.push_back(std::make_pair(line1, line2));
				nodeStack.push_back(tnp->right);
			}

		}
		std::cout << "plot(c(";
		for (unsigned i = 0; i < xList.size(); i++) {
			std::cout << std::to_string(xList[i]);
			if (i != xList.size() - 1)
				std::cout << ", ";
		}
		std::cout << "), c(";
		for (unsigned i = 0; i < yList.size(); i++) {
			std::cout << std::to_string(yList[i]);
			if (i != yList.size() - 1)
				std::cout << ", ";
		}
		std::cout << "))" << std::endl;

		for (auto& lp : lines) {
			std::cout << "lines(c(" << std::to_string(lp.first[0]) << ", " << std::to_string(lp.first[1]) << "), c("
				<< std::to_string(lp.second[0]) << ", " << std::to_string(lp.second[1]) << "))" << std::endl;
			
		}
		
	}

};

// This function tests the angle of a node relative to its parent in both trees.
bool TestValue(TreeNode* const& A_Parent_Node, TreeNode* const& A_Node, TreeNode* const& B_Parent_Node, TreeNode* const& B_Node, unsigned short Rotation) {
	if (A_Parent_Node == nullptr && B_Parent_Node == nullptr)
		return true;
	else if (A_Parent_Node == nullptr || B_Parent_Node == nullptr)
		return false;
	else if (UnitAngleFactor(A_Node->point - A_Parent_Node->point, B_Node->point - B_Parent_Node->point) == Rotation)
		return true;
	else
		return false;
}


bool SubCompare(TreeNode* const& A_Parent_Node, TreeNode* const& A_Node, TreeNode* const& B_Parent_Node, TreeNode* const& B_Node, unsigned short Rotation) {
	if (A_Node != nullptr && B_Node != nullptr) {
		if (TestValue(A_Parent_Node, A_Node, B_Parent_Node, B_Node, Rotation)) {
			if (A_Node->is_leaf() && B_Node->is_leaf())
				return true;
			else if (A_Node->is_leaf() || B_Node->is_leaf())
				return false;
			else if (!A_Node->is_leaf() && !B_Node->is_leaf()) {
				if (A_Node->right == nullptr && B_Node->right == nullptr) {
					return SubCompare(A_Node, A_Node->left, B_Node, B_Node->left, Rotation);
				}
				else if (UnitAngleFactor(A_Node->point - A_Node->left->point, A_Node->point - A_Node->right->point)
					== UnitAngleFactor(B_Node->point - B_Node->left->point, B_Node->point - B_Node->right->point)) {
					return SubCompare(A_Node, A_Node->left, B_Node, B_Node->left, Rotation) &&
						SubCompare(A_Node, A_Node->right, B_Node, B_Node->right, Rotation);
				}
				else if (UnitAngleFactor(A_Node->point - A_Node->left->point, A_Node->point - A_Node->right->point)
					== UnitAngleFactor(B_Node->point - B_Node->right->point, B_Node->point - B_Node->left->point)) {
					return SubCompare(A_Node, A_Node->left, B_Node, B_Node->right, Rotation) &&
						SubCompare(A_Node, A_Node->right, B_Node, B_Node->left, Rotation);
				}
				else
					return false;
			}
		}
		else
			return false;
	}
	else if (A_Node != nullptr || B_Node != nullptr)
		return false;
	else
		return true;
}


bool Compare(TriangleTree const& This, TriangleTree const& Other) {
	unsigned short Rotation;
	TreeNode* A_Node = nullptr;
	TreeNode* B_Node = nullptr;
	bool Value = true;
	
	if (This.root == nullptr && Other.root == nullptr)
		return true;
	else if (This.root == nullptr || Other.root == nullptr)
		return false;
	else if (This.root->is_leaf() && Other.root->is_leaf())
		return true;
	else if (This.root->is_leaf() || Other.root->is_leaf())
		return false;
	else if (This.root->is_full() && Other.root->is_full()) {
		Rotation = UnitAngleFactor(This.root->left->point - This.root->point, Other.root->left->point - Other.root->point);
		Value = SubCompare(This.root, This.root->left, Other.root, Other.root->left, Rotation) &&
			SubCompare(This.root, This.root->right, Other.root, Other.root->right, Rotation);
		if (Value == false) {
			Rotation = UnitAngleFactor(This.root->left->point - This.root->point, Other.root->right->point - Other.root->point);
			Value = SubCompare(This.root, This.root->left, Other.root, Other.root->right, Rotation) &&
				SubCompare(This.root, This.root->right, Other.root, Other.root->left, Rotation);

		}
		return Value;
	}
	else 
		return false;
}




void Reduce(std::vector<TriangleTree>& TVec) {
	TriangleTree A_Tree, Buf_T;
	unsigned long Tree_Index, T_I = 0;
	//std::vector<TriangleTree> Out_TVec;

	if (!TVec.empty()) {
		T_I = 0;
		while (T_I < TVec.size()) {
			Buf_T = TVec[T_I];
			Tree_Index = T_I;
			while (Tree_Index < TVec.size()) {

				if (T_I != Tree_Index) {
					A_Tree = TVec[Tree_Index];

					if (Compare(Buf_T, A_Tree) == true) {
						TVec.erase(TVec.begin() + Tree_Index);
					}
					else
						++Tree_Index;
				}
				else
					++Tree_Index;
			}
			++T_I;
		}
	}
}
