#pragma once
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
		if (left != nullptr) {
			left->Delete();
			delete left;
			left = nullptr;
		}
		if (right != nullptr) {
			right->Delete();
			delete right;
			right = nullptr;
		}
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
					//root->left->InsertPoint(pos);
					buf_node = buf_node->left;
				}
				else {
					buf_node = buf_node->right;
					//root->right->InsertPoint(pos);
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
	std::vector<tess_point> GetPoints(TreeNode* tnp) {
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

		LeafPoints.clear();

		LeafPoints = GetLeafPoints(InTree.root);
		unsigned LeafIndex = 0;

		while (LeafIndex < LeafPoints.size()) {
			for (const TriangleTree& BTree : Tree_Vec) {
				Buf_T = BTree;
				Point = LeafPoints[LeafIndex];
				Child_Buf = ProcessLeaf(Point, GetPoints(Buf_T.root));
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
};