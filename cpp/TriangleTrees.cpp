// TriangleTrees.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include "TriangleTrees.h"

int main()
{
	TriangleTree A_Tree, B_Tree;
	std::vector<TriangleTree> T_Vector;

	A_Tree.InsertPoint(directions::O);

	T_Vector.push_back(A_Tree);
	T_Vector = A_Tree.NextDegree(T_Vector);
	Reduce(T_Vector);

	std::cout << "Number of Trees of Degree 1: " << T_Vector.size() << "\n";

	T_Vector = A_Tree.NextDegree(T_Vector);
	Reduce(T_Vector);
	std::cout << "Number of Trees of Degree 2: " << T_Vector.size() << "\n";

	T_Vector = A_Tree.NextDegree(T_Vector);
	Reduce(T_Vector);
	std::cout << "Number of Trees of Degree 3: " << T_Vector.size() << "\n";

	T_Vector[0].PrintRPlot();

	//T_Vector = A_Tree.NextDegree(T_Vector);
	//Reduce(T_Vector); 
	//std::cout << "Number of Trees of Degree 4: " << T_Vector.size() << "\n";


    std::cout << "Hello World!\n";
}

