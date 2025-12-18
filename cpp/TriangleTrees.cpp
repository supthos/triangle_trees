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

	std::cout << "Number of Trees of Degree 1: " << T_Vector.size() << "\n";

	T_Vector = A_Tree.NextDegree(T_Vector);

	std::cout << "Number of Trees of Degree 2: " << T_Vector.size() << "\n";

	T_Vector = A_Tree.NextDegree(T_Vector);

	std::cout << "Number of Trees of Degree 3: " << T_Vector.size() << "\n";

	T_Vector = A_Tree.NextDegree(T_Vector);

	std::cout << "Number of Trees of Degree 4: " << T_Vector.size() << "\n";


    std::cout << "Hello World!\n";
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
