# triangle_trees
Binary trees overlaid on a triangular tessellation of the plane.

Tried the first time on Ada. Observed the results (Degree 1: 30, Degree 2: 7728, then memory saturation)
and translated to C++. Same result.

Modified the C++ code to look for strictly unique trees under translation, rotation and reflection.

Reads:
<br>Number of Trees of Degree 1: 3
<br>Number of Trees of Degree 2: 230

But for the third degree, the Reduce() function has to manage over three million trees, taking about one minute per tree on my computer. This means it would take about 6 years to just return the reduced list of trees of degree 3.
