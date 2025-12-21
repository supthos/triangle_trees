#pragma once
#include <cmath>
#include <numbers>
#include <vector>
#include <string>
#include <stdexcept>

using namespace std::numbers;

struct tess_point
{
	double A, B, C = 0.0;
	constexpr tess_point(double a, double b, double c)
		: A(a), B(b), C(c) {}
};

void set(tess_point& T, double a, double b, double c) {
	T.A = a;
	T.B = b;
	T.C = c;
}

void set (tess_point& T, tess_point const& S) {
	T.A = S.A;
	T.B = S.B;
	T.C = S.C;
}


tess_point operator +(tess_point const& L, tess_point const& R) {
	return tess_point{ L.A + R.A,  L.B + R.B,  L.C + R.C };
}

tess_point operator -(tess_point const& L, tess_point const& R) {
	return tess_point{ L.A - R.A,  L.B - R.B,  L.C - R.C };
}

bool operator==(tess_point const& L, tess_point const& R) {
	return L.A == R.A && L.B == R.B && L.C == R.C;
}

bool operator!=(tess_point const& L, tess_point const& R) {
	return !(L == R);
}

struct directions  {
	static constexpr tess_point O{ 0.0, 0.0, 0.0 };
	static constexpr tess_point P{ 1.0, -1.0, 0.0 };
	static constexpr tess_point Q{ 1.0, 0.0, -1.0 };
	static constexpr tess_point R{ 0.0, 1.0, -1.0 };
	static constexpr tess_point S{ -1.0, 1.0, 0.0 };
	static constexpr tess_point T{ -1.0, 0.0, 1.0 };
	static constexpr tess_point U{ 0.0, -1.0, 1.0 };

	// Add a function to get the name for a direction
	static std::string Name(const tess_point& dir) {
		if (dir == O) return "O";
		if (dir == P) return "P";
		if (dir == Q) return "Q";
		if (dir == R) return "R";
		if (dir == S) return "S";
		if (dir == T) return "T";
		if (dir == U) return "U";
		return "";
	}
};

bool is_vertex(tess_point const& T) {
	return (T.A + T.B + T.C == 0.0);
}

struct cartesian
{
	double X, Y = 0.0;
};

cartesian to_cartesian(tess_point const& T) {
	cartesian point;
	point.X = (sin(0.0) * T.A) + (sin(2.0 * pi / 3.0) * T.B) + (sin(4.0 * pi / 3.0) * T.C);
	point.Y = (cos(0.0) * T.A) + (cos(2.0 * pi / 3.0) * T.B) + (cos(4.0 * pi / 3.0) * T.C);
	return point;
}


bool is_adjacent(tess_point const& A, tess_point const& B) {
	tess_point diff = A - B;
	return diff == directions::P || diff == directions::Q ||
		diff == directions::R || diff == directions::S ||
		diff == directions::T || diff == directions::U;
}

std::vector<tess_point> Adjacents(tess_point const& T) {
	std::vector<tess_point> adjacents;
	adjacents.push_back(T + directions::P);
	adjacents.push_back(T + directions::Q);
	adjacents.push_back(T + directions::R);
	adjacents.push_back(T + directions::S);
	adjacents.push_back(T + directions::T);
	adjacents.push_back(T + directions::U);
	return adjacents;
}

double DotProduct(tess_point const& L, tess_point const& R) {
	return (L.A * R.A) + (L.B * R.B) + (L.C * R.C);
}

tess_point IntegerProduct(tess_point const& L, int k) {
	return tess_point{ L.A * k, L.B * k, L.C * k };
}

tess_point Translation(tess_point const& From, tess_point const& To) {
	return To - From;
}

tess_point UnitRotation(tess_point const& L, tess_point const& R) {
	if (is_vertex(L) && is_vertex(R)){
		return Translation(L, R) + Translation(L, directions::P);
	}
}

// Rotates a point around the origin by 60-degree increments.
tess_point RotatePoint(tess_point p, unsigned short steps) {
	steps = steps % 6; // Ensure 0-5 range
	switch (steps) {
	case 0: return p;
	case 1: return tess_point{ -p.B, -p.C, -p.A };
	case 2: return tess_point{ p.C, p.A, p.B };
	case 3: return tess_point{ -p.A, -p.B, -p.C };
	case 4: return tess_point{ p.B, p.C, p.A };
	case 5: return tess_point{ -p.C, -p.A, -p.B };
	}
	return p;
}

bool is_unit_point(tess_point const& V) {
	return V == directions::P || V == directions::Q ||
		V == directions::R || V == directions::S ||
		V == directions::T || V == directions::U;
}

unsigned short UnitVectorOrder(tess_point const& V) {
	if (V == directions::P) return 0;
	if (V == directions::Q) return 1;
	if (V == directions::R) return 2;
	if (V == directions::S) return 3;
	if (V == directions::T) return 4;
	if (V == directions::U) return 5;
	throw std::invalid_argument("The provided tess_point is not a unit vector.");
}

unsigned short UnitAngleFactor(tess_point const& L, tess_point const& R) {
	if (is_unit_point(L) && is_unit_point(R)) {
		return (UnitVectorOrder(R) + 6 - UnitVectorOrder(L)) % 6;
	}
	else {
		throw std::invalid_argument("Both provided tess_points must be unit vectors.");
	}
}


