#include <cstdio>
#include "calcul2.h"
using namespace std;

double gcd(double x, double y)
{
	while (x != y) {
if (x > y)
x = x - y;
else
y = y - x;
}

	return x;

}

int main()
{
	double printer;

	cout << gcd(24., 36.)<<endl;
	
	return 0;
}
