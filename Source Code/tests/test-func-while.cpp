#include <cstdio>
#include "calcul2.h"
using namespace std;

double f(double x, double y)
{
	while (x != y) {
if (x > y)
x = x - y;
else
y = y - x;
}

	return x + y;

}

int main()
{
	double printer;

	cout << f(15., 20.)<<endl;
	
	return 0;
}
