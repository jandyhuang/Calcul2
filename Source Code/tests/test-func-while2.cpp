#include <cstdio>
#include "calcul2.h"
using namespace std;

double f(double x)
{
	double f = 0.;

	while (!(x) == 0.) {
f = f + x;
x = x - 1.;
}

	return f;

}

int main()
{
	double printer;

	cout << f(5.)<<endl;
	
	return 0;
}
