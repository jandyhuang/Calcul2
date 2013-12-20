#include <cstdio>
#include "calcul2.h"
using namespace std;

double f(double x)
{
	double s = 0.;

	if (x == 0.)
s = 0.;
else
s = f(x - 1.) + x;

	return s;

}

int main()
{
	double printer;

	cout << f(3.)<<endl;
	
	return 0;
}
