#include <cstdio>
#include "calcul2.h"
using namespace std;

double fibo(double x)
{
	if (x < 2.)
return 1.;

	return fibo(x - 1.) + fibo(x - 2.);

}

int main()
{
	double printer;

	cout << fibo(0.)<<endl;
	
	cout << fibo(1.)<<endl;
	
	cout << fibo(5.)<<endl;
	
	return 0;
}
