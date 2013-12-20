#include <cstdio>
#include "calcul2.h"
using namespace std;

int main()
{
	double printer;

	vector<string> f_var;
	f_var.push_back("x");
	vector<double> f_begin, f_end, f_now;
	FTree f(f_var);
	
	f.AddNode(new FNode(T_OP,0,PLUS));
	f.AddNode(new FNode(T_OP,0,TIMES));
	f.AddNode(new FNode(T_VAL,3.));
	f.AddNode(new FNode(T_VAR,0,0));
	f.AddNode(new FNode(T_VAL,2.));
	
	f_now.clear();
	f_now.push_back(4.);
	printer = f.GetValue(f_now);
	printf("%lf\n",printer);
	cout << "\n";
	
	return 0;
}
