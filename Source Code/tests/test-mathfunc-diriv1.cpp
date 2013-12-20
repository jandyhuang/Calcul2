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
	f.AddNode(new FNode(T_OP,0,POWER));
	f.AddNode(new FNode(T_VAR,0,0));
	f.AddNode(new FNode(T_VAL,3.));
	f.AddNode(new FNode(T_OP,0,TIMES));
	f.AddNode(new FNode(T_VAL,3.));
	f.AddNode(new FNode(T_VAR,0,0));
	
	f_now.clear();
	f.Derive("x") -> Print();
	cout << "\n";
	
	return 0;
}
