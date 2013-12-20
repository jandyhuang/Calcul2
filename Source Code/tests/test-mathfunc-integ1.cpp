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
	f.AddNode(new FNode(T_OP,0,POWER));
	f.AddNode(new FNode(T_VAR,0,0));
	f.AddNode(new FNode(T_VAL,2.));
	f.AddNode(new FNode(T_VAR,0,0));
	
	f_begin.clear();
	f_end.clear();
	f_begin.push_back(3.);
	f_end.push_back(10.);
	cout << f.GetIntegal(f_begin, f_end);
	cout << "\n";
	
	return 0;
}
