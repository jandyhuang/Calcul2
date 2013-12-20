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
	
	f.AddNode(new FNode(T_OP,0,POWER));
	f.AddNode(new FNode(T_VAR,0,0));
	f.AddNode(new FNode(T_VAR,0,0));
	
	vector<string> g_var;
	g_var.push_back("x");
	vector<double> g_begin, g_end, g_now;
	FTree g(g_var);
	
	g.AddNode(f.Derive("x") -> Copy());
	
	f.Print();
	cout << "\n";
	
	g.Print();
	cout << "\n";
	
	f_now.clear();
	f.Derive("x") -> Print();
	cout << "\n";
	
	f_begin.clear();
	f_end.clear();
	f_begin.push_back(2.);
	f_end.push_back(5.);
	cout << f.GetIntegal(f_begin, f_end);
	cout << "\n";
	
	return 0;
}
