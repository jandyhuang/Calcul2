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
	
	f.AddNode(new FNode(T_OP,0,MINUS));
	f.AddNode(new FNode(T_OP,0,POWER));
	f.AddNode(new FNode(T_VAR,0,0));
	f.AddNode(new FNode(T_VAR,0,0));
	f.AddNode(new FNode(T_VAL,1.));
	
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
	
	g_begin.clear();
	g_end.clear();
	g_begin.push_back(1.);
	g_end.push_back(5.);
	cout << g.GetIntegal(g_begin, g_end);
	cout << "\n";
	
	f_now.clear();
	f_now.push_back(5.);
	printer = f.GetValue(f_now);
	printf("%lf\n",printer);
	cout << "\n";
	
	return 0;
}
