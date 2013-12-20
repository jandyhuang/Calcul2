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
	
	f.AddNode(new FNode(T_OP,0,TIMES));
	f.AddNode(new FNode(T_VAL,2.));
	f.AddNode(new FNode(T_VAR,0,0));
	
	vector<string> g_var;
	g_var.push_back("x");
	vector<double> g_begin, g_end, g_now;
	FTree g(g_var);
	
	g.AddNode(new FNode(T_OP,0,TIMES));
	g.AddNode(new FNode(T_VAL,3.));
	g.AddNode(new FNode(T_OP,0,SIN));
	g.AddNode(new FNode(T_VAR,0,0));
	
	vector<string> h_var;
	h_var.push_back("x");
	vector<double> h_begin, h_end, h_now;
	FTree h(h_var);
	
	h.AddNode(new FNode(T_OP,0,TIMES));
	h.AddNode(f.Copy());
	h.AddNode(g.Derive("x") -> Copy());
	
	h.Print();
	cout << "\n";
	
	h_now.clear();
	h.Derive("x") -> Print();
	cout << "\n";
	
	h_begin.clear();
	h_end.clear();
	h_begin.push_back(1.);
	h_end.push_back(3.);
	cout << h.GetIntegal(h_begin, h_end);
	cout << "\n";
	
	vector<string> q_var;
	q_var.push_back("x");
	vector<double> q_begin, q_end, q_now;
	FTree q(q_var);
	
	q.AddNode(new FNode(T_OP,0,SIN));
	q.AddNode(new FNode(T_VAR,0,0));
	
	double a = 3.;

	cout << pow(a, 2.);
	cout << "\n";
	
	return 0;
}
