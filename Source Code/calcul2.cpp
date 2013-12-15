#include <cstdio>
#include "calcul2.h"
using namespace std;

int main(){
  double printer;

  vector<string> f_var;
  f_var.push_back("x");
  f_var.push_back("y");
  vector<double> f_begin, f_end, f_now;
  FTree f(f_var);
  f.AddNode(new FNode(T_OP,0,PLUS)); //T_OP for operator
  f.AddNode(new FNode(T_OP,0,TIMES));  // when T_OP calls, the second parameter should be 0
  f.AddNode(new FNode(T_VAR,0,0)); //T_VAR for variable, 0 for the first(x), 1 for the second(y)
  f.AddNode(new FNode(T_VAL,2)); // T_VAL for value
  f.AddNode(new FNode(T_OP,0,TIMES));  
  f.AddNode(new FNode(T_VAR,0,1)); // when T_VAR calls, the second parameter should be 0
  f.AddNode(new FNode(T_VAL,3));
   
  f_now.clear();
  f_now.push_back(1);
  f_now.push_back(2);
  printer=f.GetValue(f_now);
  printf("%lf\n",printer);
  return 0;
}
