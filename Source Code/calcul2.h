#include<cmath>
#include<cstdio>
#include<cstring>
#include<alogithm>
#include<iostream>
using namespace std;

#define T_VAL 0
#define T_VAR 1
#define T_OP 2



class FormulaTree{
private:
	int type;
	float value;
	int op;
	FormulaTree *left, *right;
public:
	FormulaTree(int _type, float _value, int _op, *_left, *_right);
	void Print();
	FormulaTree* GetLeft();	
	FormulaTree* GetRight();
	void SetLeft(FormulaTree*_left);
	void SetRight(FormulaTree*_right);
	FormularTree*Deride();
	float GetValue(vector<float>_var);
	float GetIntegal(vector<float>_begin, vector<float>_end);
};
