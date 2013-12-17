#include<cmath>
#include<cstdio>
#include<cstring>
#include<algorithm>
#include<iostream>
#include<vector>
#include<string>
using namespace std;

#define T_VAL	0
#define T_VAR	1
#define T_OP	2

#define PLUS	1
#define MINUS	2
#define TIMES	3
#define DIVIDE	4
#define POWER	5
#define SQRT	8
#define SIN	9
#define COS	10
#define TAN	11
#define ASIN	12
#define ACOS	13
#define ATAN	14
#define LOG	15
#define LN	16

#define TOT	1e5

int sgn(double v){
	if(fabs(v)<1e-8)return 0;
	return v>0?1:-1;
}

void PrintOp(int op){
	switch(op){
		case PLUS:
			printf("+");break;
		case MINUS:
			printf("-");break;
		case TIMES:
			printf("*");break;
		case DIVIDE:
			printf("/");break;
		case POWER:
			printf("^");break;
		case SQRT:
			printf("sqrt");break;
		case SIN:
			printf("sin");break;
		case COS:
			printf("cos");break;
		case TAN:
			printf("tan");break;
		case ASIN:
			printf("asin");break;
		case ACOS:
			printf("acos");break;
		case ATAN:
			printf("atan");break;
		case LOG:
			printf("log");break;
		case LN:
			printf("ln");break;
		default:
			break;
	}
}

class FNode{
private:
	int type;
	double value;
	int op;
	FNode *left, *right;
public:
	FNode(int _type, double _value, int _op, FNode *_left, FNode *_right);
	FNode *Copy();
	bool IsSame(FNode *tre);
	FNode* GetLeft();	
	FNode* GetRight();
	void Destroy();
	void SetLeft(FNode*_left);
	void SetRight(FNode*_right);
	void Print(int LastOp, vector<string>variable);
	double GetValue(vector<double>_var);
	void Reduce();
	FNode*Derive(int _var);
	bool AddNode(FNode*node);
};

FNode::FNode(int _type=0, double _value=0, int _op=0, FNode *_left=NULL, FNode*_right=NULL):type(_type),
	value(_value),op(_op),left(_left),right(_right){}

FNode *FNode::Copy(){
	FNode *tmp=new FNode(type,value,op);
	if(left!=NULL)tmp->left=left->Copy();
	if(right!=NULL)tmp->right=right->Copy();
	return tmp;
}

bool FNode::IsSame(FNode *tre){
	if((left==NULL) ^ (tre->left==NULL))return false;
	if((right==NULL)^(tre->right==NULL))return false;
	if(type!=tre->type)return false;
	if(type==T_VAL)return(sgn(value-tre->value)==0);
	if(type==T_VAR)return(op==tre->op);
	if(op!=tre->op)return false;
	if(left!=NULL && !left->IsSame(tre->left))return false;
	if(right!=NULL && !right->IsSame(tre->right))return false;
	return true;
}

FNode* FNode::GetLeft(){
	return left;
}

FNode* FNode::GetRight(){
	return right;
}

void FNode::Destroy(){
	if(left!=NULL)left->Destroy();
	if(right!=NULL)right->Destroy();
	delete this;
}

void FNode::SetLeft(FNode* _left){
	if(left!=NULL)left->Destroy();
	left=_left;
}

void FNode::SetRight(FNode* _right){
	if(right!=NULL)right->Destroy();
	right=_right;
}

void FNode::Print(int LastOp, vector<string>variable){
	if(type==T_VAL){
		if(sgn(value-int(value+1e-8))==0 || sgn(value-int(value-1e-8))==0)
			printf("%.0lf",value);
		else printf("%.3lf", value);
	}else if(type==T_VAR){
		printf("%s", variable[op].c_str());
	}else{
		if(op>POWER){
			PrintOp(op);
			printf("(");
			if(right!=NULL)right->Print(op,variable);
			printf(")");
		}else if(op>=PLUS && op<=POWER && LastOp>=PLUS && LastOp<=POWER && (LastOp>op || LastOp==POWER) && (LastOp+1)/2>=(op+1)/2){
			printf("(");
			if(op>=PLUS && op<=POWER && left!=NULL)left->Print(op,variable);
			PrintOp(op);
			if(right!=NULL)right->Print(op,variable);
			printf(")");
		}
		else{
			if(left!=NULL)left->Print(op,variable);
			PrintOp(op);
			if(right!=NULL)right->Print(op,variable);
		}
	}
}

double FNode::GetValue(vector<double>_var){
	if(type==T_VAL){
		return value;
	}else if(type==T_VAR){
		return _var[op];
	}else{
		double v_left=0,v_right=0;
		if(op>=PLUS && op<=POWER)v_left=left->GetValue(_var);
		v_right=right->GetValue(_var);
		if(isnan(v_left) || isnan(v_right))return NAN;
		switch(op){
			case PLUS:
				return v_left+v_right;
			case MINUS:
				return v_left-v_right;
			case TIMES:
				return v_left*v_right;
			case DIVIDE:
				if(sgn(v_right)==0){
					printf("Math Error: Divide By ZERO.\n");
					return NAN;
				}
				return v_left/v_right;
			case POWER:
				if(sgn(v_right)==0)v_right=0;
				if(sgn(v_right)<0){
					printf("Math Error: Power of negative.\n");
					return NAN;
				}
				return pow(v_left,v_right);
			case SQRT:
				if(sgn(v_right)==0)v_right=0;
				if(sgn(v_right)<0){
					printf("Math Error: Power of negative.\n");
					return NAN;
				}
				return sqrt(v_right);
			case SIN:
				return sin(v_right);
			case COS:
				return cos(v_right);
			case TAN:
				return tan(v_right);
			case ASIN:
				if(sgn(v_right-1)==0)v_right=1;
				if(sgn(v_right+1)==0)v_right=-1;
				if(fabs(v_right)>1){
					printf("Math Error: ASIN with number out of [-1,1].\n");
					return NAN;
				}
				return asin(v_right);
			case ACOS:
				if(sgn(v_right-1)==0)v_right=1;
				if(sgn(v_right+1)==0)v_right=-1;
				if(fabs(v_right)>1){
					printf("Math Error: ACOS with number out of [-1,1].\n");
					return NAN;
				}
				return acos(v_right);
			case ATAN:
				return atan(v_right);
			case LOG:
				if(sgn(v_right)==0)v_right=0;
				if(sgn(v_right)<0){
					printf("Math Error: LOG with negative number.\n");
					return NAN;
				}
				return log(v_right)/log(2);
			case LN:
				if(sgn(v_right)==0)v_right=0;
				if(sgn(v_right)<0){
					printf("Math Error: LN with negative number.\n");
					return NAN;
				}
				return log(v_right);
		}
	}
}

void FNode::Reduce(){
	if(type==T_VAL){
		return;
	}else if(type==T_VAR){
		return;
	}else{
		if(op>=PLUS && op<=POWER){
			left->Reduce();
		}
		right->Reduce();
		if(op==PLUS && left->IsSame(right)){
			op=TIMES;
			left->type=T_VAL;
			left->value=2;
			if(left->left!=NULL)left->left->Destroy();
			if(left->right!=NULL)left->right->Destroy();
			left->left=left->right=NULL;
		}
		if(op==PLUS && right->type==T_VAL && sgn(right->value)==0){
			right->Destroy();
			FNode*tmp=left;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==PLUS && left->type==T_VAL && sgn(left->value)==0){
			left->Destroy();
			FNode*tmp=right;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==MINUS && left->IsSame(right)){
			type=T_VAL;
			value=0;
			left->Destroy();
			right->Destroy();
			left=right=NULL;
			return;
		}
		if(op==MINUS && right->type==T_VAL && sgn(right->value)==0){
			right->Destroy();
			FNode*tmp=left;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==TIMES && (left->type==T_VAL && sgn(left->value)==0 || right->type==T_VAL && sgn(right->value)==0)){
			type=T_VAL;
			value=0;
			left->Destroy();
			right->Destroy();
			left=right=NULL;
			return;
		}
		if(op==TIMES && right->type==T_VAL && sgn(right->value-1)==0){
			right->Destroy();
			FNode*tmp=left;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==TIMES && left->type==T_VAL && sgn(left->value-1)==0){
			left->Destroy();
			FNode*tmp=right;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==TIMES && left->IsSame(right)){
			op=POWER;
			right->type=T_VAL;
			right->value=2;
			if(right->left!=NULL)right->left->Destroy();
			if(right->right!=NULL)right->right->Destroy();
			right->left=right->right=NULL;
			return;
		}
		if(op==DIVIDE && right->type==T_VAL && sgn(right->value-1)==0){
			right->Destroy();
			FNode*tmp=left;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==DIVIDE && left->type==T_VAL && sgn(left->value)==0){
			right->Destroy();
			FNode*tmp=left;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==DIVIDE && left->IsSame(right)){
			type=T_VAL;
			value=1;
			left->Destroy();
			right->Destroy();
			left=right=NULL;
			return;
		}
		if(op==POWER && right->type==T_VAL && sgn(right->value-1)==0){
			right->Destroy();
			FNode*tmp=left;
			(*this)=(*tmp);
			delete(tmp);
			return;
		}
		if(op==POWER && right->type==T_VAL && sgn(right->value)==0){
			type=T_VAL;
			value=1;
			left->Destroy();
			right->Destroy();
			left=right=NULL;
			return;
		}

		if((left==NULL || left->type==T_VAL) && right->type==T_VAL){
			type=T_VAL;
			vector<double> empty;
			value=GetValue(empty);
			left->Destroy();
			right->Destroy();
			left=right=NULL;
			return;
		}
	}
}

FNode* FNode::Derive(int _var){
	FNode *tmp=new FNode;
	if(type==T_VAL){
		tmp->type=T_VAL;
		tmp->value=0;
	}else if(type==T_VAR){
		tmp->type=T_VAL;
		tmp->value=(_var==op?1:0);
	}else{
		tmp->type=T_OP;
		switch(op){
			case PLUS:
				tmp->op=PLUS;
				tmp->left=left->Derive(_var);
				tmp->right=right->Derive(_var);
				break;
			case MINUS:
				tmp->op=MINUS;
				tmp->left=left->Derive(_var);
				tmp->right=right->Derive(_var);
				break;
			case TIMES:
				tmp->op=PLUS;
				tmp->left=new FNode(T_OP,0,TIMES);
				tmp->left->left=left->Derive(_var);
				tmp->left->right=right->Copy();
				tmp->right=new FNode(T_OP,0,TIMES);
				tmp->right->left=left->Copy();
				tmp->right->right=right->Derive(_var);
				break;
			case DIVIDE:
				tmp->op=DIVIDE;
				tmp->left=new FNode(T_OP,0,MINUS);
				tmp->left->left=new FNode(T_OP,0,TIMES);
				tmp->left->left->left=left->Derive(_var);
				tmp->left->left->right=right->Copy();
				tmp->left->right=new FNode(T_OP,0,TIMES);
				tmp->left->right->left=left->Copy();
				tmp->left->right->right=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,POWER);
				tmp->right->left=right->Copy();
				tmp->right->right=new FNode(T_VAL,2);
				break;
			case POWER:
				tmp->op=TIMES;
				tmp->left=new FNode(T_OP,0,POWER);
				tmp->left->left=left->Copy();
				tmp->left->right=right->Copy();
				tmp->right=new FNode(T_OP,0,PLUS);
				tmp->right->left=new FNode(T_OP,0,DIVIDE);
				tmp->right->left->left=new FNode(T_OP,0,TIMES);
				tmp->right->left->left->left=right->Copy();
				tmp->right->left->left->right=left->Derive(_var);
				tmp->right->left->right=left->Copy();
				tmp->right->right=new FNode(T_OP,0,TIMES);
				tmp->right->right->left=new FNode(T_OP,0,LN);
				tmp->right->right->left->right=left->Copy();
				tmp->right->right->right=right->Derive(_var);
				break;
			case SQRT:
				tmp->op=DIVIDE;
				tmp->left=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,TIMES);
				tmp->right->left=new FNode(T_VAL,2);
				tmp->right->right=new FNode(T_OP,0,SQRT);
				tmp->right->right->right=right->Copy();
				break;
			case SIN:
				tmp->op=TIMES;
				tmp->left=new FNode(T_OP,0,COS);
				tmp->left->right=right->Copy();
				tmp->right=right->Derive(_var);
				break;
			case COS:
				tmp->op=TIMES;
				tmp->left=new FNode(T_OP,0,TIMES);
				tmp->left->left=new FNode(T_VAL,-1);
				tmp->left->right=new FNode(T_OP,0,SIN);
				tmp->left->right->right=right->Copy();
				tmp->right=right->Derive(_var);
				break;
			case TAN:
				tmp->op=DIVIDE;
				tmp->left=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,POWER);
				tmp->right->left=new FNode(T_OP,0,COS);
				tmp->right->left->right=right->Copy();
				tmp->right->right=new FNode(T_VAL,2);
				break;
			case ASIN:
				tmp->op=DIVIDE;
				tmp->left=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,SQRT);
				tmp->right->right=new FNode(T_OP,0,MINUS);
				tmp->right->right->left=new FNode(T_VAL,1);
				tmp->right->right->right=new FNode(T_OP,0,POWER);
				tmp->right->right->right->left=right->Copy();
				tmp->right->right->right->right=new FNode(T_VAL,2);
				break;
			case ACOS:
				tmp->op=DIVIDE;
				tmp->left=new FNode(T_OP,0,TIMES);
				tmp->left->left=new FNode(T_VAL,-1);
				tmp->left->right=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,SQRT);
				tmp->right->right=new FNode(T_OP,0,MINUS);
				tmp->right->right->left=new FNode(T_VAL,1);
				tmp->right->right->right=new FNode(T_OP,0,POWER);
				tmp->right->right->right->left=right->Copy();
				tmp->right->right->right->right=new FNode(T_VAL,2);
				break;
			case ATAN:
				tmp->op=DIVIDE;
				tmp->left=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,PLUS);
				tmp->right->left=new FNode(T_VAL,1);
				tmp->right->right=new FNode(T_OP,0,POWER);
				tmp->right->right->left=right->Copy();
				tmp->right->right->right=new FNode(T_VAL,2);
				break;
			case LOG:
				tmp->op=DIVIDE;
				tmp->left=right->Derive(_var);
				tmp->right=new FNode(T_OP,0,TIMES);
				tmp->right->left=right->Copy();
				tmp->right->right=new FNode(T_VAL,log(2.0));
				break;
			case LN:
				tmp->op=DIVIDE;
				tmp->left=right->Derive(_var);
				tmp->right=right->Copy();
				break;
		}
	}
	return tmp;
}

bool FNode::AddNode(FNode*node){
	if(type!=T_OP)return 0;
	if(op>=PLUS && op<=POWER){
		if(left==NULL){
			left=node;
			return 1;
		}
		if(left->AddNode(node))return 1;
	}
	if(right==NULL){
		right=node;
		return 1;
	}
	return right->AddNode(node);
}

class FTree{
private:
	vector<string> variable;
	FNode*root;
	double GetIntegal(vector<double>_begin,vector<double>_end,vector<double>_now,int _var);
public:
	FTree(vector<string>_variable, FNode*_root);
	FNode*Copy();
	void AddNode(FNode*node);
	void Print();
	FTree*Derive(int _var);
	double GetIntegal(vector<double>_begin, vector<double>_end);
	double GetValue(vector<double>_var);
	void Reduce();
};

FNode*FTree::Copy(){
	return root->Copy();
}

void FTree::Reduce(){
	root->Reduce();
}

FTree::FTree(vector<string>_variable, FNode*_root=NULL):variable(_variable),root(_root){}

void FTree::AddNode(FNode*node){
	if(root==NULL)root=node;
	else root->AddNode(node);
}

void FTree::Print(){
	root->Print(-1,variable);
}

FTree* FTree::Derive(int _var){
	vector<string> tmp_variable;
	tmp_variable.push_back(variable[_var]);
	FTree*tmp=new FTree(tmp_variable,root->Derive(_var));
	tmp->root->Reduce();
	return tmp;
}

double FTree::GetValue(vector<double>_var){
	return root->GetValue(_var);
}

double FTree::GetIntegal(vector<double>_begin, vector<double>_end,vector<double>_now, int _var){
	int n=_begin.size();
	double ans=0;
	int cnt=(int)pow(TOT,1.0/n);
	_now.push_back(0);
	for(int i=0;i<cnt;i++){
		_now[_var]=_begin[n-1]+(_end[n-1]-_begin[n-1])/cnt;
		ans+=(_var==n-1?GetValue(_now)/TOT:GetIntegal(_begin,_end, _now, _var+1));
	}
	return ans;
}

double FTree::GetIntegal(vector<double>_begin, vector<double>_end){
	vector<double>_now;
	return GetIntegal(_begin,_end,_now,0);
}
