def int main()
{
	polygon poly1;
	polygon poly2;
	ellipse e1;
	ellipse e2;
	poly1=[[0;0],[0;1],[1;1],[1;0]];
	poly2=[[0;0],[1;1],[1;0]];
	e1={[0;0],1,1};
	e2={[1;1],2,2};
	if(poly1>>=poly2)
	{print("poly1 has perimeter larger than or equal to poly2");
	print_newline();}
	else
	{print("poly1 has perimeter less than poly2");}
	if(e1<<=e2)
	{print("e1 has perimeter less than or equal e2");
	print_newline();}
	else
	{print("e1 has perimeter larger than  e2");}
	
	
}
