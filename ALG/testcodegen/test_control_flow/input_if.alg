def int main()
{
	line l1;
	ellipse e1;
	l1=[[1;0],[1;1]];
	e1={[0;0],1,1};
	if((l1|-e1):="tangent")
		print("the line and ellipse are tangent");	
}
