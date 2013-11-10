def int main()
{
	ellipse e1;
	line l1;
	int i;
	i=50;
	e1={[0;0],10,10};
	l1=[[50;0],[50;1]];
	Draw(e1);
	while(i>=0)
	{
		Draw(l1);
		if((l1|-e1):="tangent")
		{
			print("tangent");
			print_newline();
			display(l1);
			done;
		}
		Move(l1,-5,0);
	}
	
}
