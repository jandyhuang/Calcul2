def int main()
{
	ellipse e1;
	ellipse e2;
	int i;
	i=25;
	e1={[0;0],10,10};
	e2={[i;0],5,5};
	Draw(e1);
	while(i>=0)
	{
		i=i-.5;
		Draw(e2);
		display(e2);
		print(e1|-e2);
		print_newline();
		Move(e2,-5,0);
	}
	
}
