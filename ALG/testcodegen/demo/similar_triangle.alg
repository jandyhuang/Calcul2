def int main()
{
	polygon triangle;
	polygon triangle1;
	int i;
	int j;
	i=2;
	j=1;
	triangle=[[0;0],[i;0],[0;j]];
	triangle1=triangle;
	while(i<50)
	{
		Draw(triangle1);
		print(Area(triangle1));
		print_newline();
		i=i*2;
		j=j*2;
		triangle1=[[0;0],[i;0],[0;j]];
		
	}
	if(triangle1~triangle)
		{print("similar");
		print_newline();}
}
