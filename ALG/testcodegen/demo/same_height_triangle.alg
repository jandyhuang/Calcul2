def int main()
{
	polygon triangle;
	int i;
	int j;
	int k;
	k=-10;
	j=20;
	i=2;
	triangle=[[k;0],[0;j],[i;0]];
	for (i=2;i<50;i=i*2)
	{
		triangle=[[k;0],[0;j],[i;0]];
		print(Area(triangle));
		print_newline();
		Draw(triangle);
	}
}
