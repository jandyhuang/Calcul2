def int main()
{
	line l1;
	line l2;
	line l3;
	l1=[[0;0],[1.1;1.1]];
	l2=[[0;1],[1;2]];
	l3=[[0;0],[0;1]];
	{print("l1 and l2 ");
	print_newline();
	display((l1^l2));
	}
	if(l2//l3)
	{
	print("l3 and l2");
	print_newline();
	display((l2^l3));
	}
}
