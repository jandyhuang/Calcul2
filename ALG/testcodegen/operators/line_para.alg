def int main()
{
	line l1;
	line l2;
	line l3;
	l1=[[0;0],[1.1;1.1]];
	l2=[[0;1],[1;2]];
	l3=[[0;0],[0;1]];
	if(l1//l2)
	{print("l1 is parallel with l2");
	print_newline();
	}
	if(l2//l3)
	{
	print("l3 is parallel with l2");
	print_newline();
	}
}
