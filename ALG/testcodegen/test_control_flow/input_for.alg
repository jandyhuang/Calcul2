def int main()
{
	int i;
	line l1;
	line l2;
	i=0;
	for(i=0;i<5;i=i+1)
	{
		l1=[[1;1],[2;2]];
		l2=[[1;2],[2;i]];
		if(l1//l2)
		{
		print(i);
		print_newline();
		print("l1 parallel l2");
		print_newline();
		display(l2);
		}
	}

}
