def int main()
{
	boolean a;
	line l1;
	line l2;
	line l3;
	line l4;
	(!l1 is parallel with l2, l1 is not parallel with l4!)
	l1=[[1;1],[2;2]];
	l2=[[1;2],[2;3]];
	l3=[[1;1],[2;2]];
	l4=[[1;2],[2;2]];
	(!this should be false!)
	if((l1//l2)and(l2//l4)) {print("yes1");print_newline();}
	(!this should be true!)
	if((l1//l2)or(l2//l4)){print("yes2");print_newline();}
	(!this should be false!)
	a=not(l1//l2);
	if(a){print("yes3");print_newline();}
}
