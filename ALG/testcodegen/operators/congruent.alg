def int main()
{
	polygon poly1;
	polygon poly2;
	polygon poly3;
	polygon poly4;
	poly1=[[0;0],[0;1],[1;1],[1;0]];
	poly2=[[0;0],[1;1],[1;0]];
	poly3=[[0;0],[0;1],[1;1]];
	poly4=[[0;0],[0;2],[2;2],[2;0]];
	if(poly1~=poly2)
	{print("poly1 is congruent to poly2");
	print_newline();}
	if(poly1~=poly4)
	{print("poly1 is congruent to poly4");
	print_newline();
	}
	if(poly2~=poly3)
	{print("poly2 is congruent to poly3");
	print_newline();
	}

	
	
}
