def int main()
{
	point p1;
	line l1;
	ellipse e1;
	polygon poly1;
	p1=[0;0];
	l1=[[1;1],[2;2]];
	e1={[0;0],1,1};
	poly1=[[0;0],[1;1],[2;1],[1;0]];
	print("The original figures are as follows ");
	print_newline();
	display(p1);
	display(l1);
	display(e1);
	display(poly1);
	Move(p1,1.5,2.0);
	Move(l1,1.5,2.0);
	Move(e1,1.5,2.0);
	Move(poly1,1.5,2.0);
	print("After move they are ");
	print_newline();
	display(p1);
	display(l1);
	display(e1);
	display(poly1);
}
