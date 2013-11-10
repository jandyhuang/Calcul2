def int main()
{
	line l1;
	line l2;
	point p1;
	point p2;
	p1=[0;0];
	p2=[1;1];
	l1=[[0;0],[2;0]];
	l2=[[1;1],[2;2]];
	print("the angle of the two lines are ");
	print(getAngle(l1,l2));
	print_newline();
	print("the distance of the two points are ");
	print(getDis(p1,p2));
	print_newline();
}
