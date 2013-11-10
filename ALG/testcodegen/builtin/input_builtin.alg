def int main()
{
	line l1;
	ellipse e1;
	line l2;
	polygon poly1;
	point p1;
	point p2;
	float f;
	p1=[1;2];
	p2=[1;3];
	l2=[[1;2],[3;3]];
	l1=[[1;1],[1;2]];
	e1={[1;1],2,1};
	poly1=[[1;1],[2;2],[3;3],[5;6],[7;8]];
	display(l1);
	Move(l1,1,2);
	display(l1);
	display(e1);
	Move(e1,1,2);
	display(e1);
	display(p1);
	Move(p1,1,2);
	display(p1);
	display(poly1);
	Move(poly1,1,2);
	display(poly1);
	Area(poly1);
	Area(e1);
	Perimeter(poly1);
	Perimeter(e1);
	f=getAngle(l1,l2);
	f=getDis(p1,p2);
}
