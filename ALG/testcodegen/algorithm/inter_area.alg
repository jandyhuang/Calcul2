def int main()
{
	line l1;
	line l2;
	line l3;
	point p1;
	point p2;
	point p3;
	polygon poly;
	l1=[[1;1],[2;2]];
	l2=[[0;0],[2;0]];
	l3=[[2;2],[2;0]];
	(!display the information of the three lines!)
	display(l1);
	display(l2);
	display(l3);
	p1=l1^l2; !!p1 is the intersect point of l1 and l2
	p2=l2^l3; !!p2 is the intersect point of l2 and l3
	p3=l3^l1;	!!p3 is the intersect point of l1 and l3

	poly=[p1,p2,p3]; !! poly is the intersect area of the three lines
	print("the area of the intersection area is ");	
	print(Area(poly)); !!print the area of the intersection part of the three lines
	print_newline();
}
