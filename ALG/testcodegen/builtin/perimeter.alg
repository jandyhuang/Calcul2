def int main()
{
	ellipse e1;
	polygon poly1;
	e1={[2.3;1],1,1};
	poly1=[[0;0],[2;0],[1;1]];
	print("the perimeter of the ellipse is ");	
	print(Perimeter(e1));
	print_newline();
	print("the perimeter of the polygon is ");
	print(Perimeter(poly1));
	print_newline();
}
