!!move11 function to move a point by vector(1,1)
def void move11(point p)
{
	Move(p,1,1);
}

!!test global function declaration and reference
def int main()
{
	point p;
	p=[0;0];
	display(p);
	move11(p);
	display(p);
}
