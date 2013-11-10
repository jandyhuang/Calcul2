!!test global variable

point p;

!!move11 function to move a point by vector(1,1)

def void move11()
{
	Move(p,1,1);
}

!!test global function declaration and reference
def int main()
{
	p=[0;0];
	display(p);
	move11();
	display(p);
}
