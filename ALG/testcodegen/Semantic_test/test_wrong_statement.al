(! This code has a wrong statement in the body of function foo!)
(! The result is exception Failure("There is a statement in function foo which is not a function call or assignment, which is invalid!")!)


ellipse e1;

def void foo(ellipse e1, polygon p1)
{
   line l1;
   line l2;
   e1 = {[3;2],2,2};
   l1 // l2 ;  (!This is an invalid statement!)
}

def int main()
{
   return 0;
}