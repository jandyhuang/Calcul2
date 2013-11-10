(!In this code, a function is defined but is wrongly used afterwards!)
(!The result is exception Failure("The number of parameters given when calling function foo is wrong!")!)
int a;
def ellipse foo(ellipse e3)
{
  return Move(e3,2,1);
}



def int main()
{
    ellipse e1;
    ellipse e2;

    e2 = foo(e1,2);
    return 0;
}