(! This code has duplicated parameters in function's definition!)
(! The result is exception Failure("Duplicate parameter in function foo")!)
int a;
int b;

def void foo(int a, int b, ellipse a)
{
     print(a);
}

def int main()
{
   int i;
   return 0;
}