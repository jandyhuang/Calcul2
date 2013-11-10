(!This codes include a function whose return type is wrong!)
(!The result is exception Failure("Unmatched return type with function's defination!")
!)
int a;

def ellipse foo(int b, ellipse e1)
{
     b = 2;
     return b;
}

def int main()
{
     ellipse e2;
     ellipse e3;

     e3 = foo(2, e2);
    return 0;
}