(!In this code, we test another case of wrongly use of binop operation!)
(!The result is exception Failure("Illegal type used in a binop expression")!)


ellipse a;
ellipse b;
polygon p1;

def int main()
{
     p1 =  (a == b);
     return 0;
}