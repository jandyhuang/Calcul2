#include "alg.h"
#include "stdio.h"
#include"string.h"

int main ()
{
line * l1;
line * l2;
line * l3;
point * p1;
point * p2;
point * p3;
polygon * poly;
CvSize ImageSize1=cvSize(1000,1000);
Image1=cvCreateImage(ImageSize1,IPL_DEPTH_8U,3);
cvNot(Image1,Image1);
l1=new line(new point((float) 10,(float) 10),new point((float) 20,(float) 20));
l2=new line(new point((float) 0,(float) 0),new point((float) 20,(float) 1));
l3=new line(new point((float) 15,(float) 20),new point((float) 20,(float) 0));
Draw(Image1,*(l1));
Draw(Image1,*(l2));
Draw(Image1,*(l3));
display(l1);
display(l2);
display(l3);
p1=(getIntersect(*l1,*l2));
p2=(getIntersect(*l2,*l3));
p3=(getIntersect(*l3,*l1));
poly=new polygon(3,new point*[3]{p1,p2,p3});
cout<<(("the area of the intersection area is "));
cout<<((Area(*(poly))));
cout<<endl;;

DrawAx(Image1);
cvNamedWindow("ALG",1);
cvShowImage("ALG",Image1);
cvWaitKey(0);
}