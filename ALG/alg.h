using namespace std;
#include "stdio.h"
#include <iostream>
#include <cmath>
#include "string.h"
#include "stdlib.h"
#include </opt/local/include/opencv/cv.h>
#include </opt/local/include/opencv/highgui.h>

/****************************************************************/
//Draw a picture
IplImage *Image1;
int Position=500;
int Color=255;
int Shift=0;


class  point
{
public:
    float x;
    float y;
    point(float a,float b);
};

point::point(float a,float b){x=a;y=b;}

class line
{
public:
    point * p1;
    point  * p2;
    line(point *  a,point *  b);
};
line::line(point* a,point*  b){p1=a;p2=b;}

struct ellipse
{
    point * focusPoint;
    float a;//the length of major semi-axes
    float b;//the length of minot semi-axes
    ellipse(point * p, float x, float y);
};

ellipse::ellipse(point * p, float x, float y){focusPoint=p;a=x;b=y;}

class polygon
{
public:
    int pointNum;
    point ** pointCollection;
    polygon(int pointNum, point ** pointCollection);
};
polygon::polygon(int pointNum1, point ** pointCollection1){pointNum=pointNum1;pointCollection=pointCollection1;}

/****************************************************************/
//Draw a point
void Draw(IplImage *Image1, point p ){
    cvCircle(Image1, cvPoint(Position+p.x*10, Position-p.y*10), 2, CV_RGB(0,0,0),CV_FILLED);
}

void Draw(IplImage *Image1, line l){
    float x1 = 0;
    float y1 = 0;
    float x2 = 0;
    float y2 = 0;
    float xax1 = 0;
    float yax1 = 0;
    float xax2 = 0;
    float yax2 = 0;
    x1 = Position+l.p1->x*10;
    y1 = Position-l.p1->y*10;
    x2 = Position+l.p2->x*10;
    y2 = Position-l.p2->y*10;
    if((l.p2->x-l.p1->x)==0){
        xax1 = x1 + (1000-y1)*(x2-x1)/(y2-y1);
        yax1 = 1000;
        xax2 = x1 + (0-y1)*(x2-x1)/(y2-y1);
        yax2 = 0;
    }
    else{
        xax1 = 1000;
        xax2 = 0;
        yax1 = (y2-y1)*(1000-x1)/(x2-x1) + y1;
        yax2 = (y2-y1)*(0-x1)/(x2-x1) + y1;
    }
    
    cvLineAA(Image1, cvPoint(xax1, yax1), cvPoint(xax2, yax2), Color, Shift);
}


//Draw an ellipse
void Draw(IplImage *Image1, ellipse e){
    cvEllipseAA(Image1, cvPoint(Position+e.focusPoint->x*10, Position-10*e.focusPoint->y), cvSize(e.a*10, e.b*10), 0, 0, 360, Color, Shift);
}

//Draw a polygon
void Draw(IplImage *Image1, polygon py){
    int pointNum = py.pointNum;
    int i = 0;
    point **pointCollection = py.pointCollection;
    while (i != pointNum-1){
        cvLineAA(Image1, cvPoint(Position+(*(pointCollection[i])).x*10, Position-10*(*pointCollection[i]).y), cvPoint(Position+(*pointCollection[i+1]).x*10, Position-10*(*pointCollection[i+1]).y), Color, Shift);
        i ++ ;
    }
    cvLineAA(Image1, cvPoint(Position+10*(*pointCollection[pointNum-1]).x, Position-10*(*pointCollection[pointNum-1]).y), cvPoint(Position+10*(*pointCollection[0]).x, Position-10*(*pointCollection[0]).y), Color, Shift);
    
}

//Draw the x,y axes 
void DrawAx(IplImage *Image1){
    cvLineAA(Image1, cvPoint(0,500),cvPoint(1000,500),0,Shift);
    cvLineAA(Image1, cvPoint(500,0),cvPoint(500,1000),0,Shift);
}

/****************************************************************/




/************************The following code are used to judge the relation between two lines*******************/
void display(point *p)
{
    printf("Point:%f %f\n", p->x,p->y);
    
}

void display(line *l)
{
    printf("This is a line!\n");
    printf("The first point is (%f %f) \n",l->p1->x,l->p1->y);
    printf("The second point is (%f %f) \n", l->p2->x,l->p2->y);
}
void display(polygon *p)
{
    printf("This is a polygon with %d edges! \n",p->pointNum);
    printf("The points of this polygon is: \n");
    int num = p->pointNum,i;
    point ** C = p->pointCollection;
    for(i = 0;i<num;i++)
    {
        printf("(%f %f)\n", C[i]->x, C[i]->y);
    }
}
void display(ellipse *e)
{
    printf("This is an ellipse!\nIts focus point is (%f %f)\nIts major semi-axe's length is %f, and its minor semi-axe's length is %f\n", e->focusPoint->x, e->focusPoint->y, e->a, e->b);
}



point * getPoint(float f1, float f2)
{
    point *p=new point(f1,f2);
    return p;
}

point *getIntersect(line l1, line l2)
{
	bool isParallel(line l1, line l2);
    point *result = getPoint(0,0);
    if(isParallel(l1,l2))
    {
        printf("these two linse are parallel!\n");
        return result;// if these two lines are parallel, just return the original point with a warning.
    }
    else
    {
        float x1 = l1.p1->x, y1 = l1.p1->y, x2 = l1.p2->x,y2 = l1.p2->y;
        float h1 = l2.p1->x, k1 = l2.p1->y, h2 = l2.p2->x, k2 = l2.p2->y;
        float temp2,temp1,delta1,delta2;
	float resultX,resultY;
        delta1 = h1-h2;
        delta2 = x1-x2;
        if(delta1 == 0 && delta2==0)
		return result;
	else if(delta1==0)
        {resultX=h1;resultY=(y2-y1)*(resultX-x1)/(x2-(float)x1)+y1;
	result = getPoint(resultX, resultY);
        return result;}
        else if(delta2 == 0)
         {resultX=x1;
	resultY=(k2-k1)*(resultX-h1)/(h2-(float)h1)+k1;
	result = getPoint(resultX, resultY);
        return result;}

       
        temp2 = (k1-k2)/delta1; temp1 = (y1-y2)/delta2;
        
        float a = temp1-temp2,b = k1-y1-h1*temp2+x1*temp1;
         resultX = b/a;
        resultY = temp1*(resultX-x1) +y1;
        result = getPoint(resultX, resultY);
        return result;
    }
    
}// get the intersect point of two lines



bool isParallel(line l1, line l2)
{
    float deltaX1 = l1.p1->x-l1.p2->x,deltaY1 = l1.p1->y-l1.p2->y;
    float deltaX2 = l2.p1->x-l2.p2->x, deltaY2 = l2.p1->y-l2.p2->y;
    if(deltaX1 == 0 && deltaX2 == 0)
        return true;
    if(deltaX1*deltaX2 == 0 && deltaX1+deltaX2!=0)
        return false;
    if(deltaY1/deltaX1 != deltaY2/deltaX2)
        return false;
    return true;
}//judge whether two lines are parallel
float getDis(point *p1, point *p2)
{
    return (float)sqrt(pow((double)(p1->x-p2->x),2.0)+pow((double)(p1->y-p2->y),2.0));
}

char * getRelation(ellipse  circle1, ellipse  circle2)
{
	char * str=(char*)malloc(sizeof(char)*30);
    if(circle1.a!=circle1.b || circle2.a!=circle2.b)
    {
        printf("These two shapes are not both circles!\n");
	strcpy(str,"");
        return str;//inform a mistake
    }
    
    float r1 = circle1.a, r2 = circle2.a;
        
    float dis = getDis(circle1.focusPoint, circle2.focusPoint);
    if(fabs(dis-r1-r2)<=0.0001)
        strcpy(str,"circumscribe");// they are circumscribe
    else if(fabs(dis-fabs(r1-r2))<=0.0001)
        strcpy(str,"inscribe");// they are inscribe
    else if(dis<fabs(r1-r2))
        strcpy(str,"contain");// one circle contains another one
    else if(dis>r1+r2)
          strcpy(str,"seperate");//they are seperated
    else if(r1+r2>dis && dis > fabs(r1-r2))
       strcpy(str,"secant");// they are secant
    
    return str;
    
}


char * getRelation(line l, ellipse e)
{
    float y1 = l.p1->y, y2 = l.p2->y, x1 = l.p1->x, x2 = l.p2->x;
    float h = e.focusPoint->x, k = e.focusPoint->y, a = e.a, b= e.b;
    float k1 = y1-y2, k2 = x2-x1, k3 = 2*x1*y1-x1*y2-y1*x2;
    float Ba = (a*a*k1*k1)/(k2*k2)+b*b;
    float Bb = (2*a*a*k*k1)/k2 - (2*a*a*k1*k3)/(k2*k2)-2*b*b*h;
    float Bc = (a*a*k3*k3)/(k2*k2) +a*a*k*k -(2*a*a*k*k3)/(k2)+b*b*h*h-a*a*b*b;
    //cout<<Ba<<endl;
    char * str=(char*)malloc(sizeof(char)*30);
    if(k2 == 0)// special case for the vertical line
    {
       if(fabs((k3/k1)-h) == a)
           {strcpy(str,"tangent");return str;}//tangent
       else if((k3/k1)>h-a && (k3/k1)<h+a)
         { strcpy(str,"secant");return str;}//secant
        else
           {strcpy(str,"seperate");return str;}//seperate
      
    }
    float temp = Bb*Bb-4*Ba*Bc;
  //  cout<<temp<<endl;
   // cout<<temp<<endl;
    if(temp>0)
        {strcpy(str,"secant");return str;}//secant
    else if(temp==0)
        {strcpy(str,"tangent");return str;}//tangent
    else
       { strcpy(str,"seperate");return str;}//seperate

}




/*
int main()
{line * l1;
line * l2;
bool a;
l1=new line(new point((float) 1,(float) 1),new point((float) 2,(float) 2));
l2=new line(new point((float) 3,(float) 3),new point((float) 4,(float) 4));
a=(isParallel(*l1,*l2));

int num=3;

polygon *shape1=new polygon(num, new point[num]{point(1.1,1.2),point(1.1,1.2),point(1.1,1.2)} );
polygon *shape2=new polygon(num, new point[num]{point(1.1,1.2),point(1.1,1.2),point(1.1,1.2)} );
polygonSimilar(*shape1,*shape2);

}

*/

line * getLine(point P1, point P2)
{
    line * l=new line(&P1,&P2);
    return l;
    
}

float getAngle(line l1, line l2)// get the angle between two lines
{
    float Vx1 = l1.p1->x - l1.p2->x;
    float Vy1 = l1.p1->y - l1.p2->y;
    float Vx2 = l2.p1->x - l2.p2->x;
    float Vy2 = l2.p1->y - l2.p2->y;
    return acos((Vx1*Vx2+Vy1*Vy2)/(getDis(l1.p1,l1.p2)*getDis(l2.p1,l2.p2)));
}



bool naiveSimilar(polygon shape1, polygon shape2,int start1, int start2)// assume we know where to start to check whether two polygond are similar
{
    
    int i = 0,j = 0;
    int num1 = shape1.pointNum,num2 = shape2.pointNum;
    point ** C1 = shape1.pointCollection;
    point ** C2 = shape2.pointCollection;
	
    while(i<num1 && j<num2)
    {
            line *shape1Line1 = getLine(*C1[(start1+i)%num1],*C1[(start1+i-1+num1)%num1]);
            line *shape1Line2 = getLine(*C1[(start1+i)%num1],*C1[(start1+i+1)%num1]);
            line *shape2Line1 = getLine(*C2[(start2+j)%num2],*C2[(start2+j-1+num2)%num2]);
            line *shape2Line2 = getLine(*C2[(start2+j)%num2],*C2[(start2+j+1)%num2]);
            float shape1Line1Length = getDis(C1[(start1+i)%num1],C1[(start1+i-1+num1)%num1]);
            float shape1Line2Length = getDis(C1[(start1+i)%num1],C1[(start1+i+1)%num1]);
            float shape2Line1Length = getDis(C2[(start2+j)%num2],C2[(start2+j-1+num2)%num2]);
            float shape2Line2Length = getDis(C2[(start2+j)%num2],C2[(start2+j+1)%num2]);
            if(fabs(getAngle(*shape1Line1,*shape1Line2) - getAngle(*shape2Line1,*shape2Line2)) >= 0.001)
            {
             
                return false;
            }
            if(fabs((shape1Line1Length/shape2Line1Length)-(shape1Line2Length/shape2Line2Length)) >=0.001)
            {
                return false;//we should also check whether the ratio of edges' length are the same
            }
        
        i++;
        j++;
    }//endwhile


    return true;

}

bool naiveSimilar2(polygon shape1, polygon shape2,int start1, int start2)// assume we know where to start to check whether two polygond are similar
{
	
    int i = 0,j = 0;
    int num1 = shape1.pointNum,num2 = shape2.pointNum;
    point **C1 = shape1.pointCollection;
    point ** C2 = shape2.pointCollection;
    while(i<num1 && j<num2)
    {
	
        line *shape1Line1 = getLine(*C1[(start1+i)%num1],*C1[(start1+i-1+num1)%num1]);
        line *shape1Line2 = getLine(*C1[(start1+i)%num1],*C1[(start1+i+1)%num1]);
        line *shape2Line1 = getLine(*C2[(start2-j+num2)%num2],*C2[(start2-j+1+num2)%num2]);
        line *shape2Line2 = getLine(*C2[(start2-j+num2)%num2],*C2[(start2-j-1+num2)%num2]);
        float shape1Line1Length = getDis(C1[(start1+i)%num1],C1[(start1+i-1+num1)%num1]);
        float shape1Line2Length = getDis(C1[(start1+i)%num1],C1[(start1+i+1)%num1]);
        float shape2Line1Length = getDis(C2[(start2-j+num2)%num2],C2[(start2-j+1+num2)%num2]);
        float shape2Line2Length = getDis(C2[(start2-j+num2)%num2],C2[(start2-j-1+num2)%num2]);
        if(fabs(getAngle(*shape1Line1,*shape1Line2) - getAngle(*shape2Line1,*shape2Line2)) >= 0.001)
        {
            return false;
        }
        if(fabs((shape1Line1Length/shape2Line1Length)-(shape1Line2Length/shape2Line2Length)) >=0.001)
        {
            return false;//we should also check whether the ratio of edges' length are the same
        }
        
        i++;
        j++;
    }//end while

    return true;
    
}// counter clockwise check

bool isSimilar(polygon shape1, polygon shape2)//judge whether two polygons are similar
{
    int i;
    int num1 = shape1.pointNum;
    int num2 = shape2.pointNum;
    if(num1!=num2)
        {
	return false;// if the two polygon have different point numbers, then they are not similar
	}
    else
    {
	
       for(i = 0;i<num1;i++)
        {
	    //printf("ok");
            if(naiveSimilar(shape1,shape2,i,0) || naiveSimilar2(shape1, shape2, i, 0))
                return true;
       }
    }
}






float Perimeter(polygon p)
{
    int num = p.pointNum;
    float result = 0;
    point ** C = p.pointCollection;
    for(int i = 0;i<num;i++)
    {
        result = result +getDis(C[i%num],C[(i+1)%num]);
    }
    return result;
}


float Perimeter(ellipse e)
{
    return 3.14159*(3*(e.a+e.b)-sqrt((3*e.a+e.b)*(e.a+3*e.b)));
}

bool isPerimeterLessThan(polygon p1,polygon p2)
{
	float perimeter1=Perimeter(p1);
	float perimeter2=Perimeter(p2);
	return (perimeter1<perimeter2);
}

bool isPerimeterLessThan(ellipse e1,ellipse e2)
{
	float perimeter1=Perimeter(e1);
	float perimeter2=Perimeter(e2);
	return (perimeter1<perimeter2);
}


bool isPerimeterLessOrEqual(polygon p1,polygon p2)
{
	float perimeter1=Perimeter(p1);
	float perimeter2=Perimeter(p2);
	return (perimeter1<=perimeter2);
}

bool isPerimeterLessOrEqual(ellipse e1,ellipse e2)
{
	float perimeter1=Perimeter(e1);
	float perimeter2=Perimeter(e2);
	return (perimeter1<=perimeter2);
}

bool isPerimeterLargerThan(polygon p1,polygon p2)
{
	float perimeter1=Perimeter(p1);
	float perimeter2=Perimeter(p2);
	return (perimeter1>perimeter2);
}

bool isPerimeterLargerThan(ellipse e1,ellipse e2)
{
	float perimeter1=Perimeter(e1);
	float perimeter2=Perimeter(e2);
	return (perimeter1>perimeter2);
}

bool isPerimeterLargerOrEqual(polygon p1,polygon p2)
{
	float perimeter1=Perimeter(p1);
	float perimeter2=Perimeter(p2);
	return (perimeter1>=perimeter2);
}

bool isPerimeterLargerOrEqual(ellipse e1,ellipse e2)
{
	float perimeter1=Perimeter(e1);
	float perimeter2=Perimeter(e2);
	return (perimeter1>=perimeter2);
}




float getTriangleArea(polygon triangle)
{
    if(triangle.pointNum != 3)
    {
        printf("error in getting triangle area! This is not a triangle!\n");
        return 0;
    }
    point **C = triangle.pointCollection;
    
    float result = 0;
    result = result + (*C[0]).x*((*C[1]).y-(*C[2]).y);
    result = result + (*C[1]).x*((*C[2]).y-(*C[0]).y);
    result = result + (*C[2]).x*((*C[0]).y-(*C[1]).y);
    return fabs(result/2);
}

polygon * getPolygon(int num, point ** collection)
{
    polygon * result=new polygon(num,collection);
    return result;
}

float Area(polygon p)
{
    int num =p.pointNum;
    float result =0;
    point ** C = p.pointCollection;
    if(num<=2)
        return 0;
    if(num == 3)
        return getTriangleArea(p);
    else
    {
        point * C1[3] = {C[num-2],C[num-1],C[0]};
        point * C2[3] = {C[0],C[1],C[2]};
        polygon t1 = *getPolygon(3,C1);
        polygon t2 = *getPolygon(3,C2);
        result = result + getTriangleArea(t1);
        result = result + getTriangleArea(t2);//first add the two trianlges on the edge
        for(int i = 2;i<num-2;i++)
        {
            point *tempC[3] = {C[0],C[i],C[i+1]};
            polygon tempT = *getPolygon(3, tempC);
            result = result+getTriangleArea(tempT);
            
        }
        return result;
        
    }
}

bool operator<= (polygon p1,polygon p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1<=area2);
}

bool operator< (polygon p1,polygon p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1<area2);
}

bool operator>= (polygon p1,polygon p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1>=area2);
}

bool operator> (polygon p1,polygon p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1>area2);
}

float Area(ellipse e)
{
    return 3.14159*e.a*e.b;
}


bool operator<= (ellipse p1,ellipse p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1<=area2);
}

bool operator< (ellipse p1,ellipse p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1<area2);
}

bool operator>= (ellipse p1,ellipse p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	printf("%f%f\n",area1,area2);
	return (area1>=area2);
}

bool operator> (ellipse p1,ellipse p2)
{
	float area1=Area(p1);
	float area2=Area(p2);
	return (area1>area2);
}





bool naiveCongruent(polygon shape1, polygon shape2,int start1, int start2)// assume we know where to start to check whether two polygond are similar
{
    int i = 0,j = 0;
    int num1 = shape1.pointNum,num2 = shape2.pointNum;
    point ** C1 = shape1.pointCollection;
    point ** C2 = shape2.pointCollection;
    while(i<num1 && j<num2)
    {
        line *shape1Line1 = getLine(*C1[(start1+i)%num1],*C1[(start1+i-1+num1)%num1]);
        line *shape1Line2 = getLine(*C1[(start1+i)%num1],*C1[(start1+i+1)%num1]);
        line *shape2Line1 = getLine(*C2[(start2+j)%num2],*C2[(start2+j-1+num2)%num2]);
        line *shape2Line2 = getLine(*C2[(start2+j)%num2],*C2[(start2+j+1)%num2]);
        float shape1Line1Length = getDis(C1[(start1+i)%num1],C1[(start1+i-1+num1)%num1]);
        float shape1Line2Length = getDis(C1[(start1+i)%num1],C1[(start1+i+1)%num1]);
        float shape2Line1Length = getDis(C2[(start2+j)%num2],C2[(start2+j-1+num2)%num2]);
        float shape2Line2Length = getDis(C2[(start2+j)%num2],C2[(start2+j+1)%num2]);
        if(fabs(getAngle(*shape1Line1,*shape1Line2) - getAngle(*shape2Line1,*shape2Line2)) >= 0.0001)
        {
            
            return false;
        }
        if(fabs(shape1Line1Length-shape2Line1Length)>0.0001 || fabs(shape1Line2Length-shape2Line2Length)>0.0001)
        {
            return false;//we should also check whether the ratio of edges' length are the same
        }
        
        i++;
        j++;
    }
    return true;
    
}



bool naiveCongruent2(polygon shape1, polygon shape2,int start1, int start2)// assume we know where to start to check whether two polygond are congruent
{
    int i = 0,j = 0;
    int num1 = shape1.pointNum,num2 = shape2.pointNum;
    point ** C1 = shape1.pointCollection;
    point ** C2 = shape2.pointCollection;
    while(i<num1 && j<num2)
    {
        line *shape1Line1 = getLine(*C1[(start1+i)%num1],*C1[(start1+i-1+num1)%num1]);
        line *shape1Line2 = getLine(*C1[(start1+i)%num1],*C1[(start1+i+1)%num1]);
        line *shape2Line1 = getLine(*C2[(start2-j+num2)%num2],*C2[(start2-j+1+num2)%num2]);
        line *shape2Line2 = getLine(*C2[(start2-j+num2)%num2],*C2[(start2-j-1+num2)%num2]);
        float shape1Line1Length = getDis(C1[(start1+i)%num1],C1[(start1+i-1+num1)%num1]);
        float shape1Line2Length = getDis(C1[(start1+i)%num1],C1[(start1+i+1)%num1]);
        float shape2Line1Length = getDis(C2[(start2-j+num2)%num2],C2[(start2-j+1+num2)%num2]);
        float shape2Line2Length = getDis(C2[(start2-j+num2)%num2],C2[(start2-j-1+num2)%num2]);
        if(fabs(getAngle(*shape1Line1,*shape1Line2) - getAngle(*shape2Line1,*shape2Line2)) >= 0.0001)
        {
            return false;
        }
        if(fabs(shape1Line1Length-shape2Line1Length)>0.0001 || fabs(shape1Line2Length!=shape2Line2Length)>0.0001)
        {
            return false;//we should also check whether the ratio of edges' length are the same
        }
        
        i++;
        j++;
    }
    return true;
    
}// counter clockwise check


bool isCongruent(polygon shape1, polygon shape2)//judge whether two polygons are similar
{
    int i;
    int num1 = shape1.pointNum,num2 = shape2.pointNum;
    if(num1 != num2)
        return false;// if the two polygon have different point numbers, then they are not similar
    else
    {
        for(i = 0;i<num1;i++)
        {
            if(naiveCongruent(shape1,shape2,i,0) || naiveCongruent2(shape1, shape2, i, 0))
                return true;
        }
        return false;
        
    }
}


void Move(point &p, float x, float y)// move a point based on the vector (x,y)
{
    p.x = p.x+x;
    p.y = p.y+y;
}
void Move(line &l, float x, float y)// move a line based on the vector (x,y)
{
    l.p1->x = l.p1->x+x;
    l.p1->y = l.p1->y+y;
    l.p2->x = l.p2->x+x;
    l.p2->y = l.p2->y+y;
}
void Move(polygon &poly, float x, float y)// move a polygon based on the vector (x,y)
{
    int num = poly.pointNum,i;
    point ** pointCollection=poly.pointCollection;
    for(i = 0;i<num;i++)
    {
        pointCollection[i]->x = pointCollection[i]->x+x;
        pointCollection[i]->y = pointCollection[i]->y+y;
    }
    
}

void Move(ellipse &e, float x, float y)// move an ellipse based on the vector (x,y)
{
    Move(*(e.focusPoint), x, y);
}
/*
int main()
{
	printf("%f\n", (new point((float)1,(float)2))->x);
        printf("%f\n", (( new line( new point(1.1,1.2), new point(2.1,2.2)))->p1->x));
	printf("%f\n", ((new ellipse(new point(1.1,1.2),(float)3,(float)4))->focusPoint->x));
	//new point[]={new point(1.1,1.2),new point(1.1,1.2),new point(1.1,1.2)}
	int num=3;
	//printf("%d\n",(new int[num]{1,2})[0]);
	//point  p[]={point(1.1,1.2),point(1.1,1.2),point(1.1,1.2)};
	//new polygon(num, new point*[num]{point(1.1,1.2),point(1.1,1.2),point(1.1,1.2)} );
	//new point*[num]{new point(1.1,1.2), new point(1.1,1.2)};
	printf("%d\n", (new polygon(3, new point*[3]{new point(1.1,1.2), new point(1.1,1.2), new point(3.1,1.2)}            ))->pointNum);
	polygon *shape1=new polygon(3, new point*[3]{new point(1.1,1.2), new point(1.1,1.3), new point(3.1,1.2)}         );
	polygon *shape2=new polygon(3, new point*[3]{new point(1.1,0.5), new point(1.1,1.2), new point(3.1,1.2)}          );
	isSimilar(*shape1,*shape2);
	if(isPerimeterLargerThan(*shape1,*shape2))printf("true");
	else printf("false");
	printf("polygonarea:%f\n",Area(*shape1));
	if(*shape1<=*shape2)
	printf("polyarealess\n");
	else
	printf("polyarealarger\n");
	if(isCongruent(*shape1, *shape2))printf("congru");else printf("notcongru");
	line * l1= new line( new point(1.1,1.2), new point(2.1,2.2));
	ellipse * e1=new ellipse(new point(1.1,1.2),(float)3,(float)4);
	display(l1);
	display(e1);
	display(shape1);
	point * p=new point(1,2);
	display(p);
	Move(*p,1,1);
	display(p);
	display(shape1);
	Move(*shape1,1,2);
	display(shape1);
	display(l1);
	Move(*l1,1,2);
	display(l1);
	display(e1);
	Move(*e1,1,2);
	display(e1);
	
	
}
*/
