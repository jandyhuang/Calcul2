#include "setupBase.h"
#include <iostream>

/* Test copy constructor for SetupSet and SetupTuple */

int main(int argc, char** argv) {
  SetupSet s1;
  for(int i = -10; i < 5; ++i) {
    s1.add(new SetupInt(i));
  }

  SetupSet s2;
  for(int i = -5; i < 10; ++i) {
    s2.add(new SetupInt(i));
  }

  SetupSet s3;
  s3.add(new SetupString("cat"));
  s3.add(new SetupString("dog"));
  s3.add(new SetupString("mouse"));
	

  SetupSet s4;
  s4.add(new SetupInt(1));

  SetupSet s5;
  s5.add(new SetupInt(42));
  

  SetupSet s10 = setIntersect(s1, s2);
  SetupSet s11 = setIntersect(s1, s3);
  SetupSet s12 = setIntersect(s4, s5);
	
  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "s2 = " << s2 << std::endl; 	
  std::cout << "s3 = " << s3 << std::endl; 	
  std::cout << "s4 = " << s4 << std::endl; 	
  std::cout << "s5 = " << s5 << std::endl; 	

  std::cout << "s1 intersect s2 = " << s10 << std::endl; 
  std::cout << "s1 intersect s3 = " << s11 << std::endl; 
  std::cout << "s4 intersect s5 = " << s12 << std::endl << std::endl;; 


  return 0;
}
