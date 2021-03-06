#include "setupBase.h"
#include <iostream>

/* Test copy constructor for SetupSet and SetupTuple */

int main(int argc, char** argv) {
  SetupSet s1;
  for(int i = 1; i < 3; ++i) {
    s1.add(new SetupInt(i));
  }

  SetupSet s2;
  for(int i = 7; i < 9; ++i) {
    s2.add(new SetupInt(i));
  }

  SetupSet s3;
  s3.add(new SetupString("dog"));
  s3.add(new SetupString("cat"));
  s3.add(new SetupString("mouse"));

  SetupTuple t1, t2, t3;
  t1.add(new SetupString("a"));
  t1.add(new SetupFloat(1.0));
  t1.add(new SetupInt(1));
  t1.add(new SetupTuple(t1));

  SetupSet s4;
  s4.add(new SetupTuple(t1));

  SetupSet s10 = setCross(s1, s2);
  SetupSet s11 = setCross(s1, s3);
  SetupSet s12 = setCross(s3, s2);
  SetupSet s13 = setCross(s1, s4);

	
  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "s2 = " << s2 << std::endl; 
  std::cout << "s3 = " << s3 << std::endl; 
  std::cout << "s4 = " << s4 << std::endl << std::endl;	
  std::cout << "s1 x s2 = " << s10 << std::endl; 
  std::cout << "s1 x s3 = " << s11 << std::endl; 
  std::cout << "s3 x s2 = " << s12 << std::endl; 
  std::cout << "s1 x s4 = " << s13 << std::endl; 

  return 0;
}
