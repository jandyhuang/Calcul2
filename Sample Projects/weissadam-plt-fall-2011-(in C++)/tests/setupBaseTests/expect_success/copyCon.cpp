#include "setupBase.h"
#include <iostream>

/* Test copy constructor for SetupSet and SetupTuple */

int main(int argc, char** argv) {
  SetupSet s1;
  for(int i = -10; i < 5; ++i) {
    s1.add(new SetupInt(i));
  }

  SetupSet s2(s1);
  s2.add(new SetupInt(100));


  SetupSet s3(s2);
	
  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "s2 = " << s2 << std::endl; 	
  std::cout << "s3 = " << s3 << std::endl << std::endl; 

  SetupTuple t1;
  t1.add(new SetupString("a"));
  t1.add(new SetupFloat(1.0));
  t1.add(new SetupInt(1));
  t1.add(new SetupTuple(t1));

  SetupTuple t2(t1);
  t2.add(new SetupString("a new string"));

  SetupTuple t3(t2);
	

  std::cout << "t1 = " << t1 << std::endl;
  std::cout << "t2 = " << t2 << std::endl;
  std::cout << "t3 = " << t3 << std::endl;

  return 0;
}
