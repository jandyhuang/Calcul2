#include "setupBase.h"
#include <iostream>

/* Test copy constructor for SetupSet and SetupTuple */

int main(int argc, char** argv) {
  SetupSet s1;
  for(int i = 1; i < 10; ++i) {
    s1.add(new SetupInt(i));
  }

  SetupSet s2;
  for(int i = 3; i < 7; ++i) {
    s2.add(new SetupInt(i));
  }

  SetupSet s3;
  s3.add(new SetupString("dog"));
  s3.add(new SetupString("cat"));
  s3.add(new SetupString("mouse"));

	
  SetupTuple t1, t2;
  t1.add(new SetupString("a"));
  t1.add(new SetupFloat(1.0));
  t1.add(new SetupInt(1));
  t1.add(new SetupTuple(t1));

  t2.add(new SetupString("a"));
  t2.add(new SetupFloat(1.0));
  t2.add(new SetupInt(1));

  SetupSet s4;
  s4.add(new SetupTuple(t1));
  s4.add(new SetupTuple(t2));



  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "# s1 = " << setCardinality(s1) << std::endl << std::endl; 

  std::cout << "s2 = " << s2 << std::endl; 	
  std::cout << "# s2 = " << setCardinality(s2) << std::endl << std::endl; 

  std::cout << "s3 = " << s3 << std::endl; 	
  std::cout << "# s3 = " << setCardinality(s3) << std::endl << std::endl; 

  std::cout << "s4 = " << s4 << std::endl; 	
  std::cout << "# s4 = " << setCardinality(s4) << std::endl << std::endl; 

  return 0;
}
