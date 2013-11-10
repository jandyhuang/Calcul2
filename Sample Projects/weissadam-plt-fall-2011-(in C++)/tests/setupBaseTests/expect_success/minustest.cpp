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


  SetupSet s3 = setMinus(s1, s2);
	
  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "s2 = " << s2 << std::endl; 	
  std::cout << "s1 - s2 = " << s3 << std::endl << std::endl; 


  return 0;
}
