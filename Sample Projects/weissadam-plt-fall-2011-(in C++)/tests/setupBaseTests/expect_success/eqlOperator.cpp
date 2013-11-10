#include "setupBase.h"
#include <iostream>

/* Test for = operator for SetupInt, SetupFloat, and SetupString */

int main(int argc, char** argv) {

  SetupInt i1(1);

  SetupInt i2;
  i2 = i1;

  SetupInt i3 = i1;
	
  std::cout << "i1 = " << i1 << std::endl; 
  std::cout << "i2 = " << i2 << std::endl; 	
  std::cout << "i3 = " << i3 << std::endl << std::endl; 

  SetupFloat f1(2.0);

  SetupFloat f2;
  f2 = f1;

  SetupFloat f3 = f1;
	
  std::cout << "f1 = " << f1 << std::endl; 
  std::cout << "f2 = " << f2 << std::endl; 	
  std::cout << "f3 = " << f3 << std::endl << std::endl; 
  
  SetupString s1("a");

  SetupString s2;
  s2 = s1;

  SetupString s3 = s1;
	
  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "s2 = " << s2 << std::endl; 	
  std::cout << "s3 = " << s3 << std::endl << std::endl; 


  SetupSet set1;
  for(int i = -10; i < 5; ++i) {
    set1.add(new SetupInt(i));
  }

  SetupSet set2;
  set2 = set1;

  SetupSet set3 = set1;

  SetupSet set4;
  for(int i = 0; i < 10; ++i) {
    set4.add(new SetupInt(i));
  }

  set4 = set1;
	
  std::cout << "set1 = " << set1 << std::endl; 
  std::cout << "set2 = " << set2 << std::endl; 	
  std::cout << "set3 = " << set3 << std::endl; 
  std::cout << "set4 = " << set4 << std::endl << std::endl; 


  SetupTuple t1;
  t1.add(new SetupString("a"));
  t1.add(new SetupFloat(1.0));
  t1.add(new SetupInt(1));
  t1.add(new SetupTuple(t1));

  SetupTuple t2;
  t2 = t1;

  SetupTuple t3 = t1;
	
  SetupTuple t4;
  t4.add(new SetupString("a"));
  t4.add(new SetupTuple(t1));

  t4 = t1;

  std::cout << "t1 = " << t1 << std::endl;
  std::cout << "t2 = " << t2 << std::endl;
  std::cout << "t3 = " << t3 << std::endl;
  std::cout << "t4 = " << t4 << std::endl;

  return 0;
}
