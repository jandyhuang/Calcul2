#include "setupBase.h"
#include <iostream>

/* Test != operator on SetupSet type and SetupTuple type */

int main(int argc, char** argv) {
  SetupSet s1, s2, s3;
  for(int i = -10; i < 5; ++i) {
    s1.add(new SetupInt(i));
    s2.add(new SetupInt(i));
  }

  for(int i = -5; i < 10; ++i) {
    s3.add(new SetupInt(i));
  }
	
  std::cout << "s1 = " << s1 << std::endl; 
  std::cout << "s2 = " << s2 << std::endl; 	
  std::cout << "s3 = " << s3 << std::endl << std::endl; 

  std::cout << "s1 != s2: " << std::boolalpha << (s1!=s2) << std::endl;	
  std::cout << "s1 != s3: " << (s1!=s3) << std::endl;	
  std::cout << "s1 != s1: " << (s1!=s1) << std::endl << std::endl;	



  SetupTuple t1, t2, t3, t4;
  t1.add(new SetupString("a"));
  t1.add(new SetupFloat(1.0));
  t1.add(new SetupInt(1));
  t1.add(new SetupTuple(t1));

  t2.add(new SetupString("a"));
  t2.add(new SetupFloat(1.0));
  t2.add(new SetupInt(1));
  t2.add(new SetupTuple(t2));

  // Missing one  entry 
  t3.add(new SetupString("a"));
  t3.add(new SetupFloat(1.0));
  t3.add(new SetupInt(1));

  // Entries in different order
  t4.add(new SetupString("a"));
  t4.add(new SetupInt(1));
  t4.add(new SetupTuple(t4));
  t4.add(new SetupFloat(1.0));

  std::cout << "t1 = " << t1 << std::endl;
  std::cout << "t2 = " << t2 << std::endl;
  std::cout << "t3 = " << t3 << std::endl;
  std::cout << "t4 = " << t4 << std::endl << std::endl;
	
  std::cout << "t1 != t2: " << (t1!=t2) << std::endl;	
  std::cout << "t1 != t3: " << (t1!=t3) << std::endl;	
  std::cout << "t1 != t4: " << (t1!=t4) << std::endl;	
  std::cout << "t1 != t1: " << (t1!=t1) << std::endl;	

  return 0;
}
