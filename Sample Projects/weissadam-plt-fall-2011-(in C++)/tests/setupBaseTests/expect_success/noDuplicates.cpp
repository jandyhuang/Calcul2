#include "setupBase.h"
#include <iostream>

/* Tests that duplicates can not be entered into a Set */

int main(int argc, char** argv) {
  SetupSet s;
  for(int i = -10; i < 5; ++i) {
    s.add(new SetupInt(i));
  }

  for(int i = -5; i < 10; ++i) {
    s.add(new SetupInt(i));
  }

	
  std::cout << s << std::endl;

  SetupSet s2;
  s2.add(new SetupString("dog"));
  s2.add(new SetupString("cat"));
  s2.add(new SetupString("dog"));
  s2.add(new SetupString("dog"));
  s2.add(new SetupString("cat"));
  s2.add(new SetupString("mouse"));

  std::cout << s2 << std::endl;

  SetupSet s3;
  s3.add(new SetupFloat(1.3));
  s3.add(new SetupFloat(1.4));
  s3.add(new SetupFloat(1.5));
  s3.add(new SetupFloat(1.3));
  s3.add(new SetupFloat(1.3));
  s3.add(new SetupFloat(1.4));

  std::cout << s3 << std::endl;


  SetupTuple t1, t2, t3;
  t1.add(new SetupString("a"));
  t1.add(new SetupFloat(1.0));
  t1.add(new SetupInt(1));
  t1.add(new SetupTuple(t1));
  std::cout << "t1 = " << t1 << std::endl;

  t2.add(new SetupString("a"));
  t2.add(new SetupFloat(1.0));
  t2.add(new SetupInt(1));
  t2.add(new SetupTuple(t2));
  std::cout << "t2 = " << t2 << std::endl;;

  t3.add(new SetupString("a"));
  t3.add(new SetupFloat(1.0));
  t3.add(new SetupInt(1));
  std::cout << "t3 = " << t3 << std::endl << std::endl;;

  SetupSet s4;
  s4.add(new SetupTuple(t1));
  s4.add(new SetupTuple(t2));
  s4.add(new SetupTuple(t3));

  std::cout << "s4 = " << s4 << std::endl << std::endl;

  SetupSet s5;
  s5.add(new SetupInt(1));
  s5.add(new SetupInt(2));   
  std::cout << "s5 = " << s5 << std::endl;

  SetupSet s6;
  s6.add(new SetupInt(1));
  s6.add(new SetupInt(2));
  std::cout << "s6 = " << s6 << std::endl;

  SetupSet s7;
  s7.add(new SetupInt(10));
  s7.add(new SetupInt(20)); 
  std::cout << "s7 = " << s7 << std::endl << std::endl;

  SetupSet s8;
  s8.add(&s5);
  s8.add(&s6);
  s8.add(&s7);
  std::cout << "s8 = " << s8 << std::endl;
  return 0;
}
