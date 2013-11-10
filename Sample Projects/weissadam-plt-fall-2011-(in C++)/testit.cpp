#include "setupBase.h"
#include <iostream>
int main(int argc, char** argv) {
  SetupSet s;
  for(int i = -10; i < 10; ++i) {
    s.add(new SetupInt(i));
  }
  std::cout << s.toString() << std::endl;

  SetupTuple t;
  t.add(new SetupString("a"));
  t.add(new SetupFloat(1.0));
  t.add(new SetupInt(1));
  t.add(new SetupTuple(t));  
  std::cout << t.toString() << std::endl;

  t.add(new SetupString("b"))
    .add(new SetupFloat(1.0))
    .add(new SetupInt(1)) 
    .add(new SetupTuple(t));
  std::cout << t.toString() << std::endl;
  return 0;}
