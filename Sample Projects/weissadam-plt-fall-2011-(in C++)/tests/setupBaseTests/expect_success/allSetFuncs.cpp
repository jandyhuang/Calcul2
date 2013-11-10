#include <iostream>
#include <string>
#include "setupBase.h"
int main(){

std::cout << setCross (SetFactory(0), SetFactory(1)) << std::endl;
std::cout << setIntersect (SetFactory(0), SetFactory(1)) << std::endl;
std::cout << setMinus (SetFactory(0), SetFactory(1)) << std::endl;
std::cout << setUnion (SetFactory(0), SetFactory(1)) << std::endl;
std::cout << setCardinality (SetFactory(1).add( 2).add( 3)) << std::endl;
return 0;


}
