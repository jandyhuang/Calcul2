#include <set>
#include <list>
#include <string>
#include <sstream>
#include <iostream>
#include <typeinfo>

/*
  Authors: Andrew Ingraham, Bill Warner */

class SetupBase {
 public:
  //  virtual std::ostream toStream(std::ostream) const;
  virtual std::string toString() const = 0;
  virtual bool operator==(const SetupBase &) const = 0;
  virtual bool operator!=(const SetupBase &) const = 0;
  //virtual SetupBool& operator==(const SetupBase &) const = 0;
  //virtual SetupBool& operator!=(const SetupBase &) const = 0;
  
  virtual SetupBase* clone() const = 0;
};

class SetupBool : public SetupBase {
 public:
  bool value;
 SetupBool(bool v) : value(v) {}
 SetupBool() : value(0) {}
 SetupBool(const SetupBool& r) : value(r.value) {}
  std::string toString() const {
    std::stringstream out;
    if(value){
    	out << "true";
    }else{
    	out << "false";
    }
    return out.str();
  }

  SetupBase* clone() const {
	return new SetupBool(*this);
  }

  friend std::ostream& operator<< (std::ostream &out, SetupBool &rhs) {
	out << rhs.toString();
	return out;
  }

  bool operator==(const SetupBase &other) const {

	const SetupBool * sPtr = dynamic_cast<const SetupBool*>(&other);
	
	if(sPtr && (this->value == sPtr->value)){
		return true;
	}else{
		return false;
	}
  }   

  bool operator!=(const SetupBase &other) const {
	return !(*this == other);
  }

  SetupBool & operator=(const SetupBase &rhs){
	if(this == &rhs){
		return *this;
	}else{
		const SetupBool * sPtr = dynamic_cast<const SetupBool*>(&rhs);
		value = sPtr->value;
		return *this;
	}
  }   

};

class SetupInt : public SetupBase {
 public:
  int value;
 SetupInt(int v) : value(v) {}
 SetupInt() : value(0) {}
 SetupInt(const SetupInt& r) : value(r.value) {}
  std::string toString() const {
    std::stringstream out;
    out << value;
    return out.str();
  }

  SetupBase* clone() const {
	return new SetupInt(*this);
  }

  friend std::ostream& operator<< (std::ostream &out, SetupInt &rhs) {
	out << rhs.toString();
	return out;
  }

  bool operator==(const SetupBase &other) const {

	const SetupInt * sPtr = dynamic_cast<const SetupInt*>(&other);
	
	if(sPtr && (this->value == sPtr->value)){
		return true;
	}else{
		return false;
	}
  }   

  bool operator!=(const SetupBase &other) const {
	return !(*this == other);
  }


  SetupInt & operator=(const SetupBase &rhs){
	if(this == &rhs){
		return *this;
	}else{
		const SetupInt * sPtr = dynamic_cast<const SetupInt*>(&rhs);
		value = sPtr->value;
		return *this;
	}
  }   

};

class SetupFloat : public SetupBase {
 public:
  double value;
 SetupFloat(double v) : value(v) {}
 SetupFloat() : value(0.0) {}
 SetupFloat(const SetupFloat& r) : value(r.value) {}
  std::string toString() const {
    std::stringstream out;
    out << value;
    return out.str();
  }

  SetupBase* clone() const {
	return new SetupFloat(*this);
  }

  friend std::ostream& operator<< (std::ostream &out, SetupFloat &rhs) {
	out << rhs.toString();
	return out;
  }



  bool operator==(const SetupBase &other) const {

	const SetupFloat * sPtr = dynamic_cast<const SetupFloat*>(&other);
	
	if(sPtr && (this->value == sPtr->value)){
		return true;
	}else{
		return false;
	}
  } 

  bool operator!=(const SetupBase &other) const {
	return !(*this == other);
  }


  SetupFloat & operator=(const SetupBase &rhs){
	if(this == &rhs){
		return *this;
	}else{
		const SetupFloat * sPtr = dynamic_cast<const SetupFloat*>(&rhs);
		value = sPtr->value;
		return *this;
	}
  }   

};

class SetupString : public SetupBase {
 public:
  std::string value;

 SetupString(const char* v) : value(v) {}
 SetupString() : value("") {}
 SetupString(const SetupString& r) : value(r.value) {}

  std::string toString() const {
    std::stringstream out;
    out << "\"" << value << "\"";
    return out.str();
  }

  SetupBase* clone() const {
	return new SetupString(*this);
  }

  friend std::ostream& operator<< (std::ostream &out, SetupString &rhs) {
	out << rhs.toString();
	return out;
  }

  bool operator==(const SetupBase &other) const {

	const SetupString * sPtr = dynamic_cast<const SetupString*>(&other);

	if(sPtr && ( 0 == this->value.compare(sPtr->value) )){
		return true;
	}else{
		return false;
	}

  } 

  bool operator!=(const SetupBase &other) const {
	return !(*this == other);
  }


  SetupString & operator=(const SetupBase &rhs){
	if(this == &rhs){
		return *this;
	}else{
		const SetupString * sPtr = dynamic_cast<const SetupString*>(&rhs);
		value = sPtr->value;
		return *this;
	}
  } 

};

//
// Forward declaration allows tuples of sets

class SetupSet;

class SetupTuple : public SetupBase {
 public:
  std::list<SetupBase*> value;

  SetupTuple(){}

  ~SetupTuple(){

	value.clear();

  }

  SetupTuple(const SetupTuple& r){
	for(std::list<SetupBase*>::const_iterator it = r.value.begin(); it != r.value.end(); ++it) {
		// may be wrong - call add
		value.push_back((*it)->clone()); 
	}
  }

  SetupBase* clone() const {
	return new SetupTuple(*this);
  }

  SetupTuple(bool b) {
    add(new SetupBool(b));
  }
  SetupTuple(int i) {
    add(new SetupInt(i));
  }
  SetupTuple(double f) {
    add(new SetupFloat(f));
  }
  SetupTuple(const char* s) {
    add(new SetupString(s));
  }

  std::string toString() const {
    std::stringstream out;
    out << "(";

    for(std::list<SetupBase*>::const_iterator it = value.begin(); it != value.end(); ++it) {
      out << (*it)->toString();
      if(it != --value.end())
	out << ", ";
    }
    out << ")";
    return out.str();
  }

  friend std::ostream& operator<< (std::ostream &out, SetupTuple &rhs) {
	out << rhs.toString();
	return out;
  }


  bool operator==(const SetupBase &other) const {

	const SetupTuple * sPtr = dynamic_cast<const SetupTuple*>(&other);
	bool isSame = true;

	// Check if type SetupTuple
	if(sPtr){


		// Compare sizes
		if( this->value.size() != sPtr->value.size() ){ 
			isSame = false;
		}else{
			// Compare elements 
			std::list<SetupBase*>::const_iterator it1 = value.begin();
			std::list<SetupBase*>::const_iterator it2 = sPtr->value.begin();
	
			while(isSame && it1 != value.end()){

				if(!(**it1 == **it2)){ // FIXME - overload != operator
					isSame = false;
				}

				it1++;
				it2++;
			}
		}

		return isSame;

	}else{
		isSame = false;
	}

	return isSame;
  } 

  bool operator!=(const SetupBase &other) const {
	return !(*this == other);
  }


  SetupTuple & operator=(const SetupBase &rhs){
	if(this == &rhs){
		return *this;
	}else{
		const SetupTuple * sPtr = dynamic_cast<const SetupTuple*>(&rhs);

		value.clear();  // FIXME - implement destructors

	    	for(std::list<SetupBase*>::const_iterator it = sPtr->value.begin(); it != sPtr->value.end(); ++it) {
			// may be wrong - call add
			value.push_back((*it)->clone()); 
		}

		return *this;
	}
  }  


  bool isDuplicate(SetupBase *sbPtr) const{
	if(NULL != sbPtr){
    		for(std::list<SetupBase*>::const_iterator it = value.begin(); it != value.end(); ++it) {
			if(*sbPtr == **it){
				return true;	
			}
		}
	}
	return false;
  }
  SetupTuple& add(SetupBase* e) {
    value.push_back(e);
    return *this;
  }
  SetupTuple& add(bool i) {
    return add(new SetupBool(i));
  }
  SetupTuple& add(int i) {
    return add(new SetupInt(i));
  }
  SetupTuple& add(double i) {
    return add(new SetupFloat(i));
  }
  SetupTuple& add(const char* i) {
    return add(new SetupString(i));
  }
  SetupTuple& add(const SetupTuple i) {
    return add(new SetupTuple(i));
  }

  // may be wrong - can't find implementation
  SetupTuple& add(SetupSet& i);
};

class SetupSet : public SetupBase {
 public:
  std::set<SetupBase*> value;

  SetupSet(){}

  ~SetupSet(){

	value.clear();

  }

  SetupSet(const SetupSet& r){
    	for(std::set<SetupBase*>::const_iterator it = r.value.begin(); it != r.value.end(); ++it) {
		// may be wrong - may need to call add instead
		value.insert((*it)->clone()); 
	}
  }

  SetupSet(bool b) {
    add(new SetupBool(b));
  }
  SetupSet(int i) {
    add(new SetupInt(i));
  }
  SetupSet(double f) {
    add(new SetupFloat(f));
  }
  SetupSet(const char* s) {
    add(new SetupString(s));
  }
  SetupSet(const SetupTuple t) {
    // may be wrong - may need to call new 
    add(t);
  }

  SetupBase* clone() const {
	return new SetupSet(*this);
  }

  std::string toString() const {
    std::stringstream out;
    out << "{";
    for(std::set<SetupBase*>::const_iterator it = value.begin(); it != value.end(); ++it) {
      out << (*it)->toString();
      if(it != --value.end())
	out << ", ";
    }
    out << "}";
    return out.str();
  }
	
  friend std::ostream& operator<< (std::ostream &out, SetupSet &rhs) {
	out << rhs.toString();
	return out;
  }

  bool operator==(const SetupBase &other) const {

	bool retVal = false;
	const SetupSet * sPtr = dynamic_cast<const SetupSet*>(&other);

	// Check if type SetupSet
	if(sPtr){

		// Compare sizes
		if( this->value.size() != sPtr->value.size() ){
			retVal = false;
		}else{

			// Compare elements - Assumes no duplicates in either set.
		    	for(std::set<SetupBase*>::const_iterator it = value.begin(); it != value.end(); ++it) {
				if(!sPtr->isDuplicate(*it)){
					return false;
				}
			}
			retVal = true;

		}
	}else{
		retVal = false;
	}

	return retVal;
  } 

  bool operator!=(const SetupBase &other) const {
	return !(*this == other);
  }


  SetupSet & operator=(const SetupBase &rhs){
	if(this == &rhs){
		return *this;
	}else{
		const SetupSet * sPtr = dynamic_cast<const SetupSet*>(&rhs);

		value.clear();  // FIXME - implement destructors

	    	for(std::set<SetupBase*>::const_iterator it = sPtr->value.begin(); it != sPtr->value.end(); ++it) {
			// may be wrong - probably call new and add
			value.insert((*it)->clone()); 
		}

		return *this;
	}
  } 

  bool isDuplicate(SetupBase *sbPtr) const{

	if(NULL != sbPtr){
    		for(std::set<SetupBase*>::const_iterator it = value.begin(); it != value.end(); ++it) {
			//std::cout << "Calling == with types: " << typeid(sbPtr).name() << ", " << typeid(*it).name() << std::endl;
			if((*sbPtr) == (**it)){ 
				return true;	
			}
		}
	}
	return false;
  }


  SetupSet& add(SetupBase* e) {
    if(!isDuplicate(e)){
    	value.insert(e);
    }
    // wrong - delete e if duplicate
    return *this;
  }
  SetupSet& add(bool i) {
    return add(new SetupBool(i));
  }
  SetupSet& add(int i) {
    return add(new SetupInt(i));
  }
  SetupSet& add(double i) {
    return add(new SetupFloat(i));
  }
  SetupSet& add(const char* i) {
    return add(new SetupString(i));
  }
  SetupSet& add(const SetupTuple i) {
    return add(new SetupTuple(i));
  }
  SetupSet& add(const SetupSet i) {
    return add(new SetupSet(i));
  }
};

SetupSet & setUnion(const SetupSet &lhs, const SetupSet &rhs){
	SetupSet & retSet = *(new SetupSet(lhs));


    	for(std::set<SetupBase*>::const_iterator it = rhs.value.begin(); it != rhs.value.end(); ++it) {
		if(!retSet.isDuplicate(*it)){
			// may be wrong - may need new
			retSet.add(*it);
		}
	}

	return retSet;	
}

SetupSet & setMinus(const SetupSet &lhs, const SetupSet &rhs){
	SetupSet & retSet = *(new SetupSet());

    	for(std::set<SetupBase*>::const_iterator it = lhs.value.begin(); it != lhs.value.end(); ++it) {

		if(!rhs.isDuplicate(*it)){
			// may be wrong - may need new
			retSet.add(*it);
		}
	}

	return retSet;	
}


SetupSet & setIntersect(const SetupSet &lhs, const SetupSet &rhs){
	SetupSet & retSet = *(new SetupSet());

    	for(std::set<SetupBase*>::const_iterator it = lhs.value.begin(); it != lhs.value.end(); ++it) {

		if(rhs.isDuplicate(*it)){
			// may be wrong - may need new
			retSet.add(*it);
		}
	}

	return retSet;	
}


SetupSet & setCross(const SetupSet &lhs, const SetupSet &rhs){
	SetupSet & retSet = *(new SetupSet());


    	for(std::set<SetupBase*>::const_iterator it1 = lhs.value.begin(); it1 != lhs.value.end(); ++it1) {
	    	for(std::set<SetupBase*>::const_iterator it2 = rhs.value.begin(); it2 != rhs.value.end(); ++it2) {


			SetupTuple * stuple = new SetupTuple();

			// may be wrong - may need new
			stuple->add(*it1);
			stuple->add(*it2);
	
			if(!rhs.isDuplicate(stuple)){
			
				retSet.add(stuple);
			}else{
				// may be wrong - double check
				//delete stuple
			}
		}
	}

	return retSet;	
}

SetupTuple& SetupTuple::add(SetupSet& i) {
  return add(new SetupSet(i));
}

// may be wrong - may need to use new
SetupTuple TupleFactory(bool t) {
  return  (new SetupTuple())->add(t);
}
SetupTuple TupleFactory(int t) {
  return  (new SetupTuple())->add(t);
}
SetupTuple TupleFactory(double t) {
  return  (new SetupTuple())->add(t);
}
SetupTuple TupleFactory(const char* t) {
  return  (new SetupTuple())->add(t);
}
SetupTuple TupleFactory(SetupTuple t) {
  return  (new SetupTuple())->add(t);
}
SetupTuple TupleFactory(SetupSet t) {
  return  (new SetupTuple())->add(t);
}

SetupSet SetFactory(bool t) {
  return  (new SetupSet())->add(t);
}
SetupSet SetFactory(int t) {
  return  (new SetupSet())->add(t);
}
SetupSet SetFactory(double t) {
  return  (new SetupSet())->add(t);
}
SetupSet SetFactory(const char* t) {
  return  (new SetupSet())->add(t);
}
SetupSet SetFactory(SetupTuple t) {
  return  (new SetupSet())->add(t);
}
SetupSet SetFactory(SetupSet t) {
  return  (new SetupSet())->add(t);
}
SetupSet SetFactory(int b, int e) {
  SetupSet ret;
  for(int i = b; i <= e; ++i) {
    ret.add(i);
  }
  return ret;
}

int setCardinality(const SetupSet &s){
	return s.value.size();	
}
