def int main()
{
	string str1;
	string str2;
	string str3;
	str1="abc";
	str2="bcd";
	str3=str1;	
	if(str1:=str2)
	{print("str1 is the same with str2 ");print_newline();}
	if(str1:=str3)
	{print("str1 is the same with str3 ");print_newline();}
	if(str3:=str2)
	{print("str3 is the same with str2 ");print_newline();}
}
