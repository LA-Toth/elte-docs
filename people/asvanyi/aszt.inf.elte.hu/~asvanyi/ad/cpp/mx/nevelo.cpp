#include <iostream>
using namespace std;

// Visszaadja a megfelelõ határozott névelõt. 
// Szerzõ: Ásványi Tibor, 2008. október 15.
string nevelo(int n){
  if( n<=0 ) return "A ";
  while( n > 999 ) n /= 1000;
  if( n==1)  return "Az ";
  while( n > 9 ) n /= 10;
  if( n==5)  return "Az ";
  return "A ";
}

int main(){
  int n;
  char c;

  cout << "Az a/az + ege'sz sza'm program tesztje. \n\n";
  do{
    cout << "Az ege'sz sza'mok? (betu\" a ve'ge)\n";
    while(cin>>n){       
      cout<< nevelo(n) << n << " sza'mot ovastam be.\n";	
    }
    cin.clear();
    while(cin.get()!='\n');
    cout << "k:kile'p; ma's:a teszt megisme'tle'se.\n";
    c=cin.get();
    while(cin.get()!='\n');
  }while(c!='k' && c!='K');

  return 0;
}
