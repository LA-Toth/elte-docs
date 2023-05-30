#include <iostream>
#include<stdlib.h>

// The simplified and optimized code of 
// Andrey Khasanov
// Asvanyi Tibor, Budapest, 13.12.2012

using namespace std;

struct Node
{
  int a;
  Node* left;
  Node* right;

  Node(){left = right = 0;}
};

// Destructor should be written.
class BinTree
{
public:
  BinTree();
  void display();
  void reBuild(string& strTree);
protected:
private:
  Node* t;
  void inorder_print(Node*& t);
  void reBuild(Node*& t, string& str, int& i);
};

BinTree::BinTree()
{
  t = 0;
}

void BinTree::display()
{
  if (t != 0) {
    inorder_print(t);
    cout << endl;
  } else {
    cout << "The tree is empty.\n";
  }
}

void BinTree::reBuild(string& strTree)
{
  int i = 1;
  if(strTree.at(0)=='(') reBuild(t, strTree, i); 
}

void BinTree::inorder_print(Node*& t)
{
  if (t != 0) {
    std::cout << "(";
    inorder_print(t->left);
    std::cout << (t->a);
    inorder_print(t->right);
    std::cout << ")";
  }
}

// Note: Too short and some other erroneus inputs are not handled.
void BinTree::reBuild(Node*& t, string& str, int& i)
{
  t=new Node();
  char c = str.at(i++);
  if (c == '('){ 
    reBuild(t->left, str, i);
    c = str.at(i++);
  }
  int x = 0;
  while (c >= '0' && c <= '9') {
    x = x*10 + c-'0';
    c = str.at(i++);
  }
  t->a = x;
  if (c == '('){ 
    reBuild(t->right, str, i);
    c = str.at(i++);
  }
  if (c != ')') { 
    cout << "Input error at " << str.substr(i-1,str.length()-i+1) <<"\n";
    exit(1);
  }
}

int main()
{
  string strTree;
  cout << "Please type in the parenthized infix form of the tree:\n";
  cin >> strTree;

  BinTree tree;

  tree.reBuild(strTree);
  cout<< "Built.\n";
  tree.display();

  return 0;
}
