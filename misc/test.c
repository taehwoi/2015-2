#include <stdio.h>
int foo(int x, int y, int n)
{
  int t1 = x + y;
  int t2 = t1 * n;
  int t3 = 8 * x;
  int t4 = t2 + t3;
  int t5 = t1 * t4;
  
  return t5;
}

int main(void)
{
  int x = 3,y=2,n=5;
  foo(x,y,n);
}
