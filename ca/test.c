#include <stdio.h>

int add1(int x)
{//add with +=
  x+=1;
  return x;
}

int add2(int x)
{//add with increment operator
  x++;
  return x;
}

int add3(int x)
{//add 1 normally
  x=x+1;
  return x;
}

int add4(int x, int y)
{//add 1 with other variable(y = 1)
  x=x+y;
  return x;
}

int main(void)
{
  int x=987654321;

  add1(x);
  add2(x);
  add3(x);
  add4(x,1);
  return 0;
}
