#include <stdio.h>

int fun(int from, int to)
{
  int tmp=0;
  int result;
  int i;

  for (result=1,i = from; i < to; i++) {
    tmp = i < 10 ? tmp+i : tmp-i;
  }

  if(tmp%2==1)
    tmp=result;


  return result;
}

int main(void)
{
  int result = fun(1,10);
  

  return 0;
}
