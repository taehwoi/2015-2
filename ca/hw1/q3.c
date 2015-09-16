#include <stdio.h>
#include <limits.h>

int main(void)
{
  /*printf("%d\n", -2147483647-1 == 2147483648U);*/
  printf("%d\n",2147483647 > (int)2147483648U);
  return 0;
}
