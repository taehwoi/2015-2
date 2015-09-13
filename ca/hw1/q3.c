#include <stdio.h>

int main(void)
{
  /*printf("%d\n", -2147483647-1 == 2147483648U);*/
  printf("%u\n",2147483647U);
  printf("%u\n",-2147483647-1);
  printf("%u\n",2147483647U < -2147483647-1);
  printf("%d\n", -1 < 0U);
  return 0;
}
