#include <stdio.h>

int main(void)
{
  const int x = 0xA8F1;
  const int y = 0x17DF;
  const int z = 0xCDB8;

  printf("0x%x\n", (~x|~y));
  return 0;
}
