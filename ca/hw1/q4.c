#include <stdio.h>

int f1(unsigned w)
{
  return (int) ((w << 26) >> 26);
}
int f2(unsigned w)
{
  return ((int) w << 26) >> 26;
}

int main(void)
{
  const int w = 0xDEADBEEF;

  printf("0x%08x\n", f1(w));
  printf("0x%08x\n", f2(w));
  
  return 0;
}
