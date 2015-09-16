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
  const int w0 = 0xDEADBEEF;
  const int w1 = 0xBADDCAFE;
  const int w2 = 0x0D15ea5e;
  const int w3 = 0xfee1dead;

  printf("0x%08x\n", f1(w3));
  printf("0x%08x\n", f2(w3));
  
  return 0;
}
