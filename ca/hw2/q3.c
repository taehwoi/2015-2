#include <stdio.h>
void decode(int *xp, int *yp, int *zp)
{
  int a,b,c;

  a = *xp;
  b = *yp;
  c = *zp;

  *xp = b;
  *yp = c;
  *zp = a;
}

int main(void)
{
  int x = 1;
  int y = 2;
  int z = 3;

  decode(&x,&y,&z);

  printf("%d %d %d\n",x,y,z);
  return 0;
}
