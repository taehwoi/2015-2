#include <stdio.h>
int iffun(int x, int y, int z)
{
  int val = 0;
  if (x != 0) {
    val = 9 * x;
  }else if (z+ 3 < y) {
    val = x + y;
  }else if (z <= 6) {
    val = y + 2*x+z;
  }else {
    val = x + z;
  }
}

int main(void)
{
  int x=0,y=2,z=3;
  iffun(x,y,z);
  return 0;
}
