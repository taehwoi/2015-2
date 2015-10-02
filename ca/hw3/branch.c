#include <stdio.h>
int branch(int x, int y)
{
  int val;
  if (x < -5) {
    val = x-y;
  } else if(0) {
    val = x;
  } else {
    val = y;
  }
  return val;
}

int main(void)
{
  int x = 3, y = 5;
  branch(x,y);

  return 0;
}
