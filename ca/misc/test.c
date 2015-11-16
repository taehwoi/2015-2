#include <stdio.h>
void swap(int a, int b)
{
  int tmp;
  tmp = a;
  a = b;
  b = tmp;
}
void swap2(int a[])
{
  int tmp;
  tmp = a[0];
  a[0] = a[1];
  a[1] = tmp;
}
int main(void)
{
  int a=1,b=2;
  int A[2]={3,4};

  swap(a,b);
  /*swap(A[0],A[1]);*/
  swap2(A);
  printf("%d%d\n",a,b);
  printf("%d%d\n",A[0],A[1]);
}
