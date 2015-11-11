#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define SIZE 530000000

int src[SIZE], dst[SIZE];

/* $begin ncopy */
/*
 * ncopy - copy srcc to dst, returning number of positive ints
 * contained in srcc array.
 */
//TODO: fastest way to iterate an array
//rule 1: don't use pointer arithmetic
//rule 2: avoid conditionals
//rule 3: we HAVE to iterate an array to
//        find positive inputs, do copying while iterating
int ncopy(int *src, int *dst, int len)
{
  int count = 0;
  int i=0;
  /*memcpy(dst,src,len*sizeof(int));*/
  for (; i < len; i++) {
    /*dst[i]=src[i];*/
    count+= ((dst[i] = src[i]) > 0);

//Original:
    /*if (src[i]>0) {*/
      /*++count;*/
    /*}*/
//Opti: check sign bit to determine sign
    /*count += src[i]*/
  }

  /*while (len > 0) {*/
    /*val = *src++;*/
    /**dst++ = val;*/
    /*if (val > 0)*/
      /*count++;*/
    /*len--;*/
  /*}*/
  return count;
}
/* $end ncopy */

int main(int argc, const char *argv[])
{
  int i, count;
  int len;
  len = atoi(argv[1]);

  for (i=0; i<len; i++)
    src[i]= i+1;
  count = ncopy(src, dst, len);
  printf ("count=%d\n", count);
  /*exit(0);*/
}
