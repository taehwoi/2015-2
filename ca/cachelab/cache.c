//------------------------------------------------------------------------------
// 4190.308                     Computer Architecture                  Fall 2015
//
// Cache Simulator Lab
//
// File: cache.c
//
// (C) 2015 Computer Systems and Platforms Laboratory, Seoul National University
//
// Changelog
// 20151119   bernhard    created
//

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include "cache.h"
#include <string.h>

char RP_STR[RP_MAX+1][32] = {
  "round robin", "random", "LRU (least-recently used)",
  "MRU (most-recently used)", "LFU (least-frequently used)",
  "MFU (most-frequently used)"
};

char WP_STR[2][20] = {
  "write-allocate", "no write-allocate"
};

int lg(uint32 n)
{//don't want to include math library,
 //n is known to be power of 2
  int cnt=0;
  while(n > 1) {
    n /= 2;
    cnt++;
  }
  return cnt;

}


Cache* create_cache(uint32 capacity, uint32 blocksize, uint32 ways,
                    uint32 rp, uint32 wp, uint32 verbosity)
{
  //get number of sets
  uint32 sets = capacity / (blocksize * ways);
  uint32 tagshift = lg(blocksize * sets);
  int i;

  // check c parameters
  //   - capacity, blocksize, and ways must be powers of 2
  assert( ISPOW2(capacity) && ISPOW2(blocksize) && ISPOW2(ways) );
  //   - capacity must be > blocksize
  assert( capacity >= blocksize );
  //   - number of ways must be >= the number of blocks
  //FIXME
  assert( ways <= blocksize );

  Cache *c = malloc(sizeof(Cache));
  //reset values
  memset(c, 0, sizeof(Cache));
  //set parameters
  c->capacity = capacity;
  c->blocksize = blocksize;
  c->ways = ways;
  c->sets = sets;
  c->tagshift = tagshift;
  

  c->set = malloc(sizeof(Set) * sets);

  //for each set, allocate lines
  for (i = 0; i < sets; i++) {
    c->set[i].way = malloc(sizeof(Line) * ways);
  }
  // 3. print cache configuration
  printf("cache configuration:\n"
         "  capacity:        %6u\n"
         "  blocksize:       %6u\n"
         "  ways:            %6u\n"
         "  sets:            %6u\n"
         "  tag shift:       %6u\n"
         "  replacement:     %s\n"
         "  on write miss:   %s\n"
         "\n",
         capacity, blocksize, ways, sets, tagshift, RP_STR[rp], WP_STR[wp]); 

  // 4. return c
  return c;
}

void delete_cache(Cache *c)
{//free reversed order
  int i;
  for (i = 0; i < c->sets; i++) {
    free(c->set[i].way);
  }
  free(c->set);
  free(c);
}

void line_access(Cache *c, Line *l)
{
  // TODO
  //
  // update data structures to reflect access to a cache line
}


void line_alloc(Cache *c, Line *l, uint32 tag)
{
  // TODO
  //
  // update data structures to reflect allocation of a new block into a line
}

uint32 set_find_victim(Cache *c, Set *s)
{
  switch (c->rp) {
    case RP_RR: 
      break;
    case RP_RANDOM: 
      break;
    case RP_LRU: 
      break;
    default:
      return EXIT_FAILURE;
      
  }
  // TODO
  //
  // for a given set, return the victim line where to place the new block
  return 0;
}

//the only visible interface (or supposed to be.)
void cache_access(Cache *c, uint32 type, uint32 address, uint32 length)
{

  //compute block offset
  uint32 bl_offset = c->blocksize - 1; //fill bitmask with 1s
  bl_offset = address & bl_offset;

  //compute set index
  address >>= lg(c->blocksize); 
  uint32 set_index = c->sets - 1;
  set_index = address & set_index;

  //compute tag
  uint32 tag = (address >> lg(c->sets));

  printf("t: %x s: %d\n", tag, set_index);


  // TODO
  //
  // simulate a cache access
  // 1. compute set & tag (v)
  // 2. check if we have a cache hit
  // 3. on a cache miss, find a victim block and allocate according to the
  //    current policies
  // 4. update statistics (# accesses, # hits, # misses)

  c->s_access++;
}
