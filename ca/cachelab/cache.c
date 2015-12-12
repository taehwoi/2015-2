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
  assert( ways <= lg(blocksize) );

  Cache *c = malloc(sizeof(Cache));
  //reset values
  memset(c, 0, sizeof(Cache));
  //set parameters
  c->capacity = capacity;
  c->blocksize = blocksize;
  c->ways = ways;
  c->sets = sets;
  c->tagshift = tagshift;
  
  c->rp = rp;
  c->wp = wp;
  

  c->set = malloc(sizeof(Set) * sets);

  //for each set, allocate lines
  for (i = 0; i < sets; i++) {
    c->set[i].way = malloc(sizeof(Line) * ways);
    c->set[i].rr = 0;
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
{//free the cache
  int i;
  for (i = 0; i < c->sets; i++) {
    free(c->set[i].way);
  }
  free(c->set);
  free(c);
}

int line_access(Cache *c, Line *l, uint32 tag)
{//move the accessed Line to the front, and shift the rest
  int i;
  int hit = 0;

  for (i = 0; i < c->ways; i++) {
    l[i].age += l[i].valid; //use valid bit to increment age
    if (l[i].tag == tag && l[i].valid == 1) {
      l[i].age = 0;
      hit = 1;
    }
  }
  return hit;
  // update data structures to reflect access to a cache line
}


int line_alloc(Cache *c, Line *l, uint32 tag)
{//search the way, and put it somewhere
  int i;
  for (i = 0; i < c->ways; i++) {
    if (l[i].valid == 0) {//empty line
      l[i].tag = tag;
      l[i].valid = 1;
      l[i].age = 0;
      return 1; //success
    }
  }
  return 0; //fail : cache is full
  
}

uint32 set_find_victim(Cache *c, Set *s)
{//choose victim
  int victim=0;
  int max=0; 
  int i;
  switch (c->rp) {
    case RP_RR:
      victim = s->rr;
      s->rr = ((s->rr + 1) % c->ways);
      break;
    case RP_RANDOM: 
      victim = rand() % c->ways;
      break;
    case RP_LRU: 
      max = s->way[0].age;
      for (i = 1; i < c->ways; i++) {//search oldest line
        if (s->way[i].valid == 1 && max < s->way[i].age) {
          max = s->way[i].age;
          victim = i;
        }
      }
      break;
    default:
      return EXIT_FAILURE;
  }

  //set validity to 0 : empty line
  s->way[victim].valid = 0;
  c->s_evict++;
  return 0;
}

//the only visible interface
void cache_access(Cache *c, uint32 type, uint32 address, uint32 length)
{
  //compute block offset (not needed in our implementation)
  uint32 bl_offset = c->blocksize - 1; //fill bitmask with 1s
  bl_offset = address & bl_offset;

  //compute set index
  address >>= lg(c->blocksize); 
  uint32 set_i = c->sets - 1;
  set_i = address & set_i;

  //compute tag
  uint32 tag = (address >> lg(c->sets));

  if ( line_access(c, c->set[set_i].way, tag) )
    c->s_hit++;
  else {
    if (!(type == 1 && c->wp == WP_NOWRITEALLOC)) {
      if ( !( line_alloc(c, c->set[set_i].way, tag) ) )
      {//set full
        set_find_victim(c, &c->set[set_i]);
        line_alloc(c, c->set[set_i].way, tag);
      }
    }
    c->s_miss++;
  }

  c->s_access++;
}
