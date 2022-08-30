#include <stdbool.h>
#include <stdio.h>
struct lastwrite {
  long addr;
  long val;
};

// Simple circular buffer keeping track of the last NumWrites writes.
const static long NUMWRITES = 1000;
static struct lastwrite lastwrites[NUMWRITES];
static bool circled_buffer = false;
static long lastwrite_index = 0;

int addr_index(long addr) {
  int numwrites = circled_buffer ? NUMWRITES : lastwrite_index;
  for(int i = 0; i < numwrites; ++i) {
    if(lastwrites[i].addr == addr) {
      return i;
    }
  }
  return -1;
}

static long getbits(long val, long start, long end) {
  long mask = 0;
  for(long i = start; i <= end; ++i) {
    mask |= (1 << i);
  }
  return (val & mask) >> start;
}

void check_assump(long addr, long start, long end, long val) {
  // assert that the last write to addr is in the range [start, end]
  int index = addr_index(addr);
  if(index == -1) {
    panic("dynamic assertion failed. No record of writing to %d", addr);
  }
  if(getbits(lastwrites[index].val, start, end) != val) {
    panic("dynamic assertion failed. Last write to %d was %d",
          addr, lastwrites[index].val);
  }
}

long get32(long addr) {
  volatile long *ptr = (volatile long *)addr;
  return *ptr;
}

long put32(long addr, long val) {
  volatile long *ptr = (volatile long *)addr;
  int index = addr_index(addr);
  if(index == -1) {
    if(lastwrite_index == NUMWRITES - 1) {
      circled_buffer = true;
      lastwrite_index = 0;
    }
    index = lastwrite_index++;
  }
  lastwrites[index].addr = addr;
  lastwrites[index].val = val;
  *ptr = val;
}

// replace from start to end with val
long var_replace(long var, long start, long end, long val) {
  long mask = 0;
  for(long i = start; i <= end; ++i) {
    mask |= (1 << i);
  }
  return (var & ~mask) | (val << start);
}