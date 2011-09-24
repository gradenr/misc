/*
 * A generic qicksort function I wrote when I was bored one summer and 
 * curious about void pointers and generic C programming.
 */ 

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define TYPE long long int
#define NUM 7
//to change type change #define and replace all %d(or whatever is appropriate
//for current type) with %x so that printf can print values correctly

void myqsort(void *base, size_t num, size_t size, 
	int (*comparator) (const void *, const void *));
void swap(void *x, void *y, size_t size);
void *mybsearch(const void *key, const  void *base, size_t num, size_t size, 
	int (*comparator) (const void *, const void *));

const TYPE orig[]   = { 40, 10, 100, 90, 20, 25, 25 };
      TYPE values[] = { 40, 10, 100, 90, 20, 25, 25 };

int compare (const void * a, const void * b)
{
   return (int)( *(TYPE*)a - *(TYPE*)b );
}

int main ()
{
  int n;
  qsort (values, NUM, sizeof(TYPE), compare);
  for (n=0; n<NUM; n++)
     printf ("%lld  ",values[n]);
  printf("\n");
  TYPE ret;
  for (n=0; n<NUM; n++) {
    TYPE *ret = (TYPE *) (bsearch(&orig[n], values, NUM, sizeof(TYPE), compare));
    if (ret == 0)
	printf("couldn't find value %lld \n", orig[n]);
    else if (*ret != orig[n])
	printf("searched for %lld, got %lld, #%i\n", orig[n], *ret, ret-values);
  }
  return 0;
}

void myqsort(void *base, size_t num, size_t size,
	int (*comparator)(const void *, const void *))
{
    if (size == 1)
	return;
    int  i;
    int  farside = (num-1);
    for (i = 1; farside > i; i++)
    {
	if ((*comparator)(base, base + i*size) >= 0) {
	    swap(base + farside*size, base + i*size, size);
	    farside--;
	}
    }
    myqsort(base,              num/2,       size, comparator);
    myqsort(base + num/2*size, num - num/2, size, comparator);
}
void swap(void *x, void *y, size_t size)
{
    void * temp = malloc(size);
    memcpy(x,    temp, size);
    memcpy(y,    x,    size);
    memcpy(temp, y,    size);
}

void *mybsearch(const void *key, const  void *base, size_t num, size_t size,
	int (*comparator) (const void *, const void *))
{
    if (comparator(base, base + (num-1)*size) > 1)
	return 0;
    int mid = (num-1)/2;
    if (comparator(base+mid*size, key) > 0)
	return bsearch(key, base,              mid-1,      size, comparator);
    if (comparator(base+mid*size, key) < 0)
	return bsearch(key, base+(mid+1)*size, size-mid+1, size, comparator);
    return (void *) base+mid;
}
