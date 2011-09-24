/*
 * This program will use a binary search to find out the max amount of
 * memory it is able to allocate (the heapsize).
 */

#include <stdio.h>
#include <stdlib.h>
#define nth_bit(type) (sizeof(type) *8 -1) 	//finds the number for the last 
						//bit in a given variable type

unsigned long findHeapSize();

int main(int argc, char *argv[])
{
	unsigned long size = findHeapSize();
	printf("The heap is %lu bytes\n", size);
	return 0;
}

/*
 *  Function: findHeapSize()
 * Description: Performs a binary search to determine the size of the heap 
 *	        by repeatedly allocating memory, checking whether  
 *          it was successfully allocated, then freeing it.
 *          This process is repeated until the heapsize is determined
 * runtime:	O(n) where n is the size of a word on the prcessor
 * returns:	int with the size of availble space in the heap
 */ 
unsigned long findHeapSize()
{
	char *pointer;
	unsigned long size = 0;
	int bit;
	
	bit = nth_bit(unsigned long); //start testing with the most 
                                  //significant bit

	//We start with all bits in the long size unset, and set 
	//them one at a time to check if that bit makes the 
	while (bit >= 0) {
		size |= (1 << bit); //set the bit being tested

		pointer = (char *) malloc(sizeof(char) * size);

		if (pointer == NULL)    	//if we tried to malloc more
			size &= (~(1 << bit));  //than is available unset the bit
 
		bit--; //move to the next smallest bit
		free(pointer);
	}
	return size;
}
