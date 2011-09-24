CC = gcc
CFLAGS = -g Wall

all: find_ratio heapsize obfuscated obfuscated1 qsort sizeof strstr trans

clean:
	rm -f find_ratio heapsize obfuscated obfuscated1 qsort sizeof strstr trans

find_ratio: find_ratio.c
	${CC} -o $@ $@.c -lm

heapsize: heapsize.c
	${CC} -o $@ $@.c

obfuscated: obfuscated.c
	${CC} -o $@ $@.c

obfuscated1: obfuscated1.c
	${CC} -o $@ $@.c

qsort: qsort.c
	${CC} -o $@ $@.c

sizeof: sizeof.c
	${CC} -o $@ $@.c

splitFileIntoWords: splitFileIntoWords.c
	${CC} -o $@ $@.c

strstr: strstr.c
	${CC} -o $@ $@.c

trans: trans.c
	${CC} -o $@ $@.c
