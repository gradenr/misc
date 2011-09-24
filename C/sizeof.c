#include <stdlib.h>
#include <stdio.h>
 
 int main (int argc, char *argv[])
 {
     int a;
     int b;
     int c;
     int d;
     int e;
     int f;
     int g;
     a = sizeof (char);
     b = sizeof (short);
     c = sizeof (int);
     d = sizeof (long);
     e = sizeof (float);
     f = sizeof (double);
     g = sizeof (long double);
     printf ("sizes: char=%d, short=%d, int=%d, long=%d, float=%d, "
		"double=%d, long double=%d\n", a, b, c, d, e, f, g);
     return 0;
}
