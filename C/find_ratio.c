/*
 * This program attempts to tuen a decmial into a ratio, like if given
 * 0.16666 as input, it will print out 1/6
 * It takes to number as input, a Real number and how close the ratio 
 * needs to be the actual numer, the precision.
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define MAXDENOM 500

struct rat{
	int num;
	int denom;
};

void readPrint(double x, double y);
struct rat ratio(double num, double tol);
double round(double);
void usage(char *name);

#define STRTOD_ERROR(input,endptr) do { \
    if (input == endptr) { \
        usage(argv[0]); \
    } \
} while(0)

int main(int argc, char *argv[])
{
	double number, precision;
	char *endptr;
	
	if (argc == 1) {
		number    = 0.1666667;
		precision = 0.001;

	} else if (argc == 2) {
		number = strtod(argv[1], &endptr);
		STRTOD_ERROR(argv[1], endptr);
		precision = number / 1000;

	} else if (argc == 3) {
		number = strtod(argv[1], &endptr);
		STRTOD_ERROR(argv[1], endptr);
		
		precision = strtod(argv[2], &endptr);
		STRTOD_ERROR(argv[2], endptr);

	} else {
		usage(argv[0]);
	}

	struct rat ans = ratio(number, precision);
	printf("input: %f\noutput: %d/%d = %f\n", number, ans.num, ans.denom, (double) ans.num / ans.denom);
	return EXIT_SUCCESS;
}

struct rat ratio(double num, double tol)
{
	double i = 0;
	double error, prod, roundProd;
	do {
		i++;
                prod = num * i;
                roundProd = round(prod);

		error = prod - roundProd;
                if (error < 0) {
                    error = -error;
                }

	} while ((tol < error) && (i < MAXDENOM));
	struct rat ans = {round(num * i), i};
	
	return ans;
}

double round(double x)
{
	return (double) floor(x + 0.5);
}

void usage(char *name)
{
	printf("%s decimal [precision]\n", name);
        exit(EXIT_FAILURE);
}
