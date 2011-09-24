/*
 * This program was requested as part of the hiring process for a
 * company. They requested that I write a strstr function as well as
 * several tests without including any C libraries.
 */
#define NULL ((void *)0)
#define TRUE 1
#define FALSE 0

int subStrAt(const char *needle, const char *haystack);

int main(int argc, char **argv)
{
    if (test()) {
        return 0;
    }
    return 1;
}

/* strstr takes two strings and return the first occurrence of the 
 * first string in the second.  It returns eiher a pointer to the first
 * occurence, or a NULL pointer if it does not exist in the string.
 */
char *strstr(const char *needle, const char *haystack)
{
	//a do-while loop is necessary for when both strings are empty
	do {
		if  (subStrAt(needle, haystack)) 
			return (char *) haystack;
		haystack++;
	} while (*haystack != '\0');
	return NULL;
}

/* Returns true if the first string is a substring starting at index 0
 * of the second string
 */
int subStrAt(const char *needle, const char *haystack)
{
	while (*needle != '\0' && *haystack != '\0')
	{
		if (*needle != *haystack)
			return FALSE;
		needle++;
		haystack++; 
	}
	if (*needle == '\0')
		return TRUE;
	return FALSE;
}

/* Tests whether strstr returns the correct value for various test cases.
 * It returns true (1) if strstr passes all the tests and false (0) if
 * it fails any test.
 */
int test()
{
	//these strings are necessary to remember locations so that the 
	//answers array can point to the same locations as the haystacks array
	char *empty  = "";
	char *hello  = "hello";
	char *foobar = "foobar";
	char *bar    = "bar";
	char *abc    = "ABC ABCDE ABCDEFGH";

	//test cases
	//first 3 test combinations with an empty string
	//next two search for strings at the extremities of the haystack
	//next two search for values in the middle of longer strings
	//next two cases test for when the needle is the hay stack
	//last cases test a near matche
	//program stops testing when the haystack is a null pointer as this is
	//input that strstr canot handle (it segfaults)
	char needle[][8] = {"",    "",  "foo", "b", "o",     "ABCDEF", "ABCDEA", "foobar",  "foo", "foobat"};
	char *haystack[] = {empty, bar, empty, bar, hello,   abc,      abc,       foobar,    bar,  foobar, NULL  };
	char *answers[]  = {empty, bar, NULL,  bar, hello+4, abc+10,   NULL,      foobar,    NULL, NULL    };

	int i;
	for (i = 0; haystack[i] != NULL; i++) {
		if (strstr(needle[i], haystack[i]) != answers[i])
			return FALSE;
	}
	return TRUE;
}
