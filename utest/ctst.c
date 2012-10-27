/*  Testing C syntax
 *  call gcc -c 'this_file.c'
 */

#define TEST 5

#include <stdio.h>
#include <stdlib.h>

#if TEST==0
int main(int argc, char** argv)
{
    return 0;
}
#endif

#if TEST==1
void a(){}
void a(){}
/* FAILS
 * do not bind the same function name twice */
#endif

#if TEST==2
union b {};
struct b {};
/* FAILS
 * union and struct share a namespace */
#endif

#if TEST==3
union c {};
int c(){}
/* COMPILES
 * struct/unions and functions can share a same name */
#endif

#if TEST==4
int d;
int d(){}
/* FAILS */
#endif

#if TEST==5
int e(){}
int main()
{
    return e
	();
}
/* COMPILES
 * Whitespace is ignored */
#endif
