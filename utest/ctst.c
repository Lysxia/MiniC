/*  Testing C syntax
 *  call gcc -c 'this_file.c'
 */

#define TEST 8

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
int m()
{
    return e
	();
}
/* COMPILES
 * Whitespace is ignored */
#endif

#if TEST==6
void f()
{
    int a;
    float a;
}
/* FAILS */
#endif

#if TEST==7
int a;
void f ()
{
    char a;
}
/* COMPILES*/
#endif

#if TEST==8
void f ()
{
    int a;
    {
	int a;
    }
}
#endif

int main()
{
    return 0;
}
