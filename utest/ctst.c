/*  Testing C syntax
 *  call gcc -c 'this_file.c'
 */

#define TEST 14

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
union b { int a;};
struct b { int a;};
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

#if TEST==9
char a=0141;
#endif

#if TEST==10
char a=97,b=98;
int c()
{
    return a>b;
}
#endif

#if TEST==11
struct a {};
int a() {}
#endif

#if TEST==12
struct a {};
int b()
{
    struct a a;
    int** b;
    a=b;
}
#endif

#if TEST==13
int b()
{
    0=0;
}
#endif

#if TEST==14
int a;
int b() {
    sizeof(void);
}
#endif

int main()
{
    #if TEST==14
      printf("%d %d\n",sizeof(void),sizeof(int));
    #endif
    return 0;
}
