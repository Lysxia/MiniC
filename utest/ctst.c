/*  Testing C syntax
 *  call gcc -c 'this_file.c'
 */

#define TEST 19

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
/* Rejected
 * do not bind the same function name twice */
#endif

#if TEST==2
union b { int a;};
struct b { int a;};
/* Rejected
 * union and struct share a namespace */
#endif

#if TEST==3
union c {};
int c(){}
/* Accepted
 * struct/unions and functions can share a same name */
#endif

#if TEST==4
int d;
int d(){}
/* Rejected */
#endif

#if TEST==5
int e() {}
int m() {
    return e
  ();
}
/* Accepted
 * Whitespace is ignored */
#endif

#if TEST==6
void f() {
    int a;
    float a;
}
/* Rejected */
#endif

#if TEST==7
int a;
void f () {
    char a;
}
/* Accepted*/
#endif

#if TEST==8
void f () {
    int a;
    {
	int a;
    }
}
#endif

#if TEST==9
char a=0141;
/* Accepted */
#endif

#if TEST==10
char a=97,b=98;
int c() {
    return a>b;
}
#endif

#if TEST==11
struct a {};
int a() {}
#endif

#if TEST==12
struct a {};
int b() {
    struct a a;
    int** b;
    a=b;
}
#endif

#if TEST==13
int b() {
    0=0;
}
#endif

#if TEST==14
int a;
int b() {
    sizeof(void);
}
/* Accepted */
#endif

#if TEST==15
int b() {
    return *(0+0);
}
/* Rejected */
#endif

#if TEST==16
struct a{int b;};
int a(){
    struct a b;
    return b.a;
}
/* Rejected */
#endif

#if TEST==17
struct a{};
int a(){
    struct a b;
    return !b;
}
#endif

#if TEST==18
int a(){
    return &1;
}
#endif

#if TEST==19
int a(){
    return ++1;
}
#endif

#if TEST==20
struct a{};
int a(){
    struct a a;
    return ++a;
}
#endif

int main()
{
    #if TEST==14
      printf("%d %d\n",sizeof(void),sizeof(int));
    #endif
    return 0;
}