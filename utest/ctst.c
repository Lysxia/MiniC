/*  Testing C specs
 *  call gcc -c 'this_file.c'
 */

#include <stdio.h>
#include <stdlib.h>

#define TEST 45

#define MIN_INT 0x80000000
#define MAX_INT 0x7fffffff

void print(int m)
{
  printf("%d\n",m);
}

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

#if TEST==21
struct a{};
int a(){
    struct a a;
    if (a);
}
#endif

#if TEST==25
struct a{};
struct a a(){
    int a;
    a();
    return a;
}
// Accepted
#endif

#if TEST==26
struct a{ struct a a;};
#endif

#if TEST==28
int a;
char a;
#endif

#if TEST==29
int m()
{
    return;
}
#endif

#if TEST==31
int m(int a)
{
    {
    int a;
    }
}
#endif

int main()
{
    #if TEST==14
      printf("%d %d\n",sizeof(void),sizeof(int));
    #endif
    #if TEST==22
      struct a a;
    #endif
    #if TEST==23
      int a;
      int a;
    #endif
    #if TEST==24
      void a;
    #endif
    #if TEST==27
      //printf("
	//      ");
    #endif
    #if TEST==29
      printf("a%da\n",m());
    #endif
#if TEST==30
      printf("%d",(int) (1/0));
#endif
#if TEST==32
      char a=255;
      int c= 256;
      print(c+a);
#endif
#if TEST==33
      print((0x80000000)/(-1));
      print(0x80000000);
#endif
#if TEST==34
      print(-MIN_INT);
      print(MAX_INT);
      print(MIN_INT-MAX_INT);
      print(MIN_INT+(-MAX_INT));
      print(MAX_INT-MIN_INT);
      print(MAX_INT+(-MIN_INT));
      print(-MAX_INT-1);//segfault
      print(0x100000000);
#endif
#if TEST==35
      print((MAX_INT+1)*0);//segfault
#endif
#if TEST==36
      int i;
      int j;
      i = MAX_INT;
      j = 1;
      print(i+j);
#endif
#if TEST==37
      5/0;
#endif
#if TEST==38
      print(0x00000000);
#endif
#if TEST==39
      int i=0;
      int j=0;
      if ((i=1) || (j=1))
        print(i);
      print(j);
#endif
#if TEST==40
      int i=1;
      int j=1;
      if ((i=0)*(j=0))
        ;
      print(i);
      print(j);
#endif
#if TEST==41
      int i=MAX_INT;
      if (i+1<10)
        print(i);
      print(i);
#endif
#if TEST==42
      int i=2;
      if ((i=0)||1)
        print(1);
      print(i);
#endif
#if TEST==43
      printf("%c\n","a"[0]);
#endif
#if TEST==44
      int c[2]={1,2};
      int k=0;
      c[k]=c[++k];
      print(c[0]);
#endif
#if TEST==45
      print(((char) 128)*((char) 128));
      print(((char) 128)+(char) 129);
      print((char) 256);
#endif
    return 0;
}
