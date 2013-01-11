/* Check every part of mips.ml */

struct s1 {
  int a;
  int b;
  int c;
};

struct c1 {
  char p;
  char q;
  char r;
  char s;
  char t;
};

struct c2 {
  char c21;
  char c22;
};

struct u {
  struct c2 w;
  int x;
};

int somefun(char a,char b,int c)
{
}

int caller()
{
  somefun(1,2,3);
}

int chararray()
{
  int i;
  char *s, c;
  c = s[i];
}

int conditions()
{
  //Testing conditions
  {
    int a,b;
    a&&b;
    a||b;
  }

}

int loadstore()
{
  //load function
  //store
  //Case a=false, s<=4
  {
    struct c2 a,b;
    int c;
    struct c2 e,f;
    a=b;
    e=f;
  }
  //Case a=false, s>4
  {
    struct c1 s;
    struct c1 t;
    s=t;
  }
  //Case a=true, s>4
  {
    int a;
    struct s1 s;
    struct s1 t;
    s=t;
  }
  //Case a=true, s=4
  {
    int a;
    int b;
    a=b;
  }
  return 0;
}

struct s1 structfun()
{
  struct s1 x;
  x.a = 97;
  x.b = 98;
  x.c = 99;
  return x;
}

struct u struct2fun()
{
  struct u y;
  y.w.c21 = 108;
  y.w.c22 = 109;
  y.x = 36;
  return y;
}

int main()
{
  // For loops
  {
    int i;
    for (i=0; i<10 ; i++)
      putchar(i+97);
    putchar(10);
  }
  //struct
  {
    putchar(structfun().a);
    putchar(structfun().b);
    putchar(structfun().c);
    putchar(10);
  }
  {
    putchar(struct2fun().w.c21);
    putchar(struct2fun().w.c22);
  }
}
