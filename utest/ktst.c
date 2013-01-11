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

int main()
{
  // For loops
  {
    int i;
    for (i=0; i<10 ; i++)
      putchar(i+97);
  }
}
