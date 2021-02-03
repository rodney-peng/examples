/*
  2015/4/3 Rodney Peng

  Command line decomposition, support '\' for escape and quoted string.
  The output are left-alignment and overlapped in the input.

  TODO:
      1. use strspn() and strcspn() to simplify checking loop
      2. space characters as an argument for nextArg(), ' ' and '\t' as default if not provided
*/
#include <stdio.h>
#include <string.h>

static int nextQuotedStr( char * start, char * str, int * plen )
{
  char * src = start;
  char * dst = str;
  int  quote = *(src++);

  while (*src != '\0' && *src != quote)
  {
    if (*src == '\\') src++;
    *dst++ = *src++;
  }
  src++;
  *plen = dst - str;

  return src - start;
}

#define IS_SPACE(ch)  (ch == ' ' || ch == '\t')
#define IS_QUOTE(ch)  (ch == '\'' || ch == '"')
static int nextStr( char * start, char * str, int * plen )
{
  char * src = start;
  char * dst = str;

  while (*src != '\0' && (! IS_SPACE(*src)) && (! IS_QUOTE(*src)))
  {
    if (*src == '\\') src++;
    *dst++ = *src++;
  }
  *plen = dst - str;

  return src - start;
}

static int nextArg( char * start, char * arg, int * plen )
{
  char * src = start;
  char * dst = arg;
  int  len;

  while (IS_SPACE(*src)) src++;

  while (*src != '\0' && ! IS_SPACE(*src))
  {
    if (IS_QUOTE(*src))
      src += nextQuotedStr( src, dst, &len );
    else
      src += nextStr( src, dst, &len );

    dst += len;
  }
  if (*src != '\0') src++;
  *dst = '\0';
  *plen = (dst - arg) + 1;

  return src - start;
}
#undef IS_SPACE
#undef IS_QUOTE

static int decompose( char * cmd, char ** argv )
{
  int argc = 0;
  char * start = cmd;
  char * arg   = cmd;
  int consumed;
  int copied;

  while ((consumed = nextArg( start, arg, &copied )))
  {
    start += consumed;

    if (*arg == '\0') continue;

    argv[argc++] = arg;
    arg += copied;
  }

  return argc;
}


#ifdef SELFTEST
static void test( char * str )
{
  char * argv[32];
  int argc;
  int i;

  printf( "test (%s)\n", str );

  argc = decompose( str, argv );

  for (i = 0; i < argc; i++)
    printf( "[%i]=(%s)\n", i, argv[i] );
  printf( "\n" );
}

int main()
{
  char s0[] = "1 a 2 b\0";
  char s1[] = "c    '1   dd 33 'vtest\" \" 123 abc    '   d   w2  2ww'b \0";

  test( s0 );
  test( s1 );

  return 0;
}
#endif

