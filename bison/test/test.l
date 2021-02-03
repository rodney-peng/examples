%{
/* start here */
#include "test.tab.h"
%}

space [ \t\n]+
func  (hide|set)
ident [_a-zA-Z]+[_a-zA-Z0-9]*
num   [0-9]+

%%

{space} ;
{func}  { printf( "%s: func %s\n", __func__, yytext ); yylval.func = strdup( yytext ); return FUNC; }
{ident} { printf( "%s: ident %s\n", __func__, yytext ); yylval.ident = strdup( yytext ); return IDENT; }
{num}   { printf( "%s: num %s\n", __func__, yytext ); yylval.num = atoi( yytext ); return NUM; }

. { return yytext[0]; };

%%

int yywrap()
{
  return 1;
}

#if 0
int main( void )
{
  YY_BUFFER_STATE buf = yy_scan_string( "set   (   PacketTime   ,   Template   ,   12   )   " );

  yylex();
  yy_delete_buffer( buf );

  return 0;
}
#endif
