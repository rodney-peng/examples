%{
/* start here */
#include <stdio.h>
void yyerror(const char *s);

%}

%union {
  char * func;
  char * ident;
  int num;
}

%token <func> FUNC
%token <ident> IDENT
%token <num> NUM

%%

call: func '(' args ')';

func :	FUNC { printf( "%s: func %s\n", __func__, $1 ); };

args: | argseq;

argseq: arg | argseq ',' arg;

arg:	IDENT { printf( "%s: ident %s\n", __func__, $1 ); }
	| NUM { printf( "%s: num %d\n", __func__, $1 ); };

%%

typedef struct yy_buffer_state *YY_BUFFER_STATE;
YY_BUFFER_STATE yy_scan_string (const char * yystr );
void yy_delete_buffer (YY_BUFFER_STATE b  );

static int parse( const char * s )
{
  YY_BUFFER_STATE buf = yy_scan_string( s );

  printf( "Parse: %s\n", s );
  int result = yyparse();
  printf( "result %d\n\n", result );

  yy_delete_buffer( buf );
  return result;
}

int main( void )
{
  parse( "hide(PacketTime)" );
  parse( "set(PacketTime,Template,12)" );

  return 0;
}

void yyerror(const char *s) {
  printf( "yyerror: %s!\n", s );
}

