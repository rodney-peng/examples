/*
	gcc -o ft1 func_trace1.c
	gcc -DDEBUG -o ft1d func_trace1.c
*/

#include <stdio.h>
#include "func_trace1.h"

#ifdef DEBUG
void debug_trace( const char * func, const char *file, const char *caller, unsigned int line )
{
	printf( "%s() failed when called by %s() from %s:%u!\n", func, caller, file, line );
}
#endif

#ifndef DEBUG
int test_func( int arg )
#else
int test_func_debug( int arg,
		const char *file, const char *func, unsigned int line )
#endif
{
	int rv = arg;

	printf( "rv=%d\n", rv );

#ifdef DEBUG
	if (rv) debug_trace( "test_func", file, func, line );
#endif

	return rv;
}

int main( void )
{
	test_func( 0 );
	test_func( 1 );

	return 0;
}

