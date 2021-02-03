/*
	gcc -o ft2 func_trace2.c main2.c
	gcc -DDEBUG -o ft2d func_trace2.c main2.c

	One restriction is that "test_func( arg );" is unavailable in this file if DEBUG is defined.
	Use "test_func( arg IF_DEBUG(, __FILE__, __func__, __LINE__) );" instead.
*/

#include <stdio.h>

#define FUNC_TRACE2_IMPL
#include "func_trace2.h"

#ifdef DEBUG
void debug_trace( const char * func, const char *file, const char *caller, unsigned int line )
{
	printf( "%s() failed when called by %s() from %s:%u!\n", func, caller, file, line );
}
#endif

int test_func( int arg
		IF_DEBUG(, const char *file, const char *func, unsigned int line) )
{
	int rv = arg;

	printf( "rv=%d\n", rv );

	IF_DEBUG( if (rv) debug_trace( __func__, file, func, line ); );

	return rv;
}

