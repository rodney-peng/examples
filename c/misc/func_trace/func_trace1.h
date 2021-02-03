#ifndef DEBUG
int test_func( int arg );
#else
int test_func_debug( int arg,
		const char *file, const char *func, unsigned int line );

#define test_func( arg )  test_func_debug( arg, __FILE__, __func__, __LINE__ )
#endif

