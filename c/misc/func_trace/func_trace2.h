#ifdef DEBUG
#define IF_DEBUG(...) __VA_ARGS__
#else
#define IF_DEBUG(...)
#endif

int test_func( int arg
		IF_DEBUG(, const char *file, const char *func, unsigned int line) );

#ifndef FUNC_TRACE2_IMPL
#define test_func( arg )  test_func( arg IF_DEBUG(, __FILE__, __func__, __LINE__) )
#endif

