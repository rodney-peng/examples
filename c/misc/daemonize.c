/*

 Wrapper to work around daemon() and pthread_create() issue with uClibc and gcc
 in order to daemonize.

 Check http://lists.uclibc.org/pipermail/uclibc/2010-January/043463.html

 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#define MAX_ARGS	31

int main( int argc, char ** argv )
{
	char * args[MAX_ARGS+1];
	int argn;

	if (argc < 2) return -1;

	daemon(1, 0);
	sleep(3);

	argn = argc - 1;
	if (argn > MAX_ARGS) argn = MAX_ARGS;

	if (argn) memcpy( &(args[0]), &(argv[1]), (sizeof (args[0]))*argn );
	args[argn] = NULL;

	execv( args[0], &(args[0]) );

	return 0;
}

