
.PHONY: all

all:
	rpcgen test.x
	gcc -o svc test_svc.c svc_proc.c test_xdr.c
	gcc -o clnt clnt.c test_clnt.c test_xdr.c

clean:
	rm -f test_* test.h clnt svc
