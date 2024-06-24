g++ -shared -Wall \
    -L./rondb-build/lib \
    ndbapi_wrap.o \
    -lndbclient \
    -ondbapi_wrap.so
