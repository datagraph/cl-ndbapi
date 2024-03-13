g++ -shared -Wall \
    -L./rondb/prod_build/lib \
    ndbapi_wrap.o \
    -lndbclient \
    -ondbapi_wrap.so
