g++ -std=c++17 -fPIC \
    -I./rondb/storage/ndb/include/ \
    -I./rondb/prod_build/storage/ndb/include/ \
    -I./rondb/storage/ndb/include/ndbapi/ \
    -I./rondb/storage/ndb/src/ndbapi/ \
    -I./rondb/include \
    -I./rondb/prod_build/include \
    -I./rondb/storage/ndb/include/kernel \
    -I./rondb/storage/ndb/include/util \
    -I./rondb/storage/ndb/include/transporter \
    -I./rondb/storage/ndb/include/portlib \
    -I./rondb/ \
    -I./rondb/storage/ndb/include/mgmapi \
    -I./rondb/storage/ndb/include/debugger \
    -o ndbapi_wrap.o \
    -c src/ndbapi_wrap.cxx
