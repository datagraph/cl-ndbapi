g++ -std=c++17 -fPIC -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/ -I/mnt/ssd/home/rondb/code/rondb/prod_build/storage/ndb/include/ -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/ndbapi/ -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/src/ndbapi/ -I/mnt/ssd/home/rondb/code/rondb/include -I/mnt/ssd/home/rondb/code/rondb/prod_build/include -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/kernel -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/util -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/transporter  -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/portlib -I/mnt/ssd/home/rondb/code/rondb/ -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/mgmapi -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/debugger -o ndbapi_wrap.o -c src/ndbapi_wrap.cxx
