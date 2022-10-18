swig3.0  -v -cpperraswarn -cffi -noswig-lisp -includeall -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/ -I/mnt/ssd/home/rondb/code/rondb/prod_build/storage/ndb/include/ -I/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/ndbapi/ -c++ -module ndbapi ndbapi.i 

# fix clos file
temp="$(tempfile)"
clos="ndbapi-clos.lisp"
cat $clos > $temp
echo "(cl:in-package :libndbapi)" > $clos
cat $temp >> $clos
