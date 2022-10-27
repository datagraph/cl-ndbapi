# Installation

Get the repository:

    git clone https://github.com/datagraph/cl-ndbapi

Configure [ASDF](https://asdf.common-lisp.dev) to find the systems
specified in [`ndbapi.asd`](ndbapi.asd) and
[`ndbapi-examples.asd`](ndbapi-examples.asd). I still do that by
adding symlinks to them from my systems directory, that is, what is
apparently nowadays called
the
[old style](https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems-_002d_002d_002d-old-style.html):

    cd systems
    ln -s ../cl-ndbapi/ndbapi.asd
    ln -s ../cl-ndbapi/ndbapi-examples.asd
    cd ..

You might want to do it [differently](https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems.html).

Add a symlink to the `libndbclient.so` library that you want to use:

    cd cl-ndbapi
    ln -s /mnt/ssd/home/rondb/local/rondb/lib/libndbclient.so.6.1.0

Alternatively, you can also create a link named `libndbclient.so`.

    ln -s /mnt/ssd/home/rondb/local/rondb/lib/libndbclient.so.6.1.0 libndbclient.so

Create and link the C wrapping library `ndbapi_wrap.so`:

    make

You might need to update the two
scripts [`compile-ndbapi-wrap-cxx.sh`](compile-ndbapi-wrap-cxx.sh)
and [`link-ndbapi-wrap-cxx.sh`](link-ndbapi-wrap-cxx.sh) which are
referenced by the [`Makefile`](Makefile) and that do the actual work,
so that `g++` finds the include files and libraries of
your [RonDB](https://www.rondb.com/) installation.

**Note**: You might need to add
[this patch](cffi-patch/0001-explicitly-reset-all-foreign-boolean-type-slots.patch)
to your CFFI so that the boolean flags `:low-inclusive` and `:high-inclusive` work
properly. You can also load the lisp version of that patch in file
[0001-explicitly-reset-all-foreign-boolean-type-slots.lisp](cffi-patch/0001-explicitly-reset-all-foreign-boolean-type-slots.lisp).

That's all.


## Testing the library

As a first test, try out the
[ndbapi-simple-scan example](examples/ndbapi-simple-scan.lisp).

To do so first, create a database and table and load
[some example data](examples/data.tsv):

    mysql --socket=/path/to/mysqld.sock --local-infile
    mysql> create database mgr;
    Query OK, 1 row affected (0.13 sec)
    mysql> use mgr;
    Database changed
    mysql>    create table test
        ->           (s int unsigned not null, p int unsigned not null,
        ->            o int unsigned not null, g int unsigned not null,
        ->            index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
        ->            index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g));
    Query OK, 0 rows affected (0.34 sec)
    mysql> load data infile '/path/to/data.tsv' into table test;
    Query OK, 10 rows affected (0.01 sec)
    Records: 10  Deleted: 0  Skipped: 0  Warnings: 0

    mysql> select * from test;
    +------+------+---------+----------+
    | s    | p    | o       | g        |
    +------+------+---------+----------+
    | 1105 | 1105 | 2108201 |  2108202 |
    | 1105 | 1106 |    1108 |     1109 |
    | 1105 | 1106 |    1137 |     1119 |
    | 1105 | 1106 | 2108181 |   662743 |
    | 1105 | 1105 | 1803125 | 51483456 |
    | 1105 | 1106 | 2108179 | 51483457 |
    | 1105 | 1106 |  603481 |  2108178 |
    | 1105 | 1105 |     638 |     1106 |
    | 1105 | 1106 |       1 |     1107 |
    | 1105 | 1106 |  106314 |   662743 |
    +------+------+---------+----------+
    10 rows in set (0.00 sec)

Now start your Lisp and run the [ndbapi-simple-scan example](examples/ndbapi-simple-scan.lisp):

    sbcl
    * (require :ndbapi-examples)
    * ;; load the CFFI patch if it is not included in your CFFI already:
    * ;; (load "cffi-patch/0001-explicitly-reset-all-foreign-boolean-type-slots.lisp")
    * (ndb.simple-scan:simple-scan :connection-string "nl3:1186,nl3:1187"
                                   :database-name "mgr"
                                   :table-name "test"
                                   :index-name "gspo"
                                   :low (list 1106 1105 1105 638) :low-inclusive t
                                   :high (list 1109 1105 1106 1108) :high-inclusive t)
    table: test
    columns:        SUBJECT,    PREDICATE,       OBJECT,        GRAPH
    row     0:         1105,         1105,          638,         1106
    row     1:         1105,         1106,            1,         1107
    row     2:         1105,         1106,         1108,         1109
    NIL
    * (quit)

Observe that the call correctly retrieved the columns observing
the given `low` and `high` bounds, which are based on the order
`GSPO`, that is, `Graph`, `Subject`, `Predicate`, `Object`,
and that the result is also ordered accordingly.

For comparison, you can also compile, link and run the
[C++ version of the example](examples/ndbapi_simple_scan_in_cpp/ndbapi_simple_scan.cpp).
All done by just executing the
[respective Makefile](examples/ndbapi_simple_scan_in_cpp/Makefile):

    cd examples/ndbapi_simple_scan_in_cpp
    make

which should output:

    ./compile-cxx.sh
    ./link-cxx.sh
    ./run-cxx.sh
    Start execute
    Done executed
    Start printing
    1105    1105    638     1106
    1105    1106    1       1107
    1105    1106    1108    1109
    Done printing

Enjoy.


## License

Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.  
Distributed under the terms of the GNU General Public License, Version 2.0,  
see file [LICENSE](LICENSE) in the top level directory of this repository.
