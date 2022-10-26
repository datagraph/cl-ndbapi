# Installation

Get the repository:

    git clone https://github.com/datagraph/cl-ndbapi

Configuring ASDF to find the systems specified in `ndbapi.asd` and
`ndbapi-examples.asd`. I still do that by adding symlinks to them
from my systems directory, that is, what is apparently nowadays
called the [old style](https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems-_002d_002d_002d-old-style.html):

    cd systems
    ln -s ../cl-ndbapi/ndbapi.asd
    ln -s ../cl-ndbapi/ndbapi-examples.asd
    cd ..

You might want to do it [differently](/asdf/Configuring-ASDF-to-find-your-systems.html).

Add a symlink to the `libndbclient.so` library that you want to use:

    cd cl-ndbapi
    ln -s /mnt/ssd/home/rondb/local/rondb/lib/libndbclient.so.6.1.0

Alternatively, you can also create a link named `libndbclient.so`.

    ln -s /mnt/ssd/home/rondb/local/rondb/lib/libndbclient.so.6.1.0 libndbclient.so

Create and link the C wrapping library `ndbapi_wrap.so`:

    make

You might need to update the two scripts `compile-ndbapi-wrap-cxx.sh` and
`link-ndbapi-wrap-cxx.sh` which are referenced by the `Makefile` and that
do the actual work, so that `g++` finds the include files and libraries
of your [RonDB](https://www.rondb.com/) installation.

Start your Lisp and try out the example:

    sbcl
    * (require :ndbapi-examples)
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

Enjoy.


## License

Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
Distributed under the terms of the GNU General Public License, Version 2.0,
see file LICENSE in the top level directory of this repository.
