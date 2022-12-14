# ndbapi: Common Lisp bindings to the C++ NDB API of RonDB

This library allows to use the
[NDB&nbsp;API of RonDB](https://docs.rondb.com/rondb_ndb_api/)
in Common Lisp programs and thus write client applications that
talk to [RonDB](https://www.rondb.com/).
RonDb is a key-value store providing low latency, high throughput,
and high availability (actually,
[Class 6 Availability or "six nines"](https://www.logicalclocks.com/blog/rondb-the-worlds-fastest-key-value-store-is-now-in-the-cloud)).

Parallel to the key-value stores capabilities RonDB also provides SQL
capabilities. As RonDB is an open source distribution of NDB
Cluster, which is the distributed database system underlying
[MySQL Cluster](https://www.mysql.com/products/cluster),
these SQL capabilities are actually provided by
[running MySQL Servers on top of RonDB](https://docs.rondb.com/rondb_arch/).

As a consequence, you can use SQL to setup new databases, tables,
indexes and so forth, and also to load or export data, while you can
use the NDB&nbsp;API to efficiently retrieve data, for example, by
making use of specialized indexes. Look at the [instruction on how
to test this library](INSTALL.md#testing-the-library) for an example
for this.

> **NOTE:** This is an early release and the library still evolving
> as of 2022-10-25. MGR

## Description of the library and the interfaces it provides

The library bases on a FFI binding code that was generated using
SWIG&nbsp;3.0.12. It consists of a complete low-level and a nicer
high-level interface.

The high-level interface makes just those parts of the NDB&nbsp;API available
that I need to implement a new database backend based on RonDB for our
graph store but in a much nicer fashion, with scope-based macros such
as `ndbapi:with-ndb`, `ndbapi:with-ndb-transaction` et cetera, but you
can also explicitly free the objects with `ndbapi:ndb-free-object` or
just let the the GC take care of them, as the constructed C++ objects
are hooked up into [SBCL](http://www.sbcl.org/)'s garbage collector with
thin Lisp wrapping classes so that they get properly freed by the respective
destructors if they are not referenced anywhere anymore.

The low-level interface makes the full NDB&nbsp;API available. But is is
hard to use as it includes no class hierarchy, and thus exhibits long
names, and implements no overloading. That is, the overloaded method
`myNdb.startTransaction()`, for example, is available as the ten
separate Lisp functions from `_wrap_Ndb_startTransaction__SWIG_0` to
`_wrap_Ndb_startTransaction__SWIG_9` with varying arguments.

Care has been taken that NDB is not deinitialized by NDB_END if there
is still a cluster connection, or a cluster connection is not freed as
long there are NDB objects that were created using it. Also double
freeing is avoided, so you can combine the explicit and automatic
approaches and still be sure that eventually the GC will free the
resources when you have missed one.

The exported interface is defined as the package `:ndbapi`
in file [`src/interface.lisp`](src/interface.lisp). The implementation
is in the subdirectory [`src/`](src/), the examples in [`example/`](examples/),
and the [ASDF](https://asdf.common-lisp.dev) system definitions in the
top level directory in the files [`ndbapi.asd`](ndbapi.asd) and
[`ndbapi-examples.asd`](ndbapi-examples.asd).

An example application is available in file
[`examples/ndbapi-simple-scan.lisp`](examples/ndbapi-simple-scan.lisp).
For comparison a C++ version of the same example is implemented in file
[`examples/ndbapi_simple_scan_in_cpp/ndbapi_simple_scan.cpp`](examples/ndbapi_simple_scan_in_cpp/ndbapi_simple_scan.cpp).
It is based on other examples of ndb cluster but does not use a MySQL node.
The example uses a secondary index as a scan index to iterate over columns
in an example database observing bounds. See both files for details.

A second example application is in file
[`examples/ndbapi-scan-count.lisp`](examples/ndbapi-scan-count.lisp).
It gives an estimated count of the rows matching a bound of lower
and/or upper limits. This example uses a number of more advanced features
compared to the first example to make the count efficient, for example,
[interpreted programs](https://docs.rondb.com/rondb_ndb_api/#interpreter-commands-as-part-of-ndb-api-operations) to limit the row access to just one per partition (or fragment),
[accessing special values (or pseudo columns)](https://docs.rondb.com/rondb_ndb_api/#get-special-values), and [masking of columns](https://docs.rondb.com/rondb_ndb_api/#operation-options).
Please read the long comment at the beginning of the file
[`examples/ndbapi-scan-count.lisp`](examples/ndbapi-scan-count.lisp)
for a more detailed explanation.

## Installation

The installation instructions are in the separate file [INSTALL.md](INSTALL.md).


## Details on the generation of the FFI bindings

This library bases on a FFI binding code that was generated
using SWIG. As CFFI support was disabled in SWIG&nbsp;4.0.0 and finally
[completely removed in SWIG&nbsp;4.1.0](https://github.com/swig/swig/commit/cea25abca535fa27b89eedaf2dd978991b42e1a5),
SWIG&nbsp;3.0.12 was used, which is part of Ubuntu 20.04.2&nbsp;LTS.

NDB&nbsp;API is a rather substantial C++ library and uses many things not
possible in C while the CFFI module of SWIG has quite a few
limitations and cannot handle some C++ constructs. Still, SWIG
successfully generates a complete C wrapping library (using `extern "C"`)
named [`src/ndbapi_wrap.cxx`](src/ndbapi_wrap.cxx) and a lisp library
making all C definitions available to Lisp via CFFI constructs in
[`src/ndbapi.lisp`](src/ndbapi.lisp). Also a higher level CLOS interface
gets generated as [`src/ndbapi-clos.lisp`](src/ndbapi-clos.lisp), which
contains generated classes and generic methods.

The C wrapping library exhibits a rather low-level interface as all
hierarchical class names are rolled out with prefixes and all
overloaded methods are exported as separate functions. That is, the
overloaded method `myNdb.startTransaction()`, for examples, becomes
available as the ten separate Lisp functions from
`_wrap_Ndb_startTransaction__SWIG_0` to
`_wrap_Ndb_startTransaction__SWIG_9` with varying arguments.  Most of the time
the methods only vary in their arity, which would allow for a
simple dispatch by arity. But there are also some methods that have
the same number of arguments but of different types, as, for example,
`NdbDictionary::getValuePtr()`, the constructor of the `Column` class,
or `Table::getColumn()`.

SWIG&nbsp;3.0.1 just created CFFI bindings with the same lisp name for a
family of overloaded methods, which conflicted, of course, while the
CLOS interface just tries to call these function, ignoring the fact
that there is overloading supposed to be involved.

In the end, after exploring other versions of SWIG (see section
"Exploration of other SWIG versions ..." for details), the CFFI
bindings generated by SWIG&nbsp;3.0.1 were used, completely ignoring the
CLOS interface in `ndbapi-clos.lisp`. This simple CFFI interface could
be generated for the full NDB&nbsp;API interface and seemed complete.
Afterwards, many translation problems or errors were manually fixed.

I have change the conflicting Lisp names in an automated fashion to
also contain the `SWIG-*` postfix, to have, for example, the C function
`_wrap_new_Ndb__SWIG_1` available as `new_Ndb/SWIG-1` in Lisp. Simple
dispatch arity is implemented in file [`src/overloading.lisp`](src/overloading.lisp)
by the macro `ndbapi.ffi.overloading::overload-function-by-arity` and
used for some methods.

I just focused on having the parts available that I need to implement
a new database backend based on RonDB for our graph store rather then
exporting everything. And to have this core available in a safe
fashion so that the C/C++ NDB objects are properly constructed and
always destructed.


## Exploration of other SWIG versions for a more complete generated interface

An attempt was made to use the enhanced variant
['swig-cpp-to-cffi-for-common-lisp-enhancements`](https://github.com/glycerine/swig-cpp-to-cffi-for-common-lisp-enhancements)
of SWIG which was authored by Jason E. Aten in 2011, who spent quite
some time to make the CFFI module for SWIG work for "a large codebase
of C and C+". Sadly, this version choked on some parts of the NDB&nbsp;API,
as, for example, structs with constructors, and it just exited with
a SEGFAULT. It was possible generate a version for a smaller subset of
the NDB&nbsp;API, after taking out problematic parts of it that were determined
by having a version of SWIG with debug logging activated and running it
from within GDB.

This version of SWIG actually contained something that Aten describes as
"a synthesized dispatcher that allows natural calling of overloaded
constructors by keyword specification is provided". Meaning that for
each overloaded method first a function is generated which implemented
dispatch by arity and then for each variant generic methods are generated
that dispatch on the types. But the methods that actually would use it, as
the above mentioned `NdbDictionary::getValuePtr()`, seem to be just missing
from the CLOS interface.

In the end, this attempt was aborted, as the generated interface seemed
to be incomplete even when only parts of the NDB&nbsp;API were translated.

It is noted that Nikolai Matiushev attempted to ("Revive CFFI Common Lisp support")
in 2021](https://github.com/swig/swig/pull/1966) but this seems to
["lack an C++ support"](https://github.com/swig/swig/pull/2200#issuecomment-1114257038).
Still, if that work is continued it might lead to a nicer automatically
generated high-level interface.


## License

Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License, version 2.0,
as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License, version 2.0, for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
02110-1301 USA

A copy of the license is also reproduced in the file [LICENSE](LICENSE)
in the top level directory of this repository.
