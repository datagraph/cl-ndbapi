// Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
// Distributed under the terms of the GNU General Public License, Version 2.0,
// see file LICENSE in the top level directory of this repository.

%module ndbapi

%insert("header") %{
#include <NdbApi.hpp>
#include <API.hpp>
%}

%insert("lisphead") %{
(cl:in-package :libndbapi)
%}
#%insert("closhead") %{
#(cl:in-package :libndbapi)
#%}

%feature("intern_function", "swig-lispify");
%feature("flatnested", "1");
#%rename(__insertionoperator__) NdbOut::operator<<;
#%rename(__lshift__)          *::operator<<;
%ignore operator<<;                      // Ignore = everywhere.
%ignore spjTest;

%include "/mnt/ssd/home/rondb/code/rondb/storage/ndb/include/ndbapi/NdbApi.hpp"
