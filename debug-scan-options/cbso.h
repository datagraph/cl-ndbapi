// Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
// Distributed under the terms of the GNU General Public License, Version 2.0,
// see file LICENSE in the top level directory of this repository.

#include <cstddef>
#include <NdbApi.hpp>
#include <API.hpp>

#ifdef __cplusplus
extern "C" {
#endif
  void show_scan_options(NdbScanOperation::ScanOptions*);
#ifdef __cplusplus
}
#endif
