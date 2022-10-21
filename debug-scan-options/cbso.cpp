// Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
// Distributed under the terms of the GNU General Public License, Version 2.0,
// see file LICENSE in the top level directory of this repository.

#include "cbso.h"

void show_scan_options(NdbScanOperation::ScanOptions *optionsptr) {
  NdbScanOperation::ScanOptions options = *optionsptr;
  printf("size scanoptions: %zu\n", sizeof(NdbScanOperation::ScanOptions));
  printf("scanoptions.size: %u\n", options.size());
  printf("scanoptions.size: %p\n", (void*)(options.size));
  printf("scanoptions.optionsPresent: %llu\n", options.optionsPresent);
  printf("scanoptions.scan_flags: %u\n", options.scan_flags);
  printf("scanoptions.parallel: %u\n", options.parallel);
  printf("scanoptions.batch: %u\n", options.batch);
  printf("scanoptions.extraGetValues: %p\n", (void*)(options.extraGetValues));
  printf("scanoptions.numExtraGetValues: %u\n", options.numExtraGetValues);
  printf("scanoptions.partitionId: %u\n", options.partitionId);
  printf("scanoptions.interpretedCode: %p\n", (void*)(options.interpretedCode));
  printf("scanoptions.customData: %p\n", (void*)(options.customData));
  printf("scanoptions.partitionInfo: %p\n", (void*)(options.partitionInfo));
  printf("scanoptions.sizeOfPartInfo: %u\n", options.sizeOfPartInfo);

  printf("\n");
  //printf("scanoptions.size offset: %zu\n", offsetof(NdbScanOperation::ScanOptions, size));
  //printf("scanoptions.size sizeof: %zu\n", sizeof(options.size));
  printf("scanoptions.optionsPresent offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, optionsPresent));
  printf("scanoptions.optionsPresent sizeof: %zu\n", sizeof(options.optionsPresent));
  printf("scanoptions.scan_flags offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, scan_flags));
  printf("scanoptions.scan_flags sizeof: %zu\n", sizeof(options.scan_flags));
  printf("scanoptions.parallel offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, parallel));
  printf("scanoptions.parallel sizeof: %zu\n", sizeof(options.parallel));
  printf("scanoptions.batch offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, batch));
  printf("scanoptions.batch sizeof: %zu\n", sizeof(options.batch));
  printf("scanoptions.extraGetValues offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, extraGetValues));
  printf("scanoptions.extraGetValues sizeof: %zu\n", sizeof(options.extraGetValues));
  printf("scanoptions.numExtraGetValues offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, numExtraGetValues));
  printf("scanoptions.numExtraGetValues sizeof: %zu\n", sizeof(options.numExtraGetValues));
  printf("scanoptions.partitionId offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, partitionId));
  printf("scanoptions.partitionId sizeof: %zu\n", sizeof(options.partitionId));
  printf("scanoptions.interpretedCode offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, interpretedCode));
  printf("scanoptions.interpretedCode sizeof: %zu\n", sizeof(options.interpretedCode));
  printf("scanoptions.customData offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, customData));
  printf("scanoptions.customData sizeof: %zu\n", sizeof(options.customData));
  printf("scanoptions.partitionInfo offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, partitionInfo));
  printf("scanoptions.partitionInfo sizeof: %zu\n", sizeof(options.partitionInfo));
  printf("scanoptions.sizeOfPartInfo offset: %zu\n", offsetof(struct NdbScanOperation::ScanOptions, sizeOfPartInfo));
  printf("scanoptions.sizeOfPartInfo sizeof: %zu\n", sizeof(options.sizeOfPartInfo));
}
