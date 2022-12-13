/*
   Copyright (c) 2005, 2014, Oracle and/or its affiliates. All rights reserved.
   Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License, version 2.0,
   as published by the Free Software Foundation.

   This program is also distributed with certain software (including
   but not limited to OpenSSL) that is licensed under separate terms,
   as designated in a particular file or component or in included license
   documentation.  The authors of MySQL hereby grant you an additional
   permission to link the program and your derivative works with the
   separately licensed software that they have included with MySQL.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License, version 2.0, for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301  USA
*/

/* 
 *  ndbapi_simple_scan.cpp: Using a secondary index as a scan index in NDB API
 *
 *  This example is based on ndbapi example ndbapi_simple.cpp but
 *  modified by Max-Gerd Retzlaff <mgr@matroid.org> to iterate over
 *  a bounded selection of rows in the table by nextResult() as
 *  demonstrated in the other ndbapi example ndbapi_recattr_vs_record/main.cpp
 *  in the api_record branch of function do_indexScan, also looking at the
 *  section "Compare with old API interface\n" of the ndbapi example
 *  ndbapi_multi_cursor/main.cpp.
 *
 *  Correct output from this program is:
 *
 *  Start execute
 *  Done executed
 *  Start printing
 *  1105    1105    638     1106
 *  1105    1106    1       1107
 *  1105    1106    1108    1109
 *  Done printing
 *
 */

#include <NdbApi.hpp>
#include <stdlib.h>
// Used for cout
#include <stdio.h>
#include <iostream>

static void run_application(Ndb_cluster_connection &);

#define PRINT_ERROR(code,msg) \
  std::cout << "Error in " << __FILE__ << ", line: " << __LINE__ \
            << ", code: " << code \
            << ", msg: " << msg << "." << std::endl
#define APIERROR(error) { \
  PRINT_ERROR(error.code,error.message); \
  exit(-1); }

int main(int argc, char** argv)
{
  if (argc != 3)
  {
    std::cout << "Arguments are <connect_string cluster>.\n";
    exit(-1);
  }
  // ndb_init must be called first
  ndb_init();

  // connect to cluster and run application
  {
    const char *connectstring = argv[2];
    // Object representing the cluster
    Ndb_cluster_connection cluster_connection(connectstring);

    // Connect to cluster management server (ndb_mgmd)
    if (cluster_connection.connect(4 /* retries               */,
				   5 /* delay between retries */,
				   1 /* verbose               */))
    {
      std::cout << "Cluster management server was not ready within expected time.\n";
      exit(-1);
    }

    // Optionally connect and wait for the storage nodes (ndbd's)
    if (cluster_connection.wait_until_ready(30,0) < 0)
    {
      std::cout << "Cluster was not ready within 30 secs.\n";
      exit(-1);
    }

    // run the application code
    run_application(cluster_connection);
  }

  ndb_end(0);

  return 0;
}

static void do_scan(Ndb &);

static void run_application(Ndb_cluster_connection &cluster_connection)
{
  /********************************************
   * Connect to database via NdbApi           *
   ********************************************/
  // Object representing the database
  Ndb myNdb( &cluster_connection, "mgr" );
  if (myNdb.init()) APIERROR(myNdb.getNdbError());

  /*
   * Do different operations on database
   */
  do_scan(myNdb);
}

/*
 * create table as:
 *   create table test
 *          (s int unsigned not null, p int unsigned not null,
 *           o int unsigned not null, g int unsigned not null,
 *           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
 *           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g));
 *
 * load data with:
 *    load data infile '/path/to/data.tsv' into table test;
*/

struct Quad
{
  Uint32 s;
  Uint32 p;
  Uint32 o;
  Uint32 g;
};
struct Triple
{
  Uint32 s;
  Uint32 p;
  Uint32 o;
};
struct Tuple
{
  Uint32 s;
  Uint32 p;
};
struct Single
{
  Uint32 s;
};

size_t sizeQuad = sizeof(Quad);
size_t sizeTriple = sizeof(Triple);
size_t sizeTuple = sizeof(Tuple);
size_t sizeSingle = sizeof(Single);

static void do_scan(Ndb &myNdb)
{
  // printf("size of Quad:   %zu, %zu\n", sizeQuad, sizeQuad/sizeSingle);
  // printf("size of Triple: %zu, %zu\n", sizeTriple, sizeTriple/sizeSingle);
  // printf("size of Tuple:  %zu, %zu\n", sizeTuple, sizeTuple/sizeSingle);
  // printf("size of Single: %zu, %zu\n", sizeSingle, sizeSingle/sizeSingle);
  // printf("size of Uint32: %zu\n", sizeof(Uint32));

  NdbTransaction *myTransaction= myNdb.startTransaction();
  if (myTransaction == NULL)
    APIERROR(myNdb.getNdbError());

  const NdbDictionary::Dictionary *myDict= myNdb.getDictionary();
  const NdbDictionary::Table *test = myDict->getTable("test");
  const NdbDictionary::Index *mySIndex= myDict->getIndex("gspo", test->getName());

  if (mySIndex == NULL)
    APIERROR(myDict->getNdbError());
    
  /*
  // simple scanIndex without options
  NdbIndexScanOperation *ixScan = 
    myTransaction->scanIndex(mySIndex->getDefaultRecord(),
                             test->getDefaultRecord());
  */

  // scanIndex with options to sort and have multiple ranges
  NdbScanOperation::ScanOptions options;
  options.optionsPresent=NdbScanOperation::ScanOptions::SO_SCANFLAGS;
  Uint32 scanFlags = NdbScanOperation::SF_OrderBy | NdbScanOperation::SF_MultiRange;
  options.scan_flags=scanFlags;
  NdbIndexScanOperation *ixScan = 
    myTransaction->scanIndex(mySIndex->getDefaultRecord(),
                             test->getDefaultRecord(),
                             NdbOperation::LM_Read,
                             NULL, // mask
                             NULL, // bound
                             &options,
                             sizeof(NdbScanOperation::ScanOptions));

  /*
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
  */
 
  if (ixScan == NULL)
    APIERROR(myTransaction->getNdbError());

  // Tuple low={662743, 2219900};
  // Tuple high={662743, 3000000};
  Tuple low={662743, 2000000};
  Tuple high={662743, 2200000};
  // Tuple low={1, (Uint32)NULL};
  // Single high={662743};
  NdbIndexScanOperation::IndexBound bound;
  bound.low_key= (char*)&low;
  bound.low_key_count= sizeof(low)/sizeSingle;
  bound.low_inclusive= true;
  bound.high_key= (char*)&high;
  bound.high_key_count= sizeof(high)/sizeSingle;
  bound.high_inclusive= true;
  bound.range_no= 0;

  /*
  printf("bound.low_inclusive offset: %zu\n", offsetof(struct NdbIndexScanOperation::IndexBound, low_inclusive));
  printf("bound.low_inclusive sizeof: %zu\n", sizeof(bound.low_inclusive));
  printf("bound.high_key offset: %zu\n", offsetof(struct NdbIndexScanOperation::IndexBound, high_key));
  */
    
  /*
  Single val={662743};
  NdbIndexScanOperation::IndexBound bound;
  bound.low_key= (char*)&val;
  bound.low_key_count= sizeof(val)/sizeSingle;;
  bound.low_inclusive= true;
  bound.high_key= (char*)&val;
  bound.high_key_count= sizeof(val)/sizeSingle;;;
  bound.high_inclusive= true;
  bound.range_no= 0;
  */
  
  Quad low2 = {1106,1105,1105,638};
  Quad high2 = {1109,1105,1106,1108};
  /*
  Uint32 low2 = 1109;
  Uint32 high2 = 1119;
  //  Uint32 high = (Uint32)NULL;
  printf("low: %u, high %u\n", low.s, high.s);
  printf("low: %u, high %u\n", low.s, *((Uint32*)&high));
  printf("size: %zu, %u, %zu, %u\n", sizeof(low), low.s, sizeof(low2), low2);
  */
  NdbIndexScanOperation::IndexBound bound2;
  bound2.low_key= (char*)&low2;
  bound2.low_key_count= sizeof(low2)/sizeSingle;
  bound2.low_inclusive= true;
  bound2.high_key= (char*)&high2;
  bound2.high_key_count= sizeof(high2)/sizeSingle;
  bound2.high_inclusive= true;
  bound2.range_no= 1;

  // if (ixScan->setBound(mySIndex->getDefaultRecord(), bound))
  //   APIERROR(myTransaction->getNdbError());
  if (ixScan->setBound(mySIndex->getDefaultRecord(), bound2))
    APIERROR(myTransaction->getNdbError());

  printf("Start execute\n");
  if(myTransaction->execute( NdbTransaction::NoCommit ) != 0)
    APIERROR(myTransaction->getNdbError());

  // Check rc anyway as there might be some operation not successful
  if (myTransaction->getNdbError().status != NdbError::Success)
    APIERROR(myTransaction->getNdbError());

  printf("Done executed\n");

  printf("Start printing\n");
  Quad *prowData; // Ptr to point to our data

  int rc=0;

  while ((rc = ixScan->nextResult((const char**) &prowData,
                                  true,
                                  false)) == 0)
    {
      // printf(" PTR : %d\n", (int) prowData);
      // printf(" %u    %u    %u    %u    Range no : %2d\n", 
      //        prowData->s,
      //        prowData->p,
      //        prowData->o,
      //        prowData->g,
      //        ixScan->get_range_no());
      printf("%u\t%u\t%u\t%u\n", prowData->s, prowData->p, prowData->o, prowData->g);

      // access with pointer arithmetic of Uint32 struct members:
      //Uint32 *s = &(prowData->s);
      //printf("%u\t%u\t%u\t%u\n", *(s+0), *(s+1), *(s+2), *(s+3));
      // access with pointer arithmetic of struct members as byte (= unsigned char) pointers:
      //unsigned char *s = (unsigned char *)&(prowData->s);
      //printf("%u\t%u\t%u\t%u\n", *(Uint32*)(s+0), *(Uint32*)(s+4), *(Uint32*)(s+8), *(Uint32*)(s+12));

      // padding of first element in struct: same address location = no padding
      //printf("p: %zu\t%zu\n", (long unsigned int)(prowData), (long unsigned int)&(prowData->s));

      // direct access of struct members via Uint32 pointer arithmetic:
      //Uint32 *s = (Uint32*)prowData;
      //printf("%u\t%u\t%u\t%u\n", *(s+0), *(s+1), *(s+2), *(s+3));

      // direct access of struct members via byte (= unsigned char) pointer arithmetic:
      //unsigned char *s = (unsigned char *)prowData;
      //printf("%u\t%u\t%u\t%u\n", *(Uint32*)(s+4*0), *(Uint32*)(s+4*1), *(Uint32*)(s+4*2), *(Uint32*)(s+4*3));
    }

  if (rc != 1)  APIERROR(myTransaction->getNdbError());

  ixScan->close(true);
  printf("Done printing\n");

  myNdb.closeTransaction(myTransaction);
}
