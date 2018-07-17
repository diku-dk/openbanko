#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#include "algorithm_0.h"
#include "algorithm_1.h"
#include "algorithm_2.h"
#include "algorithm_3.h"
#include "algorithm_4.h"
#include "algorithm_5.h"
#include "algorithm_6.h"
#include "algorithm_7.h"

typedef void (*compressor)(FILE*, FILE*);
typedef void (*decompressor)(FILE*, FILE*);

struct algorithm {
  compressor compressor;
  decompressor decompressor;
};

struct algorithm algorithms[] = {
  {a0_compress, a0_decompress},
  {a1_compress, a1_decompress},
  {a2_compress, a2_decompress},
  {a3_compress, a3_decompress},
  {a4_compress, a4_decompress},
  {a5_compress, a5_decompress},
  {a6_compress, a6_decompress},
  {a7_compress, a7_decompress}
};

static const int num_algorithms =
  sizeof(algorithms)/sizeof(struct algorithm);

int main(int argc, char** argv) {
  int opt;
  int compress = -1;
  int decompress = -1;

  while ((opt = getopt(argc, argv, "d:c:")) != -1) {
    switch (opt) {
    case 'd':
      decompress = atoi(optarg);
      break;
    case 'c':
      compress = atoi(optarg);
      break;
    default:
      fprintf(stderr, "Usage: %s <-d algorithm | -c algorithm>\n",
              argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  if (compress == -1 && decompress == -1) {
    fprintf(stderr, "%s: Must pass either -d or -c.\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  if (compress != -1 && decompress != -1) {
    fprintf(stderr, "%s: Cannot pass both -d and -c.\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  if (compress >= num_algorithms || decompress >= num_algorithms) {
    fprintf(stderr, "%s: Only supports up to %d algorithms.\n",
            argv[0], num_algorithms);
    exit(EXIT_FAILURE);
  }

  if (compress != -1) {
    algorithms[compress].compressor(stdout, stdin);
  }
  else {
    algorithms[decompress].decompressor(stdout, stdin);
  }
}
