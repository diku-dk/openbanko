/*
 * sort(1), but for .bankopladeformat-files.  Sorts lexicographically.
 * Reads on stdin and outputs on stdout.  Manifests all boards in
 * memory, so unsuitable for gigantic data sets.
 */

#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <errno.h>
#include <string.h>

#include "bankopladeformat.h"

static int cmp_boards(const void *xp, const void *yp) {
  const struct board *x = xp;
  const struct board *y = yp;

  int xmask = banko_board_mask(x);
  int ymask = banko_board_mask(y);

  if (xmask < ymask) {
    return -1;
  } else if (ymask < xmask) {
    return 1;
  } else {
    return 0;
  }
}

int main(int argc, char** argv) {
  if (argc != 1) {
    fprintf(stderr, "Usage: %s < input > utput\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  struct board *boards;
  int nboards;

  if (banko_read_all_boards(stdin, &boards, &nboards) != 0) {
    fprintf(stderr, "%s: failed reading boards from stdin.\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  qsort(boards, nboards, sizeof(struct board), cmp_boards);

  banko_write_all_boards(stdout, boards, nboards);

  return 0;
}
