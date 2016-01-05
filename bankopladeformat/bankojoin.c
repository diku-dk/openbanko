#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <errno.h>

#include "bankopladeformat.h"

int main(int argc, char** argv) {
  argv=argv;
  if (argc != 1) {
    error(1, 0, "This program does not take any options.");
  }

  struct banko_reader r;
  struct banko_writer w;
  struct board b;

  banko_writer_open(&w, stdout);

  while (banko_reader_open(&r, stdin) == 0) {
    while (banko_reader_board(&r, &b) == 0) {
      banko_writer_board(&w, &b);
    }
    banko_reader_close(&r);
  }

  banko_writer_close(&w);

  return 0;
}
