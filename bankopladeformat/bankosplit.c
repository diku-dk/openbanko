#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <errno.h>

#include "bankopladeformat.h"

int main(int argc, const char** argv) {
  struct banko_reader r;
  struct board b;
  FILE *f;

  banko_reader_open(&r, stdin);

  for (int i = 1; i < argc; i++) {
    const char *s = argv[i];
    if (i+1 < argc) {
      const char *filename = argv[++i];

      char *end;
      int n = strtol(s, &end, 10);

      if (*end != 0) {
        error(1, 0, "Invalid split size: %s", s);
      }

      if ((f = fopen(filename, "w"))) {
        struct banko_writer w;
        banko_writer_open(&w, f);
        while (n-- && banko_reader_board(&r, &b) == 0) {
          banko_writer_board(&w, &b);
        }
        banko_writer_close(&w);
        fclose(f);
      } else {
        error(1, errno, "Error when opening %s", filename);
      }
    } else {
      if ((f = fopen(s, "w"))) {
        struct banko_writer w;
        banko_writer_open(&w, f);
        while (banko_reader_board(&r, &b) == 0) {
          banko_writer_board(&w, &b);
        }
        banko_writer_close(&w);
        fclose(f);
      } else {
        error(1, errno, "Error when opening %s", s);
      }
    }
  }
}
