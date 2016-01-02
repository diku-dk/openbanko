/*
 * Test program for board reader and writer.  The actual library
 * implementation is all inlined in the header file.
 */

#include "bankopladeformat.h"

int main() {
  struct board board;

  struct banko_writer writer;
  struct banko_reader reader;

  banko_writer_open(&writer, stdout);
  banko_reader_open(&reader, stdin);

  while (banko_reader_board(&reader, &board) == 0) {
    banko_writer_board(&writer, &board);
  }

  banko_writer_close(&writer);
  banko_reader_close(&reader);
}
