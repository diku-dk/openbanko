/* Statistical analysis of the deltas of a given banko board. */
#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <stdint.h>

#include "bankopladeformat.h"

int main(int argc, char** argv) {
  argv=argv;
  if (argc != 1) {
    error(1, 0, "This program does not take any options.");
  }

  struct banko_reader r;
  struct board b;

  int frequencies[BOARD_ROWS][BOARD_COLS][BOARD_COLS+2] = { 0 };
  int boards = 0;

  banko_reader_open(&r, stdin);
  while (banko_reader_board(&r, &b) == 0) {
    for (int col = 0; col < BOARD_COLS; col++) {
      uint8_t prev = col*10;
      for (int row = 0; row < BOARD_ROWS; row++) {
        uint8_t cell = b.cells[row][col];
        if (cell == 0) {
          frequencies[row][col][0]++;
        } else {
          frequencies[row][col][cell - prev + 1]++;
          prev = cell;
        }
      }
    }
    boards++;
  }
  banko_reader_close(&r);

  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int col = 0; col < BOARD_COLS; col++) {
      printf("row %d column %d: ", row, col);
      for (int i = 0; i < BOARD_COLS+2; i++) {
        printf("%.2f%% ", ((frequencies[row][col][i] / (float)boards) * 100));
      }
      printf("\n");
    }
  }

  return 0;
}
