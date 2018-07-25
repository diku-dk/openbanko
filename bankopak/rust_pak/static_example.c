#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#define BOARD_ROWS 3
#define BOARD_COLS 9

struct board {
  uint8_t cells[BOARD_ROWS][BOARD_COLS];
};

typedef struct _rust_encoder rust_encoder;
rust_encoder *rust_encoder_init();
void rust_encoder_free(rust_encoder*);
bool rust_encoder_run(const rust_encoder*, const struct board*, uint64_t*);

typedef struct _rust_decoder rust_decoder;
rust_decoder *rust_decoder_init();
void rust_decoder_free(rust_decoder*);
bool rust_decoder_run(const rust_decoder*, uint64_t, struct board*);

int main() {
  struct board my_board;

  rust_decoder* decoder = rust_decoder_init();

  rust_decoder_run(decoder, 12345, &my_board);

  for (int row = 0; row < 3; row++) {
    for (int col = 0; col < 9; col++) {
      if (col != 0) {
        printf(" ");
      }
      printf("%.2d", my_board.cells[row][col]);
    }
    printf("\n");
  }
  printf("\n");
}
