#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>

#include "futbanko.h"
#include "bankopladeformat/bankopladeformat.h"

static int read_boards(FILE *in, int8_t **boards_out, int *nboards) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);
  int capacity = 1500, used = 0, error = 0;

  int8_t *boards = malloc(capacity * sizeof(int8_t));

  while (banko_reader_board(&reader, &board) == 0) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        if (board.cells[row][col] != 0) {
          boards[used++] = board.cells[row][col];
        }
      }
    }
    if ((used % 15) != 0) {
      fprintf(stderr, "bad %d\n", used);
      error = 1;
    }

    if (used == capacity) {
      capacity *= 2;
      boards = realloc(boards, capacity * sizeof(int8_t));
    }
  }

  banko_reader_close(&reader);

  *boards_out = boards;
  *nboards= used / 15;

  return error;
}

int main(int argc, char** argv) {
  int ngames = 1000;
  int opt, verbose = 0;
  unsigned int seed = 0;
  struct timeval t_start, t_end;
  const char *device = "";

  while ((opt = getopt(argc, argv, "d:g:s:V")) != -1) {
    switch (opt) {
    case 'g':
      ngames = atoi(optarg);
      break;
    case 'V':
      verbose = 1;
      break;
    case 's':
      seed = atoi(optarg);
      break;
    case 'd':
      device = optarg;
      break;
    default:
      fprintf(stderr, "Usage: %s [-g games] [-s seed] [-d device] [-V]\n",
              argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  gettimeofday(&t_start, NULL);

  if (!seed) {
    /* The game is heavily dependent on good-quality random numbers, so
       we seed as best we are able. */
    FILE* f = fopen("/dev/urandom", "rb");
    seed ^= fread(&seed, sizeof(unsigned int), 1, f);
    fclose(f);

    seed ^= time(NULL);
    seed ^= getpid();
    seed ^= getppid();
  }

  int8_t *boards;
  int nboards;
  int error = read_boards(stdin, &boards, &nboards);

  if (error) {
    fprintf(stderr, "%s: Invalid boards on stdin\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  struct futhark_context_config *cfg = futhark_context_config_new();
  futhark_context_config_set_device(cfg, device);

  struct futhark_context *ctx = futhark_context_new(cfg);

  struct futhark_i8_3d *boards_fut = futhark_new_i8_3d(ctx, boards, nboards, 3, 5);

  assert(futhark_context_sync(ctx) == 0);
  free(boards);

  struct futhark_opaque_game_winners *winners;

  gettimeofday(&t_start, NULL);
  assert(futhark_entry_run(ctx, &winners, seed, ngames, boards_fut) == 0);
  assert(futhark_context_sync(ctx) == 0);
  gettimeofday(&t_end, NULL);

  printf("Executed %d games, of %d boards each, in %dms.\n", ngames, nboards,
         (int) ((t_end.tv_sec*1000+t_end.tv_usec/1000) -
                (t_start.tv_sec*1000+t_start.tv_usec/1000)));

  struct futhark_i32_1d *one_row_winners_fut, *two_rows_winners_fut, *three_rows_winners_fut;
  assert(futhark_entry_winners_per_game
         (ctx,
          &one_row_winners_fut, &two_rows_winners_fut, &three_rows_winners_fut,
          winners) == 0);

  int32_t *one_row_winners = calloc(ngames, sizeof(int32_t));
  int32_t *two_rows_winners = calloc(ngames, sizeof(int32_t));
  int32_t *three_rows_winners = calloc(ngames, sizeof(int32_t));

  assert(futhark_values_i32_1d(ctx, one_row_winners_fut, one_row_winners) == 0);
  assert(futhark_values_i32_1d(ctx, two_rows_winners_fut, two_rows_winners) == 0);
  assert(futhark_values_i32_1d(ctx, three_rows_winners_fut, three_rows_winners) == 0);
  assert(futhark_context_sync(ctx) == 0);

  int32_t *num_wins = calloc(nboards, sizeof(int32_t));

  for (int i = 0; i < ngames; i++) {
    if (verbose) {
      printf("Game %10d: board %d won (board %d first to one row; board %d first to two rows)\n",
             i, two_rows_winners[i], three_rows_winners[i], one_row_winners[i]);
    }
    num_wins[three_rows_winners[i]]++;
  }

  printf("Wins per board:\n");
  for (int i = 0; i < nboards; i++) {
    printf("Board %10d: %d wins\n", i, num_wins[i]);
  }

  futhark_free_i32_1d(ctx, one_row_winners_fut);
  futhark_free_i32_1d(ctx, two_rows_winners_fut);
  futhark_free_i32_1d(ctx, three_rows_winners_fut);
  futhark_free_i8_3d(ctx, boards_fut);
  futhark_free_opaque_game_winners(ctx, winners);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);

  return 0;
}
