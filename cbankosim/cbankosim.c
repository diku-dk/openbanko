/*
 * Fully automatic simulator for that classic Danish pasttime of Banko.
 *
 * Note that this simulator generates invalid boards.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>

/* Permute the size-n byte array.

   The famous "ryst posen"-operation. */
void permute(size_t n, uint8_t *p) {
  /* Fisher-Yates shuffle. */
  for (size_t i = n - 1; i >= 1; i--) {
    size_t j = rand() % (i + 1);
    uint8_t elem_j = p[j];
    p[j] = p[i];
    p[i] = elem_j;
  }
}

int timeval_subtract (struct timeval * result, struct timeval * t2,
                      struct timeval * t1)
{
  unsigned int resolution = 1000000;
  long diff = t2->tv_usec + resolution * t2->tv_sec - (t1->tv_usec +
                                                       resolution *
                                                       t1->tv_sec);
  result->tv_sec = diff / resolution;
  result->tv_usec = diff % resolution;
  return diff < 0;
}

struct board {
  uint8_t first_row[5];
  uint8_t first_remaining;

  uint8_t second_row[5];
  uint8_t second_remaining;

  uint8_t third_row[5];
  uint8_t third_remaining;
};

void reset_board(struct board *board) {
  board->first_remaining = 5;
  board->second_remaining = 5;
  board->third_remaining = 5;
}

struct player {
  size_t num_boards; /* Number of boards owned by this player. */
  struct board *boards; /* The array of boards owned by this player. */
};

void reset_player(struct player *player) {
  for (size_t i = 0; i < player->num_boards; i++) {
    reset_board(&player->boards[i]);
  }
}

int main(int argc, char** argv) {
  unsigned int seed;
  size_t nboards = 6;
  size_t nplayers = 80;
  size_t ngames = 1000;
  int opt;
  int verbose = 0, replay = 0;
  struct timeval time_start, time_end, time_diff;

  while ((opt = getopt(argc, argv, "p:b:g:r:V")) != -1) {
    switch (opt) {
    case 'p':
      nplayers = atoi(optarg);
      break;
    case 'b':
      nboards = atoi(optarg);
      break;
    case 'g':
      ngames = atoi(optarg);
      break;
    case 'V':
      verbose = 1;
      break;
    case 'r':
      replay = atoi(optarg);
      seed = replay;
      break;
    default:
      fprintf(stderr, "Usage: %s [-p players] [-b boards] [-V]\n",
              argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  gettimeofday(&time_start, NULL);

  if (!replay) {
    /* The game is heavily dependent on good-quality random numbers, so
       we seed as best we are able. */
    FILE* f = fopen("/dev/urandom", "rb");
    fread(&seed, sizeof(unsigned int), 1, f);
    fclose(f);

    seed ^= time(NULL);
    seed ^= getpid();
    seed ^= getppid();
  }

  srand(seed);

  /* Create auxillary array of the numbers 1-90.  We will scramble
     this whenever we need random numbers. */
  uint8_t numbers[90];
  for (size_t i = 0; i < 90; i++) {
    numbers[i] = i+1;
  }

  /* Construct players. */
  struct player *players = calloc(nplayers, sizeof(struct player));
  for (size_t i = 0; i < nplayers; i++) {
    /* Construct the boards for players[i]. */
    players[i].num_boards = nboards;
    players[i].boards = calloc(nboards, sizeof(struct board));
    for (size_t j = 0; j < nboards; j++) {
      permute(90, numbers);
      size_t p = 0;
      for (; p < 5; p++) {
        players[i].boards[j].first_row[p%5] = numbers[p];
      }
      for (; p < 10; p++) {
        players[i].boards[j].second_row[p%5] = numbers[p];
      }
      for (; p < 15; p++) {
        players[i].boards[j].third_row[p%5] = numbers[p];
      }
      reset_board(&players[i].boards[j]);
    }
  }

  /* Construct victory table.  Note that calloc() guarantees
     zero-initialisation. */
  int64_t *full_victories = calloc(nplayers, sizeof(int64_t));
  int64_t *row_victories = calloc(nplayers, sizeof(int64_t));

  if (verbose) {
      printf("Simulating %ld players with %ld boards each:\n\n", nplayers, nboards);

    for (size_t i = 0; i < nplayers; i++) {
      printf("Player %ld:\n", i);
      for (size_t j = 0; j < players[i].num_boards; j++) {
        printf("  Board %ld:\n", j);
        printf("    ");
        for (size_t p = 0; p < 5; p++) {
          printf("%02d ", players[i].boards[j].first_row[p]);
        }
        printf("\n      ");
        for (size_t p = 0; p < 5; p++) {
          printf("%02d ", players[i].boards[j].second_row[p]);
        }
        printf("\n    ");
        for (size_t p = 0; p < 5; p++) {
          printf("%02d ", players[i].boards[j].third_row[p]);
        }
        printf("\n");
      }
    }
  }

  for (size_t game_i = 0; game_i < ngames; game_i++) {
    /* Randomise selection. */
    permute(90, numbers);

    int can_win_row = 0;
    int row_won = 0;
    int full_won = 0;

    for (size_t number_i = 0; number_i < 90; number_i++) {
      /* Pick a number! */
      uint8_t number = numbers[number_i];

      if (verbose || replay) {
        printf("Picked number %d!\n", number);
      }

      /* Cross out every board with this number, jumping out if we
         find a winner. */
      for (size_t player_i = 0; player_i < nplayers; player_i++) {
        for (size_t board_i = 0; board_i < players[player_i].num_boards; board_i++) {
          /* */
          struct board *board = &players[player_i].boards[board_i];
          for (int i = 0; i < 5; i++) {
            if (board->first_row[i] == number) {
              board->first_remaining--;
            }
          }
          for (int i = 0; i < 5; i++) {
            if (board->second_row[i] == number) {
              board->second_remaining--;
            }
          }
          for (int i = 0; i < 5; i++) {
            if (board->third_row[i] == number) {
              board->third_remaining--;
            }
          }
          if (board->first_remaining == 0 &&
              board->second_remaining == 0 &&
              board->third_remaining == 0) {
            if (verbose || replay) {
              printf("Player %ld wins a board!\n", player_i);
            }

            full_won = 1;
            full_victories[player_i]++;
            /* Only one victory per player. */
            continue;
          }
          if (can_win_row &&
              (board->first_remaining == 0 ||
               board->second_remaining == 0 ||
               board->third_remaining == 0)) {
            if (verbose || replay) {
              printf("Player %ld wins a row!\n", player_i);
            }

            row_won = 1;
            row_victories[player_i]++;
            /* Only one victory per player. */
            continue;
          }
        }
      }

      can_win_row = !row_won;

      if (full_won) {
        if (verbose || replay) {
          printf("End of game!\n");
        }
        for (size_t player_i = 0; player_i < nplayers; player_i++) {
          reset_player(&players[player_i]);
        }
        break;
      }
    }

    if (!full_won) {
      printf("Disaster!  All numbers drawn, but no winner!\n");
      exit(EXIT_FAILURE);
    }
  }
  gettimeofday(&time_end, NULL);
  timeval_subtract(&time_diff, &time_end, &time_start);

  unsigned long elapsed_usec = time_diff.tv_sec * 1e6 + time_diff.tv_usec;

  for (size_t player_i = 0; player_i < nplayers; player_i++) {
    printf("Player %ld won %ld times (and %ld rows)\n",
           player_i, full_victories[player_i], row_victories[player_i]);
  }

  printf("Executed %ld games in %ld miliseconds (%f games per second)\n",
         ngames, elapsed_usec / 1000, (double)ngames / (elapsed_usec / (double)1e6));

  if (!replay) {
    printf("Do you want to send a replay of this game to your friends?\n");
    printf("Just tell them to run:\n%s -p %ld -b %ld -g %ld -r %d\n",
           argv[0], nplayers, nboards, ngames, seed);
  }

  return 0;
}
