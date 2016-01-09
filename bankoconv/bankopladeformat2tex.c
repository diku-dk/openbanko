#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "bankopladeformat/bankopladeformat.h"

#define CELL_START "\\begin{minipage}[t]{20mm}\\begin{center}"
#define CELL_END "\\end{center}\\end{minipage} "
#define PAGE_START "\\topskip0pt\n\\vspace*{\\fill}\n"
#define PAGE_END "\\vspace*{\\fill}\n\\newpage\n\n"

void board_to_tex(FILE *out, struct board board) {
  fputs("\\begin{tabular}{|r|r|r|r|r|r|r|r|r|}\n\\hline\n", out);
  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int col = 0; col < BOARD_COLS; col++) {
      if (board.cells[row][col] != 0) {
        fprintf(out, CELL_START "%d" CELL_END, board.cells[row][col]);
      } else {
        fputs(CELL_START CELL_END, out);
      }
      if (col != BOARD_COLS - 1) {
        fputs("& ", out);
      }
    }
    fputs("\\\\[5mm]\\hline\n", out);
  }
  fputs("\\end{tabular}\n\n", out);
}

void two_boards_page_to_tex(FILE *out, struct board board0, struct board board1) {
  fputs(PAGE_START, out);

  board_to_tex(out, board0);
  fputs("\\vspace{16mm}\n", out);
  board_to_tex(out, board1);

  fputs(PAGE_END, out);
}

void one_board_page_to_tex(FILE *out, struct board board) {
  fputs(PAGE_START, out);

  board_to_tex(out, board);

  fputs(PAGE_END, out);
}

void boards_to_tex(FILE *out, FILE *in) {
  struct board board0, board1;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  fputs("\\documentclass{article}\n", out);
  fputs("\\usepackage[a4paper,landscape,margin=12mm]{geometry}\n", out);
  fputs("\\usepackage[utf8]{inputenc}\n", out);
  fputs("\\usepackage[T1]{fontenc}\n", out);
  fputs("\\usepackage[danish]{babel}\n", out);
  fputs("\\usepackage{microtype}\n", out);
  fputs("\\usepackage{palatino}\n", out);
  fputs("\\usepackage{fancyhdr}\n", out);
  fputs("\\usepackage{array}\n", out);
  fputs("\\pagestyle{empty}\n", out);
  fputs("\\setlength{\\extrarowheight}{18mm}\n", out);
  fputs("\\begin{document}\n", out);
  fputs("\n", out);
  fputs("\\fontsize{19mm}{0mm}\\selectfont\n", out);
  fputs("\\begin{center}\n", out);

  while (1) {
    if (banko_reader_board(&reader, &board0) != 0) {
      break;
    }
    if (banko_reader_board(&reader, &board1) != 0) {
      one_board_page_to_tex(out, board0);
      break;
    }
    two_boards_page_to_tex(out, board0, board1);
  }

  fputs("\\end{center}\n", out);
  fputs("\\end{document}\n", out);

  banko_reader_close(&reader);
}

int main() {
  boards_to_tex(stdout, stdin);
  return EXIT_SUCCESS;
}
