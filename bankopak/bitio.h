#ifndef BINIO_H
#define BINIO_H

/* Warning: these functions are not thread-safe. */

static int read_bit(FILE *in);
static void write_bit(int c, FILE *out);
static void flush_bit(FILE *out);

static int write_bits_buffered = 0;
static unsigned char write_bit_buffer = 0;

static void flush_bit(FILE *out) {
  if (write_bits_buffered) {
    write_bit_buffer <<= 8 - write_bits_buffered;
    fputc(write_bit_buffer, out);
    write_bits_buffered = 0;
  }
}

static void write_bit(int c, FILE *out) {
  write_bit_buffer <<= 1;
  write_bit_buffer |= c;
  if (++write_bits_buffered == 8) {
    flush_bit(out);
  }
}


static int read_bit(FILE *in) {
  static int read_bits_buffered = 0;
  static unsigned char read_bit_buffer = 0;

  if (read_bits_buffered) {
    int c = (read_bit_buffer >> (--read_bits_buffered)) & 1;
    return c;
  } else {
    int c = fgetc(in);
    if (c == EOF) {
      return EOF;
    }
    read_bit_buffer = c;
    read_bits_buffered = 7;
    return (read_bit_buffer >> 7) & 1;
  }
}

#endif
