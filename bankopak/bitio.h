#ifndef BINIO_H
#define BINIO_H

/* Warning: these functions are not thread-safe. */

static unsigned int read_bit(FILE *in);
static void write_bit(unsigned int c, FILE *out);
static void flush_bit(FILE *out);
/* You can only unget one bit. */
static void unget_bit(unsigned int c);
static unsigned int peek_bit(FILE *out);
static unsigned int read_bits(int n, FILE *in);
static void write_bits(int n, unsigned int c, FILE *in);

static int write_bits_buffered = 0;
static unsigned char write_bit_buffer = 0;
static int read_bits_buffered = 0;
static unsigned char read_bit_buffer = 0;

static void flush_bit(FILE *out) {
  if (write_bits_buffered) {
    write_bit_buffer <<= 8 - write_bits_buffered;
    fputc(write_bit_buffer, out);
    write_bits_buffered = 0;
  }
}

static void write_bit(unsigned int c, FILE *out) {
  write_bit_buffer <<= 1;
  write_bit_buffer |= c;
  if (++write_bits_buffered == 8) {
    flush_bit(out);
  }
}

static unsigned int read_bit(FILE *in) {
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

static void unget_bit(unsigned int c) {
  read_bit_buffer |= c << read_bits_buffered++;
}

static unsigned int peek_bit(FILE *in) {
  int c = read_bit(in);
  unget_bit(c);
  return c;
}

static void write_bits(int n, unsigned int c, FILE *out) {
  for (int i = n; i >= 0; i--) {
    write_bit((c>>i)&1, out);
  }
}

static unsigned int read_bits(int n, FILE *in) {
  int res = 0;
  for (int i = n; i >= 0; i--) {
    int c = read_bit(in);
    if (c == EOF) {
      if (i == n) {
        return EOF;
      } else {
        c = 0;
      }
    }
    res <<= 1;
    res |= c;
  }
  return res;
}

#endif
