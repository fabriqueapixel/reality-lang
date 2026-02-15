#include <ctype.h>
#include <gc.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool string_eq(char *a, char *b) { return strcmp(a, b) == 0; }

char *malloc_string(const char *content) {
  size_t length = strlen(content);
  char *copy = (char *)GC_MALLOC(length + 1);
  if (copy) {
    strcpy(copy, content);
  }
  return copy;
}

char *number_to_string(int number) {
  size_t length = snprintf(NULL, 0, "%d", number);

  char *result = (char *)GC_MALLOC(length + 1);
  if (result) {
    snprintf(result, length + 1, "%d", number);
  }
  return result;
}

char *pointer_to_string(void *ptr) {
  size_t length = snprintf(NULL, 0, "%p", ptr);

  char *result = (char *)GC_MALLOC(length + 1);
  if (result) {
    snprintf(result, length + 1, "%p", ptr);
  }
  return result;
}

int add_number(int a, int b) { return a + b; }

int mul_number(int a, int b) { return a * b; }

int div_number(int a, int b) {
  if (b != 0) {
    return a / b;
  }
  // Handle division by zero case
  return 0;
}

int sub_number(int a, int b) { return a - b; }

bool equals_number(int a, int b) { return a == b; }

char *fetch_ptr(char *container, uint32_t index) { return &container[index]; }

char *ptr_add(char *ptr, uint32_t offset) { return ptr + offset; }

int greater_number(int a, int b) { return a > b; }

int less_number(int a, int b) { return a < b; }

int mod_number(int a, int b) {
  if (b != 0) {
    return a % b;
  }
  // Handle division by zero case
  return 0;
}

bool char_eq(char a, char b) { return a == b; }

char *concat_strings(char *a, char *b) {
  size_t length = strlen(a) + strlen(b);
  char *result = (char *)GC_MALLOC(length + 1);
  if (result) {
    strcpy(result, a);
    strcat(result, b);
  }
  return result;
}

void *malloc_ext(uint64_t size) { return malloc(size); }

int free_ext(void *ptr) {
  free(ptr);
  return 0;
}

void *realloc_ext(void *ptr, uint64_t size) { return GC_REALLOC(ptr, size); }

void panic_ext(char *message) {
  fprintf(stderr, "%s\n", message);
  exit(1);
}

bool is_whitespace(char c) { return isspace((unsigned char)c); }

bool is_alpha(char c) { return isalpha((unsigned char)c); }

bool is_alphanumeric(char c) { return isalnum((unsigned char)c); }

bool is_digit(char c) { return isdigit((unsigned char)c); }

void *BDWGC_malloc(size_t size) { return GC_MALLOC(size); }

void *BDWGC_realloc(void *ptr, size_t size) { return GC_REALLOC(ptr, size); }

void BDWGC_free(void *ptr) { GC_FREE(ptr); }

char int_to_char(int32_t x) { return (char)x; }

int32_t char_to_int(char c) { return (int32_t)c; }

float add_float_(float a, float b) { return a + b; }

float sub_float_(float a, float b) { return a - b; }

float mul_float_(float a, float b) { return a * b; }

float div_float_(float a, float b) {
  if (b != 0.0f) {
    return a / b;
  }
  // Handle division by zero case
  return 0.0f;
}

bool equals_float_(float a, float b) { return a == b; }

bool greater_float_(float a, float b) { return a > b; }

bool less_float_(float a, float b) { return a < b; }

float mod_float_(float a, float b) {
  if (b != 0.0f) {
    return fmodf(a, b);
  }
  // Handle division by zero case
  return 0.0f;
}

int32_t float_to_int(float f) { return (int32_t)f; }

float int_to_float(int32_t i) { return (float)i; }

char *float_to_string(float f) {
  size_t length = snprintf(NULL, 0, "%g", f);

  char *result = (char *)GC_MALLOC(length + 1);
  if (result) {
    snprintf(result, length + 1, "%g", f);
  }
  return result;
}

float string_to_float(char *str) { return strtof(str, NULL); }

int string_to_int(char *str) { return atoi(str); }

int string_to_hex(char *str) { return (int)strtol(str, NULL, 16); }

float pow_float(float base, float exponent) { return powf(base, exponent); }

float sqrt_float(float x) { return sqrtf(x); }
