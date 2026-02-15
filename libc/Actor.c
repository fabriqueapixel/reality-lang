#define GC_THREADS
#include <gc.h>
#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>

void undefined() {
    return;
}

void pthread_yield() {
    sched_yield();
}

void wait_time(int32_t milliseconds) {
    usleep(milliseconds * 1000);
}

typedef struct {
  void* (*function)(void*);
  void* environment;
} generic_function;

static void* thread_trampoline(void* arg) {
  GC_register_my_thread(NULL);
  generic_function* f = (generic_function*)arg;
  void* result = f->function(f->environment);
  return result;
}

pthread_t create_thread(generic_function* f) {
  pthread_t thread;
  pthread_create(&thread, NULL, thread_trampoline, f);
  return thread;
}

char* read_file_ext(const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) {
        return "";
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(length + 1);
    if (buffer) {
        fread(buffer, 1, length, file);
        buffer[length] = '\0';
    }

    fclose(file);
    return buffer;
}

typedef enum { 
    WRITE_FILE_SUCCESS = 0,
    WRITE_FILE_ERROR_OPEN = 1,
    WRITE_FILE_ERROR_WRITE = 2
} WriteFileResult;

WriteFileResult write_file_ext(const char* path, const char* content) {
    FILE* file = fopen(path, "wb");
    if (!file) {
        return WRITE_FILE_ERROR_OPEN;
    }

    size_t length = strlen(content);
    size_t written = fwrite(content, 1, length, file);
    fclose(file);

    if (written != length) {
        return WRITE_FILE_ERROR_WRITE;
    }

    return WRITE_FILE_SUCCESS; // Indicate success
}

void exit_program(int code) {
    exit(code);
}

char* get_current_working_directory() {
  size_t size = 1024;
  char* buffer = (char*)GC_MALLOC(size * sizeof(char));
  if (getcwd(buffer, size) != NULL) {
    return buffer;
  } else {
    GC_FREE(buffer);
    return NULL;
  }
}

bool file_exists(const char* path) { return access(path, F_OK) != -1; }

int execute_command(const char* command) { return system(command); }

bool folder_exists(const char* path) { return access(path, F_OK) != -1; }

char* get_env(const char* var) {
  char* value = getenv(var);
  if (value != NULL) {
    size_t len = strlen(value);
    char* result = (char*)GC_MALLOC((len + 1) * sizeof(char));
    strcpy(result, value);
    return result;
  } else {
    return "";
  }
}

int32_t print_int(int32_t x) {
    printf("%d\n", x);
    return x;
}

