void print(uint64_t* s) {
  uint64_t* buffer;
  buffer = s;

  while (*buffer != 0) {
    write(1, buffer, 8);
    buffer = buffer + 1;
  }
}

int main() {
  print("Hello world!");
  return 55;
}
