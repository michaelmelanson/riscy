
int fib(int x) {
  if (x <= 1) {
    return 1;
  }

  return fib(x-2) + fib(x-1);
}

int main(int argc, char **argv) {
  return fib(argc);
}
