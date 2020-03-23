int fib(int x) {
  int first;
  int second;
  int result;

  if (x == 0) {
    result = 0;
  } else if (x == 1) {
    result = 1;
  } else {
    first = fib(x-2);
    second = fib(x-1);
    result = first + second;
  }

  return result;
}

int main(int argc, char **argv) {
  int result;
  result = fib(1);
  return result;
}
