int main() {
  int a;
  goto jmp_label;

  a = 2;
  return a;

jmp_label:
  a = 1;
  return a;
}
