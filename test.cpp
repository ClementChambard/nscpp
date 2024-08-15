#include "test.h"
#define ADD(a, b) a + b

int main(int argc, char **argv) {
  NS_ASSERT_M(1 + 1 == 0, "Oh oh...");
  return ADD(1, -1);
}
