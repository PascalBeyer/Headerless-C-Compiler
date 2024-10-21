typedef struct {
  unsigned version: 2;
  unsigned type: 6;

  unsigned payload: 24;
} LanPacket;

#define CASSERT(EXPRESSION) switch (0) {case 0: case (EXPRESSION):;}

int main() {
    CASSERT(sizeof(LanPacket) == 4);
}
