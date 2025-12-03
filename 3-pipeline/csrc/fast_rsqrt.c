#include <stdint.h>

static const uint16_t rsqrt_table[32] = {
    65535, 46341, 32768, 23170, 16384, 11585, 8192, 5792,
    4096, 2896, 2048, 1448, 1024, 724, 512, 362,
    256, 181, 128, 91, 64, 45, 32, 23,
    16, 11, 8, 6, 4, 3, 2, 1};

static int clz(uint32_t x) {
    if (!x) return 32;
    int n = 0;
    if (!(x & 0xFFFF0000u)) { n += 16; x <<= 16; }
    if (!(x & 0xFF000000u)) { n += 8;  x <<= 8;  }
    if (!(x & 0xF0000000u)) { n += 4;  x <<= 4;  }
    if (!(x & 0xC0000000u)) { n += 2;  x <<= 2;  }
    if (!(x & 0x80000000u)) { n += 1; }
    return n;
}

static uint64_t mul32(uint32_t a, uint32_t b) {
    uint64_t r = 0;
    for (int i = 0; i < 32; i++) {
        if (b & (1u << i))
            r += ((uint64_t)a << i);
    }
    return r;
}

uint32_t fast_rsqrt(uint32_t x) {
    if (x == 0) return 0xFFFFFFFF;
    if (x == 1) return 65536;

    int exp = 31 - clz(x);
    uint32_t y_base = rsqrt_table[exp];
    uint32_t y_next = (exp < 31) ? rsqrt_table[exp + 1] : 0;
    uint32_t diff = y_base - y_next;
    uint32_t base = 1u << exp;
    uint32_t frac = ((x - base) << 16) >> exp;
    uint32_t y = y_base - (uint32_t)(mul32(diff, frac) >> 16);

    for (int i = 0; i < 2; i++) {
        uint32_t y2 = (uint32_t)(mul32(y, y) );
        uint32_t xy2 = (uint32_t)(mul32(x, y2) >> 16);
        uint32_t term = (3u << 16) - xy2;
        uint32_t prod = (uint32_t)(mul32(y, term) >> 16);
        y = prod >> 1;
    }
    return y;
}

void main() {
    uint32_t test_inputs[] = {1, 2, 3, 4, 10, 100, 1000, 10000, 4294967295};
    int num_tests = 9;

    for (int i = 0; i < num_tests; i++) {
        uint32_t res = fast_rsqrt(test_inputs[i]);

        volatile int* output_addr = (volatile int*)(0x100 + i * 4);
        *output_addr = res;
    }

    *((volatile int *) 0x4) = 0xDEADBEEF;
    while(1);
}
