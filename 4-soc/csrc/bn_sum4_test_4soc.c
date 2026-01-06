#include <stdint.h>
#include "mmio.h"

#define TEST_DONE_MAGIC 0xCAFEF00Du

static inline int32_t bn_sum4_hw(uint32_t acts_packed, uint32_t wts_packed)
{
    int32_t rd;
    // R-type: .insn r opcode, funct3, funct7, rd, rs1, rs2
    // custom-0 opcode = 0x0B, funct3=0, funct7=0x00 (per your BN.SUM4 decode)
    __asm__ volatile (".insn r 0x0b, 0, 0x00, %0, %1, %2"
                      : "=r"(rd)
                      : "r"(acts_packed), "r"(wts_packed));
    return rd;
}

static inline int w2_to_int(uint32_t w2)
{
    if (w2 == 1u) return  1;
    if (w2 == 2u) return -1;
    return 0;
}

static inline void acc(int32_t *s, int8_t a, uint32_t w2)
{
    switch (w2 & 3u) {
    case 1u: *s += (int32_t)a; break;   // +1
    case 2u: *s -= (int32_t)a; break;   // -1
    default: /* 0 */ break;
    }
}

static int32_t bn_sum4_ref(uint32_t acts_packed, uint32_t wts_packed)
{
    int8_t a0 = (int8_t)(acts_packed >> 0);
    int8_t a1 = (int8_t)(acts_packed >> 8);
    int8_t a2 = (int8_t)(acts_packed >> 16);
    int8_t a3 = (int8_t)(acts_packed >> 24);

    uint32_t w0 = (wts_packed >> 0) & 0x3;
    uint32_t w1 = (wts_packed >> 2) & 0x3;
    uint32_t w2 = (wts_packed >> 4) & 0x3;
    uint32_t w3 = (wts_packed >> 6) & 0x3;

    int32_t s = 0;
    acc(&s, a0, w0);
    acc(&s, a1, w1);
    acc(&s, a2, w2);
    acc(&s, a3, w3);
    return s;
}


int main(void)
{
    static const uint32_t acts[] = {
        0x04030201u, // [  1,  2,  3,  4]
        0x6407FEFFu, // [ -1, -2,  7,100]
        0x06FB7F80u, // [-128,127, -5,  6]
        0x0080FF7Fu, // [ 127, -1,-128,  0]
        0xA50A55F0u, // [ -16,  85, 10,-91] (0xF0,0x55,0x0A,0xA5)
    };

    static const uint32_t wts[] = {
        0x55u, // [+1,+1,+1,+1]
        0x61u, // [w0=+1,w1=0,w2=-1,w3=+1]
        0x92u, // [w0=-1,w1=+1,w2=0,w3=-1]
        0x19u, // [w0=+1,w1=-1,w2=+1,w3=0]
        0xA6u, // [w0=-1,w1=+1,w2=-1,w3=-1]
    };

    const int n = (int)(sizeof(acts) / sizeof(acts[0]));

    for (int i = 0; i < n; i++) {
        const uint32_t a = acts[i];
        const uint32_t w = wts[i];

        const int32_t hw = bn_sum4_hw(a, w);
        const int32_t sw = bn_sum4_ref(a, w);

        if (hw != sw) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
            *TEST_RESULT = 0xBAD00000u | ((uint32_t)i & 0xFFu);
            *TEST_DONE_FLAG = TEST_DONE_MAGIC;
#pragma GCC diagnostic pop
            while (1) __asm__ volatile("wfi");
        }
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
    *TEST_RESULT = 0x00000001u;      // PASS
    *TEST_DONE_FLAG = TEST_DONE_MAGIC;
#pragma GCC diagnostic pop

    while (1) __asm__ volatile("wfi");
    return 0;
}
