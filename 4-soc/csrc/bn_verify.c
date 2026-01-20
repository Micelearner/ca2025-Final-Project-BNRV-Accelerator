#include <stdint.h>

#ifndef RNG_SEED
#define RNG_SEED 0x12345678u
#endif
uint32_t rng = RNG_SEED;

#define BN_SUM4(rd, rs1, rs2) \
    asm volatile (".insn r 0x0b, 0, 0, %0, %1, %2" : "=r" (rd) : "r" (rs1), "r" (rs2))

#define BN_STORE(rs1, rs2) \
    asm volatile (".insn r 0x0b, 0, 1, x0, %0, %1" :: "r" (rs1), "r" (rs2))

#define BN_SUM8(rd, rs1, rs2) \
    asm volatile (".insn r 0x0b, 0, 2, %0, %1, %2" : "=r" (rd) : "r" (rs1), "r" (rs2))

uint32_t pack_acts(int8_t a0, int8_t a1, int8_t a2, int8_t a3) {
    return ((uint32_t)(uint8_t)a0 << 0)  |
           ((uint32_t)(uint8_t)a1 << 8)  |
           ((uint32_t)(uint8_t)a2 << 16) |
           ((uint32_t)(uint8_t)a3 << 24);
}

uint8_t pack_weights_8bit(int w0, int w1, int w2, int w3) {
    #define ENC(w) ((w)==1 ? 1 : ((w)==-1 ? 2 : 0))
    uint8_t res = 0;
    res |= (ENC(w0) & 0x3) << 0;       
    res |= (ENC(w1) & 0x3) << 2;
    res |= (ENC(w2) & 0x3) << 4;
    res |= (ENC(w3) & 0x3) << 6;
    return res;
}

uint32_t xorshift32(void) {
    uint32_t x = rng;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    rng = x;
    return x;
}

int8_t rand_i8(void) {
    return (int8_t)(xorshift32() & 0xFF);
}

uint32_t rand_0_2(void) {
    while (1) {
        uint32_t r = xorshift32() & 3u; // 0..3
        if (r < 3) return r;            
    }
}

int rand_w3(void) {
    uint32_t r = rand_0_2();
    return (r == 0) ? 1 : (r == 1 ? -1 : 0);
}

int dec_w2(uint32_t code2) {
    if (code2 == 0) return 0;
    if (code2 == 1) return 1;
    if (code2 == 2) return -1;
    return 0x7fffffff;
}

int32_t unpack_act_safe(uint32_t packed, int idx) {
    uint32_t val = (packed >> (8 * idx)) & 0xFF;

    if (val & 0x80) {
        return (int32_t)(val | 0xFFFFFF00);
    } 
    
    return (int32_t)val;
}

int32_t ref_sum4(uint32_t acts_packed, uint32_t w_packed8) {
    int32_t acc = 0;
    for (int i = 0; i < 4; i++) {
        int32_t a = unpack_act_safe(acts_packed, i); 
        
        uint32_t w2 = (w_packed8 >> (2*i)) & 0x3;
        int w = dec_w2(w2);
        
        acc += a * w;
    }
    return acc;
}

int32_t ref_sum8(uint32_t acts_low, uint32_t acts_high, uint32_t w_low8,  uint32_t w_high8) {
    int32_t acc = 0;
    acc += ref_sum4(acts_low,  w_low8);
    acc += ref_sum4(acts_high, w_high8);
    return acc;
}

int main() {
    volatile int32_t hw = 0, sw = 0;
 
    // Case 1: BN.SUM4 random x4
    for (int it = 0; it < 100; it++) {
        int8_t a0 = rand_i8(), a1 = rand_i8(), a2 = rand_i8(), a3 = rand_i8();
        int w0 = rand_w3(), w1 = rand_w3(), w2 = rand_w3(), w3 = rand_w3();
        if (a0 == 0){
            a0 = (int8_t)9;
            w0 = (int)1;
        }

        uint32_t acts = pack_acts(a0, a1, a2, a3);
        uint32_t w8   = (uint32_t)pack_weights_8bit(w0, w1, w2, w3);

        // HW
        BN_SUM4(hw, acts, w8);

        // SW
        sw = ref_sum4(acts, w8);

        if (hw != sw) {
            return 0x1000 | it;
        }
    }

    // Case 2: BN.STORE + BN.SUM8 random x4
    for (int it = 0; it < 100; it++) {
        // 8 weights
        int w0 = rand_w3(), w1 = rand_w3(), w2 = rand_w3(), w3 = rand_w3();
        int w4 = rand_w3(), w5 = rand_w3(), w6 = rand_w3(), w7 = rand_w3();
        if (w0 == 0){
            w0 = (int)1;
        }

        uint32_t w_low8  = (uint32_t)pack_weights_8bit(w0, w1, w2, w3);
        uint32_t w_high8 = (uint32_t)pack_weights_8bit(w4, w5, w6, w7);

        // 8 activations (int8)
        int8_t a0 = rand_i8(), a1 = rand_i8(), a2 = rand_i8(), a3 = rand_i8();
        int8_t a4 = rand_i8(), a5 = rand_i8(), a6 = rand_i8(), a7 = rand_i8();
        if (a0 == 0){
            a0 = (int8_t)-9;
        }

        uint32_t acts_low  = pack_acts(a0, a1, a2, a3);
        uint32_t acts_high = pack_acts(a4, a5, a6, a7);

        // HW
        BN_STORE(w_low8, w_high8);
        BN_SUM8(hw, acts_low, acts_high);

        // SW reference
        sw = ref_sum8(acts_low, acts_high, w_low8, w_high8);

        if (hw != sw) {
            return 0x2000 | it;
        }
    }

    return 999; // ALL PASS
}
