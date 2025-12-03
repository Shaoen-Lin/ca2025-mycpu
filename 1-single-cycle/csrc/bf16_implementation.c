#include <stdbool.h>
#include <stdint.h>

/* Software division for RV32I (no M extension) */
static unsigned long udiv(unsigned long dividend, unsigned long divisor)
{
    if (divisor == 0)
        return 0;

    unsigned long quotient = 0;
    unsigned long remainder = 0;

    for (int i = 31; i >= 0; i--) {
        remainder <<= 1;
        remainder |= (dividend >> i) & 1;

        if (remainder >= divisor) {
            remainder -= divisor;
            quotient |= (1UL << i);
        }
    }

    return quotient;
}

static unsigned long umod(unsigned long dividend, unsigned long divisor)
{
    if (divisor == 0)
        return 0;

    unsigned long remainder = 0;

    for (int i = 31; i >= 0; i--) {
        remainder <<= 1;
        remainder |= (dividend >> i) & 1;

        if (remainder >= divisor) {
            remainder -= divisor;
        }
    }

    return remainder;
}

/* Software multiplication for RV32I (no M extension) */
static uint32_t umul(uint32_t a, uint32_t b)
{
    uint32_t result = 0;
    while (b) {
        if (b & 1)
            result += a;
        a <<= 1;
        b >>= 1;
    }
    return result;
}

/* Provide __mulsi3 for GCC */
uint32_t __mulsi3(uint32_t a, uint32_t b)
{
    return umul(a, b);
}

/* ============= BFloat16 Implementation ============= */

typedef struct {
    uint16_t bits;
} bf16_t;

#define BF16_EXP_BIAS 127
#define BF16_SIGN_MASK 0x8000U
#define BF16_EXP_MASK 0x7F80U
#define BF16_MANT_MASK 0x007FU

#define BF16_NAN() ((bf16_t) {.bits = 0x7FC0})
#define BF16_ZERO() ((bf16_t) {.bits = 0x0000})

static const bf16_t bf16_one = {.bits = 0x3F80};
static const bf16_t bf16_two = {.bits = 0x4000};

static inline bool bf16_isnan(bf16_t a)
{
    return ((a.bits & BF16_EXP_MASK) == BF16_EXP_MASK) &&
           (a.bits & BF16_MANT_MASK);
}

static inline bool bf16_isinf(bf16_t a)
{
    return ((a.bits & BF16_EXP_MASK) == BF16_EXP_MASK) &&
           !(a.bits & BF16_MANT_MASK);
}

static inline bool bf16_iszero(bf16_t a)
{
    return !(a.bits & 0x7FFF);
}

static inline unsigned clz(uint32_t x)
{
    int n = 32, c = 16;
    do {
        uint32_t y = x >> c;
        if (y) {
            n -= c;
            x = y;
        }
        c >>= 1;
    } while (c);
    return n - x;
}

static inline bf16_t bf16_add(bf16_t a, bf16_t b)
{
    uint16_t sign_a = a.bits >> 15 & 0x1, sign_b = b.bits >> 15 & 1;
    int16_t exp_a = a.bits >> 7 & 0xFF, exp_b = b.bits >> 7 & 0xFF;
    uint16_t mant_a = a.bits & 0x7F, mant_b = b.bits & 0x7F;

    /* Infinity and NaN */
    if (exp_a == 0xFF) {
        if (mant_a)
            return a;
        if (exp_b == 0xFF)
            return (mant_b || sign_a == sign_b) ? b : BF16_NAN();
        return a;
    }

    /* if a is normal/denormal, but b is infinity/NaN */
    if (exp_b == 0xFF)
        return b;

    /* if a == 0, b == 0 */
    if (!exp_a && !mant_a)
        return b;
    if (!exp_b && !mant_b)
        return a;

    /* if a, b is normal */
    if (exp_a)
        mant_a |= 0x80;
    if (exp_b)
        mant_b |= 0x80;

    int16_t exp_diff = exp_a - exp_b;
    uint16_t result_sign;
    int16_t result_exp;
    uint32_t result_mant;

    /* deal with result of exp */
    if (exp_diff > 0) {
        result_exp = exp_b;
        if (exp_diff > 8)
            return a;
        mant_a <<= exp_diff;
    } else if (exp_diff < 0) {
        result_exp = exp_a;
        if (exp_diff < -8)
            return b;
        mant_b <<= -exp_diff;
    } else
        result_exp = exp_a;

    if (sign_a == sign_b) {
        result_sign = sign_a;
        result_mant = (uint32_t) mant_a + mant_b;
        uint32_t lz = clz(result_mant);
        for (unsigned i = 0; i < 32 - lz - 8; i++) {
            result_mant >>= 1;
            if (++result_exp >= 255)
                return BF16_NAN();
        }
    } else {
        if (mant_a >= mant_b) {
            result_sign = sign_a;
            result_mant = mant_a - mant_b;
        } else {
            result_sign = sign_b;
            result_mant = mant_b - mant_a;
        }
        if (!result_mant)
            return BF16_ZERO();
        if (result_mant < 0x80) {
            while (!(result_mant & 0x80)) {
                result_mant <<= 1;
                if (--result_exp <= 0)
                    return BF16_ZERO();
            }
        } else {
            uint32_t lz = clz(result_mant);
            for (unsigned i = 0; i < 32 - lz - 8; i++) {
                result_mant >>= 1;
                if (++result_exp >= 255)
                    return BF16_NAN();
            }
        }
    }
    return (bf16_t) {
        .bits =
            result_sign << 15 | (result_exp & 0xFF) << 7 | result_mant & 0x7F,
    };
}

static inline bf16_t bf16_sub(bf16_t a, bf16_t b)
{
    b.bits ^= 0x8000U;
    return bf16_add(a, b);
}

static inline bf16_t bf16_mul(bf16_t a, bf16_t b)
{
    uint16_t sign_a = (a.bits >> 15) & 1;
    uint16_t sign_b = (b.bits >> 15) & 1;
    int16_t exp_a = ((a.bits >> 7) & 0xFF);
    int16_t exp_b = ((b.bits >> 7) & 0xFF);
    uint16_t mant_a = a.bits & 0x7F;
    uint16_t mant_b = b.bits & 0x7F;

    uint16_t result_sign = sign_a ^ sign_b;

    if (exp_a == 0xFF) {
        if (mant_a)
            return a;
        if (!exp_b && !mant_b)
            return BF16_NAN();
        return (bf16_t) {.bits = (result_sign << 15) | 0x7F80};
    }
    if (exp_b == 0xFF) {
        if (mant_b)
            return b;
        if (!exp_a && !mant_a)
            return BF16_NAN();
        return (bf16_t) {.bits = (result_sign << 15) | 0x7F80};
    }
    if ((!exp_a && !mant_a) || (!exp_b && !mant_b))
        return (bf16_t) {.bits = result_sign << 15};

    int16_t exp_adjust = 0;
    if (!exp_a) {
        while (!(mant_a & 0x80)) {
            mant_a <<= 1;
            exp_adjust--;
        }
        exp_a = 1;
    } else
        mant_a |= 0x80;
    if (!exp_b) {
        while (!(mant_b & 0x80)) {
            mant_b <<= 1;
            exp_adjust--;
        }
        exp_b = 1;
    } else
        mant_b |= 0x80;

    uint32_t result_mant = (uint32_t) mant_a * mant_b;
    int32_t result_exp = (int32_t) exp_a + exp_b - BF16_EXP_BIAS + exp_adjust;

    if (result_mant & 0x8000) {
        result_mant = (result_mant >> 8) & 0x7F;
        result_exp++;
    } else
        result_mant = (result_mant >> 7) & 0x7F;

    if (result_exp >= 0xFF)
        return (bf16_t) {.bits = (result_sign << 15) | 0x7F80};
    if (result_exp <= 0) {
        if (result_exp < -6)
            return (bf16_t) {.bits = result_sign << 15};
        result_mant >>= (1 - result_exp);
        result_exp = 0;
    }

    return (bf16_t) {.bits = (result_sign << 15) | ((result_exp & 0xFF) << 7) |
                             (result_mant & 0x7F)};
}

static inline bf16_t bf16_div(bf16_t a, bf16_t b)
{
    uint16_t sign_a = (a.bits >> 15) & 1;
    uint16_t sign_b = (b.bits >> 15) & 1;
    int16_t exp_a = ((a.bits >> 7) & 0xFF);
    int16_t exp_b = ((b.bits >> 7) & 0xFF);
    uint16_t mant_a = a.bits & 0x7F;
    uint16_t mant_b = b.bits & 0x7F;

    uint16_t result_sign = sign_a ^ sign_b;

    if (exp_b == 0xFF) {
        if (mant_b)
            return b;
        /* Inf/Inf = NaN */
        if (exp_a == 0xFF && !mant_a)
            return BF16_NAN();
        return (bf16_t) {.bits = result_sign << 15};
    }
    if (!exp_b && !mant_b) {
        if (!exp_a && !mant_a)
            return BF16_NAN();
        return (bf16_t) {.bits = (result_sign << 15) | 0x7F80};
    }
    if (exp_a == 0xFF) {
        if (mant_a)
            return a;
        return (bf16_t) {.bits = (result_sign << 15) | 0x7F80};
    }
    if (!exp_a && !mant_a)
        return (bf16_t) {.bits = result_sign << 15};

    if (exp_a)
        mant_a |= 0x80;
    if (exp_b)
        mant_b |= 0x80;

    uint32_t dividend = (uint32_t) mant_a << 15;
    uint32_t divisor = mant_b;
    uint32_t quotient = 0;

    for (int i = 0; i < 16; i++) {
        quotient <<= 1;
        if (dividend >= (divisor << (15 - i))) {
            dividend -= (divisor << (15 - i));
            quotient |= 1;
        }
    }

    int32_t result_exp = (int32_t) exp_a - exp_b + BF16_EXP_BIAS;

    if (!exp_a)
        result_exp--;
    if (!exp_b)
        result_exp++;

    if (quotient & 0x8000)
        quotient >>= 8;
    else {
        while (!(quotient & 0x8000) && result_exp > 1) {
            quotient <<= 1;
            result_exp--;
        }
        quotient >>= 8;
    }
    quotient &= 0x7F;

    if (result_exp >= 0xFF)
        return (bf16_t) {.bits = (result_sign << 15) | 0x7F80};
    if (result_exp <= 0)
        return (bf16_t) {.bits = result_sign << 15};
    return (bf16_t) {.bits = (result_sign << 15) | ((result_exp & 0xFF) << 7) |
                             (quotient & 0x7F)};
}

static void test_bf16_add(void)
{
    /* 1.0 + 1.0 = 2.0 */
    bf16_t a = {.bits = 0x3F80}; /* 1.0 */
    bf16_t b = {.bits = 0x3F80}; /* 1.0 */
    bf16_t result = bf16_add(a, b);

    /* Expected: 0x4000 (2.0) */
    volatile int* output_addr = (volatile int*)(0x2000);
    *output_addr = result.bits;
}

static void test_bf16_sub(void)
{
    /* 3.0 - 2.0 = 1.0 */
    bf16_t a = {.bits = 0x4040}; /* 3.0 */
    bf16_t b = {.bits = 0x4000}; /* 2.0 */
    bf16_t result = bf16_sub(a, b);

    /* Expected: 0x3F80 (1.0) */
    volatile int* output_addr = (volatile int*)(0x2004);
    *output_addr = result.bits;
}

static void test_bf16_mul(void)
{
    /* 2.0 * 3.0 = 6.0 */
    bf16_t a = {.bits = 0x4000}; /* 2.0 */
    bf16_t b = {.bits = 0x4040}; /* 3.0 */
    bf16_t result = bf16_mul(a, b);

    /* Expected: 0x40C0 (6.0) */
    volatile int* output_addr = (volatile int*)(0x2008);
    *output_addr = result.bits;
}

static void test_bf16_div(void)
{
    /* 6.0 / 2.0 = 3.0 */
    bf16_t a = {.bits = 0x40C0}; /* 6.0 */
    bf16_t b = {.bits = 0x4000}; /* 2.0 */
    bf16_t result = bf16_div(a, b);
  
    /* Expected: 0x4040 (3.0) */
    volatile int* output_addr = (volatile int*)(0x200C);
    *output_addr = result.bits;
}

static void test_bf16_special_cases_zero(void)
{
    /* Test zero */
    bf16_t zero = BF16_ZERO();
    volatile int* output_addr = (volatile int*)(0x2010);
    *output_addr = bf16_iszero(zero);
}

static void test_bf16_special_cases_nan(void)
{
    /* Test NaN */
    bf16_t nan = BF16_NAN();
    volatile int* output_addr = (volatile int*)(0x2014);
    *output_addr = bf16_isnan(nan);
}

static void test_bf16_special_cases_inf(void)
{
    /* Test infinity */
    bf16_t inf = {.bits = 0x7F80};
    volatile int* output_addr = (volatile int*)(0x2018);
    *output_addr = bf16_isinf(inf);
}
int main(void)
{
    /* Test 1: Addition */
    test_bf16_add();

    /* Test 2: Subtraction */
    test_bf16_sub();

    /* Test 3: Multiplication */
    test_bf16_mul();
  
    /* Test 4: Division */
    test_bf16_div();
    
    /* Test 5: Special cases */
    test_bf16_special_cases_zero();
    test_bf16_special_cases_nan();
    test_bf16_special_cases_inf();

    return 0;
}
