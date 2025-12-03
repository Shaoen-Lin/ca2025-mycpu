#include <stdint.h>


#define RESULT_BASE_ADDR ((volatile int*)0x2000) 

#define NULL ((void*)0)

int* singleNumber(int* nums, int numsSize, int* returnSize) {
    unsigned int xor_sum = 0;
    
    for (int i = 0; i < numsSize; i++) {
        xor_sum ^= nums[i];
    }
    
    unsigned int diff = xor_sum & (-(unsigned int)xor_sum);

    static int result[2]; 
    result[0] = 0;
    result[1] = 0;

    for (int i = 0; i < numsSize; i++) {
        if ((nums[i] & diff) == 0) {
            result[0] ^= nums[i]; 
        } else {
            result[1] ^= nums[i]; 
        }
    }
    
    *returnSize = 2;
    return result;
}

int main() {
    int returnSize;
    int *res;
    volatile int* output_ptr = RESULT_BASE_ADDR;

    // === Test Case 1:  ===
    int nums1[] = {2, 2, 3, 3, 4, 4, 0, 1, 100, 100, 99, 99};
    res = singleNumber(nums1, 12, &returnSize);
    output_ptr[0] = res[0];
    output_ptr[1] = res[1];

    // === Test Case 2: ===
    int nums2[] = {101, 17, 102, 102, -98, 0, 1, 101, 0, 1, 99, -98, 100, 17};
    res = singleNumber(nums2, 14, &returnSize);
    output_ptr[2] = res[0];
    output_ptr[3] = res[1];

    // === Test Case 3: ===
    int nums3[] = {-2, -2, -2, 2, 2, 2, -6, -9, -2, 2, 2, -5, 2, -6, -2, -10, -11, -10, -11, -2, -6, -9};
    res = singleNumber(nums3, 22, &returnSize);
    output_ptr[4] = res[0];
    output_ptr[5] = res[1];

    while(1);
    return 0;
}
