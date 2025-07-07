#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <stdint.h>
#include <omp.h>
#include <arm_neon.h>

// gcc-14 -O3 -fopenmp ./scripts/gemm/gemm-test.c -ffast-math -fopenmp -march=native -o ./caten-gemm

#define boolean _Bool
#define _infinity INFINITY
#define _negative_infinity -INFINITY
#define _nan NAN
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

// collapse(2) ==> +10GFLOPs
// BLOCKSIZE == 64 --> 180 GFLOPS
#define BLOCKSIZE 64 // Note: PARAMETER
void gemm(int N, float* val_9, const float* restrict val_4, const float* restrict val_6) {
  static float _val_6_s[BLOCKSIZE * BLOCKSIZE] __attribute__((aligned(64)));
  static float _val_4_s[BLOCKSIZE * BLOCKSIZE] __attribute__((aligned(64)));
  static float _val_9_s[BLOCKSIZE * BLOCKSIZE] __attribute__((aligned(64)));
  #pragma omp parallel for private(_val_6_s, _val_4_s) collapse(2)
  for (int _gid0=0; _gid0<N; _gid0+=BLOCKSIZE) {
    for (int _gid1=0; _gid1<N; _gid1+=BLOCKSIZE) {
      for (int _gid2_1=0; _gid2_1<BLOCKSIZE; _gid2_1++) {
          for (int _gid3_1=0; _gid3_1<BLOCKSIZE; _gid3_1++) {
            (*(_val_9_s+(BLOCKSIZE*_gid2_1)+_gid3_1)) = 0.0;
          }
        }
      for (int _gid4=0; _gid4<N; _gid4+=BLOCKSIZE) {
        for (int _gid2_1=0; _gid2_1<BLOCKSIZE; _gid2_1+=1) {
          for (int _gid5_1=0; _gid5_1<BLOCKSIZE; _gid5_1+=1) {
            (*(_val_4_s+((BLOCKSIZE*_gid2_1)+_gid5_1))) = (*(val_4+((N*(_gid0+_gid2_1))+(_gid4+_gid5_1))));
          }
        }
        for (int _gid3_1=0; _gid3_1<BLOCKSIZE; _gid3_1+=1) {
          for (int _gid5_1=0; _gid5_1<BLOCKSIZE; _gid5_1+=1) {
            (*(_val_6_s+((BLOCKSIZE*_gid3_1)+_gid5_1))) = (*(val_6+((_gid1+_gid3_1)+(N*(_gid4+_gid5_1)))));
          }
        }
        for (int _gid2=0; _gid2<BLOCKSIZE; _gid2+=1) {
          for (int _gid3=0; _gid3<BLOCKSIZE; _gid3+=1) {
            float val_2_0_0_0 = (*(_val_9_s+((BLOCKSIZE*_gid2)+(_gid3))));
            for (int _gid5=0; _gid5<BLOCKSIZE; _gid5+=1) {
              val_2_0_0_0 = (val_2_0_0_0+((*(_val_4_s+((BLOCKSIZE*_gid2)+_gid5)))*(*(_val_6_s+((BLOCKSIZE*_gid3)+_gid5)))));
            }
            (*(_val_9_s+((BLOCKSIZE*_gid2)+_gid3))) = val_2_0_0_0;
          }
        }
      }
    }
  }
}

void gemm_naive (int N, float* val_9, float* val_4, float* val_6) {
  #pragma omp parallel for
  for (int _gid0=0; _gid0<N; _gid0+=1) {
    for (int _gid1=0; _gid1<N; _gid1+=1) {
      for (int _gid2=0; _gid2<N; _gid2+=1) {
        (*(val_9+((N*_gid0)+_gid1))) += ((*(val_4+((N*_gid0)+_gid2)))*(*(val_6+(_gid1+(N*_gid2)))));
      }
    }
  }
}

int main() {
  printf("OMP_GET_MAX_THREADS=%d\n", omp_get_max_threads());
  printf("BLOCKSIZE=%d\n", BLOCKSIZE);
  int n_sample = 10;
  for (int size = 256; size < 8094; size+=256) {
    int M = size;
    int N = size;
    int K = size;
    float *A = (float*)malloc(M * N * sizeof(float));
    float *B = (float*)malloc(N * K * sizeof(float));
    float *C = (float*)malloc(M * K * sizeof(float));
    float *C1 = (float*)malloc(M * K * sizeof(float));
    for (int i = 0; i < M*N; i++) {
      A[i] = (float)(rand() % 100);
    }
  
    for (int i = 0; i < N*K; i++) {
      B[i] = (float)(rand() % 100);
    }
    gemm_naive(size, C1, A, B);
    gemm(size, C, A, B);

    float max_diff = 0.0;
    //for (int i =0; i<10; i++)  printf("%f %f\n", C[i], C1[i]);
  
    for (int i = 0; i < M*K; i++) {
      max_diff = max(max_diff, fabs(C[i] - C1[i]));
    }
    printf("Max diff: %f\n", max_diff); 

    struct timespec start, end;
    double elapsed;
    clock_gettime(CLOCK_MONOTONIC, &start);

    for (int s = 0; s < n_sample; s++) {
      gemm(size, C, A, B);
    }

    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = (end.tv_sec - start.tv_sec)  + (end.tv_nsec - start.tv_nsec) / 1000000000.0;

    double ops = 2.0 * M * N * K * n_sample;
    // GFLOPS = ops / (elapsed * 10^9)
    double gflops = ops / (elapsed * 1e9);
    printf("M=%d | Execution time (for %d samples): %f GFLOPS. (%f seconds elapsed)\n", M, n_sample, gflops, elapsed);    
    free(A);
    free(B);
    free(C);
  }
}
