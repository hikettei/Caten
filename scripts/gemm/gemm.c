/*
  A playground for GEMM optimization.
  Compile:
    gcc-14 -O3 -fopenmp ./scripts/gemm/gemm.c -ffast-math -fopenmp -march=native
  Disassemble:
    gcc-14 -fopenmp -O3  -ffast-math -fopenmp -march=native -fopenmp-simd -fstrict-aliasing -ftree-vectorize -S ./scripts/gemm/gemm.c ./gemm.s
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <omp.h>
#include <arm_neon.h>
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))
// Optimized by AutoScheduler
#define NB 16
#define KB 16
// Implementations are copied from (caten (!matmul (make-tensor `(M N)) (make-tensor `(N K))))
// Supporting SIMD Intrinsic beyonds the compiler.
// 20~30 GFlops
void gemm_tmp(int M, int N, int K,
          const float * __restrict A,
          const float * __restrict B,
          float * __restrict C)
{
  
}


int main() {
  printf("OMP_GET_MAX_THREADS=%d\n", omp_get_max_threads());
  for (int M = 1; M < 100; M++){
    int N = 768;
    int K = 1024;
    int n_sample = 100;

    float *A = (float*)malloc(M * N * sizeof(float));
    float *B = (float*)malloc(N * K * sizeof(float));
    float *C = (float*)malloc(M * K * sizeof(float));
    for (int i = 0; i < M*N; i++) {
      A[i] = 1.0f;
    }
    for (int i = 0; i < N*K; i++) {
      B[i] = 1.0f;
    }

    struct timespec start, end;
    double elapsed;
    clock_gettime(CLOCK_MONOTONIC, &start);

    for (int s = 0; s < n_sample; s++) {
      gemm(M, N, K, A, B, C);
    }

    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = (end.tv_sec - start.tv_sec)
      + (end.tv_nsec - start.tv_nsec) / 1000000000.0;

    double ops = 2.0 * M * N * K * n_sample;
    // GFLOPS = ops / (elapsed * 10^9)
    double gflops = ops / (elapsed * 1e9);
    printf("M=%d | Execution time (for %d samples): %f GFLOPS\n", M, n_sample, gflops);

    free(A);
    free(B);
    free(C);
  }
  return 0;
}
