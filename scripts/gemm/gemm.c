// gcc-14 -O3 -fopenmp gemm.c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <omp.h>

void gemm(int M, int N, int K,
          const float * __restrict A,
          const float * __restrict B,
          float * __restrict C)
{
    // C = A(M×N) * B(N×K)
    // i: 0 to M-1, j: 0 to K-1, sum over N
    #pragma omp parallel for
    for (int i = 0; i < M; i++) {
      for (int j = 0; j < K; j++) {
            float sum = 0.0f;
            for (int n = 0; n < N; n++) {
                sum += A[i*N + n] * B[n*K + j];
            }
            C[i*K + j] = sum;
        }
    }
}

int main() {
  printf("OMP_GET_MAX_THREADS=%d\n", omp_get_max_threads());
  for (int M = 1; M < 1000; M++){
    int N = 1000;
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
