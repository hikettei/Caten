// gcc-14 ./scripts/gemm/open_blas.c -O3 -I/opt/homebrew/opt/openblas/include -L/opt/homebrew/opt/openblas/lib -lopenblas -o ./openblas-gemm
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <cblas.h>

int main() {
  int n_sample = 10;
  for (int size = 256; size < 8094; size+=256) {
  
    int M = size;
    int N = size;
    int K = size;
    
    float *A = (float*)malloc(M * N * sizeof(float));
    float *B = (float*)malloc(N * K * sizeof(float));
    float *C = (float*)malloc(M * K * sizeof(float));
        
    for (int i = 0; i < M*N; i++) {
      A[i] = (float)(rand() % 100);
    }
    for (int i = 0; i < N*K; i++) {
      B[i] = (float)(rand() % 100);
    }
    for (int i = 0; i < M*K; i++){
      C[i] = 0.0f;
    }

    struct timespec start, end;
    double elapsed;
    clock_gettime(CLOCK_MONOTONIC, &start);
    for (int s = 0; s < n_sample; s++) {
      cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                  M, K, N,
                  1.0f,
                  A, N,
                  B, K,
                  1.0f,
                  C, K);
    }
    clock_gettime(CLOCK_MONOTONIC, &end);
    elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    double ops = 2.0 * M * N * K * n_sample;
    double gflops = ops / (elapsed * 1e9);
    printf("M=%d | Execution time (for %d samples): %f GFLOPS. (%f seconds elapsed)\n", M, n_sample, gflops, elapsed);

    free(A);
    free(B);
    free(C);
  }
    
  return 0;
}
