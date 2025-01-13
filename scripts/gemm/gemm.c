/*
  A playground for GEMM optimization.
  Compile:
    gcc-14 -O3 -fopenmp ./scripts/gemm/gemm.c -ffast-math -fopenmp -march=native -I/opt/homebrew/opt/openblas/include -L/opt/homebrew/opt/openblas/lib -lopenblas
  Disassemble:
    gcc-14 -fopenmp -O3  -ffast-math -fopenmp -march=native -fopenmp-simd -fstrict-aliasing -ftree-vectorize -S ./scripts/gemm/gemm.c ./gemm.s
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <omp.h>
#include <arm_neon.h>
#include <cblas.h>

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))
// Optimized by AutoScheduler
#define NB 16
#define KB 16
// 
static inline void gemm4x4(const float *a, float *b, float *c1, float *c2, float *c3, float *c4, int nr)
{
  for (int k=0; k<4; k++) {
    float a0 = *(a+k);
    float* b0 = (b+k*nr);
    *c1 += a0 * *(b0+0); *c2 += a0 * *(b0+1); *c3 += a0 * *(b0+2); *c4 += a0 * *(b0+3);
  }
}

void gemm(int M, int N, int K,
          const float * __restrict A,
          const float * __restrict B,
          float * __restrict C)
{
  #pragma omp parallel for
  for (int _gid0=0;_gid0<512;_gid0+=1) {
    for (int _gid2=0;_gid2<512;_gid2+=4) {
      float val_2_0 = 0.0;
      float val_2_1 = 0.0;
      float val_2_2 = 0.0;
      float val_2_3 = 0.0;
      for (int _gid4=0;(_gid4<512);_gid4+=4) {
        gemm4x4((float*)&A[((512*_gid0)+_gid4)], (float*)&B[(_gid2+(512*_gid4))], &val_2_0, &val_2_1, &val_2_2, &val_2_3, 512);
      }
      C[((512*_gid0)+_gid2)] = val_2_0;
      C[((512*_gid0)+(_gid2+1))] = val_2_1;
      C[((512*_gid0)+(_gid2+2))] = val_2_2;
      C[((512*_gid0)+(_gid2+3))] = val_2_3;
    }
  }
}

int main() {
  printf("OMP_GET_MAX_THREADS=%d\n", omp_get_max_threads());
  int M = 512;
  int N = 512;
  int K = 512;
  int n_sample = 300;

  float *A = (float*)malloc(M * N * sizeof(float));
  float *B = (float*)malloc(N * K * sizeof(float));
  float *C = (float*)malloc(M * K * sizeof(float));
  float *C_openblas = (float*)malloc(M * K * sizeof(float));
  for (int i = 0; i < M*N; i++) {
    A[i] = (float)i*1e-3;
  }
  for (int i = 0; i < N*K; i++) {
    B[i] = (float)i*1e-3;
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

  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, M, K, N, 1.0f, A, N, B, K, 1.0f, C_openblas, K);

  float atol = 0.0;
  for (int i = 0; i < M*K; i++) {
    atol += abs(C[i] - C_openblas[i]);
  }
  printf("ATOL=%f\n", atol);

  free(A);
  free(B);
  free(C);
  return 0;
}
