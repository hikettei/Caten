// gcc-14 ./scripts/gemm/search_tile.c -fopenmp -O3  -ffast-math -fopenmp -march=native -fopenmp-simd -fstrict-aliasing -ftree-vectorize
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <omp.h>
#include <arm_neon.h>
// Optimized by AutoScheduler
void gemm(int M, int N, int K,
          const float * __restrict A,
          const float * __restrict B,
          float * __restrict C, int NB, int KB)
{
#pragma omp parallel for collapse(2)
  for (int i = 0; i < M; i++) {
    for (int j0 = 0; j0 < K; j0 += KB) {
      int jMax = (j0 + KB < K) ? (j0 + KB) : K;
      int validK = jMax - j0;
      int kBlocks = validK / 16;
      int kRemain = validK % 16;
      for (int n0 = 0; n0 < N; n0 += NB) {
        int nMax = (n0 + NB < N) ? (n0 + NB) : N;
        for (int kb = 0; kb < kBlocks; kb++) {
          int jBase = j0 + kb*16;
          float32x4_t c_vec0 = vdupq_n_f32(0.0f);
          float32x4_t c_vec1 = vdupq_n_f32(0.0f);
          float32x4_t c_vec2 = vdupq_n_f32(0.0f);
          float32x4_t c_vec3 = vdupq_n_f32(0.0f);
          for (int n = n0; n < nMax; n++) {
            float a_val = A[i*N + n];
            float32x4_t b_vec0 = vld1q_f32(&B[n*K + jBase +  0]);
            float32x4_t b_vec1 = vld1q_f32(&B[n*K + jBase +  4]);
            float32x4_t b_vec2 = vld1q_f32(&B[n*K + jBase +  8]);
            float32x4_t b_vec3 = vld1q_f32(&B[n*K + jBase + 12]);
            float32x4_t a_vec = vdupq_n_f32(a_val);
            c_vec0 = vmlaq_f32(c_vec0, a_vec, b_vec0);
            c_vec1 = vmlaq_f32(c_vec1, a_vec, b_vec1);
            c_vec2 = vmlaq_f32(c_vec2, a_vec, b_vec2);
            c_vec3 = vmlaq_f32(c_vec3, a_vec, b_vec3);
          }
          float tmpC[16];
          vst1q_f32(&tmpC[0], c_vec0);
          vst1q_f32(&tmpC[4], c_vec1);
          vst1q_f32(&tmpC[8], c_vec2);
          vst1q_f32(&tmpC[12], c_vec3);
          for (int x = 0; x < 16; x++) {
            C[i*K + (jBase + x)] += tmpC[x];
          }
        }
        // Reminder
        for (int j = j0 + kBlocks*16; j < jMax; j++) {
          float sum = 0.0f;
          for (int n = n0; n < nMax; n++) {
            sum += A[i*N + n] * B[n*K + j];
          }
          C[i*K + j] += sum;
        }
      }
    }
  }
}

int main() {
  printf("OMP_GET_MAX_THREADS=%d\n", omp_get_max_threads());
  float max = 0.0f;
  int best_kb = 0;
  int best_mb = 0;
  for (int kb = 1; kb < 33; kb+=1){
    for (int mb = 1; mb < 33; mb+=1){
      int M = 10;
      int N = 768;
      int K = 1024;
      int n_sample = 10;

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
        gemm(M, N, K, A, B, C, kb, mb);
      }
      clock_gettime(CLOCK_MONOTONIC, &end);
      elapsed = (end.tv_sec - start.tv_sec)
        + (end.tv_nsec - start.tv_nsec) / 1000000000.0;

      double ops = 2.0 * M * N * K * n_sample;
      // GFLOPS = ops / (elapsed * 10^9)
      double gflops = ops / (elapsed * 1e9);
      if (gflops > max){
        max = gflops;
        best_kb = kb;
        best_mb = mb;
      }
      printf("(%d, %d) | Execution time (for %d samples): %f GFLOPS\n", kb, mb, n_sample, gflops);
      free(A);
      free(B);
      free(C);
    }
  }
  printf("Max GFLOPS: %f (MB, KB)=(%d, %d)\n", max, best_mb, best_kb);
}
