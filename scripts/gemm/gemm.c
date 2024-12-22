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

// Optimized by AutoScheduler
#define NB 16
#define KB 16
// [TODO] Can't we achieve 150 GFlops with only using gcc?
// Supporting SIMD Intrinsic beyonds the compiler.
// 20~30 GFlops
void gemm_naive_tailed(int M, int N, int K,
          const float * __restrict A,
          const float * __restrict B,
          float * __restrict C)
{
  // Note(hikettei) I didn't know loop collapse is effective for smaller M
  // Loop Interchange doesn't look effective than I expected, memory layouts have a big impact?
  #pragma omp parallel for collapse(2)
  for (int i = 0; i < M; i++) {
    for (int j0 = 0; j0 < K; j0 += KB) {
      int jMax = (j0 + KB < K) ? (j0 + KB) : K;
      for (int n0 = 0; n0 < N; n0 += NB) {
        int nMax = (n0 + NB < N) ? (n0 + NB) : N;
        for (int j = j0; j < jMax; j++) {
          float sum = 0.0f;
          #pragma omp simd reduction(+:sum)
          for (int n = n0; n < nMax; n++) {
            sum += A[i*N + n] * B[n*K + j];
          }
          C[i*K + j] += sum;
        }
      }
    }
  }
}

void gemm(const int64_t m, const int64_t n, const int64_t k, const float* restrict val_19, const float* restrict val_16, float* val_22) {
  for (int _gid0=0; _gid0<(m+-((m%4))); _gid0+=4) {
    for (int _gid2=0; _gid2<(k+-((k%4))); _gid2+=4) {
      float val_7_0_0 = 0.0;
      float val_7_1_0 = 0.0;
      float val_7_2_0 = 0.0;
      float val_7_3_0 = 0.0;
      float val_7_0_1 = 0.0;
      float val_7_1_1 = 0.0;
      float val_7_2_1 = 0.0;
      float val_7_3_1 = 0.0;
      float val_7_0_2 = 0.0;
      float val_7_1_2 = 0.0;
      float val_7_2_2 = 0.0;
      float val_7_3_2 = 0.0;
      float val_7_0_3 = 0.0;
      float val_7_1_3 = 0.0;
      float val_7_2_3 = 0.0;
      float val_7_3_3 = 0.0;
      for (int _gid4=0; _gid4<(n+-((n%4))); _gid4+=4) {
        val_7_0_0 = (val_7_0_0+(val_16[((n*_gid0)+_gid4)]*val_19[(_gid2+(k*_gid4))]));
        val_7_1_0 = (val_7_1_0+(val_16[((n*(_gid0+1))+_gid4)]*val_19[(_gid2+(k*_gid4))]));
        val_7_2_0 = (val_7_2_0+(val_16[((n*(_gid0+2))+_gid4)]*val_19[(_gid2+(k*_gid4))]));
        val_7_3_0 = (val_7_3_0+(val_16[((n*(_gid0+3))+_gid4)]*val_19[(_gid2+(k*_gid4))]));
        val_7_0_1 = (val_7_0_1+(val_16[((n*_gid0)+_gid4)]*val_19[((_gid2+1)+(k*_gid4))]));
        val_7_1_1 = (val_7_1_1+(val_16[((n*(_gid0+1))+_gid4)]*val_19[((_gid2+1)+(k*_gid4))]));
        val_7_2_1 = (val_7_2_1+(val_16[((n*(_gid0+2))+_gid4)]*val_19[((_gid2+1)+(k*_gid4))]));
        val_7_3_1 = (val_7_3_1+(val_16[((n*(_gid0+3))+_gid4)]*val_19[((_gid2+1)+(k*_gid4))]));
        val_7_0_2 = (val_7_0_2+(val_16[((n*_gid0)+_gid4)]*val_19[((_gid2+2)+(k*_gid4))]));
        val_7_1_2 = (val_7_1_2+(val_16[((n*(_gid0+1))+_gid4)]*val_19[((_gid2+2)+(k*_gid4))]));
        val_7_2_2 = (val_7_2_2+(val_16[((n*(_gid0+2))+_gid4)]*val_19[((_gid2+2)+(k*_gid4))]));
        val_7_3_2 = (val_7_3_2+(val_16[((n*(_gid0+3))+_gid4)]*val_19[((_gid2+2)+(k*_gid4))]));
        val_7_0_3 = (val_7_0_3+(val_16[((n*_gid0)+_gid4)]*val_19[((_gid2+3)+(k*_gid4))]));
        val_7_1_3 = (val_7_1_3+(val_16[((n*(_gid0+1))+_gid4)]*val_19[((_gid2+3)+(k*_gid4))]));
        val_7_2_3 = (val_7_2_3+(val_16[((n*(_gid0+2))+_gid4)]*val_19[((_gid2+3)+(k*_gid4))]));
        val_7_3_3 = (val_7_3_3+(val_16[((n*(_gid0+3))+_gid4)]*val_19[((_gid2+3)+(k*_gid4))]));
        val_7_0_0 = (val_7_0_0+(val_16[((n*_gid0)+(_gid4+1))]*val_19[(_gid2+(k*(_gid4+1)))]));
        val_7_1_0 = (val_7_1_0+(val_16[((n*(_gid0+1))+(_gid4+1))]*val_19[(_gid2+(k*(_gid4+1)))]));
        val_7_2_0 = (val_7_2_0+(val_16[((n*(_gid0+2))+(_gid4+1))]*val_19[(_gid2+(k*(_gid4+1)))]));
        val_7_3_0 = (val_7_3_0+(val_16[((n*(_gid0+3))+(_gid4+1))]*val_19[(_gid2+(k*(_gid4+1)))]));
        val_7_0_1 = (val_7_0_1+(val_16[((n*_gid0)+(_gid4+1))]*val_19[((_gid2+1)+(k*(_gid4+1)))]));
        val_7_1_1 = (val_7_1_1+(val_16[((n*(_gid0+1))+(_gid4+1))]*val_19[((_gid2+1)+(k*(_gid4+1)))]));
        val_7_2_1 = (val_7_2_1+(val_16[((n*(_gid0+2))+(_gid4+1))]*val_19[((_gid2+1)+(k*(_gid4+1)))]));
        val_7_3_1 = (val_7_3_1+(val_16[((n*(_gid0+3))+(_gid4+1))]*val_19[((_gid2+1)+(k*(_gid4+1)))]));
        val_7_0_2 = (val_7_0_2+(val_16[((n*_gid0)+(_gid4+1))]*val_19[((_gid2+2)+(k*(_gid4+1)))]));
        val_7_1_2 = (val_7_1_2+(val_16[((n*(_gid0+1))+(_gid4+1))]*val_19[((_gid2+2)+(k*(_gid4+1)))]));
        val_7_2_2 = (val_7_2_2+(val_16[((n*(_gid0+2))+(_gid4+1))]*val_19[((_gid2+2)+(k*(_gid4+1)))]));
        val_7_3_2 = (val_7_3_2+(val_16[((n*(_gid0+3))+(_gid4+1))]*val_19[((_gid2+2)+(k*(_gid4+1)))]));
        val_7_0_3 = (val_7_0_3+(val_16[((n*_gid0)+(_gid4+1))]*val_19[((_gid2+3)+(k*(_gid4+1)))]));
        val_7_1_3 = (val_7_1_3+(val_16[((n*(_gid0+1))+(_gid4+1))]*val_19[((_gid2+3)+(k*(_gid4+1)))]));
        val_7_2_3 = (val_7_2_3+(val_16[((n*(_gid0+2))+(_gid4+1))]*val_19[((_gid2+3)+(k*(_gid4+1)))]));
        val_7_3_3 = (val_7_3_3+(val_16[((n*(_gid0+3))+(_gid4+1))]*val_19[((_gid2+3)+(k*(_gid4+1)))]));
        val_7_0_0 = (val_7_0_0+(val_16[((n*_gid0)+(_gid4+2))]*val_19[(_gid2+(k*(_gid4+2)))]));
        val_7_1_0 = (val_7_1_0+(val_16[((n*(_gid0+1))+(_gid4+2))]*val_19[(_gid2+(k*(_gid4+2)))]));
        val_7_2_0 = (val_7_2_0+(val_16[((n*(_gid0+2))+(_gid4+2))]*val_19[(_gid2+(k*(_gid4+2)))]));
        val_7_3_0 = (val_7_3_0+(val_16[((n*(_gid0+3))+(_gid4+2))]*val_19[(_gid2+(k*(_gid4+2)))]));
        val_7_0_1 = (val_7_0_1+(val_16[((n*_gid0)+(_gid4+2))]*val_19[((_gid2+1)+(k*(_gid4+2)))]));
        val_7_1_1 = (val_7_1_1+(val_16[((n*(_gid0+1))+(_gid4+2))]*val_19[((_gid2+1)+(k*(_gid4+2)))]));
        val_7_2_1 = (val_7_2_1+(val_16[((n*(_gid0+2))+(_gid4+2))]*val_19[((_gid2+1)+(k*(_gid4+2)))]));
        val_7_3_1 = (val_7_3_1+(val_16[((n*(_gid0+3))+(_gid4+2))]*val_19[((_gid2+1)+(k*(_gid4+2)))]));
        val_7_0_2 = (val_7_0_2+(val_16[((n*_gid0)+(_gid4+2))]*val_19[((_gid2+2)+(k*(_gid4+2)))]));
        val_7_1_2 = (val_7_1_2+(val_16[((n*(_gid0+1))+(_gid4+2))]*val_19[((_gid2+2)+(k*(_gid4+2)))]));
        val_7_2_2 = (val_7_2_2+(val_16[((n*(_gid0+2))+(_gid4+2))]*val_19[((_gid2+2)+(k*(_gid4+2)))]));
        val_7_3_2 = (val_7_3_2+(val_16[((n*(_gid0+3))+(_gid4+2))]*val_19[((_gid2+2)+(k*(_gid4+2)))]));
        val_7_0_3 = (val_7_0_3+(val_16[((n*_gid0)+(_gid4+2))]*val_19[((_gid2+3)+(k*(_gid4+2)))]));
        val_7_1_3 = (val_7_1_3+(val_16[((n*(_gid0+1))+(_gid4+2))]*val_19[((_gid2+3)+(k*(_gid4+2)))]));
        val_7_2_3 = (val_7_2_3+(val_16[((n*(_gid0+2))+(_gid4+2))]*val_19[((_gid2+3)+(k*(_gid4+2)))]));
        val_7_3_3 = (val_7_3_3+(val_16[((n*(_gid0+3))+(_gid4+2))]*val_19[((_gid2+3)+(k*(_gid4+2)))]));
        val_7_0_0 = (val_7_0_0+(val_16[((n*_gid0)+(_gid4+3))]*val_19[(_gid2+(k*(_gid4+3)))]));
        val_7_1_0 = (val_7_1_0+(val_16[((n*(_gid0+1))+(_gid4+3))]*val_19[(_gid2+(k*(_gid4+3)))]));
        val_7_2_0 = (val_7_2_0+(val_16[((n*(_gid0+2))+(_gid4+3))]*val_19[(_gid2+(k*(_gid4+3)))]));
        val_7_3_0 = (val_7_3_0+(val_16[((n*(_gid0+3))+(_gid4+3))]*val_19[(_gid2+(k*(_gid4+3)))]));
        val_7_0_1 = (val_7_0_1+(val_16[((n*_gid0)+(_gid4+3))]*val_19[((_gid2+1)+(k*(_gid4+3)))]));
        val_7_1_1 = (val_7_1_1+(val_16[((n*(_gid0+1))+(_gid4+3))]*val_19[((_gid2+1)+(k*(_gid4+3)))]));
        val_7_2_1 = (val_7_2_1+(val_16[((n*(_gid0+2))+(_gid4+3))]*val_19[((_gid2+1)+(k*(_gid4+3)))]));
        val_7_3_1 = (val_7_3_1+(val_16[((n*(_gid0+3))+(_gid4+3))]*val_19[((_gid2+1)+(k*(_gid4+3)))]));
        val_7_0_2 = (val_7_0_2+(val_16[((n*_gid0)+(_gid4+3))]*val_19[((_gid2+2)+(k*(_gid4+3)))]));
        val_7_1_2 = (val_7_1_2+(val_16[((n*(_gid0+1))+(_gid4+3))]*val_19[((_gid2+2)+(k*(_gid4+3)))]));
        val_7_2_2 = (val_7_2_2+(val_16[((n*(_gid0+2))+(_gid4+3))]*val_19[((_gid2+2)+(k*(_gid4+3)))]));
        val_7_3_2 = (val_7_3_2+(val_16[((n*(_gid0+3))+(_gid4+3))]*val_19[((_gid2+2)+(k*(_gid4+3)))]));
        val_7_0_3 = (val_7_0_3+(val_16[((n*_gid0)+(_gid4+3))]*val_19[((_gid2+3)+(k*(_gid4+3)))]));
        val_7_1_3 = (val_7_1_3+(val_16[((n*(_gid0+1))+(_gid4+3))]*val_19[((_gid2+3)+(k*(_gid4+3)))]));
        val_7_2_3 = (val_7_2_3+(val_16[((n*(_gid0+2))+(_gid4+3))]*val_19[((_gid2+3)+(k*(_gid4+3)))]));
        val_7_3_3 = (val_7_3_3+(val_16[((n*(_gid0+3))+(_gid4+3))]*val_19[((_gid2+3)+(k*(_gid4+3)))]));
      }
      for (int _gid5=(n+-((n%4))); _gid5<n; _gid5+=1) {
        val_7_0_0 = (val_7_0_0+(val_16[((n*_gid0)+_gid5)]*val_19[(_gid2+(k*_gid5))]));
        val_7_1_0 = (val_7_1_0+(val_16[((n*(_gid0+1))+_gid5)]*val_19[(_gid2+(k*_gid5))]));
        val_7_2_0 = (val_7_2_0+(val_16[((n*(_gid0+2))+_gid5)]*val_19[(_gid2+(k*_gid5))]));
        val_7_3_0 = (val_7_3_0+(val_16[((n*(_gid0+3))+_gid5)]*val_19[(_gid2+(k*_gid5))]));
        val_7_0_1 = (val_7_0_1+(val_16[((n*_gid0)+_gid5)]*val_19[((_gid2+1)+(k*_gid5))]));
        val_7_1_1 = (val_7_1_1+(val_16[((n*(_gid0+1))+_gid5)]*val_19[((_gid2+1)+(k*_gid5))]));
        val_7_2_1 = (val_7_2_1+(val_16[((n*(_gid0+2))+_gid5)]*val_19[((_gid2+1)+(k*_gid5))]));
        val_7_3_1 = (val_7_3_1+(val_16[((n*(_gid0+3))+_gid5)]*val_19[((_gid2+1)+(k*_gid5))]));
        val_7_0_2 = (val_7_0_2+(val_16[((n*_gid0)+_gid5)]*val_19[((_gid2+2)+(k*_gid5))]));
        val_7_1_2 = (val_7_1_2+(val_16[((n*(_gid0+1))+_gid5)]*val_19[((_gid2+2)+(k*_gid5))]));
        val_7_2_2 = (val_7_2_2+(val_16[((n*(_gid0+2))+_gid5)]*val_19[((_gid2+2)+(k*_gid5))]));
        val_7_3_2 = (val_7_3_2+(val_16[((n*(_gid0+3))+_gid5)]*val_19[((_gid2+2)+(k*_gid5))]));
        val_7_0_3 = (val_7_0_3+(val_16[((n*_gid0)+_gid5)]*val_19[((_gid2+3)+(k*_gid5))]));
        val_7_1_3 = (val_7_1_3+(val_16[((n*(_gid0+1))+_gid5)]*val_19[((_gid2+3)+(k*_gid5))]));
        val_7_2_3 = (val_7_2_3+(val_16[((n*(_gid0+2))+_gid5)]*val_19[((_gid2+3)+(k*_gid5))]));
        val_7_3_3 = (val_7_3_3+(val_16[((n*(_gid0+3))+_gid5)]*val_19[((_gid2+3)+(k*_gid5))]));
      }
      val_22[((k*_gid0)+_gid2)] = val_7_0_0;
      val_22[((k*(_gid0+1))+_gid2)] = val_7_1_0;
      val_22[((k*(_gid0+2))+_gid2)] = val_7_2_0;
      val_22[((k*(_gid0+3))+_gid2)] = val_7_3_0;
      val_22[((k*_gid0)+(_gid2+1))] = val_7_0_1;
      val_22[((k*(_gid0+1))+(_gid2+1))] = val_7_1_1;
      val_22[((k*(_gid0+2))+(_gid2+1))] = val_7_2_1;
      val_22[((k*(_gid0+3))+(_gid2+1))] = val_7_3_1;
      val_22[((k*_gid0)+(_gid2+2))] = val_7_0_2;
      val_22[((k*(_gid0+1))+(_gid2+2))] = val_7_1_2;
      val_22[((k*(_gid0+2))+(_gid2+2))] = val_7_2_2;
      val_22[((k*(_gid0+3))+(_gid2+2))] = val_7_3_2;
      val_22[((k*_gid0)+(_gid2+3))] = val_7_0_3;
      val_22[((k*(_gid0+1))+(_gid2+3))] = val_7_1_3;
      val_22[((k*(_gid0+2))+(_gid2+3))] = val_7_2_3;
      val_22[((k*(_gid0+3))+(_gid2+3))] = val_7_3_3;
    }
    for (int _gid3=(k+-((k%4))); _gid3<k; _gid3+=1) {
      float val_7_0 = 0.0;
      float val_7_1 = 0.0;
      float val_7_2 = 0.0;
      float val_7_3 = 0.0;
      for (int _gid4=0; _gid4<(n+-((n%4))); _gid4+=4) {
        val_7_0 = (val_7_0+(val_16[((n*_gid0)+_gid4)]*val_19[(_gid3+(k*_gid4))]));
        val_7_1 = (val_7_1+(val_16[((n*(_gid0+1))+_gid4)]*val_19[(_gid3+(k*_gid4))]));
        val_7_2 = (val_7_2+(val_16[((n*(_gid0+2))+_gid4)]*val_19[(_gid3+(k*_gid4))]));
        val_7_3 = (val_7_3+(val_16[((n*(_gid0+3))+_gid4)]*val_19[(_gid3+(k*_gid4))]));
        val_7_0 = (val_7_0+(val_16[((n*_gid0)+(_gid4+1))]*val_19[(_gid3+(k*(_gid4+1)))]));
        val_7_1 = (val_7_1+(val_16[((n*(_gid0+1))+(_gid4+1))]*val_19[(_gid3+(k*(_gid4+1)))]));
        val_7_2 = (val_7_2+(val_16[((n*(_gid0+2))+(_gid4+1))]*val_19[(_gid3+(k*(_gid4+1)))]));
        val_7_3 = (val_7_3+(val_16[((n*(_gid0+3))+(_gid4+1))]*val_19[(_gid3+(k*(_gid4+1)))]));
        val_7_0 = (val_7_0+(val_16[((n*_gid0)+(_gid4+2))]*val_19[(_gid3+(k*(_gid4+2)))]));
        val_7_1 = (val_7_1+(val_16[((n*(_gid0+1))+(_gid4+2))]*val_19[(_gid3+(k*(_gid4+2)))]));
        val_7_2 = (val_7_2+(val_16[((n*(_gid0+2))+(_gid4+2))]*val_19[(_gid3+(k*(_gid4+2)))]));
        val_7_3 = (val_7_3+(val_16[((n*(_gid0+3))+(_gid4+2))]*val_19[(_gid3+(k*(_gid4+2)))]));
        val_7_0 = (val_7_0+(val_16[((n*_gid0)+(_gid4+3))]*val_19[(_gid3+(k*(_gid4+3)))]));
        val_7_1 = (val_7_1+(val_16[((n*(_gid0+1))+(_gid4+3))]*val_19[(_gid3+(k*(_gid4+3)))]));
        val_7_2 = (val_7_2+(val_16[((n*(_gid0+2))+(_gid4+3))]*val_19[(_gid3+(k*(_gid4+3)))]));
        val_7_3 = (val_7_3+(val_16[((n*(_gid0+3))+(_gid4+3))]*val_19[(_gid3+(k*(_gid4+3)))]));
      }
      for (int _gid5=(n+-((n%4))); _gid5<n; _gid5+=1) {
        val_7_0 = (val_7_0+(val_16[((n*_gid0)+_gid5)]*val_19[(_gid3+(k*_gid5))]));
        val_7_1 = (val_7_1+(val_16[((n*(_gid0+1))+_gid5)]*val_19[(_gid3+(k*_gid5))]));
        val_7_2 = (val_7_2+(val_16[((n*(_gid0+2))+_gid5)]*val_19[(_gid3+(k*_gid5))]));
        val_7_3 = (val_7_3+(val_16[((n*(_gid0+3))+_gid5)]*val_19[(_gid3+(k*_gid5))]));
      }
      val_22[((k*_gid0)+_gid3)] = val_7_0;
      val_22[((k*(_gid0+1))+_gid3)] = val_7_1;
      val_22[((k*(_gid0+2))+_gid3)] = val_7_2;
      val_22[((k*(_gid0+3))+_gid3)] = val_7_3;
    }
  }
  for (int _gid1=(m+-((m%4))); _gid1<m; _gid1+=1) {
    for (int _gid2=0; _gid2<(k+-((k%4))); _gid2+=4) {
      float val_7_0 = 0.0;
      float val_7_1 = 0.0;
      float val_7_2 = 0.0;
      float val_7_3 = 0.0;
      for (int _gid4=0; _gid4<(n+-((n%4))); _gid4+=4) {
        val_7_0 = (val_7_0+(val_16[((n*_gid1)+_gid4)]*val_19[(_gid2+(k*_gid4))]));
        val_7_1 = (val_7_1+(val_16[((n*_gid1)+_gid4)]*val_19[((_gid2+1)+(k*_gid4))]));
        val_7_2 = (val_7_2+(val_16[((n*_gid1)+_gid4)]*val_19[((_gid2+2)+(k*_gid4))]));
        val_7_3 = (val_7_3+(val_16[((n*_gid1)+_gid4)]*val_19[((_gid2+3)+(k*_gid4))]));
        val_7_0 = (val_7_0+(val_16[((n*_gid1)+(_gid4+1))]*val_19[(_gid2+(k*(_gid4+1)))]));
        val_7_1 = (val_7_1+(val_16[((n*_gid1)+(_gid4+1))]*val_19[((_gid2+1)+(k*(_gid4+1)))]));
        val_7_2 = (val_7_2+(val_16[((n*_gid1)+(_gid4+1))]*val_19[((_gid2+2)+(k*(_gid4+1)))]));
        val_7_3 = (val_7_3+(val_16[((n*_gid1)+(_gid4+1))]*val_19[((_gid2+3)+(k*(_gid4+1)))]));
        val_7_0 = (val_7_0+(val_16[((n*_gid1)+(_gid4+2))]*val_19[(_gid2+(k*(_gid4+2)))]));
        val_7_1 = (val_7_1+(val_16[((n*_gid1)+(_gid4+2))]*val_19[((_gid2+1)+(k*(_gid4+2)))]));
        val_7_2 = (val_7_2+(val_16[((n*_gid1)+(_gid4+2))]*val_19[((_gid2+2)+(k*(_gid4+2)))]));
        val_7_3 = (val_7_3+(val_16[((n*_gid1)+(_gid4+2))]*val_19[((_gid2+3)+(k*(_gid4+2)))]));
        val_7_0 = (val_7_0+(val_16[((n*_gid1)+(_gid4+3))]*val_19[(_gid2+(k*(_gid4+3)))]));
        val_7_1 = (val_7_1+(val_16[((n*_gid1)+(_gid4+3))]*val_19[((_gid2+1)+(k*(_gid4+3)))]));
        val_7_2 = (val_7_2+(val_16[((n*_gid1)+(_gid4+3))]*val_19[((_gid2+2)+(k*(_gid4+3)))]));
        val_7_3 = (val_7_3+(val_16[((n*_gid1)+(_gid4+3))]*val_19[((_gid2+3)+(k*(_gid4+3)))]));
      }
      for (int _gid5=(n+-((n%4))); _gid5<n; _gid5+=1) {
        val_7_0 = (val_7_0+(val_16[((n*_gid1)+_gid5)]*val_19[(_gid2+(k*_gid5))]));
        val_7_1 = (val_7_1+(val_16[((n*_gid1)+_gid5)]*val_19[((_gid2+1)+(k*_gid5))]));
        val_7_2 = (val_7_2+(val_16[((n*_gid1)+_gid5)]*val_19[((_gid2+2)+(k*_gid5))]));
        val_7_3 = (val_7_3+(val_16[((n*_gid1)+_gid5)]*val_19[((_gid2+3)+(k*_gid5))]));
      }
      val_22[((k*_gid1)+_gid2)] = val_7_0;
      val_22[((k*_gid1)+(_gid2+1))] = val_7_1;
      val_22[((k*_gid1)+(_gid2+2))] = val_7_2;
      val_22[((k*_gid1)+(_gid2+3))] = val_7_3;
    }
    for (int _gid3=(k+-((k%4))); _gid3<k; _gid3+=1) {
      float val_7 = 0.0;
      for (int _gid4=0; _gid4<(n+-((n%4))); _gid4+=4) {
        val_7 = (val_7+(val_16[((n*_gid1)+_gid4)]*val_19[(_gid3+(k*_gid4))]));
        val_7 = (val_7+(val_16[((n*_gid1)+(_gid4+1))]*val_19[(_gid3+(k*(_gid4+1)))]));
        val_7 = (val_7+(val_16[((n*_gid1)+(_gid4+2))]*val_19[(_gid3+(k*(_gid4+2)))]));
        val_7 = (val_7+(val_16[((n*_gid1)+(_gid4+3))]*val_19[(_gid3+(k*(_gid4+3)))]));
      }
      for (int _gid5=(n+-((n%4))); _gid5<n; _gid5+=1) {
        val_7 = (val_7+(val_16[((n*_gid1)+_gid5)]*val_19[(_gid3+(k*_gid5))]));
      }
      val_22[((k*_gid1)+_gid3)] = val_7;
    }
  }
}
// 20~100 GFlops (while OpenBLAS achieves 100~200 GFlops)
// with M <= 60 almost the same speed as OpenBLAS...
void gemm1(int M, int N, int K,
          const float * __restrict A,
          const float * __restrict B,
          float * __restrict C)
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
