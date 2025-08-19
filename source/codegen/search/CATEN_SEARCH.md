- [ ] 英語で書き直す
- [ ] 読んだ文献全部まとめておく

# Caten Search Introduction

```
Workflow:
    [Input Tensor Graph]
             | 
      [Maximize Fusion]
             |
    [BEAM w/ small space]
             |
   [Find an optimal kernel]
```

## Loop Fusion

### Background1. "Flash" operations

```c
Pool
Matmul+Softmax+Matmul
Matmul+Matmul (No beneficial)
Matmul+ReLU+Matmul (Maybe beneficial?)
```

```c
// Case1: How to fuse them?
[Matmul : 0 <= i <= 10 and 0 <= j <= 10 and 0 <= k <= 10]
[ReLU   : 0 <= i <= 100] => Reshape(10, 10) and Fuse
// Approach:
BandをCoincident
```

```c
[Conv+ReLU : 0 <= N <= 10 and 0 <= Cout <= 6 and 0 <= FH <= 20 and 0 <= FW <= 20 and 0 <= Cout <= 3 and 0 <= Kh <= 6 and 0 <= Kw <= 6]
=> Reshape(10, 6, 10, 10, 2, 2, 3, 6, 6)
[Pool      : 0 <= N <= 60 and 0 <= H <= 10 and 0 <= W <= 10 and 0 <= PH <= 2 and 0 <= PW <= 2]
=> Reshape(10, 6, 10, 10, 2, 2)

and apply full_fuse iteratively?
```

### Background2. Computation Complexity

Related:

- [ ] https://dl.acm.org/doi/fullHtml/10.1145/3416510
- [ ] https://arxiv.org/pdf/1803.10726

### Approach:

Goals:

- [ ] 入力はAccess RelationsとBand
- [ ] Always move the graph into beneficial direction.
- [ ] O(n) (or polynomial time)
- [ ] Post Fuse Reduction+Elwise (e.g.: coalesced relu + matmul fusion)
- [ ] Reimplement previous scheduler w/ new search

## BEAM Search
