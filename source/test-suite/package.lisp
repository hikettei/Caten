(in-package :cl-user)

(defpackage :caten/test-suite
  (:use :cl :rove
   :caten :caten/nn :caten/air :caten/aasm
   :caten/avm :caten/common.dtype :alexandria
   :caten/codegen :caten/llm :py4cl
   :caten/codegen/scheduler))

(in-package :caten/test-suite)

(python-exec "import torch
import torch.nn.functional as f
import numpy as np")
(import-module "numpy" :as "np" :reload t)
(import-module "torch.nn.functional" :as "f" :reload t)
(import-function "torch.from_numpy")
(import-function "torch.matmul")
(import-function "torch.transpose")
(import-function "list" :as "py.list")
(import-function "slice")
