# Usage: `python ./train_data.py`

# This script downloads MNIST datum and saves as npy format
# cl-waffe2 loads the file later.

from torchvision.datasets import MNIST
import numpy as np

mnist_train = MNIST('~/tmp/mnist', train=True, download=True)
mnist_test  = MNIST('~/tmp/mnist', train=False, download=True)

train_set = mnist_train.data.numpy()
test_set  = mnist_test.data.numpy()

train_label = mnist_train.targets.numpy()
test_label  = mnist_test.targets.numpy()

print(train_set.shape)
print(test_set.shape)

def one_hot (array):
    n_labels = len(np.unique(array))
    return np.eye(n_labels)[array]

print(one_hot(train_label).shape)
print(one_hot(test_label).shape)

import os
if not os.path.isdir("./mnist_data"):
    os.mkdir("./mnist_data")


np.save("./mnist_data/train_data.npy", train_set.astype('float32'))
np.save("./mnist_data/test_data.npy",  test_set.astype('float32'))

np.save("./mnist_data/train_label.npy", one_hot(train_label).astype('float32'))
np.save("./mnist_data/test_label.npy",  one_hot(test_label).astype('float32'))
