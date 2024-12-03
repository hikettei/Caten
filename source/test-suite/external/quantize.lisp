(in-package :caten/test-suite)

(python-exec
"

import numpy as np
from gguf import GGUFWriter, GGMLQuantizationType
from gguf.quants import quantize, GGML_QUANT_SIZES

def write_dummy_gguf(quantization_type, filename):
    data = np.arange(64, dtype=np.float32)
    gguf_writer = GGUFWriter(filename, \"test_arch\")

    if quantization_type == 'f32':
        tensor = data.astype(np.float32)
        gguf_writer.add_tensor(\"tensor\", tensor)
    elif quantization_type == 'f16':
        tensor = data.astype(np.float16)
        gguf_writer.add_tensor(\"tensor\", tensor)
    elif quantization_type == 'q8_0':
        block_size, _ = GGML_QUANT_SIZES[GGMLQuantizationType.Q8_0]
        required_length = ((data.size + block_size - 1) // block_size) * block_size
        if data.size < required_length:
            data = np.pad(data, (0, required_length - data.size), mode='constant', constant_values=0)
        tensor = quantize(data, GGMLQuantizationType.Q8_0)
        gguf_writer.add_tensor(\"tensor\", tensor, raw_dtype=GGMLQuantizationType.Q8_0)
    else:
        raise ValueError(\"Unsupported quantization type\")

    gguf_writer.write_header_to_file()
    gguf_writer.write_kv_data_to_file()
    gguf_writer.write_tensors_to_file()
    gguf_writer.close()
")

(python-exec
"
def read_gguf_file(gguf_file_path):
    reader = GGUFReader(gguf_file_path)
    tensors_data = {}
    results = []
    for tensor in reader.tensors:
        if tensor.tensor_type != GGMLQuantizationType.F32 and tensor.tensor_type != GGMLQuantizationType.F16:
            data = dequantize(tensor.data, tensor.tensor_type)
        else:
            data = tensor.data
        tensors_data[tensor.name] = data
        results.append(data)

    return results")

(import-function "write_dummy_gguf")
(import-function "read_gguf_file")


(write_dummy_gguf "dummy.gguf" "F16")

(print (change-facet (read_gguf "dummy.gguf") :tensor))

(print (change-facet (caten/gguf:tensor-info-buffer (car (caten/gguf:gguf-tensor-info (caten/gguf:load-gguf "dummy.gguf")))) :tensor)))))
(deftest test-dequantization-q8_0
(generate_dummy_gguf "dummy.gguf" "Q8_0")
(with-given-dtype ((:float32 . "float32"))
  (assert-equal
   (:atol 1e-5 :rtol 1e-6)
   (change-facet (read_gguf "dummy.gguf") :tensor)
   (change-facet (caten/gguf:tensor-info-buffer (car (caten/gguf:gguf-tensor-info (caten/gguf:load-gguf "dummy.gguf")))) :tensor))))

(deftest test-dequantization-f16
  (generate_dummy_gguf "dummy.gguf" "F16")
  (with-given-dtype ((:float32 . "float32"))
    (assert-equal
     (:atol 1e-5 :rtol 1e-6)
     (change-facet (read_gguf "dummy.gguf") :tensor)
     (change-facet (caten/gguf:tensor-info-buffer (car (caten/gguf:gguf-tensor-info (caten/gguf:load-gguf "dummy.gguf")))) :tensor))))

(deftest test-dequantization-f32
(generate_dummy_gguf "dummy.gguf" "F32")
(with-given-dtype ((:float32 . "float32"))
  (assert-equal
   (:atol 1e-5 :rtol 1e-6)
   (change-facet (read_gguf "dummy.gguf") :tensor)
   (change-facet (caten/gguf:tensor-info-buffer (car (caten/gguf:gguf-tensor-info (caten/gguf:load-gguf "dummy.gguf")))) :tensor))))
