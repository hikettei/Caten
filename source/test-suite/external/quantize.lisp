(in-package :caten/test-suite)

(python-exec
 "
def test_dequantization(values, scale):
  #pygguf reader vs cl reader
  pass
"
 )

(python-exec
"
def write_gguf(filename, tensors):
    MAGIC = b\"GGUF\"
    VERSION = 3  # Set to 3 to match pygguf's expectation
    N_KV_PAIRS = 0  # No key-value pairs for simplicity

    GGML_TYPES = {
        \"F32\": 0,
        \"F16\": 1,
        \"Q8_0\": 8,
    }

    with open(filename, \"wb\") as f:
        f.write(MAGIC)                                 # Magic string (4 bytes)
        f.write(struct.pack(\"<I\", VERSION))           # Version (4 bytes)
        f.write(struct.pack(\"<Q\", len(tensors)))      # Number of tensors (8 bytes)
        f.write(struct.pack(\"<Q\", N_KV_PAIRS))        # Number of key-value pairs (8 bytes)

        header_end_offset = f.tell()

        tensor_metadata = []
        tensor_data_blocks = []
        metadata_offsets = []

        for tensor in tensors:
            metadata_offsets.append(f.tell())

            f.write(b\'\\x00\' * 64)  # Placeholder of 64 bytes for metadata

            # Prepare the tensor data based on the requested type
            data = tensor[\"data\"].flatten()
            target_type = tensor[\"type\"]  # Specify target type for this tensor
            if target_type == \"F32\":
                tensor_binary_data = data.astype(np.float32).tobytes()
                ggml_type = GGML_TYPES[\"F32\"]
            elif target_type == \"F16\":
                tensor_binary_data = data.astype(np.float16).tobytes()
                ggml_type = GGML_TYPES[\"F16\"]
            elif target_type == \"Q8_0\":
                blocks = []
                size = data.size
                for i in range(0, size, 32):
                    block_data = data[i:i+32]
                    if len(block_data) < 32:
                        block_data = np.pad(block_data, (0, 32 - len(block_data)), 'constant', constant_values=0)
                    scale = np.max(np.abs(block_data)) / 127 if np.any(block_data) else 0
                    scale = np.float16(scale)
                    if scale > 0:
                        quantized_values = np.round(block_data / scale).astype(np.int8)
                    else:
                        quantized_values = np.zeros(32, dtype=np.int8)
                    block_bytes = struct.pack(\"<e\", scale) + quantized_values.tobytes()
                    blocks.append(block_bytes)
                tensor_binary_data = b\'\'.join(blocks)
                ggml_type = GGML_TYPES[\"Q8_0\"]
            else:
                raise ValueError(f\"Unsupported type: {target_type}\")

            tensor_data_blocks.append(tensor_binary_data)

            tensor_metadata.append({
                \"name\": tensor['name'],
                \"ggml_type\": ggml_type,
                \"n_dims\": len(tensor['shape']),
                \"shape\": tensor['shape'],
                \"data_offset\": None,  # Will set this after writing data
                \"data_size\": len(tensor_binary_data),
            })

        start = f.tell()

        for idx, tensor_data in enumerate(tensor_data_blocks):
            data_offset = f.tell()
            data_offset_aligned = (data_offset + 31) & ~31
            if data_offset_aligned != data_offset:
                padding_size = data_offset_aligned - data_offset
                f.write(b\'\\x00\' * padding_size)
            metadata = tensor_metadata[idx]
            metadata[\"data_offset\"] = data_offset_aligned - start
            f.seek(data_offset_aligned)
            f.write(tensor_data)

        for idx, metadata in enumerate(tensor_metadata):
            f.seek(metadata_offsets[idx])
            name_bytes = metadata['name'].encode('utf-8')
            name_length = len(name_bytes)
            f.write(struct.pack(\"<Q\", name_length))              # Name length (8 bytes)
            f.write(name_bytes)                                    # Name
            f.write(struct.pack(\"<I\", metadata['n_dims']))      # Number of dimensions (4 bytes)
            for dim in reversed(metadata['shape']):              # Dimensions (8 bytes each)
                f.write(struct.pack(\"<Q\", dim))
            f.write(struct.pack(\"<I\", metadata['ggml_type']))    # Data type (4 bytes)
            f.write(struct.pack(\"<Q\", metadata['data_offset']))  # Data offset (8 bytes)
            metadata_size = (
                8 + name_length +                     # Name length and name
                4 +                                   # Number of dimensions
                8 * metadata['n_dims'] +             # Dimensions
                4 +                                   # Data type
                8                                     # Data offset
            )
            padding = 64 - metadata_size
            if padding > 0:
                f.write(b\'\\x00\' * padding)
            elif padding < 0:
                raise ValueError(f\"Metadata for tensor '{metadata['name']}' exceeds 64 bytes. Increase the placeholder size.\")
")


(python-exec
"
def generate_dummy_gguf(filename, type):
    tensor1 = {
        \"name\": \"tensor1\",
        \"shape\": (64,),
        \"data\": np.arange(0,64,dtype=np.float32).astype(np.float32),
        \"type\": type
    }
    write_gguf(filename, [tensor1])")

(python-exec
"
def read_gguf(filename):
  with open(filename, \"rb\") as f:
    magic = f.read(4)
    if magic != b\"GGUF\":
        raise ValueError(f\"Invalid magic string: {magic}\")
    version, n_tensors, n_kv = struct.unpack(\"<IQQ\", f.read(4+8+8))

    info, tensorinfo = gguf.load_gguf(f)

    print(tensorinfo)
    results = []
    for name in tensorinfo:
      weights = gguf.load_gguf_tensor(f, tensorinfo, name)
      results.append(weights)
    return weights
")

(import-function "generate_dummy_gguf")
(import-function "read_gguf")

;(in-package :caten/gguf)

;; generate a dummy gguf file with a single tensor of shape 64 and with random values from -1 to 1
(generate_dummy_gguf "dummy1.gguf" "Q8_0")

(print (read_gguf "dummy1.gguf"))

(in-package :caten/gguf)
(defparameter test (load-gguf "dummy.gguf"))
(print (tensor-info-buffer (car (gguf-tensor-info test))))

(in-package :caten/test-suite)

(deftest test-dequantization-q8_0
  (with-given-dtype ((:float32 . "float32"))
    (assert-equal
     (:atol 1e-5 :rtol 1e-6))
    
    ))

(deftest test-dequantization-f16
  (with-given-dtype ((:float32 . "float32"))
    ))


(deftest test-dequantization-f32
  (with-given-dtype ((:float32 . "float32"))
    ))
