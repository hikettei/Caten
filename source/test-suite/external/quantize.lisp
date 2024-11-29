(in-package :caten/test-suite)

(python-exec
 "
def test_dequantization(values, scale):
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
        \"Q4_0\": 2,
        \"Q5_0\": 6,
        \"Q8_0\": 8,
        \"Q2_K\": 10,
        \"Q3_K\": 11,
        \"Q4_K\": 12,
        \"Q5_K\": 13,
        \"Q6_K\": 14,
    }

    GGML_TYPE_Q8_0 = GGML_TYPES[\"Q8_0\"]

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

            f.write(b'\\x00' * 64)  # Placeholder of 64 bytes for metadata

            # Quantize the data to Q8_0 format
            data = tensor[\"data\"].flatten()
            size = data.size

            blocks = []
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

            tensor_binary_data = b''.join(blocks)
            tensor_data_blocks.append(tensor_binary_data)

            tensor_metadata.append({
                \"name\": tensor['name'],
                \"ggml_type\": GGML_TYPE_Q8_0,
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
                f.write(b'\\x00' * padding_size)
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
                f.write(b'\\x00' * padding)
            elif padding < 0:
                raise ValueError(f\"Metadata for tensor '{metadata['name']}' exceeds 64 bytes. Increase the placeholder size.\")
")

(in-package :caten/gguf)


(defparameter *scale-factor-bytes*
  (make-array 2
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x00 #x30)))


(defparameter *quantized-values*
  (make-array 32
              :element-type '(unsigned-byte 8)
              :initial-contents
              (let ((values '(1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8
                              9 -9 10 -10 11 -11 12 -12 13 -13 14 -14 15 -15 16 -16)))
                    (mapcar (lambda (v)
                              (if (>= v 0)
                                  v
                                  (+ 256 v)))
                            values))))


(defparameter *data*
  (let ((total-length (+ (length *scale-factor-bytes*) (length *quantized-values*))))
    (make-array total-length
                :element-type '(unsigned-byte 8)
                :initial-contents
                (concatenate 'list *scale-factor-bytes* *quantized-values*))))

(defparameter *input-buffer* (fast-io:make-input-buffer :vector *data*))

(defparameter *test-tensor-info*
  (make-tensor-info "test-tensor"
                    1           ;; Number of dimensions
                    '(32)       ;; Dimensions
                    :Q8_0       ;; Tensor type
                    0))         ;; Offset

(print (dequantize :Q8_0 *input-buffer* *test-tensor-info*))