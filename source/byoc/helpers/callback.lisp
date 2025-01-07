;; Note1: only macOS should need this
;; Note2: create a closure that calls the stored callback (ensure it is thread-safe!)
;; Note3: this thing can be realized with only using cffi?
(c
"#include <stdio.h>
typedef void (*callback_t)(void);
static callback_t stored_cb = NULL;

void* create_callback(void (*lisp_callback)(void)) {
  stored_cb = lisp_callback;      
  return (^{ stored_cb(); });
}
")
