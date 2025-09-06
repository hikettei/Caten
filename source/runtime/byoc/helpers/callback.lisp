;; TODO: Implement this without using cffi-grovel.
(c
 "
#ifdef __APPLE__

#include <stdio.h>
#include<Block.h>

typedef void (^CFFIBlock)();
CFFIBlock make_callback_closure(void (*lisp_callback)(void)) {
    return (CFFIBlock)Block_copy(^{ lisp_callback(); });
}

void free_callback_closure(CFFIBlock closure) {
    Block_release(closure);
}

#else

#include <stdio.h>

void* closure_cffi_callback(void (*lisp_callback)(void)) {
    fprintf(stderr, \"Error(caten/byoc/helper/callback.lisp): Apple Blocks extension not supported on this platform.\n\");
    return NULL; 
}

#endif
")
