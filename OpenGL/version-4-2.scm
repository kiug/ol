; OpenGL 4.2 (2011)

(define-library (OpenGL version-4-2)
   (export
      (exports (OpenGL version-4-1))
    GL_VERSION_4_2
   )

   (import
      (r5rs base) (owl io)
      (OpenGL version-4-1))
   (begin
   (define GL_VERSION_4_2 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))