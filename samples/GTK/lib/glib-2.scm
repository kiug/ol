(define-library (lib glib-2)
   (export
      gint gulong gpointer

      G_APPLICATION_FLAGS_NONE

      GCallback
      GClosureNotify
      GConnectFlags
      GApplication*

      G_CALLBACK

      g_signal_connect
      g_application_run
   )
   (import
      (scheme core)
      (otus ffi))

(begin

(define G_APPLICATION_FLAGS_NONE 0)

(define gint fft-int)
(define gulong fft-unsigned-long)
(define gpointer fft-void*)
(define GCallback type-callable) ; void (*GCallback)(void)
(define GClosureNotify type-callable) ; void (*GClosureNotify)(gpointer, GClosure)
(define GConnectFlags fft-int) ; enum
(define GApplication* fft-void*)

(define G_CALLBACK make-callback)

(define GLIB (load-dynamic-library "libglib-2.0.so"))
(define GOBJECT (load-dynamic-library "libgobject-2.0.so"))
(define GIO (load-dynamic-library "libgio-2.0.so"))

(define g_signal_connect_data (GOBJECT gulong "g_signal_connect_data" gpointer type-string GCallback gpointer GClosureNotify GConnectFlags))
(define (g_signal_connect instance detailed_signal c_handler data)
   (g_signal_connect_data instance detailed_signal c_handler data #false 0))
(define g_application_run (GIO fft-int "g_application_run" GApplication* fft-int fft-void*))

))