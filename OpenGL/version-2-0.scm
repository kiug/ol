; OpenGL 2.0 (2004)

(define-library (OpenGL version-2-0)
   (export
      (exports (OpenGL version-1-5))
    GL_VERSION_2_0

glCreateShader
GL_VERTEX_SHADER
GL_FRAGMENT_SHADER
glShaderSource
glCompileShader
glCreateProgram
glAttachShader
glDetachShader
glLinkProgram
glUseProgram
glGetShaderiv
GL_COMPILE_STATUS
GL_LINK_STATUS
GL_VALIDATE_STATUS
GL_INFO_LOG_LENGTH
glGetShaderInfoLog
glGetUniformLocation
glUniform1i
glUniform1f
glUniform2f
glEnableVertexAttribArray
glVertexAttribPointer
GL_FLOAT
glDrawArrays
  )
  
   (import
      (r5rs base) (owl io)
      (OpenGL version-1-5))
   (begin
   (define GL_VERSION_2_0 1)
   
   (define % (dlopen GL_LIBRARY RTLD_LAZY))

;	using GLchar		= System.Byte;		// char

(define GLchar** type-tuple)
(define GLint* type-vector-raw)
(define GLsizei* type-vector-raw)
(define GLchar* type-string)
(define void* type-vector-raw)

  (define glCreateShader    (glGetProcAddress GLuint "glCreateShader" GLenum))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (glGetProcAddress GLvoid "glShaderSource" GLuint GLsizei GLchar** GLint*))
  (define glCompileShader   (glGetProcAddress GLvoid "glCompileShader" GLuint))
  (define glCreateProgram   (glGetProcAddress GLuint "glCreateProgram"))
  (define glAttachShader    (glGetProcAddress GLvoid "glAttachShader" GLuint GLuint))
  (define glDetachShader    (glGetProcAddress GLvoid "glDetachShader" GLuint GLuint))
  (define glLinkProgram     (glGetProcAddress GLvoid "glLinkProgram" GLuint))
  (define glUseProgram      (glGetProcAddress GLvoid "glUseProgram" GLuint))
  (define glGetShaderiv     (glGetProcAddress GLvoid "glGetShaderiv" GLuint GLenum GLint*))
    (define GL_COMPILE_STATUS  #x8B81)
    (define GL_LINK_STATUS     #x8B82)
    (define GL_VALIDATE_STATUS #x8B83)
    (define GL_INFO_LOG_LENGTH #x8B84)
  (define glGetShaderInfoLog (glGetProcAddress GLvoid "glGetShaderInfoLog" GLuint GLsizei GLsizei* GLchar*))
  (define glGetUniformLocation (glGetProcAddress GLint "glGetUniformLocation" GLuint GLchar*))
    (define glUniform1i     (glGetProcAddress GLvoid "glUniform1i" GLint GLint))
    (define glUniform1f     (glGetProcAddress GLvoid "glUniform1f" GLint GLfloat))
    (define glUniform2f     (glGetProcAddress GLvoid "glUniform2f" GLint GLfloat GLfloat))
  (define glEnableVertexAttribArray (glGetProcAddress GLvoid "glEnableVertexAttribArray" GLuint))
  (define glVertexAttribPointer (glGetProcAddress GLvoid "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei void*))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (glGetProcAddress GLvoid "glDrawArrays" GLenum GLint GLsizei))

))