#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

#define PROCESSING_COLOR_SHADER

varying vec4 vertColor;

void main() {
  gl_FragColor = vec4(0.0, 0.0, 1, 1);
  //gl_FragColor = vertColor;
}
