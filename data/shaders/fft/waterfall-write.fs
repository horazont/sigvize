uniform sampler2D fftData;

void main() {
  gl_FragColor = vec4(texture2D(fftData, vec2(gl_TexCoord[0].x, 0.0)).x, 0.0, 0.0, 1.0);
}
