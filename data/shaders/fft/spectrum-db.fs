uniform sampler2D fftData;
uniform float fftScale;
uniform float fftdBMin, fftdBMax;
const float fftdBLog = 3.321928094887362;

void main() {
  float pos = gl_TexCoord[0].y;
  vec2 peak = texture2D(fftData, vec2(gl_TexCoord[0].x, 0.0)).xy;
  peak = ((10.0 * (log2(peak) / fftdBLog)) - fftdBMin) / (fftdBMax - fftdBMin);
  peak *= fftScale;
  
  vec3 color = vec3(0.0, 0.0, 0.0);
  
  if (peak.x >= pos) {
    color += vec3(0.0, 1.0, 1.0) * (exp((pos - peak.x) * 2.0) + 0.5);
  }
    
  if (peak.y >= pos) {
    color += vec3(1.0, 0.0, 0.0) * (exp((pos - peak.y) * 8.0) + 0.1);
  }
  
  gl_FragColor = vec4(color, 1.0);
}
