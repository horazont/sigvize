uniform sampler1D fftData;
uniform float fftAvgStep;

void main(void)
{
  float p = gl_TexCoord[0].x;
  float s = fftAvgStep;

  vec2 peak = vec2(texture1D(fftData, p).r, 0.0);
  peak.y = (peak.x + texture1D(fftData, p - s * 3.0).r +
               texture1D(fftData, p - s * 2.0).r +
               texture1D(fftData, p - s).r +
               texture1D(fftData, p + s).r +
               texture1D(fftData, p + s * 2.0).r +
               texture1D(fftData, p + s * 3.0).r) / 7.0;
  gl_FragColor = vec4(peak, 0.0, 1.0);
}
