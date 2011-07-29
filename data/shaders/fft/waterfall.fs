uniform sampler2D fftData;
uniform float fftScale;
uniform float scale;
uniform float bias;

vec3 htorgb(float h)
{
  float clampedH = h;
  if (clampedH < 0.0) {
//    clampedH = 6.0 - clampedH;
    clampedH = 0.0;
  }
  if (abs(clampedH) >= 6.0) {
//    clampedH = clampedH - floor(clampedH / 6.0) * 6.0;
    clampedH = 6.0;
  }

  float f = clampedH - floor(clampedH);

  float q = 1.0 - f;

  if (clampedH < 1.0) {
    return vec3(1.0, f, 0.0);
  } else if (clampedH < 2.0) {
    return vec3(q, 1.0, 0.0);
  } else if (clampedH < 3.0) {
    return vec3(0.0, 1.0, f);
  } else if (clampedH < 4.0) {
    return vec3(0.0, q, 1.0);
  } else if (clampedH < 5.0) {
    return vec3(f, 0.0, 1.0);
  } else if (clampedH <= 6.0) {
    return vec3(1.0, 0.0, q);
  }
}

void main(void)
{
  float peak = texture2D(fftData, vec2(gl_TexCoord[0])).r * fftScale;
  gl_FragColor = vec4(htorgb(peak * scale + bias) * min(peak * 6.0, 1.0) + vec3(1.0, 1.0, 1.0) * max(0.0, (peak - 1.0) * 6.0), 1.0);
  //float peak = texture2D(fftData, vec2(gl_TexCoord[0])).r;
  //gl_FragColor = vec4(1.0, peak, 0.0, 1.0);
}
