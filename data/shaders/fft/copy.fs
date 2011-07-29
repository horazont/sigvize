uniform sampler2D input;
void main(void)
{
  float peak = texture2D(input, vec2(gl_TexCoord[0])).r;
  gl_FragColor = vec4(peak, 0.0, 0.0, 1.0);
}
