#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

#define PROCESSING_TEXTURE_SHADER

vec2 cx_inv(vec2 a) {
    float x = a.x;
    float y = a.y;
    float nsq = (x*x)+(y*y);
    if (nsq <= 0.0000001) {
        nsq = 0.0000001;
    }
    return vec2(x/nsq, -y/nsq);
}

#define cx_mul(a, b) vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x)
#define cx_div(a, b) (cx_mul(a , cx_inv(b)))

uniform sampler2D texture;
uniform vec2 texOffset;
uniform float modelscale;
uniform int framecount;
uniform float mousex;
uniform float mousey;
uniform float aspect_ratio;
uniform float zoom;
varying vec4 vertColor;
varying vec4 vertTexCoord;


vec2 cx_mobius(vec2 a) {
    vec2 c1 = a - vec2(1.0,0.0);
    vec2 c2 = a + vec2(1.0,0.0);
    return cx_div(c1, c2);
}


vec2 mirror_tc(vec2 tc) {
    vec2 r;
    if (mod(floor(tc.s), 2.0) == 0.0) {
        r.s = fract(tc.s);
    }
    else {
        r.s = 1.0 - fract(tc.s);
    }
    if (mod(floor(tc.t), 2.0) == 0.0) {
        r.t = fract(tc.t);
    }
    else {
        r.t = 1.0 - fract(tc.t);
    }

    return r;
}

float round(float a) {
    return floor(a + 0.5);
}

vec4 get_integer_circles_color(vec2 c) {
    vec4 pixel;
    float line_width = 0.15;
    float dnorm = length(c);
    float nearest_int = abs(dnorm-float(round(dnorm)));
    if (nearest_int < line_width) {
        float a =  1.0 - nearest_int/line_width;
        pixel = vec4(a,a,a,a);
    }
    else {
        pixel = vec4(0.0,0.0,0.0,0.0);
    }
    return pixel;
}


void main(void) {
  //float zoom = 0.1;
  //float f = float(framecount) * 0.1;
  //float tx = vertTexCoord.t * 200.0;
  //float ty = vertTexCoord.s * 200.0;
  //float xoff = (0.5 + cos(tx + f) * 0.5) * 0.005;
  //float yoff = (0.5 + sin(ty + f) * 0.5) * 0.005;
  //float xoff = noise(vertTexCoord.s);
  //float yoff = noise(vertTexCoord.t);

  //vec2 off = gl_TexCoord[0].st - vec2(0.0, 0.0);

  vec4 position = vec4(0.0, 0.0, 0.0, 0.0);
  vec4 off4 = (vertTexCoord - 0.5) * vec4(aspect_ratio,1.0, aspect_ratio,1.0) * zoom;
  off4 = off4 + vec4(vec2(aspect_ratio,1.0) * position.st, 0.0, 0.0);

  vec2 off = off4.st;
  //  vec2 off = vertTexCoord.st - vec2(0.5, 0.5);

  //off = off * vec2(aspect_ratio, 1.0);

  vec2 tc = cx_mobius(cx_div(vec2(-mousex, mousey), off ));
  tc = cx_mobius(cx_div(vec2(-mousex, mousey), tc )); 
 // vec2 vertTexCoord2 = cx_mobius(vec2(mousex, mousey));
  //vec4 vertTexCoord2 = vertTexCoord + vec4(xoff, yoff, 0.0, 0.0);
//  vertTexCoord2.t = vertTexCoord.t;

   tc = tc - vec2(0.5 * aspect_ratio * zoom, // + aspect_ratio * zoom,
                 0.5 * zoom);
   //tc = tc * texOffset;
                 //0.5 * zoom + aspect_ratio * zoom);
  // tc = cx_mobius(cx_div(vec2(-mousex, mousey), tc ));

  //vec4 col =  texture2D(texture, vec2(fract(tc.s), fract(tc.t)));
  //vec4 col =  texture2D(texture, mirror_tc(tc));
  vec4 col =  texture2D(texture, tc);
  //vec4 col =  texture2D(texture, gl_TexCoord[0].st);
  //vec4 col =  texture2D(texture, vec2(fract(vertTexCoord2.s), fract(vertTexCoord2.t)));
  gl_FragColor = col;


}
