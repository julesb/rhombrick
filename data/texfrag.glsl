#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

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
uniform int framecount;
uniform float aspect_ratio;
uniform float viewx;
uniform float viewy;
uniform float mousex;
uniform float mousey;
uniform float zoom;

varying vec4 vertColor;
varying vec4 vertTexCoord;

float round(float a) {
    return floor(a + 0.5);
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

vec2 cx_mobius(vec2 a) {
    vec2 c1 = a - vec2(1.0,0.0);
    vec2 c2 = a + vec2(1.0,0.0);
    return cx_div(c1, c2);
}

vec4 get_cursor_color(vec2 c) {
    vec4 col = vec4(1,1,0,1);
    float d = length(c - vec2(mousex, mousey));
    return vec4(col.rgb / d, 1);
}

vec4 get_grid_pixel_color(vec2 c) {
    int grid_pixel = 0;
    float bri=0.;
    int val = 0;
    vec4 linecolor = vec4(1.0, 1.0, 1.0, 1.0);
    vec4 bgcolor = vec4(0.0,0.0,0.0,0.0);
    vec2 nearest_int = vec2(abs(c.x - round(c.x)), abs(c.y - round(c.y)));
    float linewidth = 0.125;
    if ((nearest_int.x < linewidth) && (nearest_int.y < linewidth)) { // line intersection
      bri = 2.0 - (nearest_int.x+nearest_int.y) / linewidth; // better
      //linecolor.a = bri;
      return linecolor * bri;
      //return vec4(linecolor, bri);
      //return vec4(bri,bri,bri,bri);
    }
    else if ((nearest_int.x < linewidth) || (nearest_int.y < linewidth)) {
      if (nearest_int.x < linewidth) {
            bri = ((1.-nearest_int.x/(linewidth)));
            //linecolor.a = bri;
            return linecolor * bri;
            //return vec4(1.0,1.0,1.0,bri);
            //return vec4(bri,bri,bri,bri);
      }
      else if (nearest_int.y < linewidth) {
        bri = ((1.-nearest_int.y/(linewidth)));
        //linecolor.a = bri;
        return linecolor * bri;
        //return vec4(1.0,1.0,1.0,bri);
        //return vec4(bri,bri,bri,bri);
      }
    }
    else {
        linecolor = bgcolor;
    }
    return linecolor;
    //return vec4(1.0,0.0,0.0,0.0);
  }


vec4 get_integer_circles_color(vec2 c) {
    vec4 pixel;
    float line_width = 0.5;
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

//vec2 world_to_screen(vec2 w) {
//}

//vec2 screen_to_world(vec2 s) {
//}

void main() {
    vec2 cen = vec2(0.5*aspect_ratio, 0.5);
    vec2 tc0 = vertTexCoord.st;
    //tc = vec2(mousex, mousey) - (tc - cen) * zoom ;
    vec2 tc = vec2(viewx, viewy) - (tc0 - cen) * zoom ;
    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));


    gl_FragColor = (get_integer_circles_color(tc)
                 + get_grid_pixel_color(tc)
                 + vec4(2,2,2,1) * texture2D(texture, mirror_tc(tc))
                 + get_cursor_color(tc0)) * 0.25;
      //gl_FragColor = get_grid_pixel_color(tc);
      //gl_FragColor = texture2D(texture, vertTexCoord.st) * vertColor;
}
