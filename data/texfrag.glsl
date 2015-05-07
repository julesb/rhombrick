#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

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



void main() {
    vec2 cen = vec2(0.5*aspect_ratio, 0.5);
    vec2 tc = vertTexCoord.st;
    //tc = vec2(mousex, mousey) - (tc - cen) * zoom ;
    tc = vec2(viewx, viewy) - (tc - cen) * zoom ;
    gl_FragColor = (get_integer_circles_color(tc)
                 + get_grid_pixel_color(tc)
                 * texture2D(texture, tc)) * 0.333;
      //gl_FragColor = get_grid_pixel_color(tc);
      //gl_FragColor = texture2D(texture, vertTexCoord.st) * vertColor;
}
