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

vec4 ccols[3];

uniform sampler2D texture;
uniform vec2 texOffset;
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
    vec4 col = vec4(0.8,0.8,1,1);
    vec2 m = vec2(-mousex, -mousey) + vec2(viewx, viewy);
    float d = 1.0 *  sqrt(length(c - m));
    return vec4(col.rgb / d, 1.0);
}

vec4 get_grid_pixel_color(vec2 c) {
    int grid_pixel = 0;
    float bri=0.;
    int val = 0;
    vec4 linecolor = vec4(1.0, 0.8, 0.8, 0.0);
    vec4 bgcolor = vec4(0.0,0.0,0.0,0.0);
    vec2 nearest_int = vec2(abs(c.x - round(c.x)), abs(c.y - round(c.y)));
    float linewidth = 0.05;
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


vec4 get_integer_circles_color(vec2 c, vec4 col) {
    vec4 pixel;
    vec4 basecol = col; //vec4(1.0, 1.0, 0.75, 1.0);
    float line_width = 0.3;
    float dnorm = length(c);
    float nearest_int = abs(dnorm-float(round(dnorm)));
    if (nearest_int < line_width) {
        float a =  1.0 - nearest_int/line_width;
        pixel = vec4(a,a,a,a) * basecol;
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
    ccols[0] = vec4(1.0,0.0,0.0,1.0);
    ccols[1] = vec4(0.0,1.0,0.0,1.0);
    ccols[2] = vec4(0.2,0.2,1.0,1.0);
    vec2 cen = vec2(0.5*aspect_ratio, 0.5);
    vec2 tc0 = vertTexCoord.st;
    vec2 tc1 = tc0 / vec2(aspect_ratio, 1.0);
    //tc = vec2(mousex, mousey) - (tc - cen) * zoom ;
    vec2 tc = vec2(viewx, viewy) - (tc0 - cen) * zoom ;

    vec2 imtc = tc / vec2(-aspect_ratio, 1.0);

 
    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
//    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
//    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
//    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
//    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
//    tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));
 //   tc = cx_mobius(cx_div(vec2(-mousex+viewx, -mousey+viewy), tc ));



    float numiters = 3.0;
    float rad = 10.0;
    float PI = 3.1415926;
    vec4 circlescol = vec4(0.0,0.0,0.0,1.0);
    float a = 0.0;
    float x = 0.0;
    float y = 0.0;
    float i = 0.0;
    float astep =  2.0 * PI / numiters;
    for (i=0.0; i < numiters; i+=1.0) {
        a = i * astep;
        x = cos(a) * rad + sin(float(framecount)* (i+1.0) / 93.023);
        y = sin(a) * rad + sin(float(framecount) * (i+1.0) / 101.1);
        circlescol +=  get_integer_circles_color(tc + vec2(x,y), ccols[int(i)]);
    }
    circlescol = circlescol * (1.0/numiters);
    circlescol = vec4(circlescol.rgb, 1.0);

    vec4 pixelcol = (
                //(get_integer_circles_color(tc, vec4(1.0,1.0,1.0,1.0))
                //+ get_integer_circles_color(tc + vec2(10.0, 0.0), vec4(1.0,1.0,1.0,1.0)))
                //+ get_integer_circles_color(tc + vec2(0.0, 1.0), vec4(1.0,1.0,1.0,1.0))
                //get_grid_pixel_color(tc)
                + circlescol
                 //+ vec4(2,2,2,1) * texture2D(texture, (fract(imtc)))
                 //+ vec4(2,2,2,1) * texture2D(texture, mirror_tc(tc))
                 + get_cursor_color(tc)) * 0.9;
    gl_FragColor = vec4(pixelcol.rgb, 1.0);
/*
    gl_FragColor = (get_integer_circles_color(tc)
                 //+ get_grid_pixel_color(tc)
                 //+ vec4(2,2,2,1) * texture2D(texture, imtc)
                 + vec4(2,2,2,1) * texture2D(texture, mirror_tc(imtc))
                 + get_cursor_color(tc)) * 0.275;
      //gl_FragColor = get_grid_pixel_color(tc);
      //gl_FragColor = texture2D(texture, vertTexCoord.st) * vertColor;
*/
}
