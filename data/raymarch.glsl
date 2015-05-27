#extension GL_EXT_gpu_shader4 : enable

#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

/*
   Based on tutorial at:
   http://www.geeks3d.com/20130524/building-worlds-with-distance-functions-in-glsl-raymarching-glslhacker-tutorial-opengl/
*/

uniform sampler2D texture;
uniform vec2 texOffset;
uniform float modelscale;
uniform float aspect_ratio;
uniform float mousex;
uniform float mousey;
uniform float framecount;
uniform float swidth;
uniform float sheight;
uniform vec3 cam_pos;
uniform vec3 cam_lookat;
uniform float time;
//uniform vec2 resolution;

varying vec4 vertColor;
varying vec4 vertTexCoord;

//uniform vec2 mouse;
float PI=3.14159265;

vec2 obj_floor(in vec3 p) {
    return vec2(p.y+0.0,0);
}

vec2 obj_sphere(in vec3 p) {
    float d = length(p)-1.0;
    return vec2(d,3);
}

vec2 obj_torus(in vec3 p) {
    vec2 r = vec2(2.0,1.5);
    vec2 q = vec2(length(p.xz)-r.x,p.y);
    float d = length(q)-r.y;
    return vec2(d,2);
}

vec2 obj_round_box(vec3 p) {
    float d = length(max(abs(p)-vec3(0.5,10.0,0.5),0.0))-0.25;
    return vec2(d,1);
}

vec2 op_union(vec2 a, vec2 b) {
    float d = min(a.x, b.x);
    if (d < b.x)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
}

vec2 op_sub(vec2 a, vec2 b) {
    float d = max(a.x, -b.x);
    return vec2(d,2);
}

vec2 op_blend(vec3 p, vec2 a, vec2 b) {
    float s = smoothstep(length(p), 0.0, 1.0);
    float d = mix(a.x, b.x, s);
    if (s < 0.5)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
}

float smin( float a, float b, float k ) {
    float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
    return mix( b, a, h ) - k*h*(1.0-h);
}

vec2 obj_sine(vec3 p) {
    return vec2((sin(p.x * 0.2059)
               + sin(p.y * 0.231)
               + sin(p.z * 0.226)) * 0.333, 4.0);
}

vec2 obj_ufo(vec3 p) {
    return op_union(op_blend(p, obj_round_box(p),
                                obj_torus(p)),
                    obj_sphere(p));
}

vec2 op_rep(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_ufo(q);
}


vec2 distance_to_obj(in vec3 p) {
    //return op_union(obj_floor(p) + vec2(sin(p.x) + sin(p.y) + sin(p.z), 0.0),
    return op_union(obj_floor(p),
                    op_rep(p, vec3(5.5 + sin(float(framecount) / 631.23) * 0.75 * p.x,
                                   5.5 + cos(float(framecount) / 693.27) * 0.75 * p.y,
                                   5.5 + cos(float(framecount) / 600.21) * 0.75 * p.z))
                        );

//    return op_union(obj_floor(p), obj_sine(p));
//    return obj_floor(p);
}


vec3 get_integer_circles_color(vec2 c, vec3 col) {
    vec3 pixel;
    vec3 basecol = col; //vec4(1.0, 1.0, 0.75, 1.0);
    float line_width = 0.4;
    float dnorm = length(c);
    float nearest_int = abs(dnorm-float(round(dnorm)));
    if (nearest_int < line_width) {
        float a =  1.0 - nearest_int/line_width;
        pixel = vec3(a,a,a) * basecol;
    }
    else {
        pixel = vec3(0.0,0.0,0.0);
    }
    return pixel;
}

vec3 floor_color(in vec3 p) {
    float m = 0.5;
    vec3 c = vec3(0.0); //get_integer_circles_color(p.xz, vec3(1.0,1.0,1.0));
    if (fract(p.x*m)>m) {
        if (fract(p.z*m)>m)
            return vec3(0,0.1,m) + c;
        else
            return vec3(0,0,0) + c;
    }
    else {
        if (fract(p.z*m)>m)
            return vec3(0,0,0) + c;
        else
            return vec3(0,0.3,0) + c;
    }
}

vec3 prim_color(in vec3 p, int i) {
    if (i == 0)
        return floor_color(p);
    else if (i == 1)
        return vec3(1.0,0.0,0.0);
    else if (i == 2)
        return vec3(0,0,1);
    else if (i == 3)
        return vec3(1,1,0);
    else
        return vec3(0.3, 0.3, 0.3);
}

void main(void) {
    vec2 q = vertTexCoord.st;
    vec2 vPos = -vec2(aspect_ratio, 1.0) + 2.0 * q;
    //vec2 vPos = -1.0 + 2.0 * q;

    // Camera up vector.
    vec3 vuv=vec3(0,-1,0); 

    // Camera lookat.
    vec3 vrp = cam_lookat;

    // Camera pos
    vec3 prp = cam_pos;

    // Camera setup.
    vec3 vpn=normalize(vrp-prp);
    vec3 u=normalize(cross(vuv,vpn));
    vec3 v=cross(vpn,u);
    vec3 vcv=(prp+vpn);
    //vec3 scrCoord=vcv+vPos.x*u*swidth/sheight+vPos.y*v;
    //vec3 scrCoord=vcv+vPos.x*u*resolution.x/resolution.y+vPos.y*v;
    float fov = 0.5;
    vec3 scrCoord=vcv + vPos.x*u*fov
                      + vPos.y*v*fov;
    vec3 scp=normalize(scrCoord-prp);

    // Raymarching.
    const vec3 e=vec3(0.02,0,0);
    const float maxd=100.0; //Max depth
    vec2 d=vec2(0.02,0.0);
    vec3 c,p,N;

    float f=1.0; // near plane?

    for(int i=0;i<256;i++) {
        if ((abs(d.x) < .001) || (f > maxd)) 
            break;
        f+=d.x;
        p=prp+scp*f;
        d = distance_to_obj(p);
    }

    if (f < maxd) {
        c = prim_color(p, int(d.y));
    
        vec3 n = vec3(d.x-distance_to_obj(p-e.xyy).x,
                      d.x-distance_to_obj(p-e.yxy).x,
                      d.x-distance_to_obj(p-e.yyx).x);
        N = normalize(n);

        //simple phong lighting, LightPosition = CameraPosition
        float b=dot(N,normalize(prp-p));
        gl_FragColor=vec4((b*c + pow(b,16.0)) * (1.0-f*.01), 1.0);
    }
    else 
        gl_FragColor=vec4(0.0,0.0,0.0,1.0); //background color

}
