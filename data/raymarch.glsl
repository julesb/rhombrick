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
uniform float aspect_ratio;
uniform float mousex;
uniform float mousey;
uniform float framecount;
uniform float swidth;
uniform float sheight;
uniform vec3 cam_pos;
uniform vec3 cam_lookat;
uniform float time;

varying vec4 vertColor;
varying vec4 vertTexCoord;

float PI=3.14159265;



vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }

float snoise(vec3 v)
  { 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;
  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)
  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)
  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);
  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );
  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));
  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;
  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
  }





float hash( float n )
{
    return fract(sin(n)*43758.5453);
}

float noise( vec3 x )
{
    // The noise function returns a value in the range -1.0f -> 1.0f
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y*57.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+0.0), hash(n+1.0),f.x),
                   mix( hash(n+57.0), hash(n+58.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+170.0), hash(n+171.0),f.x),f.y),f.z);
}

vec2 obj_floor(in vec3 p) {
    return vec2(p.y+0.0,0);
}

vec2 obj_sphere(in vec3 p, float r) {
    float d = length(p)-r;
    return vec2(d,0);
}

vec2 obj_torus(in vec3 p) {
    vec2 r = vec2(5.0,1.0);
    vec2 q = vec2(length(p.xz)-r.x,p.y);
    float d = length(q)-r.y;
    return vec2(d,2);
}

vec2 obj_round_box(in vec3 p) {
    float d = length(max(abs(p)-vec3(0.5,2.0,0.5),0.0))-0.25;
    return vec2(d,1);
}

vec2 obj_box( vec3 p, vec3 b ){
  vec3 d = abs(p) - b;
  return vec2(min(max(d.x,max(d.y,d.z)),0.0)+length(max(d,0.0)), 2);
}

vec2 obj_capsule(vec3 p, vec3 a, vec3 b, float r ) {
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return vec2(length( pa - ba*h ) - r, 4);
}

vec2 obj_cross(vec3 p, float r) {
    float inf = 1.0 / 0.0;
    vec2 b1 = obj_box(p.xyz, vec3(inf,r,r));
    vec2 b2 = obj_box(p.yzx, vec3(r,inf,r));
    vec2 b3 = obj_box(p.zxy, vec3(r,r,inf));
    return min(b1,min(b2,b3));
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
    if (d < b.x)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
}

vec2 op_intersect(vec2 a, vec2 b) {
    float d = max(a.x, b.x);
    if (d < b.x)
        return vec2(d,a.y);
    else
        return vec2(d,b.y);
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
    return vec2((sin(p.x * 0.07459)
               + sin(p.y * 0.098131)
               + sin(p.z * 0.05826)) * 0.5, 4);
}

vec2 op_displace(vec3 p, vec2 obj) {
    float d = cos(p.x * 1.0)
            * cos(p.y * 1.0)
            * cos(p.z * 1.0)
            * (sin(float(framecount) * 0.01 + (p.z+p.x+p.y)*1.0 + obj.y*PI/4.0))
            //* (sin(float(framecount) * 0.1 + obj.y*(PI/2.0)) * 0.5 + 0.5)
            * 0.5;
            
    return vec2(obj.x+d, obj.y);
}

vec3 deform( in vec3 p, in float time, out float sca )
{
    float s = 0.034*sqrt(dot(p*p,p*p));
    //float s = 1.0;

    p = p/s;

    //p.xyz += 16.0*sin(0.5*vec3(1.0,1.1,1.3)*time+vec3(0.0,2.0,4.0));
    
    sca = s;
    
	return p;
}


vec2 obj_ufo(vec3 p) {
    return op_union(
        op_union(
            op_union(op_displace(p, obj_round_box(p)),
                     op_displace(p, obj_torus(p))),
            op_displace(p, obj_sphere(p, 1.5)) ),
        op_union(
            op_displace(p, obj_capsule(p, vec3(2.5, 0.0, 0.0), vec3(-2.5, 0.0, 0.0), 0.75 )),
            op_displace(p, obj_capsule(p, vec3(0.0, 0.0, -2.5), vec3(0.0, 0.0, 2.5), 0.75 )) )
        //obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 )
        //op_displace(p, obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 ))
        
        );
}

vec2 op_noise(vec3 p, vec2 obj) {

    return vec2(obj.x + noise(p*0.5), obj.y);
}

vec2 op_rep(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_ufo(q);
}

vec2 obj_repcross(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_cross(q, 1.0);
}

vec2 obj_grid(vec3 p) {
    return obj_repcross(p, vec3(20.0,20.0,20.0));
}
vec2 obj_invertedgrid(vec3 p) {
    float s = 1.0;
    vec3 p2 = (deform(p,float(framecount)*0.0333, s));
    vec2 g = obj_grid(p2) * vec2(s, 1.0);
    return g;
}

vec2 distance_to_obj(in vec3 p) {
//    return op_union(obj_floor(p) + vec2(sin(p.x) + sin(p.y) + sin(p.z), 0.0),
//    return op_union(obj_floor(p),
//                    op_rep(p, vec3(20.0 + sin(float(framecount) / 231.23) * 1.0 *p.x,
//                                   20.0 + cos(float(framecount) / 213.27) * 1.0 *p.y,
//                                   20.0 + cos(float(framecount) / 290.21) * 1.0 *p.z)) );

//  return  op_displace(p,  op_noise(p, obj_floor(p))); 
//  return op_union(obj_floor(p), op_displace(p, obj_sine(p)));
//  return op_union(obj_floor(p), op_noise(p, obj_sine(p)));
    //return op_union(op_noise(p, obj_sine(p)), op_rep(p, vec3(25.0, 20.0, 25.0)));
//    return op_displace(p, op_noise(p, obj_floor(p)));
//    return op_union(obj_floor(p), op_rep(p, vec3(15.0, 10.0, 15.0)));

//    return op_union(
//            op_displace(p, obj_capsule(p, vec3(5.5, 0.0, 0.0), vec3(-5.5, 0.0, 0.0), 2.0 )),
//            op_displace(p, obj_capsule(p, vec3(0.0, 0.0, 5.5), vec3(0.0, 0.0, -5.5), 2.0 )));

//    return obj_fractal(p);
//    return obj_noise(p);

//    return op_union(obj_floor(p), obj_sine(p));
//    return obj_floor(p);
//    return obj_box(p, vec3(5.0,13.0,8.0));
    //return op_intersect(obj_sine(p), obj_grid(p)); 
    //return op_displace(p, obj_invertedgrid(p));
    return obj_invertedgrid(p);
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

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d ) {
    return a + b*cos( 6.28318*(c*t+d) );
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
    else if (i == 4)
        return vec3(0.0, 1.0, 0.0);
    else if (i == 5)
        return vec3(0.8, 0.8, 1.0);
    else if (i == 6) {
//        vec3 col = pal(noise(p * 2.0), vec3(0.5,0.5,0.5),
//                                        vec3(0.5,0.5,0.5),
//                                        vec3(1.0,1.0,1.0),
//                                        vec3(0.0,0.33,0.67) );
        vec3 col = pal(noise(p * 1.0), vec3(0.75,0.75,0.75), // br
                                       vec3(0.75,0.75,0.75), // co
                                       vec3(1.0,1.0,1.0), // freq
                                       vec3(0.02,0.40,0.50) ); // phase

//        float n = noise(p * 10.0);
        return col;
    }
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
    
    float nsteps = 0.0;

//    vec2 cam_dist = distance_to_obj(cam_pos);
//    if (cam_dist.x < 0.0) {
//        gl_FragColor=vec4(0.25*prim_color(cam_pos, int(cam_dist.y)),1.0);
//    }
//    else {
    for(int i=0;i<256;i++) {
        if ((abs(d.x) < 0.001) || (f > maxd)) 
            break;
        f+=d.x;
        p=prp+scp*f;
        d = distance_to_obj(p);
        nsteps = nsteps + 1.0;
    }

    if (f < maxd) {
        c = prim_color(p, int(d.y));
    
        vec3 n = vec3(d.x-distance_to_obj(p-e.xyy).x,
                      d.x-distance_to_obj(p-e.yxy).x,
                      d.x-distance_to_obj(p-e.yyx).x);
        N = normalize(n);

        //simple phong lighting, LightPosition = CameraPosition
        float b=dot(N,normalize(prp-p));
        gl_FragColor=vec4((b*c + pow(b,16.0)) * (1.0-f*0.01), 1.0);

        // display raymarchings steps as brightness
        //nsteps = nsteps / 256.0 ;
        //gl_FragColor=(vec4(nsteps, nsteps, nsteps, 1.0) + vec4(c.xyz, 1.0)) * 0.5;
    }
    else 
        gl_FragColor=vec4(0.0,0.0,0.0,1.0); //background color
//}

}
