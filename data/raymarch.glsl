#extension GL_EXT_gpu_shader4 : enable

#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

#define MANDELBULB 0
#define MANDELBOX 1
#define FRACTALTYPE MANDELBULB
//#define FRACTALTYPE -1

/*
   Based on tutorial at:
   http://www.geeks3d.com/20130524/building-worlds-with-distance-functions-in-glsl-raymarching-glslhacker-tutorial-opengl/
*/
vec3 prim_cols[9];

uniform sampler2D texture;
uniform float aspect_ratio;
uniform float mousex;
uniform float mousey;
uniform float framecount;
uniform float swidth;
uniform float sheight;
uniform vec3 cam_pos;
uniform vec3 cam_lookat;
uniform float cam_fov;
uniform float time;
uniform float blend_coef;
varying vec4 vertColor;
varying vec4 vertTexCoord;
uniform float ray_hit_epsilon;
uniform float palette_offset;
uniform float gamma;
uniform float glow_intensity;

float PI=3.14159265;

const float NOISE_DETAIL =0.5;

const float MB_INNER_SPHERE = 0.72;
const float MBOX_SCALE = 8.0;
const float MBULB_SCALE = 128.0;

vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }

float length2(vec3 p, float n) {
//    (x^n+y^n+z^n)^(1/n)
    return pow((pow(p.x,n) + pow(p.y, n) + pow(p.z, n)), 1.0 / n);
}

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

float sd_plane(in vec3 p, in vec3 n, in float o) {
    return dot(p, n) + o; 
}

vec2 obj_floor(in vec3 p) {
    return vec2(p.y+10.0,0.0);
}

vec2 obj_sphere(in vec3 p, float r) {
    float d = length(p) -r;
    return vec2(d,9.0);
}

vec2 obj_torus(in vec3 p) {
    vec2 r = vec2(5.0,1.0);
    vec2 q = vec2(length(p.xz)-r.x,p.y);
    float d = length(q)-r.y;
    return vec2(d,2.0);
}

vec2 obj_round_box(in vec3 p) {
    float d = length(max(abs(p)-vec3(0.75,2.0,0.75),0.0))-0.25;
    return vec2(d,1.0);
}

vec2 obj_box( vec3 p, vec3 b ){
  vec3 d = abs(p) - b;
  return vec2(min(max(d.x,max(d.y,d.z)),0.0)+length(max(d,0.0)), 6.0);
}

vec2 obj_cylinder( vec3 p, vec3 c ) {
  return vec2(length(p.xz-c.xy)-c.z, 8.0);
}

vec2 obj_capsule(vec3 p, vec3 a, vec3 b, float r ) {
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return vec2(length( pa - ba*h ) - r, 3.0);
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


vec2 op_sblend(vec3 p, vec2 a, vec2 b) {
    float sm = smin(a.x, b.x, blend_coef);
    float c = smin(a.y, b.y, blend_coef);
    return vec2(sm, c);
}

vec2 obj_sine(vec3 p) {
    //vec3 pf = floor(p);
    return vec2(//smoothstep(0.0, 0.5, 
                (sin(floor(p.x+0.0) * 0.2)
               + sin(floor(p.y+0.0) * 0.2)
               + sin(floor(p.z+0.0) * 0.2))
               * 0.5 + 0.5, 5.0);
}

vec2 op_displace(vec3 p, vec2 obj) {
    float m = framecount * 0.01;
    vec3 v = cos(p*0.5+m);
    float d = v.x*v.y*v.z * 2.0;
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
    return op_sblend(p,
        op_sub(
            op_displace(p, obj_sphere(p, 1.25)),
            op_union(op_displace(p, obj_round_box(p)),
                     op_displace(p, obj_torus(p)))),
        op_sblend(p,
            op_displace(p, obj_capsule(p, vec3(3.5, 0.0, 0.0), vec3(-3.5, 0.0, 0.0), 0.95 )),
            op_displace(p, obj_capsule(p, vec3(0.0, 0.0, -3.5), vec3(0.0, 0.0, 3.5), 0.95 )) )
        //obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 )
        //op_displace(p, obj_capsule(p, vec3(0.0, 1.5, 0.0), vec3(0.0, -1.5, 0.0), 0.5 ))
        
        );
}

vec2 obj_invertedufo(vec3 p) {
    float s = 1.0;
    vec3 p2 = (deform(p,float(framecount)*0.0333, s));
    vec2 g = obj_ufo(p2) * vec2(s, 1.0);
    return g;
}



/*
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
*/
vec2 op_noise(vec3 p, vec2 obj) {

    return vec2(obj.x + 0.5*snoise(p*NOISE_DETAIL), obj.y);
}

vec2 obj_noise(vec3 p) {
    return vec2(length(p), 5.1);
}

vec2 op_rep(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_ufo(q);
}

vec2 obj_cross(vec3 p, float r) {
    float inf = 1.0 / 0.0;
    //vec2 b1 = obj_box(p.xyz, vec3(inf,r,r));
    //vec2 b2 = obj_box(p.yzx, vec3(r,inf,r));
    //vec2 b3 = obj_box(p.zxy, vec3(r,r,inf));
    vec2 b1 = obj_cylinder(p.xyz, vec3(0.0,0.0,r));
    vec2 b2 = obj_cylinder(p.yzx, vec3(0.0,0.0,r));
    vec2 b3 = obj_cylinder(p.zxy, vec3(0.0,0.0,r));
    //return min(b1,min(b2,b3));
    return op_sblend(p, b1, op_sblend(p, b2, b3));
}

vec2 obj_repcross(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_cross(q, 2.0);
}

vec2 obj_grid(vec3 p) {
    return obj_repcross(p, vec3(25.0,25.0,25.0));
}
vec2 obj_invertedgrid(vec3 p) {
    float s = 1.0;
    vec3 p2 = (deform(p,float(framecount)*0.01333, s));
    vec2 g = obj_grid(p2) * vec2(s, 1.0);
    return g;
}
vec2 obj_invertedgrid_rep(vec3 p, vec3 c) {
    vec3 q = mod(p,c)-0.5*c;
    return obj_invertedgrid(q) ;
}

float sd_tetra(in vec3 p) {
    float s = 2.0;
  	vec3 a1 = normalize(vec3(1,1,1));
	vec3 a2 = normalize(vec3(-1,-1,1));
	vec3 a3 = normalize(vec3(1,-1,-1));
	vec3 a4 = normalize(vec3(-1,1,-1));
    float p1 = sd_plane(p, a1, s);
    float p2 = sd_plane(p, a2, s);
    float p3 = sd_plane(p, a3, s);
    float p4 = sd_plane(p, a4, s);
    float d = min(min(min(p1, p2), p3), p4);
    return -d;
}

vec2 obj_tetrahedron(in vec3 p, in float s) {
/*
    vec3 a1 = normalize(vec3(1,1,1));
	vec3 a2 = normalize(vec3(-1,-1,1));
	vec3 a3 = normalize(vec3(1,-1,-1));
	vec3 a4 = normalize(vec3(-1,1,-1));
    float p1 = sd_plane(p, a1, s);
    float p2 = sd_plane(p, a2, s);
    float p3 = sd_plane(p, a3, s);
    float p4 = sd_plane(p, a4, s);
    float d = min(min(min(p1, p2), p3), p4);
    return vec2(-d, 1.0);
*/
    return vec2(sd_tetra(p/s) * s, 4.0);
}

float sd_mandelbulb(in vec3 pos, out float AO) {
	vec3 z = pos;
	float dr = 1.0;
	float r = 0.0;
    int iters = 32;
    float power = 8.0;
    float bailout = 2.0;
    AO = 1.0;
	for (int i = 0; i < iters ; i++) {
        AO *= 0.725;
		r = length(z);
		if (r>bailout) {
            AO = min((AO + 0.075) * 4.1, 1.0);
            //return min(length(pos) - MB_INNER_SPHERE, 0.5 * log(r) * r / dr);
            break;
        }
		
		// convert to polar coordinates
		float theta = acos(z.z/r);
		float phi = atan(z.y,z.x);
		dr =  pow( r, power-1.0)*power*dr + 1.0;
		
		// scale and rotate the point
		float zr = pow( r,power);
		theta = theta*power;
		phi = phi*power;
		
		// convert back to cartesian coordinates
		z = zr*vec3(sin(theta)*cos(phi), sin(phi)*sin(theta), cos(theta));
		z+=pos;
	}
	return 0.5*log(r)*r/dr;
}

float sd_mandelbulb(vec3 p) {
    float ignore;
    return sd_mandelbulb(p, ignore);
}


void sphereFold(inout vec3 z, inout float dz)
{
	float r2 = dot(z,z);
	if (r2 < 0.5)
    { 
		float temp = 2.0;
		z *= temp;
		dz*= temp;
	}
    else if (r2 < 1.0)
    { 
		float temp = 1.0 / r2;
		z *= temp;
		dz*= temp;
	}
}

void boxFold(inout vec3 z, inout float dz)
{
	z = clamp(z, -1.0, 1.0) * 2.0 - z;
}

float sd_mandelbox(vec3 z, out float AO) {
    //AO = 1.0;
    int iters = 20;
    float scale = 2.0;
	vec3 offset = z;
	float dr = 1.0;
    //float aostep = 1.0 / 
	for (int n = 0; n < iters; n++) {
        //AO *= 0.725;
		boxFold(z,dr);
		sphereFold(z,dr);
        z = scale * z + offset;
        dr = dr * abs(scale) + 1.0;
	}
	float r = length(z);
    AO = log(r)/32.0;
    //AO = 1.0 - (r / abs(dr))/ float(iters);

    //AO = min((AO + 0.075) * 4.1, 1.0);
	return  r / abs(dr);
}

float sd_mandelbox(vec3 p) {
	float ignore;
	return sd_mandelbox(p, ignore);
}

vec2 obj_invertedmandel(vec3 p) {
    float s = 1.0;
    vec3 p2 = (deform(p,float(framecount)*0.01333, s));
    vec2 g = vec2(sd_mandelbox(p2), 8.0) * vec2(s, 1.0);
    return g;
}

float sierp(vec3 z) {
    float scale = 2.0;
	vec3 a1 = vec3(1,1,1);
	vec3 a2 = vec3(-1,-1,1);
	vec3 a3 = vec3(1,-1,-1);
	vec3 a4 = vec3(-1,1,-1);
	vec3 c;
	int n = 0;
	float dist, d;
	while (n < 8) {
		 //c = a1; dist =  sd_tetra(z-a1);
	     //d = sd_tetra(z-a2); if (d < dist) { c = a2; dist=d; }
		 //d = sd_tetra(z-a3); if (d < dist) { c = a3; dist=d; }
		 //d = sd_tetra(z-a4); if (d < dist) { c = a4; dist=d; }
		 c = a1; dist = length(z-a1);
	     d = length(z-a2); if (d < dist) { c = a2; dist=d; }
		 d = length(z-a3); if (d < dist) { c = a3; dist=d; }
		 d = length(z-a4); if (d < dist) { c = a4; dist=d; }
		z = scale*z-c*(scale-1.0);
		n++;
    }
 
	return length(z) * pow(scale, float(-n));
}

vec2 obj_sierp(vec3 p) {
    return vec2(sierp(p), 5.0);
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
    //return op_rep(p, vec3(25.0, 20.0, 25.0));
//    return op_displace(p, op_noise(p, obj_floor(p)));

    // blobby noise blend infinite grid
    //return op_noise(p, op_sblend(p, op_displace(p, obj_grid(p)),
    //                                op_rep(p, vec3(15.0, 15.0, 15.0))));

//      return           obj_invertedufo(p);

    // blobby infinite grid
    //return op_sblend(p, op_displace(p, obj_grid(p)),
    //                    op_rep(p, vec3(15.0, 15.0, 15.0)));

//    return op_sblend(p, op_displace(p, obj_grid(p)),
//                        op_rep(p, vec3(15.0, 15.0, 15.0)));

//    return op_sblend(p,
//            op_displace(p, obj_capsule(p, vec3(5.5, 0.0, 0.0), vec3(-5.5, 0.0, 0.0), 2.0 )),
//            op_displace(p, obj_capsule(p, vec3(0.0, 0.0, 5.5), vec3(0.0, 0.0, -5.5), 2.0 )));

//    return op_union(obj_cylinder(p, vec3(1.0, 1.0, 1.0)),
//                    op_noise(p, op_displace(p, obj_grid(p))));

//    return op_noise(p, obj_invertedgrid_rep(p, vec3(8.0,8.0,8.0)));

//    return op_sblend(p, obj_floor(p), obj_sine(p));

    // low freq noise blend plane
    //return op_noise(p, op_sblend(p, obj_floor(p),
    //                                op_displace(p, obj_sphere(p, 20.0) )));
//    return obj_box(p, vec3(5.0,13.0,8.0));
//    return op_intersect(obj_sine(p), obj_grid(p)); 
//    
//    return op_intersect(op_rep(p, vec3(10.0,10.0,10.0)),
//                    obj_invertedgrid_rep(p, vec3(10.0,10.0,10.0) ));
    
    //return op_displace(p, obj_grid(p)); //, obj_invertedgrid_rep(p, vec3(4.0,4.0,4.0) );

//    return obj_invertedgrid(p);



    float s = MBOX_SCALE;
////    vec2 g = op_noise(p, op_sblend(p, op_displace(p, obj_grid(p)),
////                                    op_rep(p, vec3(15.0, 15.0, 15.0))));
//    vec2 g = op_noise(p, obj_invertedgrid(p/s) * vec2(1.0/s, 1.0));
//    return g;
//    return op_union(obj_floor(p), obj_sine(p));
    
    //return op_union(obj_floor(p),
    //            obj_sine(p*s) * vec2(1.0/s, 1.0/s));
    
    //return op_noise(p, obj_sine(p*s) * vec2(1.0/s, 1.0));

//    return vec2(g.x, g.y);

    //return obj_cylinder(p, vec3(0.0, 0.0, 2.0));

    //return obj_sierp(p*s) * vec2(1.0/s, 1.0);
 //   return op_union(obj_floor(p),
 //                   obj_sierp(p/s) * vec2(s, 1.0));

#if FRACTALTYPE == MANDELBULB
    return op_sblend(p, vec2(sd_mandelbulb(p/MBULB_SCALE)*MBULB_SCALE, 8.0),
                        obj_sphere(p, MBULB_SCALE * 0.5));
    //return vec2(sd_mandelbulb(p/MBULB_SCALE)*MBULB_SCALE, 8.0);

    //return op_union(obj_floor(p),
    //                vec2(sd_mandelbulb(p/s)*s, 9.0));
//    return op_sub(
//                  obj_sphere(p, MBULB_SCALE * 1.0),
//            vec2(sd_mandelbulb(p/MBULB_SCALE)*MBULB_SCALE, 8.0));
#elif FRACTALTYPE == MANDELBOX

    //return obj_invertedmandel(p);

    return vec2( sd_mandelbox(p/MBOX_SCALE)*MBOX_SCALE, 8.0);
    
    //return vec2(sd_mandelbox(p), 8.0);

#endif





//    return op_intersect(vec2(sd_mandelbulb(p/s)*s, 8.0),
//                        vec2(sd_plane(p, vec3(1.0,0.0,0.0), 0.0), 4.0) );
    //return obj_plane(p, vec3(0.0,1.0,0.0));
/*    
    return
        op_union(
          obj_floor(p),
          op_sblend(p,
            obj_tetrahedron(p, 3.0),
            obj_capsule(p, vec3(10.5, 0.0, 0.0), vec3(-10.5, 0.0, 0.0), 5.0 )))  ;
*/
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
    return a + b*cos( palette_offset +  6.28318*(c*t+d) );
}

vec3 ansi_gradient(float t) {
	return mod(floor(t * vec3(8.0, 4.0, 2.0)), 2.0);
}

float sqr(float n) {return n*n;}

vec3 rainbow_gradient(float t) {
	vec3 c = 1.0 - pow(abs(vec3(t) - vec3(0.65, 0.5, 0.2)) * vec3(3.0, 3.0, 5.0), vec3(1.5, 1.3, 1.7));
	c.r = max((0.15 - sqr(abs(t - 0.04) * 5.0)), c.r);
	c.g = (t < 0.5) ? smoothstep(0.04, 0.45, t) : c.g;
	return clamp(c, 0.0, 1.0);
}
vec3 heatmap_gradient(float t) {
	return clamp((pow(t, 1.5) * 0.8 + 0.2) * vec3(smoothstep(0.0, 0.35, t) + t * 0.5, smoothstep(0.5, 1.0, t), max(1.0 - t * 1.7, t * 7.0 - 6.0)), 0.0, 1.0);
}
vec3 neon_gradient(float t) {
	return clamp(vec3(t * 1.3 + 0.1, sqr(abs(0.43 - t) * 1.7), (1.0 - t) * 1.7), 0.0, 1.0);
}

vec3 stripe_gradient(float t) {
	return vec3(mod(floor(t * 32.0), 2.0) * 0.2 + 0.8);
}

vec3 grey_gradient(float t) {
	return vec3(clamp(0.0,1.0, t));
}

vec3 fire_gradient(float t) {
	return max(pow(vec3(min(t * 1.02, 1.0)), vec3(1.7, 25.0, 100.0)), 
			   vec3(0.06 * pow(max(1.0 - abs(t - 0.35), 0.0), 5.0)));
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


// Rainbow (more yellow, narrower green, deeper red)
vec3 rainbow2_gradient(float t) {
    return pal(t, vec3(0.55,0.4,0.3),vec3(0.50,0.51,0.35)+0.1,vec3(0.8,0.75,0.8),vec3(0.075,0.33,0.67)+0.21);
}

vec3 prim_color(in vec3 p, float i) {
    prim_cols[0] = vec3(0.5,0.5,0.5);
    prim_cols[1] = vec3(1.0,0.0,0.0);
    prim_cols[2] = vec3(1.0,0.25,0.0);
    prim_cols[3] = vec3(1.0,1.0,0.0);
    prim_cols[4] = vec3(0.0,1.0,0.0);
    prim_cols[5] = vec3(0.0,0.0,1.0);
    prim_cols[6] = vec3(1.0,0.0,1.0);
    prim_cols[7] = vec3(0.5,0.0,1.0);
    prim_cols[8] = vec3(1.0,1.0,1.0);

    if (i == 0.0)
        return floor_color(p);
    else if (i <= 8.0) {
        return prim_cols[int(i)];
    }
    else {
         return pal((0.5*snoise(p*NOISE_DETAIL)+framecount*0.01)*2.0*PI, vec3(0.5),
                                                           vec3(0.5),
                                                           vec3(0.5),
                                                           //p);
                                                           vec3(0.,1.0,0.0) + 1.0 );
/*
        int i1 = int(i);
        int i2 = int(mod(floor(i)+1.0, 8.0));
        vec3 c1 = prim_cols[i1];
        vec3 c2 = prim_cols[i2];
        float t = fract(i);
        return mix(c1, c2, t);
        */
    }

/*
    else if (i == 1.0)
        return vec3(1.0,0.0,0.0);
    else if (i == 2.0)
        return vec3(0,0,1);
    else if (i == 3.0)
        return vec3(1,1,0);
    else if (i == 4.0)
        return vec3(0.0, 1.0, 0.0);
    else if (i == 5.0)
        return vec3(0.8, 0.8, 1.0);
    else if (i == 6.0) {
//        vec3 col = pal(noise(p * 2.0), vec3(0.5,0.5,0.5),
//                                        vec3(0.5,0.5,0.5),
//                                        vec3(1.0,1.0,1.0),
//                                        vec3(0.0,0.33,0.67) );
//        vec3 col = pal(noise(p * 1.0), vec3(0.75,0.75,0.75), // br
//                                       vec3(0.75,0.75,0.75), // co
//                                       vec3(1.0,1.0,1.0), // freq
//                                       vec3(0.02,0.40,0.50) ); // phase
        //vec3 col =pal(noise(p*4.0),vec3(0.5),vec3(0.55),vec3(0.45),vec3(0.00,0.10,0.20) + 0.47 );
        float n = noise(p*4.0);
        vec3 col =pal(n, vec3(0.5),vec3(0.55),vec3(0.45),vec3(0.0,0.1,0.2) + 0.47 );
//        float n = noise(p * 10.0);
        return col;
    }
    else {
        float n = length(p); //noise(p*4.0);
//        vec3 col =pal((0.5*snoise(p*NOISE_DETAIL)+0.5)*2.0*PI, vec3(0.5),
//                                                           vec3(0.5),
//                                                           vec3(0.75),
//                                                           //p);
//                                                           vec3(0.22,0.3,0.2) + 0.0 );

        vec3 col =pal(i*1.0, vec3(0.5),vec3(0.5),vec3(0.45),vec3(0.60,0.80,0.99) + 0.47 );
//        float n = noise(p * 10.0);
        return col;
     }
     */
}

vec3 lambert(vec3 p, vec3 n, vec3 l) {
    return vec3(dot(normalize(l-p), n));
}


// http://www.iquilezles.org/www/articles/rmshadows/rmshadows.htm

float iqshadow( in vec3 ro, in vec3 rd, float mint, float maxt ) {
    for( float t=mint; t < maxt; ) {
        float h = distance_to_obj(ro + rd*t).x;
        if( h<0.001 )
            return 0.0;
        t += h;
    }
    return 1.0;
}

float iqsoftshadow( in vec3 ro, in vec3 rd, float mint, float maxt, float k ) {
    float res = 1.0;
    for( float t=mint; t < maxt; ) {
        float h = distance_to_obj(ro + rd*t).x;
        if( h<0.01 )
            return 0.0;
        res = min( res, k*h/t );
        t += h;
    }
    return res;
}

void main(void) {
    vec2 q = vertTexCoord.st;
    vec2 vPos = -vec2(aspect_ratio, 1.0) + 2.0 * q;
    //vec2 vPos = -1.0 + 2.0 * q;

    float lightspeed = 0.05;
    float lightrad = 800.0;
    vec3 lightpos = vec3(cos(framecount*lightspeed)*lightrad,
                         400.0,
                         sin(framecount*lightspeed)*lightrad);
    //vec3 lightpos = normalize(cam_pos*vec3(-1.0, 1.0, 1.0)) * 200.0;
    //vec3 lightpos =  vec3(400.0,400.0,400.0);
    //vec3 lightpos = cam_pos;

    // Camera up vector.
    vec3 vuv=vec3(0,-1,0); 

    // Camera lookat.
    vec3 vrp = cam_lookat;

    // Camera pos
    vec3 prp = cam_pos;

    // Camera setup.
    vec3 vpn=normalize(vrp-prp);
    //vec3 u = normalize(cam_pos);
    vec3 u=normalize(cross(vuv,vpn));
    vec3 v=cross(vpn,u);
    vec3 vcv=(prp+vpn);
    //vec3 scrCoord=vcv+vPos.x*u*swidth/sheight+vPos.y*v;
    //vec3 scrCoord=vcv+vPos.x*u*resolution.x/resolution.y+vPos.y*v;
    float fov = cam_fov; //PI/3.0;
    vec3 scrCoord=vcv + vPos.x*u*fov
                      + vPos.y*v*fov;
    vec3 scp=normalize(scrCoord-prp);

    // Raymarching.
    const vec3 e=vec3(0.02,0,0);
    const float maxd=200.0; //Max depth
    vec2 d=vec2(0.01,0.0);
    vec3 c,p,N;

    float f=0.01; // near plane?
    
    float nsteps = 0.0;

//    vec2 cam_dist = distance_to_obj(cam_pos);
//    if (cam_dist.x < 0.0) {
//        gl_FragColor=vec4(0.25*prim_color(cam_pos, int(cam_dist.y)),1.0);
//    }
//    else {
    for(int i=0;i<256;i++) {
        if ((abs(d.x) < ray_hit_epsilon) || (f > maxd)) {
            break;
        }
        f+=d.x;
        p=prp+scp*f;
        d = distance_to_obj(p);
        nsteps = nsteps + 1.0;
    }


    float AO;
#if FRACTALTYPE == MANDELBULB
        float m = sd_mandelbulb(p/MBULB_SCALE, AO); //*MBULB_SCALE;
#elif FRACTALTYPE == MANDELBOX
        float m = sd_mandelbox(p/MBOX_SCALE, AO); // * MBOX_SCALE;
#endif

#if FRACTALTYPE < 0
    vec3 glowcol = (rainbow2_gradient(AO*1.0));// + vec3(1.0, 1.0, 1.0)) * 0.5 ;
#else
    vec3 glowcol = vec3(1.0, 1.0, 1.0);
#endif
    vec3 glow = vec3(nsteps/256.0) * glowcol * glow_intensity;

    if (f < maxd) {
 
        //float AO;
        //c = pal(AO*2.0*PI, vec3(0.5), vec3(0.5), vec3(0.3, 0.3, 0.3), vec3(0.0,0.0,0.5) + 0.0 );

        vec3 n = vec3(d.x-distance_to_obj(p-e.xyy).x,
                      d.x-distance_to_obj(p-e.yxy).x,
                      d.x-distance_to_obj(p-e.yyx).x);
        N = normalize(n);

        //float cam_dist = distance_to_obj(cam_pos).x;
        //vec3 dotfade = vec3(smoothstep(0.1, 0.5, f)) * c * vec3(hash(f))* f ;
        //nsteps = nsteps / 256.0 ;
        //vec3 glow = vec3(nsteps/256.0) *  vec3(0.8,0.8,1.0) * 1.0;
        //vec3 glow = vec3(nsteps/256.0) * c * 1.0;

        //simple phong lighting, LightPosition = CameraPosition
        float b=dot(N,normalize(prp-p));
        //gl_FragColor=vec4(glow, 1.0);
        
        
        //vec3 fc = vec3(glow + dotfade * (b*c + pow(b,32.0)) * (1.0-f*0.01));
        //vec3 fc = vec3(AO* (b*c + pow(b,32.0)) * (1.0-f*0.005));
        //vec3 fc = vec3(0.0*glow + AO* c*1.0);
#if FRACTALTYPE >= 0
        float amb_shad = 0.125;
        float amb_lamb = 0.5;

        c =  rainbow2_gradient(AO*1.0); // * hash(f);
        //c = pow(c, vec3(gamma));
        vec3 phong =  vec3((b + pow(b,32.0))) * 0.5; // * (1.0-f*0.005));
        vec3 lamb = amb_lamb + (1.0 - amb_lamb) * lambert(p, N, lightpos);
        
        float shad = amb_shad + (1.0 - amb_shad) * iqsoftshadow(p, normalize(lightpos), 0.1, 100.0, 4.0);

        //*
        //vec3 fc = (lamb*1.0 + phong + glow) * AO * c * shad;
        vec3 fc = (phong + lamb) * shad  * c + glow*shad;
        //vec3 fc = (lamb*AO + phong*shad ) * c + glow;
        //vec3 fc = shad*c + glow;
        fc = pow(fc, vec3(gamma));
        gl_FragColor = vec4(fc, 1.0);
#else
        c = prim_color(p, d.y);
        vec3 fc = vec3((b*c + pow(b,16.0)) * (1.0-f*0.005));
        gl_FragColor= vec4((fc+glow)*1.0, 1.0);

#endif
        // *
        // display raymarchings steps as brightness
        //nsteps = nsteps / 256.0 ;
        //gl_FragColor=(vec4(nsteps, nsteps, nsteps, 1.0) + vec4(c.xyz, 1.0)) * 0.5;
    }
    else {
        //gl_FragColor=vec4(0.8,0.8,1.0,1.0); //background color
        //vec2 mp = vec2(mousex, mousey) ;
        //vec2 uv = vec2(0.5  + (cos(mp.x*2.0*PI) * 0.5),
        //          0.5  + (sin(mp.x*2.0*PI) * 0.5));
        //vec2 uv = vec2(vpn.y * PI*2.0, vpn.z * PI*0.99);
        
        //vec2 uv = (q + vpn.xy) * vec2(1.0, 1.0);
        //vec4 texcol = texture2D(texture, uv);
        //vec4 texcol = texture2D(texture, (q+vpn.xy) * 0.5);
        //gl_FragColor=vec4(texcol.xyz, 1.0); //background color
        vec3 bgcol = vec3(0.0,0.0,0.0);
        //vec3 bgcol = rainbow2_gradient(mousey* PI);
        // vec3 glow = vec3(nsteps/256.0) *  vec3(0.8,0.8,1.0) * 1.0;
        gl_FragColor=vec4(bgcol+glow,1.0); //background color
    }

}
