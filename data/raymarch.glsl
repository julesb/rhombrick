#extension GL_EXT_gpu_shader4 : enable

#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif


uniform sampler2D texture;
uniform vec2 texOffset;
uniform float modelscale;
uniform float aspect_ratio;
uniform float viewx;
uniform float viewy;
uniform float mousex;
uniform float mousey;
uniform float zoom;


//uniform vec3 cam_pos;
uniform float time;
//uniform vec2 resolution;

varying vec4 vertColor;
varying vec4 vertTexCoord;

//uniform vec2 mouse;
float PI=3.14159265;
vec3 cam_pos = vec3(5.0, 1.0, 0.0);


vec2 obj_union(in vec2 obj0, in vec2 obj1) {
  if (obj0.x < obj1.x)
  	return obj0;
  else
  	return obj1;
}

vec2 obj_floor(in vec3 p) {
  return vec2(p.y+10.0,0);
}

vec2 obj_sphere(in vec3 p) {
  float d = length(p)-1.5;
  return vec2(d,0);
}

vec2 obj_torus(in vec3 p) {
  vec2 r = vec2(2.5,0.9);
  vec2 q = vec2(length(p.xz)-r.x,p.y);
  float d = length(q)-r.y;
  return vec2(d,2);
}

vec2 obj_round_box(vec3 p) {
  float d = length(max(abs(p)-vec3(1.0,1.0,1.0),0.0))-0.25;
  return vec2(d,1);
}


vec2 op_union(vec2 a, vec2 b) {
  float d = min(a.x, b.x);
  return vec2(d,1);
}

vec2 op_sub(vec2 a, vec2 b) {
  float d = max(a.x, -b.x);
  return vec2(d,1);
}


vec2 op_blend(vec3 p, vec2 a, vec2 b) {
 float s = smoothstep(length(p), 0.0, 1.0);
 float d = mix(a.x, b.x, s);
 return vec2(d,1);
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
    return op_rep(p, vec3(5.0, 5.0, 5.0));
//    return obj_union(obj_floor(p), obj_ufo(p));
//                     op_union(op_blend(p, obj_round_box(p),
//                                        obj_torus(p)),
//                            obj_sphere(p)));
    //return obj_union(obj_floor(p), op_sub(obj_round_box(p), obj_sphere(p)));

//    return obj_union(
//            obj_floor(p),
//            op_union(obj_round_box(p),
//                     obj_sphere(p))); 
    
    //return obj_union(obj_floor(p), obj_round_box(p));
    //return obj_floor(p);
}

vec3 floor_color(in vec3 p) {
  float m = 0.5;
  if (fract(p.x*m)>m) {
    if (fract(p.z*m)>m)
      return vec3(0,0.1,m);
    else
      return vec3(0,0,0);
  }
  else {
    if (fract(p.z*m)>m)
      return vec3(0,0,0);
    else
      return vec3(0,0.3,0);
   }
}

// Primitive color
vec3 prim_c(in vec3 p, int i) {
  if (i == 0)
    return vec3(0.6,0.6,0.8);
  else if (i == 1)
    return vec3(0,0,1);
  else if (i == 2)
    return vec3(0,1,1);
  else
    return vec3(0.5, 0.5, 0.5);
}

void main(void) {
    vec2 cen = vec2(0.5*aspect_ratio, 0.5);
    vec2 tc0 = vertTexCoord.st;
//    vec2 tc1 = tc0 / vec2(aspect_ratio, 1.0);
//    //tc = vec2(mousex, mousey) - (tc - cen) * zoom ;
    vec2 tc = tc0 - vec2(viewx, viewy);

  vec2 q = tc; //vertTexCoord.st; // - vec2(0.5,0.5); // / vec2(aspect_ratio, 1.0);
  // vec2 q = gl_TexCoord[0].xy;
  vec2 vPos = -1.0 + 2.0 * q;

  // Camera up vector.
  vec3 vuv=vec3(0,-1,0); 
  
  // Camera lookat.
  vec3 vrp=vec3(0,0,0);

  float mx=mousex*PI*2.0 * 2.0;
  float my=mousey*PI/2.01 * 2.0;
  vec3 prp= // vec3(viewx*10.0, 1.0, viewy*10.0) 
            + vec3(cos(my)*cos(mx),sin(my),cos(my)*sin(mx))*6.0; 
  //vec3 prp = vec3(viewx*10.0, 1.0, viewy*10.0) + prp;
  //vec3 prp = cam_pos;

  // Camera setup.
  vec3 vpn=normalize(vrp-prp);
  vec3 u=normalize(cross(vuv,vpn));
  vec3 v=cross(vpn,u);
  vec3 vcv=(prp+vpn);
  //vec3 scrCoord=vcv+vPos.x*u*resolution.x/resolution.y+vPos.y*v;
  vec3 scrCoord=vcv+vPos.x*u*0.8+vPos.y*v*0.8;
  vec3 scp=normalize(scrCoord-prp);

  // Raymarching.
  const vec3 e=vec3(0.02,0,0);
  const float maxd=150.0; //Max depth
  vec2 d=vec2(0.02,0.0);
  vec3 c,p,N;

  float f=1.0;
  for(int i=0;i<256;i++)
  {
    if ((abs(d.x) < .001) || (f > maxd)) 
      break;
    
    f+=d.x;
    p=prp+scp*f;
    d = distance_to_obj(p);
  }

  if (f < maxd)
  {
    // y is used to manage materials.
    if (d.y==0.0) 
      c=floor_color(p);
    else
      c=prim_c(p, int(d.y));
    
    vec3 n = vec3(d.x-distance_to_obj(p-e.xyy).x,
                  d.x-distance_to_obj(p-e.yxy).x,
                  d.x-distance_to_obj(p-e.yyx).x);
    N = normalize(n);
    float b=dot(N,normalize(prp-p));
    //simple phong lighting, LightPosition = CameraPosition
    gl_FragColor=vec4((b*c+pow(b,16.0))*(1.0-f*.01),1.0);
    //gl_FragColor=vec4(0,1,0,1);
  }
  else 
    gl_FragColor=vec4(0.0,0.0,0.0,1.0); //background color

}
