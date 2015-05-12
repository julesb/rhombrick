void main()
{   
    gl_texcoord[0] = gl_multitexcoord0;
    gl_position = ftransform();     
}
