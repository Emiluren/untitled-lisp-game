#version 330 core

layout (location = 0) in vec3 vertex_pos;

uniform float x_offset;

void main() {
    gl_Position.xyz = vertex_pos + vec3(x_offset, 0, 0);
    gl_Position.w = 1.0;
}
