#version 330 core

layout (location = 0) in vec3 vertex_pos;

uniform mat4 mat;

void main() {
    gl_Position = mat * vec4(vertex_pos, 1.0);
}
