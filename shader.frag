#version 330
uniform sampler2D tex;

in vec2 f_tex_coord;
out vec4 out_color;

void main() {
    out_color = texture(tex, f_tex_coord);
}
