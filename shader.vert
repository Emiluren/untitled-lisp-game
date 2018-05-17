#version 330
const int MAX_BONES = 100;

uniform mat4 view_m;
uniform mat4 bone_offsets[MAX_BONES];

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texture_coord;
layout (location = 3) in ivec4 bone_ids;
layout (location = 4) in vec4 bone_weights;

out vec3 f_normal;
out vec2 f_tex_coord;

void main() {
    mat4 bone_transform =
        bone_offsets[bone_ids[0]] * bone_weights[0] +
        bone_offsets[bone_ids[1]] * bone_weights[1] +
        bone_offsets[bone_ids[2]] * bone_weights[2] +
        bone_offsets[bone_ids[3]] * bone_weights[3];
    // 70 is to scale down (TODO: scale using matrix instead)
    vec4 local_position = bone_transform * vec4(position, 70);
    gl_Position = view_m * local_position;
    f_tex_coord = texture_coord;
}
