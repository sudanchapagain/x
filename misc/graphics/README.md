building
--------

required dependencies:

```
glfw-devel
glew-devel
mesa-libGL-devel
freeglut-devel
```

for nix use this shell:

```
# pure shell because i had weird case of math.h not being available
nix-shell --pure -p stdenv.cc meson ninja pkg-config freeglut libGL libGLU
```

to compile just use meson's standard process

```sh
meson setup build
meson compile -C build
```

to run just find the binaries in build dir.

resource
--------

- <https://learopengl.com>
- <https://learnopengl.com/Guest-Articles/2021/Scene/Frustum-Culling>
