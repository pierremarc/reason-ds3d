open ReasonMatrix;
open ReasonMatrix.Common;

let ftransmat4opt = (om: option(mat4), v: vec3) =>
  switch (om) {
  | None => v
  | Some(m) => Vec3Mod.transformMat4(m, v)
  };

let dropZ = (v: vec3) => Vec2Mod.fromValues(Vec3.fst3(v), Vec3.snd3(v));

let scalarDiv2 = (s: float, a: vec2) =>
  Vec2Mod.fromValues(fst(a) /. s, snd(a) /. s);

let scalarMul2 = (s: float, a: vec2) =>
  Vec2Mod.fromValues(fst(a) *. s, fst(a) *. s);

let zAxis = Vec3Mod.fromValues(0.0, 0.0, 1.0);

type camera = {
  pos: vec3,
  target: vec3,
  viewport: vec2,
};
type transform = (vec3, bool) => vec2;

let getTranformFunction: camera => transform =
  cam => {
    let (trgt0, trgt1, trgt2) = cam.target;
    let tref = Vec3Mod.fromValues(trgt0, trgt1, trgt2 +. 10.0);
    let ct = Vec3Mod.substract(cam.target, cam.pos);
    let tref0 = Vec3Mod.substract(tref, cam.pos);

    let angle = Vec3Mod.angle(ct, zAxis);
    let normal = Vec3Mod.cross(zAxis, ct);
    let rotMat = Mat4Mod.fromRotation(normal, -. angle);

    let trefRot =
      switch (rotMat) {
      | None => tref0
      | Some(m) => Vec3Mod.transformMat4(m, tref0)
      };

    let dist = Vec3Mod.distance(cam.pos, cam.target);

    let ref2d = Vec2Mod.fromValues(Vec3.fst3(trefRot), Vec3.snd3(trefRot));
    let refAngle = Vec2Mod.angle(Vec2Mod.fromValues(0.0, -1.0), ref2d);

    let zrot =
      Mat3Mod.fromRotation(fst(ref2d) < 0.0 ? refAngle : -. refAngle);

    let toCenter =
      Mat3Mod.fromTranslation(scalarDiv2(2.0, cam.viewport));

    (pt, perspective) => {
      let scale =
        switch (perspective) {
        | false => fst(cam.viewport) /. dist
        | true => fst(cam.viewport) /. Vec3Mod.distance(cam.pos, pt)
        };
      pt
      |> Vec3Mod.substract(cam.pos)
      |> ftransmat4opt(rotMat)
      |> dropZ
      |> Vec2Mod.transformMat3(zrot)
      |> scalarMul2(scale)
      |> Vec2Mod.transformMat3(toCenter);
    };
  };