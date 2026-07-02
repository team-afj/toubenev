include Brr.El.Style

let grid_template_columns = Jstr.v "grid-template-columns"
let grid_template_rows = Jstr.v "grid-template-rows"
let grid_auto_columns = Jstr.v "grid-auto-columns"
let grid_auto_rows = Jstr.v "grid-auto-rows"

module Color = struct
  type rgb = int * int * int

  let make red green blue = (red, green, blue)

  let to_css (r, g, b) =
    "rgb(" ^ string_of_int r ^ " " ^ string_of_int g ^ " " ^ string_of_int b
    ^ ")"

  (* See https://stackoverflow.com/a/49321304  *)

  (** Algorithm MarkMix Input: color1: Color, (rgb) The first color to mix
      color2: Color, (rgb) The second color to mix mix: Number, (0..1) The mix
      ratio. 0 ==> pure Color1, 1 ==> pure Color2 Output: color: Color, (rgb)
      The mixed color

      //Convert each color component from 0..255 to 0..1 r1, g1, b1 ←
      Normalize(color1) r2, g2, b2 ← Normalize(color1)

      //Apply inverse sRGB companding to convert each channel into linear light
      r1, g1, b1 ← sRGBInverseCompanding(r1, g1, b1) r2, g2, b2 ←
      sRGBInverseCompanding(r2, g2, b2)

      //Linearly interpolate r, g, b values using mix (0..1) r ←
      LinearInterpolation(r1, r2, mix) g ← LinearInterpolation(g1, g2, mix) b ←
      LinearInterpolation(b1, b2, mix)

      //Compute a measure of brightness of the two colors using empirically
      determined gamma gamma ← 0.43 brightness1 ← Pow(r1+g1+b1, gamma)
      brightness2 ← Pow(r2+g2+b2, gamma)

      //Interpolate a new brightness value, and convert back to linear light
      brightness ← LinearInterpolation(brightness1, brightness2, mix) intensity
      ← Pow(brightness, 1/gamma)

      //Apply adjustment factor to each rgb value based if ((r+g+b) != 0) then
      factor ← (intensity / (r+g+b)) r ← r * factor g ← g * factor b ← b *
      factor end if

      //Apply sRGB companding to convert from linear to perceptual light r, g, b
      ← sRGBCompanding(r, g, b)

      //Convert color components from 0..1 to 0..255 Result ← MakeColor(r, g, b)
      End Algorithm MarkMix *)
  let mark_mix (c1 : rgb) (c2 : rgb) =
    let clamp_float lo hi x = max lo (min hi x) in
    let clamp_01 x = clamp_float 0.0 1.0 x in
    let normalize_channel x =
      let x = clamp_float 0.0 255.0 (float_of_int x) in
      x /. 255.0
    in
    let srgb_inverse_compand x =
      if x <= 0.04045 then x /. 12.92 else ((x +. 0.055) /. 1.055) ** 2.4
    in
    let srgb_compand x =
      if x <= 0.0031308 then 12.92 *. x
      else (1.055 *. (x ** (1.0 /. 2.4))) -. 0.055
    in
    let lerp a b t = a +. ((b -. a) *. t) in
    let to_int_channel x =
      int_of_float (floor ((clamp_01 x *. 255.0) +. 0.5))
    in
    let r1, g1, b1 = c1 in
    let r2, g2, b2 = c2 in
    let r1 = srgb_inverse_compand (normalize_channel r1) in
    let g1 = srgb_inverse_compand (normalize_channel g1) in
    let b1 = srgb_inverse_compand (normalize_channel b1) in
    let r2 = srgb_inverse_compand (normalize_channel r2) in
    let g2 = srgb_inverse_compand (normalize_channel g2) in
    let b2 = srgb_inverse_compand (normalize_channel b2) in

    let gamma = 0.43 in
    let brightness1 = (r1 +. g1 +. b1) ** gamma in
    let brightness2 = (r2 +. g2 +. b2) ** gamma in
    fun ratio ->
      let t = clamp_01 ratio in
      let r = lerp r1 r2 t in
      let g = lerp g1 g2 t in
      let b = lerp b1 b2 t in
      let brightness = lerp brightness1 brightness2 t in
      let intensity = brightness ** (1.0 /. gamma) in
      let sum = r +. g +. b in
      let r, g, b =
        if sum = 0.0 then (r, g, b)
        else
          let factor = intensity /. sum in
          (r *. factor, g *. factor, b *. factor)
      in
      let r = srgb_compand (clamp_01 r) in
      let g = srgb_compand (clamp_01 g) in
      let b = srgb_compand (clamp_01 b) in
      (to_int_channel r, to_int_channel g, to_int_channel b)
end
