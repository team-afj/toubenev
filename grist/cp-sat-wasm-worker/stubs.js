
//Provides: cpSatModulePromise
const cpSatModulePromise = import('or-tools-wasm/cp-sat').then(({ CpSat }) => {
  // This worker should execute the solver directly instead of spawning the
  // package's own browser worker bridge.
  CpSat.setWorkerBridgeEnabled(false);
  return CpSat;
});

//Provides: normalizeUint8Array
function normalizeUint8Array(value, label) {
  if (value instanceof Uint8Array) return value;
  if (value instanceof ArrayBuffer) return new Uint8Array(value);
  if (ArrayBuffer.isView(value)) {
    return new Uint8Array(value.buffer, value.byteOffset, value.byteLength);
  }
  throw new TypeError(`${label} must be a Uint8Array or ArrayBuffer view`);
}

//Provides: normalizeOptionalUint8Array
function normalizeOptionalUint8Array(value, label) {
  return value == null ? null : normalizeUint8Array(value, label);
}

//Provides: caml_pbrt_varint_size_byte
//Requires: caml_int64_lo32, caml_int64_hi32
function caml_pbrt_varint_size_byte(v_i) {
  // js_of_ocaml int64 is a pair of unsigned 32-bit halves: lo = bits 0-31, hi = bits 32-63.
  // Thresholds mirror the C pbrt_varint_size() function (uint64 semantics).
  var lo = caml_int64_lo32(v_i) >>> 0;
  var hi = caml_int64_hi32(v_i) >>> 0;
  if (hi === 0) {
    if (lo <= 0x7f) return 1;
    if (lo <= 0x3fff) return 2;
    if (lo <= 0x1fffff) return 3;
    if (lo <= 0xfffffff) return 4;
    return 5; // lo <= 0xffffffff, hi === 0  →  value <= 4294967295
  }
  // hi > 0  →  value >= 2^32
  if (hi <= 0x7) return 5;  // value <= 34359738367   (2^35 - 1)
  if (hi <= 0x3ff) return 6;  // value <= 4398046511103  (2^42 - 1)
  if (hi <= 0x1ffff) return 7;  // value <= 562949953421311 (2^49 - 1)
  if (hi <= 0xffffff) return 8;  // value <= 72057594037927935 (2^56 - 1)
  if (hi <= 0x7fffffff) return 9;  // value <= 9223372036854775807 (INT64_MAX)
  return 10;
}

//Provides: caml_pbrt_varint_byte
//Requires: caml_int64_lo32, caml_int64_hi32, caml_bytes_unsafe_set
function caml_pbrt_varint_byte(v_b, v_start, v_i) {
  var lo = caml_int64_lo32(v_i) >>> 0;
  var hi = caml_int64_hi32(v_i) >>> 0;
  var pos = v_start;
  while (lo >= 0x80 || hi !== 0) {
    caml_bytes_unsafe_set(v_b, pos++, (lo & 0x7f) | 0x80);
    // Shift the 64-bit value right by 7: bring the low 7 bits of hi into the top of lo.
    lo = ((lo >>> 7) | ((hi & 0x7f) << 25)) >>> 0;
    hi = hi >>> 7;
  }
  caml_bytes_unsafe_set(v_b, pos, lo);
  return 0; // unit
}

//Provides: cp_sat_wasm_uint8_array_of_string
function cp_sat_wasm_uint8_array_of_string(value) {
  const bytes = new Uint8Array(value.length);
  for (let index = 0; index < value.length; index += 1) {
    bytes[index] = value.charCodeAt(index) & 0xff;
  }
  return bytes;
}

//Provides: cp_sat_wasm_string_of_uint8_array
function cp_sat_wasm_string_of_uint8_array(value) {
  const bytes = normalizeUint8Array(value, 'value');
  const chunkSize = 0x8000;
  let result = '';
  for (let index = 0; index < bytes.length; index += chunkSize) {
    const chunk = bytes.subarray(index, Math.min(index + chunkSize, bytes.length));
    result += String.fromCharCode(...chunk);
  }
  return result;
}

//Provides: cp_sat_wasm_solve_raw
async function cp_sat_wasm_solve_raw(model, params) {
  const CpSat = await cpSatModulePromise;
  return await CpSat.solveRaw(
    normalizeUint8Array(model, 'model'),
    normalizeOptionalUint8Array(params, 'params'),
  );
}

//Provides: cp_sat_wasm_solve
async function cp_sat_wasm_solve(model, params) {
  const CpSat = await cpSatModulePromise;
  return await CpSat.solve(
    normalizeUint8Array(model, 'model'),
    normalizeOptionalUint8Array(params, 'params'),
  );
}

//Provides: cp_sat_wasm_validate
async function cp_sat_wasm_validate(model) {
  const CpSat = await cpSatModulePromise;
  return await CpSat.validate(normalizeUint8Array(model, 'model'));
}
