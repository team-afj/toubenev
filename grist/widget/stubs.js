//Provides: array_of_obj_from_obj_of_arrays
function array_of_obj_from_obj_of_arrays(table) {
  const keys = Object.keys(table);
  console.log(table)
  return table[keys[0]].map((_, i) => Object.fromEntries(keys.map(k => [k, table[k][i]])));
}
