import { CpSat, setWorkerBridgeEnabled, type SatParameters } from 'or-tools-wasm/cp-sat';

type MagicSquareExpr = {
  vars: number[];
  coeffs: number[];
  offset: number;
};

type MagicSquareModel = {
  name: string;
  variables: Array<{ name: string; domain: number[] }>;
  constraints: Array<
    | { name: string; all_diff: { exprs: MagicSquareExpr[] } }
    | { name: string; linear: { vars: number[]; coeffs: number[]; domain: number[] } }
  >;
};

function buildMagicSquareModel(size: number): MagicSquareModel {
  const numCells = size * size;
  const variables: MagicSquareModel['variables'] = [];
  const domain: [number, number] = [1, numCells];
  for (let row = 0; row < size; ++row) {
    for (let col = 0; col < size; ++col) {
      variables.push({ name: `cell_${row}_${col}`, domain: [...domain] });
    }
  }

  const constraints: MagicSquareModel['constraints'] = [];
  constraints.push({
    name: 'all_diff',
    all_diff: {
      exprs: Array.from({ length: numCells }, (_, index) => ({
        vars: [index],
        coeffs: [1],
        offset: 0,
      })),
    },
  });

  const target = (size * (size * size + 1)) / 2;
  const addSum = (name: string, vars: number[]) => {
    constraints.push({
      name,
      linear: { vars, coeffs: Array(vars.length).fill(1), domain: [target, target] },
    });
  };

  for (let row = 0; row < size; ++row) {
    addSum(`row_${row}`, Array.from({ length: size }, (_, col) => row * size + col));
  }
  for (let col = 0; col < size; ++col) {
    addSum(`col_${col}`, Array.from({ length: size }, (_, row) => row * size + col));
  }
  addSum('diag_main', Array.from({ length: size }, (_, index) => index * size + index));
  addSum('diag_anti', Array.from({ length: size }, (_, index) => index * size + (size - 1 - index)));

  return { name: `magic_square_${size}`, variables, constraints };
}

setWorkerBridgeEnabled(true);
const size = 5;
const model = await CpSat.createModel(buildMagicSquareModel(size));
const validation = await CpSat.validate(model);
if (!validation.ok) throw new Error(validation.message);

const workers = 4;
const params: SatParameters = { numWorkers: workers, logSearchProgress: true };
const result = await CpSat.solve(model, params);
console.log(result.response?.solution);
