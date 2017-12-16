const div = 2147483647;
const aMul = 16807;
const bMul = 48271;
const aStart = 679;
const bStart = 771;
const aValues = [];
const bValues = [];
const aIterations = 40000000;
const bIterations = 5000000;

let count = 0;
let a = aStart;
let b = bStart;
for (let i = 0; i < aIterations || aValues.length < bIterations || bValues.length < bIterations; i++) {
  a = (a * aMul) % div;
  b = (b * bMul) % div;
  if (i < aIterations && (a & 0xffff) === (b & 0xffff)) {
    count++;
  }
  if (aValues.length < bIterations && a % 4 === 0) {
    aValues.push(a);
  }
  if (bValues.length < bIterations && b % 8 === 0) {
    bValues.push(b);
  }
}

console.log(`Part 1: ${count}`);

count = 0;
for (let i = 0; i < bIterations; i++) {
  if ((aValues[i] & 0xffff) === (bValues[i] & 0xffff)) {
    count++;
  }
}

console.log(`Part 2: ${count}`);
