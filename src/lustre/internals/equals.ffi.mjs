export const isReferenceEqual = (a, b) => a === b;

// This isEqual implementation has to support JSON literals, i.e. values that
// can be produced by using the gleam/json module.
// It is a highly specialised version of https://github.com/planttheidea/fast-equals.
export const isEqual = (a, b) => {
  if (a === b) {
    return true;
  }

  if (a == null || b == null) {
    return false;
  }

  const type = typeof a;
  if (type !== typeof b) {
    return false;
  }

  // we do not support NaN, and both values being equal has already
  // been handled above.
  if (type !== 'object') {
    return false;
  }

  const ctor = a.constructor;
  if (ctor !== b.constructor) {
    return false;
  }

  if (Array.isArray(a)) {
    return areArraysEqual(a, b);
  }

  return areObjectsEqual(a, b);
};

const areArraysEqual = (a, b) => {
  let index = a.length;

  if (index !== b.length) {
    return false;
  }

  while (index--) {
    if (!isEqual(a[index], b[index])) {
      return false;
    }
  }

  return true;
}

const areObjectsEqual = (a, b) => {
  const properties = Object.keys(a);
  let index = properties.length;

  if (Object.keys(b).length !== index) {
    return false;
  }

  while (index--) {
    const property = properties[index];
    if (!Object.hasOwn(b, property)) {
      return false;
    }
    if (!isEqual(a[property], b[property])) {
      return false;
    }
  }

  return true;
}

