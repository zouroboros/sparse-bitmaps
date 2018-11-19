#include "SparseMatrix.h"

#include <stdint.h>
#include <stdio.h>

#include "Callback.h"

uint16_t min(uint16_t x, uint16_t y) {
  if(x >= y) {
    return y;
  }
  return x;
}

uint16_t readValue(uint8_t* array, uint16_t start, uint16_t end, uint16_t default_value) {
  uint16_t shift = 0;
  for (uint16_t i = start; i < end; i++) {
    if(array[i] < 255) {
      return shift + array[i];
    } else {
      shift += 255;
    }
  }
  return default_value;
}

uint16_t shift(uint8_t* array, uint16_t start, uint16_t end, uint16_t default_value) {
  for (uint16_t i = start; i < end; i++) {
    if(array[i] < 255) {
      return i - start;
    }
  }
  return default_value;
}

void entries(SparseBoolMatrix matrix, Callback* callback) {
    uint16_t rp = 0;
    for(uint16_t i = 0; i < matrix.rows; i++) {
        uint16_t start_row = readValue(matrix.row_pointer, rp, matrix.row_pointer_length, 0);
        uint16_t rp_shift = shift(matrix.row_pointer, rp, matrix.row_pointer_length, 0);
        rp++;
        rp += rp_shift;
        uint16_t end_row = readValue(matrix.row_pointer, rp, matrix.row_pointer_length, matrix.column_indices_length);

        for (int16_t ci = start_row; ci < end_row; ci++) {
          uint16_t j = readValue(matrix.column_indices, ci, end_row, 0);
          uint16_t ci_shift = shift(matrix.column_indices, ci, end_row, 0);
          ci += ci_shift;
          callback->entry(i + 1, j + 1);
        }
    }
}

void fullMatrix(SparseBoolMatrix matrix, uint8_t* fullMatrix) {
    MatrixCallback* callback = new MatrixCallback(matrix.rows, matrix.columns, fullMatrix);
    entries(matrix, callback);
}
