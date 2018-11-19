#include "Callback.h"

#include <stdint.h>

MatrixCallback::MatrixCallback(uint16_t rows, uint16_t columns, uint8_t* matrix) {
  this->rows = rows;
  this->columns = columns;
  this->matrix = matrix;
}

void MatrixCallback::entry(uint16_t i, uint16_t j) {
  this->matrix[this->columns * (i - 1) + j - 1] = 1;
}

FunctionCallback::FunctionCallback(void (*fnptr)(uint16_t, uint16_t)) {
  this->fnptr = fnptr;
}

void FunctionCallback::entry(uint16_t i, uint16_t j) {
  this->fnptr(i, j);
}
