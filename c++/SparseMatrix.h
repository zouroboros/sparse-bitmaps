#ifndef _SPARSEMATRIX_H_
#define _SPARSEMATRIX_H_

#include <stdint.h>

#include "Callback.h"

// structure to save a sparse matrix without its values
typedef struct {
  uint16_t rows;
  uint16_t columns;
  uint16_t row_pointer_length;
  uint16_t column_indices_length;
  uint8_t *column_indices;
  uint8_t *row_pointer;
} SparseBoolMatrix;

void fullMatrix(SparseBoolMatrix, uint8_t*);
void entries(SparseBoolMatrix, Callback*);

#endif
