#ifndef _CALLBACK_H_
#define _CALLBACK_H_

#include <stdint.h>

class Callback {
public:
  virtual void entry(uint16_t i, uint16_t j) = 0;
};

class FunctionCallback : public Callback {
private:
  void (*fnptr)(uint16_t, uint16_t);
public:
  FunctionCallback(void (*fptr)(uint16_t, uint16_t));
  void entry(uint16_t, uint16_t);
};

class MatrixCallback : public Callback {
private:
  uint16_t rows;
  uint16_t columns;
  uint8_t* matrix;
public:
  MatrixCallback(uint16_t, uint16_t, uint8_t*);
  void entry(uint16_t i, uint16_t j);
};

#endif
