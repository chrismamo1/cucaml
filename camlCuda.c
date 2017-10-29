#include <cuda.h>
#include <cuda_runtime.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>

#define BUFFER_SIZE 8192

CUcontext *ctx = NULL;

void initCuda() {
  cuInit(0);
  CUdevice cuDevice;
  cuDeviceGet(&cuDevice, 0);

  /* Create context */
  CUcontext cuContext;
  cuCtxCreate(&cuContext, 0, cuDevice);

  ctx = malloc(sizeof(CUcontext));
  *ctx = cuContext;
}

CAMLprim value mapFloatArrayInPlace(value arr, value length, value kernelCode) {
  if (ctx == NULL) {
    initCuda();
  }
  long int arrLen = Long_val(length);
  CUdeviceptr arr_D;
  size_t kernelCodeLength = caml_string_length(kernelCode);
  char *kernelSource = String_val(kernelCode);
  double *nativeArr = (double*)arr;

  CUmodule cuModule;
  CUjit_option options[3];
  void* values[3];
  char error_log[BUFFER_SIZE];
  int err;
  options[0] = CU_JIT_ERROR_LOG_BUFFER;
  values[0]  = (void*)error_log;
  options[1] = CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES;
  values[1]  = (void*)BUFFER_SIZE;
  options[2] = CU_JIT_TARGET_FROM_CUCONTEXT;
  values[2]  = 0;
  err = cuModuleLoadDataEx(&cuModule, kernelSource, 3, options, values);
  printf("Loading the module got this error: %d\n", err);
  if (err != CUDA_SUCCESS)
      printf("Link error:\n%s\n", error_log);

  cuMemAlloc(&arr_D, sizeof(double) * arrLen);
  cuMemcpyHtoD(arr_D, nativeArr, sizeof(double) * arrLen);

  CUfunction kernel;
  cuModuleGetFunction(&kernel, cuModule, "myTestKernel");
  printf("Successfully loaded the test kernel");
  void* args[] = { &arr_D };
  cuLaunchKernel(
      kernel,
      1, 1, 1,
      arrLen, 1, 1,
      0, 0, args, 0);

  /*kernel<<<1, 16>>>(xd);*/
  /*cudaDeviceSynchronize();*/
  if (cuCtxSynchronize() != CUDA_SUCCESS) {
    return caml_copy_double(-1.0);
  } else {
    cuMemcpyDtoH(nativeArr, arr_D, sizeof(double) * arrLen);
    nativeArr[0] = -1.0;
    printf("nativeArr[1] = %lf\n", nativeArr[1]);
    return caml_copy_double(5.0);
  }
}

CAMLprim value printFloatArray(value arr, value length) {
  long int l = Long_val(length);
  int i;
  double *nativeArr = (double*)arr;
  double sum = 0.0;
  for (i = 0; i < l; ++i) {
    printf("%f\n", nativeArr[i]);
    sum += nativeArr[i];
  }
  /*CAMLreturn(Val_unit);*/
  return caml_copy_double(sum);
}