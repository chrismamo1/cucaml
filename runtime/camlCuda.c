#include <cuda.h>
#include <cuda_runtime.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <string.h>

#define BUFFER_SIZE 8192

CUcontext *ctx = NULL;
char *cachedCode = NULL;
CUmodule cuModule;

/*int sHash(char *s) {
  int rv = 0;
  char *t;
  for (t = s; *t != '\0'; t++) {
    rv = rv + *t;
    rv = rv % 512;
  }
}*/

int maxGridX, maxGridY, maxGridZ, maxBlockX, maxBlockY, maxBlockZ;
int maxThreadsPerBlock;

void initCuda() {
  cuInit(0);
  CUdevice cuDevice;
  cuDeviceGet(&cuDevice, 0);

  cuDeviceGetAttribute(&maxGridX, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X, cuDevice);
  cuDeviceGetAttribute(&maxBlockX, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X, cuDevice);
  cuDeviceGetAttribute(&maxThreadsPerBlock, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, cuDevice);

  /* Create context */
  CUcontext cuContext;
  cuCtxCreate(&cuContext, 0, cuDevice);

  ctx = malloc(sizeof(CUcontext));
  *ctx = cuContext;
}

CAMLprim value mapFloatArrayInPlace(value async, value arr, value length, value kernelCode) {
  if (ctx == NULL) {
    initCuda();
  }
  size_t consumed = 0;
  long int arrLen = Long_val(length);
  /*if (arrLen > maxGridX * maxBlockX) {
    printf("ERROR: mapFloatArrayInPlace: cannot handle arrays that big yet.\n");
    printf("Max block x: %d\n", maxBlockX);
    printf("Max grid x: %d\n", maxGridX);
    printf("Max threads per block: %d\n", maxThreadsPerBlock);
    return caml_copy_double(-1.0);
  }*/
  CUdeviceptr arr_D;
  size_t kernelCodeLength = caml_string_length(kernelCode);
  size_t i;
  char *kernelSource = String_val(kernelCode);
  double *nativeArr = (double*)&(Double_field(arr, 0));

  if (cachedCode == NULL || strcmp(cachedCode, kernelSource) != 0) {
    free(cachedCode);
    cachedCode = strdup(kernelSource);

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
    if (err != CUDA_SUCCESS)
        printf("Link error:\n%s\n", error_log);
  }

  if (arrLen < maxBlockX) {
    cuMemAlloc(&arr_D, sizeof(double) * arrLen);
    cuMemcpyHtoD(arr_D, nativeArr, sizeof(double) * arrLen);

    CUfunction kernel;
    cuModuleGetFunction(&kernel, cuModule, "myTestKernel");
    void* args[] = { &arr_D };
    cuLaunchKernel(
      kernel,
      1, 1, 1,
      arrLen, 1, 1,
      0, 0, args, 0);
  } else {
    CUfunction kernel;
    cuModuleGetFunction(&kernel, cuModule, "myTestKernel");
    consumed = 0;
    while (consumed < arrLen) {
      CUdeviceptr arr_D;
      int gridX;
      int blockX;
      if (arrLen - consumed < maxBlockX) {
        blockX = arrLen - consumed;
        gridX = 1;
      } else {
        blockX = maxBlockX;
        gridX = (arrLen - consumed) / blockX;
        if (gridX > maxGridX) {
          gridX = maxGridX;
        }
      }
      cuMemAlloc(&arr_D, sizeof(double) * blockX * gridX);
      cuMemcpyHtoD(arr_D, nativeArr + consumed, sizeof(double) * blockX * gridX);
      void* args[] = { &arr_D };
      cuLaunchKernel(
            kernel,
            gridX, 1, 1,
            blockX, 1, 1,
            0, 0, args, 0);
      if (cuCtxSynchronize() != CUDA_SUCCESS) {
        return caml_copy_double(-1.0);
      }
      cuMemcpyDtoH(nativeArr + consumed, arr_D, sizeof(double) * blockX * gridX);
      cuMemFree(arr_D);
      consumed += blockX * gridX;
    }
    return caml_copy_double(5.0);
  }

  /*kernel<<<1, 16>>>(xd);*/
  /*cudaDeviceSynchronize();*/
  if (cuCtxSynchronize() != CUDA_SUCCESS) {
    return caml_copy_double(-1.0);
  } else {
    cuMemcpyDtoH(nativeArr, arr_D, sizeof(double) * arrLen);
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