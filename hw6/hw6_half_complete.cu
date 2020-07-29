/*
 * Find BLANK and replace your own code.
 * And submit report why do you replace the blank that way.
 */

#include<stdlib.h>
#include<iostream>
#include<fstream>
#include<vector>
#include<string>

#define TILE_WIDTH 6 /* set TILE_WIDTH 16 for the evaluation! */
#define MAXPOOL_INPUT_FILENAME "input.txt"
#define A_FILENAME "a.txt"
#define B_FILENAME "b.txt"
#define C_FILENAME "c.txt"

using namespace std;

__global__ void maxpool(float *input, float *output, const int input_size, const int filter_size) {
    // input : input_matrix address
    // output : output buffer address
    // input_size : width, height of input matrix
    // filter_size : filter_size of maxpolling
    // all input, output matrices are vectorized

    int col = blockDim.x * blockIdx.x + threadIdx.x;
    int row = blockDim.y * blockIdx.y + threadIdx.y;

    // out of bound

    // CHANGE

	float tmp = 0.0;
	float Max = -999999.9;
	

	if(col < (input_size/filter_size) && row < (input_size/filter_size)){
	
		for(int i = 0; i < filter_size; i++){
			for(int j = 0; j < filter_size; j++){
				tmp = input[(input_size*filter_size*row)+(filter_size*col)+(input_size*j)+i];	
				if(Max<tmp)
					Max = tmp;
			}
		}	 
		output[((input_size/filter_size)*row)+col] = Max;
	}
	//printf("thread_made\n");
}

__global__ void gemm(float *a, float *b, float *c, const float alpha, const float beta, float *output, const int input_size){
    // a, b, c : input matrix address
    // alpha, beta : input constant
    // output : output buffer address
    // input_size : width, height of input matrix
    // all input, output matrices are vectorized

    int tx = threadIdx.x, ty = threadIdx.y;
    int bx = blockIdx.x,  by = blockIdx.y;

    int row = by*blockDim.y + ty;
    int col = bx*blockDim.x + tx;
    
    if(row>=input_size ||col>=input_size) { return; }
    
    // allocate 2D tiles in __shared__ memory
    __shared__ float s_a[TILE_WIDTH][TILE_WIDTH];
    __shared__ float s_b[TILE_WIDTH][TILE_WIDTH];

    float result = 0;

    // make sure you handle the case when the matrix sizes are not
    // multiple of TILE_WIDTH!
    // loop over the tiles of the input in phases
	
	//1. write one shared memory
	//s_a[ty][tx] = alpha*a[row*input_size + col];
	//s_b[ty][tx] = b[row*input_size + col];
	//__syncthreads();


    for(int p = 0; p < input_size/TILE_WIDTH+1 ;p++){
        // CHANGE

        // You need to use __syncthreads() a few times
        // to synchronize the threads in a thread block.
	
	//2.TILE이 딱 나눠떨어질때
	if(p < input_size/TILE_WIDTH ){
		
		//for(int i = 0 ; i < TILE_WIDTH ; i++)
		//	for(int j= 0; j< TILE_WIDTH; j++)
		s_a[ty][tx] = alpha* a[row*input_size + p*TILE_WIDTH+tx]; 
		//s_b[ty][tx] = b[col*input_size + p*TILE_WIDTH+ty];
		s_b[ty][tx] =b[(ty+p*TILE_WIDTH)*input_size + col];
		__syncthreads();
		for(int i = 0; i<TILE_WIDTH; i++)
			result += s_a[ty][i]*s_b[i][tx];

		//__syncthreads();
	}
	//3.TILE이 딱 안나눠떨어질때
	else if(p == input_size/TILE_WIDTH && input_size%TILE_WIDTH > 0){
		//for(int i = 0 ; i < input_size%TILE_WIDTH ;i++ ){
		//	for(int j = 0; j<input_size%TILE_WIDTH; j++)
		s_a[ty][tx] = alpha* a[row*input_size + p*(TILE_WIDTH)+tx]; 
		//s_b[ty][tx] = b[col*input_size + p*TILE_WIDTH+ty];
		s_b[ty][tx] =b[(ty+p*(TILE_WIDTH))*input_size + col];
		__syncthreads();

		if(blockIdx.y >= input_size/TILE_WIDTH){	
		for(int i = 0; i < TILE_WIDTH; i++){
		//	if(by < input_size/TILE_WIDTH || bx < TILE_WIDTH)
		//		result += s_a[ty][i]*s_b[i][tx];
		//	else if(i < input_size%TILE_WIDTH)
		//		result += s_a[ty][i]*s_b[i][tx];
			result += s_a[ty][i]*s_b[i][tx];
		}
		}
/*
		else if(blockIdx.x == input) {
		for(int i = 0; i < input_size%TILE_WIDTH; i++){
		//	if(by < input_size/TILE_WIDTH || bx < TILE_WIDTH)
		//		result += s_a[ty][i]*s_b[i][tx];
		//	else if(i < input_size%TILE_WIDTH)
		//		result += s_a[ty][i]*s_b[i][tx];
			result += s_a[ty][i]*s_b[i][tx];
		}
		}
*/
		else{
		for(int i = 0; i < input_size%TILE_WIDTH; i++){
		//	if(by < input_size/TILE_WIDTH || bx < TILE_WIDTH)
		//		result += s_a[ty][i]*s_b[i][tx];
		//	else if(i < input_size%TILE_WIDTH)
		//		result += s_a[ty][i]*s_b[i][tx];
			result += s_a[ty][i]*s_b[i][tx];
		}
		}
		
		
	}
	//__syncthreads();
    }
	__syncthreads();
    // write out the result to output[row*input_size + col] 
    // CHANGE
	output[row*input_size + col] = result + (beta * c[row*input_size + col]);
	//__syncthreads(); 
}


int main(int argc, char **argv) {
    if(argc < 4) {
        cout << "usage : " << argv[0] << " input_size filter_size alpha beta\n" << "example : " << argv[0] << " 100 2 0.5 0.8\n";
        return 1;
    }
    const int input_size = stoi(argv[1]);
    const int filter_size = stoi(argv[2]); // used for maxpooling
    const float alpha = stof(argv[3]);
    const float beta = stof(argv[4]);
    const int maxpool_output_size = input_size/filter_size;

    // check input_siize is power of 2
    if(input_size == 0 && (input_size & (input_size-1))){
        cout << "input_size must be power of 2\n";
        return 1;
    }

    if(filter_size == 0){
        cout << "filter_size cannot be 0\n";
        return 1;
    }

    float maxpool_input[input_size*input_size];
    float a[input_size*input_size];
    float b[input_size*input_size];
    float c[input_size*input_size];
    
    // read input matrices 
    ifstream input_in(MAXPOOL_INPUT_FILENAME);
    ifstream a_in(A_FILENAME);
    ifstream b_in(B_FILENAME);
    ifstream c_in(C_FILENAME);

    for (int i = 0; i < input_size*input_size; ++i) {
        input_in >> maxpool_input[i];
        a_in >> a[i];
        b_in >> b[i];
        c_in >> c[i];
    }
    
    // prints inputs for debugging.
    cout<<"filter size : "<<filter_size;
    cout<<"\n========== MAXPOOL_INPUT ==========\n";
    for (int i = 0; i < input_size * input_size; ++i) {
        if(i%input_size==0) cout<<"\n";
        cout<<maxpool_input[i]<<" ";
    }
    cout<<"\nalpha : "<<alpha<<'\n';
    cout<<"========== A ==========\n";
    for (int i = 0; i < input_size * input_size; ++i) {
        if(i%input_size==0) cout<<"\n";
        cout<<a[i]<<" ";
    }
    cout<<"\n========== B ==========\n";
    for (int i = 0; i < input_size * input_size; ++i) {
        if(i%input_size==0) cout<<"\n";
        cout<<b[i]<<" ";
    }
    cout<<"\nbeta : "<<beta<<'\n';
    cout<<"========== C ==========\n";
    for (int i = 0; i < input_size * input_size; ++i) {
        if(i%input_size==0) cout<<"\n";
        cout<<c[i]<<" ";
    }
    cout<<'\n';
       
    // set thread, block dimensions
    const dim3 block_size(TILE_WIDTH, TILE_WIDTH);
    const dim3 num_of_maxpool_blocks(maxpool_output_size/block_size.x+1, maxpool_output_size/block_size.y+1);
    const dim3 num_of_blocks(input_size/block_size.x+1, input_size/block_size.y+1);

    // memory allocation for the device
    float *dev_mem_a, *dev_mem_b, *dev_mem_c, *dev_mem_input, *gemm_output, *maxpool_output;
    cudaMalloc(&dev_mem_a, sizeof(float) * input_size * input_size);
    cudaMalloc(&dev_mem_b, sizeof(float) * input_size * input_size);
    cudaMalloc(&dev_mem_c, sizeof(float) * input_size * input_size);
    cudaMalloc(&gemm_output, sizeof(float) * input_size * input_size);
    cudaMalloc(&dev_mem_input, sizeof(float) * input_size * input_size);
    cudaMalloc(&maxpool_output, sizeof(float) * maxpool_output_size * maxpool_output_size);
    
    // copy variable to device memory
    cudaMemcpy(dev_mem_a, a, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);
    cudaMemcpy(dev_mem_b, b, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);
    cudaMemcpy(dev_mem_c, c, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);
    cudaMemcpy(dev_mem_input, maxpool_input, sizeof(float) * input_size * input_size, cudaMemcpyHostToDevice);

    // launch CUDA kernels

    // First launch gemm kernel
    gemm<<<num_of_blocks, block_size>>>(dev_mem_a, dev_mem_b, dev_mem_c, alpha, beta, gemm_output, input_size);
    cudaDeviceSynchronize();
    cudaError_t error = cudaGetLastError();
    if(error!=cudaSuccess) {
        fprintf(stderr, "ERROR %s\n", cudaGetErrorString(error));
        return 1;
    }
 
    // Then run maxpooling
    maxpool<<<num_of_maxpool_blocks, block_size>>>(dev_mem_input, maxpool_output, input_size, filter_size);
    cudaDeviceSynchronize();
    error = cudaGetLastError();
    if(error!=cudaSuccess) {
        fprintf(stderr, "ERROR %s\n", cudaGetErrorString(error));
        return 1;
    }
 
    // allocate output buf in main memory
    float *gemm_output_buf = (float*) malloc (sizeof(float)*input_size*input_size);
    float *maxpool_output_buf = (float*) malloc (sizeof(float)*maxpool_output_size*maxpool_output_size);
    
    // copy results from device to host
    cudaMemcpy(gemm_output_buf, gemm_output, sizeof(float)*input_size*input_size, cudaMemcpyDeviceToHost);
    cudaMemcpy(maxpool_output_buf, maxpool_output, sizeof(float)*maxpool_output_size*maxpool_output_size, cudaMemcpyDeviceToHost);
    
    // prints the results
    cout<<"\n========== GEMM OUTPUT ==========\n";
    for (int i = 0; i < input_size * input_size; ++i) {
        if(i%input_size==0) cout<<"\n";
        cout<<gemm_output_buf[i]<<" ";
    }
    cout<<"\n========== MAXPOOL OUTPUT ==========\n";
    for (int i = 0; i < maxpool_output_size * maxpool_output_size; ++i) {
        if(i%maxpool_output_size==0) cout<<"\n";
        cout<<maxpool_output_buf[i]<<" ";
    }
    cout<<'\n';

    cudaFree(dev_mem_a);
    cudaFree(dev_mem_b);
    cudaFree(dev_mem_c);
    cudaFree(gemm_output);
    cudaFree(dev_mem_input);
    cudaFree(maxpool_output);
    free(gemm_output_buf);
    free(maxpool_output_buf);
    return 0;
}