#define _GNU_SOURCE

#include <stdio.h>
#include <linux/sched.h> // for use scheduling function
#include <sys/syscall.h>
#include <unistd.h>
#include <time.h> // for use function about time
#include <stdint.h>
#include <stdlib.h> // for use atoi, string - int
#include <signal.h> // for use SIGINT_handler (ctrl + c)
#include <sys/wait.h> // for use wait() function

#define ROW (100)
#define COL ROW


struct sched_attr {
	uint32_t size; // size of structure
	uint32_t sched_policy; // policy
	uint64_t sched_flags; //flags
	int32_t sched_nice;
	
	uint32_t sched_priority; // priority
	
	// about DEAD_LINE
	uint64_t sched_runtime;
	uint64_t sched_deadline;
	uint64_t sched_period;
};


int cpuid;
double running_time; // calculate of execution time
long count; // number of operations


static int sched_setattr(pid_t pid, const struct sched_attr *attr, unsigned int flags)
{
	return syscall(SYS_sched_setattr, pid, attr, flags); // scheduling -> Round Robin
}

int calc(int time, int cpu);
static void sig_handler(int signo);

int main(int argc, char* argv[]) // command list arguments, number of process, running time of process
{
	int pnum = atoi(argv[1]); // pnum = number of processS
	int total_time = atoi(argv[2]); // total_time = running time of process
	int ret;
	pid_t pid; // child process

	struct sched_attr attr = { // for scheduling
		.size = sizeof(struct sched_attr),
		.sched_priority = 10, // set priority to 10 for assignment (sched.h)
		//.sched_flags = ,
		//.sched_runtime = ,
		.sched_policy = SCHED_RR // Round-Robin scheduling
	};

	
	if(ret = sched_setattr(getpid(), &attr, 0) == -1){ // call 'sched_setattr' function
		perror("Error! Please get sudo mode\n");
		return -1;
	}

	wait(NULL); // wait for finish of child process
	


	for(; cpuid<pnum; cpuid++){
		if(pid = fork() > 0){ // this branch is for parent process
			// use system call 'fork()' to make child process
			printf("Creating Process: #%d\n", cpuid);
		}
		else if(pid == 0){
			calc(total_time, cpuid);
			exit(0);
		}
		else {
			perror("Error!"); // if pid is less than 0, make error
			return -1;
		}
	}		
	
	
}

int calc(int time, int cpu){ // do operation and get a time for that
	int matrixA[ROW][COL];
	int matrixB[ROW][COL];
	int matrixC[ROW][COL];
	int i, j, k;
	//double running_time = 0; // added
	//long count = 0; // added

	struct timespec begin, end; // variable for calculate the time start~end


	if(signal(SIGINT, (void*)sig_handler) == SIG_ERR){ // 
		printf("SIGNAL ERROR!\n");
		return -1;
	}

	clock_gettime(CLOCK_MONOTONIC, &begin); // start of calculate
	while(1){
		for(i=0; i<ROW; i++){
			for(j=0; j<COL; j++){
				for(k=0; k<COL; k++){
					matrixC[i][j] += matrixA[i][k] * matrixB[k][j];
				}
			}
		}

		count++;
		clock_gettime(CLOCK_MONOTONIC, &end); // end of calculate
		running_time = (end.tv_sec - begin.tv_sec)*1000000000 + (end.tv_nsec - begin.tv_nsec); //nsec


		if((int)(running_time/1000000)%100 == 0 || (int)(running_time/1000000)%100 <= 1.5){ //msec
			printf("PROCESS #%02d count = %ld 100\n", cpu, count);
		}	

		if((running_time / 1000000000) >= time){ //sec	
			printf("DONE!! PROCESS #%02d : %06ld %.0lf\n", cpu, count, (running_time / 1000000));
			break;
		}

		
	}

	return 0;
}

static void sig_handler(int signo) // if input is Ctrl + C -> exit process after print
{
	printf("DONE!! PROCESS #%02d : %06ld %.0lf\n", cpuid, count, (running_time / 1000000));

	exit(0);
}
