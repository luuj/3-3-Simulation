#ifndef DLTGenerator_h
#define DLTGenerator_h
#include <math.h>
#include <random>
#include <iostream>
#include <fstream>

//Parameters
#define DOSE_LEVEL 10 //N+1 dose levels as it includes DL0
#define STARTING_DOSE 2 //Starting dose level

#define SIMULATION_COUNT 1 // Number of iterations in a single simulation
#define SIMULATION_NUMBER 150 // Number of simulations

#define PRINT 0 //0=No print, 1=print

double uniformRandGen(double startNum, double endNum);
double calculateGamma(double maxDLT, double beta, int maxDose);
double calculateProbabilityDLT(double dose, double beta, double gamma);
double* generateDLTProbability(double& beta, double& gamma, int P_LEVEL, int B_LEVEL);

#endif
