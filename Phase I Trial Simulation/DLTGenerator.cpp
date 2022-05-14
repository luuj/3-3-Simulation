#include "DLTGenerator.h"

/*Generates a random number between 0 and 1 with uniform distribution*/
double uniformRandGen(double startNum, double endNum)
{
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(startNum, endNum);
    
    return dis(gen);
}

//Calculates gamma variable given a P(N) and beta value
double calculateGamma(double maxDLT, double beta, int maxDose)
{
    return ((log(maxDLT + 1)) / (beta*(maxDose-1)) - 1) / (maxDose-1);
}

//Calculates a single probability using probability equation and given parameters
double calculateProbabilityDLT(double dose, double beta, double gamma)
{
    return pow(M_E, beta*dose*(1+(gamma*dose))) - 1;
}

//Creates a list of MAX_DOSE_LEVEL probabilities using formula
double* generateDLTProbability(double& beta, double& gamma, int P_LEVEL, int B_LEVEL)
{
    static double list[DOSE_LEVEL+1];
    double low=0, high=0;
    
    switch (P_LEVEL)
    {
        case 0: //Low P(N)
            low=0.1; high=0.3;
            break;
        case 1: //Medium P(N)
            low=0.3; high=0.5;
            break;
        case 2: //High P(N)
            low=0.5; high=0.8;
            break;
    }
    
    double maxDLT = uniformRandGen(low,high); // Maximum DLT probability P(9)
    
    //Select low, medium, or high beta category
    switch (B_LEVEL)
    {
        case 0: //Low Beta
            beta = uniformRandGen(0,(maxDLT/(DOSE_LEVEL-1))*0.33)*1.5;
            break;
        case 1: //Medium Beta
            beta = uniformRandGen((maxDLT/(DOSE_LEVEL-1))*0.33,(maxDLT/(DOSE_LEVEL-1))*0.66)*1.5;
            //beta = uniformRandGen((maxDLT/(DOSE_LEVEL-1))*0.66,maxDLT/(DOSE_LEVEL-1));
            break;
        case 2: //High Beta
            beta = uniformRandGen((maxDLT/(DOSE_LEVEL-1))*0.66,maxDLT/(DOSE_LEVEL-1))*1.5;
            break;
    }
    gamma = calculateGamma(maxDLT, beta, DOSE_LEVEL);
    
    list[0] = 0; //Base level of 0.00 probability
    for (int i=1; i<DOSE_LEVEL; i++)
    {
        list[i] = calculateProbabilityDLT(i, beta, gamma);
    }
    
    return list;
}


