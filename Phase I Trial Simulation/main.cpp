#include "main.h"

/*Initializes excel file columns*/
void initializeOutput(std::ofstream& dataFile, int currDir, int currB)
{
    std::string currPLevel, currBLevel;
    
    switch(currB) //Select directory
    {
        case 0: currBLevel = "Low Beta"; break;
        case 1: currBLevel = "Medium Beta"; break;
        case 2: currBLevel = "High Beta"; break;
    }
    
    switch(currDir) //Select directory
    {
        case 0: currPLevel = "Low P(D)"; break;
        case 1: currPLevel = "Medium P(D)"; break;
        case 2: currPLevel = "High P(D)"; break;
    }
    
    std::string fileName = currBLevel + "_" + currPLevel + ".csv";
    dataFile.open(fileName, std::ios::out);

    std::string row_labels = "Simulation #, Final MTD Probability, Dose levels tested, DLT count, Total patient count, Patient count (Dose < .25), Patient count (.25 <= Dose <= .35), Patient count (> .35), P(D), Beta, Gamma";
    for (int i=1; i<DOSE_LEVEL; i++)
        row_labels += ", P(" + std::to_string(i) + ")";
    row_labels += "\n";
    dataFile << row_labels;
}

void runSimulation(int p_cat, int b_cat)
{
    int MTD, totalPatientCount, totalDLT, doseLevelsTested, unacceptable, acceptable_med, acceptable_low;
    double beta, gamma;
    int patientCount[DOSE_LEVEL]; //Patient count for each dose level
    
    std::ofstream dataFile; initializeOutput(dataFile, p_cat, b_cat); //Excel file outstream
    
    //Run test z # of times
    for (int z=0; z<SIMULATION_NUMBER; z++)
    {
        double* DLT = generateDLTProbability(beta, gamma, p_cat, b_cat); //Array of DLT probabilities
        
        if (PRINT)
        {
            std::cout << "Beta:" << beta << " Gamma:" << gamma << " P(n):" << DLT[DOSE_LEVEL-1] << std::endl;
            for (int testI=0; testI<DOSE_LEVEL; testI++)
                std::cout << "P[" << testI << "]:" << DLT[testI] << " ";
            std::cout << std::endl;
        }
        
        //Run simulation i # of times
        for (int i=0; i<SIMULATION_COUNT; i++)
        {
            MTD = runSimulation(DLT, patientCount, doseLevelsTested, totalDLT);
            
            //Calculate meta-data for each simulation
            totalPatientCount = unacceptable = acceptable_med = acceptable_low = 0;
            for (int k=0; k<DOSE_LEVEL; k++)
            {
                totalPatientCount += patientCount[k];
                
                if (DLT[k] > 0.35) //Calculate unacceptable patient count
                    unacceptable += patientCount[k];
                
                if (DLT[k] < 0.25)
                    acceptable_low += patientCount[k];
                
                if (DLT[k] >= 0.25 && DLT[k] <= 0.35)
                    acceptable_med += patientCount[k];
            }
            
            //Output data to excel format
            std::string row_label = std::to_string(z) + "," + std::to_string(DLT[MTD]) + "," + std::to_string(doseLevelsTested) + "," + std::to_string(totalDLT) + "," + std::to_string(totalPatientCount) + "," + std::to_string(acceptable_low) + "," + std::to_string(acceptable_med) + "," + std::to_string(unacceptable);
            
            row_label += "," + std::to_string(DLT[DOSE_LEVEL-1]) + "," + std::to_string(beta) + "," + std::to_string(gamma);
            
            for (int j=1; j<DOSE_LEVEL; j++)
                row_label += "," + std::to_string(DLT[j]);
            
            row_label += "\n";
            dataFile << row_label;
            
            if (PRINT)
                std::cout << "Final MTD:" << MTD << " Total pt count:" << totalPatientCount << " Dose # tested:" << doseLevelsTested << " Total DLT count:" << totalDLT << "\n\n";
        }
    }
    dataFile.close();
}

int main(int argc, const char * argv[])
{
    for (int i=0; i<3; i++)
        for (int k=0; k<3; k++)
            runSimulation(k,i);
    
    return 0;
}
