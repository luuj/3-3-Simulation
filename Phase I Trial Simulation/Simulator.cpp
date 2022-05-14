#include "Simulator.h"

/*Generates three patients and checks if they get a DLT with current probability*/
int testDLT(double probability)
{
    int count = 0;
    for (int i=0; i<3; i++)
    {
        if (uniformRandGen(0,1) < probability)
            count++;
    }
    return count;
}

/*Runs 3+3 rule algorithm until MTD is discovered*/
int runSimulation(double* DLT, int* patientCount, int& doseLevelsTested, int& totalDLT)
{
    //Initialize data
    bool escalationStopped = false;
    int dltCount, currentDoseLevel = STARTING_DOSE; //Set current dose level
    totalDLT = 0; doseLevelsTested = STARTING_DOSE;
    for (int i=0; i<DOSE_LEVEL; i++)
        patientCount[i] = 0;
    
    //3+3 Design Algorithm
    while (currentDoseLevel < DOSE_LEVEL)
    {
        dltCount = testDLT(DLT[currentDoseLevel]); //Enter in 3 patients at current dose level
        totalDLT+= dltCount;
        patientCount[currentDoseLevel] += 3;
        
        if (PRINT)
            std::cout << "Curr Dose Level:" << currentDoseLevel << " Patients on dose:" << patientCount[currentDoseLevel] << " DLTs:" << dltCount << std::endl;
        
        //Apply rule based on # of DLTs
        switch (dltCount)
        {
            case 0: //0 patients experience a DLT so dose escalation occurs
                if (!escalationStopped)
                    currentDoseLevel++;
                break;
                
            case 1://1 patient experiences a DLT so three patients are added at same dose level
                if (patientCount[currentDoseLevel] == 6 && !escalationStopped)
                {
                    doseLevelsTested = std::max(currentDoseLevel, doseLevelsTested);
                    escalationStopped = true;
                    currentDoseLevel--;
                }
                break;
                
            default://2 or 3 patients experience a DLT, stop dose escalation and check previous dose level
                doseLevelsTested = std::max(currentDoseLevel, doseLevelsTested);
                escalationStopped = true;
                currentDoseLevel--;
                break;
        }
        
        if (escalationStopped) //De-escalation starts
        {
            if (patientCount[currentDoseLevel] == 6) //If previous dose level has 6 patients, declare it as MTD, else add 3 more patients at previous dose
                return currentDoseLevel;
        }
    }
    
    //If final dose level is reached and MTD still not discovered
    doseLevelsTested = DOSE_LEVEL-1;
    return DOSE_LEVEL-1;
}
