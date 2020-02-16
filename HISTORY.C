/***************************************************************************
 *
 * PROGRAM NAME: HISTORY.C
 * -------------
 *
 * REVISION LEVEL: 1.2
 * ---------------
 *
 * WHAT THIS PROGRAM DOES:
 * -----------------------
 *  Write history entries to the OS/2 history settings (.HISTORY EA)
 *
 * ROUTINES:
 * ---------
 *  History
 *
 * COMPILE REQUIREMENTS:
 * ---------------------
 *  IBM C++ Set/2 Compiler Version 2.0
 *  IBM OS/2 2.1 Programmers Toolkit
 *
 * REQUIRED FILES:
 * ---------------
 *  CUTIL.H
 *
 * REQUIRED LIBRARIES:
 * -------------------
 *  OS2386.LIB    -   OS/2 32-Bit import library
 *
 * CHANGE LOG:
 * -----------
 * 
 *  Ver.    Date      Comment
 *  ----    --------  -------
 *  1.20    02-19-94  First release
 *
 *  Copyright (C) 1994 Noller & Breining Software
 *
 ******************************************************************************/
#define INCL_DOSMEMMGR
#define INCL_DOSDATETIME
#define INCL_DOSMISC
#include <os2.h>
#include <string.h>
#include <cutil.h>

BOOL History (PCHAR szFileName, PCHAR szUser, PCHAR szEvent)
    {
    STRUC_EAT_DATA  arValue[MAXHISTORYLINES + 2];
    DATETIME        DateTime;
    CHAR            szNewEntry[CCHMAXHISTORY];
    CHAR            szDate[20], szTime[20];
    PUCHAR          IvTable[4];
    ULONG           IvCount = 4;
    ULONG           ul;
    BOOL            bRC;

    IvTable[0] = szUser;
    IvTable[1] = szEvent;
    IvTable[2] = szDate;
    IvTable[3] = szTime;

    DosGetDateTime (&DateTime);
    GetDateTime (&DateTime, szTime, szDate);
    DosInsertMessage (IvTable, IvCount,
                      HISTORYTEMPLATE, sizeof (HISTORYTEMPLATE) + 1,
                      szNewEntry, CCHMAXHISTORY-1, &ul);
    szNewEntry[ul] = '\0';

    /* Prepare memory area for reading actual history EA */
    DosAllocMem ((PPVOID) &arValue[0].pValue, MAXHISTORYLINES * CCHMAXHISTORY,
         PAG_COMMIT | PAG_READ | PAG_WRITE);

    memset (arValue[0].pValue, '\0', MAXHISTORYLINES * CCHMAXHISTORY);

    arValue[0].uscValue = CCHMAXHISTORY;

    for (ul=1; ul<MAXHISTORYLINES; ul++)
       {
       arValue[ul].uscValue = CCHMAXHISTORY;
       arValue[ul].pValue   = arValue[ul-1].pValue + CCHMAXHISTORY;
       }

    arValue[MAXHISTORYLINES].pValue = NULL;
    bRC = EAReadMV (szFileName, EA_HISTORYNAME, EAT_MVMT, arValue);

    if (bRC)
        {
        /* Search for last used element */
        for (ul=0; (ul<MAXHISTORYLINES) && (arValue[ul].uscValue!=0); ul++)
            ;

        /* Enter new history line */
        arValue[ul].pValue   = szNewEntry;
        arValue[ul].uscValue = strlen(szNewEntry);
        arValue[ul].usEAType = EAT_ASCII;

        /* Write back history EA */
        arValue[ul+1].pValue   = NULL;
        arValue[ul+1].uscValue = 0;
        bRC = EAWriteMV (szFileName, EA_HISTORYNAME, EAT_MVMT,
            (ul == MAXHISTORYLINES) ? arValue+1 : arValue);
        }

    return bRC;
    }
