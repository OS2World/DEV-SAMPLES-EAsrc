/***************************************************************************
 *
 * PROGRAM NAME: DATETIME.C
 * -------------
 *
 * REVISION LEVEL: 1.2
 * ---------------
 *
 * WHAT THIS PROGRAM DOES:
 * -----------------------
 *  Get date and time in country specific format
 *
 * ROUTINES:
 * ---------
 *  GetDateTime
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
#define INCL_DOS

#include <os2.h>
#include <string.h>

/* --- internal function only --- */
PCHAR itoNc (PCHAR, USHORT, USHORT);

/*******************************************************************
   Get date and time in country specific format
   Entry:  pDateTime: Pointer to DATETIME-structure
   Exit:   szTime: string with time (minimal buffer length 10 chars)
           szDate: string with date (minimal buffer length 11 chars)
   return: Return value of DosQueryCtryInfo
 *******************************************************************/
APIRET GetDateTime (PDATETIME pDateTime, PCHAR szTime, PCHAR szDate)
    {
    ULONG ulLen;
    PCHAR pszString;
    COUNTRYCODE CtryCode;
    COUNTRYINFO CtryInfo;
    APIRET rc;

    *szTime = *szDate = '\0';
    ulLen = sizeof (COUNTRYINFO);
    CtryCode.country  = 0;
    CtryCode.codepage = 0;

    if ((rc = DosQueryCtryInfo (ulLen, &CtryCode, &CtryInfo, &ulLen)) == 0)
        {
        /* Assembly of date */
        switch (CtryInfo.fsDateFmt)
            {
            case 1:
                pszString = itoNc (szDate, pDateTime->day, 10) + 1;
                pszString = itoNc (pszString, pDateTime->month, 10) + 1;
                pszString = itoNc (pszString, pDateTime->year, 1000);
                break;

            case 2:
                pszString = itoNc (szDate, pDateTime->year, 1000) + 1;
                pszString = itoNc (pszString, pDateTime->month, 10) + 1;
                pszString = itoNc (pszString, pDateTime->day, 10);
                break;

            default:
                pszString = itoNc (szDate, pDateTime->month, 10) + 1;
                pszString = itoNc (pszString, pDateTime->day, 10) + 1;
                pszString = itoNc (pszString, pDateTime->year, 1000);
            }
        szDate[2] = szDate[5] = CtryInfo.szDateSeparator[0];
        *pszString = '\0';

        /* Assembly of time */
        if (CtryInfo.fsTimeFmt)
            pszString = itoNc (szTime, pDateTime->hours, 10);
        else
            pszString = itoNc (szTime, ((pDateTime->hours+11) % 12) + 1, 10);
        *pszString++ = CtryInfo.szTimeSeparator[0];
        pszString = itoNc (pszString, pDateTime->minutes, 10);
        *pszString++ = CtryInfo.szTimeSeparator[0];
        pszString = itoNc (pszString, pDateTime->seconds, 10);
        *pszString = '\0';
        if (CtryInfo.fsTimeFmt == 0)
            strcpy (pszString, (pDateTime->hours-1 < 13) ? "AM" : "PM");
        }

    return rc;
    }

PCHAR itoNc (PCHAR psz, USHORT usVal, USHORT N)
    {
    USHORT i;

    for (i=N; i>0; i/=10)
        {
        *psz++ = (CHAR)(usVal / i) + '0';
        usVal %= i;
        }

    return psz;
    }
