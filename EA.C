/***************************************************************************
 *
 * PROGRAM NAME: EA.C
 * -------------
 *
 * REVISION LEVEL: 1.2
 * ---------------
 *
 * WHAT THIS PROGRAM DOES:
 * -----------------------
 *  Routines for handling EAs.
 *
 * ROUTINES:
 * ---------
 *  CreateGEAList
 *  CreateFEAList
 *  CreateEAOPRd
 *  CreateEAOPWr
 *  EAWriteASCII
 *  EAReadASCII
 *  EAWriteMV
 *  EAReadMV
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
#define INCL_DOSFILEMGR
#include <os2.h>

#include <string.h>
#include <stdlib.h>
#include <cutil.h>

#pragma pack (1)
typedef struct _STRUC_EAT_SV        /* structure for EAT_ASCII */
    {
    USHORT usEAType;
    USHORT uscValue;
    CHAR   cValue[1];
    } STRUC_EAT_SV;
typedef STRUC_EAT_SV *PSTRUC_EAT_SV;

typedef struct _STRUC_MVST          /* EA-structure for strucEA */
    {                               /*  in STRUC_EA_MV          */
    USHORT uscValue;                /*  with EAT_MVST           */
    BYTE   bValue[1];
    } STRUC_MVST;
typedef STRUC_MVST *PSTRUC_MVST;

typedef struct _STRUC_MVMT          /* EA-structure fr strucEA */
    {                               /*  in STRUC_EA_MV          */
    USHORT usEAType;                /*  with EAT_MVMT           */
    USHORT uscValue;
    BYTE   bValue[1];
    } STRUC_MVMT;
typedef STRUC_MVMT *PSTRUC_MVMT;

typedef struct _STRUC_EAT_MV        /* strukture for EAT_MVMT/MVST */
    {
    USHORT          usEAType;
    USHORT          usCodepage;
    USHORT          uscEA;
    STRUC_MVMT      strucEA[1];
    } STRUC_EAT_MV;
typedef STRUC_EAT_MV *PSTRUC_EAT_MV;

#pragma pack ()

/*****************************************************************************
 * --- Internal function only ---
 * Create a Get-EA-Liste (GEAList)
 * The GEAList contains only 1 GEA-Eintrag.
 * Entry:  pszName: Name of EA
 * return: Pointer to GEAList
 *****************************************************************************/
PGEA2LIST CreateGEAList (PCHAR pszName)
    {
    PGEA2LIST pGEAl;

    DosAllocMem ((PPVOID)&pGEAl, sizeof (GEA2LIST) + strlen (pszName),
        PAG_COMMIT | PAG_READ | PAG_WRITE);
    pGEAl->cbList = sizeof (GEA2LIST) + strlen (pszName);
    pGEAl->list->oNextEntryOffset = 0;          /* last entry */
    pGEAl->list->cbName = (CHAR) strlen (pszName);
    strcpy (pGEAl->list->szName, pszName);

    return pGEAl;
    }

/*****************************************************************************
 * --- Internal function only ---
 * Create a Full-EA-Liste (FEAList)
 * The FEAList contains only 1 FEA-Eintrag.
 * Entry:  pszName : Name of EA
 *         pValue  : Value of EA
 *         uscValue: Length of EA value
 * return: Pointer to FEAList
 *****************************************************************************/
PFEA2LIST CreateFEAList (PCHAR pszName, PBYTE pValue, USHORT uscValue)
    {
    PFEA2LIST pFEAl;

    DosAllocMem ((PPVOID)&pFEAl, sizeof (FEA2LIST) + strlen (pszName) + uscValue,
        PAG_COMMIT | PAG_READ | PAG_WRITE);
    pFEAl->cbList = sizeof (FEA2LIST) + strlen (pszName) + uscValue;
    pFEAl->list->oNextEntryOffset = 0;          /* last entry */
    pFEAl->list->fEA = 0;                       /* no flags */
    pFEAl->list->cbName = (CHAR) strlen (pszName);
    pFEAl->list->cbValue = uscValue;
    strcpy (pFEAl->list->szName, pszName);
    memcpy ((PBYTE)pFEAl->list->szName+strlen(pszName)+1, pValue, uscValue);

    return pFEAl;
    }

/*****************************************************************************
 * --- Internal function only ---
 * Create an EAOP-Struktur with FEA-buffer at the end. This buffer may be used
 * for DosFind*, DosGetFileInfo or DosGetPathInfo-calls.
 * Entry:  ulcBuffer: Size of buffer (EAOP2 + FEAList)
 *         pGEAl:     Pointer to GEAList
 * return: Pointer to EAOP-structure
 *****************************************************************************/
PEAOP2 CreateEAOPRd (ULONG ulcBuffer, PGEA2LIST pGEAl)
    {
    PEAOP2 pEAOP;

    DosAllocMem ((PPVOID)&pEAOP, ulcBuffer, PAG_COMMIT | PAG_READ | PAG_WRITE);
    pEAOP->fpGEA2List = pGEAl;
    pEAOP->fpFEA2List = (FEA2LIST *)(pEAOP + 1);
    pEAOP->fpFEA2List->cbList = ulcBuffer - sizeof (EAOP2);

    return pEAOP;
    }

/*****************************************************************************
 * --- Internal function only ---
 * Create an EAOP-Struktur with FEA-buffer at the end. This buffer may be used
 * for DosSetFileInfo or DosSetPathInfo-calls.
 * Entry:  pFEAl:     Pointer to FEAList
 * return: Pointer to EAOP-structure
 *****************************************************************************/
PEAOP2 CreateEAOPWr (PFEA2LIST pFEAl)
    {
    PEAOP2 pEAOP;

    DosAllocMem ((PPVOID)&pEAOP, sizeof (PEAOP2), PAG_COMMIT | PAG_READ | PAG_WRITE);
    pEAOP->fpGEA2List = NULL;
    pEAOP->fpFEA2List = pFEAl;

    return pEAOP;
    }

/*****************************************************************************
 * Write EAT_ASCII-EAs (value of EA is ASCII-string).
 * Entry:  pszPathName: Filename
 *         pszEAName:   Name of EA
 *         pszString:   Value of EA (ASCIIZ-String)
 * return: TRUE:  EA was written
 *         FALSE: Error occured
 *****************************************************************************/
BOOL EAWriteASCII (PCHAR pszPathName, PCHAR pszEAName, PCHAR pszString)
    {
    BOOL          bRC;
    PSTRUC_EAT_SV peaASCII;
    PFEA2LIST     pFEAl;
    PEAOP2        pEAOP;

    /* Fill EA structure */
    DosAllocMem ((PPVOID)&peaASCII, sizeof (STRUC_EAT_SV) + strlen (pszString) - 1,
        PAG_COMMIT | PAG_READ | PAG_WRITE);
    peaASCII->usEAType = EAT_ASCII;
    peaASCII->uscValue = (USHORT) strlen (pszString);
    memcpy (peaASCII->cValue, pszString, strlen (pszString));

    /* Create FEA-list */
    pFEAl = CreateFEAList (pszEAName, (PBYTE) peaASCII,
        sizeof (STRUC_EAT_SV) + (USHORT) strlen (pszString) - 1);

    /* Create EAOP-strukture */
    pEAOP = CreateEAOPWr (pFEAl);

    /* Write EA */
    bRC = (DosSetPathInfo (pszPathName, FIL_QUERYEASIZE,
        pEAOP, sizeof (EAOP2), DSPI_WRTTHRU)) ? FALSE : TRUE;

    /* Deallocate buffers */
    DosFreeMem (pEAOP);
    DosFreeMem (pFEAl);
    DosFreeMem (peaASCII);

    return bRC;
    }

/*****************************************************************************
 * Read EAT_ASCII-EAs (value of EA is ASCII-string).
 * Entry:  pszPathName: Filename
 *         pszEAName:   Name of EA
 *         puscValue:   Size of buffer 'pszString'
 * Exit:   pszString:   Value of EA (ASCIIZ)
 *         puscValue:   Length of value of EA (strlen (pszString))
 * return: TRUE:  EA was read
 *         FALSE: Error occured
 *****************************************************************************/
BOOL EAReadASCII (PCHAR pszPathName, PCHAR pszEAName, PCHAR pszString, PUSHORT puscValue)
    {
    BOOL        bRC;
    LONG        lcBytes;
    FILESTATUS4 ffb4;
    PGEA2LIST   pGEAl;
    PEAOP2      pEAOP;
    union _pEA
        {
        PFEA2   pFEA;
        PUSHORT pWord;
        PBYTE   pByte;
        } pEA;

    if (*puscValue > 0)             /* Prepare buffer for case of an error */
        *pszString = '\0';
    bRC = FALSE;                    /* Prepare return value for cas of an error */

    if (!DosQueryPathInfo (pszPathName, FIL_QUERYEASIZE, &ffb4, sizeof (FILESTATUS4)))
        {
        pGEAl = CreateGEAList (pszEAName);
        pEAOP = CreateEAOPRd (sizeof (EAOP2) + ffb4.cbList, pGEAl);
        if (!DosQueryPathInfo (pszPathName, FIL_QUERYEASFROMLIST,
                pEAOP, sizeof (EAOP2)))
            {
            pEA.pFEA = pEAOP->fpFEA2List->list;
            if (pEA.pFEA->cbValue != 0)
                {
                pEA.pByte = (PBYTE)&(pEA.pFEA->szName) + pEA.pFEA->cbName + 1;
                if (*pEA.pWord++ == EAT_ASCII)
                    {
                    lcBytes = min (*pEA.pWord, (LONG) *puscValue-1);
                    if (lcBytes > 0)
                        {
                        memcpy (pszString, pEA.pWord + 1, lcBytes);
                        pszString[lcBytes] = '\0';
                        }
                    *puscValue = *pEA.pWord;
                    bRC = TRUE;
                    }
                }
            else
                {
                /* EA not present */
                *puscValue = 0;
                bRC = TRUE;
                }
            }
        DosFreeMem (pEAOP);
        DosFreeMem (pGEAl);
        }

    /* In case of an error return 0 as length of string */
    if (!bRC)
        *puscValue = 0;

    return bRC;
    }

/*****************************************************************************
 * Write single-value-EAs (value of EA may be any type)
 * Entry:  pszPathName: Filename
 *         pszEAName:   Name if EA
 *         pstrucValue: Input data structure (see CUTIL.H)
 * return: TRUE:  EA was written
 *         FALSE: Error occured
 *****************************************************************************/
BOOL EAWrite (PCHAR pszPathName, PCHAR pszEAName, PSTRUC_EAT_DATA pstrucValue)
    {
    BOOL          bRC;
    PSTRUC_EAT_SV peaData;
    PFEA2LIST     pFEAl;
    PEAOP2        pEAOP;

    /* Fill EA structure */
    DosAllocMem ((PPVOID)&peaData,
        sizeof (STRUC_EAT_SV) + pstrucValue->uscValue - 1,
        PAG_COMMIT | PAG_READ | PAG_WRITE);
    peaData->usEAType = pstrucValue->usEAType;
    peaData->uscValue = pstrucValue->uscValue;
    memcpy (peaData->cValue, pstrucValue->pValue, pstrucValue->uscValue);

    /* Create FEA-list */
    pFEAl = CreateFEAList (pszEAName, (PBYTE) peaData,
        sizeof (STRUC_EAT_SV) + pstrucValue->uscValue - 1);

    /* Createt EAOP-structure */
    pEAOP = CreateEAOPWr (pFEAl);

    /* Write EA */
    bRC = (DosSetPathInfo (pszPathName, FIL_QUERYEASIZE,
        pEAOP, sizeof (EAOP2), DSPI_WRTTHRU)) ? FALSE : TRUE;

    /* Deallocate buffers */
    DosFreeMem (pEAOP);
    DosFreeMem (pFEAl);
    DosFreeMem (peaData);

    return bRC;
    }

/*****************************************************************************
 * Read single-value-EAs (value of EA may be any type)
 * If pstrucValue->uscValue is larger on return as the value on entry,
 * the buffer is too small to hold the complete EA. The buffer will be
 * filled with the correct value up to the end. EAT_ASCII EAs will be
 * terminated with '\0'.
 * Entry:  pszPathName: Filename
 *         pszEAName:   Name of EA
 *         pstrucValue: Input data structure (see CUTIL.H)
 * Exit:   pstrucValue: Result
 * return: TRUE:  EA was read
 *         FALSE: Error occured
 *****************************************************************************/
BOOL EARead (PCHAR pszPathName, PCHAR pszEAName, PSTRUC_EAT_DATA pstrucValue)
    {
    BOOL        bRC;
    LONG        lcBytes;
    FILESTATUS4 ffb4;
    PGEA2LIST   pGEAl;
    PEAOP2      pEAOP;
    union _pEA
        {
        PFEA2   pFEA;
        PUSHORT pWord;
        PBYTE   pByte;
        } pEA;

    bRC = FALSE;                    /* Prepare return value for case of an error */

    if (!DosQueryPathInfo (pszPathName, FIL_QUERYEASIZE, &ffb4, sizeof (FILESTATUS4)))
        {
        pGEAl = CreateGEAList (pszEAName);
        pEAOP = CreateEAOPRd (sizeof (EAOP2) + ffb4.cbList, pGEAl);
        if (!DosQueryPathInfo (pszPathName, FIL_QUERYEASFROMLIST,
                pEAOP, sizeof (EAOP2)))
            {
            pEA.pFEA = pEAOP->fpFEA2List->list;
            if (pEA.pFEA->cbValue != 0)
                {
                pEA.pByte = (PBYTE)&(pEA.pFEA->szName) + pEA.pFEA->cbName + 1;
                pstrucValue->usEAType = *pEA.pWord++;
                lcBytes = min (*pEA.pWord, (LONG)pstrucValue->uscValue);
                if (lcBytes > 0)
                    {
                    memcpy (pstrucValue->pValue, pEA.pWord + 1, lcBytes);
                    if (pstrucValue->usEAType == EAT_ASCII)
                        {
                        /* EAT_ASCII-EAs will always be terminated with '\0' */
                        lcBytes = min (*pEA.pWord, (LONG)pstrucValue->uscValue - 1);
                        pstrucValue->pValue[lcBytes] = '\0';
                        }
                    }
                pstrucValue->uscValue = *pEA.pWord;
                bRC = TRUE;
                }
            else
                {
                /* EA not present */
                pstrucValue->uscValue = 0;
                bRC = TRUE;
                }
            }
        DosFreeMem (pEAOP);
        DosFreeMem (pGEAl);
        }

    /* In case of an error return 0 as length of string */
    if (!bRC)
        pstrucValue->uscValue = 0;

    return bRC;
    }

/*****************************************************************************
 * Write multi-value EAs (EAT_MVST and EAT_MVMT) (EA may contain more than
 * 1 value. All values may be of same type (EAT_MVST) or different type
 * (EAT_MVMT)).
 * Data is entered in the structure arValue[]. The last entry must
 * contain arValue[].pValue = NULL.
 * If type is EAT_MVST, only the first value arValue[0].usEAType is relevant.
 * Entry:  pszPathName: Filename
 *         pszEAName:   Name of EA
 *         usEAType:    EAT_MVST or EAT_MVMT
 *         arValue:     Entry data structure
 * return: TRUE:  EA was written
 *         FALSE: Error occured
 *****************************************************************************/
BOOL EAWriteMV (PCHAR pszPathName, PCHAR pszEAName,
                USHORT usEAType, STRUC_EAT_DATA arValue[])
    {
    BOOL            bRC;
    ULONG           ulcValue, ulcBytes, i;
    PSTRUC_EAT_MV   peaMV;
    union _pEA
        {
        PSTRUC_MVMT pMT;                /* Multitype EA  */
        PSTRUC_MVST pST;                /* Singletype EA */
        } pEA;
    PFEA2LIST       pFEAl;
    PEAOP2          pEAOP;

    /* Count EA values; Result is in ulcValue */
    for (ulcValue=0; arValue[ulcValue].pValue != NULL; ulcValue++);
    if (ulcValue == 0)
        return TRUE;

    /* Allocate memory for attributes; STRUC_EAT_MV contains */
    /* a MVMT-structure on default even at MVST attributes.  */
    ulcBytes = sizeof (STRUC_EAT_MV);
    switch (usEAType)
        {
        case EAT_MVST:
            ulcBytes += (ulcValue-1)*sizeof (STRUC_MVST);
            break;

        case EAT_MVMT:
            ulcBytes += (ulcValue-1)*sizeof (STRUC_MVMT);
            break;

        default:
            return FALSE;
        }
    for (i=0; i<ulcValue; i++)
        ulcBytes += arValue[i].uscValue - 1;
    DosAllocMem ((PPVOID)&peaMV, ulcBytes, PAG_COMMIT | PAG_READ | PAG_WRITE);

    /* Fill EA structure */
    peaMV->usEAType   = usEAType;
    peaMV->usCodepage = 0;
    peaMV->uscEA      = ulcValue;
    pEA.pMT = peaMV->strucEA;
    if (usEAType == EAT_MVST)                       /* For EA_MVST: Type of first */
        {                                           /*  EA is type of all EAs     */
        pEA.pMT->usEAType = arValue[0].usEAType;
        pEA.pMT = (PSTRUC_MVMT)&(peaMV->strucEA[0].uscValue);
        }
    for (i=0; i<ulcValue; i++)
        {
        if (usEAType == EAT_MVMT)
            {
            pEA.pMT->usEAType = arValue[i].usEAType;
            pEA.pST = (PSTRUC_MVST)&(pEA.pMT->uscValue);
            }
        /* for EA_MVMT: usEAType is written. From now on */
        /* pEA is used as PSTRUC_MVST-pointer.           */
        pEA.pST->uscValue  = arValue[i].uscValue;
        memcpy (pEA.pST->bValue, arValue[i].pValue, pEA.pST->uscValue);
        pEA.pST = (PSTRUC_MVST)(pEA.pST->bValue + pEA.pST->uscValue);
        }

    /* Create FEA-list */
    pFEAl = CreateFEAList (pszEAName, (PBYTE) peaMV, ulcBytes);

    /* Create EAOP-structure */
    pEAOP = CreateEAOPWr (pFEAl);

    /* Write EA */
    bRC = (DosSetPathInfo (pszPathName, FIL_QUERYEASIZE,
        pEAOP, sizeof (EAOP2), DSPI_WRTTHRU)) ? FALSE : TRUE;

    /* Deallocate buffers */
    DosFreeMem (pEAOP);
    DosFreeMem (pFEAl);
    DosFreeMem (peaMV);

    return bRC;
    }

/*****************************************************************************
 * Read Multi-Value EAs (EAT_MVST und EAT_MVMT). (EA may contain more than
 * 1 value. All values may be of same type (EAT_MVST) or different type
 * (EAT_MVMT)).
 * Data is entered in the structure arValue[]. The last entry must
 * contain arValue[].pValue = NULL.
 * If type is EAT_MVST, the EA-type is returned in arValue[0].usEAType.
 * If the arValue-structure is too long, all remaining entries of
 * arValue[].uscValue are set to 0 gesetzt.
 * EAT_ASCII-EAs will always be terminated with '\0'. If the buffer is too
 * small (arValue[].uscValue), the string will be cut; but terminated with
 * '\0' nevertheless.
 * Entry:  pszPathName: Filename
 *         pszEAName:   Name of EA
 *         usEAType:    EAT_MVST or EAT_MVMT
 *         arValue:     Entry data structure
 * Exit:   arValue:     Result
 * return: TRUE:  EA was read
 *         FALSE: Error occured
 *****************************************************************************/
BOOL EAReadMV (PCHAR pszPathName, PCHAR pszEAName,
               USHORT usEAType, STRUC_EAT_DATA arValue[])
    {
    USHORT      usEAType2;
    BOOL        bRC;
    ULONG       ulcValue, ulcMaxEA, ulcBytes, i;
    FILESTATUS4 ffb4;
    PGEA2LIST   pGEAl;
    PEAOP2      pEAOP;
    union _pEA
        {
        PFEA2           pFEA;
        PSTRUC_EAT_MV   peaMV;
        PSTRUC_MVST     pST;
        PSTRUC_MVMT     pMT;
        PUSHORT         pWord;
        PBYTE           pByte;
        } pEA;

    /* Count ASCII-strings; Result is in ulcValue */
    for (ulcValue=0; arValue[ulcValue].pValue != NULL; ulcValue++);
    if (ulcValue == 0)
        return TRUE;

    /* Read der EAs */
    bRC = FALSE;
    if (!DosQueryPathInfo (pszPathName, FIL_QUERYEASIZE, &ffb4, sizeof (FILESTATUS4)))
        {
        pGEAl = CreateGEAList (pszEAName);
        pEAOP = CreateEAOPRd (sizeof (EAOP2) + ffb4.cbList, pGEAl);
        if (!DosQueryPathInfo (pszPathName, FIL_QUERYEASFROMLIST,
                pEAOP, sizeof (EAOP2)))
            {
            pEA.pFEA = pEAOP->fpFEA2List->list;
            /* Attribute found? */
            if (pEA.pFEA->cbValue != 0)
                {
                pEA.pByte = (PBYTE)&(pEA.pFEA->szName) + pEA.pFEA->cbName + 1;
                /* Attribute types correct? */
                if (pEA.peaMV->usEAType == usEAType)
                    {
                    ulcMaxEA = pEA.peaMV->uscEA;
                    pEA.pMT = pEA.peaMV->strucEA;
                    if (usEAType == EAT_MVST)
                        {
                        usEAType2 = arValue[0].usEAType = pEA.pMT->usEAType;
                        pEA.pMT = (PSTRUC_MVMT)&(pEA.pMT->uscValue);
                        }
                    for (i=0; i<ulcValue; i++)
                        {
                        if (i<ulcMaxEA)
                            {
                            if (usEAType == EAT_MVMT)
                                {
                                arValue[i].usEAType = pEA.pMT->usEAType;
                                pEA.pST = (PSTRUC_MVST)&(pEA.pMT->uscValue);
                                }
                            else
                                arValue[i].usEAType = usEAType2;
                            ulcBytes = min (pEA.pST->uscValue, arValue[i].uscValue);
                            /* EAT_ASCII-EAs will always be terminated with '\0' */
                            if (arValue[i].usEAType == EAT_ASCII)
                                {
                                ulcBytes = min (ulcBytes, arValue[i].uscValue-1);
                                arValue[i].pValue[ulcBytes] = '\0';
                                }
                            arValue[i].uscValue = ulcBytes;
                            memcpy (arValue[i].pValue, pEA.pST->bValue, ulcBytes);
                            }
                        else
                            arValue[i].uscValue = 0;
                        pEA.pST = (PSTRUC_MVST)(pEA.pST->bValue + pEA.pST->uscValue);
                        }
                    arValue[ulcValue].uscValue = 0;
                    bRC = TRUE;
                    }
                }
            else
                {
                /* EA not present */
                bRC = TRUE;
                for (i=0; i<ulcValue; i++)
                    {
                    arValue[i].uscValue = 0;
                    arValue[i].usEAType = 0;
                    }
                }
            }

        DosFreeMem (pEAOP);
        DosFreeMem (pGEAl);
        }

    /* In case of an error fill all lengths with 0 */
    if (!bRC)
        for (i=0; i<ulcValue; i++)
            arValue[i].uscValue = 0;

    return bRC;
    }
