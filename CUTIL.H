/*******************************************************************
   Common definitons for CUTIL.LIB
 *******************************************************************/
#ifndef __CUTIL__
  #define __CUTIL__

  /* definitions for the history-function */
  #define HISTORYTEMPLATE "%1: %2 (%3 / %4)"    /* Outline of history line */

  #define CCHMAXHISTORY         64          /* Max. length of line in protocol field  */
  #define MAXHISTORYLINES       256         /* Max. number of lines in protokol field */

  #define EA_HISTORYNAME    ".HISTORY"      /* EA-name for history */

  /* structure for entry of data to EAWriteMVMT */
  typedef struct _STRUC_EAT_DATA
      {
      USHORT          usEAType;             /* EAT_* */
      USHORT          uscValue;             /* length of pValue */
      PBYTE           pValue;               /* buffer */
      } STRUC_EAT_DATA;
  typedef STRUC_EAT_DATA *PSTRUC_EAT_DATA;

  BOOL   _System EAWriteASCII (PCHAR, PCHAR, PCHAR);
  BOOL   _System EAReadASCII  (PCHAR, PCHAR, PCHAR, PUSHORT);
  BOOL   _System EAWrite (PCHAR, PCHAR, PSTRUC_EAT_DATA);
  BOOL   _System EARead  (PCHAR, PCHAR, PSTRUC_EAT_DATA);
  BOOL   _System EAWriteMV (PCHAR, PCHAR, USHORT, PSTRUC_EAT_DATA);
  BOOL   _System EAReadMV  (PCHAR, PCHAR, USHORT, PSTRUC_EAT_DATA);
  BOOL   _System History (PCHAR, PCHAR, PCHAR);
  APIRET _System GetDateTime (PDATETIME, PCHAR, PCHAR);

#endif /* __CUTIL__ */

