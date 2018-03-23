000100030401     H*--------------------------------------------------------------------------------------------*
000200030401     H DECEDIT('0,') DATEDIT(*DMY.)
000300030401     H*--------------------------------------------------------------------------------------------*
000400030404     D*---
000500030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
000600030404     D*---
000700091211     D TRBMFCDS      e ds
000800001006     D*---
000900070607     C*
001000070607     C* Ricevimento parametri
001100090513     C     *ENTRY        PLIST
001200090514     C                   PARM                    IFSNAM           10
001300090513     C                   PARM                    FILNAM           10
001400090513     C                   PARM                    JOBNAM           10
001500090513     C                   PARM                    USRNAM           10
001600090513     C                   PARM                    JOBNUM            6
001700090513     C                   PARM                    NUMFIL            6
001800090514     C                   PARM                    SUBFLDR          50
001900090514     C*
002000090514     C* Verifico correttezza formale sotto-cartella
002100090514     C                   IF        SUBFLDR <> *blanks AND
002200090514     C                             %subst(%trim(SUBFLDR):%len(%trim(SUBFLDR)):1)
002300090514     C                             <> '/'
002400090514     C                   EVAL      SUBFLDR = %trim(SUBFLDR) + '/'
002500090514     C                   ENDIF
002600001006     C*
002700091211     C* Valorizzo la DS pgm utilità TRBMFCC
002800091211     C                   CLEAR                   TRBMFCDS
002900030404     C                   EVAL      DFCFNAM  = FILNAM
003000030404     C                   EVAL      DFCJNAM  = JOBNAM
003100030404     C                   EVAL      DFCUNAM  = USRNAM
003200030404     C                   EVAL      DFCJNUM  = JOBNUM
003300030404     C                   EVAL      DFCFNUM  = NUMFIL
003400091211     C                   EVAL      DFCFOUT  = 'TRBMUWWF'
003500040907     C                   EVAL      DFCLOUT  = 'QTEMP'
003600030514     C                   EVAL      DFCFOPT  = 'R'
003700030404     C*
003800090513     C                   CALL(e)   'TRBMAF01C'
003900090514     C                   PARM                    IFSNAM
004000030401     C                   PARM                    FILNAM
004100030401     C                   PARM                    JOBNAM
004200030401     C                   PARM                    USRNAM
004300030401     C                   PARM                    JOBNUM
004400030401     C                   PARM                    NUMFIL
004500090514     C                   PARM                    SUBFLDR
004600091211     C                   PARM                    TRBMFCDS
004700001006     C*
004800090513     C                   SETON                                        LR
