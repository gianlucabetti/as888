000100010914     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES)
000200010925     H*PARMS COMMIT(*NONE) DATFMT(*ISO) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000300010903     H*------------------------------------------------------------------------*
000400010903     H* Aggiorna i campi ...... dei files ........                             *
000500010903     H*                  DCTVPC           FNDCT00F                             *
000600010903     H*                  DCLVPR           FNDCL00F                             *
000700010903     H*                  DCLVPT           FNDCL00F                             *
000800010903     H*                  DCLVPL           FNDCL00F                             *
000900010903     H*                  DPTVVM           FNDPT00F                             *
001000010903     H* valorizzando tali campi divisa - nel caso di importi valorizza-        *
001100010919     H* ti - con la divisa corrente BARTOLINI della tabella GED DANNI          *
001200010919     H* (se NON già valorizzati).                                              *
001300010903     H*------------------------------------------------------------------------*
001400010910     H*
001500010903     H DATEDIT(*YMD)   EXPROPTS(*RESDECPOS)
001600010903     D*
001700010903     D*------------------------------------------------------------------------*
001800010903     D* SCHIERE
001900010903     D*------------------------------------------------------------------------*
002000010903     D*
002100010903     D* Schiera per costruzione stringhe di comando SQL
002200010919     D CMD             S             80    dim(10) ctdata perrcd(1)
002300010903     D*
002400010903     D*------------------------------------------------------------------------*
002500010903     D* STRUTTURE DATI
002600010903     D*------------------------------------------------------------------------*
002700010903     D*
002800010903     D*------------------------------------------------------------------------*
002900010903     D* VARIABILI
003000010903     D*------------------------------------------------------------------------*
003100010903     D*
003200010903     D STRINGA         S            256    INZ
003300010903     D $X              S              3  0 INZ
003400010903     C*
003500010903     C*------------------------------------------------------------------------*
003600010903     C* MAIN LINE
003700010903     C*------------------------------------------------------------------------*
003800010903     C*
003900010903     C* ESECUZIONE COMANDI
004000010903     C                   EXSR      EXESQL_DCT
004100010903     C                   EXSR      EXESQL_DCL
004200010903     C                   EXSR      EXESQL_DPT
004300010903     C*
004400010903     C* FINE
004500010903     C                   MOVEL     *ON           *INLR
004600010903     C*
004700010903     C*------------------------------------------------------------------------*
004800010903     C* EXESQL_DCT - ESECUZIONE COMANDI PER FILE FNDCT00F
004900010903     C*------------------------------------------------------------------------*
005000010903     C     EXESQL_DCT    BEGSR
005100010903     C*
005200010903     C* Composizione stringa per composizione comando SQL da passare.
005300010903     C*
005400010903     C* - campo DCTVPC:
005500010903     C                   EVAL      $X = 1
005600010903     C                   EXSR      EXESQL
005700010903     C*
005800010903     C                   ENDSR
005900010903     C*
006000010903     C*------------------------------------------------------------------------*
006100010903     C* EXESQL_DCL - ESECUZIONE COMANDI PER FILE FNDCL00F
006200010903     C*------------------------------------------------------------------------*
006300010903     C     EXESQL_DCL    BEGSR
006400010903     C*
006500010903     C* Composizione stringa per composizione comando SQL da passare.
006600010903     C*
006700010903     C* - campo DCLVPR:
006800010910     C                   EVAL      $X = 2
006900010903     C                   EXSR      EXESQL
007000010903     C*
007100010903     C* - campo DCLVPI:
007200010910     C                   EVAL      $X = 3
007300010903     C                   EXSR      EXESQL
007400010903     C*
007500010903     C* - campo DCLVPL:
007600010910     C                   EVAL      $X = 4
007700010903     C                   EXSR      EXESQL
007800010903     C*
007900010903     C                   ENDSR
008000010903     C*
008100010903     C*------------------------------------------------------------------------*
008200010903     C* EXESQL_DPT - ESECUZIONE COMANDI PER FILE FNDPT00F
008300010903     C*------------------------------------------------------------------------*
008400010903     C     EXESQL_DPT    BEGSR
008500010903     C*
008600010903     C* Composizione stringa per composizione comando SQL da passare.
008700010903     C*
008800010903     C* - campo DPTVVM:
008900010910     C                   EVAL      $X = 5
009000010903     C                   EXSR      EXESQL
009100010903     C*
009200010903     C                   ENDSR
009300010903     C*
009400010903     C*------------------------------------------------------------------------*
009500010903     C* EXESQL - CREAZIONE STRINGA SQL PER COMANDO SQL DA ESEGUIRE
009600010903     C*------------------------------------------------------------------------*
009700010903     C     EXESQL        BEGSR
009800010903     C*
009900010903     C                   reset                   STRINGA
010000010903     C*
010100010919     C                   eval      STRINGA = CMD($X)
010200010903     C*
010300010903     C/EXEC SQL
010400010903     C+     EXECUTE IMMEDIATE :STRINGA
010500010903     C/END-EXEC
010600010903     C*
010700010903     C                   ENDSR
010800010903     C*------------------------------------------------------------------------*
010900010919** CMD
011000010914UPDATE FNDCT00F SET DCTVPV='ITL'  WHERE DCTVPV='   '  AND DCTIPV<>0
011100010910UPDATE FNDCL00F SET DCLVPR='ITL'  WHERE DCLVPR='   '  AND DCLIPR<>0
011200010910UPDATE FNDCL00F SET DCLVPT='ITL'  WHERE DCLVPT='   '  AND DCLIPT<>0
011300010910UPDATE FNDCL00F SET DCLVPL='ITL'  WHERE DCLVPL='   '  AND DCLIPL<>0
011400010910UPDATE FNDPT00F SET DPTVVM='ITL'  WHERE DPTVVM='   '  AND DPTVLM<>0
