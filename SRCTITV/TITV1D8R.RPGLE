000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300060419
000400060419     D*------------------
000500060419     D* DS "XSRDA8" - CONTROLLA DATA (8)
000600060419     D*------------------
000700060419     D WLBDA8          DS                  INZ
000800060419     D  G08DAT                 1      8  0
000900060419     D  G08INV                 9     16  0
001000060419     D  G08ERR                17     17
001100060419     D  G08TGI                18     22  0
001200080213
001300080213     D*------------------
001400080213     D* DEFINIZIONE DS PASSAGGIO PARAMETRI ARCHITTETTURA
001500080213     D*------------------
001600080213     D KPJBA         E DS
001700080213
001800080213     D*------------
001900080213     D* DS X ESECUZIONE STATISTICA
002000080213     D*------------
002100080213     D TIS1W2IDS     E DS                  INZ
002200080213
002300080213     D*------------------
002400080213     D* DS X PASSAGGIO PARAMETRI
002500080213     D*------------------
002600080213     D PARAM           DS                  INZ
002700080213     D  PRMTIS1W2IDS                       LIKE(TIS1W2IDS)
002800080213     D  PRMDAT                       10
002900080213     D  PRMORA                        8
003000080213     D  PRMSUF                        1
003100080213     D  PRMTLG                        2
003200080213     D  PRMFILE                       1
003300080213     D  PRMVGD                        1
003400080213     D  PRMCLIVAS                     8
003500060418
003600080213     D*------------------
003700060331     D* VARIABILI DI WRK
003800080213     D*------------------
003900080213     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
004000080213     D wMeseCor01      s              8  0 INZ
004100080214     D wMesePre01      s              8  0 INZ
004200080213     D wMesePre31      s              8  0 INZ
004300080214     D wMesePre01Inv   s              8  0 INZ
004400080214     D wMesePre31Inv   s              8  0 INZ
004500080214     D dataIn          s              8  0 INZ
004600080214     D dataOut         s              8  0 INZ
004700080213
004800080213     D*------------------
004900080213     D* COSTANTI RELATIVE AL CLIENTE
005000080213     D*------------------
005100080213     D cost_SUF        s              1    inz('U')                             * Singolo O Unifican
005200080213     D cost_TLE        s              2    inz('WW')                            * Tipo LEgame
005300080213     D cost_KSC        s              8    inz('00632910')                      * Cliente Mittente
005400080213     D cost_CLIVAS     s              8    inz('01020001')                      * Cliente VAS
005500080213
005600080213
005700080213     C*
005800080213     C* Calcolo le date necessarie
005900080213     C                   exsr      calDATE
006000080213     C*
006100080213     C* Imposto il passaggio parametri
006200080213     C                   exsr      pasPAR
006300080213     C*
006400080213     C* Chiamo il *pgm standard con i parametri personalizzati
006500080213     C                   call      'TIS7W5C'
006600080213     C                   parm                    KPJBA
006700060418     C*
006800921023     C                   SETON                                        LR
006900080213
007000080213
007100080213
007200080213     C     pasPAR        BEGSR
007300080213     C*
007400080213     C                   CLEAR                   TIS1W2IDS
007500080213     C*
007600080213     C                   MOVE(P)   cost_KSC      wVIDKSCnum        8 0
007700080213     C                   MOVE      wVIDKSCnum    KSC
007800080214     C                   MOVE      wMesePre01Inv wVIDDAIAlfa       8
007900080213     C                   EVAL      DIN = %subst(wVIDDAIAlfa:1:2)+'/'+
008000080213     C                                   %subst(wVIDDAIAlfa:3:2)+'/'+
008100080213     C                                   %subst(wVIDDAIAlfa:5:4)
008200080214     C                   MOVE      wMesePre31Inv wVIDDAFAlfa       8
008300080213     C                   EVAL      DFI = %subst(wVIDDAFAlfa:1:2)+'/'+
008400080213     C                                   %subst(wVIDDAFAlfa:3:2)+'/'+
008500080213     C                                   %subst(wVIDDAFAlfa:5:4)
008600080213     C                   EVAL      LNA = 999
008700080213     C                   EVAL      CKC = 'S'
008800080213     C                   EVAL      CKG = 'S'
008900080213     C                   EVAL      CKA = 'S'
009000080213     C                   EVAL      CKM = 'S'
009100080213     C                   EVAL      CKP = 'S'
009200080213     C                   EVAL      CKT = 'S'
009300080213     C                   EVAL      CKL = 'S'
009400080606     C                   EVAL      SERVIZIO_C = 'C'
009500080606     C                   EVAL      SERVIZIO_D = 'D'
009600080606     C                   EVAL      SERVIZIO_E = 'E'
009700091111     C                   EVAL      SERVIZIO_H = 'H'
009800080213     C*
009900080213     C* Edito la data e ora corrente
010000080213     C                   MOVEL     WN8           wDATCORAlfa       8
010100080213     C                   MOVEL     ORACOR        wORACORAlfa       6
010200080213     C                   EVAL      PRMDAT = %subst(wDATCORAlfa:1:2)+'/'+
010300080213     C                                      %subst(wDATCORAlfa:3:2)+'/'+
010400080213     C                                      %subst(wDATCORAlfa:5:4)
010500080213     C                   EVAL      PRMORA = %subst(wORACORAlfa:1:2)+':'+
010600080213     C                                      %subst(wORACORAlfa:3:2)+':'+
010700080213     C                                      %subst(wORACORAlfa:5:2)
010800080213     C*
010900080213     C                   EVAL      PRMTIS1W2IDS = TIS1W2IDS
011000080213     C*
011100080213     C                   EVAL      prmsuf     = cost_SUF
011200080213     C                   EVAL      prmtlg     = cost_TLE
011300080213     C                   EVAL      prmfile    = 'S'
011400080213     C                   EVAL      prmvgd     = 'S'
011500080213     C                   EVAL      prmclivas  = cost_CLIVAS
011600080213     C*
011700080213     C                   MOVEL     PARAM         KPJBU
011800080213     C*
011900080213     C                   ENDSR
012000080213
012100080213
012200080213
012300060419
012400080213     C     calDATE       BEGSR
012500080213     C*
012600080213     C* Dalla data corrente ottengo il primo del mese corrente
012700080213     C                   movel(P)  wAAAAMMcor    wMeseCor01
012800080213     C                   move      '01'          wMeseCor01
012900080213     C*
013000080213     C* Dal primo del mese corrente ottengo l'ultimo del mese precedente
013100080213     C     *ISO          move      wMeseCor01    wDataISO
013200080213     C     wDataISO      subdur    1:*D          wDataISO
013300080213     C     *ISO          move      wDataISO      wMesePre31
013400080214     C* Inverto la data finale elaborazione
013500080214     C                   z-add     wMesePre31    dataIn
013600080214     C                   exsr      invDATE
013700080214     C                   z-add     dataOut       wMesePre31Inv
013800080213     C*
013900080213     C* Dal ultimo del mese precedente ottengo il primo del mese precedente
014000080213     C                   movel(P)  wMesePre31    wMesePre01
014100080213     C                   move      '01'          wMesePre01
014200080214     C* Inverto la data iniziale elaborazione
014300080214     C                   z-add     wMesePre01    dataIn
014400080214     C                   exsr      invDATE
014500080214     C                   z-add     dataOut       wMesePre01Inv
014600060419     C*
014700060419     C                   ENDSR
014800080214
014900080214
015000080214
015100080214
015200080214     C     invDATE       BEGSR
015300080214     C*
015400080214     C                   z-add     dataIn        g08inv
015500080214     C                   z-add     *zeros        g08dat
015600080214     C                   movel     '3'           g08err
015700080214     C                   call      'XSRDA8'
015800080214     C                   parm                    wlbda8
015900080214     C                   z-add     g08dat        dataOut                        *DATA CORRENTE GGMMA
016000080214     C*
016100080214     C                   ENDSR
016200060418
016300060419
016400080213
016500080213
016600060418
016700991027      /TITLE Operazioni iniziali.
016800991027     C     *inzsr        BEGSR
016900991027     C*
017000080213     C     *ENTRY        plist
017100080213     C                   parm                    KPJBA
017200060419     C*
017300060419     C* CALCOLA LA DATA CORRENTE
017400060419     C                   time                    wn14             14 0
017500060419     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
017600080213     C                   movel     wn14          oracor            6 0          *OAR (6) IN HH:MM:SS
017700060419     C                   z-add     wn8           g08dat
017800060419     C                   z-add     *zeros        g08inv
017900060419     C                   movel     '0'           g08err
018000060419     C                   call      'XSRDA8'
018100060419     C                   parm                    wlbda8
018200060419     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
018300080213     C                   movel     datcor        wAAAAMMcor        6 0
018400991027     C*
018500991027     C                   ENDSR
