000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     D*-------------------
000600000000     D* ARCHITETTURA
000700000000     D*-------------------
000800000000     D KPJBA         E DS
000900080325     D*------------------
001000080325     D* DS "XSRDA8" - CONTROLLA DATA (8)
001100080325     D*------------------
001200080325     D WLBDA8          DS                  INZ
001300080325     D  G08DAT                 1      8  0
001400080325     D  G08INV                 9     16  0
001500080325     D  G08ERR                17     17
001600080325     D  G08TGI                18     22  0
001700060426     D*-------------------
001800060608     D* DS PARAMETRI DI LANCIO PGM
001900060608     D*-------------------
002000060608     D PARAM           DS                  INZ
002100060608     D  PARVER                        5  0
002200060608     D  PARICCC                       1
002300060608     D  PARICAL                       1
002400060608     D  PARICDY                       1
002500060608     D  PARICDP                       1
002600060608     D  PARICSC                       1
002700060608     D  PARICRO                       1
002800060608     D  PARICLO                       1
002900060608     D  PARICCU                       1
003000080424     D  PARVERDDE                     8  0
003100080424     D  PARSTRBOL                     8  0
003200000000     C*------------------------------------------------------------------------*
003300000000     C* MAIN LINE
003400000000     C*------------------------------------------------------------------------*
003500060427     C*
003600060427     C* Elaborazone archivio "Country"
003700060608     C                   IF        PARICCC = 'X'
003800060608     C                   CALL      'TISIE1R1'
003900060608     C                   PARM                    KPJBA
004000060608     C                   ENDIF
004100060427     C*
004200060427     C* Elaborazone archivio "Customs"
004300060608     C                   IF        PARICCU = 'X'
004400060608     C                   CALL      'TISIE1R4'
004500060608     C                   PARM                    KPJBA
004600060608     C                   ENDIF
004700060427     C*
004800060427     C* Elaborazone archivio "Service"
004900060608     C                   IF        PARICSC = 'X'
005000060608     C                   CALL      'TISIE1R5'
005100060608     C                   PARM                    KPJBA
005200060608     C                   ENDIF
005300060427     C*
005400060427     C* Elaborazone archivio "Depots"
005500060608     C                   IF        PARICDP = 'X'
005600060608     C                   CALL      'TISIE1R6'
005700060608     C                   PARM                    KPJBA
005800060608     C                   ENDIF
005900081008     C*****************
006000081008     C*
006100081008     C* Elaborazone archivio "Allow"
006200081008     C                   IF        PARICAL = 'X'
006300081008     C                   CALL      'TISIE1R2'
006400081008     C                   PARM                    KPJBA
006500081008     C                   ENDIF
006600081008     C*
006700081008     C* Elaborazone archivio "Deny"
006800081008     C                   IF        PARICDY = 'X'
006900081008     C                   CALL      'TISIE1R3'
007000081008     C                   PARM                    KPJBA
007100081008     C                   ENDIF
007200060427     C*
007300060427     C* Elaborazone archivio "Routes"
007400060608     C                   IF        PARICRO = 'X'
007500060608     C                   CALL      'TISIE1R7'
007600060608     C                   PARM                    KPJBA
007700060608     C                   ENDIF
007800060515     C*
007900060515     C* Elaborazone archivio "Location"
008000060608     C                   IF        PARICLO = 'X'
008100060608     C                   CALL      'TISIE1R8'
008200060608     C                   PARM                    KPJBA
008300060608     C                   ENDIF
008400080325     C*
008500151103     C*  Pulizia dei depot dei legati
008600151103     C                   CALL      'TISIE1R9C'
008700151103     C                   PARM                    KPJBA
008800151103     C*
008900080325     C* Infine lancio simulazione ri-calcolo instradamento DPD GeoPost
009000080325     C                   MOVEL(P)  PARVERDDE     In_DataRifAlf     8
009100080424     C                   MOVEL(P)  PARSTRBOL     In_StrBolleAlf    8
009200080325     C*
009300081009 xxx C***                CALL      'TISIE0R1'
009400081009     C***                PARM                    In_DataRifAlf
009500081009     C***                PARM                    In_StrBolleAlf
009600000000     C*
009700000000     C                   SETON                                        LR
009800000000     C*------------------------------------------------------------------------*
009900980403     C* *INZSR - OPERAZIONI INIZIALI
010000000000     C*------------------------------------------------------------------------*
010100980403     C     *INZSR        BEGSR
010200000000     C*
010300000000     C* RICEVIMENTO PARAMETRI
010400000000     C     *ENTRY        PLIST
010500000000     C                   PARM                    KPJBA
010600060608     C                   MOVEL(P)  KPJBU         PARAM
010700080325     C*
010800080325     C* CALCOLA LA DATA CORRENTE
010900080325     C                   time                    wn14             14 0
011000080325     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
011100080325     C                   movel     wn14          oracor            6 0          *ORA
011200080325     C                   z-add     wn8           g08dat
011300080325     C                   z-add     *zeros        g08inv
011400080325     C                   movel     '0'           g08err
011500080325     C                   call      'XSRDA8'
011600080325     C                   parm                    wlbda8
011700080325     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
011800000000     C*
011900000000     C                   ENDSR
