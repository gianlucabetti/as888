000100960329      ***********************************************************************
000200031113     H dftactgrp(*yes)
000300031113     H*------------------------------------------------------------------------*
000400031113     H* Raggruppamento bolle x stesso destinatario da EDIVAB a FIVAB
000500031113     H*------------------------------------------------------------------------*
000600031113     H DECEDIT('0,') DATEDIT(*DMY.)
000700960329     F*---------------------------------------------------------------------*
000800960329     F*  DATA BASE                                                          *
000900960329     F*---------------------------------------------------------------------*
001000070802     FEDIVAB1L  UF   E           K DISK    COMMIT
001100121102     FEDIVAD1L  UF   E           K DISK    COMMIT
001101171219     FEDIVAD11L UF   E           K DISK    COMMIT
001200121102     FEDIVAT1L  UF   E           K DISK    COMMIT
001300121102     FEDIVAX1L  UF   E           K DISK    COMMIT
001400031113     FEDIVABQT  O    E             DISK    RENAME(EDIVAB00:EDIVABQT00)
001500031113     FEDIVADQT  O    E             DISK    RENAME(EDIVAD00:EDIVADQT00)
001501171219     FEDIVAD10QTO    E             DISK    RENAME(EDIVAD100:EDIVADQT1)
001600031113     FEDIVATQT  O    E             DISK    RENAME(EDIVAT00:EDIVATQT00)
001700070807     FEDIVAXQT  O    E             DISK    RENAME(EDIVAX00:EDIVAXQT00)
001800031113     D*--------------------*
001900031113     D* DS ESTERNE
002000031113     D*--------------------*
002100031113     D TRTC86DS      E DS
002200041116     D CNCR80        E DS
002300031113     D*--------------------*
002400031113     D* VARIABILI RIFERITE AL DATA BASE
002500031113     D*--------------------*
002600041116     D TRUL06DS      E DS
002700041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
002800041116     D KPJBA         E DS
002900041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000960329     C*---------------------------------------------------------------------*
003100031113     C* MAIN
003200960925     C*---------------------------------------------------------------------*
003300031113     C*
003400031113     C* Elaboro il VAB
003500040712     C     KEYva_1       SETLL     EDIVAB1L
003600031113     C                   IF        %equal(EDIVAB1L)
003700040712     C     KEYva_1       READE     EDIVAB1L
003800031113     C                   DOW       not %eof(EDIVAB1L)
003900041116     c* solo se il p.o. gestione è della £1 di simfel
004000041116     c     vabfgs        lookup    lin                                    50
004100041116     c
004200041116     c                   if        *in50  and vabfgs>0
004300031113     C                   WRITE     EDIVABQT00
004400031113     C                   DELETE    EDIVAB1L
004500121102     C*
004600121102     C*************
004700121102     C* Elaboro i file "appendice" unicamente in funzone del VAB
004800121102     C*
004900121102     C* Elaboro i VAD
005000121102     C     KEYva_C       SETLL     EDIVAD1L
005100121102     C                   IF        %equal(EDIVAD1L)
005200121102     C     KEYva_C       READE     EDIVAD1L
005300121102     C                   DOW       not %eof(EDIVAD1L)
005400121102     C                   WRITE     EDIVADQT00
005500121102     C                   DELETE    EDIVAD1L
005600121102     C     KEYva_C       READE     EDIVAD1L
005700121102     C                   ENDDO
005800121102     C                   ENDIF
005801171219     C* Elaboro i VAD10
005802171219     C     KEYva_C       SETLL     EDIVAD11L
005803171219     C                   IF        %equal(EDIVAD11L)
005804171219     C     KEYva_C       READE     EDIVAD11L
005805171219     C                   DOW       not %eof(EDIVAD11L)
005806171219     C                   WRITE     EDIVADQT1
005807171219     C                   DELETE    EDIVAD11L
005808171219     C     KEYva_C       READE     EDIVAD11L
005809171219     C                   ENDDO
005810171219     C                   ENDIF
005900121102     C*
006000121102     C* Elaboro i VAT
006100121102     C     KEYva_C       SETLL     EDIVAT1L
006200121102     C                   IF        %equal(EDIVAT1L)
006300121102     C     KEYva_C       READE     EDIVAT1L
006400121102     C                   DOW       not %eof(EDIVAT1L)
006500121102     C                   WRITE     EDIVATQT00
006600121102     C                   DELETE    EDIVAT1L
006700121102     C     KEYva_C       READE     EDIVAT1L
006800121102     C                   ENDDO
006900121102     C                   ENDIF
007000121102     C*
007100121102     C* Elaboro i VAX
007200121102     C     KEYva_C       SETLL     EDIVAX1L
007300121102     C                   IF        %equal(EDIVAX1L)
007400121102     C     KEYva_C       READE     EDIVAX1L
007500121102     C                   DOW       not %eof(EDIVAX1L)
007600121102     C                   WRITE     EDIVAXQT00
007700121102     C                   DELETE    EDIVAX1L
007800121102     C     KEYva_C       READE     EDIVAX1L
007900121102     C                   ENDDO
008000121102     C                   ENDIF
008100121102     C*
008200121102     C*************
008300121102     C*
008400041116     c                   endif
008500041116     c
008600040712     C     KEYva_1       READE     EDIVAB1L
008700031113     C                   ENDDO
008800031113     C                   ENDIF
008900031113     C*
009000960925     C                   SETON                                        LR
009100960925     C*---------------------------------------------------------------------*
009200960925     C* *INZSR: Operazioni iniziali
009300041116     ù*---------------------------------------------------------------------*
009400960925     C     *INZSR        BEGSR
009500960925     C*
009600020919     C*  Reperisco parametri in entrata
009700020919     C     *ENTRY        PLIST
009800031113     C                   PARM                    TRTC86DS
009900031113     C*---
010000031113     C* CHIAVI DI LETTURA
010100031113     C*---
010200121102     C*
010300121102     C* CHIAVE DI LETTURA X EDIVAB1L/EDIVAD1L/EDIVAT1L/EDIVAX1L
010400040712     C     KEYva_1       KLIST
010500031113     C                   KFLD                    D86CCM
010600031113     C                   KFLD                    D86CMR
010700031113     C                   KFLD                    D86CNT
010800121102     C*
010900121102     C* CHIAVE DI LETTURA X EDIVAB1L/EDIVAD1L/EDIVAT1L/EDIVAX1L
011000121102     C     KEYva_C       KLIST
011100121102     C                   KFLD                    VABFGS
011200121102     C                   KFLD                    D86CCM
011300121102     C                   KFLD                    D86CMR
011400121102     C                   KFLD                    D86CNT
011500121102     C                   KFLD                    VABAAS
011600121102     C                   KFLD                    VABLNP
011700121102     C                   KFLD                    VABNRS
011800121102     C                   KFLD                    VABNSP
011900121102     C*
012000041116     C*  Richiamo XPARUT
012100041116     C                   Z-ADD     1             CODUT             1 0
012200041116     C                   CALL      'X§PARUT'
012300041116     C                   PARM                    UT§DSE
012400041116     C                   MOVEL     REC80         CNCR80
012500041116     c
012600041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
012700041116     C                   CLEAR                   TRUL06DS
012800041116     C                   MOVE      '£1'          D06COD
012900041116     C                   MOVEL     SIMFEL        D06KEY
013000041116     C                   MOVEL     'L'           D06TLA
013100041116     C                   MOVEL     TRUL06DS      KPJBU
013200041116      *
013300041116     C                   CALL      'TRUL06R'
013400041116     C                   PARM                    KPJBA
013500041116     C                   MOVEL     KPJBU         TRUL06DS
013600960925     C*
013700960925     C                   ENDSR
