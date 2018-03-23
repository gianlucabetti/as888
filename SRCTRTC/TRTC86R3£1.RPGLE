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
001100070802     FEDIVAD2L  UF   E           K DISK    COMMIT
001200070802     FEDIVAT2L  UF   E           K DISK    COMMIT
001201070807     FEDIVAX2L  UF   E           K DISK    COMMIT
001300031113     FEDIVABQT  O    E             DISK    RENAME(EDIVAB00:EDIVABQT00)
001400031113     FEDIVADQT  O    E             DISK    RENAME(EDIVAD00:EDIVADQT00)
001500031113     FEDIVATQT  O    E             DISK    RENAME(EDIVAT00:EDIVATQT00)
001501070807     FEDIVAXQT  O    E             DISK    RENAME(EDIVAX00:EDIVAXQT00)
001600031113     D*--------------------*
001700031113     D* DS ESTERNE
001800031113     D*--------------------*
001900031113     D TRTC86DS      E DS
002000041116     D CNCR80        E DS
002100031113     D*--------------------*
002200031113     D* VARIABILI RIFERITE AL DATA BASE
002300031113     D*--------------------*
002400031113     D kvatTRC         S                   LIKE(VATTRC) INZ('E')
002500041116     D TRUL06DS      E DS
002600041116     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
002700041116     D KPJBA         E DS
002800041116     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900960329     C*---------------------------------------------------------------------*
003000031113     C* MAIN
003100960925     C*---------------------------------------------------------------------*
003200031113     C*
003300031113     C* Elaboro il VAB
003400040712     C     KEYva_1       SETLL     EDIVAB1L
003500031113     C                   IF        %equal(EDIVAB1L)
003600040712     C     KEYva_1       READE     EDIVAB1L
003700031113     C                   DOW       not %eof(EDIVAB1L)
003800041116     c* solo se il p.o. gestione è della £1 di simfel
003900041116     c     vabfgs        lookup    lin                                    50
004000041116     c
004100041116     c                   if        *in50  and vabfgs>0
004200031113     C                   WRITE     EDIVABQT00
004300031113     C                   DELETE    EDIVAB1L
004400041116     c                   endif
004500041116     c
004600040712     C     KEYva_1       READE     EDIVAB1L
004700031113     C                   ENDDO
004800031113     C                   ENDIF
004900040712     C*
005000040712     C* Elaboro i VAD
005100040712     C     KEYva_1       SETLL     EDIVAD2L
005200040712     C                   IF        %equal(EDIVAD2L)
005300040712     C     KEYva_1       READE     EDIVAD2L
005400040712     C                   DOW       not %eof(EDIVAD2L)
005500041116     c* solo se il p.o. gestione è della £1 di simfel
005600041116     c     vadfgs        lookup    lin                                    50
005700041116     c
005800041116     c                   if        *in50  and vadfgs>0
005900040712     C                   WRITE     EDIVADQT00
006000040712     C                   DELETE    EDIVAD2L
006100041116     c                   endif
006200041116     c
006300040712     C     KEYva_1       READE     EDIVAD2L
006400040712     C                   ENDDO
006500040712     C                   ENDIF
006600040712     C*
006700040712     C* Elaboro i VAT
006800040712     C     KEYva_1       SETLL     EDIVAT2L
006900040712     C                   IF        %equal(EDIVAT2L)
007000040712     C     KEYva_1       READE     EDIVAT2L
007100040712     C                   DOW       not %eof(EDIVAT2L)
007200041116     c* solo se il p.o. gestione è della £1 di simfel
007300041116     c     vatfgs        lookup    lin                                    50
007400041116     c
007500041116     c                   if        *in50    and vatfgs>0
007600040712     C                   WRITE     EDIVATQT00
007700040712     C                   DELETE    EDIVAT2L
007800041116     c                   endif
007900041116     c
008000040712     C     KEYva_1       READE     EDIVAT2L
008100040712     C                   ENDDO
008200040712     C                   ENDIF
008201070807     C*
008202070807     C* Elaboro i VAX
008203070807     C     KEYva_1       SETLL     EDIVAX2L
008204070807     C                   IF        %equal(EDIVAX2L)
008205070807     C     KEYva_1       READE     EDIVAX2L
008206070807     C                   DOW       not %eof(EDIVAX2L)
008207070807     c* solo se il p.o. gestione è della £1 di simfel
008208070807     c     vaxfgs        lookup    lin                                    50
008209070807     c
008210070807     c                   if        *in50    and vaxfgs>0
008211070807     C                   WRITE     EDIVAXQT00
008212070807     C                   DELETE    EDIVAX2L
008213070807     c                   endif
008214070807     c
008215070807     C     KEYva_1       READE     EDIVAX2L
008216070807     C                   ENDDO
008217070807     C                   ENDIF
008300031113     C*
008400960925     C                   SETON                                        LR
008500960925     C*---------------------------------------------------------------------*
008600960925     C* *INZSR: Operazioni iniziali
008700041116     ù*---------------------------------------------------------------------*
008800960925     C     *INZSR        BEGSR
008900960925     C*
009000020919     C*  Reperisco parametri in entrata
009100020919     C     *ENTRY        PLIST
009200031113     C                   PARM                    TRTC86DS
009300031113     C*---
009400031113     C* CHIAVI DI LETTURA
009500031113     C*---
009600070807     C* CHIAVE DI LETTURA X EDIVAB1L/EDIVAD2L/EDIVAT2L/EDIVAX2L
009700040712     C     KEYva_1       KLIST
009800031113     C                   KFLD                    D86CCM
009900031113     C                   KFLD                    D86CMR
010000031113     C                   KFLD                    D86CNT
010100041116     C*  Richiamo XPARUT
010200041116     C                   Z-ADD     1             CODUT             1 0
010300041116     C                   CALL      'X§PARUT'
010400041116     C                   PARM                    UT§DSE
010500041116     C                   MOVEL     REC80         CNCR80
010600041116     c
010700041116      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
010800041116     C                   CLEAR                   TRUL06DS
010900041116     C                   MOVE      '£1'          D06COD
011000041116     C                   MOVEL     SIMFEL        D06KEY
011100041116     C                   MOVEL     'L'           D06TLA
011200041116     C                   MOVEL     TRUL06DS      KPJBU
011300041116      *
011400041116     C                   CALL      'TRUL06R'
011500041116     C                   PARM                    KPJBA
011600041116     C                   MOVEL     KPJBU         TRUL06DS
011700960925     C*
011800960925     C                   ENDSR
