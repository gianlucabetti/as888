000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES)
000200000000     H*PARMS CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      ********************************************************************
000500000000      *  BAK851R -  PULIZIA AUTOMATICA FILE KPLOG00F                     *
000600000000      ********************************************************************
000700000000      *
000800000000     FKPLOG03L  UF   E           K DISK
000900000000     FKPLOS00F  O    E             DISK
001000000000      *--- DS Area Dati di Controllo Modulo Base PRO
001100000000     D KPDT1         E DS           512
001200000000X0124X******--- DS calcolo Data Riferimento
001300000000X0124X***** XANA04        E DS
001400000000      *--- DS inversione Data
001500000000     D                 DS
001600000000     D  DATA                   1      6  0
001700000000X0124X*****  GG                     1      2  0
001800000000  "  X*****  AA                     5      6  0
001900000000  "  D   AA                    1      2  0
002000000000  "  D   GG                    5      6  0
002100000000  "   *---
002200000000X0124D WDataISO        S               D
002300000000X0211D TipLan          S              1A
002400000000X0211D WTipLan         S              1A
002500000000X0124 *---
002600000000      *
002700000000      *----------------------------------------------------*
002800000000      *   FLUSSO PRINCIPALE                                *
002900000000      *----------------------------------------------------*
003000000000X0211 *
003100000000  "  C     *Entry        PList
003200000000X0211C                   Parm                    TipLan
003300000000      *
003400000000     C                   EXSR      DEFCAM
003500000000      *
003600000000     C     KP1CNL        IFNE      '3'
003700000000X0211C     WTipLan       OREQ      '1'
003800000000      * Calcolo Data riferimento
003900000000     C                   EXSR      CALDAT
004000000000     C     *LOVAL        SETLL     KPLOG03L
004100000000     C                   READ      KPLOG03L                               21
004200000000     C     *IN21         DOWEQ     '0'
004300000000     C     LOGDAC        ANDLE     DATRI7
004400000000     C     KP1CNL        IFEQ      '2'
004500000000      * Copia record su KPLOS00F
004600000000     C                   EXSR      CPYREC
004700000000     C                   ENDIF
004800000000      * Cancella record su KPLOG00F
004900000000     C                   DELETE    KPLOG000
005000000000     C                   READ      KPLOG03L                               21
005100000000     C                   ENDDO
005200000000     C                   ENDIF
005300000000      * Fine Elaborazione
005400000000     C                   SETON                                        LR
005500000000      *
005600000000      *----------------------------------------------------*
005700000000      * CALDAT - CALCOLA DATA FINO ALLA QUALE CANCELLARE   *
005800000000      *----------------------------------------------------*
005900000000      *
006000000000     C     CALDAT        BEGSR
006100000000X0124X*****              Z-ADD     UDATE         DATRIF            6 0
006200000000  "  X*****              Z-ADD     UDATE         DATA
006300000000  "  X*****              MOVE      GG            DATRIF
006400000000  "  X*****              MOVEL     AA            DATRIF
006500000000  "  X*****              Z-ADD     DATRIF        A04DT1
006600000000  "  X****** SE ANNO < 40 SIAMO NEL 2000 !!!
006700000000  "  X*****AA            IFLT      40
006800000000  "  X*****              MOVEL     1             A04DT1
006900000000  "  X*****              ENDIF
007000000000  "  X*****              Z-ADD     0             A04DT2
007100000000  "  X*****              Z-ADD     0             A04DIF
007200000000  "  X*****              SUB       KP1NGG        A04DIF
007300000000  "  X*****              CALL      'XANA04R'
007400000000  "  X*****              PARM                    XANA04
007500000000  "  X*****              Z-ADD     A04DT2        DATRI7            7 0
007600000000  "  C     *DMY          Move      UDate         WDataISO
007700000000  "  C                   SubDur    KP1NGG:*D     WDataISO
007800000000  "  C     *YMD          Move      WDataISO      DATA
007900000000  "  C                   Z-Add     DATA          DATRI7            7 0
008000000000  "  C* SE ANNO < 40 SIAMO NEL 2000 !!!
008100000000  "  C     AA            IFLT      40
008200000000  "  C                   MOVEL     1             DATRI7
008300000000X0124C                   ENDIF
008400000000     C                   ENDSR
008500000000      *
008600000000      *----------------------------------------------------*
008700000000      * CPYREC - COPIA RECORD KPLOG SU KPLOS               *
008800000000      *----------------------------------------------------*
008900000000      *
009000000000     C     CPYREC        BEGSR
009100000000      *
009200000000     C                   MOVEL     LOGATT        LOSATT
009300000000     C                   Z-ADD     LOGNRA        LOSNRA
009400000000     C                   MOVEL     LOGNME        LOSNME
009500000000     C                   MOVEL     LOGCBI        LOSCBI
009600000000     C                   MOVEL     LOGCBE        LOSCBE
009700000000     C                   Z-ADD     LOGDAC        LOSDAC
009800000000     C                   Z-ADD     LOGORC        LOSORC
009900000000     C                   Z-ADD     LOGDAI        LOSDAI
010000000000     C                   Z-ADD     LOGORI        LOSORI
010100000000     C                   Z-ADD     LOGDAF        LOSDAF
010200000000     C                   Z-ADD     LOGORF        LOSORF
010300000000     C                   MOVEL     LOGDEA        LOSDEA
010400000000     C                   MOVEL     LOGKPJ        LOSKPJ
010500000000     C                   MOVEL     LOGSTP        LOSSTP
010600000000     C                   WRITE     KPLOS000
010700000000      *
010800000000     C                   ENDSR
010900000000      *
011000000000      *----------------------------------------------------*
011100000000      * DEFCAM - DEFINIZIONE CAMPI                         *
011200000000      *----------------------------------------------------*
011300000000      *
011400000000     C     DEFCAM        BEGSR
011500000000      *
011600000000     C     *DTAARA       DEFINE                  KPDT1
011700000000     C                   IN        KPDT1
011800000000X0211 *
011900000000  "  C                   If        %Parms > 0
012000000000  "  C                   Eval      WTipLan = TipLan
012100000000  "  C                   Else
012200000000  "  C                   Eval      WTipLan = '0'
012300000000X0211C                   EndIf
012400000000      *
012500000000     C                   ENDSR
