000100020212     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200020212     H*PARMS CVTOPT(*NONE)
000300020212     H DECEDIT('0,') DATEDIT(*DMY.)
000400020212      *         ----------------------------------------------        *
000500020212      *          Toglie società a valere dai movimenti di conti       *
000600020212      *          senza trattamento intercompany                       *
000700020212      *         ----------------------------------------------        *
000800020212      *---------------------------------------------------------------*
000900020212      *                                                               *
001000020212     FANRCO01L  IF   E           K DISK
001100020212      *
001200020212     FNDMOV01L  UF   E           K DISK
001300020212     F                                     RENAME(NDMOV000:NDMOV01)
001400020212      *
001500020212     FNDMOV05L  IF   E           K DISK
001600020212     F                                     RENAME(NDMOV000:NDMOV05)
001700020212      *
001800020212      *                                                               *
001900020212      *--------
002000020212      *  PARAMETRI ESTERNI
002100020212      *--------
002200020212     D KPJBA         E DS
002300020212      *--------
002400020212     D NDc447DS      E DS
002500020212     D  Err447       E                     INZ('0')
002600020212     D*
002700020212      *---------------------------------------------------------------*
002800020212      *  FLUSSO PRINCIPALE                                            *
002900020212      *---------------------------------------------------------------*
003000020212      *
003100020212      * Esecuzione
003200020212     C                   EXSR      ESEGUI
003300020212      *
003400020212      *--------
003500020212     C                   EXSR      ENDPGM
003600020212      *
003700020212      /EJECT
003800020212     C************************************************************
003900020212     C* FINE PROGRAMMA
004000020212     C************************************************************
004100020212     C     ENDPGM        BEGSR
004200020212      *
004300020212     C                   SETON                                        Lr
004400020212     C                   RETURN
004500020212      *
004600020212     C                   ENDSR
004700020212     C/EJECT
004800020212      ******************************************************
004900020212      * ESECUZIONE
005000020212      ******************************************************
005100020212     C     ESEGUI        BEGSR
005200020212     C*
005300090504>>>>>C                   EVAL      *IN30 = *OFF
005400020212     C     SOCIETA447    SETLL     ANRCO01L
005500020212     C*
005600020212     C                   DOW       *IN30 = *OFF
005700020212     C     SOCIETA447    READE     ANRCO01L                               30
005800020212     C                   IF        *IN30 = *ON
005900020212     C                   LEAVE
006000020212     C                   ENDIF
006100020212     C*
006200020212     C                   IF        RCOKSC <> *BLANK AND RCOTPCNTGR = *BLANK
006300020212     C                   EXSR      CLEARMOV
006400020212     C                   ENDIF
006500020212     C*
006600020212     C                   ENDDO
006700020212     C*
006800020212     C                   ENDSR
006900020212      ******************************************************
007000020212      * PULIZIA CAMPO MOVSOCVAL
007100020212      ******************************************************
007200020212     C     CLEARMOV      BEGSR
007300020212     C*
007400050526C1825 * dopo aver pulito tutti i mov del 1° conto valido, *in31=*on;
007500050526  "   * se non si rinizializza *in31=*off, non si esegue la 'pulizia'
007600050526  "   * dal 2° conto valido in poi.
007700050526C1825C                   eval      *in31 = *off
007800020212     C     K05MOV05      SETLL     NDMOV05L
007900020212     C*
008000020212     C                   DOW       *IN31 = *OFF
008100020212     C     K04MOV05      READE     NDMOV05L                               31
008200020212     C*
008300020212     C* SE HO RAGGIUNTO LA FINE DEL FILE
008400020212     C*
008500020212     C                   IF        *IN31 = *ON
008600020212     C                   LEAVE
008700020212     C                   ENDIF
008800020212     C*
008900020212     C* SE DATA REGISTRAZIONE È COMPRESA FRA QUELLE PASSATE IN DS
009000020212     C*
009100020212     C                   IF        MOVDTREG <= DTREGA447
009200090504>>>>>C                   IF        MovSocVal <> *BLANK
009300020212     C     K03MOV01      CHAIN     NDMOV01L                           21
009400020212     C                   CLEAR                   MOVSOCVAL
009500020212     C                   UPDATE    NDMOV01
009600090504>>>>>C                   ENDIF
009700020212     C*
009800020212     C                   ELSE
009900020212     C                   LEAVE
010000020212     C                   ENDIF
010100020212     C*
010200020212     C                   ENDDO
010300020212     C*
010400020212     C                   ENDSR
010500020212      ******************************************************
010600020212      *
010700020212     C     K05MOV05      KLIST
010800020212     C                   KFLD                    SOCIETA447
010900020212     C                   KFLD                    CTB447
011000020212     C                   KFLD                    RCOKCC
011100020212     C                   KFLD                    RCOKSC
011200020212     C                   KFLD                    DTREGDA447
011300020212      *
011400020212     C     K04MOV05      KLIST
011500020212     C                   KFLD                    SOCIETA447
011600020212     C                   KFLD                    CTB447
011700020212     C                   KFLD                    RCOKCC
011800020212     C                   KFLD                    RCOKSC
011900020212      *
012000020212     C     K03MOV01      KLIST
012100020212     C                   KFLD                    MOVSYS
012200020212     C                   KFLD                    MOVNRASREG
012300020212     C                   KFLD                    MOVNRRIGAM
012400020212      *
012500020212      ******************************************************
012600020212      * OPERAZIONI INIZIALI
012700020212      ******************************************************
012800020212     C     *INZSR        BEGSR
012900020212     C*
013000020212     C     *ENTRY        PLIST
013100020212     C                   PARM                    KPJBA
013200020212      *
013300020212     C                   MOVEL     KPJBU         Ndc447DS
013400020212     c*
013500020212     c                   ENDSR
