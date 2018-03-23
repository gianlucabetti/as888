000100970110     H*------------------------------------------------------------------------*
000200970110     H* TARIFFE DA CONTO ECONOMICO - STAMPA DATI TIPO - FILTRO                 *
000300970110     H*------------------------------------------------------------------------*
000400970110     H DATEDIT(*YMD)
000500970110     F*------------------------------------------------------------------------*
000600970110     F* DATA BASE
000700970110     F*------------------------------------------------------------------------*
000800040804     FTeetc01l  If   E           k DISK
000900970110     D*-------------------
001000970110     D* DS DI PROCEDURA
001100970110     D*-------------------
001200040804     D tnte00ds      E DS
001300970110     D*-------------------
001400970110     D* ARCHITETTURA
001500970110     D*-------------------
001600970110     D KPJBA         E DS
001700040804     D*------------------
001800040804     D* DS PARAMETRI DI LANCIO
001900040804     D*------------------
002000040804     D PARAM           DS
002100040804     D  PARKSC                 1      7  0
002200040804     D  PARFPA                 8     10  0
002300040804     D  PARFAR                11     13  0
002400040804     D  PARFAP                14     16  0
002500040804     D  PARDS1                17     24  0
002600040804     D  PARAA1                17     20  0
002700040804     D  PARMG1                21     24  0
002800040804     D  PARDS2                25     32  0
002900040804     D  PARTAR                33     35
003000040804     D  PARCTO                36     36
003100040804     D  PARKOV                37     43  0
003200040804     D  PARCAR                37     41  0
003300040804     D  PARCTR                44     46  0
003400040804     D  PARTTR                44     44
003500040804     D  PARPRG                47     49  0
003600040804     D  PARLNP                50     52  0
003700040805     D  PARFIE                53     53
003800040804     D  PARSIM                54     54
003900040804     D  PARELA                55     55
004000040804     D  PARDIV                56     58
004100040804     D  PARUNI                59     59
004200040805     D  PARsta                60     60
004300040804     D  PARint                61     61
004400040805     D  PARsie                62     62
004401091130     D  PARdsf                63     63
004500040805     d*
004600040805     D SFIE            S              1    DIM(2) inz                           *ITALIA/ESTERO
004700970110     C*------------------------------------------------------------------------*
004800970110     C* MAIN LINE
004900970110     C*------------------------------------------------------------------------*
005000040804     C*
005100040804     c                   if        parlnp <> 999
005200040804     c     ketclnp       setll     teetc01l
005300040804     c                   else
005400040804     c     ketc          setll     teetc01l
005500040804     c                   end
005600040804     c                   do        *hival
005700040804     c                   if        parlnp <> 999
005800040804     c     ketclnp       reade     teetc01l
005900040804     c                   else
006000040804     c     ketc          reade     teetc01l
006100040804     c                   end
006200040804     c*
006300040804     c                   if        %eof(teetc01l)
006400040804     c                   leave
006500040804     c                   end
006600970110     C*
006700040804     C                   MOVEL     etcCTO        D00CTO                         *TARIFFA O OFFERTA
006800040804     C                   Z-ADD     etcKSC        D00KSC                         *CLIENTE O VISITA
006900040804     C                   Z-ADD     etcCTR        D00CTR                         *TARIFFA
007000040804     C                   Z-ADD     etcPRG        D00PRG                         *PROGRESSIVO
007100040804     C                   Z-ADD     etcLNP        D00LNP                         *LINEA DI PARTENZA
007300040804     C                   MOVEL     'F06'         D00OP1                         *LIVELLO PROGRAMMA
007400040804     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
007500040804     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
007600040804     C                   MOVEL     '0'           D00ERR                         *ERRORE
007700040804     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
007800040804     C*                                          D00TSP                         *TIPO SERVIZIO
007900040805     C                   MOVEL     parsie        D00FIE                         *FLAG ITALIA/ESTERO
007901091130     C                   MOVEL     pardsf        D00dsf                         *FLAG ITALIA/ESTERO
008000040804     C                   MOVEL     'T'           D00SCT                         *REPPRESENTAZIONE CT
008100040804     C                   MOVEL     'EUR'         D00DIV                         *DIVISA TARIFFE/OFFE
008200040805     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
008300040805DO  4C                   DO        2             K                 1 0
008400040805IF  5C     SFIE(K)       IFNE      *BLANKS
008500040805     C                   MOVEL     SFIE(K)       d00sie
008600040804     C                   MOVEL(p)  tnte00ds      KPJBU
008700040804     C                   CALL      'TNTE43R'
008800970110     C                   PARM                    KPJBA
008900040804     C                   PARM                    PARAM
009000040804     c* stampo i parametri di lancio solo la 1° volta
009100040804     C                   MOVEL     'N'           parint                         *TARIFFA ITA/EST
009200040805     c                   end
009300040804     c                   enddo
009400040805     c                   enddo
009500970421     C*
009600040804     C                   seton                                        lr
009700970110     C*------------------------------------------------------------------------*
009800970110     C* *INZSR - OPERAZIONI INIZIALI
009900970110     C*------------------------------------------------------------------------*
010000970110     C     *INZSR        BEGSR
010100970110     C*--------------------
010200970110     C* RICEVIMENTO PARAMETRI
010300970110     C*--------------------
010400970110     C     *ENTRY        PLIST
010500970110     C                   PARM                    KPJBA
010600040804     c                   movel     kpjbu         param
010700040805     c* se la scelta tariffa è sia italia che estero imposto sia la I che la
010800040805     c* E altrimenti una delle due
010900040805     c                   if        parfie = *blanks
011000040805     c                   movel     'I'           sfie(1)
011100040805     c                   movel     'E'           sfie(2)
011200040805     c                   else
011300040805     c                   movel     parfie        sfie(1)
011400040805     c                   movel     ' '           sfie(2)
011500040805     c                   end
011600040804     C* LETTURA TEETC01L - COMPLETA
011700040804     C     ketclnp       KLIST
011800040804     C                   KFLD                    parela                         *DATI SEDE O FILIALE
011900040804     C                   KFLD                    PARCTO                         *TARIFFA O OFFERTA
012000040804     C                   KFLD                    PARKOV                         *CLIENTE O VISITA
012100040804     C                   KFLD                    PARCTR                         *TARIFFA
012200040804     C                   KFLD                    PARPRG                         *PROGRESSIVO
012300040804     C                   KFLD                    parLNP                         *LINEA PARTENZA
012400040804     C     ketc          KLIST
012500040804     C                   KFLD                    parela                         *DATI SEDE O FILIALE
012600040804     C                   KFLD                    PARCTO                         *TARIFFA O OFFERTA
012700040804     C                   KFLD                    PARKOV                         *CLIENTE O VISITA
012800040804     C                   KFLD                    PARCTR                         *TARIFFA
012900040804     C                   KFLD                    PARPRG                         *PROGRESSIVO
013000970110     C*
013100970110     C                   ENDSR
