000100990901     H*PARMS OPTION(*NOXREF)
000200990901     H*PARMS DFTACTGRP(*NO)
000300000218     H*PARMS ACTGRP(QILE)
000400990901     H*PARMS CVTOPT(*NONE)
000500000218     H*PARMS BNDDIR(PJXBND)
000600991021     H DECEDIT('0,') DATEDIT(*DMY.)
000700990323     F*
000800991108     F*            Aggiornamento OAFAT00F
000900990902     F*---------------------------------------------------------------------
001000990901     F* PARAMETRI
001100990901     F*
001200990901     F*  Ricevuti:
001300990902     F*
001400990901     F*  Ritorna:
001500990323     F*
001600990323     F*----------------------------------------------------*
001700990323     F* immettere qui i files aperti in I per decodifiche/controlli
001800990323     F* (ricordatevi che è più performante avere prima i files in INPUT
001900990323     F*  e poi quelli in UPDATE)
002000990323     F*------------
002100991109     FOAMOA08L  IF   E           K DISK
002200991109     FOAFAT01L  UF   E           K DISK
002300990323     F*------------
002400991021     D KPJBA         E DS
002500991021     F*------------
002600990909     D*-------------
002700990909     D* Variabili
002800991109     D ktda            s                   like(moatda)
002900990323     C*----------------------------------------------------*
003000990323     C*                MAIN LINE PROGRAM
003100990323     C*----------------------------------------------------*
003200991109     C*
003300991109     C     *loval        setll     oafat01l
003400991109     C                   read      oafat01l                               21
003500991109     C*
003600991109   b1C     *IN21         doweq     *off
003700991109     C*
003800991109     C                   move      'O'           ktda
003900991109     C     k08moa06      chain     oamoa08l                           22
004000991109   b2C     *in22         ifeq      *on
004100991109     C                   move      'R'           ktda
004200991109     C     k08moa06      chain     oamoa08l                           22
004300991109   e2C                   endif
004400991109   b2C     *in22         ifeq      *off
004500991109     C                   z-add     moacrd        fatcrd
004600991109     C                   z-add     moacmb        fatcmb
004700991109     C                   update    oafat000
004800991109   e2C                   endif
004900991109     C                   read      oafat01l                               21
005000991109     C*
005100991109   e1C                   enddo
005200991109     C*
005300990323     C*
005400990323     C* fine programma
005500990323     C                   EXSR      ENDPGM
005600990323     C/EJECT
005700990323     C************************************************************
005800990323     C* Fine Programma
005900990323     C************************************************************
006000990323     C     ENDPGM        BEGSR
006100990323     C*
006200990323     C* passaggio dati al pgm chiamante
006300991025     C*
006400991021     C                   SETON                                        LR
006500990323     C*
006600990323     C                   ENDSR
006700990323     C/EJECT
006800990323     C************************************************************
006900990323     C* OPERAZIONI INIZIALI
007000990323     C************************************************************
007100990323     C     *INZSR        BEGSR
007200990323     C*
007300990323     C* Reperimento parametri
007400990323     C*
007500990323     C     *ENTRY        PLIST
007600991021     C                   PARM                    KPJBA
007700990323     C*
007800990323     C                   ENDSR
007900990323     C/EJECT
008000990323     C************************************************************
008100990323     C* DEFINIZIONE KLIST
008200990323     C************************************************************
008300990323     C     DefKlist      BEGSR
008400990323     C*
008500990323     C* klist
008600990909     C*
008700991109     C     k08moa06      KLIST
008800991109     C                   KFLD                    FATsoc
008900991109     C                   KFLD                    KTDA
009000991109     C                   KFLD                    FATksf
009100991109     C                   KFLD                    FATdat
009200991109     C                   KFLD                    FATsrf
009300991109     C                   KFLD                    FATnum
009400991021     C*
009500991021     C                   ENDSR
009600990323     C/EJECT
