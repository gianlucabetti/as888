000100990422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990422     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* MPH945R0 *-MF/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*        ELIMINA MOVIMENTI GIACENZA INIZ. X RIPARTENZA         *
000600000000     F*        ---------------------------------------------         *
000700000000     F*          -  FILE : MPMMP04L                                  *
000800000000     F*                                                              *
000900000000     F*--------------------------------------------------------------*
001000990422     FtmMMP04L  UF   E           K DISK
001100000000     D KPJBA         E DS
001200990422     D  MSGERR               453    502
001300000000     D*-----PASSAGGIO PARAMETRI
001400990422     D ds945           DS
001500990422     D  vaa945                        4  0
001600990422     D  MAG945                        7
001700990422     D  Dtd945                        8  0
001800990422     D  Nrd945                        7  0
001900990422     D  naa945                        4  0
002000990422     D  Dtm945                        8  0
002100990422     D  Nrm945                        7  0
002200990422     D  LIS945                        3
002300990422     D  ind945                        1  0
002400990422     d  soc945                        3
002500000000     C/EJECT
002600000000     C*----------------------------------------------------*
002700000000     C     *ENTRY        PLIST                                                  *
002800000000     C                   PARM                    KPJBA                          *
002900000103     c                   movel     kpjbu         ds945
003000000000     C*----------------------------------------------------*
003100000000     C*--- CHIAVE X POSIZINAMENTO INIZIALE SU MOVIMENTI
003200000000     C     KEYMOV        KLIST
003300000103     C                   KFLD                    NAA945k
003400000103     C                   KFLD                    DTM945k
003500000103     C                   z-add     NAA945        naa945k           4 0
003600000103     C                   z-add     DTM945        dtm945k           8 0
003700000000     C*----------------------------------------------------*
003800990422     C     KEYMOV        SETLL     tmMMP04L                           90
003900000000     C   90              GOTO      FINE
004000000000     C                   DO        *HIVAL
004100990422     C     KEYMOV        READE     tmMMP04L                               91
004200000000     C   91              GOTO      FINE
004300000000     C     MMPNMV        IFEQ      NRM945
004400000000     C     MMPCAU        IFEQ      'GIA'
004500860221     C     MAG945        IFNE      '9999999'
004600860221     C     MMPMAG        IFEQ      MAG945
004700860125     C                   MOVE      'A'           MMPATT
004800000000     C                   EXCEPT    ANNULL
004900000000     C                   END
005000000000     C                   END
005100860221     C     MAG945        IFEQ      '9999999'
005200860221     C                   MOVE      'A'           MMPATT
005300860221     C                   EXCEPT    ANNULL
005400860221     C                   END
005500860221     C                   END
005600860221     C                   END
005700000000     C                   END
005800000000     C*--- FINE LAVORO ------------------------------------*
005900000000     C     FINE          TAG
006000000000     C                   SETON                                            LR
006100000000     C*----------------------------------------------------*
006200990422     OtmMMP000  E            ANNULL
006300860125     O                       MMPATT
