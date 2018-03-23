000100990415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300880517     H* OCO350R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*             - PULIZIA ARCHIVIO ORDINI CLIENTI -FILTRO-       *
000600000000     H*             ------------------------------------------       *
000700000000     H*                                                      AZ.O350 *
000800000000     H*--------------------------------------------------------------*
000900990415     Ftii321D   CF   E             WORKSTN
001000000000     D PARAM2          DS
001100990415     D  VIDFAT                        8  0
001200990415     D  socpas                        3
001300000000     D KPJBA         E DS
001400990415     D soc001        E DS                  EXTNAME(xsoc001ds)
001500990415     D xsocds          DS          1000
001600990415     d dataiso         s               d   datfmt(*iso)
001700990415     d dataeur         s               d   datfmt(*eur)
001800000000     I*---- PASSAGGIO PARAMETRI
001900000000     C*----------------------------------------------------*
002000000000     C     *ENTRY        PLIST                                                  *
002100000000     C                   PARM                    KPJBA                          *
002200990415     C*---------- RICERCA DITTA :
002300990415     C                   MOVEL     'SOC001'      TIPXSC
002400990415     C                   MOVEL     *blank        SOCXSC
002500990415     C                   EXSR      REPSOC
002600990415     C     RTNXSC        IFNE      '1'
002700990415     C                   MOVEL     XSOCDS        SOC001
002800990415     C                   MOVEL     xscsoc        socpas
002900990415     c                   end
003000990415     c                   time                    como14           14 0
003100990415     c                   move      como14        woggi             8 0
003200990415     c                   move      woggi         dataeur
003300990415     c                   move      dataeur       vidfat
003400880422     C     SU            TAG
003500880422     C                   EXFMT     OCO350D1
003600990415     C   Kc              GOTO      FINE
003700880518     C     VIDFAT        CABEQ     0             SU
003800990415     C     *dmy          test(d)                 vidfat                 70      *
003900990415     c  n70*dmy          move      vidfat        dataeur
004000990415     c  n70              move      dataeur       vidfat
004100990415     C   70*eur          test(d)                 vidfat                 70      *
004200990415     C   70              goto      su                                           *
004300880517     C*CONTROLLI SUPERATI
004400880517     C                   SETON                                        98
004500880518     C   98
004600880518     CANNKA              GOTO      SU
004700000228     C                   MOVEL     'I322'        KCOAZ
004800880422     C                   MOVEL     PARAM2        KPJBU
004900880518     C                   CALL      'BCH09'
005000880518     C                   PARM                    KPJBA
005100880422     C                   CALL      'BCH10'
005200880422     C                   PARM                    KPJBA
005300880422     C     FINE          TAG
005400880422     C                   SETON                                        LR
005500990415     C*----------------------------------------------------*
005600990415     C* Reperimento dati società
005700990415     C*----------------------------------------------------*
005800990415     C     REPSOC        BEGSR
005900990415     C*
006000990415     C                   CALLB     'XSOC'
006100990415     C                   PARM                    TIPXSC            6
006200990415     C                   PARM                    SOCXSC            3
006300990415     C                   PARM                    CDSXSC            9 0
006400990415     C                   PARM                    MODXSC            3
006500990415     C                   PARM      *blanks       RTNXSC            1
006600990415     C                   PARM                    XSOCDS
006700990415     C                   PARM                    KPJBA
006800990415     C*
006900990415     C                   ENDSR
