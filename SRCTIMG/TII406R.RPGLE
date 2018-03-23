000100990415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* OFN350R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*             - PULIZIA ARCHIVIO ORDINI FORNIT. -FILTRO-       *
000600000000     H*             ------------------------------------------       *
000700000000     H*                                                      AZ.N350 *
000800000000     H*--------------------------------------------------------------*
000900990415     Ftii406D   CF   E             WORKSTN
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
003100990415     c                   move      como14        vidfat
003200000000     C     SU            TAG
003300000000     C                   EXFMT     OFN350D1
003400990415     C   Kc              GOTO      FINE
003500000000     C     VIDFAT        CABEQ     0             SU
003600990415     C     *dmy          test(d)                 vidfat                 70      *
003700990415     c  n70*dmy          move      vidfat        dataeur
003800990415     c  n70              move      dataeur       vidfat
003900990415     C   70*eur          test(d)                 vidfat                 70      *
004000990415     C   70              goto      su                                           *
004100000000     C*CONTROLLI SUPERATI
004200000000     C                   SETON                                        98
004300000000     C   98
004400000000     CANNKA              GOTO      SU
004500990415     C                   MOVEL     'I407'        KCOAZ
004600000000     C                   MOVEL     PARAM2        KPJBU
004700000000     C                   CALL      'BCH09'
004800000000     C                   PARM                    KPJBA
004900000000     C                   CALL      'BCH10'
005000000000     C                   PARM                    KPJBA
005100000000     C     FINE          TAG
005200000000     C                   SETON                                        LR
005300990415     C*----------------------------------------------------*
005400990415     C* Reperimento dati società
005500990415     C*----------------------------------------------------*
005600990415     C     REPSOC        BEGSR
005700990415     C*
005800990415     C                   CALLB     'XSOC'
005900990415     C                   PARM                    TIPXSC            6
006000990415     C                   PARM                    SOCXSC            3
006100990415     C                   PARM                    CDSXSC            9 0
006200990415     C                   PARM                    MODXSC            3
006300990415     C                   PARM      *blanks       RTNXSC            1
006400990415     C                   PARM                    XSOCDS
006500990415     C                   PARM                    KPJBA
006600990415     C*
006700990415     C                   ENDSR
