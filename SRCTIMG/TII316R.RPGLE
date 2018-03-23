000100990203     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300990317     FTII316D   CF   E             WORKSTN
000400880224     D PARAM2          DS
000500990203     D  AMELA                         4  0
000600990203     d  socpas                        3
000700000000     D KPJBA         E DS
000800990203     D soc001        E DS                  EXTNAME(xsoc001ds)
000900990203     D xsocds          DS          1000
001000000000     I*---- PASSAGGIO PARAMETRI
001100000000     C*----------------------------------------------------*
001200000000     C     *ENTRY        PLIST                                                  *
001300000000     C                   PARM                    KPJBA                          *
001400990203     C*---------- RICERCA DITTA :
001500990203     C                   MOVEL     'SOC001'      TIPXSC
001600990203     C                   MOVEL     *blank        SOCXSC
001700990203     C                   EXSR      REPSOC
001800990203     C     RTNXSC        IFNE      '1'
001900990203     C                   MOVEL     XSOCDS        SOC001
002000990203     C                   MOVEL     xscsoc        socpas
002100990203     c                   end
002200000000     C******************************************************
002300000000     C*AGGANCIO PER CODICE CLIENTE E N. ORDINE
002400990203     C                   Z-ADD     0             AMELA
002500000000     C     STAR          TAG
002600000000     C                   SETOFF                                       98
002700000000     C                   Z-ADD     0             VIDAM
002800000000     C     SU            TAG
002900000000     C                   EXFMT     OCO813D1
003000990203     C                   SETOFF                                       02
003100990203     C   Kl              GOTO      FINE
003200990203     C     VIDAM         CABEQ     0             STAR                     02
003300000000     C** FINE LAVORO
003400990203     C                   MOVEL     VIDAM         MMELA             2 0
003500990203     C                   MOVE      VIDAM         AAELA             2 0
003600990203     C                   MOVEL     AAELA         AMELA             4 0
003700990203     C                   MOVE      MMELA         AMELA
003800000000     C                   SETON                                        98
003900000000     C   98
004000000000     CANNKA              GOTO      SU
004100000000     C   KA              DO
004200990827     C                   MOVEL     'I317'        KCOAZ
004300990827     C                   MOVEL     PARAM2        KPJBU
004400990916     C                   CALL      'BCH09'
004500990827     C                   PARM                    KPJBA
004600990916     C                   CALL      'BCH10'
004700990916     C                   PARM                    KPJBA
004800000000     C                   END
004900000000     C   KA              GOTO      STAR
005000000000     C     FINE          TAG
005100000000     C                   SETON                                        LR
005200990203     C*----------------------------------------------------*
005300990203     C* Reperimento dati società
005400990203     C*----------------------------------------------------*
005500990203     C     REPSOC        BEGSR
005600990203     C*
005700990203     C                   CALLB     'XSOC'
005800990203     C                   PARM                    TIPXSC            6
005900990203     C                   PARM                    SOCXSC            3
006000990203     C                   PARM                    CDSXSC            9 0
006100990203     C                   PARM                    MODXSC            3
006200990203     C                   PARM      *blanks       RTNXSC            1
006300990203     C                   PARM                    XSOCDS
006400990203     C                   PARM                    KPJBA
006500990203     C*
006600990203     C                   ENDSR
