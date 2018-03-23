000100990127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300870513     H* OCO300R *-OC/38-*--------------------------------------------*
000400870513     H*-----------------*                                            *
000500870513     H*             - SIMULAZIONE ASSEGNAZIONE - FILTRO -            *
000600870513     H*             ---------------------------------------          *
000700870513     H*                                                      AZ.O301 *
000800870513     H*         FILE : OCO300D  -                                    *
000900870513     H*                                                              *
001000870513     H*--------------------------------------------------------------*
001100990317     FTII303D   CF   E             WORKSTN
001200870513     D PARAM2          DS
001300990127     D  VIDDCP                 1      8  0
001400990127     D  VIDVAL                 9     13  2
001500990127     D  VIDFLS                14     14
001600990127     D  VIDIMP                15     29  0
001700990127     D  socpas                30     32
001800990127     D soc001        E DS                  EXTNAME(xsoc001ds)
001900990127     D xsocds          DS          1000
002000990127     d dataiso         s               d   datfmt(*iso)
002100990127     d dataeur         s               d   datfmt(*eur)
002200870513     D KPJBA         E DS
002300870513     I*---- PASSAGGIO PARAMETRI
002400870513     C*----------------------------------------------------*
002500870513     C     *ENTRY        PLIST                                                  *
002600870513     C                   PARM                    KPJBA                          *
002700990127     C*---------- RICERCA DITTA :
002800990127     C                   MOVEL     'SOC001'      TIPXSC
002900990127     C                   MOVEL     *blank        SOCXSC
003000990127     C                   EXSR      REPSOC
003100990127     C     RTNXSC        IFNE      '1'
003200990127     C                   MOVEL     XSOCDS        SOC001
003300990127     C                   MOVEL     xscrgs        desdit
003400990127     C                   MOVEL     xscsoc        socpas
003500990127     c                   end
003600990127     c                   time                    como14           14 0
003700990127     c                   move      como14        woggi             8 0
003800870827     C     STAR          TAG
003900990127     C                   Z-ADD     woggi         VIDDCP
004000870513     C                   Z-ADD     0             VIDVAL
004100870514     C                   MOVEL     'N'           VIDFLS
004200870513     C                   Z-ADD     0             VIDIMP
004300870827     C     SU            TAG
004400870513     C                   EXFMT     OCO300D1
004500990409     C   Kc              GOTO      FINE
004600990127     c     *eur          test(d)                 viddcp                 97
004700990127     c                   if        *in97
004800990127     c     *dmy          test(d)                 viddcp                 97
004900990127     c  n97*dmy          move      viddcp        dataeur
005000990127     c  n97              move      dataeur       viddcp
005100990127     c                   end
005200990127     c   97              goto      su
005300870513     C** FINE LAVORO
005400870513     C                   SETON                                        98
005500870513     C   98
005600870513     CANNKA              GOTO      SU
005700870513     C   KA              DO
005800990317     C                   MOVEL     'I305'        KCOAZ
005900870513     C                   MOVEL     PARAM2        KPJBU
006000990415     C                   CALL      'BCH09'
006100990415     C                   PARM                    KPJBA
006200990415     C                   CALL      'BCH10'
006300870513     C                   PARM                    KPJBA
006400870513     C                   END
006500870513     C     FINE          TAG
006600870513     C                   SETON                                        LR
006700990127     C*----------------------------------------------------*
006800990127     C* Reperimento dati società
006900990127     C*----------------------------------------------------*
007000990127     C     REPSOC        BEGSR
007100990127     C*
007200990127     C                   CALLB     'XSOC'
007300990127     C                   PARM                    TIPXSC            6
007400990127     C                   PARM                    SOCXSC            3
007500990127     C                   PARM                    CDSXSC            9 0
007600990127     C                   PARM                    MODXSC            3
007700990127     C                   PARM      *blanks       RTNXSC            1
007800990127     C                   PARM                    XSOCDS
007900990127     C                   PARM                    KPJBA
008000990127     C*
008100990127     C                   ENDSR
