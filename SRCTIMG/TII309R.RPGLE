000100990127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300990317     FTII309D   CF   E             WORKSTN
000400990127     Ftmmoc01L  IF   E           K DISK
000500000000     D PARAM2          DS
000600990127     D  VIDCLI                 1      8
000700990127     D  VIDNOR                 9     13  0
000800990127     D  socpas                14     16
000900990127     D soc001        E DS                  EXTNAME(xsoc001ds)
001000990127     D xsocds          DS          1000
001100000000     D KPJBA         E DS
001200000000     I*---- PASSAGGIO PARAMETRI
001300000000     C*----------------------------------------------------*
001400000000     C     *ENTRY        PLIST                                                  *
001500000000     C                   PARM                    KPJBA                          *
001600990127     C*---------- RICERCA DITTA :
001700990127     C                   MOVEL     'SOC001'      TIPXSC
001800990127     C                   MOVEL     *blank        SOCXSC
001900990127     C                   EXSR      REPSOC
002000990127     C     RTNXSC        IFNE      '1'
002100990127     C                   MOVEL     XSOCDS        SOC001
002200990127     C                   MOVEL     xscrgs        desdit
002300990127     C                   MOVEL     xscsoc        socpas
002400990127     c                   end
002500870630     C******************************************************
002600870630     C*AGGANCIO PER CODICE CLIENTE E N. ORDINE
002700870630     C     KEYMOC        KLIST
002800870630     C                   KFLD                    VIDCLI
002900870630     C                   KFLD                    VIDNOR
003000870630     C*AGGANCIO PER CODICE CLIENTE
003100870630     C     KEYM          KLIST
003200870630     C                   KFLD                    VIDCLI
003300870630     C*
003400000000     C     STAR          TAG
003500870703     C                   SETOFF                                       98
003600000000     C                   MOVEL     *BLANKS       VIDCLI
003700870630     C                   Z-ADD     0             VIDNOR
003800990127     C                   MOVEL     99999999      VIDCLI
003900871119     C                   Z-ADD     99999         VIDNOR
004000000000     C     SU            TAG
004100870630     C                   EXFMT     OCO307D1
004200990409     C   Kc              GOTO      FINE
004300870630     C*CLIENTE E N. ORDINE OBBLIGATORI MAGGIORI DI ZERO
004400870630     C     VIDCLI        CABEQ     *BLANKS       STAR
004500870630     C     VIDNOR        CABEQ     0             STAR
004600870630     C*ERRORE SE SI RICHIEDE PER TUTTI I CLIENTI UN  N.ORDINE SPECIFICO
004700870630     C*
004800990127     C     VIDCLI        IFEQ      '99999999'
004900870630     C     VIDNOR        IFNE      99999
005000990127     C     KEYM          CHAIN     tmmoc000                           65
005100870710     C   65              DO
005200870710     C                   SETON                                        95
005300870630     C                   GOTO      SU
005400870710     C                   END
005500870630     C                   END
005600870630     C                   END
005700870630     C*CONTROLLO CHE IL CLIENTE E IL N. ORDINE ESISTANO SE NO SEGNALO
005800870630     C*L'ERRORE
005900990127     C     VIDCLI        IFNE      '99999999'
006000870630     C     VIDNOR        IFNE      99999
006100990127     C     KEYMOC        CHAIN     tmmoc000                           70
006200870630     C   70              DO
006300870630     C                   SETON                                        50
006400870630     C                   GOTO      SU
006500870630     C                   END
006600870630     C                   END
006700870630     C                   END
006800870630     C*CONTROLLO CHE IL CLIENTE ESISTA SE NO SEGNALO ERRORE
006900990127     C     VIDCLI        IFNE      '99999999'
007000870630     C     VIDNOR        IFEQ      99999
007100990127     C     KEYM          CHAIN     tmmoc000                           60
007200870630     C   60              DO
007300870710     C                   SETON                                        95
007400870630     C                   GOTO      SU
007500870630     C                   END
007600870630     C                   END
007700870630     C                   END
007800000000     C** FINE LAVORO
007900000000     C                   SETON                                        98
008000000000     C   98
008100000000     CANNKA              GOTO      SU
008200000000     C   KA              DO
008300990317     C                   MOVEL     'I310'        KCOAZ
008400000000     C                   MOVEL     PARAM2        KPJBU
008500000000     C                   CALL      'BCH09'
008600000000     C                   PARM                    KPJBA
008700000000     C                   CALL      'BCH10'
008800000000     C                   PARM                    KPJBA
008900000000     C                   END
009000870702     C   KA              GOTO      STAR
009100000000     C     FINE          TAG
009200000000     C                   SETON                                        LR
009300990127     C*----------------------------------------------------*
009400990127     C* Reperimento dati società
009500990127     C*----------------------------------------------------*
009600990127     C     REPSOC        BEGSR
009700990127     C*
009800990127     C                   CALLB     'XSOC'
009900990127     C                   PARM                    TIPXSC            6
010000990127     C                   PARM                    SOCXSC            3
010100990127     C                   PARM                    CDSXSC            9 0
010200990127     C                   PARM                    MODXSC            3
010300990127     C                   PARM      *blanks       RTNXSC            1
010400990127     C                   PARM                    XSOCDS
010500990127     C                   PARM                    KPJBA
010600990127     C*
010700990127     C                   ENDSR
