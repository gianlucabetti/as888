000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200040917      * FITS01R *----------------------------------------------------*
000300040917      *         - RICHEISTA P.O. PER CREAZIONE AMBIENTE DI TEST      *
000400940915      *--------------------------------------------------------------*
000500040917     FFITS01D   CF   E             WORKSTN
000600040917     F                                     SFILE(TS01S01:NRR1)
000700040917     F                                     SFILE(ts01S02:NRR2)
000800951013     FAZORG01L  IF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100020805      *
001200040917     D CTL             S              3    DIM(500)
001300040917     D skpo            S           1500
001400970102     D*
001500950901     D FLA             S              3  0 DIM(300)
001600950901     D TAA             S              3  0 DIM(300)
001700960302     D*
001800040917     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001900040917      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
002000040917     D FNLV55DS      E DS
002100040917     D OG148         E DS
002200040917     D KPJBA         E DS
002300040917     D CNCR80        E DS
002400040917     D UT§DSE0F      E DS
002500040917     D savnrr1         S                   LIKE(nrr1)
002600040917     C**********************************************************************
002700040917     C     *ENTRY        PLIST
002800040917     C                   PARM                    KPJBA
002900040917     C                   Z-ADD     1             CODUT
003000040917     C                   CALL      'X§PARUT'
003100040917     C                   PARM                    UT§DSE0F
003200040917     C                   MOVEL     REC80         CNCR80
003300040917     C                   MOVEL     RAGUT         VIDRSU
003400040917     c*
003500040917     C                   Z-ADD     1             NRR1
003600040917     C                   Z-ADD     1             REC1
003700040917     c* Pulizia sfl
003800040917     C                   SETON                                        70
003900040917     C                   WRITE     ts01C01
004000040917     C                   SETOFF                                       70
004100040917     c                   do        500           nrr1
004200040917     c                   clear                   vidffv
004300040917     c                   clear                   savffv
004400040917     c                   clear                   desffv
004500040917     c                   if        nrr1=1
004600040917     c                   seton                                        51
004700040917     c                   endif
004800040917     c
004900040917     c                   write     ts01s01
005000040917     c                   setoff                                       51
005100040917     c                   enddo
005200040920     c                   eval      vidlib='ZTRAPRD'
005300040920     c                   eval      vidgru='ZTRAGRPF'
005400040920     c                   eval      viduni='ZUNIGRP'
005500040917     c
005600040917     C     FOR01         TAG
005700000828      *
005800040917     C                   WRITE     ts01T01
005900040917     C                   WRITE     ts01Z01
006000040917     C                   EXFMT     ts01C01
006100970102     C                   SETOFF                                       289050
006200040917     C                   SETOFF                                       51
006300040917     C* F3 - FINE
006400040917     C   KC              GOTO      FINE
006500900515      *  CONTROLLI SFL
006600040917     C                   EXSR      CTRD01
006700040917     c   90              goto      for01
006800000825      *
006900040917      * F10=INSERIMENTO TERMINAL DI PARTENZA
007000040917     c                   if        *inkj
007100040917     C                   EXSR      INSTER
007200040917     c                   goto      for01
007300040917     c                   endif
007400040917     c*
007500040917     c  nkf              goto      for01
007600040917     C*
007700040917      * F6  - SOTTOMETTO ELABORAZIONE
007800951026     C     *INKF         IFEQ      *ON
007900040917     c                   movea     ctl           skpo
008000050311     c                   call      'FITS02C'
008100040917     c                   parm                    vidlib
008200040917     c                   parm                    vidgru
008300040917     c                   parm                    viduni
008400040917     c                   parm                    skpo
008500040917     C                   GOTO      FINE
008600951026     C                   ENDIF
008700900515      *
008800000000     C     FINE          TAG
008900960129     C*
009000040917     c                   clear                   fnlv55ds
009100040917     C                   MOVEL     'C'           D55TLA
009200040917     C                   CALL      'FNLV55R'
009300040917     C                   PARM                    FNLV55DS
009400000000     C                   SETON                                        LR
009500900515      *-----------------------------------------------------***********
009600040917      * CONTROLLI SFL                                       *  CTRD01 *
009700900515      *-----------------------------------------------------***********
009800040917     C     CTRD01        BEGSR
009900900515      *
010000040917     C                   SETOFF                                       909650
010100040917     C                   SETOFF                                       2351
010200951025     C                   CLEAR                   CTL
010300040917     C                   CLEAR                   savnrr1
010400900515      *
010500040917     C                   Z-ADD     1             NRR1              3 0
010600040917    1C                   DO        500
010700040917     C     NRR1          CHAIN     ts01S01                            81
010800951025      *
010900951025    2C     VIDFFV        IFEQ      *BLANKS
011000900515     C     VIDFFV        OREQ      *ZEROS
011100900522     C                   MOVEL     *BLANKS       VIDFFV
011200900522     C                   MOVEL     *BLANKS       DESFFV
011300900515      *
011400951025   X2C                   ELSE
011500900522      *
011600900522      * RICERCA FILIALE
011700900522      *
011800951027     C     '?'           SCAN      VIDFFV                                 31
011900951027    3C   31              DO
012000940915     C                   SETON                                        90
012100040917     C                   EXSR      casd24
012200900522     C                   MOVEL     §COD          VIDFFV
012300900530     C                   MOVEL     *BLANKS       DESFFV
012400900530     C                   MOVEL     §§DES         DESFFV
012500040917     c                   seton                                        509023
012600040917     c                   seton                                        51
012700951025    3C                   END
012800900522      *
012900951025     C                   TESTN                   VIDFFV               83
013000951025     C                   MOVE      VIDFFV        W001A             1
013100951025    3C     *IN83         IFEQ      *ON
013200951026     C     W001A         ANDGE     '0'
013300040917     C                   MOVE      VIDFFV        CODFIL            3 0
013400951025     C                   EXSR      CTRFIL
013500040917     c                   if        not *in34
013600040917     c                   movel     orgdes        desffv
013700040917     c                   if        savffv=*blanks
013800040917     c                   seton                                        23
013900040917     c                   endif
014000040917     c                   movel     vidffv        savffv
014100040917     c                   endif
014200040917     c
014300951025     C                   ELSE
014400951026     C                   SETON                                        34
014500951025    3C                   ENDIF
014600951025     C*
014700951026    3C     *IN34         IFEQ      *OFF
014800911105      *
014900900529      * CONTROLLO CHE LA FILIALE NON SIA RIPETUTA
015000040917     C     VIDFFV        LOOKUP    CTL                                    50
015100911105     C* TROVATO ERRORE FIL DOPPIA
015200040917    4C     *IN50         IFEQ      *ON
015300040917     C                   MOVEL     MSG(1)        VIDMSG
015400040917     C                   SETON                                        905128
015500040917   x4C                   ELSE
015600940909     C*
015700911105     C* NON TROVATO MEMORIZZO
015800040917     c                   z-add     1             j                 4 0
015900970102     C     '   '         LOOKUP    CTL(J)                                 83
016000970102     C   83              MOVEL     VIDFFV        CTL(J)
016100040917     c* memorizzio l'ultimo record scritto
016200040917     c                   eval      savnrr1=nrr1
016300040917    4C                   ENDIF
016400040917     c
016500040917   x3c                   else
016600040917     c                   seton                                        509028
016700040917     c                   seton                                        51
016800040917     C                   MOVEL     MSG(2)        VIDMSG
016900040917    3C                   ENDIF
017000040917    2C                   ENDIF
017100951025      *
017200040917     c                   movel     *in23         savi23
017300040917     C                   UPDATE    ts01S01
017400040917     c                   setoff                                       235051
017500951025      *
017600951025     C* POSIZIONO IL CURSORE NELLA PAGIN PER 1 ERRORE
017700951026     C   90
017800951026     CANN96              DO
017900040917     C                   Z-ADD     NRR1          REC1
018000951026     C                   SETON                                        96
018100951025     C                   ENDDO
018200040917     c*
018300040917     c   90              goto      endct1
018400900515      *
018500040917     C                   ADD       1             NRR1
018600951025     C                   ENDDO
018700040917     c
018800040917     c* Se nessun errore posizioamento cursore sul 1 record sfl
018900040917     c                   if        not *in90
019000040917     c                   if        savnrr1=0
019100040917     C     1             CHAIN     ts01S01                            81
019200040917     c                   else
019300040917     C     savnrr1       CHAIN     ts01S01                            81
019400040917     c                   endif
019500040917     c                   movel     savi23        *in23
019600040917     c                   seton                                        51
019700040917     C                   UPDATE    ts01S01
019800040917     c                   setoff                                       235051
019900040917     c                   z-add     1             nrr1
020000040917     c                   z-add     1             rec1
020100040917     c                   endif
020200900515      *
020300040917     C     ENDCT1        ENDSR
020400000825     C*-----------------------------------------------------***********
020500040917     C* INSERIMENTO TERMINAL partenza CON F10
020600000825     C*-----------------------------------------------------***********
020700000825     C     INSTER        BEGSR
020800000828      *
020900000828      * EMETTO SUBFILE VUOTO
021000000828      *
021100040917     C                   SETOFF                                       62
021200040917     C                   Z-ADD     0             NRR2              3 0
021300000828     C                   SETON                                        65
021400040917     C                   WRITE     ts01C02
021500000828     C                   SETOFF                                       65
021600040917     c
021700040917     c                   do        140           nrr2
021800040917     C                   CLEAR                   V02TE2
021900040917     C                   CLEAR                   V02DE2
022000040917     C                   WRITE     ts01s02
022100040917     c                   enddo
022200040917     c
022300040917     C                   Z-ADD     1             REC2
022400040917     C                   WRITE     ts01T01
022500040917     C                   WRITE     ts01Z02
022600040917     C                   WRITE     ts01c02
022700000828     C     INIINT        TAG
022800040917     C                   EXFMT     ts01C02
022900000828     C                   SETOFF                                       626368
023000040917      * F12- - ritorno
023100000828     C   KL              GOTO      ENDINT
023200000828      *
023300040917      * CONTROLLO
023400040917      *
023500040917    1C     1             DO        140           WN                5 0
023600040917     C     WN            CHAIN     ts01S02                            69
023700000829      *
023800040917     C                   SETOFF                                       62
023900040917     C                   CLEAR                   V02DE2
024000000829      *
024100040917    2C     V02TE2        IFEQ      *ALL'0'
024200040917     C                   MOVE      *BLANKS       V02TE2
024300040917    2C                   ENDIF
024400000829      *
024500040917     C     '?'           SCAN      V02TE2                                 69
024600040917    2C     *IN69         IFEQ      *ON
024700040917     C                   EXSR      casd24
024800040917     C                   MOVEL     §COD          V02TE2
024900040917     C                   WRITE     ts01T01
025000040917     C                   WRITE     ts01Z02
025100040917     C                   WRITE     ts01c02
025200040917    2C                   ENDIF
025300000829      *
025400000829      *
025500040917    2C     V02TE2        IFNE      *BLANKS
025600040917     C                   TESTN                   V02TE2               69
025700040917     C                   MOVE      V02TE2        W1ALF             1
025800040917    3C     *IN69         IFEQ      *OFF
025900000829     C     W1ALF         ORLT      '0'
026000000829     C                   SETON                                        62
026100040917   x3C                   ELSE
026200020805     C                   CLEAR                   FNLV55DS
026300040917     C                   MOVEL     'P'           D55TPT
026400040917     C                   MOVE      V02TE2        D55LIN
026500040917     C                   MOVE      *date         d55drf
026600000829     C                   CALL      'FNLV55R'
026700020805     C                   PARM                    FNLV55DS
026800040917    4C     D55ERR        IFNE      ' '
026900000829     C                   SETON                                        62
027000040917   x4C                   ELSE
027100040917    5C     D55LIN        IFNE      D55TFp
027200000829     C                   SETON                                        62
027300000829     C                   ELSE
027400000829     C     D55LIN        CHAIN     AZORG01L                           69
027500040917     C  N69              MOVEL(P)  ORGDES        V02DE2
027600040917    5C                   ENDIF
027700040917    4C                   ENDIF
027800040917    3C                   ENDIF
027900040917    2C                   ENDIF
028000040917      * errore
028100040917     C                   UPDATE    ts01S02
028200040917     c
028300040917    2C     *IN62         IFEQ      *ON
028400000829     C                   LEAVE
028500040917    2c                   endif
028600040917     c
028700040917    1c                   enddo
028800000829      *
028900000829      *
029000000828      * SE VI SONO ERRORI O NON E' STATO PREMUTO F6 RIEMETTO
029100040917    1C     *IN62         IFEQ      *ON
029200040917     C                   Z-ADD     NRR2          REC2
029300000829     C                   GOTO      INIINT
029400040917    1C                   ENDIF
029500040917     c
029600040917     C  Nkf              GOTO      INIINT
029700000828      *
029800000829      * SE E' STATO PREMUTO F6 (INSERIMENTO) E NON VI SONO ERRORI
029900040917      * ESPLODO I P.O. DI partenza x AGGIUNGERLI NELLA VIDEATA PRECEDENTE
030000000828      *
030100040917     c                   eval       nrr1=savnrr1
030200040917     c                   clear                   wposiz            1
030300040917    1C     1             DO        *HIVAL        WN
030400040917     C     WN            CHAIN     ts01S02                            69
030500000829     C   69              LEAVE
030600040917    2C     V02TE2        IFNE      *BLANKS
030700040917     C                   MOVE      V02TE2        WTE1              3 0
030800000830     C                   EXSR      AGGTER
030900040917    2C                   ENDIF
031000040917    1C                   ENDDO
031100000829      *
031200000828     C     ENDINT        ENDSR
031300000829     C*----------------------------------------------------------------
031400000829     C* AGGIORNAMENTO LINEE DI ARRIVO DOPO F6 DI INSERIMENTO TERMINAL
031500000829     C*----------------------------------------------------------------
031600000829     C     AGGTER        BEGSR
031700000829     C                   SETON                                        23
031800000830      *
031900000830     C                   Z-ADD     1             L
032000000830     C     WTE1          LOOKUP    TAA(L)                                 31
032100001006    1C     *IN31         DOWEQ     *ON
032200000830     C*
032300040917     C                   MOVEL     FLA(L)        Wpo               3 0
032400000830     C                   EXSR      AGGSF3
032500000830     C*
032600000830     C                   ADD       1             L
032700000830     C     WTE1          LOOKUP    TAA(L)                                 31
032800001006    1C                   ENDDO
032900000830      *
033000000829     C                   ENDSR
033100000830     C*----------------------------------------------------------------
033200000830     C* AGGIORNAMENTO SUBFILE LNA DA INSERIMENTO TERMINAL
033300000830     C*----------------------------------------------------------------
033400000830     C     AGGSF3        BEGSR
033500000830      *
033600040917     c                   add       1             nrr1
033700040917     C     nrr1          CHAIN     ts01S01                            22
033800040917     c
033900040917     C  n22VIDFFV        IFEQ      *BLANKS
034000000830     C                   MOVEL(P)  '<-- Ins.'    DESFFV
034100040917     C                   MOVEL     Wpo           VIDFFV
034200040917     C                   UPDATE    ts01S01
034300000830     C     WPOSIZ        IFEQ      ' '
034400000830     C                   MOVE      'P'           WPOSIZ
034500040917     C                   Z-ADD     NRR1          REC1
034600000830     C                   ENDIF
034700000830     C                   ENDIF
034800000830      *
034900000830     C                   ENDSR
035000940909     C**************************************************************************
035100940909     C*   R O U T I N E    I N I Z I A L E
035200940909     C**************************************************************************
035300940909     C     *INZSR        BEGSR
035400040917      *
035500040917      * CARICO TUTTi  i p.o.   DI AZORG00F CON IL RELATIVO TERMINAL part
035600040917     C                   Z-ADD     1             L                 4 0
035700040917     C     *LOVAL        SETLL     AZORG
035800040917     C                   READ      AZORG                                  83
035900040917 --- C     *IN83         DOWEQ     *OFF
036000040917     C     ORGFVA        IFEQ      ' '
036100040917     C     ORGFAG        IFEQ      'F'
036200040917     C     ORGFAG        OREQ      'A'
036300040917     c
036400040917     C                   MOVEL     ORGDE8        OG148
036500040917     C     §OGLFV        IFNE      'N'
036600040917     C                   Z-ADD     ORGFIL        FLA(L)
036700040917     C                   CLEAR                   FNLV55DS
036800040917     C                   MOVEL     'P'           D55TPT
036900040917     C                   MOVE      ORGFIL        D55LIN
037000040917     C                   MOVE      *date         D55DRF
037100040917     C                   CALL      'FNLV55R'
037200040917     C                   PARM                    FNLV55DS
037300040917     C                   Z-ADD     D55TFp        TAA(L)
037400040917     C                   ENDIF
037500040917     C                   ADD       1             L
037600040917     C                   ENDIF
037700040917     C                   ENDIF
037800040917     C                   READ      AZORG                                  83
037900040917 --- C                   ENDDO
038000040917     C                   ENDSR
038100951025     C**************************************************************************
038200951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
038300951025     C**************************************************************************
038400951025     C     CTRFIL        BEGSR
038500020213
038600951025     C     CODFIL        CHAIN     AZORG01L                           34
038700951025    1C  N34ORGFVA        IFNE      ' '
038800951025     C     ORGFAG        ORNE      'A'
038900951025     C     ORGFAG        ANDNE     'F'
039000951025     C                   SETON                                        34
039100951025    1C                   ENDIF
039200951025     C*
039300951025     C                   ENDSR
039400040917     C*---------------------------------------------------------------*
039500040917     C* CALL AL PGM TNSD19R DI RICERCA LINEE DI ARRIVO
039600040917     C*---------------------------------------------------------------*
039700040917     C     casd24        BEGSR
039800040917     C                   MOVEL     *BLANKS       §COD
039900040917     C                   MOVEL     *BLANKS       §TIP
040000040917     C                   MOVEL     *BLANKS       §§DES
040100040917     C                   MOVEL     *BLANKS       §§pra
040200040917     C                   CALL      'TNSD24R'
040300040917     C                   PARM                    §Cod              3
040400040917     C                   PARM                    §Tip              1
040500040917     C                   PARM                    §§Des            25
040600040917     C                   PARM                    §§Pra             3
040700040917     C                   ENDSR
040800020805      **************************************************************************
040900960302** SCHIERA MSG - MESSAGGI DI ERRORE
041000040917Punto Operativo gia' presente                                                 16
041100040917Punto operativo errato                                                        17
