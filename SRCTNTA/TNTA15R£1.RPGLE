000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941220      * TNTA15R *----------------------------------------------------*
000300900525      *   - INTERROGAZIONE TARIFFE GIACENZA                      LC. *
000400900515      *--------------------------------------------------------------*
000500050317      * A.G.  17/03/05                                               *
000600050317      * TOLTO COMANDO F9 DI STAMPA LETTERA --------------------------*
000700050317      * non dovrebbe servire a nulla nel caso in cui bisogna         *
000800050317      * ripristinarlo basta riattivare il comando F9 e il relativo   *
000900050317      * indicatore di risposta  09 (togliere setoff 09)              *
001000050317      *--------------------------------------------------------------*
001100941220     FTNTA14D   CF   E             WORKSTN
001200900515     FCNACO00F  IF   E           K DISK
001300990622     FTITGC01L  IF   E           K DISK
001400950106     FFNCLS01L  IF   E           K DISK
001500900523     FTABEL00F  IF   E           K DISK
001600900515      *
001700900525     D DES             S             20    DIM(1) CTDATA PERRCD(1)
001800920206     D TES             S             30    DIM(1) CTDATA PERRCD(1)
001900900517      *
002000950106     D                 DS
002100950106     D  VIDFL1                 1      1
002200950106     D  VIDFL0                10     10
002300950106     D  FLG                    1     10
002400950106     D                                     DIM(10)
002500900514     D KPJBA         E DS
002600900516      *
002700940316     D DS1H          E DS
002800020520     D*CNCR80        E DS
002900020520     D*UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000900523      *
003100020520     D*PARAM1          DS
003200020520     D* CODUTE                 1      1  0
003300020520     D* RAGUTE                 2     49
003400020520     D* KCR                   50     65
003500020520     D* CLIITA                66     69  0
003600020520     D* CLIEST                70     73  0
003700020520     D* FORITA                74     77  0
003800020520     D* FOREST                78     81  0
003900020520     D* AGEITA                82     85  0
004000020520     D* AGEEST                86     89  0
004100020520     D* CCNIVA                90     93  0
004200020520     D* CCNBAN                94     97  0
004300020520     D* CCNIND                98    101  0
004400941221     D*
004500941221     D* RICHIAMO A PGM DI STAMPA LETTERA     - FNLV45C -
004600981116     D DSLV45        E DS                  EXTNAME(FNLV45DS)
004700021022     D FNLV41DS      E DS
004800941221     D*
004900941221     D* PARAM PASSATA DA PGM RICHIAMANTI
005000900522     D PARAM           DS
005100940316     D* CODICE CLIENTE
005200940316     D  PASCLI                10     16
005300941221     D* DECODIFICA CODICE TARIFFA
005400941221     D  PASDCT                17     26
005500941221     D* DESCRIZIONE TARIFFA
005600941221     D  PASDCV                27     41
005700990824     D* CODICE DIVISA
005800990824     D  VICDIV                43     45
005900941221     D* CODICE TARIFFA E PROGRESSIVO
006000941221     D  PASCTR               125    127  0
006100941221     D  PASPRG               128    130  0
006200990914      *
006300990914     D WLBDAT          DS
006400990914     D  G02DAT                 1      8  0
006500990914     D  G02INV                 9     16  0
006600990914     D  G02ERR                17     17
006700990914     D  G02TGI                18     22  0
006800020520     D*
006900020520     D tibs34ds      E DS                  inz
007000020520     D ddatiute      e ds
007100020520     D azuteds       e ds                  extname(AZUTE00F)
007200020520     D dLAT          e ds
007300020520     D codut           S                   LIKE(TBLkut) inz(1)
007400941221     I*
007500900515      *---------------------------------------------------------------*
007600000000     C     *ENTRY        PLIST
007700000000     C                   PARM                    KPJBA
007800050317     C                   MOVEL     KPJBU         PARAM
007900050317     c                   setoff                                       09
008000900516      *
008100020520     C*                  Z-ADD     1             CODUT
008200020520     C*                  CALL      'X§PARUT'
008300020520     C*                  PARM                    UT§DSE
008400020520     C*                  MOVEL     RAGUT         RSUT             20
008500020520     C*                  MOVEL     REC80         CNCR80
008600020520     C*                  MOVEL     RAGUT         PAXDUT           30
008700900515      *
008800020520     C*                  MOVE      *ZEROS        PARAM1
008900020520     C*                  Z-ADD     1             CODUTE
009000020520     C*                  CALL      'XPARCCC'
009100020520     C*                  PARM                    PARAM1
009200020520     c*
009300020520     c     *dtaara       define    §azute        azuteds
009400020520     c     *dtaara       define    §datiute      ddatiute
009500020520     C                   in(E)     *dtaara
009600020520     C                   IF        %error  or RSUT = *blanks
009700020520     C                   CLEAR                   tibs34ds
009800020520     C                   CALL      'TIBS34R'
009900020520     C                   PARM                    tibs34ds
010000020520     C                   in        *dtaara
010100020520     c                   endif
010200020520     c* N.B.:Dal momento che la call al tbs34r mi serve solo per reperire il
010300020520     c* capoconto clienti italia e la ragione sociale utente non testo la
010400020520     c* presenza di eventuali errori
010500020520     C                   MOVEL(P)  RSUT          PAXDUT           30
010600020919      *
010700020919     C                   MOVEL     SIMFEL        simfelA           3
010800020919      *
010900941221     C* PRENDO LA DATA DEL GIORNO DA TIME
011000941221     C                   TIME                    W0140            14 0
011100941221     C                   MOVE      W0140         UDATE8            8 0
011200900515      *---------------------------------------------------------------*
011300900515      * CHIAVI                                                        *
011400900515      *---------------------------------------------------------------*
011500900515     C     KACO          KLIST
011600900515     C                   KFLD                    CODUT
011700020520     C*                  KFLD                    CLIITA
011800020520     C                   KFLD                    dutkci
011900900517     C                   KFLD                    CODCLI            7 0
012000900517      *
012100950106     C     KTGC          KLIST
012200900517     C                   KFLD                    CODCLI
012300940903     C                   KFLD                    VIDCTR
012400940903     C                   KFLD                    VIDPRG
012500900522      *
012600900522     C     KAZO          KLIST
012700900522     C                   KFLD                    CODFIL            3 0
012800900523      *
012900900523     C     KTAB          KLIST
013000900523     C                   KFLD                    CODUT
013100900523     C                   KFLD                    CODTAB            2
013200900523     C                   KFLD                    KEYTAB            8
013300900515      *---------------------------------------------------------------*
013400900515      * FLUSSO PRINCIPALE                                             *
013500900515      *---------------------------------------------------------------*
013600900515      *          =============
013700900509     C     INIZIO        TAG
013800900515      *          =============
013900900515     C                   EXSR      PULIZ
014000940316     C*
014100920206     C                   MOVEL     TES(1)        VIDTES
014200940316      * RICHIAMATO
014300940316     C     PASCLI        IFNE      *BLANKS
014400940316     C                   SETON                                        02
014500900522     C                   MOVE      PASCLI        VIDCLI
014600940903     C                   MOVE      PASCTR        VIDCTR
014700940903     C                   MOVE      PASPRG        VIDPRG
014800940903     C                   MOVE      PASDCT        DESCTR
014900940903     C                   MOVE      PASDCV        VIDDCV
015000900522     C                   GOTO      CHIAM
015100900522     C                   END
015200900522      *
015300900515      *          =============
015400900517     C     FMT01         TAG
015500900515      *          =============
015600941220     C                   EXFMT     TA14D01
015700960404     C   KC              GOTO      FINE
015800900522      *          =============
015900900522     C     CHIAM         TAG
016000900522      *          =============
016100900517     C                   EXSR      CTR1
016200900517     C   90              GOTO      FMT01
016300900516      *
016400990622     C     KTGC          CHAIN     TITGC01L                           80
016500900517     C  N80              EXSR      RIEMPI
016600940316     C   80
016700940316     CANN02              GOTO      FMT01
016800940316     C   80
016900940316     CAN 02              GOTO      FINE
017000900517      *
017100900515      *          =============
017200900517     C     FMT02         TAG
017300900515      *          =============
017400941220     C                   EXFMT     TA14D02
017500940316     C* CMD12 - RITORNO
017600960404     C   KL              DO
017700940316     C   02              GOTO      FINE
017800940316     C  N02              GOTO      INIZIO
017900900523     C                   END
018000940316     C* CONTROLLI
018100900517     C                   EXSR      CTR2
018200900517     C   90              GOTO      FMT02
018300900522      *
018400940316     C* CMD9 - STAMPA LETTERA
018500021022     C   09              DO
018600021022      *
018700021022      * Richiamo FnLv40 per aggiornare file testi e reperire ds1h
018800021022     C                   CLEAR                   Ds1H
018900021022     C                   CLEAR                   Fnlv41ds
019000021022     C                   MOVEL     '3'           I41MOD
019100021022     C                   MOVEL     '45'          I41LET
019200021022     C                   CALL      'FNLV41R'
019300021022     C                   PARM                    FNLV41DS
019400021022      *
019500021022      * Se non ho errori stampo
019600021022     C     O41ERR        IFNE      'E'
019700021022     C                   MOVEL     O41DTA        DS1H
019800981116     C                   CLEAR                   DSLV45
019900981116     C                   MOVEL     §1HPGS        I45PGM
020000981116     C                   Z-ADD     §1HNCO        I45NCO
020100020520     C*                  Z-ADD     CLIITA        I45KCC
020200020520     C                   Z-ADD     dutkci        I45KCC
020300981116     C                   Z-ADD     UDATE8        I45GDS
020400981116     C                   MOVEL     VIDCLI        I45KSC
020500981116     C                   MOVEL     '45'          I45TTX
020600981116     C                   MOVEL     VIDCTR        I45CTR
020700981116     C                   MOVEL     VIDPRG        I45PRG
020800981116     C                   MOVEL     'C'           I45AST
020900981116     C                   MOVEL     'I'           I45LIN
021000941221     C                   CALL      'FNLV45C'
021100940316     C                   PARM                    KPJBA
021200981116     C                   PARM                    DSLV45
021300940316     C                   ENDIF
021400940316     C*
021500940316     C  N02              GOTO      FMT01
021600940316     C   02              GOTO      FINE
021700940316     C                   ENDDO
021800900517      *
021900000000     C     FINE          TAG
022000000000     C                   SETON                                        LR
022100900515      *-----------------------------------------------------***********
022200900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
022300900515      *-----------------------------------------------------***********
022400900515     C     PULIZ         BEGSR
022500900515      *
022600900517     C                   MOVE      *BLANKS       VIDCLI
022700900517     C                   MOVEL     *BLANKS       DESCLI
022800940903     C                   Z-ADD     0             VIDCTR
022900940903     C                   MOVEL     *BLANKS       DESCTR
023000940903     C                   MOVEL     *BLANKS       VIDDCV
023100990914     C                   Z-ADD     0             VIDDUV
023200940903     C                   Z-ADD     0             VIDPRG
023300900517     C                   Z-ADD     0             VIDSGV
023400940903     C                   Z-ADD     0             VIDSGR
023500900517     C                   Z-ADD     0             VIDSGP
023600900517     C                   Z-ADD     0             VIDSGD
023700900517     C                   Z-ADD     0             VIDSG1
023800900517     C                   Z-ADD     0             VIDSG2
023900900517     C                   Z-ADD     0             VIDSG3
024000900517     C                   Z-ADD     0             VIDST1
024100900517     C                   Z-ADD     0             VIDST2
024200900517     C                   Z-ADD     0             VIDST3
024300900517     C                   Z-ADD     0             VIDSTM
024400900517     C                   Z-ADD     0             VIDRPV
024500900517     C                   Z-ADD     0             VIDSGF
024600911120     C                   Z-ADD     0             VIDGGR
024700940903     C                   MOVEL     ' '           VIDFAF
024800940903     C                   MOVEL     ' '           VIDTCM
024900940903     C                   MOVEL     ' '           VIDTFG
025000940903     C                   CLEAR                   DESTCM
025100940903     C                   CLEAR                   DESTFG
025200900515      *
025300941220     C                   MOVEL     'TNTA15R'     VIDPGM
025400900525     C                   SETON                                        9899
025500900524      *
025600900514     C                   ENDSR
025700900515      *-----------------------------------------------------***********
025800900517      * CONTROLLI FORMATO 1                                 *  CTR1   *
025900900515      *-----------------------------------------------------***********
026000900517     C     CTR1          BEGSR
026100900515      *
026200900515     C                   SETOFF                                       90
026300900515      *
026400900517      * RICERCA CLIENTE
026500900517      *
026600940316     C     '?'           SCAN      VIDCLI                                 21
026700900517     C   21              DO
026800900517     C                   SETON                                        04
026900000821     C                   CLEAR                   PAXDIT
027000020520     C*                  Z-ADD     CLIITA        ACOKCC
027100020520     C                   Z-ADD     dutkci        ACOKCC
027200900517     C                   MOVEL     *BLANKS       ACORAG
027300900517     C                   MOVEL     DESCLI        ACORAG
027400000821     C                   Z-ADD     1             PAXNUM
027500000821     C                   Z-ADD     0             PAXSTA            1 0
027600000821     C                   CALL      'XALFA3BR'
027700000821     C                   PARM                    PAXDUT
027800900517     C                   PARM                    CODUT
027900900517     C                   PARM                    ACORAG
028000900517     C                   PARM                    ACOKCC
028100000821     C                   PARM                    PAXSTA
028200000821     C                   PARM                    PAXFLR           90
028300000821     C                   PARM                    PAXDIT            3
028400000821     C                   PARM                    PAXNUM            2 0
028500000821     C                   PARM                    PAXKCM           80
028600000821     C                   PARM                    PAXKSM          140
028700000821     C                   PARM                    PAXKDM           60
028800000821     C     PAXSTA        IFNE      -1
028900000821     C                   MOVEL     PAXKSM        VIDCLI
029000900517     C                   ELSE
029100900517     C                   MOVE      *ZEROS        VIDCLI
029200900517     C                   END
029300900517     C                   END
029400900523      *----------------------------------
029500900517      * CONTROLLO CLIENTE
029600900517      *
029700900517     C     VIDCLI        IFNE      *BLANKS
029800900517     C     VIDCLI        ANDNE     *ZEROS
029900900517     C     VIDCLI        ANDNE     '8888888'
030000940316     C*
030100900517     C                   MOVE      VIDCLI        CODCLI
030200900522     C                   MOVEL     *BLANKS       DESCLI
030300900517     C     KACO          CHAIN     CNACO00F                           30
030400900523      *--------------
030500940316     C  N30              MOVEL     ACORAG        DESCLI
030600900523      *--------------
030700900517     C   30              MOVEL     *ALL'?'       DESCLI
030800900517     C   30              SETON                                        90
030900900517     C                   END
031000900523      *----------------------------------
031100900517      *
031200900517     C     VIDCLI        IFEQ      '8888888'
031300900517     C                   MOVEL     *BLANKS       DESCLI
031400900525     C                   MOVEL     DES(1)        DESCLI
031500900525     C                   MOVE      VIDCLI        CODCLI
031600900517     C                   END
031700900522      *
031800900522     C     VIDCLI        IFEQ      *BLANKS
031900900522     C     VIDCLI        OREQ      *ZEROS
032000900522     C                   SETON                                        30  90
032100900522     C                   END
032200900517      *
032300900517     C   90              GOTO      FICTR1
032400900517      *
032500900517     C     FICTR1        ENDSR
032600900517      *-----------------------------------------------------***********
032700900517      * CONTROLLI FORMATO 2                                 *  CTR2   *
032800900517      *-----------------------------------------------------***********
032900900517     C     CTR2          BEGSR
033000900517      *
033100900517     C                   SETOFF                                       90
033200900517     C                   SETOFF                                       313233
033300900517     C                   SETOFF                                       34
033400900523     C                   SETOFF                                       7273
033500900523      *--------------------
033600900523     C     VIDSG2        IFNE      0
033700900523     C     VIDSG1        ANDEQ     0
033800900523     C                   SETON                                        72  90
033900900523     C                   END
034000900523     C   90              GOTO      FICTR2
034100900523      *--------------------
034200900523     C     VIDSG3        IFNE      0
034300900523     C     VIDSG2        ANDEQ     0
034400900523     C                   SETON                                        73  90
034500900523     C                   END
034600900523     C   90              GOTO      FICTR2
034700900523      *--------------------
034800900517     C     VIDSG1        IFNE      999
034900900517     C     VIDSG2        ANDNE     999
035000900517     C     VIDSG3        ANDNE     999
035100900517     C                   SETON                                        34  90
035200900517     C                   END
035300900517     C   90              GOTO      FICTR2
035400900517      *--------------------
035500900517     C     VIDSG1        IFEQ      0
035600900517     C     VIDST1        ANDNE     0
035700900517     C                   SETON                                        31  90
035800900517     C                   END
035900900517     C   90              GOTO      FICTR2
036000900517      *
036100900522     C     VIDSGF        IFNE      0
036200900517     C     VIDST1        IFEQ      0
036300900517     C     VIDSG1        ANDNE     0
036400900517     C                   SETON                                        41  90
036500900517     C                   END
036600900522     C                   END
036700900517     C   90              GOTO      FICTR2
036800900517      *--------------------
036900900517     C     VIDSG2        IFEQ      0
037000900517     C     VIDST2        ANDNE     0
037100900517     C                   SETON                                        32  90
037200900517     C                   END
037300900517     C   90              GOTO      FICTR2
037400900517      *
037500900517     C     VIDST2        IFEQ      0
037600900517     C     VIDSG2        ANDNE     0
037700900517     C                   SETON                                        42  90
037800900517     C                   END
037900900517     C   90              GOTO      FICTR2
038000900517      *--------------------
038100900517     C     VIDSG3        IFEQ      0
038200900517     C     VIDST3        ANDNE     0
038300900517     C                   SETON                                        33  90
038400900517     C                   END
038500900517     C   90              GOTO      FICTR2
038600900517      *
038700900517     C     VIDST3        IFEQ      0
038800900517     C     VIDSG3        ANDNE     0
038900900517     C                   SETON                                        43  90
039000900517     C                   END
039100900517     C   90              GOTO      FICTR2
039200900517      *--------------------
039300900522      * GG2 < GG1
039400900522     C     VIDSG2        IFNE      0
039500900522     C     VIDSG2        IFLT      VIDSG1
039600900522     C                   SETON                                        44  90
039700900522     C                   END
039800900522     C                   END
039900900522     C   90              GOTO      FICTR2
040000900522      *--------------------
040100900522      * GG3 < GG2
040200900522     C     VIDSG3        IFNE      0
040300900522     C     VIDSG3        IFLT      VIDSG2
040400900522     C                   SETON                                        45  90
040500900522     C                   END
040600900522     C                   END
040700900522     C   90              GOTO      FICTR2
040800900522      *--------------------
040900900522      * TR2 < TR1
041000900522     C     VIDST2        IFNE      0
041100900522     C     VIDST2        IFLT      VIDST1
041200900522     C                   SETON                                        46  90
041300900522     C                   END
041400900522     C                   END
041500900522     C   90              GOTO      FICTR2
041600900522      *--------------------
041700900522      * TR3 < TR2
041800900522     C     VIDST3        IFNE      0
041900900522     C     VIDST3        IFLT      VIDST2
042000900522     C                   SETON                                        47  90
042100900522     C                   END
042200900522     C                   END
042300900522     C   90              GOTO      FICTR2
042400900522      *--------------------
042500900517      *
042600900517     C     FICTR2        ENDSR
042700900515      *-----------------------------------------------------***********
042800900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
042900900515      *-----------------------------------------------------***********
043000900515     C     RIEMPI        BEGSR
043100900517      *
043200990914     C*
043300990914     C* GIRO DATA ULTIMA VARIAZIONE
043400990914     C*
043500990914     C                   Z-ADD     TGCDUV        G02INV
043600990914     C                   MOVEL     '3'           G02ERR
043700990914     C                   CALL      'XSRDA8'
043800990914     C                   PARM                    WLBDAT
043900990914     C                   Z-ADD     G02DAT        VIDDUV
044000900517     C                   MOVE      TGCKSC        VIDCLI
044100900517     C                   Z-ADD     TGCSGV        VIDSGV
044200940903     C                   Z-ADD     TGCSGR        VIDSGR
044300900517     C                   Z-ADD     TGCSGP        VIDSGP
044400900517     C                   Z-ADD     TGCSGD        VIDSGD
044500900517     C                   Z-ADD     TGCSG1        VIDSG1
044600900517     C                   Z-ADD     TGCSG2        VIDSG2
044700900517     C                   Z-ADD     TGCSG3        VIDSG3
044800900517     C                   Z-ADD     TGCST1        VIDST1
044900900517     C                   Z-ADD     TGCST2        VIDST2
045000900517     C                   Z-ADD     TGCST3        VIDST3
045100900517     C                   Z-ADD     TGCSTM        VIDSTM
045200900517     C                   Z-ADD     TGCRPV        VIDRPV
045300900517     C                   Z-ADD     TGCSGF        VIDSGF
045400911120     C                   Z-ADD     TGCGGR        VIDGGR
045500940903     C                   MOVEL     TGCFAF        VIDFAF
045600940903     C                   MOVEL     TGCTCM        VIDTCM
045700940903     C                   MOVEL     TGCTFG        VIDTFG
045800950106     C*
045900950106     C* SE IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
046000950106     C*   FINE GICENZA SONO VUOTI, LI PROPONGO PRENDENDOLI DAL P.D.C.
046100041029     C**   VIDTFG        IFEQ      ' '
046200041029     C***  VIDTCM        OREQ      ' '
046300950106     C     CODCLI        CHAIN     FNCLS000                           35
046400950106     C     *IN35         IFEQ      *OFF
046500950106     C                   MOVEA     CLSFLO        FLG
046600041029      * valorizzo  il campo di comodo
046700041029     c                   eval      anatcm = '(in anagrafica = ' + vidfl0 +
046800041029     c                             ' )'
046900041029      * valorizzo  il campo di comodo
047000041029     c                   eval      anatfg = '(in anagrafica = ' + vidfl1 +
047100041029     c                             ' )'
047200041029     C**** VIDTFG        IFEQ      ' '
047300041029     C****               MOVEL     VIDFL1        VIDTFG
047400041029     C****               ENDIF
047500041029     C**** VIDTCM        IFEQ      ' '
047600041029     C****               MOVEL     VIDFL0        VIDTCM
047700041029     C****               ENDIF
047800041029
047900950106     C                   ENDIF
048000041029     C****               ENDIF
048100950106     C*
048200950106     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
048300950106     C                   MOVEL     '2F'          CODTAB
048400950106     C                   MOVEL     *BLANKS       KEYTAB
048500950106     C                   MOVEL     VIDTCM        KEYTAB
048600950106     C     KTAB          CHAIN     TABEL                              35
048700950106     C  N35              MOVEL     TBLUNI        DESTCM
048800950106     C*
048900950106     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
049000950106     C                   MOVEL     '2H'          CODTAB
049100950106     C                   MOVEL     *BLANKS       KEYTAB
049200950106     C                   MOVEL     VIDTFG        KEYTAB
049300950106     C     KTAB          CHAIN     TABEL                              35
049400950106     C  N35              MOVEL     TBLUNI        DESTFG
049500950106     C*
049600950106     C* CONTROLLO SE ANNULLATO
049700950105     C     TGCATB        IFNE      ' '
049800900525     C                   SETON                                        50
049900900517     C                   ELSE
050000900525     C                   SETOFF                                       50
050100900517     C                   END
050200900515      *
050300900517     C                   ENDSR
050400920206**
050500900525TARIFFA DI CARTELLO
050600920206**
050700920206*** INTERROGAZIONE TARIFFE ***
