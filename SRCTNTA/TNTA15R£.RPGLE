000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941220      * TNTA15R *----------------------------------------------------*
000300060207      *   - INTERROGAZIONE TARIFFE GIACENZA                          *
000400900515      *--------------------------------------------------------------*
000500941220     FTNTA14D   CF   E             WORKSTN
000600900515     FCNACO00F  IF   E           K DISK
000700990622     FTITGC01L  IF   E           K DISK
000800950106     FFNCLS01L  IF   E           K DISK
000900900523     FTABEL00F  IF   E           K DISK
001000900515      *
001100920206     D TES             S             30    DIM(1) CTDATA PERRCD(1)
001200900517      *
001300950106     D                 DS
001400950106     D  VIDFL1                 1      1
001500950106     D  VIDFL0                10     10
001600950106     D  FLG                    1     10
001700950106     D                                     DIM(10)
001800900514     D KPJBA         E DS
001900900516      *
002000940316     D DS1H          E DS
002100941221     D*
002200941221     D* RICHIAMO A PGM DI STAMPA LETTERA     - FNLV45C -
002300981116     D DSLV45        E DS                  EXTNAME(FNLV45DS)
002400021022     D FNLV41DS      E DS
002500941221     D*
002600941221     D* PARAM PASSATA DA PGM RICHIAMANTI
002700900522     D PARAM           DS
002800940316     D* CODICE CLIENTE
002900940316     D  PASCLI                10     16
003000941221     D* DECODIFICA CODICE TARIFFA
003100941221     D  PASDCT                17     26
003200941221     D* DESCRIZIONE TARIFFA
003300941221     D  PASDCV                27     41
003400990824     D* CODICE DIVISA
003500990824     D  VICDIV                43     45
003600941221     D* CODICE TARIFFA E PROGRESSIVO
003700941221     D  PASCTR               125    127  0
003800941221     D  PASPRG               128    130  0
003900990914      *
004000990914     D WLBDAT          DS
004100990914     D  G02DAT                 1      8  0
004200990914     D  G02INV                 9     16  0
004300990914     D  G02ERR                17     17
004400990914     D  G02TGI                18     22  0
004500020520     D*
004600020520     D tibs34ds      E DS                  inz
004700020520     D ddatiute      e ds
004800020520     D azuteds       e ds                  extname(AZUTE00F)
004900020520     D codut           S                   LIKE(TBLkut) inz(1)
005000941221     I*
005100900515      *---------------------------------------------------------------*
005200000000     C     *ENTRY        PLIST
005300000000     C                   PARM                    KPJBA
005400050317     C                   MOVEL     KPJBU         PARAM
005500020520     c     *dtaara       define    §azute        azuteds
005600020520     c     *dtaara       define    §datiute      ddatiute
005700020520     C                   in(E)     *dtaara
005800020520     C                   IF        %error  or RSUT = *blanks
005900020520     C                   CLEAR                   tibs34ds
006000020520     C                   CALL      'TIBS34R'
006100020520     C                   PARM                    tibs34ds
006200020520     C                   in        *dtaara
006300020520     c                   endif
006400020520     c* N.B.:Dal momento che la call al tbs34r mi serve solo per reperire il
006500020520     c* capoconto clienti italia e la ragione sociale utente non testo la
006600020520     c* presenza di eventuali errori
006700020520     C                   MOVEL(P)  RSUT          PAXDUT           30
006800020919      *
006900020919     C                   MOVEL     SIMFEL        simfelA           3
007000020919      *
007100941221     C* PRENDO LA DATA DEL GIORNO DA TIME
007200941221     C                   TIME                    W0140            14 0
007300941221     C                   MOVE      W0140         UDATE8            8 0
007400900515      *---------------------------------------------------------------*
007500900515      * CHIAVI                                                        *
007600900515      *---------------------------------------------------------------*
007700900515     C     KACO          KLIST
007800900515     C                   KFLD                    CODUT
007900020520     C                   KFLD                    dutkci
008000900517     C                   KFLD                    CODCLI            7 0
008100900517      *
008200950106     C     KTGC          KLIST
008300900517     C                   KFLD                    CODCLI
008400940903     C                   KFLD                    VIDCTR
008500940903     C                   KFLD                    VIDPRG
008600900522      *
008700900522     C     KAZO          KLIST
008800900522     C                   KFLD                    CODFIL            3 0
008900900523      *
009000900523     C     KTAB          KLIST
009100900523     C                   KFLD                    CODUT
009200900523     C                   KFLD                    CODTAB            2
009300900523     C                   KFLD                    KEYTAB            8
009400900515      *---------------------------------------------------------------*
009500900515      * FLUSSO PRINCIPALE                                             *
009600900515      *---------------------------------------------------------------*
009700900515     C                   EXSR      PULIZ
009800940316     C*
009900920206     C                   MOVEL     TES(1)        VIDTES
010000900522     C                   MOVE      PASCLI        VIDCLI
010100940903     C                   MOVE      PASCTR        VIDCTR
010200940903     C                   MOVE      PASPRG        VIDPRG
010300940903     C                   MOVE      PASDCT        DESCTR
010400940903     C                   MOVE      PASDCV        VIDDCV
010500060207     C                   MOVE      VIDCLI        CODCLI
010600900516      *
010700990622     C     KTGC          CHAIN     TITGC01L                           80
010800900517     C  N80              EXSR      RIEMPI
010900900515      *          =============
011000060207     C     FMT01         TAG
011100900515      *          =============
011200060207     C                   EXFMT     TA14D01
011300940316     C* CMD12 - RITORNO
011400060207     C   kl              GOTO      FINE
011500940316     C* CONTROLLI
011600060207     C                   EXSR      CTR1
011700060207     C   90              GOTO      FMT01
011800900522      *
011900060207     C                   GOTO      FINE
012000900517      *
012100000000     C     FINE          TAG
012200000000     C                   SETON                                        LR
012300900515      *-----------------------------------------------------***********
012400900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
012500900515      *-----------------------------------------------------***********
012600900515     C     PULIZ         BEGSR
012700900515      *
012800900517     C                   MOVE      *BLANKS       VIDCLI
012900900517     C                   MOVEL     *BLANKS       DESCLI
013000940903     C                   Z-ADD     0             VIDCTR
013100940903     C                   MOVEL     *BLANKS       DESCTR
013200940903     C                   MOVEL     *BLANKS       VIDDCV
013300990914     C                   Z-ADD     0             VIDDUV
013400940903     C                   Z-ADD     0             VIDPRG
013500900517     C                   Z-ADD     0             VIDSGV
013600940903     C                   Z-ADD     0             VIDSGR
013700900517     C                   Z-ADD     0             VIDSGP
013800900517     C                   Z-ADD     0             VIDSGD
013900900517     C                   Z-ADD     0             VIDSG1
014000900517     C                   Z-ADD     0             VIDSG2
014100900517     C                   Z-ADD     0             VIDSG3
014200900517     C                   Z-ADD     0             VIDST1
014300900517     C                   Z-ADD     0             VIDST2
014400900517     C                   Z-ADD     0             VIDST3
014500900517     C                   Z-ADD     0             VIDSTM
014600900517     C                   Z-ADD     0             VIDRPV
014700900517     C                   Z-ADD     0             VIDSGF
014800911120     C                   Z-ADD     0             VIDGGR
014900940903     C                   MOVEL     ' '           VIDFAF
015000940903     C                   MOVEL     ' '           VIDTCM
015100940903     C                   MOVEL     ' '           VIDTFG
015200940903     C                   CLEAR                   DESTCM
015300940903     C                   CLEAR                   DESTFG
015400900515      *
015500941220     C                   MOVEL     'TNTA15R'     VIDPGM
015600900525     C                   SETON                                        9899
015700900524      *
015800900514     C                   ENDSR
015900900517      *-----------------------------------------------------***********
016000060207      * CONTROLLI FORMATO 1                                 *  CTR1   *
016100900517      *-----------------------------------------------------***********
016200060207     C     CTR1          BEGSR
016300900517      *
016400900517     C                   SETOFF                                       90
016500900517     C                   SETOFF                                       313233
016600900517     C                   SETOFF                                       34
016700900523     C                   SETOFF                                       7273
016800900523      *--------------------
016900900523     C     VIDSG2        IFNE      0
017000900523     C     VIDSG1        ANDEQ     0
017100900523     C                   SETON                                        72  90
017200900523     C                   END
017300060207     C   90              GOTO      FICTR1
017400900523      *--------------------
017500900523     C     VIDSG3        IFNE      0
017600900523     C     VIDSG2        ANDEQ     0
017700900523     C                   SETON                                        73  90
017800900523     C                   END
017900060207     C   90              GOTO      FICTR1
018000900523      *--------------------
018100900517     C     VIDSG1        IFNE      999
018200900517     C     VIDSG2        ANDNE     999
018300900517     C     VIDSG3        ANDNE     999
018400900517     C                   SETON                                        34  90
018500900517     C                   END
018600060207     C   90              GOTO      FICTR1
018700900517      *--------------------
018800900517     C     VIDSG1        IFEQ      0
018900900517     C     VIDST1        ANDNE     0
019000900517     C                   SETON                                        31  90
019100900517     C                   END
019200060207     C   90              GOTO      FICTR1
019300900517      *
019400900522     C     VIDSGF        IFNE      0
019500900517     C     VIDST1        IFEQ      0
019600900517     C     VIDSG1        ANDNE     0
019700900517     C                   SETON                                        41  90
019800900517     C                   END
019900900522     C                   END
020000060207     C   90              GOTO      FICTR1
020100900517      *--------------------
020200900517     C     VIDSG2        IFEQ      0
020300900517     C     VIDST2        ANDNE     0
020400900517     C                   SETON                                        32  90
020500900517     C                   END
020600060207     C   90              GOTO      FICTR1
020700900517      *
020800900517     C     VIDST2        IFEQ      0
020900900517     C     VIDSG2        ANDNE     0
021000900517     C                   SETON                                        42  90
021100900517     C                   END
021200060207     C   90              GOTO      FICTR1
021300900517      *--------------------
021400900517     C     VIDSG3        IFEQ      0
021500900517     C     VIDST3        ANDNE     0
021600900517     C                   SETON                                        33  90
021700900517     C                   END
021800060207     C   90              GOTO      FICTR1
021900900517      *
022000900517     C     VIDST3        IFEQ      0
022100900517     C     VIDSG3        ANDNE     0
022200900517     C                   SETON                                        43  90
022300900517     C                   END
022400060207     C   90              GOTO      FICTR1
022500900517      *--------------------
022600900522      * GG2 < GG1
022700900522     C     VIDSG2        IFNE      0
022800900522     C     VIDSG2        IFLT      VIDSG1
022900900522     C                   SETON                                        44  90
023000900522     C                   END
023100900522     C                   END
023200060207     C   90              GOTO      FICTR1
023300900522      *--------------------
023400900522      * GG3 < GG2
023500900522     C     VIDSG3        IFNE      0
023600900522     C     VIDSG3        IFLT      VIDSG2
023700900522     C                   SETON                                        45  90
023800900522     C                   END
023900900522     C                   END
024000060207     C   90              GOTO      FICTR1
024100900522      *--------------------
024200900522      * TR2 < TR1
024300900522     C     VIDST2        IFNE      0
024400900522     C     VIDST2        IFLT      VIDST1
024500900522     C                   SETON                                        46  90
024600900522     C                   END
024700900522     C                   END
024800060207     C   90              GOTO      FICTR1
024900900522      *--------------------
025000900522      * TR3 < TR2
025100900522     C     VIDST3        IFNE      0
025200900522     C     VIDST3        IFLT      VIDST2
025300900522     C                   SETON                                        47  90
025400900522     C                   END
025500900522     C                   END
025600060207     C   90              GOTO      FICTR1
025700900522      *--------------------
025800900517      *
025900060207     C     FICTR1        ENDSR
026000900515      *-----------------------------------------------------***********
026100900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
026200900515      *-----------------------------------------------------***********
026300900515     C     RIEMPI        BEGSR
026400900517      *
026500060207      * ragione sociale cliente
026600060207     C     KACO          CHAIN     CNaco00F
026700060207     c                   If        %Found(cnaco00f)
026800060207     c                   eval      descli = acorag
026900060207     c                   endif
027000990914     C*
027100990914     C* GIRO DATA ULTIMA VARIAZIONE
027200990914     C*
027300990914     C                   Z-ADD     TGCDUV        G02INV
027400990914     C                   MOVEL     '3'           G02ERR
027500990914     C                   CALL      'XSRDA8'
027600990914     C                   PARM                    WLBDAT
027700990914     C                   Z-ADD     G02DAT        VIDDUV
027800900517     C                   MOVE      TGCKSC        VIDCLI
027900900517     C                   Z-ADD     TGCSGV        VIDSGV
028000940903     C                   Z-ADD     TGCSGR        VIDSGR
028100900517     C                   Z-ADD     TGCSGP        VIDSGP
028200900517     C                   Z-ADD     TGCSGD        VIDSGD
028300900517     C                   Z-ADD     TGCSG1        VIDSG1
028400900517     C                   Z-ADD     TGCSG2        VIDSG2
028500900517     C                   Z-ADD     TGCSG3        VIDSG3
028600900517     C                   Z-ADD     TGCST1        VIDST1
028700900517     C                   Z-ADD     TGCST2        VIDST2
028800900517     C                   Z-ADD     TGCST3        VIDST3
028900900517     C                   Z-ADD     TGCSTM        VIDSTM
029000900517     C                   Z-ADD     TGCRPV        VIDRPV
029100900517     C                   Z-ADD     TGCSGF        VIDSGF
029200911120     C                   Z-ADD     TGCGGR        VIDGGR
029300940903     C                   MOVEL     TGCFAF        VIDFAF
029400940903     C                   MOVEL     TGCTCM        VIDTCM
029500940903     C                   MOVEL     TGCTFG        VIDTFG
029600950106     C*
029700950106     C* SE IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
029800950106     C*   FINE GICENZA SONO VUOTI, LI PROPONGO PRENDENDOLI DAL P.D.C.
029900950106     C     CODCLI        CHAIN     FNCLS000                           35
030000950106     C     *IN35         IFEQ      *OFF
030100950106     C                   MOVEA     CLSFLO        FLG
030200041029      * valorizzo  il campo di comodo
030300041029     c                   eval      anatcm = '(in anagrafica = ' + vidfl0 +
030400041029     c                             ' )'
030500041029      * valorizzo  il campo di comodo
030600041029     c                   eval      anatfg = '(in anagrafica = ' + vidfl1 +
030700041029     c                             ' )'
030800041029
030900950106     C                   ENDIF
031000950106     C*
031100950106     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
031200950106     C                   MOVEL     '2F'          CODTAB
031300950106     C                   MOVEL     *BLANKS       KEYTAB
031400950106     C                   MOVEL     VIDTCM        KEYTAB
031500950106     C     KTAB          CHAIN     TABEL                              35
031600950106     C  N35              MOVEL     TBLUNI        DESTCM
031700950106     C*
031800950106     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
031900950106     C                   MOVEL     '2H'          CODTAB
032000950106     C                   MOVEL     *BLANKS       KEYTAB
032100950106     C                   MOVEL     VIDTFG        KEYTAB
032200950106     C     KTAB          CHAIN     TABEL                              35
032300950106     C  N35              MOVEL     TBLUNI        DESTFG
032400950106     C*
032500950106     C* CONTROLLO SE ANNULLATO
032600950105     C     TGCATB        IFNE      ' '
032700900525     C                   SETON                                        50
032800900517     C                   ELSE
032900900525     C                   SETOFF                                       50
033000900517     C                   END
033100900515      *
033200900517     C                   ENDSR
033300920206**
033400920206*** INTERROGAZIONE TARIFFE ***
