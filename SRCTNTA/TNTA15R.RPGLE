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
001900941221     D*
002000941221     D* PARAM PASSATA DA PGM RICHIAMANTI
002100900522     D PARAM           DS
002200940316     D* CODICE CLIENTE
002300940316     D  PASCLI                10     16
002400941221     D* DECODIFICA CODICE TARIFFA
002500941221     D  PASDCT                17     26
002600941221     D* DESCRIZIONE TARIFFA
002700941221     D  PASDCV                27     41
002800990824     D* CODICE DIVISA
002900990824     D  VICDIV                43     45
003000941221     D* CODICE TARIFFA E PROGRESSIVO
003100941221     D  PASCTR               125    127  0
003200941221     D  PASPRG               128    130  0
003300990914      *
003400990914     D WLBDAT          DS
003500990914     D  G02DAT                 1      8  0
003600990914     D  G02INV                 9     16  0
003700990914     D  G02ERR                17     17
003800990914     D  G02TGI                18     22  0
003900020520     D*
004000020520     D tibs34ds      E DS                  inz
004100020520     D ddatiute      e ds
004200020520     D azuteds       e ds                  extname(AZUTE00F)
004300020520     D codut           S                   LIKE(TBLkut) inz(1)
004400941221     I*
004500900515      *---------------------------------------------------------------*
004600000000     C     *ENTRY        PLIST
004700000000     C                   PARM                    KPJBA
004800050317     C                   MOVEL     KPJBU         PARAM
004900020520     c     *dtaara       define    §azute        azuteds
005000020520     c     *dtaara       define    §datiute      ddatiute
005100020520     C                   in(E)     *dtaara
005200020520     C                   IF        %error  or RSUT = *blanks
005300020520     C                   CLEAR                   tibs34ds
005400020520     C                   CALL      'TIBS34R'
005500020520     C                   PARM                    tibs34ds
005600020520     C                   in        *dtaara
005700020520     c                   endif
005800020520     c* N.B.:Dal momento che la call al tbs34r mi serve solo per reperire il
005900020520     c* capoconto clienti italia e la ragione sociale utente non testo la
006000020520     c* presenza di eventuali errori
006100020520     C                   MOVEL(P)  RSUT          PAXDUT           30
006200020919      *
006300020919     C                   MOVEL     SIMFEL        simfelA           3
006400020919      *
006500941221     C* PRENDO LA DATA DEL GIORNO DA TIME
006600941221     C                   TIME                    W0140            14 0
006700941221     C                   MOVE      W0140         UDATE8            8 0
006800900515      *---------------------------------------------------------------*
006900900515      * CHIAVI                                                        *
007000900515      *---------------------------------------------------------------*
007100900515     C     KACO          KLIST
007200900515     C                   KFLD                    CODUT
007300020520     C                   KFLD                    dutkci
007400900517     C                   KFLD                    CODCLI            7 0
007500900517      *
007600950106     C     KTGC          KLIST
007700900517     C                   KFLD                    CODCLI
007800940903     C                   KFLD                    VIDCTR
007900940903     C                   KFLD                    VIDPRG
008000900522      *
008100900522     C     KAZO          KLIST
008200900522     C                   KFLD                    CODFIL            3 0
008300900523      *
008400900523     C     KTAB          KLIST
008500900523     C                   KFLD                    CODUT
008600900523     C                   KFLD                    CODTAB            2
008700900523     C                   KFLD                    KEYTAB            8
008800900515      *---------------------------------------------------------------*
008900900515      * FLUSSO PRINCIPALE                                             *
009000900515      *---------------------------------------------------------------*
009100900515     C                   EXSR      PULIZ
009200940316     C*
009300920206     C                   MOVEL     TES(1)        VIDTES
009400900522     C                   MOVE      PASCLI        VIDCLI
009500940903     C                   MOVE      PASCTR        VIDCTR
009600940903     C                   MOVE      PASPRG        VIDPRG
009700940903     C                   MOVE      PASDCT        DESCTR
009800940903     C                   MOVE      PASDCV        VIDDCV
009900060207     C                   MOVE      VIDCLI        CODCLI
010000900516      *
010100990622     C     KTGC          CHAIN     TITGC01L                           80
010200900517     C  N80              EXSR      RIEMPI
010300900515      *          =============
010400060207     C     FMT01         TAG
010500900515      *          =============
010600060207     C                   EXFMT     TA14D01
010700940316     C* CMD12 - RITORNO
010800060207     C   kl              GOTO      FINE
010900940316     C* CONTROLLI
011000060207     C                   EXSR      CTR1
011100060207     C   90              GOTO      FMT01
011200900522      *
011300060207     C                   GOTO      FINE
011400900517      *
011500000000     C     FINE          TAG
011600000000     C                   SETON                                        LR
011700900515      *-----------------------------------------------------***********
011800900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
011900900515      *-----------------------------------------------------***********
012000900515     C     PULIZ         BEGSR
012100900515      *
012200900517     C                   MOVE      *BLANKS       VIDCLI
012300900517     C                   MOVEL     *BLANKS       DESCLI
012400940903     C                   Z-ADD     0             VIDCTR
012500940903     C                   MOVEL     *BLANKS       DESCTR
012600940903     C                   MOVEL     *BLANKS       VIDDCV
012700990914     C                   Z-ADD     0             VIDDUV
012800940903     C                   Z-ADD     0             VIDPRG
012900900517     C                   Z-ADD     0             VIDSGV
013000940903     C                   Z-ADD     0             VIDSGR
013100900517     C                   Z-ADD     0             VIDSGP
013200900517     C                   Z-ADD     0             VIDSGD
013300900517     C                   Z-ADD     0             VIDSG1
013400900517     C                   Z-ADD     0             VIDSG2
013500900517     C                   Z-ADD     0             VIDSG3
013600900517     C                   Z-ADD     0             VIDST1
013700900517     C                   Z-ADD     0             VIDST2
013800900517     C                   Z-ADD     0             VIDST3
013900900517     C                   Z-ADD     0             VIDSTM
014000900517     C                   Z-ADD     0             VIDRPV
014100900517     C                   Z-ADD     0             VIDSGF
014200911120     C                   Z-ADD     0             VIDGGR
014300940903     C                   MOVEL     ' '           VIDFAF
014400940903     C                   MOVEL     ' '           VIDTCM
014500940903     C                   MOVEL     ' '           VIDTFG
014600940903     C                   CLEAR                   DESTCM
014700940903     C                   CLEAR                   DESTFG
014800900515      *
014900941220     C                   MOVEL     'TNTA15R'     VIDPGM
015000900525     C                   SETON                                        9899
015100900524      *
015200900514     C                   ENDSR
015300900517      *-----------------------------------------------------***********
015400060207      * CONTROLLI FORMATO 1                                 *  CTR1   *
015500900517      *-----------------------------------------------------***********
015600060207     C     CTR1          BEGSR
015700900517      *
015800900517     C                   SETOFF                                       90
015900900517     C                   SETOFF                                       313233
016000900517     C                   SETOFF                                       34
016100900523     C                   SETOFF                                       7273
016200900523      *--------------------
016300900523     C     VIDSG2        IFNE      0
016400900523     C     VIDSG1        ANDEQ     0
016500900523     C                   SETON                                        72  90
016600900523     C                   END
016700060207     C   90              GOTO      FICTR1
016800900523      *--------------------
016900900523     C     VIDSG3        IFNE      0
017000900523     C     VIDSG2        ANDEQ     0
017100900523     C                   SETON                                        73  90
017200900523     C                   END
017300060207     C   90              GOTO      FICTR1
017400900523      *--------------------
017500900517     C     VIDSG1        IFNE      999
017600900517     C     VIDSG2        ANDNE     999
017700900517     C     VIDSG3        ANDNE     999
017800900517     C                   SETON                                        34  90
017900900517     C                   END
018000060207     C   90              GOTO      FICTR1
018100900517      *--------------------
018200900517     C     VIDSG1        IFEQ      0
018300900517     C     VIDST1        ANDNE     0
018400900517     C                   SETON                                        31  90
018500900517     C                   END
018600060207     C   90              GOTO      FICTR1
018700900517      *
018800900522     C     VIDSGF        IFNE      0
018900900517     C     VIDST1        IFEQ      0
019000900517     C     VIDSG1        ANDNE     0
019100900517     C                   SETON                                        41  90
019200900517     C                   END
019300900522     C                   END
019400060207     C   90              GOTO      FICTR1
019500900517      *--------------------
019600900517     C     VIDSG2        IFEQ      0
019700900517     C     VIDST2        ANDNE     0
019800900517     C                   SETON                                        32  90
019900900517     C                   END
020000060207     C   90              GOTO      FICTR1
020100900517      *
020200900517     C     VIDST2        IFEQ      0
020300900517     C     VIDSG2        ANDNE     0
020400900517     C                   SETON                                        42  90
020500900517     C                   END
020600060207     C   90              GOTO      FICTR1
020700900517      *--------------------
020800900517     C     VIDSG3        IFEQ      0
020900900517     C     VIDST3        ANDNE     0
021000900517     C                   SETON                                        33  90
021100900517     C                   END
021200060207     C   90              GOTO      FICTR1
021300900517      *
021400900517     C     VIDST3        IFEQ      0
021500900517     C     VIDSG3        ANDNE     0
021600900517     C                   SETON                                        43  90
021700900517     C                   END
021800060207     C   90              GOTO      FICTR1
021900900517      *--------------------
022000900522      * GG2 < GG1
022100900522     C     VIDSG2        IFNE      0
022200900522     C     VIDSG2        IFLT      VIDSG1
022300900522     C                   SETON                                        44  90
022400900522     C                   END
022500900522     C                   END
022600060207     C   90              GOTO      FICTR1
022700900522      *--------------------
022800900522      * GG3 < GG2
022900900522     C     VIDSG3        IFNE      0
023000900522     C     VIDSG3        IFLT      VIDSG2
023100900522     C                   SETON                                        45  90
023200900522     C                   END
023300900522     C                   END
023400060207     C   90              GOTO      FICTR1
023500900522      *--------------------
023600900522      * TR2 < TR1
023700900522     C     VIDST2        IFNE      0
023800900522     C     VIDST2        IFLT      VIDST1
023900900522     C                   SETON                                        46  90
024000900522     C                   END
024100900522     C                   END
024200060207     C   90              GOTO      FICTR1
024300900522      *--------------------
024400900522      * TR3 < TR2
024500900522     C     VIDST3        IFNE      0
024600900522     C     VIDST3        IFLT      VIDST2
024700900522     C                   SETON                                        47  90
024800900522     C                   END
024900900522     C                   END
025000060207     C   90              GOTO      FICTR1
025100900522      *--------------------
025200900517      *
025300060207     C     FICTR1        ENDSR
025400900515      *-----------------------------------------------------***********
025500900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
025600900515      *-----------------------------------------------------***********
025700900515     C     RIEMPI        BEGSR
025800900517      *
025900060207      * ragione sociale cliente
026000060207     C     KACO          CHAIN     CNaco00F
026100060207     c                   If        %Found(cnaco00f)
026200060207     c                   eval      descli = acorag
026300060207     c                   endif
026400990914     C*
026500990914     C* GIRO DATA ULTIMA VARIAZIONE
026600990914     C*
026700990914     C                   Z-ADD     TGCDUV        G02INV
026800990914     C                   MOVEL     '3'           G02ERR
026900990914     C                   CALL      'XSRDA8'
027000990914     C                   PARM                    WLBDAT
027100990914     C                   Z-ADD     G02DAT        VIDDUV
027200900517     C                   MOVE      TGCKSC        VIDCLI
027300900517     C                   Z-ADD     TGCSGV        VIDSGV
027400940903     C                   Z-ADD     TGCSGR        VIDSGR
027500900517     C                   Z-ADD     TGCSGP        VIDSGP
027600900517     C                   Z-ADD     TGCSGD        VIDSGD
027700900517     C                   Z-ADD     TGCSG1        VIDSG1
027800900517     C                   Z-ADD     TGCSG2        VIDSG2
027900900517     C                   Z-ADD     TGCSG3        VIDSG3
028000900517     C                   Z-ADD     TGCST1        VIDST1
028100900517     C                   Z-ADD     TGCST2        VIDST2
028200900517     C                   Z-ADD     TGCST3        VIDST3
028300900517     C                   Z-ADD     TGCSTM        VIDSTM
028400900517     C                   Z-ADD     TGCRPV        VIDRPV
028500900517     C                   Z-ADD     TGCSGF        VIDSGF
028600911120     C                   Z-ADD     TGCGGR        VIDGGR
028700940903     C                   MOVEL     TGCFAF        VIDFAF
028800940903     C                   MOVEL     TGCTCM        VIDTCM
028900940903     C                   MOVEL     TGCTFG        VIDTFG
029000950106     C*
029100950106     C* SE IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
029200950106     C*   FINE GICENZA SONO VUOTI, LI PROPONGO PRENDENDOLI DAL P.D.C.
029300950106     C     CODCLI        CHAIN     FNCLS000                           35
029400950106     C     *IN35         IFEQ      *OFF
029500950106     C                   MOVEA     CLSFLO        FLG
029600041029      * valorizzo  il campo di comodo
029700041029     c                   eval      anatcm = '(in anagrafica = ' + vidfl0 +
029800041029     c                             ' )'
029900041029      * valorizzo  il campo di comodo
030000041029     c                   eval      anatfg = '(in anagrafica = ' + vidfl1 +
030100041029     c                             ' )'
030200041029
030300950106     C                   ENDIF
030400950106     C*
030500950106     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
030600950106     C                   MOVEL     '2F'          CODTAB
030700950106     C                   MOVEL     *BLANKS       KEYTAB
030800950106     C                   MOVEL     VIDTCM        KEYTAB
030900950106     C     KTAB          CHAIN     TABEL                              35
031000950106     C  N35              MOVEL     TBLUNI        DESTCM
031100950106     C*
031200950106     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
031300950106     C                   MOVEL     '2H'          CODTAB
031400950106     C                   MOVEL     *BLANKS       KEYTAB
031500950106     C                   MOVEL     VIDTFG        KEYTAB
031600950106     C     KTAB          CHAIN     TABEL                              35
031700950106     C  N35              MOVEL     TBLUNI        DESTFG
031800950106     C*
031900950106     C* CONTROLLO SE ANNULLATO
032000950105     C     TGCATB        IFNE      ' '
032100900525     C                   SETON                                        50
032200900517     C                   ELSE
032300900525     C                   SETOFF                                       50
032400900517     C                   END
032500900515      *
032600900517     C                   ENDSR
032700920206**
032800920206*** INTERROGAZIONE TARIFFE ***
