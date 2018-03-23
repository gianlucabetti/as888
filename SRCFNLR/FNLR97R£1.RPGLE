000100000000     H DATEDIT(*DMY)
000200000000      *****************************************************************
000300941228      *  Nome programma:  FNLR97R
000400000000      *  Descrizione   :  Visualizzazione affidabilita'
000500000000      *  Data creazione:  11 NOV 1993
000600000000      *****************************************************************
000700000000      *  PARAMETRI CHE IL PROGRAMMA RICEVE E/O RESTITUISCE
000800000000      *
000900950102      *  R97AP   IN    Indica se la visualizzazione avviene il gestione
001000000000      *                arrivi (= 'A') o partenze (= 'P')
001100950102      *  R97EVE  IN    Se nella bolla vi sono degli eventi abilito il
001200000000      *                tasto funzionale CMD5
001300000000      *****************************************************************
001400000000     FTABEL00F  IF   E           K DISK
001500941228     FFNLR97D   CF   E             WORKSTN
001600970731     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
001700000000      *
001800970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
001900000000     D KPJBA         E DS
002000950102     D DS97          E DS                  EXTNAME(FNLR97)
002100070326     D* DS PER FNLV80R - decodifica cons richiesta
002200070326     D fnlv80ds      E DS
002300950102     D PARA64          DS
002400950102     D* PASSAGGIO DATI A INTERROGAZIONE DETTAGLIO COLLI - FNLS64R -
002500950102     D  P64AAS                 1      4  0
002600950102     D  P64LNP                 5      7  0
002700950102     D  P64NRS                 8      9  0
002800950102     D  P64NSP                10     16  0
002900950102     D  P64F03                17     17
003000950102     D  P64FLG                18     18
003100950102     D  P64PGM                19     28
003200950102     D  P64RSU                29     48
003300950102     D  P64CML                49     49
003400950102      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
003500950102     D  P64CLI                50     50
003600950102     D PARAM1          DS
003700950102     D  PA1AAS                14     17  0
003800950102     D  PA1LNP                18     20  0
003900950102     D  PA1NRS                21     22  0
004000950102     D  PA1NSP                23     29  0
004100950102     D  PA1F03                30     30
004200950102     D  PA1FLG                31     31
004300950102     D  PA1PGM                32     41
004400950102     D  PA1DSA                42     61
004500950102     D  PA1DSP                63     70  0
004600950102     D  PA1RMN                71     85  0
004700950102     D  PA1LNA                86     88  0
004800950102     D  PA1LN1                89    108
004900950102     D  PA1RSD               109    143
005000950102     D  PA1DCP               144    151  0
005100950102     D  PA1TMC               152    152
005200941230     D WLBDAT          DS
005300941230     D  G02DAT                 1      8  0
005400941230     D  G02INV                 9     16  0
005500941230     D  G02ERR                17     17
005600941230     D  G02TGI                18     22  0
005700941230      **************************************************************************
005800000000     C     *ENTRY        PLIST
005900000000     C                   PARM                    KPJBA
006000950102     C                   MOVEL     KPJBU         DS97
006100000000      *
006200000000     C     TABELK        KLIST                                                  TABEL00F
006300000000     C                   KFLD                    TBLKUT
006400000000     C                   KFLD                    TBLCOD
006500000000     C                   KFLD                    TBLKEY
006600000000      *
006700000000      *****************************************************************
006800000000      *           M A I N       L I N E
006900000000      *****************************************************************
007000000000      *
007100000000      * Verifico se vi sono degli eventi da visualizzare
007200950102     C     R97EVE        COMP      *BLANK                             3030      F5=EVENTI
007300000000      *
007400000000      * Eseguo la chiama al programma che imposta i campi video
007500970925     C                   CLEAR                   DS99
007600950102     C                   MOVEL     R97AP         D98TBO                         TIPO BOLLA (P/A)
007700950102     C                   Z-ADD     R97AAS        D98AAS                         ANNO SPEDIZIONE
007800950102     C                   Z-ADD     R97LNP        D98LNP                         FILIALE PART.
007900950102     C                   Z-ADD     R97NRS        D98NRS                         NR.SERIE SPED.
008000950102     C                   Z-ADD     R97NSP        D98NSP                         NR.SPEDIZIONE
008100970925     C                   CALL      'TNSD99R'                            99
008200970925     C                   PARM                    DS99
008300000000      *
008400970731     C   99              MOVEL     MSG(1)        V1CMSG
008500970731     C**
008600970731     C  N99D98ERR        IFNE      '0'
008700970731     C                   MOVEL     D98MSG        V1CMSG
008800970731     C                   SETON                                        99
008900970731     C                   ENDIF
009000000000      *
009100000000     C                   EXSR      DECOD
009200000000      *
009300000000      * Emetto il formato video
009400000000     C     EMETTO        TAG
009500950102     C                   EXFMT     LR97D01
009600950102     C*
009700950102     C* SE PREMUTO F3 LO COMUNICO AL CHIAMANTE
009800950220     C                   CLEAR                   R97F03                         NO USCITA CON F3
009900950102     C   KC              MOVEL     '1'           R97F03
010000950102     C   KC              MOVEL     DS97          KPJBU
010100950102     C   KC              GOTO      FINE
010200000000      *
010300000000      * F5=Eventi
010400000000     C   KE              DO
010500950102     C                   Z-ADD     R97AAS        PA1AAS
010600950102     C                   Z-ADD     R97LNP        PA1LNP
010700950102     C                   Z-ADD     R97NRS        PA1NRS
010800950102     C                   Z-ADD     R97NSP        PA1NSP
010900950102     C                   MOVEL     'FNLR97R'     PA1PGM
011000950102     C                   MOVEL     R97RSU        PA1DSA
011100950102     C                   Z-ADD     R97DSP        PA1DSP
011200950102     C                   Z-ADD     R97RMN        PA1RMN
011300950102     C                   Z-ADD     R97LNA        PA1LNA
011400950102     C                   MOVEL     R97DLA        PA1LN1
011500950102     C                   MOVEL     R97RSD        PA1RSD
011600950102     C                   Z-ADD     R97DCP        PA1DCP
011700950102     C                   MOVEL     R97TMC        PA1TMC
011800950102     C                   MOVEL     PARAM1        KPJBU
011900941228     C                   CALL      'FNLRB5R'
012000000000     C                   PARM                    KPJBA
012100950220     C                   MOVEL     KPJBU         PARAM1
012200950220     C                   MOVEL     PA1F03        R97F03                         PREMUTO F3=FINE
012300950220     C     R97F03        CABNE     *BLANKS       FINE
012400000000     C                   GOTO      EMETTO
012500000000     C                   ENDDO
012600000000      *
012700000000      * F21=Dettaglio segnacolli
012800000000     C   KV              DO
012900000000      *
013000950102     C     R97AP         IFEQ      'A'                                          'A'=ARRIVI
013100950102     C                   CLEAR                   PARAM1
013200950102     C                   Z-ADD     R97AAS        PA1AAS
013300950102     C                   Z-ADD     R97LNP        PA1LNP
013400950102     C                   Z-ADD     R97NRS        PA1NRS
013500950102     C                   Z-ADD     R97NSP        PA1NSP
013600950102     C                   MOVEL     'FNLR97R'     PA1PGM
013700950102     C                   MOVEL     R97RSU        PA1DSA
013800950102     C                   Z-ADD     R97DSP        PA1DSP
013900950102     C                   Z-ADD     R97RMN        PA1RMN
014000950102     C                   Z-ADD     R97LNA        PA1LNA
014100950102     C                   MOVEL     R97DLA        PA1LN1
014200950102     C                   MOVEL     R97RSD        PA1RSD
014300950102     C                   Z-ADD     R97DCP        PA1DCP
014400950102     C                   MOVEL     R97TMC        PA1TMC
014500950102     C                   MOVEL     PARAM1        KPJBU
014600941228     C                   CALL      'FNLR37R'
014700000000     C                   PARM                    KPJBA
014800950220     C                   MOVEL     KPJBU         PARAM1
014900950220     C                   MOVEL     PA1F03        R97F03                         PREMUTO F3=FINE
015000950220     C     R97F03        CABNE     *BLANKS       FINE
015100000000     C                   GOTO      EMETTO
015200000000     C                   ENDIF
015300000000      *
015400950102     C     R97AP         IFEQ      'P'                                          'P'=PARTENZE
015500950102     C                   CLEAR                   PARA64
015600950102     C                   MOVEL     R97AAS        P64AAS
015700950102     C                   MOVEL     R97LNP        P64LNP
015800950102     C                   MOVEL     R97NRS        P64NRS
015900950102     C                   MOVEL     R97NSP        P64NSP
016000950102     C                   MOVEL     'FNLS05R'     P64PGM
016100950102     C                   MOVEL     R97RSU        P64RSU
016200950102     C                   MOVEL     R97CML        P64CML
016300950102     C                   MOVEL     PARA64        KPJBU
016400941230     C                   CALL      'FNLS64R'
016500000000     C                   PARM                    KPJBA
016600950220     C                   MOVEL     KPJBU         PARA64
016700950220     C                   MOVEL     P64F03        R97F03                         PREMUTO F3=FINE
016800950220     C     R97F03        CABNE     *BLANKS       FINE
016900000000     C                   GOTO      EMETTO
017000970731     C                   ENDIF
017100000000      *
017200000000     C                   ENDDO
017300000000      *
017400000000      *
017500950102     C     FINE          TAG
017600000000     C                   SETON                                        LR
017700000000      *****************************************************************
017800000000      * Decodifico i campi a video e inverto le date
017900000000      *****************************************************************
018000000000     C     DECOD         BEGSR
018100950102     C*
018200950102     C                   MOVEL     R97DLA        V1DLNA
018300950102     C                   MOVEL     R97RSD        V1CRSD
018400950102     C*
018500950102     C* DATA 1° CONSEGNA PARZIALE (6 CARATTERI)
018600950102     C                   Z-ADD     *ZERO         V1CDCP
018700950102     C     R97DCP        IFNE      *ZERO
018800950102     C                   MOVEL     R97DCP        V1CDCP
018900950102     C                   MOVE      R97DCP        WDUE                           DUE CIFRE
019000950102     C                   MOVE      WDUE          V1CDCP
019100950102     C                   ENDIF
019200000000      *
019300000000      * Testo se si tratta di espresso
019400950102     C                   MOVEL     D98TSP        V1CTSP
019500950102     C                   MOVE      *BLANKS       V1DTSP
019600941230     C                   Z-ADD     1             TBLKUT
019700941230     C                   MOVEL     '5E'          TBLCOD
019800941230     C                   MOVE      *BLANKS       TBLKEY
019900941230     C                   MOVEL     D98TSP        TBLKEY
020000970731     C     TABELK        CHAIN     TABEL00F                           98
020100970731     C  N98TBLFLG        COMP      *BLANKS                            9898
020200970731     C  N98              MOVEL     TBLUNI        V1DTSP
020300000000      *
020400000000      * Testo se c'e' fermo deposito
020500000000     C                   MOVE      *BLANKS       DEPOSV
020600051107     c                   select
020700051107     c                   when      d98ffd='S'
020800051107     C                   MOVE      'SI '         DEPOSV
020900051107     c                   when      d98ffd='M'
021000051107     C                   MOVE      'SAB'         DEPOSV
021100051107     C                   ENDsl
021200000000      *
021300000000      * Decodifico il tipo mancata consegna (causale evento)
021400000000     C     D98CMC        IFNE      *BLANKS
021500000000     C                   MOVE      D98CMC        MANCAV                         A VIDEO
021600000000     C                   Z-ADD     1             TBLKUT
021700000000     C                   MOVE      '2A'          TBLCOD
021800000000     C                   MOVE      *BLANKS       TBLKEY
021900000000     C                   MOVEL     D98CMC        TBLKEY
022000970731     C     TABELK        CHAIN     TABEL00F                           98
022100970731     C  N98TBLFLG        COMP      *BLANKS                            9898
022200970731     C  N98              MOVEL     TBLUNI        §2ADEV
022300000000      *
022400000000     C                   ELSE
022500000000      *
022600000000      * Se mancata consegna e' vuoto metto il codice lungo 1 della DS
022700950102     C     R97TMC        IFNE      *BLANK
022800950102     C                   MOVEL     R97TMC        MANCAV                         A VIDEO
022900000000     C                   Z-ADD     1             TBLKUT
023000000000     C                   MOVE      '2A'          TBLCOD
023100000000     C                   MOVE      *BLANKS       TBLKEY
023200950102     C                   MOVEL     R97TMC        TBLKEY
023300970731     C     TABELK        CHAIN     TABEL00F                           98
023400970731     C  N98TBLFLG        COMP      *BLANKS                            9898
023500970731     C  N98              MOVEL     TBLUNI        §2ADEV
023600000000     C                   ENDIF
023700000000      *
023800000000     C                   ENDIF
023900000000      *
024000051124      * Decodifico consegna richiesta
024100950102     C                   MOVE      *BLANKS       V1DTCR
024200051124     c                   clear                   w98dcr
024300051124     c                   if        d98dcr>0 or d98hcr>0
024400051124     c                   clear                   fnlv80ds
024500051124     c                   eval      ilv80aas=d98aas
024600051124     c                   eval      ilv80lnp=d98lnp
024700051124     c                   eval      ilv80nrs=d98nrs
024800051124     c                   eval      ilv80nsp=d98nsp
024900051124     c                   eval      ilv80mgs=d98mgs
025000051124     c                   eval      ilv80tc1=d98tc1
025100051124     c                   eval      ilv80tc2=d98tc2
025200051124     c                   eval      ilv80dcr=d98dcr
025300051124     c                   eval      ilv80hcr=d98hcr
025400051124     c                   eval      ilv80tcr=d98tcr
025500051124     c                   call      'FNLV80R'
025600051124     c                   parm                    kpjba
025700051124     c                   parm                    fnlv80ds
025800051124     c
025900051124     c                   eval      w98dcr=olv80dcr6
026000051124     c                   eval      v1dtcr=olv80tcrd
026100051124     c                   endif
026200000000      *
026300000000      * Inverto la data inizio giacenza
026400000000     C     D98DAG        IFNE      *ZERO
026500941230     C                   Z-ADD     D98DAG        G02INV
026600941230     C                   Z-ADD     *ZERO         G02DAT
026700941230     C                   MOVEL     '3'           G02ERR
026800941230     C                   CALL      'XSRDA8'
026900941230     C                   PARM                    WLBDAT
027000950102     C                   MOVEL     G02DAT        W98DAG
027100950102     C                   MOVE      G02DAT        WDUE              2 0          DUE CIFRE
027200950102     C                   MOVE      WDUE          W98DAG
027300000000     C                   ENDIF
027400000000      *
027500000000      * Inverto la data fine giacenza
027600000000     C     D98DCG        IFNE      *ZERO
027700941230     C                   Z-ADD     D98DCG        G02INV
027800941230     C                   Z-ADD     *ZERO         G02DAT
027900941230     C                   MOVEL     '3'           G02ERR
028000941230     C                   CALL      'XSRDA8'
028100941230     C                   PARM                    WLBDAT
028200951109     C                   MOVEL     G02DAT        W98DCG
028300950102     C                   MOVE      G02DAT        WDUE                           DUE CIFRE
028400951109     C                   MOVE      WDUE          W98DCG
028500000000     C                   ENDIF
028600000000      *
028700000000      * Inverto la data spedizione
028800070516     c                   clear                   desdsp
028900000000     C     D98DSP        IFNE      *ZERO
029000941230     C                   Z-ADD     D98DSP        G02INV
029100941230     C                   Z-ADD     *ZERO         G02DAT
029200941230     C                   MOVEL     '3'           G02ERR
029300941230     C                   CALL      'XSRDA8'
029400941230     C                   PARM                    WLBDAT
029500941230     C                   Z-ADD     G02DAT        W98DSP
029600000000     C                   Z-ADD     W98DSP        DASPCV                         SPEDIZ.CLIENTE
029700070516     c                   eval      desdsp='Data Spedizione'
029800000000     C                   ENDIF
029900070516     c
030000070516     c* Inverto la data ritiro se serve per il calcolo
030100070516     c                   if        d98fdrt='S' and d98dbr>0
030200070516     C                   Z-ADD     D98dbr        G02INV
030300070516     C                   Z-ADD     *ZERO         G02DAT
030400070516     C                   MOVEL     '3'           G02ERR
030500070516     C                   CALL      'XSRDA8'
030600070516     C                   PARM                    WLBDAT
030700070516     C                   Z-ADD     G02DAT        W98DSP
030800070516     c                   eval      desdsp='Dt Ritiro Merce'
030900070516     C                   ENDIF
031000000000      *
031100000000      * Inverto la data prevista consegna cliente
031200000000     C     D98DCE        IFNE      *ZERO
031300941230     C                   Z-ADD     D98DCE        G02INV
031400941230     C                   Z-ADD     *ZERO         G02DAT
031500941230     C                   MOVEL     '3'           G02ERR
031600941230     C                   CALL      'XSRDA8'
031700941230     C                   PARM                    WLBDAT
031800941230     C                   Z-ADD     G02DAT        W98DCE
031900000000     C                   ENDIF
032000000000      *
032100000000      * Inverto la data consegna merce
032200000000     C     D98DCM        IFNE      *ZERO
032300941230     C                   Z-ADD     D98DCM        G02INV
032400941230     C                   Z-ADD     *ZERO         G02DAT
032500941230     C                   MOVEL     '3'           G02ERR
032600941230     C                   CALL      'XSRDA8'
032700941230     C                   PARM                    WLBDAT
032800941230     C                   Z-ADD     G02DAT        W98DCM
032900000000     C                   ENDIF
033000000000      *
033100000000      * Inverto la data arrivo reale trazione
033200000000     C     D98DTI        IFNE      *ZERO
033300941230     C                   Z-ADD     D98DTI        G02INV
033400941230     C                   Z-ADD     *ZERO         G02DAT
033500941230     C                   MOVEL     '3'           G02ERR
033600941230     C                   CALL      'XSRDA8'
033700941230     C                   PARM                    WLBDAT
033800941230     C                   Z-ADD     G02DAT        W98DTI
033900000000     C                   ENDIF
034000000000      *
034100000000      * Inverto la data prevista consegna interna
034200000000     C     D98DCI        IFNE      *ZERO
034300941230     C                   Z-ADD     D98DCI        G02INV
034400941230     C                   Z-ADD     *ZERO         G02DAT
034500941230     C                   MOVEL     '3'           G02ERR
034600941230     C                   CALL      'XSRDA8'
034700941230     C                   PARM                    WLBDAT
034800941230     C                   Z-ADD     G02DAT        W98DCI
034900000000     C                   ENDIF
035000000000      *
035100000000     C                   ENDSR
035200970731**
035300970731Programma in errore: TELEFONARE IN SEDE!!!
