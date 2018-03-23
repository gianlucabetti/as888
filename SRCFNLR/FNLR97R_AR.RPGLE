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
002300950102     D PARAM1          DS
002400950102     D  PA1AAS                14     17  0
002500950102     D  PA1LNP                18     20  0
002600950102     D  PA1NRS                21     22  0
002700950102     D  PA1NSP                23     29  0
002800950102     D  PA1F03                30     30
002900950102     D  PA1FLG                31     31
003000950102     D  PA1PGM                32     41
003100950102     D  PA1DSA                42     61
003200950102     D  PA1DSP                63     70  0
003300950102     D  PA1RMN                71     85  0
003400950102     D  PA1LNA                86     88  0
003500950102     D  PA1LN1                89    108
003600090227     D  PA1RSc               109    143
003700090227     D  pa1DCP               144    151  0
003800090227     D  pa1TMC               152    152
003900090227     D  PA1TBO               153    153
004000090227     D  PA1cod               154    160  0
004100941230     D WLBDAT          DS
004200941230     D  G02DAT                 1      8  0
004300941230     D  G02INV                 9     16  0
004400941230     D  G02ERR                17     17
004500941230     D  G02TGI                18     22  0
004600941230      **************************************************************************
004700000000     C     *ENTRY        PLIST
004800000000     C                   PARM                    KPJBA
004900950102     C                   MOVEL     KPJBU         DS97
005000000000      *
005100000000     C     TABELK        KLIST                                                  TABEL00F
005200000000     C                   KFLD                    TBLKUT
005300000000     C                   KFLD                    TBLCOD
005400000000     C                   KFLD                    TBLKEY
005500000000      *
005600000000      *****************************************************************
005700000000      *           M A I N       L I N E
005800000000      *****************************************************************
005900000000      *
006000000000      * Verifico se vi sono degli eventi da visualizzare
006100950102     C     R97EVE        COMP      *BLANK                             3030      F5=EVENTI
006200000000      *
006300000000      * Eseguo la chiama al programma che imposta i campi video
006400970925     C                   CLEAR                   DS99
006500950102     C                   MOVEL     R97AP         D98TBO                         TIPO BOLLA (P/A)
006600950102     C                   Z-ADD     R97AAS        D98AAS                         ANNO SPEDIZIONE
006700950102     C                   Z-ADD     R97LNP        D98LNP                         FILIALE PART.
006800950102     C                   Z-ADD     R97NRS        D98NRS                         NR.SERIE SPED.
006900950102     C                   Z-ADD     R97NSP        D98NSP                         NR.SPEDIZIONE
007000970925     C                   CALL      'TNSD99R'                            99
007100970925     C                   PARM                    DS99
007200000000      *
007300970731     C   99              MOVEL     MSG(1)        V1CMSG
007400970731     C**
007500970731     C  N99D98ERR        IFNE      '0'
007600970731     C                   MOVEL     D98MSG        V1CMSG
007700970731     C                   SETON                                        99
007800970731     C                   ENDIF
007900000000      *
008000000000     C                   EXSR      DECOD
008100000000      *
008200000000      * Emetto il formato video
008300000000     C     EMETTO        TAG
008400950102     C                   EXFMT     LR97D01
008500950102     C*
008600950102     C* SE PREMUTO F3 LO COMUNICO AL CHIAMANTE
008700950220     C                   CLEAR                   R97F03                         NO USCITA CON F3
008800950102     C   KC              MOVEL     '1'           R97F03
008900950102     C   KC              MOVEL     DS97          KPJBU
009000950102     C   KC              GOTO      FINE
009100000000      *
009200000000      * F5=Eventi
009300000000     C   KE              DO
009400950102     C                   Z-ADD     R97AAS        PA1AAS
009500950102     C                   Z-ADD     R97LNP        PA1LNP
009600950102     C                   Z-ADD     R97NRS        PA1NRS
009700950102     C                   Z-ADD     R97NSP        PA1NSP
009800950102     C                   MOVEL     'FNLR97R'     PA1PGM
009900950102     C                   MOVEL     R97RSU        PA1DSA
010000950102     C                   Z-ADD     R97DSP        PA1DSP
010100950102     C                   Z-ADD     R97RMN        PA1RMN
010200950102     C                   Z-ADD     R97LNA        PA1LNA
010300950102     C                   MOVEL     R97DLA        PA1LN1
010400090227     C                   MOVEL     R97RSD        PA1RSc
010500950102     C                   Z-ADD     R97DCP        PA1DCP
010600950102     C                   MOVEL     R97TMC        PA1TMC
010700950102     C                   MOVEL     PARAM1        KPJBU
010800941228     C                   CALL      'FNLRB5R'
010900000000     C                   PARM                    KPJBA
011000950220     C                   MOVEL     KPJBU         PARAM1
011100950220     C                   MOVEL     PA1F03        R97F03                         PREMUTO F3=FINE
011200950220     C     R97F03        CABNE     *BLANKS       FINE
011300000000     C                   GOTO      EMETTO
011400000000     C                   ENDDO
011500000000      *
011600000000      * F21=Dettaglio segnacolli
011700000000     C   KV              DO
011800000000      *
011900950102     C                   CLEAR                   PARAM1
012000090227     c                   movel     r97ap         pa1tbo
012100950102     C                   Z-ADD     R97AAS        PA1AAS
012200950102     C                   Z-ADD     R97LNP        PA1LNP
012300950102     C                   Z-ADD     R97NRS        PA1NRS
012400950102     C                   Z-ADD     R97NSP        PA1NSP
012500950102     C                   MOVEL     'FNLR97R'     PA1PGM
012600950102     C                   MOVEL     R97RSU        PA1DSA
012700950102     C                   Z-ADD     R97DSP        PA1DSP
012800950102     C                   Z-ADD     R97RMN        PA1RMN
012900950102     C                   Z-ADD     R97LNA        PA1LNA
013000950102     C                   MOVEL     R97DLA        PA1LN1
013100090227     c*
013200090227     c                   if        pa1tbo='A'
013300090227     C                   MOVEL     R97RSD        PA1RSc
013400090227     c                   else
013500090227     C                   MOVEL     R97RSm        PA1RSc
013600090227     C                   MOVEL     R97ccm        PA1cod
013700090227     c                   endif
013800950102     C                   MOVEL     PARAM1        KPJBU
013900941228     C                   CALL      'FNLR37R'
014000000000     C                   PARM                    KPJBA
014100950220     C                   MOVEL     KPJBU         PARAM1
014200950220     C                   MOVEL     PA1F03        R97F03                         PREMUTO F3=FINE
014300950220     C     R97F03        CABNE     *BLANKS       FINE
014400000000     C                   GOTO      EMETTO
014500000000      *
014600000000     C                   ENDDO
014700000000      *
014800000000      *
014900950102     C     FINE          TAG
015000000000     C                   SETON                                        LR
015100000000      *****************************************************************
015200000000      * Decodifico i campi a video e inverto le date
015300000000      *****************************************************************
015400000000     C     DECOD         BEGSR
015500950102     C*
015600950102     C                   MOVEL     R97DLA        V1DLNA
015700950102     C                   MOVEL     R97RSD        V1CRSD
015800950102     C*
015900950102     C* DATA 1° CONSEGNA PARZIALE (6 CARATTERI)
016000950102     C                   Z-ADD     *ZERO         V1CDCP
016100950102     C     R97DCP        IFNE      *ZERO
016200950102     C                   MOVEL     R97DCP        V1CDCP
016300950102     C                   MOVE      R97DCP        WDUE                           DUE CIFRE
016400950102     C                   MOVE      WDUE          V1CDCP
016500950102     C                   ENDIF
016600000000      *
016700000000      * Testo se si tratta di espresso
016800950102     C                   MOVEL     D98TSP        V1CTSP
016900950102     C                   MOVE      *BLANKS       V1DTSP
017000941230     C                   Z-ADD     1             TBLKUT
017100941230     C                   MOVEL     '5E'          TBLCOD
017200941230     C                   MOVE      *BLANKS       TBLKEY
017300941230     C                   MOVEL     D98TSP        TBLKEY
017400970731     C     TABELK        CHAIN     TABEL00F                           98
017500970731     C  N98TBLFLG        COMP      *BLANKS                            9898
017600970731     C  N98              MOVEL     TBLUNI        V1DTSP
017700000000      *
017800000000      * Testo se c'e' fermo deposito
017900000000     C                   MOVE      *BLANKS       DEPOSV
018000051107     c                   select
018100051107     c                   when      d98ffd='S'
018200051107     C                   MOVE      'SI '         DEPOSV
018300051107     c                   when      d98ffd='M'
018400051107     C                   MOVE      'SAB'         DEPOSV
018500051107     C                   ENDsl
018600000000      *
018700000000      * Decodifico il tipo mancata consegna (causale evento)
018800000000     C     D98CMC        IFNE      *BLANKS
018900000000     C                   MOVE      D98CMC        MANCAV                         A VIDEO
019000000000     C                   Z-ADD     1             TBLKUT
019100000000     C                   MOVE      '2A'          TBLCOD
019200000000     C                   MOVE      *BLANKS       TBLKEY
019300000000     C                   MOVEL     D98CMC        TBLKEY
019400970731     C     TABELK        CHAIN     TABEL00F                           98
019500970731     C  N98TBLFLG        COMP      *BLANKS                            9898
019600970731     C  N98              MOVEL     TBLUNI        §2ADEV
019700000000      *
019800000000     C                   ELSE
019900000000      *
020000000000      * Se mancata consegna e' vuoto metto il codice lungo 1 della DS
020100950102     C     R97TMC        IFNE      *BLANK
020200950102     C                   MOVEL     R97TMC        MANCAV                         A VIDEO
020300000000     C                   Z-ADD     1             TBLKUT
020400000000     C                   MOVE      '2A'          TBLCOD
020500000000     C                   MOVE      *BLANKS       TBLKEY
020600950102     C                   MOVEL     R97TMC        TBLKEY
020700970731     C     TABELK        CHAIN     TABEL00F                           98
020800970731     C  N98TBLFLG        COMP      *BLANKS                            9898
020900970731     C  N98              MOVEL     TBLUNI        §2ADEV
021000000000     C                   ENDIF
021100000000      *
021200000000     C                   ENDIF
021300000000      *
021400051124      * Decodifico consegna richiesta
021500950102     C                   MOVE      *BLANKS       V1DTCR
021600051124     c                   clear                   w98dcr
021700051124     c                   if        d98dcr>0 or d98hcr>0
021800051124     c                   clear                   fnlv80ds
021900051124     c                   eval      ilv80aas=d98aas
022000051124     c                   eval      ilv80lnp=d98lnp
022100051124     c                   eval      ilv80nrs=d98nrs
022200051124     c                   eval      ilv80nsp=d98nsp
022300051124     c                   eval      ilv80mgs=d98mgs
022400051124     c                   eval      ilv80tc1=d98tc1
022500051124     c                   eval      ilv80tc2=d98tc2
022600051124     c                   eval      ilv80dcr=d98dcr
022700051124     c                   eval      ilv80hcr=d98hcr
022800051124     c                   eval      ilv80tcr=d98tcr
022900051124     c                   call      'FNLV80R'
023000051124     c                   parm                    kpjba
023100051124     c                   parm                    fnlv80ds
023200051124     c
023300051124     c                   eval      w98dcr=olv80dcr6
023400051124     c                   eval      v1dtcr=olv80tcrd
023500051124     c                   endif
023600000000      *
023700000000      * Inverto la data inizio giacenza
023800000000     C     D98DAG        IFNE      *ZERO
023900941230     C                   Z-ADD     D98DAG        G02INV
024000941230     C                   Z-ADD     *ZERO         G02DAT
024100941230     C                   MOVEL     '3'           G02ERR
024200941230     C                   CALL      'XSRDA8'
024300941230     C                   PARM                    WLBDAT
024400950102     C                   MOVEL     G02DAT        W98DAG
024500950102     C                   MOVE      G02DAT        WDUE              2 0          DUE CIFRE
024600950102     C                   MOVE      WDUE          W98DAG
024700000000     C                   ENDIF
024800000000      *
024900000000      * Inverto la data fine giacenza
025000000000     C     D98DCG        IFNE      *ZERO
025100941230     C                   Z-ADD     D98DCG        G02INV
025200941230     C                   Z-ADD     *ZERO         G02DAT
025300941230     C                   MOVEL     '3'           G02ERR
025400941230     C                   CALL      'XSRDA8'
025500941230     C                   PARM                    WLBDAT
025600951109     C                   MOVEL     G02DAT        W98DCG
025700950102     C                   MOVE      G02DAT        WDUE                           DUE CIFRE
025800951109     C                   MOVE      WDUE          W98DCG
025900000000     C                   ENDIF
026000000000      *
026100000000      * Inverto la data spedizione
026200070516     c                   clear                   desdsp
026300000000     C     D98DSP        IFNE      *ZERO
026400941230     C                   Z-ADD     D98DSP        G02INV
026500941230     C                   Z-ADD     *ZERO         G02DAT
026600941230     C                   MOVEL     '3'           G02ERR
026700941230     C                   CALL      'XSRDA8'
026800941230     C                   PARM                    WLBDAT
026900941230     C                   Z-ADD     G02DAT        W98DSP
027000000000     C                   Z-ADD     W98DSP        DASPCV                         SPEDIZ.CLIENTE
027100070516     c                   eval      desdsp='Data Spedizione'
027200000000     C                   ENDIF
027300070516     c
027400070516     c* Inverto la data ritiro se serve per il calcolo
027500070516     c                   if        d98fdrt='S' and d98dbr>0
027600070516     C                   Z-ADD     D98dbr        G02INV
027700070516     C                   Z-ADD     *ZERO         G02DAT
027800070516     C                   MOVEL     '3'           G02ERR
027900070516     C                   CALL      'XSRDA8'
028000070516     C                   PARM                    WLBDAT
028100070516     C                   Z-ADD     G02DAT        W98DSP
028200070516     c                   eval      desdsp='Dt Ritiro Merce'
028300070516     C                   ENDIF
028400000000      *
028500000000      * Inverto la data prevista consegna cliente
028600000000     C     D98DCE        IFNE      *ZERO
028700941230     C                   Z-ADD     D98DCE        G02INV
028800941230     C                   Z-ADD     *ZERO         G02DAT
028900941230     C                   MOVEL     '3'           G02ERR
029000941230     C                   CALL      'XSRDA8'
029100941230     C                   PARM                    WLBDAT
029200941230     C                   Z-ADD     G02DAT        W98DCE
029300000000     C                   ENDIF
029400000000      *
029500000000      * Inverto la data consegna merce
029600000000     C     D98DCM        IFNE      *ZERO
029700941230     C                   Z-ADD     D98DCM        G02INV
029800941230     C                   Z-ADD     *ZERO         G02DAT
029900941230     C                   MOVEL     '3'           G02ERR
030000941230     C                   CALL      'XSRDA8'
030100941230     C                   PARM                    WLBDAT
030200941230     C                   Z-ADD     G02DAT        W98DCM
030300000000     C                   ENDIF
030400000000      *
030500000000      * Inverto la data arrivo reale trazione
030600000000     C     D98DTI        IFNE      *ZERO
030700941230     C                   Z-ADD     D98DTI        G02INV
030800941230     C                   Z-ADD     *ZERO         G02DAT
030900941230     C                   MOVEL     '3'           G02ERR
031000941230     C                   CALL      'XSRDA8'
031100941230     C                   PARM                    WLBDAT
031200941230     C                   Z-ADD     G02DAT        W98DTI
031300000000     C                   ENDIF
031400000000      *
031500000000      * Inverto la data prevista consegna interna
031600000000     C     D98DCI        IFNE      *ZERO
031700941230     C                   Z-ADD     D98DCI        G02INV
031800941230     C                   Z-ADD     *ZERO         G02DAT
031900941230     C                   MOVEL     '3'           G02ERR
032000941230     C                   CALL      'XSRDA8'
032100941230     C                   PARM                    WLBDAT
032200941230     C                   Z-ADD     G02DAT        W98DCI
032300000000     C                   ENDIF
032400000000      *
032500000000     C                   ENDSR
032600970731**
032700970731Programma in errore: TELEFONARE IN SEDE!!!
