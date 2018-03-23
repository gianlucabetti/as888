000100051116      *PARMS  TGTRLS(*PRV)
000200000000     H*------------------------------------------------------------------------*
000300050318     H* STAMPA DELIVERY TIME - CREAZIONE FILE                                  *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900000000     FTABEL00F  IF   E           K DISK
001000020925     FTNTBE01L  UF   E           K DISK
001100970707     FAZCPC01L  IF   E           K DISK
001200980210     FAZCPL01L  IF   E           K DISK
001300051114     FWADLY00F  O    E           K DISK    USROPN
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* COMANDI CLP
001900000000     D*-------------------
002000020925     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
002100960709     D*-------------------
002200980216     D* PROVINCIE
002300960709     D*-------------------
002400051116     d PRV             s                   dim(600)  inz  like(DLYprv)          *PROVINCIA
002500051116     d Pcts            s                   dim(600)  inz  like(§PRcts)          *CODICE TASSAZIONE
002600051116     d Pfcr            s                   dim(600)  inz  like(DLYfcr)          *CAPOLUOGO DI REGION
002700051116     d Prap            s                   dim(600)  inz  like(DLYrap)          *REGIONE APPARTENENZ
002800051116     d Pcor            s                   dim(600)  inz  like(DLYcor)          *ORDINAMENTO
002900010716     D*-------------------
003000010716     D* TERMINAL PARTENZA
003100010716     D*-------------------
003200051116     D OFIL            S              3  0 DIM(250)  inz                        *CODICE
003300051116     D OTFP            S              3  0 DIM(250)  inz                        *CODICE
003400000000     D*------------------------------------------------------------------------*
003500000000     D* INPUT
003600000000     D*------------------------------------------------------------------------*
003700980210     D*-------------------
003800980210     D* DS REPERIMENTO DATI UTENTE
003900980210     D*-------------------
004000020919     D TIBS34DS      E DS                                                       *Profili utente
004100020919     D DDATIUTE      E DS                                                       *Dati utente
004200020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004300000000     D*-------------------
004400980210     D* DS CALCOLA TEMPI DELIVERY
004500000000     D*-------------------
004600980210     D SD99DS        E DS                  EXTNAME(TNSD99DS)
004700971020     D*------------------
004800980210     D* DS REPERIMENTO TERMINALs
004900971020     D*------------------
005000020925     D FNLV55DS      E DS
005100960709     D*-------------------
005200960709     D* DS PROVINCIE
005300960709     D*-------------------
005400960709     D DSPR          E DS
005500000000     D*-------------------
005600000000     D* DS CODICI TASSAZIONE
005700000000     D*-------------------
005800000000     D DSCT          E DS
005900020925     D*-------------------
006000020925     D* DS STATISTICHE DATI DIVERSI
006100020925     D*-------------------
006200020925     D DSDD          E DS
006300980210     D*------------------
006400980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500980210     D*------------------
006600980210     D WLBDA8          DS
006700980210     D  G08DAT                 1      8  0
006800980210     D  G08INV                 9     16  0
006900980210     D  G08ERR                17     17
007000980210     D  G08TGI                18     22  0
007100000000     D*-------------------
007200000000     D* DS PARAMETRI DI LANCIO
007300000000     D*-------------------
007400980210     D PARAM           DS
007500980210     D  PARLNP                 1      3
007600980210     D  PARTEM                 4      4
007700980210     D  PARTSP                 5      5
007800980210     D  PARISO                 6      6
007900980210     D  PARDIS                 7      7
008000980210     D  PARSSA                 8      8
008100980210     D  PARND1                 9     83
008200980210     D  PARND2                84    158
008300980210     D  PARND3               159    233
008400980210     D*-------------------
008500980210     D* ARCHITETTURA
008600980210     D*-------------------
008700980210     D KPJBA         E DS
008800020830     D*-------------------
008900020925     D* DS REPERIMENTO P.O. IN GESTIONE
009000020830     D*-------------------
009100020925     D tibs56ds      E DS                                                       *per FILIALE remota
009200020830     D  ski                    7    756  0 dim(250)
009300020925     D trul26ds      E DS                                                       *per SEDE sede
009400020925     D  skpo                  11    760  0 dim(250)
009500020925     D  skpot                761   1510  0 dim(250)
009600051116      *
009700051116      * V A R I A B I L I
009800051116      *
009900051116     d XX              s              3  0 inz
010000000000     C*------------------------------------------------------------------------*
010100000000     C* MAIN LINE
010200000000     C*------------------------------------------------------------------------*
010300051116      *
010400051116      * Carica le tabelle occorrenti
010500051116     c                   exsr      CarTAB
010600020925     C*
010700050318     C* PULISCE E RICREA IL FILE SOLO SE:
010800020925     C* data corrente > data ultima creazione workfile
010900020925IF  0c     DATCOR        IFGT      §SDDDLY
011000020925     C*
011100050318     C* OPERAZIONI INIZIALI APERTURA FILE
011200020925     C                   EXSR      OPNFIL
011300000000     C*
011400980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
011500980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
011600000000     C                   EXSR      SETFIL
011700000000     C*
011800000000     C* CICLO FINO A FINE FILE
011900000000DO  2C     $FINE         DOWEQ     '0'
012000980216     C*
012100980216     C* OPERAZIONI PER NUOVO CAP
012200980216     C                   EXSR      NEWCAP
012300980216     C*
012400980216     C* CICLO FINO A ROTTURA CAP
012500980216DO  3C     $FINE         DOWEQ     '0'
012600980216     C     CPCNAR        ANDEQ     DEPNAR
012700980216     C     CPCCAP        ANDEQ     DEPCAP
012800980302     C*
012900980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
013000980302     C                   Z-ADD     CPCLNA        WILNA                          *LINEA ARRIVO
013100980302     C                   Z-ADD     CPCLOL        WILOL                          *LINEA OLTRE
013200980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
013300980302     C                   EXSR      CALLNA
013400980302     C                   Z-ADD     WOLIN         DEPLIN                         *DEPOSITO LNA CAP
013500980302     C*
013600020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
013700980220     C                   MOVEL     'C'           $DLY
013800980216     C                   MOVEL     *BLANKS       WLOC
013900980216     C                   MOVEL     CPCISO        WISO
014000980302     C                   Z-ADD     WOLIN         WLNA
014100980219     C                   MOVEL     'N'           WDLV
014200020925do  4C     1             DO        250           J
014300020925if  5C                   IF        OTFP(J) > *ZEROS
014400010716     C                   Z-ADD     OTFP(J)       DEPTFP
014500980210     C                   EXSR      CREDLY
014600040415     C                   Z-ADD     *ZEROS        DEPTIC
014700040415     C                   Z-ADD     *ZEROS        DEPTCC
014800090515     C                   Z-ADD     *ZEROS        DEPTIe
014900090515     C                   Z-ADD     *ZEROS        DEPTCe
015000020925e   5C                   ENDIF
015100020925e   4C                   ENDDO
015200980216     C*
015300980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
015400980216     C                   EXSR      SETCPL
015500980216     C*
015600980216     C* CICLO FINO A FINE FILE
015700980216DO  4C     $FINL         DOWEQ     '0'
015800980216     C*
015900980216     C* SCRIVE IL RECORD DEL WORKFILE
016000980220     C                   MOVEL     'L'           $DLY
016100980216     C                   MOVEL     CPLLOC        WLOC
016200980220     C                   MOVEL     CPLDLV        WDLV
016300980302     C*
016400980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
016500980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
016600980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
016700980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
016800980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
016900980302     C                   EXSR      CALLNA
017000980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
017100980302     C                   Z-ADD     WOLIN         WLNA
017200980220X   5C                   ELSE                                                   *LIVELLO CAP
017300980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
017400980302     C                   Z-ADD     DEPLIN        WLNA
017500980220E   5C                   ENDIF
017600020925     C*
017700020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
017800020925do  5C     1             DO        250           J
017900020925if  6C                   IF        OTFP(J) > *ZEROS
018000020925     C                   Z-ADD     OTFP(J)       DEPTFP
018100020925     C                   EXSR      CREDLY
018200040415     C                   Z-ADD     *ZEROS        DEPTIC
018300040415     C                   Z-ADD     *ZEROS        DEPTCC
018400090515     C                   Z-ADD     *ZEROS        DEPTIe
018500090515     C                   Z-ADD     *ZEROS        DEPTCe
018600020925e   6C                   ENDIF
018700020925e   5C                   ENDDO
018800980216     C*
018900980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
019000980216     C                   EXSR      LETCPL
019100980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
019200000000     C*
019300980216     C* LETTURA RECORD SUCCESSIVO -CAP-
019400000000     C                   EXSR      LETFIL
019500980216E   3C                   ENDDO                                                  *ROTTURA CAP
019600980216E   2C                   ENDDO                                                  *FINE -CAP-
019700000000E   1C                   ENDIF
019800000000     C*
019900000000     C* OPERAZIONI DI CHIUSURA
020000971020     C                   EXSR      FINSR
020100020925E   0C                   ENDIF
020200000000     C*
020300000000     C                   SETON                                        LR
020400000000     C*------------------------------------------------------------------------*
020500980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
020600000000     C*------------------------------------------------------------------------*
020700000000     C     SETFIL        BEGSR
020800000000     C*
020900970707     C                   Z-ADD     P96VER        KCCVER
021000970707     C     KEYCPC        SETLL     AZCPC01L                           99
021100000000IF  1C     *IN99         IFEQ      *ON
021200000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
021300000000X   1C                   ELSE
021400000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
021500000000E   1C                   ENDIF
021600000000     C*
021700000000     C                   ENDSR
021800000000     C*------------------------------------------------------------------------*
021900980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
022000000000     C*------------------------------------------------------------------------*
022100000000     C     LETFIL        BEGSR
022200000000     C*
022300000000     C* LEGGE FINO A:
022400000000     C*     - FINE FILE
022500000000     C*     - TROVATO RECORD VALIDO
022600000000     C                   MOVEL     'N'           WRECOK
022700000000DO  1C     $FINE         DOWEQ     '0'
022800000000     C     WRECOK        ANDEQ     'N'
022900970707     C     KEYCPC        READE     AZCPC01L                               99
023000000000IF  2C     *IN99         IFEQ      *ON
023100000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
023200000000X   2C                   ELSE
023300000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
023400000000E   2C                   ENDIF
023500000000E   1C                   ENDDO
023600960701     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
024000000000     C*------------------------------------------------------------------------*
024100000000     C     CHKREC        BEGSR
024200000000     C*
024300000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
024400000000     C*
024500000000     C* CAP ESTERO, ESCLUDE
024600970707IF  1C     CPCNAR        IFNE      *BLANKS
024700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024800000000E   1C                   ENDIF
024900000000     C*
025000000000     C* CAP FITTIZIO, ESCLUDE
025100970707IF  1C     CPCFIT        IFEQ      'S'
025200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025300000000E   1C                   ENDIF
025400980217     C*
025500980217     C* CAP OBSOLETO, ESCLUDE
025600980217IF  1C     CPCFI1        IFEQ      'O'
025700980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025800980217E   1C                   ENDIF
025900000000     C*
026000000000     C                   ENDSR
026100980302     C*------------------------------------------------------------------------*
026200980302     C* CALLNA - CALCOLA LA LINEA ARRIVO DA CONSIDERARE
026300980302     C*------------------------------------------------------------------------*
026400980302     C     CALLNA        BEGSR
026500020802     C*
026600980302     C                   Z-ADD     WILNA         WOLIN                          *LINEA STANDARD
026700980302     C*
026800980302     C                   ENDSR
026900980216     C*------------------------------------------------------------------------*
027000980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
027100980216     C*------------------------------------------------------------------------*
027200980216     C     NEWCAP        BEGSR
027300980216     C*
027400980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
027500980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
027600980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
027700980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
027800980220     C*
027900980220     C* AZZERA LE VARIABILI DI LAVORO
028000980220     C                   Z-ADD     *ZEROS        DEPTIC                         *TEMPO INTERNO CORRI
028100980220     C                   Z-ADD     *ZEROS        DEPTCC                         *TEMPO CLIENTE CORRI
028200980220     C                   Z-ADD     *ZEROS        DEPTIE                         *TEMPO INTERNO ESPRE
028300980220     C                   Z-ADD     *ZEROS        DEPTCE                         *TEMPO CLIENTE ESPRE
028400980216     C*
028500980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
028600980216     C                   Z-ADD     1             I
028700980216     C                   SETOFF                                           28
028800980216     C     CPCPRV        LOOKUP    PRV(I)                                 28
028900980216IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
029000980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
029100980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
029200980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
029300980216X   1C                   ELSE                                                   *NON TROVATO
029400980216     C                   MOVEL     *BLANKS       DEPFCR
029500980216     C                   MOVEL     *BLANKS       DEPRAP
029600980216     C                   MOVEL     *BLANKS       DEPCOR
029700980216E   1C                   ENDIF
029800980216     C*
029900980216     C                   ENDSR
030000980216     C*------------------------------------------------------------------------*
030100980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
030200980216     C*------------------------------------------------------------------------*
030300980216     C     SETCPL        BEGSR
030400980216     C*
030500980216     C* REIMPOSTA LE VARIABILI DI LAVORO
030600980216     C                   MOVEL     '0'           $FINL
030700980216     C*
030800980216     C                   Z-ADD     P96VER        KCLVER
030900980216     C                   MOVEL     DEPNAR        KCLNAR
031000980216     C                   MOVEL     DEPCAP        KCLCAP
031100980216     C     KEYCPL        SETLL     AZCPL01L                           98
031200980216IF  1C     *IN98         IFEQ      *ON
031300980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
031400980216X   1C                   ELSE
031500980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
031600980216E   1C                   ENDIF
031700980216     C*
031800980216     C                   ENDSR
031900980216     C*------------------------------------------------------------------------*
032000980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
032100980216     C*------------------------------------------------------------------------*
032200980216     C     LETCPL        BEGSR
032300980216     C*
032400980216     C* LEGGE FINO A:
032500980216     C*     - FINE FILE
032600980216     C*     - TROVATO RECORD VALIDO
032700980216     C                   MOVEL     'N'           WRECOK
032800980216DO  1C     $FINL         DOWEQ     '0'
032900980216     C     WRECOK        ANDEQ     'N'
033000980216     C     KEYCPL        READE     AZCPL01L                               98
033100980216IF  2C     *IN98         IFEQ      *ON
033200980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
033300980216X   2C                   ELSE
033400980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
033500980216E   2C                   ENDIF
033600980216E   1C                   ENDDO
033700980216     C*
033800980216     C                   ENDSR
033900980216     C*------------------------------------------------------------------------*
034000980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
034100980216     C*------------------------------------------------------------------------*
034200980216     C     CHKCPL        BEGSR
034300980216     C*
034400980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
034500980216     C*
034600980217     C* LOCALITA' OBSOLETA, ESCLUDE
034700980217IF  1C     CPLFI1        IFEQ      'O'
034800980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034900980217E   1C                   ENDIF
035000980216     C*
035100980216     C                   ENDSR
035200980216     C*------------------------------------------------------------------------*
035300980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
035400980216     C*------------------------------------------------------------------------*
035500980216     C     CREDLY        BEGSR
035600980216     C*
035700050318     C                   CLEAR                   WADLY000
035800980216     C*---
035900980216     C* DATI DEDOTTI
036000980216     C*---
036100980216     C                   MOVEL     DEPTFP        DLYLNP                         *LINEA PARTENZA
036200980216     C                   MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
036300980216     C                   MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
036400980216     C                   MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
036500980216     C*---
036600980216     C* DATI CAPPARIO
036700980216     C*---
036800980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
036900980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
037000980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
037100980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
037200980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
037300980219     C                   MOVEL     WDLV          DLYDLV                         *STAMPA LOCALITA'
037400980216     C*---
037500980216     C* DATI CALCOLATI
037600980216     C*---
037700980220     C                   MOVEL     'C'           DLYTSP                         *TIPO SERVIZIO
037800980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
037900980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
038000040415     C     DEPTIC        ANDNE     *ZEROS                                       *CAMBIO D TP
038100040415     C     DEPTCC        ANDNE     *ZEROS                                       *CAMBIO D TP
038200980220     C                   Z-ADD     DEPTIC        DLYTRI                         *TEMPO INTERNO CORRI
038300980220     C                   Z-ADD     DEPTCC        DLYTRC                         *TEMPO CLIENTE CORRI
038400980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
038500040415     C                                                                          *'L' O NEW TERM.PART
038600980216     C*
038700980216     C* RECUPERA IL TERMINAL ARRIVO DEL CAP
038800980216     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
038900980216     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
039000980216     C                   Z-ADD     WLNA          D55LIN                         *LINEA
039100020527     C                   Z-ADD     P96DRI        D55DRF                         *DATA RIFERIMENTO
039200980216     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
039300980216     C                   CALL      'FNLV55R'
039400020925     C                   PARM                    FNLV55DS
039500980220IF  2C     D55ERR        IFNE      *BLANKS                                      *ERRORE
039600980216     C                   Z-ADD     WLNA          D55TFA                         *SE STESSA
039700980220E   2C                   ENDIF
039800980216     C                   Z-ADD     D55TFA        DEPTFA                         *TERMINAL ARRIVO
039900980216     C* CORRIERE
040000980216     C                   CLEAR                   SD99DS
040100980216     C                   MOVEL     'C'           D98TSP                         *TIPO SERVIZIO
040200020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
040300980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
040400980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
040500980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
040600980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
040700980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
040800980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
040900980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
041000980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
041100980216     C                   Z-ADD     1             D98NCD                         *COLLI
041200980216     C                   CALL      'TNSD99R'
041300980216     C                   PARM                    SD99DS
041400980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
041500980220IF  3C     D98TRI        IFGT      24
041600980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
041700980220X   3C                   ELSE
041800980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
041900980220E   3C                   ENDIF
042000980220IF  3C     D98TRC        IFGT      24
042100980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
042200980220X   3C                   ELSE
042300980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
042400980220E   3C                   ENDIF
042500980220X   2C                   ELSE                                                   *ERRORI
042600980216     C                   Z-ADD     *ZEROS        DLYTRI
042700980216     C                   Z-ADD     *ZEROS        DLYTRC
042800980220E   2C                   ENDIF
042900980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
043000980220     C                   Z-ADD     DLYTRI        DEPTIC                         *TEMPO INTERNO CORRI
043100980220     C                   Z-ADD     DLYTRC        DEPTCC                         *TEMPO CLIENTE CORRI
043200980220E   2C                   ENDIF
043300980220E   1C                   ENDIF
043400050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
043500980216     C* ESPRESSO
043600980220     C                   MOVEL     'E'           DLYTSP                         *TIPO SERVIZIO
043700980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
043800980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
043900090515     C     DEPTIe        ANDNE     *ZEROS                                       *CAMBIO D TP
044000090515     C     DEPTCe        ANDNE     *ZEROS                                       *CAMBIO D TP
044100980220     C                   Z-ADD     DEPTIE        DLYTRI                         *TEMPO INTERNO ESPRE
044200980220     C                   Z-ADD     DEPTCE        DLYTRC                         *TEMPO CLIENTE ESPRE
044300980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
044400980216     C                   CLEAR                   SD99DS
044500980216     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO
044600020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
044700980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
044800980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
044900980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
045000980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
045100980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
045200980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
045300980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
045400980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
045500980216     C                   Z-ADD     1             D98NCD                         *COLLI
045600980216     C                   CALL      'TNSD99R'
045700980216     C                   PARM                    SD99DS
045800980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
045900980220IF  3C     D98TRI        IFGT      24
046000980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
046100980220X   3C                   ELSE
046200980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
046300980220E   3C                   ENDIF
046400980220IF  3C     D98TRC        IFGT      24
046500980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
046600980220X   3C                   ELSE
046700980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
046800980220E   3C                   ENDIF
046900980220X   2C                   ELSE                                                   *ERRORI
047000980216     C                   Z-ADD     *ZEROS        DLYTRI
047100980216     C                   Z-ADD     *ZEROS        DLYTRC
047200980220E   2C                   ENDIF
047300980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
047400980220     C                   Z-ADD     DLYTRI        DEPTIE                         *TEMPO INTERNO ESPRE
047500980220     C                   Z-ADD     DLYTRC        DEPTCE                         *TEMPO CLIENTE ESPRE
047600980220E   2C                   ENDIF
047700980220E   1C                   ENDIF
047800050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
047900980216     C*
048000980216     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200971020     C* FINSR - OPERAZIONI FINALI
048300000000     C*------------------------------------------------------------------------*
048400971020     C     FINSR         BEGSR
048500971020     C*
048600971020     C* CHIUDE I FILE DI LAVORO
048700051114     C                   CLOSE     WADLY00F
048800020925     C*
048900020925     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
049000020925     C                   MOVEL(P)  'SDD'         KBECOD
049100020925     C                   MOVEL(P)  '1'           KBEKE1
049200020925     C     KEYTBE        CHAIN     TNTBE01L
049300020925IF  1C                   IF        %found(TNTBE01L)
049400020925     C                   Z-ADD     DATCOR        §SDDDLY
049500020925     C                   MOVEL     DSDD          TBEUNI
049600020925     C                   UPDATE    TNTBE000
049700020925E   1C                   ENDIF
049800971020     C*
049900980211     C* CHIUDE I PGM CHIAMATI
050000980210     C                   CLEAR                   SD99DS                         *CALCOLA DELIVERY
050100000000     C                   MOVEL     'C'           D98TLA
050200971009     C                   CALL      'TNSD99R'
050300980210     C                   PARM                    SD99DS
050400980211     C*
050500020925     C                   CLEAR                   FNLV55DS                       *RECUPERA TERMINALs
050600971020     C                   MOVEL     'C'           D55TLA
050700971020     C                   CALL      'FNLV55R'
050800020925     C                   PARM                    FNLV55DS
050900000000     C*
051000000000     C                   ENDSR
051100000000     C*------------------------------------------------------------------------*
051200050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
051300000000     C*------------------------------------------------------------------------*
051400000000     C     OPNFIL        BEGSR
051500020925     c*
051600050318     c* Pulisce il file
051700000000     C                   Z-ADD     80            LENGH            15 5
051800030304     C                   MOVEL(P)  CMD(1)        COMMAN           80
051900050318     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
052000030304     C                   PARM                    COMMAN
052100000000     C                   PARM                    LENGH
052200021202     C                   IF        %error
052300021202     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
052400021202     C                   ENDIF
052500020925     C*
052600050318     C* Apre il file
052700051114     C                   OPEN      WADLY00F                                     *APRE FILE
052800960701     C*
052900000000     C                   ENDSR
053000960709     C*------------------------------------------------------------------------*
053100980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
053200960709     C*------------------------------------------------------------------------*
053300960709     C     CARTAB        BEGSR
053400020925     C*---
053500020925     C* Dati statistiche
053600020925     C*---
053700020925     C                   CLEAR                   DSDD
053800020925     C                   MOVEL(P)  'SDD'         KBECOD
053900020925     C                   MOVEL(P)  '1'           KBEKE1
054000020925     C     KEYTBE        CHAIN(N)  TNTBE01L
054100020925     C                   IF        %found(TNTBE01L)
054200020925     C                   MOVEL     TBEUNI        DSDD
054300020925     C                   ENDIF
054400980211     C*---
054500980211     C* PROVINCIE
054600980211     C*---
054700980216     C                   Z-ADD     *ZEROS        I                 5 0
054800020919     C                   Z-ADD     1             KTBKUT
054900960709     C                   MOVEL     'PR'          KTBCOD
055000960709     C     KE2TAB        SETLL     TABEL00F
055100960709     C     KE2TAB        READE     TABEL00F                               99
055200960709DO  1C     *IN99         DOWEQ     *OFF
055300960709IF  2C     TBLFLG        IFEQ      *BLANKS
055400960709     C                   ADD       1             I
055500960709     C                   MOVEL     TBLUNI        DSPR
055600960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
055700960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
055800051116     c                   movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
055900960709E   2C                   ENDIF
056000960709     C     KE2TAB        READE     TABEL00F                               99
056100960709E   1C                   ENDDO
056200051116      *
056300051116      * Imposta il progressivo per le province (diverse) con lo stesso
056400051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
056500051116      *   - che NON può essere uguale per due province diverse:
056600051116      *   ha causato confusione nel reperimento dei tempi di consegna
056700051116      *   e/o nell'elenco delle località)
056800051116     c                   clear                   xx
056900051116do  1c                   DOW       xx       <  %elem(PRV)
057000051116     c                   add       1             xx
057100051116if  2c                   if        Prv(xx)  =  *blanks
057200051116     c                   leave
057300051116e   2c                   endif
057400051116      * - scarto le province per le quali è già stata impostata la
057500051116      *   "sigla" finale della provincia per il codice ordinamento
057600051116     c                   if        Pcor(xx) <> *blanks
057700051116     c                   iter
057800051116     c                   endif
057900051116      * - cerco le altre province con lo stesso codice tassazione
058000051116     c                   eval      I        =  xx
058100051116do  2c                   DOW       I        <  %elem(PRV)
058200051116     c                   add       1             I
058300051116     c                   eval      *in28    =  *off
058400051116     c     Pcts(xx)      lookup    Pcts(I)                                28
058500051116if  3c                   if        *in28    =  *off
058600051116     c                   leave
058700051116e   3c                   endif
058800051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
058900051116      *   memorizzo la provincia stessa - da posporre al codice
059000051116      *   ordinamento
059100051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
059200051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
059300051116      *   (ora memorizzo "   LO", "   FO", "   RN")
059400051116if  3c                   if        Pfcr(xx) =  *blanks
059500051116     c                   move      Prv(xx)       Pcor(xx)
059600051116e   3c                   endif
059700051116if  3c                   if        Pfcr(I)  =  *blanks
059800051116     c                   move      Prv(I)        Pcor(I)
059900051116e   3c                   endif
060000051116e   2c                   ENDDO
060100051116e   1c                   ENDDO
060200980211     C*---
060300960709     C* DATI CODICI TASSAZIONE
060400980211     C*---
060500020919     C                   Z-ADD     1             KTBKUT
060600960709     C                   MOVEL     'CT'          KTBCOD
060700041213DO  1C     1             DO        600           I
060800960709IF  2C     PRV(I)        IFNE      *BLANKS
060900960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
061000960709     C     KEYTAB        CHAIN     TABEL00F                           99
061100960709IF  3C     *IN99         IFEQ      *OFF
061200960709     C     TBLFLG        ANDEQ     *BLANKS
061300960709     C                   MOVEL     TBLUNI        DSCT
061400960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
061500960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
061600960709E   3C                   ENDIF
061700020925E   3C                   ENDIF
061800960709E   1C                   ENDDO
061900980302     C*---
062000010716     C* TERMINAL SU ELABORATORE
062100020925     c* SEDE    --> TUTTI I TERMINAL PARTENZA
062200020925     c* FILIALE --> TUTTI i TERMINAL PARTENZA DELL'AREA
062300980302     C*---
062400020925     c                   CLEAR                   OFil
062500020925     c                   CLEAR                   OTfp
062600020925IF  1C                   IF        DUTLPO = 'S'                                 TRASMISSIONE IN SEDE
062700020925     C                   clear                   trul26ds
062800020925     C                   EVAL      I26TLA = 'L'
062900020925     C                   EVAL      I26TPT = 'P'
063000020925     C                   CALL      'TRUL26R'
063100020925     C                   PARM                    trul26ds
063200020925     C                   Z-ADD     skpot         OFil                           *terminal -TUTTI-
063300020925X   1C                   ELSE                                                   RICEZIONE IN FILIALE
063400020925     C                   CLEAR                   tibs56ds
063500020925     C                   EVAL      I56PPO = DUTPOU
063600020925     C                   EVAL      I56MOD = 'POA'
063700020925     C                   CALL      'TIBS56R'
063800020925     C                   PARM                    tibs56ds
063900020925do  2C     1             DO        250           I
064000020925if  3C                   IF        ski(I) > *ZEROS
064100020925     C                   CLEAR                   FNLV55DS
064200020925     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
064300020925     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
064400020925     C                   Z-ADD     ski(I)        D55LIN                         *LINEA
064500020925     C                   Z-ADD     p96dri        D55DRF                         *DATA RIFERIMENTO
064600020925     C                   CALL      'FNLV55R'
064700020925     C                   PARM                    FNLV55DS
064800020925if  4C     D55ERR        IFNE      *BLANKS                                      *ERRORE
064900020925     C                   Z-ADD     dutpou        D55TFP                         *SE STESSA
065000020925e   4C                   ENDIF
065100020925     c                   Z-ADD     d55tfp        ski(I)                         *FIL diventa TFP
065200020925e   3C                   ENDIF
065300020925e   2C                   ENDDO
065400020925     C                   Z-ADD     ski           OFil                           *terminal -AREA-
065500020925e   1C                   ENDIF
065600020925     c*
065700020925     c* Estrapola SOLO i terminal
065800010716     C                   Z-ADD     *ZEROS        J                 5 0
065900020925do  1C     1             DO        250           I
066000020925if  2C                   IF        OFil(I) > *ZEROS
066100020925     C     OFil(i)       LOOKUP    OTfp                                   55
066200020925if  3C                   IF        *IN55 = *OFF                                 *NON TROVATO
066300010716     C                   ADD       1             J
066400020925     C                   Z-ADD     OFil(i)       OTfp(J)
066500020925e   3C                   ENDIF
066600020925e   2C                   ENDIF
066700020925e   1C                   ENDDO
066800960709     C*
066900960709     C                   ENDSR
067000020919     C*--------------------------------------------------------------------------------------------*
067100020919     C* REPERISCE I DATI UTENTE
067200020919     C*--------------------------------------------------------------------------------------------*
067300020919     C     REPDATIUTE    BEGSR
067400020919     C*
067500020919     C* INIZIALIZZA VARIABILI DI WRK
067600020919     C                   CLEAR                   TIBS34DS
067700020919     C                   CLEAR                   AZUTEDS
067800020919     C                   CLEAR                   DDATIUTE
067900020919     C*
068000020919     C     *DTAARA       DEFINE    §azute        azuteds
068100020919     C     *DTAARA       DEFINE    §datiute      ddatiute
068200020919     C                   IN(E)     *DTAARA
068300020919if  1C                   IF        %Error
068400020919     c                   EVAL      I34Tla = 'L'
068500020919     C                   CALL      'TIBS34R'
068600020919     C                   PARM                    Tibs34Ds
068700020919     C                   IN        *DTAARA
068800020919e   1C                   ENDIF
068900020919     C*
069000020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
069100020919     C                   MOVEL(P)  RSUT          DSFIRS           20
069200020919     C*
069300020919     C                   ENDSR
069400000000     C*------------------------------------------------------------------------*
069500000000     C* *INZSR - OPERAZIONI INIZIALI
069600000000     C*------------------------------------------------------------------------*
069700000000     C     *INZSR        BEGSR
069800980211     C*---
069900000000     C* RICEVIMENTO PARAMETRI
070000980211     C*---
070100000000     C     *ENTRY        PLIST
070200000000     C                   PARM                    KPJBA
070300000000     C                   MOVEL     KPJBU         PARAM
070400980211     C*---
070500000000     C* VARIABILI RIFERITE AL DATABASE
070600980211     C*---
070700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
070800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
070900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
071000020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
071100020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
071200020925     C     *LIKE         DEFINE    TBLKEY        KTBKEY
071300980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
071400980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
071500980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
071600980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
071700980216     C     *LIKE         DEFINE    CPCLNA        DEPTFP                         *DEPOSITI
071800980216     C     *LIKE         DEFINE    CPCLNA        DEPTFA
071900980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
072000980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
072100980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
072200980216     C     *LIKE         DEFINE    DLYFCR        DEPFCR
072300980216     C     *LIKE         DEFINE    DLYRAP        DEPRAP
072400980216     C     *LIKE         DEFINE    DLYCOR        DEPCOR
072500980302     C     *LIKE         DEFINE    CPCLNA        DEPLIN
072600980220     C     *LIKE         DEFINE    DLYTRI        DEPTIC
072700980220     C     *LIKE         DEFINE    DLYTRC        DEPTCC
072800980220     C     *LIKE         DEFINE    DLYTRI        DEPTIE
072900980220     C     *LIKE         DEFINE    DLYTRC        DEPTCE
073000980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
073100980216     C     *LIKE         DEFINE    CPCISO        WISO
073200980216     C     *LIKE         DEFINE    CPCLNA        WLNA
073300980219     C     *LIKE         DEFINE    CPLDLV        WDLV
073400980302     C     *LIKE         DEFINE    CPCLNA        WILNA
073500980302     C     *LIKE         DEFINE    CPCLOL        WILOL
073600980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
073700980211     C*---
073800000000     C* CHIAVI DI LETTURA
073900980211     C*---
074000980211     C     KEYTAB        KLIST                                                  *TABEL00F
074100980211     C                   KFLD                    KTBKUT                          -UTENTE
074200980211     C                   KFLD                    KTBCOD                          -TABELLA
074300980211     C                   KFLD                    KTBKEY                          -CHIAVE
074400980211     C     KE2TAB        KLIST                                                  *TABEL00F
074500980211     C                   KFLD                    KTBKUT                          -UTENTE
074600980211     C                   KFLD                    KTBCOD                          -TEBELLA
074700020925     C     KEYTBE        KLIST                                                  *TNTBE01L
074800020925     C                   KFLD                    KBECOD                          -TABELLA
074900020925     C                   KFLD                    KBEKE1                          -CHIAVE
075000980211     C     KEYCPC        KLIST                                                  *AZCPC00F
075100980211     C                   KFLD                    KCCVER                          -VERSIONE
075200980211     C     KEYCPL        KLIST                                                  *AZCPL00F
075300980211     C                   KFLD                    KCLVER                          -VERSIONE
075400980216     C                   KFLD                    KCLNAR                          -NAZIONE
075500980216     C                   KFLD                    KCLCAP                          -CAP
075600980211     C*---
075700000000     C* VARIABILI DI CONTROLLO
075800980211     C*---
075900980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
076000980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
076100980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
076200980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
076300050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
076400020527     C*
076500020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
076600020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
076700020527     C                   Z-ADD     N8            G08DAT
076800020527     C                   Z-ADD     *ZEROS        G08INV
076900020527     C                   MOVEL     '0'           G08ERR
077000020527     C                   CALL      'XSRDA8'
077100020527     C                   PARM                    WLBDA8
077200020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
077300020925     C*
077400020925     C* REPERISCE I DATI UTENTE
077500020925     C                   EXSR      REPDATIUTE
077600970707     C*
077700020527     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
077800020527     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
077900020919IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
078000030304     C***                Z-ADD     *ZEROS        P96DRI            8 0
078100030304     C                   Z-ADD     DATCOR        P96DRI            8 0
078200020527X   1C                   ELSE                                                   S E D E
078300020527     C                   Z-ADD     DATCOR        P96DRI            8 0
078400020527E   1C                   ENDIF
078500970707     C                   Z-ADD     *ZEROS        P96VER            5 0
078600970707     C                   CALL      'TISI96R'
078700970707     C                   PARM                    P96DRI
078800970707     C                   PARM                    P96VER
078900040415     C*
079000040415     C* REIPOSTO LA DATA CORRENTE ALLA DATA D RIFERIMENTO X IL REPERIMENTO VERSIONE CAPPARIO ALLA
079100040415     C* DATA CORRENTE XCHE' IN USCITA DAL *PGM TISI96R VALORIZZA IL P96DRI CON LA DATA D DECORRENZA
079200040415     C* DELLA VERSIONE REPERITA
079300040415     C                   Z-ADD     DATCOR        P96DRI
079400960709     C*
079500000000     C                   ENDSR
079600000000     C*------------------------------------------------------------------------*
079700000000**   CMD -COMANDI CLP-
079800050318CLRPFM FILE(WADLY00F)
