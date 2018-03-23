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
014800020925e   5C                   ENDIF
014900020925e   4C                   ENDDO
015000980216     C*
015100980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
015200980216     C                   EXSR      SETCPL
015300980216     C*
015400980216     C* CICLO FINO A FINE FILE
015500980216DO  4C     $FINL         DOWEQ     '0'
015600980216     C*
015700980216     C* SCRIVE IL RECORD DEL WORKFILE
015800980220     C                   MOVEL     'L'           $DLY
015900980216     C                   MOVEL     CPLLOC        WLOC
016000980220     C                   MOVEL     CPLDLV        WDLV
016100980302     C*
016200980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
016300980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
016400980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
016500980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
016600980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
016700980302     C                   EXSR      CALLNA
016800980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
016900980302     C                   Z-ADD     WOLIN         WLNA
017000980220X   5C                   ELSE                                                   *LIVELLO CAP
017100980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
017200980302     C                   Z-ADD     DEPLIN        WLNA
017300980220E   5C                   ENDIF
017400020925     C*
017500020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
017600020925do  5C     1             DO        250           J
017700020925if  6C                   IF        OTFP(J) > *ZEROS
017800020925     C                   Z-ADD     OTFP(J)       DEPTFP
017900020925     C                   EXSR      CREDLY
018000040415     C                   Z-ADD     *ZEROS        DEPTIC
018100040415     C                   Z-ADD     *ZEROS        DEPTCC
018200020925e   6C                   ENDIF
018300020925e   5C                   ENDDO
018400980216     C*
018500980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
018600980216     C                   EXSR      LETCPL
018700980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
018800000000     C*
018900980216     C* LETTURA RECORD SUCCESSIVO -CAP-
019000000000     C                   EXSR      LETFIL
019100980216E   3C                   ENDDO                                                  *ROTTURA CAP
019200980216E   2C                   ENDDO                                                  *FINE -CAP-
019300000000E   1C                   ENDIF
019400000000     C*
019500000000     C* OPERAZIONI DI CHIUSURA
019600971020     C                   EXSR      FINSR
019700020925E   0C                   ENDIF
019800000000     C*
019900000000     C                   SETON                                        LR
020000000000     C*------------------------------------------------------------------------*
020100980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
020200000000     C*------------------------------------------------------------------------*
020300000000     C     SETFIL        BEGSR
020400000000     C*
020500970707     C                   Z-ADD     P96VER        KCCVER
020600970707     C     KEYCPC        SETLL     AZCPC01L                           99
020700000000IF  1C     *IN99         IFEQ      *ON
020800000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
020900000000X   1C                   ELSE
021000000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
021100000000E   1C                   ENDIF
021200000000     C*
021300000000     C                   ENDSR
021400000000     C*------------------------------------------------------------------------*
021500980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
021600000000     C*------------------------------------------------------------------------*
021700000000     C     LETFIL        BEGSR
021800000000     C*
021900000000     C* LEGGE FINO A:
022000000000     C*     - FINE FILE
022100000000     C*     - TROVATO RECORD VALIDO
022200000000     C                   MOVEL     'N'           WRECOK
022300000000DO  1C     $FINE         DOWEQ     '0'
022400000000     C     WRECOK        ANDEQ     'N'
022500970707     C     KEYCPC        READE     AZCPC01L                               99
022600000000IF  2C     *IN99         IFEQ      *ON
022700000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
022800000000X   2C                   ELSE
022900000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
023000000000E   2C                   ENDIF
023100000000E   1C                   ENDDO
023200960701     C*
023300000000     C                   ENDSR
023400000000     C*------------------------------------------------------------------------*
023500980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
023600000000     C*------------------------------------------------------------------------*
023700000000     C     CHKREC        BEGSR
023800000000     C*
023900000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
024000000000     C*
024100000000     C* CAP ESTERO, ESCLUDE
024200970707IF  1C     CPCNAR        IFNE      *BLANKS
024300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024400000000E   1C                   ENDIF
024500000000     C*
024600000000     C* CAP FITTIZIO, ESCLUDE
024700970707IF  1C     CPCFIT        IFEQ      'S'
024800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024900000000E   1C                   ENDIF
025000980217     C*
025100980217     C* CAP OBSOLETO, ESCLUDE
025200980217IF  1C     CPCFI1        IFEQ      'O'
025300980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025400980217E   1C                   ENDIF
025500000000     C*
025600000000     C                   ENDSR
025700980302     C*------------------------------------------------------------------------*
025800980302     C* CALLNA - CALCOLA LA LINEA ARRIVO DA CONSIDERARE
025900980302     C*------------------------------------------------------------------------*
026000980302     C     CALLNA        BEGSR
026100020802     C*
026200980302     C                   Z-ADD     WILNA         WOLIN                          *LINEA STANDARD
026300980302     C*
026400980302     C                   ENDSR
026500980216     C*------------------------------------------------------------------------*
026600980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
026700980216     C*------------------------------------------------------------------------*
026800980216     C     NEWCAP        BEGSR
026900980216     C*
027000980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
027100980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
027200980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
027300980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
027400980220     C*
027500980220     C* AZZERA LE VARIABILI DI LAVORO
027600980220     C                   Z-ADD     *ZEROS        DEPTIC                         *TEMPO INTERNO CORRI
027700980220     C                   Z-ADD     *ZEROS        DEPTCC                         *TEMPO CLIENTE CORRI
027800980220     C                   Z-ADD     *ZEROS        DEPTIE                         *TEMPO INTERNO ESPRE
027900980220     C                   Z-ADD     *ZEROS        DEPTCE                         *TEMPO CLIENTE ESPRE
028000980216     C*
028100980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
028200980216     C                   Z-ADD     1             I
028300980216     C                   SETOFF                                           28
028400980216     C     CPCPRV        LOOKUP    PRV(I)                                 28
028500980216IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
028600980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
028700980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
028800980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
028900980216X   1C                   ELSE                                                   *NON TROVATO
029000980216     C                   MOVEL     *BLANKS       DEPFCR
029100980216     C                   MOVEL     *BLANKS       DEPRAP
029200980216     C                   MOVEL     *BLANKS       DEPCOR
029300980216E   1C                   ENDIF
029400980216     C*
029500980216     C                   ENDSR
029600980216     C*------------------------------------------------------------------------*
029700980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
029800980216     C*------------------------------------------------------------------------*
029900980216     C     SETCPL        BEGSR
030000980216     C*
030100980216     C* REIMPOSTA LE VARIABILI DI LAVORO
030200980216     C                   MOVEL     '0'           $FINL
030300980216     C*
030400980216     C                   Z-ADD     P96VER        KCLVER
030500980216     C                   MOVEL     DEPNAR        KCLNAR
030600980216     C                   MOVEL     DEPCAP        KCLCAP
030700980216     C     KEYCPL        SETLL     AZCPL01L                           98
030800980216IF  1C     *IN98         IFEQ      *ON
030900980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
031000980216X   1C                   ELSE
031100980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
031200980216E   1C                   ENDIF
031300980216     C*
031400980216     C                   ENDSR
031500980216     C*------------------------------------------------------------------------*
031600980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
031700980216     C*------------------------------------------------------------------------*
031800980216     C     LETCPL        BEGSR
031900980216     C*
032000980216     C* LEGGE FINO A:
032100980216     C*     - FINE FILE
032200980216     C*     - TROVATO RECORD VALIDO
032300980216     C                   MOVEL     'N'           WRECOK
032400980216DO  1C     $FINL         DOWEQ     '0'
032500980216     C     WRECOK        ANDEQ     'N'
032600980216     C     KEYCPL        READE     AZCPL01L                               98
032700980216IF  2C     *IN98         IFEQ      *ON
032800980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
032900980216X   2C                   ELSE
033000980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
033100980216E   2C                   ENDIF
033200980216E   1C                   ENDDO
033300980216     C*
033400980216     C                   ENDSR
033500980216     C*------------------------------------------------------------------------*
033600980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
033700980216     C*------------------------------------------------------------------------*
033800980216     C     CHKCPL        BEGSR
033900980216     C*
034000980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
034100980216     C*
034200980217     C* LOCALITA' OBSOLETA, ESCLUDE
034300980217IF  1C     CPLFI1        IFEQ      'O'
034400980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034500980217E   1C                   ENDIF
034600980216     C*
034700980216     C                   ENDSR
034800980216     C*------------------------------------------------------------------------*
034900980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
035000980216     C*------------------------------------------------------------------------*
035100980216     C     CREDLY        BEGSR
035200980216     C*
035300050318     C                   CLEAR                   WADLY000
035400980216     C*---
035500980216     C* DATI DEDOTTI
035600980216     C*---
035700980216     C                   MOVEL     DEPTFP        DLYLNP                         *LINEA PARTENZA
035800980216     C                   MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
035900980216     C                   MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
036000980216     C                   MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
036100980216     C*---
036200980216     C* DATI CAPPARIO
036300980216     C*---
036400980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
036500980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
036600980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
036700980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
036800980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
036900980219     C                   MOVEL     WDLV          DLYDLV                         *STAMPA LOCALITA'
037000980216     C*---
037100980216     C* DATI CALCOLATI
037200980216     C*---
037300980220     C                   MOVEL     'C'           DLYTSP                         *TIPO SERVIZIO
037400980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
037500980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
037600040415     C     DEPTIC        ANDNE     *ZEROS                                       *CAMBIO D TP
037700040415     C     DEPTCC        ANDNE     *ZEROS                                       *CAMBIO D TP
037800980220     C                   Z-ADD     DEPTIC        DLYTRI                         *TEMPO INTERNO CORRI
037900980220     C                   Z-ADD     DEPTCC        DLYTRC                         *TEMPO CLIENTE CORRI
038000980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
038100040415     C                                                                          *'L' O NEW TERM.PART
038200980216     C*
038300980216     C* RECUPERA IL TERMINAL ARRIVO DEL CAP
038400980216     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
038500980216     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
038600980216     C                   Z-ADD     WLNA          D55LIN                         *LINEA
038700020527     C                   Z-ADD     P96DRI        D55DRF                         *DATA RIFERIMENTO
038800980216     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
038900980216     C                   CALL      'FNLV55R'
039000020925     C                   PARM                    FNLV55DS
039100980220IF  2C     D55ERR        IFNE      *BLANKS                                      *ERRORE
039200980216     C                   Z-ADD     WLNA          D55TFA                         *SE STESSA
039300980220E   2C                   ENDIF
039400980216     C                   Z-ADD     D55TFA        DEPTFA                         *TERMINAL ARRIVO
039500980216     C* CORRIERE
039600980216     C                   CLEAR                   SD99DS
039700980216     C                   MOVEL     'C'           D98TSP                         *TIPO SERVIZIO
039800020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
039900980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
040000980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
040100980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
040200980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
040300980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
040400980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
040500980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
040600980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
040700980216     C                   Z-ADD     1             D98NCD                         *COLLI
040800980216     C                   CALL      'TNSD99R'
040900980216     C                   PARM                    SD99DS
041000980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
041100980220IF  3C     D98TRI        IFGT      24
041200980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
041300980220X   3C                   ELSE
041400980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
041500980220E   3C                   ENDIF
041600980220IF  3C     D98TRC        IFGT      24
041700980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
041800980220X   3C                   ELSE
041900980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
042000980220E   3C                   ENDIF
042100980220X   2C                   ELSE                                                   *ERRORI
042200980216     C                   Z-ADD     *ZEROS        DLYTRI
042300980216     C                   Z-ADD     *ZEROS        DLYTRC
042400980220E   2C                   ENDIF
042500980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
042600980220     C                   Z-ADD     DLYTRI        DEPTIC                         *TEMPO INTERNO CORRI
042700980220     C                   Z-ADD     DLYTRC        DEPTCC                         *TEMPO CLIENTE CORRI
042800980220E   2C                   ENDIF
042900980220E   1C                   ENDIF
043000050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
043100980216     C* ESPRESSO
043200980220     C                   MOVEL     'E'           DLYTSP                         *TIPO SERVIZIO
043300980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
043400980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
043500980220     C                   Z-ADD     DEPTIE        DLYTRI                         *TEMPO INTERNO ESPRE
043600980220     C                   Z-ADD     DEPTCE        DLYTRC                         *TEMPO CLIENTE ESPRE
043700980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
043800980216     C                   CLEAR                   SD99DS
043900980216     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO
044000020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
044100980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
044200980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
044300980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
044400980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
044500980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
044600980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
044700980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
044800980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
044900980216     C                   Z-ADD     1             D98NCD                         *COLLI
045000980216     C                   CALL      'TNSD99R'
045100980216     C                   PARM                    SD99DS
045200980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
045300980220IF  3C     D98TRI        IFGT      24
045400980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
045500980220X   3C                   ELSE
045600980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
045700980220E   3C                   ENDIF
045800980220IF  3C     D98TRC        IFGT      24
045900980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
046000980220X   3C                   ELSE
046100980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
046200980220E   3C                   ENDIF
046300980220X   2C                   ELSE                                                   *ERRORI
046400980216     C                   Z-ADD     *ZEROS        DLYTRI
046500980216     C                   Z-ADD     *ZEROS        DLYTRC
046600980220E   2C                   ENDIF
046700980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
046800980220     C                   Z-ADD     DLYTRI        DEPTIE                         *TEMPO INTERNO ESPRE
046900980220     C                   Z-ADD     DLYTRC        DEPTCE                         *TEMPO CLIENTE ESPRE
047000980220E   2C                   ENDIF
047100980220E   1C                   ENDIF
047200050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
047300980216     C*
047400980216     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600971020     C* FINSR - OPERAZIONI FINALI
047700000000     C*------------------------------------------------------------------------*
047800971020     C     FINSR         BEGSR
047900971020     C*
048000971020     C* CHIUDE I FILE DI LAVORO
048100051114     C                   CLOSE     WADLY00F
048200020925     C*
048300020925     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
048400020925     C                   MOVEL(P)  'SDD'         KBECOD
048500020925     C                   MOVEL(P)  '1'           KBEKE1
048600020925     C     KEYTBE        CHAIN     TNTBE01L
048700020925IF  1C                   IF        %found(TNTBE01L)
048800020925     C                   Z-ADD     DATCOR        §SDDDLY
048900020925     C                   MOVEL     DSDD          TBEUNI
049000020925     C                   UPDATE    TNTBE000
049100020925E   1C                   ENDIF
049200971020     C*
049300980211     C* CHIUDE I PGM CHIAMATI
049400980210     C                   CLEAR                   SD99DS                         *CALCOLA DELIVERY
049500000000     C                   MOVEL     'C'           D98TLA
049600971009     C                   CALL      'TNSD99R'
049700980210     C                   PARM                    SD99DS
049800980211     C*
049900020925     C                   CLEAR                   FNLV55DS                       *RECUPERA TERMINALs
050000971020     C                   MOVEL     'C'           D55TLA
050100971020     C                   CALL      'FNLV55R'
050200020925     C                   PARM                    FNLV55DS
050300000000     C*
050400000000     C                   ENDSR
050500000000     C*------------------------------------------------------------------------*
050600050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
050700000000     C*------------------------------------------------------------------------*
050800000000     C     OPNFIL        BEGSR
050900020925     c*
051000050318     c* Pulisce il file
051100000000     C                   Z-ADD     80            LENGH            15 5
051200030304     C                   MOVEL(P)  CMD(1)        COMMAN           80
051300050318     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
051400030304     C                   PARM                    COMMAN
051500000000     C                   PARM                    LENGH
051600021202     C                   IF        %error
051700021202     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
051800021202     C                   ENDIF
051900020925     C*
052000050318     C* Apre il file
052100051114     C                   OPEN      WADLY00F                                     *APRE FILE
052200960701     C*
052300000000     C                   ENDSR
052400960709     C*------------------------------------------------------------------------*
052500980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
052600960709     C*------------------------------------------------------------------------*
052700960709     C     CARTAB        BEGSR
052800020925     C*---
052900020925     C* Dati statistiche
053000020925     C*---
053100020925     C                   CLEAR                   DSDD
053200020925     C                   MOVEL(P)  'SDD'         KBECOD
053300020925     C                   MOVEL(P)  '1'           KBEKE1
053400020925     C     KEYTBE        CHAIN(N)  TNTBE01L
053500020925     C                   IF        %found(TNTBE01L)
053600020925     C                   MOVEL     TBEUNI        DSDD
053700020925     C                   ENDIF
053800980211     C*---
053900980211     C* PROVINCIE
054000980211     C*---
054100980216     C                   Z-ADD     *ZEROS        I                 5 0
054200020919     C                   Z-ADD     1             KTBKUT
054300960709     C                   MOVEL     'PR'          KTBCOD
054400960709     C     KE2TAB        SETLL     TABEL00F
054500960709     C     KE2TAB        READE     TABEL00F                               99
054600960709DO  1C     *IN99         DOWEQ     *OFF
054700960709IF  2C     TBLFLG        IFEQ      *BLANKS
054800960709     C                   ADD       1             I
054900960709     C                   MOVEL     TBLUNI        DSPR
055000960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
055100960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
055200051116     c                   movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
055300960709E   2C                   ENDIF
055400960709     C     KE2TAB        READE     TABEL00F                               99
055500960709E   1C                   ENDDO
055600051116      *
055700051116      * Imposta il progressivo per le province (diverse) con lo stesso
055800051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
055900051116      *   - che NON può essere uguale per due province diverse:
056000051116      *   ha causato confusione nel reperimento dei tempi di consegna
056100051116      *   e/o nell'elenco delle località)
056200051116     c                   clear                   xx
056300051116do  1c                   DOW       xx       <  %elem(PRV)
056400051116     c                   add       1             xx
056500051116if  2c                   if        Prv(xx)  =  *blanks
056600051116     c                   leave
056700051116e   2c                   endif
056800051116      * - scarto le province per le quali è già stata impostata la
056900051116      *   "sigla" finale della provincia per il codice ordinamento
057000051116     c                   if        Pcor(xx) <> *blanks
057100051116     c                   iter
057200051116     c                   endif
057300051116      * - cerco le altre province con lo stesso codice tassazione
057400051116     c                   eval      I        =  xx
057500051116do  2c                   DOW       I        <  %elem(PRV)
057600051116     c                   add       1             I
057700051116     c                   eval      *in28    =  *off
057800051116     c     Pcts(xx)      lookup    Pcts(I)                                28
057900051116if  3c                   if        *in28    =  *off
058000051116     c                   leave
058100051116e   3c                   endif
058200051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
058300051116      *   memorizzo la provincia stessa - da posporre al codice
058400051116      *   ordinamento
058500051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
058600051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
058700051116      *   (ora memorizzo "   LO", "   FO", "   RN")
058800051116if  3c                   if        Pfcr(xx) =  *blanks
058900051116     c                   move      Prv(xx)       Pcor(xx)
059000051116e   3c                   endif
059100051116if  3c                   if        Pfcr(I)  =  *blanks
059200051116     c                   move      Prv(I)        Pcor(I)
059300051116e   3c                   endif
059400051116e   2c                   ENDDO
059500051116e   1c                   ENDDO
059600980211     C*---
059700960709     C* DATI CODICI TASSAZIONE
059800980211     C*---
059900020919     C                   Z-ADD     1             KTBKUT
060000960709     C                   MOVEL     'CT'          KTBCOD
060100041213DO  1C     1             DO        600           I
060200960709IF  2C     PRV(I)        IFNE      *BLANKS
060300960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
060400960709     C     KEYTAB        CHAIN     TABEL00F                           99
060500960709IF  3C     *IN99         IFEQ      *OFF
060600960709     C     TBLFLG        ANDEQ     *BLANKS
060700960709     C                   MOVEL     TBLUNI        DSCT
060800960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
060900960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
061000960709E   3C                   ENDIF
061100020925E   3C                   ENDIF
061200960709E   1C                   ENDDO
061300980302     C*---
061400010716     C* TERMINAL SU ELABORATORE
061500020925     c* SEDE    --> TUTTI I TERMINAL PARTENZA
061600020925     c* FILIALE --> TUTTI i TERMINAL PARTENZA DELL'AREA
061700980302     C*---
061800020925     c                   CLEAR                   OFil
061900020925     c                   CLEAR                   OTfp
062000020925IF  1C                   IF        DUTLPO = 'S'                                 TRASMISSIONE IN SEDE
062100020925     C                   clear                   trul26ds
062200020925     C                   EVAL      I26TLA = 'L'
062300020925     C                   EVAL      I26TPT = 'P'
062400020925     C                   CALL      'TRUL26R'
062500020925     C                   PARM                    trul26ds
062600020925     C                   Z-ADD     skpot         OFil                           *terminal -TUTTI-
062700020925X   1C                   ELSE                                                   RICEZIONE IN FILIALE
062800020925     C                   CLEAR                   tibs56ds
062900020925     C                   EVAL      I56PPO = DUTPOU
063000020925     C                   EVAL      I56MOD = 'POA'
063100020925     C                   CALL      'TIBS56R'
063200020925     C                   PARM                    tibs56ds
063300020925do  2C     1             DO        250           I
063400020925if  3C                   IF        ski(I) > *ZEROS
063500020925     C                   CLEAR                   FNLV55DS
063600020925     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
063700020925     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
063800020925     C                   Z-ADD     ski(I)        D55LIN                         *LINEA
063900020925     C                   Z-ADD     p96dri        D55DRF                         *DATA RIFERIMENTO
064000020925     C                   CALL      'FNLV55R'
064100020925     C                   PARM                    FNLV55DS
064200020925if  4C     D55ERR        IFNE      *BLANKS                                      *ERRORE
064300020925     C                   Z-ADD     dutpou        D55TFP                         *SE STESSA
064400020925e   4C                   ENDIF
064500020925     c                   Z-ADD     d55tfp        ski(I)                         *FIL diventa TFP
064600020925e   3C                   ENDIF
064700020925e   2C                   ENDDO
064800020925     C                   Z-ADD     ski           OFil                           *terminal -AREA-
064900020925e   1C                   ENDIF
065000020925     c*
065100020925     c* Estrapola SOLO i terminal
065200010716     C                   Z-ADD     *ZEROS        J                 5 0
065300020925do  1C     1             DO        250           I
065400020925if  2C                   IF        OFil(I) > *ZEROS
065500020925     C     OFil(i)       LOOKUP    OTfp                                   55
065600020925if  3C                   IF        *IN55 = *OFF                                 *NON TROVATO
065700010716     C                   ADD       1             J
065800020925     C                   Z-ADD     OFil(i)       OTfp(J)
065900020925e   3C                   ENDIF
066000020925e   2C                   ENDIF
066100020925e   1C                   ENDDO
066200960709     C*
066300960709     C                   ENDSR
066400020919     C*--------------------------------------------------------------------------------------------*
066500020919     C* REPERISCE I DATI UTENTE
066600020919     C*--------------------------------------------------------------------------------------------*
066700020919     C     REPDATIUTE    BEGSR
066800020919     C*
066900020919     C* INIZIALIZZA VARIABILI DI WRK
067000020919     C                   CLEAR                   TIBS34DS
067100020919     C                   CLEAR                   AZUTEDS
067200020919     C                   CLEAR                   DDATIUTE
067300020919     C*
067400020919     C     *DTAARA       DEFINE    §azute        azuteds
067500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
067600020919     C                   IN(E)     *DTAARA
067700020919if  1C                   IF        %Error
067800020919     c                   EVAL      I34Tla = 'L'
067900020919     C                   CALL      'TIBS34R'
068000020919     C                   PARM                    Tibs34Ds
068100020919     C                   IN        *DTAARA
068200020919e   1C                   ENDIF
068300020919     C*
068400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
068500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
068600020919     C*
068700020919     C                   ENDSR
068800000000     C*------------------------------------------------------------------------*
068900000000     C* *INZSR - OPERAZIONI INIZIALI
069000000000     C*------------------------------------------------------------------------*
069100000000     C     *INZSR        BEGSR
069200980211     C*---
069300000000     C* RICEVIMENTO PARAMETRI
069400980211     C*---
069500000000     C     *ENTRY        PLIST
069600000000     C                   PARM                    KPJBA
069700000000     C                   MOVEL     KPJBU         PARAM
069800980211     C*---
069900000000     C* VARIABILI RIFERITE AL DATABASE
070000980211     C*---
070100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
070200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
070300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
070400020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
070500020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
070600020925     C     *LIKE         DEFINE    TBLKEY        KTBKEY
070700980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
070800980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
070900980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
071000980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
071100980216     C     *LIKE         DEFINE    CPCLNA        DEPTFP                         *DEPOSITI
071200980216     C     *LIKE         DEFINE    CPCLNA        DEPTFA
071300980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
071400980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
071500980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
071600980216     C     *LIKE         DEFINE    DLYFCR        DEPFCR
071700980216     C     *LIKE         DEFINE    DLYRAP        DEPRAP
071800980216     C     *LIKE         DEFINE    DLYCOR        DEPCOR
071900980302     C     *LIKE         DEFINE    CPCLNA        DEPLIN
072000980220     C     *LIKE         DEFINE    DLYTRI        DEPTIC
072100980220     C     *LIKE         DEFINE    DLYTRC        DEPTCC
072200980220     C     *LIKE         DEFINE    DLYTRI        DEPTIE
072300980220     C     *LIKE         DEFINE    DLYTRC        DEPTCE
072400980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
072500980216     C     *LIKE         DEFINE    CPCISO        WISO
072600980216     C     *LIKE         DEFINE    CPCLNA        WLNA
072700980219     C     *LIKE         DEFINE    CPLDLV        WDLV
072800980302     C     *LIKE         DEFINE    CPCLNA        WILNA
072900980302     C     *LIKE         DEFINE    CPCLOL        WILOL
073000980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
073100980211     C*---
073200000000     C* CHIAVI DI LETTURA
073300980211     C*---
073400980211     C     KEYTAB        KLIST                                                  *TABEL00F
073500980211     C                   KFLD                    KTBKUT                          -UTENTE
073600980211     C                   KFLD                    KTBCOD                          -TABELLA
073700980211     C                   KFLD                    KTBKEY                          -CHIAVE
073800980211     C     KE2TAB        KLIST                                                  *TABEL00F
073900980211     C                   KFLD                    KTBKUT                          -UTENTE
074000980211     C                   KFLD                    KTBCOD                          -TEBELLA
074100020925     C     KEYTBE        KLIST                                                  *TNTBE01L
074200020925     C                   KFLD                    KBECOD                          -TABELLA
074300020925     C                   KFLD                    KBEKE1                          -CHIAVE
074400980211     C     KEYCPC        KLIST                                                  *AZCPC00F
074500980211     C                   KFLD                    KCCVER                          -VERSIONE
074600980211     C     KEYCPL        KLIST                                                  *AZCPL00F
074700980211     C                   KFLD                    KCLVER                          -VERSIONE
074800980216     C                   KFLD                    KCLNAR                          -NAZIONE
074900980216     C                   KFLD                    KCLCAP                          -CAP
075000980211     C*---
075100000000     C* VARIABILI DI CONTROLLO
075200980211     C*---
075300980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
075400980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
075500980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
075600980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
075700050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
075800020527     C*
075900020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
076000020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
076100020527     C                   Z-ADD     N8            G08DAT
076200020527     C                   Z-ADD     *ZEROS        G08INV
076300020527     C                   MOVEL     '0'           G08ERR
076400020527     C                   CALL      'XSRDA8'
076500020527     C                   PARM                    WLBDA8
076600020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
076700020925     C*
076800020925     C* REPERISCE I DATI UTENTE
076900020925     C                   EXSR      REPDATIUTE
077000970707     C*
077100020527     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
077200020527     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
077300020919IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
077400030304     C***                Z-ADD     *ZEROS        P96DRI            8 0
077500030304     C                   Z-ADD     DATCOR        P96DRI            8 0
077600020527X   1C                   ELSE                                                   S E D E
077700020527     C                   Z-ADD     DATCOR        P96DRI            8 0
077800020527E   1C                   ENDIF
077900970707     C                   Z-ADD     *ZEROS        P96VER            5 0
078000970707     C                   CALL      'TISI96R'
078100970707     C                   PARM                    P96DRI
078200970707     C                   PARM                    P96VER
078300040415     C*
078400040415     C* REIPOSTO LA DATA CORRENTE ALLA DATA D RIFERIMENTO X IL REPERIMENTO VERSIONE CAPPARIO ALLA
078500040415     C* DATA CORRENTE XCHE' IN USCITA DAL *PGM TISI96R VALORIZZA IL P96DRI CON LA DATA D DECORRENZA
078600040415     C* DELLA VERSIONE REPERITA
078700040415     C                   Z-ADD     DATCOR        P96DRI
078800960709     C*
078900000000     C                   ENDSR
079000000000     C*------------------------------------------------------------------------*
079100000000**   CMD -COMANDI CLP-
079200050318CLRPFM FILE(WADLY00F)
