000100000000     H*------------------------------------------------------------------------*
000200050318     H* STAMPA DELIVERY TIME - CREAZIONE FILE                                  *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTABEL00F  IF   E           K DISK
000900020925     FTNTBE01L  UF   E           K DISK
001000970707     FAZCPC01L  IF   E           K DISK
001100980210     FAZCPL01L  IF   E           K DISK
001200050318     FWADLY01L  UF A E           K DISK    USROPN
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* COMANDI CLP
001800000000     D*-------------------
001900020925     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
002000960709     D*-------------------
002100980216     D* PROVINCIE
002200960709     D*-------------------
002300041213     D PRV             S              2    DIM(600)                             *PROVINCIA
002400041213     D PCTS            S              2    DIM(600)                             *CODICE TASSAZIONE
002500041213     D PFCR            S              1    DIM(600)                             *CAPOLUOGO DI REGION
002600041213     D PRAP            S              2    DIM(600)                             *REGIONE APPARTENENZ
002700041213     D PCOR            S              3    DIM(600)                             *ORDINAMENTO
002800010716     D*-------------------
002900010716     D* TERMINAL PARTENZA
003000010716     D*-------------------
003100020925     D OFIL            S              3  0 DIM(250)                             *CODICE
003200020925     D OTFP            S              3  0 DIM(250)                             *CODICE
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600980210     D*-------------------
003700980210     D* DS REPERIMENTO DATI UTENTE
003800980210     D*-------------------
003900020919     D TIBS34DS      E DS                                                       *Profili utente
004000020919     D DDATIUTE      E DS                                                       *Dati utente
004100020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004200000000     D*-------------------
004300980210     D* DS CALCOLA TEMPI DELIVERY
004400000000     D*-------------------
004500980210     D SD99DS        E DS                  EXTNAME(TNSD99DS)
004600971020     D*------------------
004700980210     D* DS REPERIMENTO TERMINALs
004800971020     D*------------------
004900020925     D FNLV55DS      E DS
005000960709     D*-------------------
005100960709     D* DS PROVINCIE
005200960709     D*-------------------
005300960709     D DSPR          E DS
005400000000     D*-------------------
005500000000     D* DS CODICI TASSAZIONE
005600000000     D*-------------------
005700000000     D DSCT          E DS
005800020925     D*-------------------
005900020925     D* DS STATISTICHE DATI DIVERSI
006000020925     D*-------------------
006100020925     D DSDD          E DS
006200980210     D*------------------
006300980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006400980210     D*------------------
006500980210     D WLBDA8          DS
006600980210     D  G08DAT                 1      8  0
006700980210     D  G08INV                 9     16  0
006800980210     D  G08ERR                17     17
006900980210     D  G08TGI                18     22  0
007000000000     D*-------------------
007100000000     D* DS PARAMETRI DI LANCIO
007200000000     D*-------------------
007300980210     D PARAM           DS
007400980210     D  PARLNP                 1      3
007500980210     D  PARTEM                 4      4
007600980210     D  PARTSP                 5      5
007700980210     D  PARISO                 6      6
007800980210     D  PARDIS                 7      7
007900980210     D  PARSSA                 8      8
008000980210     D  PARND1                 9     83
008100980210     D  PARND2                84    158
008200980210     D  PARND3               159    233
008300980210     D*-------------------
008400980210     D* ARCHITETTURA
008500980210     D*-------------------
008600980210     D KPJBA         E DS
008700020830     D*-------------------
008800020925     D* DS REPERIMENTO P.O. IN GESTIONE
008900020830     D*-------------------
009000020925     D tibs56ds      E DS                                                       *per FILIALE remota
009100020830     D  ski                    7    756  0 dim(250)
009200020925     D trul26ds      E DS                                                       *per SEDE sede
009300020925     D  skpo                  11    760  0 dim(250)
009400020925     D  skpot                761   1510  0 dim(250)
009500000000     C*------------------------------------------------------------------------*
009600000000     C* MAIN LINE
009700000000     C*------------------------------------------------------------------------*
009800020925     C*
009900050318     C* PULISCE E RICREA IL FILE SOLO SE:
010000020925     C* data corrente > data ultima creazione workfile
010100020925IF  0c     DATCOR        IFGT      §SDDDLY
010200020925     C*
010300050318     C* OPERAZIONI INIZIALI APERTURA FILE
010400020925     C                   EXSR      OPNFIL
010500000000     C*
010600980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
010700980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
010800000000     C                   EXSR      SETFIL
010900000000     C*
011000000000     C* CICLO FINO A FINE FILE
011100000000DO  2C     $FINE         DOWEQ     '0'
011200980216     C*
011300980216     C* OPERAZIONI PER NUOVO CAP
011400980216     C                   EXSR      NEWCAP
011500980216     C*
011600980216     C* CICLO FINO A ROTTURA CAP
011700980216DO  3C     $FINE         DOWEQ     '0'
011800980216     C     CPCNAR        ANDEQ     DEPNAR
011900980216     C     CPCCAP        ANDEQ     DEPCAP
012000980302     C*
012100980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
012200980302     C                   Z-ADD     CPCLNA        WILNA                          *LINEA ARRIVO
012300980302     C                   Z-ADD     CPCLOL        WILOL                          *LINEA OLTRE
012400980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
012500980302     C                   EXSR      CALLNA
012600980302     C                   Z-ADD     WOLIN         DEPLIN                         *DEPOSITO LNA CAP
012700980302     C*
012800020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
012900980220     C                   MOVEL     'C'           $DLY
013000980216     C                   MOVEL     *BLANKS       WLOC
013100980216     C                   MOVEL     CPCISO        WISO
013200980302     C                   Z-ADD     WOLIN         WLNA
013300980219     C                   MOVEL     'N'           WDLV
013400020925do  4C     1             DO        250           J
013500020925if  5C                   IF        OTFP(J) > *ZEROS
013600010716     C                   Z-ADD     OTFP(J)       DEPTFP
013700980210     C                   EXSR      CREDLY
013800040415     C                   Z-ADD     *ZEROS        DEPTIC
013900040415     C                   Z-ADD     *ZEROS        DEPTCC
014000020925e   5C                   ENDIF
014100020925e   4C                   ENDDO
014200980216     C*
014300980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
014400980216     C                   EXSR      SETCPL
014500980216     C*
014600980216     C* CICLO FINO A FINE FILE
014700980216DO  4C     $FINL         DOWEQ     '0'
014800980216     C*
014900980216     C* SCRIVE IL RECORD DEL WORKFILE
015000980220     C                   MOVEL     'L'           $DLY
015100980216     C                   MOVEL     CPLLOC        WLOC
015200980220     C                   MOVEL     CPLDLV        WDLV
015300980302     C*
015400980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
015500980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
015600980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
015700980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
015800980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
015900980302     C                   EXSR      CALLNA
016000980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
016100980302     C                   Z-ADD     WOLIN         WLNA
016200980220X   5C                   ELSE                                                   *LIVELLO CAP
016300980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
016400980302     C                   Z-ADD     DEPLIN        WLNA
016500980220E   5C                   ENDIF
016600020925     C*
016700020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
016800020925do  5C     1             DO        250           J
016900020925if  6C                   IF        OTFP(J) > *ZEROS
017000020925     C                   Z-ADD     OTFP(J)       DEPTFP
017100020925     C                   EXSR      CREDLY
017200040415     C                   Z-ADD     *ZEROS        DEPTIC
017300040415     C                   Z-ADD     *ZEROS        DEPTCC
017400020925e   6C                   ENDIF
017500020925e   5C                   ENDDO
017600980216     C*
017700980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
017800980216     C                   EXSR      LETCPL
017900980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
018000000000     C*
018100980216     C* LETTURA RECORD SUCCESSIVO -CAP-
018200000000     C                   EXSR      LETFIL
018300980216E   3C                   ENDDO                                                  *ROTTURA CAP
018400980216E   2C                   ENDDO                                                  *FINE -CAP-
018500000000E   1C                   ENDIF
018600000000     C*
018700000000     C* OPERAZIONI DI CHIUSURA
018800971020     C                   EXSR      FINSR
018900020925E   0C                   ENDIF
019000000000     C*
019100000000     C                   SETON                                        LR
019200000000     C*------------------------------------------------------------------------*
019300980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
019400000000     C*------------------------------------------------------------------------*
019500000000     C     SETFIL        BEGSR
019600000000     C*
019700970707     C                   Z-ADD     P96VER        KCCVER
019800970707     C     KEYCPC        SETLL     AZCPC01L                           99
019900000000IF  1C     *IN99         IFEQ      *ON
020000000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
020100000000X   1C                   ELSE
020200000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
020300000000E   1C                   ENDIF
020400000000     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
020800000000     C*------------------------------------------------------------------------*
020900000000     C     LETFIL        BEGSR
021000000000     C*
021100000000     C* LEGGE FINO A:
021200000000     C*     - FINE FILE
021300000000     C*     - TROVATO RECORD VALIDO
021400000000     C                   MOVEL     'N'           WRECOK
021500000000DO  1C     $FINE         DOWEQ     '0'
021600000000     C     WRECOK        ANDEQ     'N'
021700970707     C     KEYCPC        READE     AZCPC01L                               99
021800000000IF  2C     *IN99         IFEQ      *ON
021900000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
022000000000X   2C                   ELSE
022100000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
022200000000E   2C                   ENDIF
022300000000E   1C                   ENDDO
022400960701     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
022800000000     C*------------------------------------------------------------------------*
022900000000     C     CHKREC        BEGSR
023000000000     C*
023100000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
023200000000     C*
023300000000     C* CAP ESTERO, ESCLUDE
023400970707IF  1C     CPCNAR        IFNE      *BLANKS
023500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023600000000E   1C                   ENDIF
023700000000     C*
023800000000     C* CAP FITTIZIO, ESCLUDE
023900970707IF  1C     CPCFIT        IFEQ      'S'
024000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024100000000E   1C                   ENDIF
024200980217     C*
024300980217     C* CAP OBSOLETO, ESCLUDE
024400980217IF  1C     CPCFI1        IFEQ      'O'
024500980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024600980217E   1C                   ENDIF
024700000000     C*
024800000000     C                   ENDSR
024900980302     C*------------------------------------------------------------------------*
025000980302     C* CALLNA - CALCOLA LA LINEA ARRIVO DA CONSIDERARE
025100980302     C*------------------------------------------------------------------------*
025200980302     C     CALLNA        BEGSR
025300020802     C*
025400980302     C                   Z-ADD     WILNA         WOLIN                          *LINEA STANDARD
025500980302     C*
025600980302     C                   ENDSR
025700980216     C*------------------------------------------------------------------------*
025800980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
025900980216     C*------------------------------------------------------------------------*
026000980216     C     NEWCAP        BEGSR
026100980216     C*
026200980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
026300980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
026400980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
026500980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
026600980220     C*
026700980220     C* AZZERA LE VARIABILI DI LAVORO
026800980220     C                   Z-ADD     *ZEROS        DEPTIC                         *TEMPO INTERNO CORRI
026900980220     C                   Z-ADD     *ZEROS        DEPTCC                         *TEMPO CLIENTE CORRI
027000980220     C                   Z-ADD     *ZEROS        DEPTIE                         *TEMPO INTERNO ESPRE
027100980220     C                   Z-ADD     *ZEROS        DEPTCE                         *TEMPO CLIENTE ESPRE
027200980216     C*
027300980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
027400980216     C                   Z-ADD     1             I
027500980216     C                   SETOFF                                           28
027600980216     C     CPCPRV        LOOKUP    PRV(I)                                 28
027700980216IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
027800980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
027900980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
028000980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
028100980216X   1C                   ELSE                                                   *NON TROVATO
028200980216     C                   MOVEL     *BLANKS       DEPFCR
028300980216     C                   MOVEL     *BLANKS       DEPRAP
028400980216     C                   MOVEL     *BLANKS       DEPCOR
028500980216E   1C                   ENDIF
028600980216     C*
028700980216     C                   ENDSR
028800980216     C*------------------------------------------------------------------------*
028900980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
029000980216     C*------------------------------------------------------------------------*
029100980216     C     SETCPL        BEGSR
029200980216     C*
029300980216     C* REIMPOSTA LE VARIABILI DI LAVORO
029400980216     C                   MOVEL     '0'           $FINL
029500980216     C*
029600980216     C                   Z-ADD     P96VER        KCLVER
029700980216     C                   MOVEL     DEPNAR        KCLNAR
029800980216     C                   MOVEL     DEPCAP        KCLCAP
029900980216     C     KEYCPL        SETLL     AZCPL01L                           98
030000980216IF  1C     *IN98         IFEQ      *ON
030100980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
030200980216X   1C                   ELSE
030300980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
030400980216E   1C                   ENDIF
030500980216     C*
030600980216     C                   ENDSR
030700980216     C*------------------------------------------------------------------------*
030800980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
030900980216     C*------------------------------------------------------------------------*
031000980216     C     LETCPL        BEGSR
031100980216     C*
031200980216     C* LEGGE FINO A:
031300980216     C*     - FINE FILE
031400980216     C*     - TROVATO RECORD VALIDO
031500980216     C                   MOVEL     'N'           WRECOK
031600980216DO  1C     $FINL         DOWEQ     '0'
031700980216     C     WRECOK        ANDEQ     'N'
031800980216     C     KEYCPL        READE     AZCPL01L                               98
031900980216IF  2C     *IN98         IFEQ      *ON
032000980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
032100980216X   2C                   ELSE
032200980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
032300980216E   2C                   ENDIF
032400980216E   1C                   ENDDO
032500980216     C*
032600980216     C                   ENDSR
032700980216     C*------------------------------------------------------------------------*
032800980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
032900980216     C*------------------------------------------------------------------------*
033000980216     C     CHKCPL        BEGSR
033100980216     C*
033200980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
033300980216     C*
033400980217     C* LOCALITA' OBSOLETA, ESCLUDE
033500980217IF  1C     CPLFI1        IFEQ      'O'
033600980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
033700980217E   1C                   ENDIF
033800980216     C*
033900980216     C                   ENDSR
034000980216     C*------------------------------------------------------------------------*
034100980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
034200980216     C*------------------------------------------------------------------------*
034300980216     C     CREDLY        BEGSR
034400980216     C*
034500050318     C                   CLEAR                   WADLY000
034600980216     C*---
034700980216     C* DATI DEDOTTI
034800980216     C*---
034900980216     C                   MOVEL     DEPTFP        DLYLNP                         *LINEA PARTENZA
035000980216     C                   MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
035100980216     C                   MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
035200980216     C                   MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
035300980216     C*---
035400980216     C* DATI CAPPARIO
035500980216     C*---
035600980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
035700980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
035800980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
035900980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
036000980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
036100980219     C                   MOVEL     WDLV          DLYDLV                         *STAMPA LOCALITA'
036200980216     C*---
036300980216     C* DATI CALCOLATI
036400980216     C*---
036500980220     C                   MOVEL     'C'           DLYTSP                         *TIPO SERVIZIO
036600980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
036700980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
036800040415     C     DEPTIC        ANDNE     *ZEROS                                       *CAMBIO D TP
036900040415     C     DEPTCC        ANDNE     *ZEROS                                       *CAMBIO D TP
037000980220     C                   Z-ADD     DEPTIC        DLYTRI                         *TEMPO INTERNO CORRI
037100980220     C                   Z-ADD     DEPTCC        DLYTRC                         *TEMPO CLIENTE CORRI
037200980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
037300040415     C                                                                          *'L' O NEW TERM.PART
037400980216     C*
037500980216     C* RECUPERA IL TERMINAL ARRIVO DEL CAP
037600980216     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
037700980216     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
037800980216     C                   Z-ADD     WLNA          D55LIN                         *LINEA
037900020527     C                   Z-ADD     P96DRI        D55DRF                         *DATA RIFERIMENTO
038000980216     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
038100980216     C                   CALL      'FNLV55R'
038200020925     C                   PARM                    FNLV55DS
038300980220IF  2C     D55ERR        IFNE      *BLANKS                                      *ERRORE
038400980216     C                   Z-ADD     WLNA          D55TFA                         *SE STESSA
038500980220E   2C                   ENDIF
038600980216     C                   Z-ADD     D55TFA        DEPTFA                         *TERMINAL ARRIVO
038700980216     C* CORRIERE
038800980216     C                   CLEAR                   SD99DS
038900980216     C                   MOVEL     'C'           D98TSP                         *TIPO SERVIZIO
039000020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
039100980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
039200980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
039300980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
039400980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
039500980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
039600980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
039700980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
039800980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
039900980216     C                   Z-ADD     1             D98NCD                         *COLLI
040000980216     C                   CALL      'TNSD99R'
040100980216     C                   PARM                    SD99DS
040200980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
040300980220IF  3C     D98TRI        IFGT      24
040400980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
040500980220X   3C                   ELSE
040600980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
040700980220E   3C                   ENDIF
040800980220IF  3C     D98TRC        IFGT      24
040900980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
041000980220X   3C                   ELSE
041100980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
041200980220E   3C                   ENDIF
041300980220X   2C                   ELSE                                                   *ERRORI
041400980216     C                   Z-ADD     *ZEROS        DLYTRI
041500980216     C                   Z-ADD     *ZEROS        DLYTRC
041600980220E   2C                   ENDIF
041700980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
041800980220     C                   Z-ADD     DLYTRI        DEPTIC                         *TEMPO INTERNO CORRI
041900980220     C                   Z-ADD     DLYTRC        DEPTCC                         *TEMPO CLIENTE CORRI
042000980220E   2C                   ENDIF
042100980220E   1C                   ENDIF
042200050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
042300980216     C* ESPRESSO
042400980220     C                   MOVEL     'E'           DLYTSP                         *TIPO SERVIZIO
042500980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
042600980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
042700980220     C                   Z-ADD     DEPTIE        DLYTRI                         *TEMPO INTERNO ESPRE
042800980220     C                   Z-ADD     DEPTCE        DLYTRC                         *TEMPO CLIENTE ESPRE
042900980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
043000980216     C                   CLEAR                   SD99DS
043100980216     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO
043200020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
043300980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
043400980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
043500980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
043600980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
043700980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
043800980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
043900980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
044000980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
044100980216     C                   Z-ADD     1             D98NCD                         *COLLI
044200980216     C                   CALL      'TNSD99R'
044300980216     C                   PARM                    SD99DS
044400980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
044500980220IF  3C     D98TRI        IFGT      24
044600980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
044700980220X   3C                   ELSE
044800980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
044900980220E   3C                   ENDIF
045000980220IF  3C     D98TRC        IFGT      24
045100980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
045200980220X   3C                   ELSE
045300980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
045400980220E   3C                   ENDIF
045500980220X   2C                   ELSE                                                   *ERRORI
045600980216     C                   Z-ADD     *ZEROS        DLYTRI
045700980216     C                   Z-ADD     *ZEROS        DLYTRC
045800980220E   2C                   ENDIF
045900980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
046000980220     C                   Z-ADD     DLYTRI        DEPTIE                         *TEMPO INTERNO ESPRE
046100980220     C                   Z-ADD     DLYTRC        DEPTCE                         *TEMPO CLIENTE ESPRE
046200980220E   2C                   ENDIF
046300980220E   1C                   ENDIF
046400050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
046500980216     C*
046600980216     C                   ENDSR
046700000000     C*------------------------------------------------------------------------*
046800971020     C* FINSR - OPERAZIONI FINALI
046900000000     C*------------------------------------------------------------------------*
047000971020     C     FINSR         BEGSR
047100971020     C*
047200971020     C* CHIUDE I FILE DI LAVORO
047300050318     C                   CLOSE     WADLY01L
047400020925     C*
047500020925     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
047600020925     C                   MOVEL(P)  'SDD'         KBECOD
047700020925     C                   MOVEL(P)  '1'           KBEKE1
047800020925     C     KEYTBE        CHAIN     TNTBE01L
047900020925IF  1C                   IF        %found(TNTBE01L)
048000020925     C                   Z-ADD     DATCOR        §SDDDLY
048100020925     C                   MOVEL     DSDD          TBEUNI
048200020925     C                   UPDATE    TNTBE000
048300020925E   1C                   ENDIF
048400971020     C*
048500980211     C* CHIUDE I PGM CHIAMATI
048600980210     C                   CLEAR                   SD99DS                         *CALCOLA DELIVERY
048700000000     C                   MOVEL     'C'           D98TLA
048800971009     C                   CALL      'TNSD99R'
048900980210     C                   PARM                    SD99DS
049000980211     C*
049100020925     C                   CLEAR                   FNLV55DS                       *RECUPERA TERMINALs
049200971020     C                   MOVEL     'C'           D55TLA
049300971020     C                   CALL      'FNLV55R'
049400020925     C                   PARM                    FNLV55DS
049500000000     C*
049600000000     C                   ENDSR
049700000000     C*------------------------------------------------------------------------*
049800050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
049900000000     C*------------------------------------------------------------------------*
050000000000     C     OPNFIL        BEGSR
050100020925     c*
050200050318     c* Pulisce il file
050300000000     C                   Z-ADD     80            LENGH            15 5
050400030304     C                   MOVEL(P)  CMD(1)        COMMAN           80
050500050318     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
050600030304     C                   PARM                    COMMAN
050700000000     C                   PARM                    LENGH
050800021202     C                   IF        %error
050900021202     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
051000021202     C                   ENDIF
051100020925     C*
051200050318     C* Apre il file
051300050318     C                   OPEN      WADLY01L                                     *APRE FILE
051400960701     C*
051500000000     C                   ENDSR
051600960709     C*------------------------------------------------------------------------*
051700980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
051800960709     C*------------------------------------------------------------------------*
051900960709     C     CARTAB        BEGSR
052000020925     C*---
052100020925     C* Dati statistiche
052200020925     C*---
052300020925     C                   CLEAR                   DSDD
052400020925     C                   MOVEL(P)  'SDD'         KBECOD
052500020925     C                   MOVEL(P)  '1'           KBEKE1
052600020925     C     KEYTBE        CHAIN(N)  TNTBE01L
052700020925     C                   IF        %found(TNTBE01L)
052800020925     C                   MOVEL     TBEUNI        DSDD
052900020925     C                   ENDIF
053000980211     C*---
053100980211     C* PROVINCIE
053200980211     C*---
053300980216     C                   Z-ADD     *ZEROS        I                 5 0
053400020919     C                   Z-ADD     1             KTBKUT
053500960709     C                   MOVEL     'PR'          KTBCOD
053600960709     C     KE2TAB        SETLL     TABEL00F
053700960709     C     KE2TAB        READE     TABEL00F                               99
053800960709DO  1C     *IN99         DOWEQ     *OFF
053900960709IF  2C     TBLFLG        IFEQ      *BLANKS
054000960709     C                   ADD       1             I
054100960709     C                   MOVEL     TBLUNI        DSPR
054200960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
054300960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
054400960709E   2C                   ENDIF
054500960709     C     KE2TAB        READE     TABEL00F                               99
054600960709E   1C                   ENDDO
054700980211     C*---
054800960709     C* DATI CODICI TASSAZIONE
054900980211     C*---
055000020919     C                   Z-ADD     1             KTBKUT
055100960709     C                   MOVEL     'CT'          KTBCOD
055200041213DO  1C     1             DO        600           I
055300960709IF  2C     PRV(I)        IFNE      *BLANKS
055400960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
055500960709     C     KEYTAB        CHAIN     TABEL00F                           99
055600960709IF  3C     *IN99         IFEQ      *OFF
055700960709     C     TBLFLG        ANDEQ     *BLANKS
055800960709     C                   MOVEL     TBLUNI        DSCT
055900960709     C                   MOVEL     §CTFCR        PFCR(I)                        *CAPOLUOGO DI REGION
056000960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
056100960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
056200960709E   3C                   ENDIF
056300020925E   3C                   ENDIF
056400960709E   1C                   ENDDO
056500980302     C*---
056600010716     C* TERMINAL SU ELABORATORE
056700020925     c* SEDE    --> TUTTI I TERMINAL PARTENZA
056800020925     c* FILIALE --> TUTTI i TERMINAL PARTENZA DELL'AREA
056900980302     C*---
057000020925     c                   CLEAR                   OFil
057100020925     c                   CLEAR                   OTfp
057200020925IF  1C                   IF        DUTLPO = 'S'                                 TRASMISSIONE IN SEDE
057300020925     C                   clear                   trul26ds
057400020925     C                   EVAL      I26TLA = 'L'
057500020925     C                   EVAL      I26TPT = 'P'
057600020925     C                   CALL      'TRUL26R'
057700020925     C                   PARM                    trul26ds
057800020925     C                   Z-ADD     skpot         OFil                           *terminal -TUTTI-
057900020925X   1C                   ELSE                                                   RICEZIONE IN FILIALE
058000020925     C                   CLEAR                   tibs56ds
058100020925     C                   EVAL      I56PPO = DUTPOU
058200020925     C                   EVAL      I56MOD = 'POA'
058300020925     C                   CALL      'TIBS56R'
058400020925     C                   PARM                    tibs56ds
058500020925do  2C     1             DO        250           I
058600020925if  3C                   IF        ski(I) > *ZEROS
058700020925     C                   CLEAR                   FNLV55DS
058800020925     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
058900020925     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
059000020925     C                   Z-ADD     ski(I)        D55LIN                         *LINEA
059100020925     C                   Z-ADD     p96dri        D55DRF                         *DATA RIFERIMENTO
059200020925     C                   CALL      'FNLV55R'
059300020925     C                   PARM                    FNLV55DS
059400020925if  4C     D55ERR        IFNE      *BLANKS                                      *ERRORE
059500020925     C                   Z-ADD     dutpou        D55TFP                         *SE STESSA
059600020925e   4C                   ENDIF
059700020925     c                   Z-ADD     d55tfp        ski(I)                         *FIL diventa TFP
059800020925e   3C                   ENDIF
059900020925e   2C                   ENDDO
060000020925     C                   Z-ADD     ski           OFil                           *terminal -AREA-
060100020925e   1C                   ENDIF
060200020925     c*
060300020925     c* Estrapola SOLO i terminal
060400010716     C                   Z-ADD     *ZEROS        J                 5 0
060500020925do  1C     1             DO        250           I
060600020925if  2C                   IF        OFil(I) > *ZEROS
060700020925     C     OFil(i)       LOOKUP    OTfp                                   55
060800020925if  3C                   IF        *IN55 = *OFF                                 *NON TROVATO
060900010716     C                   ADD       1             J
061000020925     C                   Z-ADD     OFil(i)       OTfp(J)
061100020925e   3C                   ENDIF
061200020925e   2C                   ENDIF
061300020925e   1C                   ENDDO
061400960709     C*
061500960709     C                   ENDSR
061600020919     C*--------------------------------------------------------------------------------------------*
061700020919     C* REPERISCE I DATI UTENTE
061800020919     C*--------------------------------------------------------------------------------------------*
061900020919     C     REPDATIUTE    BEGSR
062000020919     C*
062100020919     C* INIZIALIZZA VARIABILI DI WRK
062200020919     C                   CLEAR                   TIBS34DS
062300020919     C                   CLEAR                   AZUTEDS
062400020919     C                   CLEAR                   DDATIUTE
062500020919     C*
062600020919     C     *DTAARA       DEFINE    §azute        azuteds
062700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
062800020919     C                   IN(E)     *DTAARA
062900020919if  1C                   IF        %Error
063000020919     c                   EVAL      I34Tla = 'L'
063100020919     C                   CALL      'TIBS34R'
063200020919     C                   PARM                    Tibs34Ds
063300020919     C                   IN        *DTAARA
063400020919e   1C                   ENDIF
063500020919     C*
063600020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
063700020919     C                   MOVEL(P)  RSUT          DSFIRS           20
063800020919     C*
063900020919     C                   ENDSR
064000000000     C*------------------------------------------------------------------------*
064100000000     C* *INZSR - OPERAZIONI INIZIALI
064200000000     C*------------------------------------------------------------------------*
064300000000     C     *INZSR        BEGSR
064400980211     C*---
064500000000     C* RICEVIMENTO PARAMETRI
064600980211     C*---
064700000000     C     *ENTRY        PLIST
064800000000     C                   PARM                    KPJBA
064900000000     C                   MOVEL     KPJBU         PARAM
065000980211     C*---
065100000000     C* VARIABILI RIFERITE AL DATABASE
065200980211     C*---
065300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
065400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
065500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
065600020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
065700020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
065800020925     C     *LIKE         DEFINE    TBLKEY        KTBKEY
065900980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
066000980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
066100980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
066200980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
066300980216     C     *LIKE         DEFINE    CPCLNA        DEPTFP                         *DEPOSITI
066400980216     C     *LIKE         DEFINE    CPCLNA        DEPTFA
066500980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
066600980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
066700980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
066800980216     C     *LIKE         DEFINE    DLYFCR        DEPFCR
066900980216     C     *LIKE         DEFINE    DLYRAP        DEPRAP
067000980216     C     *LIKE         DEFINE    DLYCOR        DEPCOR
067100980302     C     *LIKE         DEFINE    CPCLNA        DEPLIN
067200980220     C     *LIKE         DEFINE    DLYTRI        DEPTIC
067300980220     C     *LIKE         DEFINE    DLYTRC        DEPTCC
067400980220     C     *LIKE         DEFINE    DLYTRI        DEPTIE
067500980220     C     *LIKE         DEFINE    DLYTRC        DEPTCE
067600980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
067700980216     C     *LIKE         DEFINE    CPCISO        WISO
067800980216     C     *LIKE         DEFINE    CPCLNA        WLNA
067900980219     C     *LIKE         DEFINE    CPLDLV        WDLV
068000980302     C     *LIKE         DEFINE    CPCLNA        WILNA
068100980302     C     *LIKE         DEFINE    CPCLOL        WILOL
068200980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
068300980211     C*---
068400000000     C* CHIAVI DI LETTURA
068500980211     C*---
068600980211     C     KEYTAB        KLIST                                                  *TABEL00F
068700980211     C                   KFLD                    KTBKUT                          -UTENTE
068800980211     C                   KFLD                    KTBCOD                          -TABELLA
068900980211     C                   KFLD                    KTBKEY                          -CHIAVE
069000980211     C     KE2TAB        KLIST                                                  *TABEL00F
069100980211     C                   KFLD                    KTBKUT                          -UTENTE
069200980211     C                   KFLD                    KTBCOD                          -TEBELLA
069300020925     C     KEYTBE        KLIST                                                  *TNTBE01L
069400020925     C                   KFLD                    KBECOD                          -TABELLA
069500020925     C                   KFLD                    KBEKE1                          -CHIAVE
069600980211     C     KEYCPC        KLIST                                                  *AZCPC00F
069700980211     C                   KFLD                    KCCVER                          -VERSIONE
069800980211     C     KEYCPL        KLIST                                                  *AZCPL00F
069900980211     C                   KFLD                    KCLVER                          -VERSIONE
070000980216     C                   KFLD                    KCLNAR                          -NAZIONE
070100980216     C                   KFLD                    KCLCAP                          -CAP
070200980211     C*---
070300000000     C* VARIABILI DI CONTROLLO
070400980211     C*---
070500980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
070600980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
070700980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
070800980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
070900050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
071000020527     C*
071100020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
071200020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
071300020527     C                   Z-ADD     N8            G08DAT
071400020527     C                   Z-ADD     *ZEROS        G08INV
071500020527     C                   MOVEL     '0'           G08ERR
071600020527     C                   CALL      'XSRDA8'
071700020527     C                   PARM                    WLBDA8
071800020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
071900020925     C*
072000020925     C* REPERISCE I DATI UTENTE
072100020925     C                   EXSR      REPDATIUTE
072200970707     C*
072300020527     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
072400020527     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
072500020919IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
072600030304     C***                Z-ADD     *ZEROS        P96DRI            8 0
072700030304     C                   Z-ADD     DATCOR        P96DRI            8 0
072800020527X   1C                   ELSE                                                   S E D E
072900020527     C                   Z-ADD     DATCOR        P96DRI            8 0
073000020527E   1C                   ENDIF
073100970707     C                   Z-ADD     *ZEROS        P96VER            5 0
073200970707     C                   CALL      'TISI96R'
073300970707     C                   PARM                    P96DRI
073400970707     C                   PARM                    P96VER
073500040415     C*
073600040415     C* REIPOSTO LA DATA CORRENTE ALLA DATA D RIFERIMENTO X IL REPERIMENTO VERSIONE CAPPARIO ALLA
073700040415     C* DATA CORRENTE XCHE' IN USCITA DAL *PGM TISI96R VALORIZZA IL P96DRI CON LA DATA D DECORRENZA
073800040415     C* DELLA VERSIONE REPERITA
073900040415     C                   Z-ADD     DATCOR        P96DRI
074000010717     C*
074100010717     C* CARICA LE TABELLE OCCORRENTI
074200010717     C                   EXSR      CARTAB
074300960709     C*
074400000000     C                   ENDSR
074500000000     C*------------------------------------------------------------------------*
074600000000**   CMD -COMANDI CLP-
074700050318CLRPFM FILE(WADLY00F)
