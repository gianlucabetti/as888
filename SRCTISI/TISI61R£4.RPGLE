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
002000091211     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
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
008400091204     d  PARlin               234    234
008500091204     d  PARold               235    235
008600091204     d  PARdtEla             236    243
008700091211     d  PARelaprova          244    244
008800980210     D*-------------------
008900980210     D* ARCHITETTURA
009000980210     D*-------------------
009100980210     D KPJBA         E DS
009200020830     D*-------------------
009300020925     D* DS REPERIMENTO P.O. IN GESTIONE
009400020830     D*-------------------
009500020925     D tibs56ds      E DS                                                       *per FILIALE remota
009600020830     D  ski                    7    756  0 dim(250)
009700020925     D trul26ds      E DS                                                       *per SEDE sede
009800020925     D  skpo                  11    760  0 dim(250)
009900020925     D  skpot                761   1510  0 dim(250)
010000051116      *
010100051116      * V A R I A B I L I
010200051116      *
010300051116     d XX              s              3  0 inz
010400000000     C*------------------------------------------------------------------------*
010500000000     C* MAIN LINE
010600000000     C*------------------------------------------------------------------------*
010700051116      *
010800051116      * Carica le tabelle occorrenti
010900051116     c                   exsr      CarTAB
011000020925     C*
011100050318     C* PULISCE E RICREA IL FILE SOLO SE:
011200020925     C* data corrente > data ultima creazione workfile
011300020925IF  0c     DATCOR        IFGT      §SDDDLY
011400020925     C*
011500050318     C* OPERAZIONI INIZIALI APERTURA FILE
011600020925     C                   EXSR      OPNFIL
011700000000     C*
011800980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
011900980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
012000000000     C                   EXSR      SETFIL
012100000000     C*
012200000000     C* CICLO FINO A FINE FILE
012300000000DO  2C     $FINE         DOWEQ     '0'
012400980216     C*
012500980216     C* OPERAZIONI PER NUOVO CAP
012600980216     C                   EXSR      NEWCAP
012700980216     C*
012800980216     C* CICLO FINO A ROTTURA CAP
012900980216DO  3C     $FINE         DOWEQ     '0'
013000980216     C     CPCNAR        ANDEQ     DEPNAR
013100980216     C     CPCCAP        ANDEQ     DEPCAP
013200980302     C*
013300980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
013400980302     C                   Z-ADD     CPCLNA        WILNA                          *LINEA ARRIVO
013500980302     C                   Z-ADD     CPCLOL        WILOL                          *LINEA OLTRE
013600980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
013700980302     C                   EXSR      CALLNA
013800980302     C                   Z-ADD     WOLIN         DEPLIN                         *DEPOSITO LNA CAP
013900980302     C*
014000020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
014100980220     C                   MOVEL     'C'           $DLY
014200980216     C                   MOVEL     *BLANKS       WLOC
014300980216     C                   MOVEL     CPCISO        WISO
014400980302     C                   Z-ADD     WOLIN         WLNA
014500980219     C                   MOVEL     'N'           WDLV
014600020925do  4C     1             DO        250           J
014700020925if  5C                   IF        OTFP(J) > *ZEROS
014800010716     C                   Z-ADD     OTFP(J)       DEPTFP
014900980210     C                   EXSR      CREDLY
015000040415     C                   Z-ADD     *ZEROS        DEPTIC
015100040415     C                   Z-ADD     *ZEROS        DEPTCC
015200090515     C                   Z-ADD     *ZEROS        DEPTIe
015300090515     C                   Z-ADD     *ZEROS        DEPTCe
015400090921     C                   Z-ADD     *ZEROS        DEPTIh
015500090921     C                   Z-ADD     *ZEROS        DEPTCh
015600020925e   5C                   ENDIF
015700020925e   4C                   ENDDO
015800980216     C*
015900980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
016000980216     C                   EXSR      SETCPL
016100980216     C*
016200980216     C* CICLO FINO A FINE FILE
016300980216DO  4C     $FINL         DOWEQ     '0'
016400980216     C*
016500980216     C* SCRIVE IL RECORD DEL WORKFILE
016600980220     C                   MOVEL     'L'           $DLY
016700980216     C                   MOVEL     CPLLOC        WLOC
016800980220     C                   MOVEL     CPLDLV        WDLV
016900980302     C*
017000980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
017100980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
017200980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
017300980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
017400980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
017500980302     C                   EXSR      CALLNA
017600980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
017700980302     C                   Z-ADD     WOLIN         WLNA
017800980220X   5C                   ELSE                                                   *LIVELLO CAP
017900980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
018000980302     C                   Z-ADD     DEPLIN        WLNA
018100980220E   5C                   ENDIF
018200020925     C*
018300020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
018400020925do  5C     1             DO        250           J
018500020925if  6C                   IF        OTFP(J) > *ZEROS
018600020925     C                   Z-ADD     OTFP(J)       DEPTFP
018700020925     C                   EXSR      CREDLY
018800040415     C                   Z-ADD     *ZEROS        DEPTIC
018900040415     C                   Z-ADD     *ZEROS        DEPTCC
019000090515     C                   Z-ADD     *ZEROS        DEPTIe
019100090515     C                   Z-ADD     *ZEROS        DEPTCe
019200090921     C                   Z-ADD     *ZEROS        DEPTIh
019300090921     C                   Z-ADD     *ZEROS        DEPTCh
019400020925e   6C                   ENDIF
019500020925e   5C                   ENDDO
019600980216     C*
019700980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
019800980216     C                   EXSR      LETCPL
019900980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
020000000000     C*
020100980216     C* LETTURA RECORD SUCCESSIVO -CAP-
020200000000     C                   EXSR      LETFIL
020300980216E   3C                   ENDDO                                                  *ROTTURA CAP
020400980216E   2C                   ENDDO                                                  *FINE -CAP-
020500000000E   1C                   ENDIF
020600000000     C*
020700000000     C* OPERAZIONI DI CHIUSURA
020800971020     C                   EXSR      FINSR
020900020925E   0C                   ENDIF
021000000000     C*
021100000000     C                   SETON                                        LR
021200000000     C*------------------------------------------------------------------------*
021300980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
021400000000     C*------------------------------------------------------------------------*
021500000000     C     SETFIL        BEGSR
021600000000     C*
021700970707     C                   Z-ADD     P96VER        KCCVER
021800970707     C     KEYCPC        SETLL     AZCPC01L                           99
021900000000IF  1C     *IN99         IFEQ      *ON
022000000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
022100000000X   1C                   ELSE
022200000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
022300000000E   1C                   ENDIF
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
022800000000     C*------------------------------------------------------------------------*
022900000000     C     LETFIL        BEGSR
023000000000     C*
023100000000     C* LEGGE FINO A:
023200000000     C*     - FINE FILE
023300000000     C*     - TROVATO RECORD VALIDO
023400000000     C                   MOVEL     'N'           WRECOK
023500000000DO  1C     $FINE         DOWEQ     '0'
023600000000     C     WRECOK        ANDEQ     'N'
023700970707     C     KEYCPC        READE     AZCPC01L                               99
023800000000IF  2C     *IN99         IFEQ      *ON
023900000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
024000000000X   2C                   ELSE
024100000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
024200000000E   2C                   ENDIF
024300000000E   1C                   ENDDO
024400960701     C*
024500000000     C                   ENDSR
024600000000     C*------------------------------------------------------------------------*
024700980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
024800000000     C*------------------------------------------------------------------------*
024900000000     C     CHKREC        BEGSR
025000000000     C*
025100000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
025200000000     C*
025300000000     C* CAP ESTERO, ESCLUDE
025400970707IF  1C     CPCNAR        IFNE      *BLANKS
025500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025600000000E   1C                   ENDIF
025700000000     C*
025800000000     C* CAP FITTIZIO, ESCLUDE
025900970707IF  1C     CPCFIT        IFEQ      'S'
026000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026100000000E   1C                   ENDIF
026200980217     C*
026300980217     C* CAP OBSOLETO, ESCLUDE
026400980217IF  1C     CPCFI1        IFEQ      'O'
026500980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026600980217E   1C                   ENDIF
026700000000     C*
026800000000     C                   ENDSR
026900980302     C*------------------------------------------------------------------------*
027000980302     C* CALLNA - CALCOLA LA LINEA ARRIVO DA CONSIDERARE
027100980302     C*------------------------------------------------------------------------*
027200980302     C     CALLNA        BEGSR
027300020802     C*
027400980302     C                   Z-ADD     WILNA         WOLIN                          *LINEA STANDARD
027500980302     C*
027600980302     C                   ENDSR
027700980216     C*------------------------------------------------------------------------*
027800980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
027900980216     C*------------------------------------------------------------------------*
028000980216     C     NEWCAP        BEGSR
028100980216     C*
028200980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
028300980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
028400980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
028500980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
028600980220     C*
028700980220     C* AZZERA LE VARIABILI DI LAVORO
028800980220     C                   Z-ADD     *ZEROS        DEPTIC                         *TEMPO INTERNO CORRI
028900980220     C                   Z-ADD     *ZEROS        DEPTCC                         *TEMPO CLIENTE CORRI
029000090921     C                   Z-ADD     *ZEROS        DEPTIE                         *TEMPO INTERNO "E"
029100090921     C                   Z-ADD     *ZEROS        DEPTCE                         *TEMPO CLIENTE "E"
029200090921     C                   Z-ADD     *ZEROS        DEPTIh                         *TEMPO INTERNO "H"
029300090921     C                   Z-ADD     *ZEROS        DEPTCh                         *TEMPO CLIENTE "H"
029400980216     C*
029500980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
029600980216     C                   Z-ADD     1             I
029700980216     C                   SETOFF                                           28
029800980216     C     CPCPRV        LOOKUP    PRV(I)                                 28
029900980216IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
030000980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
030100980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
030200980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
030300980216X   1C                   ELSE                                                   *NON TROVATO
030400980216     C                   MOVEL     *BLANKS       DEPFCR
030500980216     C                   MOVEL     *BLANKS       DEPRAP
030600980216     C                   MOVEL     *BLANKS       DEPCOR
030700980216E   1C                   ENDIF
030800980216     C*
030900980216     C                   ENDSR
031000980216     C*------------------------------------------------------------------------*
031100980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
031200980216     C*------------------------------------------------------------------------*
031300980216     C     SETCPL        BEGSR
031400980216     C*
031500980216     C* REIMPOSTA LE VARIABILI DI LAVORO
031600980216     C                   MOVEL     '0'           $FINL
031700980216     C*
031800980216     C                   Z-ADD     P96VER        KCLVER
031900980216     C                   MOVEL     DEPNAR        KCLNAR
032000980216     C                   MOVEL     DEPCAP        KCLCAP
032100980216     C     KEYCPL        SETLL     AZCPL01L                           98
032200980216IF  1C     *IN98         IFEQ      *ON
032300980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
032400980216X   1C                   ELSE
032500980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
032600980216E   1C                   ENDIF
032700980216     C*
032800980216     C                   ENDSR
032900980216     C*------------------------------------------------------------------------*
033000980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
033100980216     C*------------------------------------------------------------------------*
033200980216     C     LETCPL        BEGSR
033300980216     C*
033400980216     C* LEGGE FINO A:
033500980216     C*     - FINE FILE
033600980216     C*     - TROVATO RECORD VALIDO
033700980216     C                   MOVEL     'N'           WRECOK
033800980216DO  1C     $FINL         DOWEQ     '0'
033900980216     C     WRECOK        ANDEQ     'N'
034000980216     C     KEYCPL        READE     AZCPL01L                               98
034100980216IF  2C     *IN98         IFEQ      *ON
034200980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
034300980216X   2C                   ELSE
034400980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
034500980216E   2C                   ENDIF
034600980216E   1C                   ENDDO
034700980216     C*
034800980216     C                   ENDSR
034900980216     C*------------------------------------------------------------------------*
035000980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
035100980216     C*------------------------------------------------------------------------*
035200980216     C     CHKCPL        BEGSR
035300980216     C*
035400980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
035500980216     C*
035600980217     C* LOCALITA' OBSOLETA, ESCLUDE
035700980217IF  1C     CPLFI1        IFEQ      'O'
035800980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
035900980217E   1C                   ENDIF
036000980216     C*
036100980216     C                   ENDSR
036200980216     C*------------------------------------------------------------------------*
036300980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
036400980216     C*------------------------------------------------------------------------*
036500980216     C     CREDLY        BEGSR
036600980216     C*
036700050318     C                   CLEAR                   WADLY000
036800980216     C*---
036900980216     C* DATI DEDOTTI
037000980216     C*---
037100980216     C                   MOVEL     DEPTFP        DLYLNP                         *LINEA PARTENZA
037200980216     C                   MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
037300980216     C                   MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
037400980216     C                   MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
037500980216     C*---
037600980216     C* DATI CAPPARIO
037700980216     C*---
037800980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
037900980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
038000980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
038100980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
038200980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
038300980219     C                   MOVEL     WDLV          DLYDLV                         *STAMPA LOCALITA'
038400980216     C*---
038500980216     C* DATI CALCOLATI
038600980216     C*---
038700980220     C                   MOVEL     'C'           DLYTSP                         *TIPO SERVIZIO
038800980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
038900980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
039000040415     C     DEPTIC        ANDNE     *ZEROS                                       *CAMBIO D TP
039100040415     C     DEPTCC        ANDNE     *ZEROS                                       *CAMBIO D TP
039200980220     C                   Z-ADD     DEPTIC        DLYTRI                         *TEMPO INTERNO CORRI
039300980220     C                   Z-ADD     DEPTCC        DLYTRC                         *TEMPO CLIENTE CORRI
039400980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
039500040415     C                                                                          *'L' O NEW TERM.PART
039600980216     C*
039700980216     C* RECUPERA IL TERMINAL ARRIVO DEL CAP
039800980216     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
039900980216     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
040000980216     C                   Z-ADD     WLNA          D55LIN                         *LINEA
040100020527     C                   Z-ADD     P96DRI        D55DRF                         *DATA RIFERIMENTO
040200980216     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
040300980216     C                   CALL      'FNLV55R'
040400020925     C                   PARM                    FNLV55DS
040500980220IF  2C     D55ERR        IFNE      *BLANKS                                      *ERRORE
040600980216     C                   Z-ADD     WLNA          D55TFA                         *SE STESSA
040700980220E   2C                   ENDIF
040800980216     C                   Z-ADD     D55TFA        DEPTFA                         *TERMINAL ARRIVO
040900090921     C* "C"- esxpress
041000980216     C                   CLEAR                   SD99DS
041100980216     C                   MOVEL     'C'           D98TSP                         *TIPO SERVIZIO
041200020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
041300980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
041400980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
041500980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
041600980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
041700980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
041800980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
041900980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
042000980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
042100980216     C                   Z-ADD     1             D98NCD                         *COLLI
042200980216     C                   CALL      'TNSD99R'
042300980216     C                   PARM                    SD99DS
042400980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
042500980220IF  3C     D98TRI        IFGT      24
042600980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
042700980220X   3C                   ELSE
042800980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
042900980220E   3C                   ENDIF
043000980220IF  3C     D98TRC        IFGT      24
043100980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
043200980220X   3C                   ELSE
043300980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
043400980220E   3C                   ENDIF
043500980220X   2C                   ELSE                                                   *ERRORI
043600980216     C                   Z-ADD     *ZEROS        DLYTRI
043700980216     C                   Z-ADD     *ZEROS        DLYTRC
043800980220E   2C                   ENDIF
043900980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
044000980220     C                   Z-ADD     DLYTRI        DEPTIC                         *TEMPO INTERNO CORRI
044100980220     C                   Z-ADD     DLYTRC        DEPTCC                         *TEMPO CLIENTE CORRI
044200980220E   2C                   ENDIF
044300980220E   1C                   ENDIF
044400090921
044500050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
044600090921     c
044700090921     C* "E"- priority
044800980220     C                   MOVEL     'E'           DLYTSP                         *TIPO SERVIZIO
044900980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
045000980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
045100090515     C     DEPTIe        ANDNE     *ZEROS                                       *CAMBIO D TP
045200090515     C     DEPTCe        ANDNE     *ZEROS                                       *CAMBIO D TP
045300980220     C                   Z-ADD     DEPTIE        DLYTRI                         *TEMPO INTERNO ESPRE
045400980220     C                   Z-ADD     DEPTCE        DLYTRC                         *TEMPO CLIENTE ESPRE
045500980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
045600980216     C                   CLEAR                   SD99DS
045700980216     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO
045800020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
045900980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
046000980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
046100980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
046200980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
046300980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
046400980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
046500980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
046600980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
046700980216     C                   Z-ADD     1             D98NCD                         *COLLI
046800980216     C                   CALL      'TNSD99R'
046900980216     C                   PARM                    SD99DS
047000980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
047100980220IF  3C     D98TRI        IFGT      24
047200980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
047300980220X   3C                   ELSE
047400980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
047500980220E   3C                   ENDIF
047600980220IF  3C     D98TRC        IFGT      24
047700980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
047800980220X   3C                   ELSE
047900980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
048000980220E   3C                   ENDIF
048100980220X   2C                   ELSE                                                   *ERRORI
048200980216     C                   Z-ADD     *ZEROS        DLYTRI
048300980216     C                   Z-ADD     *ZEROS        DLYTRC
048400980220E   2C                   ENDIF
048500980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
048600980220     C                   Z-ADD     DLYTRI        DEPTIE                         *TEMPO INTERNO ESPRE
048700980220     C                   Z-ADD     DLYTRC        DEPTCE                         *TEMPO CLIENTE ESPRE
048800980220E   2C                   ENDIF
048900980220E   1C                   ENDIF
049000050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
049100980216     C*
049200090921     C* "H"- H 10 30
049300090921     C                   MOVEL     'H'           DLYTSP                         *TIPO SERVIZIO
049400090921IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
049500090921     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
049600090921     C     DEPTIh        ANDNE     *ZEROS                                       *CAMBIO D TP
049700090921     C     DEPTCh        ANDNE     *ZEROS                                       *CAMBIO D TP
049800090921     C                   Z-ADD     DEPTIh        DLYTRI                         *TEMPO INTERNO ESPRE
049900090921     C                   Z-ADD     DEPTCh        DLYTRC                         *TEMPO CLIENTE ESPRE
050000090921X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
050100090921     C                   CLEAR                   SD99DS
050200090921     C                   MOVEL     'H'           D98TSP                         *TIPO SERVIZIO
050300090921     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
050400090921     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
050500090921     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
050600090921     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
050700090921     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
050800090921     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
050900090921     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
051000090921     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
051100090921     C                   MOVEL     WISO          D98FIN                         *INOLTRO
051200090921     C                   Z-ADD     1             D98NCD                         *COLLI
051300090921     C                   CALL      'TNSD99R'
051400090921     C                   PARM                    SD99DS
051500090921IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
051600090921IF  3C     D98TRI        IFGT      24
051700090921     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
051800090921X   3C                   ELSE
051900090921     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
052000090921E   3C                   ENDIF
052100090921IF  3C     D98TRC        IFGT      24
052200090921     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
052300090921X   3C                   ELSE
052400090921     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
052500090921E   3C                   ENDIF
052600090921X   2C                   ELSE                                                   *ERRORI
052700090921     C                   Z-ADD     *ZEROS        DLYTRI
052800090921     C                   Z-ADD     *ZEROS        DLYTRC
052900090921E   2C                   ENDIF
053000090921IF  2C     $DLY          IFEQ      'C'                                          *CAP
053100090921     C                   Z-ADD     DLYTRI        DEPTIh                         *TEMPO INTERNO ESPRE
053200090921     C                   Z-ADD     DLYTRC        DEPTCh                         *TEMPO CLIENTE ESPRE
053300090921E   2C                   ENDIF
053400090921E   1C                   ENDIF
053500090921     c
053600090921     C                   WRITE     WADLY000                                     I N S E R I M E N T
053700980216     C                   ENDSR
053800000000     C*------------------------------------------------------------------------*
053900971020     C* FINSR - OPERAZIONI FINALI
054000000000     C*------------------------------------------------------------------------*
054100971020     C     FINSR         BEGSR
054200971020     C*
054300971020     C* CHIUDE I FILE DI LAVORO
054400051114     C                   CLOSE     WADLY00F
054500020925     C*
054600020925     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
054700020925     C                   MOVEL(P)  'SDD'         KBECOD
054800020925     C                   MOVEL(P)  '1'           KBEKE1
054900020925     C     KEYTBE        CHAIN     TNTBE01L
055000020925IF  1C                   IF        %found(TNTBE01L)
055100020925     C                   Z-ADD     DATCOR        §SDDDLY
055200020925     C                   MOVEL     DSDD          TBEUNI
055300020925     C                   UPDATE    TNTBE000
055400020925E   1C                   ENDIF
055500971020     C*
055600980211     C* CHIUDE I PGM CHIAMATI
055700980210     C                   CLEAR                   SD99DS                         *CALCOLA DELIVERY
055800000000     C                   MOVEL     'C'           D98TLA
055900971009     C                   CALL      'TNSD99R'
056000980210     C                   PARM                    SD99DS
056100980211     C*
056200020925     C                   CLEAR                   FNLV55DS                       *RECUPERA TERMINALs
056300971020     C                   MOVEL     'C'           D55TLA
056400971020     C                   CALL      'FNLV55R'
056500020925     C                   PARM                    FNLV55DS
056600000000     C*
056700000000     C                   ENDSR
056800000000     C*------------------------------------------------------------------------*
056900050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
057000000000     C*------------------------------------------------------------------------*
057100000000     C     OPNFIL        BEGSR
057200091211     C                   Z-ADD     80            LENGH            15 5
057300091211     c* se si tratta di una prova, creo il file, vuoto in EDPES
057400091211    1c                   if        parelaprova='S'
057500091211     c* creazione file
057600091211     C                   MOVEL(P)  CMD(2)        COMMAN           80
057700091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
057800091211     C                   PARM                    COMMAN
057900091211     C                   PARM                    LENGH
058000091211     c
058100091211     c* Se c'e' già lo pulisco
058200091211    2C                   IF        %error
058300091211     C                   MOVEL(P)  CMD(3)        COMMAN           80
058400091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
058500091211     C                   PARM                    COMMAN
058600091211     c                   PARM                    LENGH
058700091211    3C                   IF        %error
058800091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
058900091211    3C                   ENDIF
059000091211    2C                   ENDIF
059100091211     c
059200091211     c* Eseguo ovrdbf
059300091211     C                   MOVEL(P)  CMD(4)        COMMAN           80
059400091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
059500091211     C                   PARM                    COMMAN
059600091211     c                   PARM                    LENGH
059700091211    3C                   IF        %error
059800091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
059900091211    3C                   ENDIF
060000091214     c
060100091211   x1c                   else
060200020925     c*
060300050318     c* Pulisce il file
060400030304     C                   MOVEL(P)  CMD(1)        COMMAN           80
060500050318     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
060600030304     C                   PARM                    COMMAN
060700000000     C                   PARM                    LENGH
060800091211    2C                   IF        %error
060900021202     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
061000091211    2C                   ENDIF
061100091211    1C                   ENDIF
061200020925     C*
061300050318     C* Apre il file
061400051114     C                   OPEN      WADLY00F                                     *APRE FILE
061500960701     C*
061600000000     C                   ENDSR
061700960709     C*------------------------------------------------------------------------*
061800980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
061900960709     C*------------------------------------------------------------------------*
062000960709     C     CARTAB        BEGSR
062100020925     C*---
062200020925     C* Dati statistiche
062300020925     C*---
062400020925     C                   CLEAR                   DSDD
062500020925     C                   MOVEL(P)  'SDD'         KBECOD
062600020925     C                   MOVEL(P)  '1'           KBEKE1
062700020925     C     KEYTBE        CHAIN(N)  TNTBE01L
062800020925     C                   IF        %found(TNTBE01L)
062900020925     C                   MOVEL     TBEUNI        DSDD
063000020925     C                   ENDIF
063100980211     C*---
063200980211     C* PROVINCIE
063300980211     C*---
063400980216     C                   Z-ADD     *ZEROS        I                 5 0
063500020919     C                   Z-ADD     1             KTBKUT
063600960709     C                   MOVEL     'PR'          KTBCOD
063700960709     C     KE2TAB        SETLL     TABEL00F
063800960709     C     KE2TAB        READE     TABEL00F                               99
063900960709DO  1C     *IN99         DOWEQ     *OFF
064000960709IF  2C     TBLFLG        IFEQ      *BLANKS
064100960709     C                   ADD       1             I
064200960709     C                   MOVEL     TBLUNI        DSPR
064300960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
064400960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
064500051116     c                   movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
064600960709E   2C                   ENDIF
064700960709     C     KE2TAB        READE     TABEL00F                               99
064800960709E   1C                   ENDDO
064900051116      *
065000051116      * Imposta il progressivo per le province (diverse) con lo stesso
065100051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
065200051116      *   - che NON può essere uguale per due province diverse:
065300051116      *   ha causato confusione nel reperimento dei tempi di consegna
065400051116      *   e/o nell'elenco delle località)
065500051116     c                   clear                   xx
065600051116do  1c                   DOW       xx       <  %elem(PRV)
065700051116     c                   add       1             xx
065800051116if  2c                   if        Prv(xx)  =  *blanks
065900051116     c                   leave
066000051116e   2c                   endif
066100051116      * - scarto le province per le quali è già stata impostata la
066200051116      *   "sigla" finale della provincia per il codice ordinamento
066300051116     c                   if        Pcor(xx) <> *blanks
066400051116     c                   iter
066500051116     c                   endif
066600051116      * - cerco le altre province con lo stesso codice tassazione
066700051116     c                   eval      I        =  xx
066800051116do  2c                   DOW       I        <  %elem(PRV)
066900051116     c                   add       1             I
067000051116     c                   eval      *in28    =  *off
067100051116     c     Pcts(xx)      lookup    Pcts(I)                                28
067200051116if  3c                   if        *in28    =  *off
067300051116     c                   leave
067400051116e   3c                   endif
067500051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
067600051116      *   memorizzo la provincia stessa - da posporre al codice
067700051116      *   ordinamento
067800051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
067900051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
068000051116      *   (ora memorizzo "   LO", "   FO", "   RN")
068100051116if  3c                   if        Pfcr(xx) =  *blanks
068200051116     c                   move      Prv(xx)       Pcor(xx)
068300051116e   3c                   endif
068400051116if  3c                   if        Pfcr(I)  =  *blanks
068500051116     c                   move      Prv(I)        Pcor(I)
068600051116e   3c                   endif
068700051116e   2c                   ENDDO
068800051116e   1c                   ENDDO
068900980211     C*---
069000960709     C* DATI CODICI TASSAZIONE
069100980211     C*---
069200020919     C                   Z-ADD     1             KTBKUT
069300960709     C                   MOVEL     'CT'          KTBCOD
069400041213DO  1C     1             DO        600           I
069500960709IF  2C     PRV(I)        IFNE      *BLANKS
069600960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
069700960709     C     KEYTAB        CHAIN     TABEL00F                           99
069800960709IF  3C     *IN99         IFEQ      *OFF
069900960709     C     TBLFLG        ANDEQ     *BLANKS
070000960709     C                   MOVEL     TBLUNI        DSCT
070100960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
070200960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
070300960709E   3C                   ENDIF
070400020925E   3C                   ENDIF
070500960709E   1C                   ENDDO
070600980302     C*---
070700010716     C* TERMINAL SU ELABORATORE
070800020925     c* SEDE    --> TUTTI I TERMINAL PARTENZA
070900020925     c* FILIALE --> TUTTI i TERMINAL PARTENZA DELL'AREA
071000980302     C*---
071100020925     c                   CLEAR                   OFil
071200020925     c                   CLEAR                   OTfp
071300020925IF  1C                   IF        DUTLPO = 'S'                                 TRASMISSIONE IN SEDE
071400020925     C                   clear                   trul26ds
071500020925     C                   EVAL      I26TLA = 'L'
071600020925     C                   EVAL      I26TPT = 'P'
071700020925     C                   CALL      'TRUL26R'
071800020925     C                   PARM                    trul26ds
071900020925     C                   Z-ADD     skpot         OFil                           *terminal -TUTTI-
072000020925X   1C                   ELSE                                                   RICEZIONE IN FILIALE
072100020925     C                   CLEAR                   tibs56ds
072200020925     C                   EVAL      I56PPO = DUTPOU
072300020925     C                   EVAL      I56MOD = 'POA'
072400020925     C                   CALL      'TIBS56R'
072500020925     C                   PARM                    tibs56ds
072600020925do  2C     1             DO        250           I
072700020925if  3C                   IF        ski(I) > *ZEROS
072800020925     C                   CLEAR                   FNLV55DS
072900020925     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
073000020925     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
073100020925     C                   Z-ADD     ski(I)        D55LIN                         *LINEA
073200020925     C                   Z-ADD     p96dri        D55DRF                         *DATA RIFERIMENTO
073300020925     C                   CALL      'FNLV55R'
073400020925     C                   PARM                    FNLV55DS
073500020925if  4C     D55ERR        IFNE      *BLANKS                                      *ERRORE
073600020925     C                   Z-ADD     dutpou        D55TFP                         *SE STESSA
073700020925e   4C                   ENDIF
073800020925     c                   Z-ADD     d55tfp        ski(I)                         *FIL diventa TFP
073900020925e   3C                   ENDIF
074000020925e   2C                   ENDDO
074100020925     C                   Z-ADD     ski           OFil                           *terminal -AREA-
074200020925e   1C                   ENDIF
074300020925     c*
074400020925     c* Estrapola SOLO i terminal
074500010716     C                   Z-ADD     *ZEROS        J                 5 0
074600020925do  1C     1             DO        250           I
074700020925if  2C                   IF        OFil(I) > *ZEROS
074800020925     C     OFil(i)       LOOKUP    OTfp                                   55
074900020925if  3C                   IF        *IN55 = *OFF                                 *NON TROVATO
075000010716     C                   ADD       1             J
075100020925     C                   Z-ADD     OFil(i)       OTfp(J)
075200020925e   3C                   ENDIF
075300020925e   2C                   ENDIF
075400020925e   1C                   ENDDO
075500960709     C*
075600960709     C                   ENDSR
075700020919     C*--------------------------------------------------------------------------------------------*
075800020919     C* REPERISCE I DATI UTENTE
075900020919     C*--------------------------------------------------------------------------------------------*
076000020919     C     REPDATIUTE    BEGSR
076100020919     C*
076200020919     C* INIZIALIZZA VARIABILI DI WRK
076300020919     C                   CLEAR                   TIBS34DS
076400020919     C                   CLEAR                   AZUTEDS
076500020919     C                   CLEAR                   DDATIUTE
076600020919     C*
076700020919     C     *DTAARA       DEFINE    §azute        azuteds
076800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
076900020919     C                   IN(E)     *DTAARA
077000020919if  1C                   IF        %Error
077100020919     c                   EVAL      I34Tla = 'L'
077200020919     C                   CALL      'TIBS34R'
077300020919     C                   PARM                    Tibs34Ds
077400020919     C                   IN        *DTAARA
077500020919e   1C                   ENDIF
077600020919     C*
077700020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
077800020919     C                   MOVEL(P)  RSUT          DSFIRS           20
077900020919     C*
078000020919     C                   ENDSR
078100000000     C*------------------------------------------------------------------------*
078200000000     C* *INZSR - OPERAZIONI INIZIALI
078300000000     C*------------------------------------------------------------------------*
078400000000     C     *INZSR        BEGSR
078500980211     C*---
078600000000     C* RICEVIMENTO PARAMETRI
078700980211     C*---
078800000000     C     *ENTRY        PLIST
078900000000     C                   PARM                    KPJBA
079000000000     C                   MOVEL     KPJBU         PARAM
079100091204
079200091204     c* Se passata la data elaboro cappario alla data passata altrimenti
079300091204     c*  alla data corrente
079400091204     c                   clear                   wdatavers
079500091204     c                   if        pardtela>*zeros
079600091204     c                   movel     pardtela      wdatavers         8 0
079700091204     c                   endif
079800980211     C*---
079900000000     C* VARIABILI RIFERITE AL DATABASE
080000980211     C*---
080100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
080200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
080300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
080400020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
080500020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
080600020925     C     *LIKE         DEFINE    TBLKEY        KTBKEY
080700980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
080800980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
080900980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
081000980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
081100980216     C     *LIKE         DEFINE    CPCLNA        DEPTFP                         *DEPOSITI
081200980216     C     *LIKE         DEFINE    CPCLNA        DEPTFA
081300980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
081400980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
081500980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
081600980216     C     *LIKE         DEFINE    DLYFCR        DEPFCR
081700980216     C     *LIKE         DEFINE    DLYRAP        DEPRAP
081800980216     C     *LIKE         DEFINE    DLYCOR        DEPCOR
081900980302     C     *LIKE         DEFINE    CPCLNA        DEPLIN
082000980220     C     *LIKE         DEFINE    DLYTRI        DEPTIC
082100980220     C     *LIKE         DEFINE    DLYTRC        DEPTCC
082200980220     C     *LIKE         DEFINE    DLYTRI        DEPTIE
082300980220     C     *LIKE         DEFINE    DLYTRC        DEPTCE
082400090921     C     *LIKE         DEFINE    DLYTRI        DEPTIh
082500090921     C     *LIKE         DEFINE    DLYTRC        DEPTCh
082600980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
082700980216     C     *LIKE         DEFINE    CPCISO        WISO
082800980216     C     *LIKE         DEFINE    CPCLNA        WLNA
082900980219     C     *LIKE         DEFINE    CPLDLV        WDLV
083000980302     C     *LIKE         DEFINE    CPCLNA        WILNA
083100980302     C     *LIKE         DEFINE    CPCLOL        WILOL
083200980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
083300980211     C*---
083400000000     C* CHIAVI DI LETTURA
083500980211     C*---
083600980211     C     KEYTAB        KLIST                                                  *TABEL00F
083700980211     C                   KFLD                    KTBKUT                          -UTENTE
083800980211     C                   KFLD                    KTBCOD                          -TABELLA
083900980211     C                   KFLD                    KTBKEY                          -CHIAVE
084000980211     C     KE2TAB        KLIST                                                  *TABEL00F
084100980211     C                   KFLD                    KTBKUT                          -UTENTE
084200980211     C                   KFLD                    KTBCOD                          -TEBELLA
084300020925     C     KEYTBE        KLIST                                                  *TNTBE01L
084400020925     C                   KFLD                    KBECOD                          -TABELLA
084500020925     C                   KFLD                    KBEKE1                          -CHIAVE
084600980211     C     KEYCPC        KLIST                                                  *AZCPC00F
084700980211     C                   KFLD                    KCCVER                          -VERSIONE
084800980211     C     KEYCPL        KLIST                                                  *AZCPL00F
084900980211     C                   KFLD                    KCLVER                          -VERSIONE
085000980216     C                   KFLD                    KCLNAR                          -NAZIONE
085100980216     C                   KFLD                    KCLCAP                          -CAP
085200980211     C*---
085300000000     C* VARIABILI DI CONTROLLO
085400980211     C*---
085500980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
085600980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
085700980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
085800980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
085900050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
086000020527     C*
086100020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
086200020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
086300020527     C                   Z-ADD     N8            G08DAT
086400020527     C                   Z-ADD     *ZEROS        G08INV
086500020527     C                   MOVEL     '0'           G08ERR
086600020527     C                   CALL      'XSRDA8'
086700020527     C                   PARM                    WLBDA8
086800020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
086900020925     C*
087000020925     C* REPERISCE I DATI UTENTE
087100020925     C                   EXSR      REPDATIUTE
087200970707     C*
087300020527     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
087400020527     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
087500020919IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
087600030304     C***                Z-ADD     *ZEROS        P96DRI            8 0
087700030304     C                   Z-ADD     DATCOR        P96DRI            8 0
087800020527X   1C                   ELSE                                                   S E D E
087900020527     C                   Z-ADD     DATCOR        P96DRI            8 0
088000020527E   1C                   ENDIF
088100091204     c
088200091204     c* Se passata data uso questa
088300091204     c                   if        wdatavers>0
088400091204     C                   Z-ADD     wdatavers     P96DRI            8 0
088500091204     c                   endif
088600970707     C                   Z-ADD     *ZEROS        P96VER            5 0
088700970707     C                   CALL      'TISI96R'
088800970707     C                   PARM                    P96DRI
088900970707     C                   PARM                    P96VER
089000040415     C*
089100091211     C* REIPOSTO LA DATA CORRENTE (oppure la data passata al pgm ) nella data di riferimento
089200091211     C*  XCHE' IN USCITA il  *PGM TISI96R VALORIZZA IL P96DRI CON LA DATA D DECORRENZA
089300091211     C*  DELLA VERSIONE REPERITA
089400040415     C                   Z-ADD     DATCOR        P96DRI
089500091211     c                   if        wdatavers>0
089600091211     C                   Z-ADD     wdatavers     P96DRI            8 0
089700091211     c                   endif
089800960709     C*
089900000000     C                   ENDSR
090000000000     C*------------------------------------------------------------------------*
090100000000**   CMD -COMANDI CLP-
090200050318CLRPFM FILE(WADLY00F)
090300091211CRTDUPOBJ OBJ(WADLY00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(EDPES)
090400091211CLRPFM FILE(EDPES/WADLY00F)
090500091211OVRDBF FILE(WADLY00F) TOFILE(EDPES/WADLY00F)
