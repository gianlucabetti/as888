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
001300121012     FAZCPE01L  IF   E           K DISK
001301150624     FAZcln01L  IF   E           K DISK
001400051114     FWADLY00F  O    E           K DISK    USROPN
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* COMANDI CLP
002000000000     D*-------------------
002100091211     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
002200960709     D*-------------------
002300980216     D* PROVINCIE
002400960709     D*-------------------
002500051116     d PRV             s                   dim(600)  inz  like(DLYprv)          *PROVINCIA
002600051116     d Pcts            s                   dim(600)  inz  like(§PRcts)          *CODICE TASSAZIONE
002700051116     d Pfcr            s                   dim(600)  inz  like(DLYfcr)          *CAPOLUOGO DI REGION
002800051116     d Prap            s                   dim(600)  inz  like(DLYrap)          *REGIONE APPARTENENZ
002900051116     d Pcor            s                   dim(600)  inz  like(DLYcor)          *ORDINAMENTO
003000010716     D*-------------------
003100010716     D* TERMINAL PARTENZA
003200010716     D*-------------------
003300051116     D OFIL            S              3  0 DIM(250)  inz                        *CODICE
003400051116     D OTFP            S              3  0 DIM(250)  inz                        *CODICE
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003800980210     D*-------------------
003900980210     D* DS REPERIMENTO DATI UTENTE
004000980210     D*-------------------
004100020919     D TIBS34DS      E DS                                                       *Profili utente
004200020919     D DDATIUTE      E DS                                                       *Dati utente
004300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004400000000     D*-------------------
004500980210     D* DS CALCOLA TEMPI DELIVERY
004600000000     D*-------------------
004700980210     D SD99DS        E DS                  EXTNAME(TNSD99DS)
004800971020     D*------------------
004900980210     D* DS REPERIMENTO TERMINALs
005000971020     D*------------------
005100020925     D FNLV55DS      E DS
005200960709     D*-------------------
005300960709     D* DS PROVINCIE
005400960709     D*-------------------
005500960709     D DSPR          E DS
005600000000     D*-------------------
005700000000     D* DS CODICI TASSAZIONE
005800000000     D*-------------------
005900000000     D DSCT          E DS
006000020925     D*-------------------
006100020925     D* DS STATISTICHE DATI DIVERSI
006200020925     D*-------------------
006300020925     D DSDD          E DS
006400980210     D*------------------
006500980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600980210     D*------------------
006700980210     D WLBDA8          DS
006800980210     D  G08DAT                 1      8  0
006900980210     D  G08INV                 9     16  0
007000980210     D  G08ERR                17     17
007100980210     D  G08TGI                18     22  0
007200000000     D*-------------------
007300000000     D* DS PARAMETRI DI LANCIO
007400000000     D*-------------------
007500980210     D PARAM           DS
007600980210     D  PARLNP                 1      3
007700980210     D  PARTEM                 4      4
007800980210     D  PARTSP                 5      5
007900980210     D  PARISO                 6      6
008000980210     D  PARDIS                 7      7
008100980210     D  PARSSA                 8      8
008200980210     D  PARND1                 9     83
008300980210     D  PARND2                84    158
008400980210     D  PARND3               159    233
008500091204     d  PARlin               234    234
008600091204     d  PARold               235    235
008700091204     d  PARdtEla             236    243
008800091211     d  PARelaprova          244    244
008900980210     D*-------------------
009000980210     D* ARCHITETTURA
009100980210     D*-------------------
009200980210     D KPJBA         E DS
009300020830     D*-------------------
009400020925     D* DS REPERIMENTO P.O. IN GESTIONE
009500020830     D*-------------------
009600020925     D tibs56ds      E DS                                                       *per FILIALE remota
009700020830     D  ski                    7    756  0 dim(250)
009800020925     D trul26ds      E DS                                                       *per SEDE sede
009900020925     D  skpo                  11    760  0 dim(250)
010000020925     D  skpot                761   1510  0 dim(250)
010001150624
010002150624     D clnmat          DS
010003150624     D  mat                    1     31    dim(31)
010004150624     D clnpom          DS
010005150624     D  pom                    1     31    dim(31)
010100051116      *
010200051116      * V A R I A B I L I
010300051116      *
010400051116     d XX              s              3  0 inz
010401150624     d datacur         s               d   datfmt(*iso)
010402150624     d wfes            s              1
010500000000     C*------------------------------------------------------------------------*
010600000000     C* MAIN LINE
010700000000     C*------------------------------------------------------------------------*
010800051116      *
010900051116      * Carica le tabelle occorrenti
011000051116     c                   exsr      CarTAB
011100020925     C*
011200050318     C* PULISCE E RICREA IL FILE SOLO SE:
011300020925     C* data corrente > data ultima creazione workfile
011400020925IF  0c     DATCOR        IFGT      §SDDDLY
011500020925     C*
011600050318     C* OPERAZIONI INIZIALI APERTURA FILE
011700020925     C                   EXSR      OPNFIL
011800000000     C*
011900980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
012000980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
012100000000     C                   EXSR      SETFIL
012200000000     C*
012300000000     C* CICLO FINO A FINE FILE
012400000000DO  2C     $FINE         DOWEQ     '0'
012500980216     C*
012600980216     C* OPERAZIONI PER NUOVO CAP
012700980216     C                   EXSR      NEWCAP
012800980216     C*
012900980216     C* CICLO FINO A ROTTURA CAP
013000980216DO  3C     $FINE         DOWEQ     '0'
013100980216     C     CPCNAR        ANDEQ     DEPNAR
013200980216     C     CPCCAP        ANDEQ     DEPCAP
013300980302     C*
013400980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
013500980302     C                   Z-ADD     CPCLNA        WILNA                          *LINEA ARRIVO
013600121015     C                   Z-ADD     CPClna        deplna                         *LINEA OLTRE
013700980302     C                   Z-ADD     CPCLOL        WILOL                          *LINEA OLTRE
013800121015     C                   Z-ADD     CPCLOL        deplol                         *LINEA OLTRE
013900121012     C                   Z-ADD     CPCLOS        WILOS                          *LINEA sotto
014000121015     C                   Z-ADD     CPCLOS        deplos                         *LINEA sotto
014100980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
014200980302     C                   EXSR      CALLNA
014300980302     C                   Z-ADD     WOLIN         DEPLIN                         *DEPOSITO LNA CAP
014400980302     C*
014500020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
014600980220     C                   MOVEL     'C'           $DLY
014700980216     C                   MOVEL     *BLANKS       WLOC
014800980216     C                   MOVEL     CPCISO        WISO
014900980302     C                   Z-ADD     WOLIN         WLNA
015000980219     C                   MOVEL     'N'           WDLV
015100121015     c
015200020925do  4C     1             DO        250           J
015300020925if  5C                   IF        OTFP(J) > *ZEROS
015400010716     C                   Z-ADD     OTFP(J)       DEPTFP
015500121015     c
015600121012     c* per ogni terminal di partenza verifico se c'e' una eccezione
015700121015     c                   if        cpcese='S'
015800121012     c     keycpe        chain     azcpe01l
015900121012     c                   if        %found(azcpe01l) and cpeatb=' '
016000121012     C                   Z-ADD     CPeLNA        WILNA                          *LINEA ARRIVO
016100121012     C                   Z-ADD     CPeLOL        WILOL                          *LINEA OLTRE
016200121012     C                   Z-ADD     CPeLOS        WILOS                          *LINEA sotto
016300121012     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
016400121012     C                   EXSR      CALLNA
016500121012     C                   Z-ADD     WOLIN         WLNA
016600121015     c                   else
016700121015     C                   Z-ADD     depLNA        WILNA                          *LINEA ARRIVO
016800121015     C                   Z-ADD     depLOL        WILOL                          *LINEA OLTRE
016900121015     C                   Z-ADD     depLOS        WILOS                          *LINEA sotto
017000121015     C                   Z-ADD     deplin        WLNA
017100121012     c                   endif
017200121015     c                   endif
017300121012     c
017400980210     C                   EXSR      CREDLY
017500121012     c
017600040415     C                   Z-ADD     *ZEROS        DEPTIC
017700040415     C                   Z-ADD     *ZEROS        DEPTCC
017800090515     C                   Z-ADD     *ZEROS        DEPTIe
017900090515     C                   Z-ADD     *ZEROS        DEPTCe
018000090921     C                   Z-ADD     *ZEROS        DEPTIh
018100090921     C                   Z-ADD     *ZEROS        DEPTCh
018200020925e   5C                   ENDIF
018300020925e   4C                   ENDDO
018400980216     C*
018500980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
018600980216     C                   EXSR      SETCPL
018700980216     C*
018800980216     C* CICLO FINO A FINE FILE
018900980216DO  4C     $FINL         DOWEQ     '0'
019000980216     C*
019100980216     C* SCRIVE IL RECORD DEL WORKFILE
019200980220     C                   MOVEL     'L'           $DLY
019300980216     C                   MOVEL     CPLLOC        WLOC
019400980220     C                   MOVEL     CPLDLV        WDLV
019500980302     C*
019600980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
019700980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
019800980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
019900980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
020000121012     C                   Z-ADD     CPLLOs        WILOs                          *LINEA sotto
020100980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
020200980302     C                   EXSR      CALLNA
020300980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
020400980302     C                   Z-ADD     WOLIN         WLNA
020500980220X   5C                   ELSE                                                   *LIVELLO CAP
020600980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
020700980302     C                   Z-ADD     DEPLIN        WLNA
020800121015     C                   Z-ADD     DEPlna        Wilna
020900121015     C                   Z-ADD     DEPlol        Wilol
021000121015     C                   Z-ADD     DEPlos        Wilos
021100980220E   5C                   ENDIF
021200020925     C*
021300020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
021400020925do  5C     1             DO        250           J
021500020925if  6C                   IF        OTFP(J) > *ZEROS
021600020925     C                   Z-ADD     OTFP(J)       DEPTFP
021700121015     c
021800121015IF  7C     CPLLIV        IFEQ      'C'                                          *LIVELLO LOCALITA'
021900121015     c
022000121015    8c                   if        cpcese='S'
022100121015     c* per ogni terminal di partenza verifico se c'e' una eccezione
022200121015     c     keycpe        chain     azcpe01l
022300121015    9c                   if        %found(azcpe01l) and cpeatb=' '
022400121015     C                   Z-ADD     CPeLNA        WILNA                          *LINEA ARRIVO
022500121015     C                   Z-ADD     CPeLOL        WILOL                          *LINEA OLTRE
022600121015     C                   Z-ADD     CPeLOS        WILOS                          *LINEA sotto
022700121015     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
022800121015     C                   EXSR      CALLNA
022900121015     C                   Z-ADD     WOLIN         WLNA
023000121015   x9c                   else
023100121015     C                   Z-ADD     depLNA        WILNA                          *LINEA ARRIVO
023200121015     C                   Z-ADD     depLOL        WILOL                          *LINEA OLTRE
023300121015     C                   Z-ADD     depLOS        WILOS                          *LINEA sotto
023400121015     C                   Z-ADD     deplin        WLNA
023500121015    9c                   endif
023600121015    8c                   endif
023700121015     c
023800121015    7c                   endif
023900121015     c
024000020925     C                   EXSR      CREDLY
024100121015     c
024200040415     C                   Z-ADD     *ZEROS        DEPTIC
024300040415     C                   Z-ADD     *ZEROS        DEPTCC
024400090515     C                   Z-ADD     *ZEROS        DEPTIe
024500090515     C                   Z-ADD     *ZEROS        DEPTCe
024600090921     C                   Z-ADD     *ZEROS        DEPTIh
024700090921     C                   Z-ADD     *ZEROS        DEPTCh
024800020925e   6C                   ENDIF
024900020925e   5C                   ENDDO
025000980216     C*
025100980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
025200980216     C                   EXSR      LETCPL
025300980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
025400000000     C*
025500980216     C* LETTURA RECORD SUCCESSIVO -CAP-
025600000000     C                   EXSR      LETFIL
025700980216E   3C                   ENDDO                                                  *ROTTURA CAP
025800980216E   2C                   ENDDO                                                  *FINE -CAP-
025900000000E   1C                   ENDIF
026000000000     C*
026100000000     C* OPERAZIONI DI CHIUSURA
026200971020     C                   EXSR      FINSR
026300020925E   0C                   ENDIF
026400000000     C*
026500000000     C                   SETON                                        LR
026600000000     C*------------------------------------------------------------------------*
026700980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
026800000000     C*------------------------------------------------------------------------*
026900000000     C     SETFIL        BEGSR
027000000000     C*
027100970707     C                   Z-ADD     P96VER        KCCVER
027200970707     C     KEYCPC        SETLL     AZCPC01L                           99
027300000000IF  1C     *IN99         IFEQ      *ON
027400000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
027500000000X   1C                   ELSE
027600000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
027700000000E   1C                   ENDIF
027800000000     C*
027900000000     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
028200000000     C*------------------------------------------------------------------------*
028300000000     C     LETFIL        BEGSR
028400000000     C*
028500000000     C* LEGGE FINO A:
028600000000     C*     - FINE FILE
028700000000     C*     - TROVATO RECORD VALIDO
028800000000     C                   MOVEL     'N'           WRECOK
028900000000DO  1C     $FINE         DOWEQ     '0'
029000000000     C     WRECOK        ANDEQ     'N'
029100970707     C     KEYCPC        READE     AZCPC01L                               99
029200000000IF  2C     *IN99         IFEQ      *ON
029300000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
029400000000X   2C                   ELSE
029500000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
029600000000E   2C                   ENDIF
029700000000E   1C                   ENDDO
029800960701     C*
029900000000     C                   ENDSR
030000000000     C*------------------------------------------------------------------------*
030100980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
030200000000     C*------------------------------------------------------------------------*
030300000000     C     CHKREC        BEGSR
030400000000     C*
030500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
030600000000     C*
030700000000     C* CAP ESTERO, ESCLUDE
030800970707IF  1C     CPCNAR        IFNE      *BLANKS
030900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031000000000E   1C                   ENDIF
031100000000     C*
031200000000     C* CAP FITTIZIO, ESCLUDE
031300970707IF  1C     CPCFIT        IFEQ      'S'
031400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031500000000E   1C                   ENDIF
031600980217     C*
031700980217     C* CAP OBSOLETO, ESCLUDE
031800980217IF  1C     CPCFI1        IFEQ      'O'
031900980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032000980217E   1C                   ENDIF
032100000000     C*
032200000000     C                   ENDSR
032300980302     C*------------------------------------------------------------------------*
032400980302     C* CALLNA - CALCOLA LA LINEA ARRIVO DA CONSIDERARE
032500980302     C*------------------------------------------------------------------------*
032600980302     C     CALLNA        BEGSR
032700020802     C*
032800980302     C                   Z-ADD     WILNA         WOLIN                          *LINEA STANDARD
032900980302     C*
033000980302     C                   ENDSR
033100980216     C*------------------------------------------------------------------------*
033200980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
033300980216     C*------------------------------------------------------------------------*
033400980216     C     NEWCAP        BEGSR
033500980216     C*
033600980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
033700980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
033800980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
033900980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
034000980220     C*
034100980220     C* AZZERA LE VARIABILI DI LAVORO
034200980220     C                   Z-ADD     *ZEROS        DEPTIC                         *TEMPO INTERNO CORRI
034300980220     C                   Z-ADD     *ZEROS        DEPTCC                         *TEMPO CLIENTE CORRI
034400090921     C                   Z-ADD     *ZEROS        DEPTIE                         *TEMPO INTERNO "E"
034500090921     C                   Z-ADD     *ZEROS        DEPTCE                         *TEMPO CLIENTE "E"
034600090921     C                   Z-ADD     *ZEROS        DEPTIh                         *TEMPO INTERNO "H"
034700090921     C                   Z-ADD     *ZEROS        DEPTCh                         *TEMPO CLIENTE "H"
034800980216     C*
034900980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
035000980216     C                   Z-ADD     1             I
035100980216     C                   SETOFF                                           28
035200980216     C     CPCPRV        LOOKUP    PRV(I)                                 28
035300980216IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
035400980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
035500980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
035600980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
035700980216X   1C                   ELSE                                                   *NON TROVATO
035800980216     C                   MOVEL     *BLANKS       DEPFCR
035900980216     C                   MOVEL     *BLANKS       DEPRAP
036000980216     C                   MOVEL     *BLANKS       DEPCOR
036100980216E   1C                   ENDIF
036200980216     C*
036300980216     C                   ENDSR
036400980216     C*------------------------------------------------------------------------*
036500980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
036600980216     C*------------------------------------------------------------------------*
036700980216     C     SETCPL        BEGSR
036800980216     C*
036900980216     C* REIMPOSTA LE VARIABILI DI LAVORO
037000980216     C                   MOVEL     '0'           $FINL
037100980216     C*
037200980216     C                   Z-ADD     P96VER        KCLVER
037300980216     C                   MOVEL     DEPNAR        KCLNAR
037400980216     C                   MOVEL     DEPCAP        KCLCAP
037500980216     C     KEYCPL        SETLL     AZCPL01L                           98
037600980216IF  1C     *IN98         IFEQ      *ON
037700980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
037800980216X   1C                   ELSE
037900980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
038000980216E   1C                   ENDIF
038100980216     C*
038200980216     C                   ENDSR
038300980216     C*------------------------------------------------------------------------*
038400980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
038500980216     C*------------------------------------------------------------------------*
038600980216     C     LETCPL        BEGSR
038700980216     C*
038800980216     C* LEGGE FINO A:
038900980216     C*     - FINE FILE
039000980216     C*     - TROVATO RECORD VALIDO
039100980216     C                   MOVEL     'N'           WRECOK
039200980216DO  1C     $FINL         DOWEQ     '0'
039300980216     C     WRECOK        ANDEQ     'N'
039400980216     C     KEYCPL        READE     AZCPL01L                               98
039500980216IF  2C     *IN98         IFEQ      *ON
039600980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
039700980216X   2C                   ELSE
039800980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
039900980216E   2C                   ENDIF
040000980216E   1C                   ENDDO
040100980216     C*
040200980216     C                   ENDSR
040300980216     C*------------------------------------------------------------------------*
040400980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
040500980216     C*------------------------------------------------------------------------*
040600980216     C     CHKCPL        BEGSR
040700980216     C*
040800980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
040900980216     C*
041000980217     C* LOCALITA' OBSOLETA, ESCLUDE
041100980217IF  1C     CPLFI1        IFEQ      'O'
041200980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
041300980217E   1C                   ENDIF
041400980216     C*
041500980216     C                   ENDSR
041600980216     C*------------------------------------------------------------------------*
041700980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
041800980216     C*------------------------------------------------------------------------*
041900980216     C     CREDLY        BEGSR
042000980216     C*
042100050318     C                   CLEAR                   WADLY000
042200980216     C*---
042300980216     C* DATI DEDOTTI
042400980216     C*---
042500980216     C                   MOVEL     DEPTFP        DLYLNP                         *LINEA PARTENZA
042600980216     C                   MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
042700980216     C                   MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
042800980216     C                   MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
042900980216     C*---
043000980216     C* DATI CAPPARIO
043100980216     C*---
043200980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
043300980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
043400980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
043500980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
043600980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
043700980219     C                   MOVEL     WDLV          DLYDLV                         *STAMPA LOCALITA'
043800980216     C*---
043900980216     C* DATI CALCOLATI
044000980216     C*---
044100980220     C                   MOVEL     'C'           DLYTSP                         *TIPO SERVIZIO
044200980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
044300980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
044400040415     C     DEPTIC        ANDNE     *ZEROS                                       *CAMBIO D TP
044500040415     C     DEPTCC        ANDNE     *ZEROS                                       *CAMBIO D TP
044600980220     C                   Z-ADD     DEPTIC        DLYTRI                         *TEMPO INTERNO CORRI
044700980220     C                   Z-ADD     DEPTCC        DLYTRC                         *TEMPO CLIENTE CORRI
044800980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
044900040415     C                                                                          *'L' O NEW TERM.PART
045000980216     C*
045100980216     C* RECUPERA IL TERMINAL ARRIVO DEL CAP
045200980216     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
045300980216     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
045400980216     C                   Z-ADD     WLNA          D55LIN                         *LINEA
045500020527     C                   Z-ADD     P96DRI        D55DRF                         *DATA RIFERIMENTO
045600980216     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
045700980216     C                   CALL      'FNLV55R'
045800020925     C                   PARM                    FNLV55DS
045900980220IF  2C     D55ERR        IFNE      *BLANKS                                      *ERRORE
046000980216     C                   Z-ADD     WLNA          D55TFA                         *SE STESSA
046100980220E   2C                   ENDIF
046200980216     C                   Z-ADD     D55TFA        DEPTFA                         *TERMINAL ARRIVO
046300090921     C* "C"- esxpress
046400980216     C                   CLEAR                   SD99DS
046500980216     C                   MOVEL     'C'           D98TSP                         *TIPO SERVIZIO
046600020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
046700980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
046800980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
046900980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
047000980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
047100980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
047200980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
047300980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
047400980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
047500980216     C                   Z-ADD     1             D98NCD                         *COLLI
047600980216     C                   CALL      'TNSD99R'
047700980216     C                   PARM                    SD99DS
047800980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
047900980220IF  3C     D98TRI        IFGT      24
048000980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
048100980220X   3C                   ELSE
048200980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
048300980220E   3C                   ENDIF
048400980220IF  3C     D98TRC        IFGT      24
048500980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
048600980220X   3C                   ELSE
048700980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
048800980220E   3C                   ENDIF
048900980220X   2C                   ELSE                                                   *ERRORI
049000980216     C                   Z-ADD     *ZEROS        DLYTRI
049100980216     C                   Z-ADD     *ZEROS        DLYTRC
049200980220E   2C                   ENDIF
049300980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
049400980220     C                   Z-ADD     DLYTRI        DEPTIC                         *TEMPO INTERNO CORRI
049500980220     C                   Z-ADD     DLYTRC        DEPTCC                         *TEMPO CLIENTE CORRI
049600980220E   2C                   ENDIF
049700980220E   1C                   ENDIF
049800090921
049900121012     c* imposto le 3 linee di arrivo previste dal cap/località
050000121012     c                   z-add     wilna         dlylna
050100121012     c                   z-add     wilol         dlylnao
050200121012     c                   z-add     wilos         dlylnas
050300121012     c
050400050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
050500090921     c
050600090921     C* "E"- priority
050700980220     C                   MOVEL     'E'           DLYTSP                         *TIPO SERVIZIO
050800980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
050900980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
051000090515     C     DEPTIe        ANDNE     *ZEROS                                       *CAMBIO D TP
051100090515     C     DEPTCe        ANDNE     *ZEROS                                       *CAMBIO D TP
051200980220     C                   Z-ADD     DEPTIE        DLYTRI                         *TEMPO INTERNO ESPRE
051300980220     C                   Z-ADD     DEPTCE        DLYTRC                         *TEMPO CLIENTE ESPRE
051400980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
051500980216     C                   CLEAR                   SD99DS
051600980216     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO
051700020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
051800980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
051900980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
052000980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
052100980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
052200980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
052300980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
052400980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
052500980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
052600980216     C                   Z-ADD     1             D98NCD                         *COLLI
052700980216     C                   CALL      'TNSD99R'
052800980216     C                   PARM                    SD99DS
052900980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
053000980220IF  3C     D98TRI        IFGT      24
053100980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
053200980220X   3C                   ELSE
053300980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
053400980220E   3C                   ENDIF
053500980220IF  3C     D98TRC        IFGT      24
053600980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
053700980220X   3C                   ELSE
053800980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
053900980220E   3C                   ENDIF
054000980220X   2C                   ELSE                                                   *ERRORI
054100980216     C                   Z-ADD     *ZEROS        DLYTRI
054200980216     C                   Z-ADD     *ZEROS        DLYTRC
054300980220E   2C                   ENDIF
054400980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
054500980220     C                   Z-ADD     DLYTRI        DEPTIE                         *TEMPO INTERNO ESPRE
054600980220     C                   Z-ADD     DLYTRC        DEPTCE                         *TEMPO CLIENTE ESPRE
054700980220E   2C                   ENDIF
054800980220E   1C                   ENDIF
054900050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
055000980216     C*
055100090921     C* "H"- H 10 30
055200090921     C                   MOVEL     'H'           DLYTSP                         *TIPO SERVIZIO
055300090921IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
055400090921     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
055500090921     C     DEPTIh        ANDNE     *ZEROS                                       *CAMBIO D TP
055600090921     C     DEPTCh        ANDNE     *ZEROS                                       *CAMBIO D TP
055700090921     C                   Z-ADD     DEPTIh        DLYTRI                         *TEMPO INTERNO ESPRE
055800090921     C                   Z-ADD     DEPTCh        DLYTRC                         *TEMPO CLIENTE ESPRE
055900090921X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
056000090921     C                   CLEAR                   SD99DS
056100090921     C                   MOVEL     'H'           D98TSP                         *TIPO SERVIZIO
056200090921     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
056300090921     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
056400090921     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
056500090921     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
056600090921     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
056700090921     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
056800090921     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
056900090921     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
057000090921     C                   MOVEL     WISO          D98FIN                         *INOLTRO
057100090921     C                   Z-ADD     1             D98NCD                         *COLLI
057200090921     C                   CALL      'TNSD99R'
057300090921     C                   PARM                    SD99DS
057400090921IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
057500090921IF  3C     D98TRI        IFGT      24
057600090921     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
057700090921X   3C                   ELSE
057800090921     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
057900090921E   3C                   ENDIF
058000090921IF  3C     D98TRC        IFGT      24
058100090921     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
058200090921X   3C                   ELSE
058300090921     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
058400090921E   3C                   ENDIF
058500090921X   2C                   ELSE                                                   *ERRORI
058600090921     C                   Z-ADD     *ZEROS        DLYTRI
058700090921     C                   Z-ADD     *ZEROS        DLYTRC
058800090921E   2C                   ENDIF
058900090921IF  2C     $DLY          IFEQ      'C'                                          *CAP
059000090921     C                   Z-ADD     DLYTRI        DEPTIh                         *TEMPO INTERNO ESPRE
059100090921     C                   Z-ADD     DLYTRC        DEPTCh                         *TEMPO CLIENTE ESPRE
059200090921E   2C                   ENDIF
059300090921E   1C                   ENDIF
059400090921     c
059500090921     C                   WRITE     WADLY000                                     I N S E R I M E N T
059600980216     C                   ENDSR
059700000000     C*------------------------------------------------------------------------*
059800971020     C* FINSR - OPERAZIONI FINALI
059900000000     C*------------------------------------------------------------------------*
060000971020     C     FINSR         BEGSR
060100971020     C*
060200971020     C* CHIUDE I FILE DI LAVORO
060300051114     C                   CLOSE     WADLY00F
060400020925     C*
060500020925     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE
060600020925     C                   MOVEL(P)  'SDD'         KBECOD
060700020925     C                   MOVEL(P)  '1'           KBEKE1
060800020925     C     KEYTBE        CHAIN     TNTBE01L
060900020925IF  1C                   IF        %found(TNTBE01L)
061000020925     C                   Z-ADD     DATCOR        §SDDDLY
061100020925     C                   MOVEL     DSDD          TBEUNI
061200020925     C                   UPDATE    TNTBE000
061300020925E   1C                   ENDIF
061400971020     C*
061500980211     C* CHIUDE I PGM CHIAMATI
061600980210     C                   CLEAR                   SD99DS                         *CALCOLA DELIVERY
061700000000     C                   MOVEL     'C'           D98TLA
061800971009     C                   CALL      'TNSD99R'
061900980210     C                   PARM                    SD99DS
062000980211     C*
062100020925     C                   CLEAR                   FNLV55DS                       *RECUPERA TERMINALs
062200971020     C                   MOVEL     'C'           D55TLA
062300971020     C                   CALL      'FNLV55R'
062400020925     C                   PARM                    FNLV55DS
062500000000     C*
062600000000     C                   ENDSR
062700000000     C*------------------------------------------------------------------------*
062800050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
062900000000     C*------------------------------------------------------------------------*
063000000000     C     OPNFIL        BEGSR
063100091211     C                   Z-ADD     80            LENGH            15 5
063200091211     c* se si tratta di una prova, creo il file, vuoto in EDPES
063300091211    1c                   if        parelaprova='S'
063400091211     c* creazione file
063500091211     C                   MOVEL(P)  CMD(2)        COMMAN           80
063600091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
063700091211     C                   PARM                    COMMAN
063800091211     C                   PARM                    LENGH
063900091211     c
064000091211     c* Se c'e' già lo pulisco
064100091211    2C                   IF        %error
064200091211     C                   MOVEL(P)  CMD(3)        COMMAN           80
064300091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
064400091211     C                   PARM                    COMMAN
064500091211     c                   PARM                    LENGH
064600091211    3C                   IF        %error
064700091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
064800091211    3C                   ENDIF
064900091211    2C                   ENDIF
065000091211     c
065100091211     c* Eseguo ovrdbf
065200091211     C                   MOVEL(P)  CMD(4)        COMMAN           80
065300091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
065400091211     C                   PARM                    COMMAN
065500091211     c                   PARM                    LENGH
065600091211    3C                   IF        %error
065700091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
065800091211    3C                   ENDIF
065900091214     c
066000091211   x1c                   else
066100020925     c*
066200050318     c* Pulisce il file
066300030304     C                   MOVEL(P)  CMD(1)        COMMAN           80
066400050318     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
066500030304     C                   PARM                    COMMAN
066600000000     C                   PARM                    LENGH
066700091211    2C                   IF        %error
066800021202     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
066900091211    2C                   ENDIF
067000091211    1C                   ENDIF
067100020925     C*
067200050318     C* Apre il file
067300051114     C                   OPEN      WADLY00F                                     *APRE FILE
067400960701     C*
067500000000     C                   ENDSR
067600960709     C*------------------------------------------------------------------------*
067700980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
067800960709     C*------------------------------------------------------------------------*
067900960709     C     CARTAB        BEGSR
068000020925     C*---
068100020925     C* Dati statistiche
068200020925     C*---
068300020925     C                   CLEAR                   DSDD
068400020925     C                   MOVEL(P)  'SDD'         KBECOD
068500020925     C                   MOVEL(P)  '1'           KBEKE1
068600020925     C     KEYTBE        CHAIN(N)  TNTBE01L
068700020925     C                   IF        %found(TNTBE01L)
068800020925     C                   MOVEL     TBEUNI        DSDD
068900020925     C                   ENDIF
069000980211     C*---
069100980211     C* PROVINCIE
069200980211     C*---
069300980216     C                   Z-ADD     *ZEROS        I                 5 0
069400020919     C                   Z-ADD     1             KTBKUT
069500960709     C                   MOVEL     'PR'          KTBCOD
069600960709     C     KE2TAB        SETLL     TABEL00F
069700960709     C     KE2TAB        READE     TABEL00F                               99
069800960709DO  1C     *IN99         DOWEQ     *OFF
069900960709IF  2C     TBLFLG        IFEQ      *BLANKS
070000960709     C                   ADD       1             I
070100960709     C                   MOVEL     TBLUNI        DSPR
070200960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
070300960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
070400051116     c                   movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
070500960709E   2C                   ENDIF
070600960709     C     KE2TAB        READE     TABEL00F                               99
070700960709E   1C                   ENDDO
070800051116      *
070900051116      * Imposta il progressivo per le province (diverse) con lo stesso
071000051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
071100051116      *   - che NON può essere uguale per due province diverse:
071200051116      *   ha causato confusione nel reperimento dei tempi di consegna
071300051116      *   e/o nell'elenco delle località)
071400051116     c                   clear                   xx
071500051116do  1c                   DOW       xx       <  %elem(PRV)
071600051116     c                   add       1             xx
071700051116if  2c                   if        Prv(xx)  =  *blanks
071800051116     c                   leave
071900051116e   2c                   endif
072000051116      * - scarto le province per le quali è già stata impostata la
072100051116      *   "sigla" finale della provincia per il codice ordinamento
072200051116     c                   if        Pcor(xx) <> *blanks
072300051116     c                   iter
072400051116     c                   endif
072500051116      * - cerco le altre province con lo stesso codice tassazione
072600051116     c                   eval      I        =  xx
072700051116do  2c                   DOW       I        <  %elem(PRV)
072800051116     c                   add       1             I
072900051116     c                   eval      *in28    =  *off
073000051116     c     Pcts(xx)      lookup    Pcts(I)                                28
073100051116if  3c                   if        *in28    =  *off
073200051116     c                   leave
073300051116e   3c                   endif
073400051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
073500051116      *   memorizzo la provincia stessa - da posporre al codice
073600051116      *   ordinamento
073700051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
073800051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
073900051116      *   (ora memorizzo "   LO", "   FO", "   RN")
074000051116if  3c                   if        Pfcr(xx) =  *blanks
074100051116     c                   move      Prv(xx)       Pcor(xx)
074200051116e   3c                   endif
074300051116if  3c                   if        Pfcr(I)  =  *blanks
074400051116     c                   move      Prv(I)        Pcor(I)
074500051116e   3c                   endif
074600051116e   2c                   ENDDO
074700051116e   1c                   ENDDO
074800980211     C*---
074900960709     C* DATI CODICI TASSAZIONE
075000980211     C*---
075100020919     C                   Z-ADD     1             KTBKUT
075200960709     C                   MOVEL     'CT'          KTBCOD
075300041213DO  1C     1             DO        600           I
075400960709IF  2C     PRV(I)        IFNE      *BLANKS
075500960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
075600960709     C     KEYTAB        CHAIN     TABEL00F                           99
075700960709IF  3C     *IN99         IFEQ      *OFF
075800960709     C     TBLFLG        ANDEQ     *BLANKS
075900960709     C                   MOVEL     TBLUNI        DSCT
076000960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
076100960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
076200960709E   3C                   ENDIF
076300020925E   3C                   ENDIF
076400960709E   1C                   ENDDO
076500980302     C*---
076600010716     C* TERMINAL SU ELABORATORE
076700020925     c* SEDE    --> TUTTI I TERMINAL PARTENZA
076800020925     c* FILIALE --> TUTTI i TERMINAL PARTENZA DELL'AREA
076900980302     C*---
077000020925     c                   CLEAR                   OFil
077100020925     c                   CLEAR                   OTfp
077200020925IF  1C                   IF        DUTLPO = 'S'                                 TRASMISSIONE IN SEDE
077300020925     C                   clear                   trul26ds
077400020925     C                   EVAL      I26TLA = 'L'
077500020925     C                   EVAL      I26TPT = 'P'
077600020925     C                   CALL      'TRUL26R'
077700020925     C                   PARM                    trul26ds
077800020925     C                   Z-ADD     skpot         OFil                           *terminal -TUTTI-
077900020925X   1C                   ELSE                                                   RICEZIONE IN FILIALE
078000020925     C                   CLEAR                   tibs56ds
078100020925     C                   EVAL      I56PPO = DUTPOU
078200020925     C                   EVAL      I56MOD = 'POA'
078300020925     C                   CALL      'TIBS56R'
078400020925     C                   PARM                    tibs56ds
078500020925do  2C     1             DO        250           I
078600020925if  3C                   IF        ski(I) > *ZEROS
078700020925     C                   CLEAR                   FNLV55DS
078800020925     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
078900020925     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
079000020925     C                   Z-ADD     ski(I)        D55LIN                         *LINEA
079100020925     C                   Z-ADD     p96dri        D55DRF                         *DATA RIFERIMENTO
079200020925     C                   CALL      'FNLV55R'
079300020925     C                   PARM                    FNLV55DS
079400020925if  4C     D55ERR        IFNE      *BLANKS                                      *ERRORE
079500020925     C                   Z-ADD     dutpou        D55TFP                         *SE STESSA
079600020925e   4C                   ENDIF
079700020925     c                   Z-ADD     d55tfp        ski(I)                         *FIL diventa TFP
079800020925e   3C                   ENDIF
079900020925e   2C                   ENDDO
080000020925     C                   Z-ADD     ski           OFil                           *terminal -AREA-
080100020925e   1C                   ENDIF
080200020925     c*
080300020925     c* Estrapola SOLO i terminal
080400010716     C                   Z-ADD     *ZEROS        J                 5 0
080500020925do  1C     1             DO        250           I
080600020925if  2C                   IF        OFil(I) > *ZEROS
080700020925     C     OFil(i)       LOOKUP    OTfp                                   55
080800020925if  3C                   IF        *IN55 = *OFF                                 *NON TROVATO
080900010716     C                   ADD       1             J
081000020925     C                   Z-ADD     OFil(i)       OTfp(J)
081100020925e   3C                   ENDIF
081200020925e   2C                   ENDIF
081300020925e   1C                   ENDDO
081400960709     C*
081500960709     C                   ENDSR
081600020919     C*--------------------------------------------------------------------------------------------*
081700020919     C* REPERISCE I DATI UTENTE
081800020919     C*--------------------------------------------------------------------------------------------*
081900020919     C     REPDATIUTE    BEGSR
082000020919     C*
082100020919     C* INIZIALIZZA VARIABILI DI WRK
082200020919     C                   CLEAR                   TIBS34DS
082300020919     C                   CLEAR                   AZUTEDS
082400020919     C                   CLEAR                   DDATIUTE
082500020919     C*
082600020919     C     *DTAARA       DEFINE    §azute        azuteds
082700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
082800020919     C                   IN(E)     *DTAARA
082900020919if  1C                   IF        %Error
083000020919     c                   EVAL      I34Tla = 'L'
083100020919     C                   CALL      'TIBS34R'
083200020919     C                   PARM                    Tibs34Ds
083300020919     C                   IN        *DTAARA
083400020919e   1C                   ENDIF
083500020919     C*
083600020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
083700020919     C                   MOVEL(P)  RSUT          DSFIRS           20
083800020919     C*
083900020919     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* *INZSR - OPERAZIONI INIZIALI
084200000000     C*------------------------------------------------------------------------*
084300000000     C     *INZSR        BEGSR
084400980211     C*---
084500000000     C* RICEVIMENTO PARAMETRI
084600980211     C*---
084700000000     C     *ENTRY        PLIST
084800000000     C                   PARM                    KPJBA
084900000000     C                   MOVEL     KPJBU         PARAM
085000091204
085100091204     c* Se passata la data elaboro cappario alla data passata altrimenti
085200091204     c*  alla data corrente
085300091204     c                   clear                   wdatavers
085400091204     c                   if        pardtela>*zeros
085500091204     c                   movel     pardtela      wdatavers         8 0
085600091204     c                   endif
085700980211     C*---
085800000000     C* VARIABILI RIFERITE AL DATABASE
085900980211     C*---
086000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
086100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
086200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
086300020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
086400020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
086500020925     C     *LIKE         DEFINE    TBLKEY        KTBKEY
086600980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
086700980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
086800980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
086900980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
087000980216     C     *LIKE         DEFINE    CPCLNA        DEPTFP                         *DEPOSITI
087100980216     C     *LIKE         DEFINE    CPCLNA        DEPTFA
087200980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
087300980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
087400980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
087500980216     C     *LIKE         DEFINE    DLYFCR        DEPFCR
087600980216     C     *LIKE         DEFINE    DLYRAP        DEPRAP
087700980216     C     *LIKE         DEFINE    DLYCOR        DEPCOR
087800121015     C     *LIKE         DEFINE    CPCLNA        DEPLna
087900121015     C     *LIKE         DEFINE    CPCLNA        DEPLIN
088000121015     C     *LIKE         DEFINE    CPCLNA        DEPlol
088100121015     C     *LIKE         DEFINE    CPCLNA        DEPlos
088200980220     C     *LIKE         DEFINE    DLYTRI        DEPTIC
088300980220     C     *LIKE         DEFINE    DLYTRC        DEPTCC
088400980220     C     *LIKE         DEFINE    DLYTRI        DEPTIE
088500980220     C     *LIKE         DEFINE    DLYTRC        DEPTCE
088600090921     C     *LIKE         DEFINE    DLYTRI        DEPTIh
088700090921     C     *LIKE         DEFINE    DLYTRC        DEPTCh
088800980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
088900980216     C     *LIKE         DEFINE    CPCISO        WISO
089000980216     C     *LIKE         DEFINE    CPCLNA        WLNA
089100980219     C     *LIKE         DEFINE    CPLDLV        WDLV
089200980302     C     *LIKE         DEFINE    CPCLNA        WILNA
089300980302     C     *LIKE         DEFINE    CPCLOL        WILOL
089400121012     C     *LIKE         DEFINE    CPCLOS        WILOS
089500980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
089600980211     C*---
089700000000     C* CHIAVI DI LETTURA
089800980211     C*---
089900980211     C     KEYTAB        KLIST                                                  *TABEL00F
090000980211     C                   KFLD                    KTBKUT                          -UTENTE
090100980211     C                   KFLD                    KTBCOD                          -TABELLA
090200980211     C                   KFLD                    KTBKEY                          -CHIAVE
090300980211     C     KE2TAB        KLIST                                                  *TABEL00F
090400980211     C                   KFLD                    KTBKUT                          -UTENTE
090500980211     C                   KFLD                    KTBCOD                          -TEBELLA
090600020925     C     KEYTBE        KLIST                                                  *TNTBE01L
090700020925     C                   KFLD                    KBECOD                          -TABELLA
090800020925     C                   KFLD                    KBEKE1                          -CHIAVE
090900980211     C     KEYCPC        KLIST                                                  *AZCPC00F
091000980211     C                   KFLD                    KCCVER                          -VERSIONE
091100980211     C     KEYCPL        KLIST                                                  *AZCPL00F
091200980211     C                   KFLD                    KCLVER                          -VERSIONE
091300980216     C                   KFLD                    KCLNAR                          -NAZIONE
091400980216     C                   KFLD                    KCLCAP                          -CAP
091500121012     C     KEYCPE        KLIST                                                  *AZCPL00F
091600121012     C                   KFLD                    p96VER                          -VERSIONE
091700121012     C                   KFLD                    depNAR                          -NAZIONE
091800121012     C                   KFLD                    depCAP                          -CAP
091900121012     C                   KFLD                    deptfp                          -TFP
092000980211     C*---
092100000000     C* VARIABILI DI CONTROLLO
092200980211     C*---
092300980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
092400980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
092500980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
092600980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
092700050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
092800020527     C*
092900020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
093000020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
093100020527     C                   Z-ADD     N8            G08DAT
093200020527     C                   Z-ADD     *ZEROS        G08INV
093300020527     C                   MOVEL     '0'           G08ERR
093400020527     C                   CALL      'XSRDA8'
093500020527     C                   PARM                    WLBDA8
093600020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
093601150623     c* se data corrente cade in giorno festivo la incremento fino ad arrivare
093602150623     c* al primo giorno lavorativo
093603150624     c                   eval      datacur=%date(datcor:*iso)
093604150624     c                   exsr      sr_datafes
093605150624     c                   eval      datcor=%dec(datacur)
093606150624     c
093700020925     C*
093800020925     C* REPERISCE I DATI UTENTE
093900020925     C                   EXSR      REPDATIUTE
094000970707     C*
094100020527     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
094200020527     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
094300020919IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
094400030304     C***                Z-ADD     *ZEROS        P96DRI            8 0
094500030304     C                   Z-ADD     DATCOR        P96DRI            8 0
094600020527X   1C                   ELSE                                                   S E D E
094700020527     C                   Z-ADD     DATCOR        P96DRI            8 0
094800020527E   1C                   ENDIF
094900091204     c
095000091204     c* Se passata data uso questa
095100091204     c                   if        wdatavers>0
095200091204     C                   Z-ADD     wdatavers     P96DRI            8 0
095300091204     c                   endif
095400970707     C                   Z-ADD     *ZEROS        P96VER            5 0
095500970707     C                   CALL      'TISI96R'
095600970707     C                   PARM                    P96DRI
095700970707     C                   PARM                    P96VER
095800040415     C*
095900091211     C* REIPOSTO LA DATA CORRENTE (oppure la data passata al pgm ) nella data di riferimento
096000091211     C*  XCHE' IN USCITA il  *PGM TISI96R VALORIZZA IL P96DRI CON LA DATA D DECORRENZA
096100091211     C*  DELLA VERSIONE REPERITA
096200040415     C                   Z-ADD     DATCOR        P96DRI
096300091211     c                   if        wdatavers>0
096400091211     C                   Z-ADD     wdatavers     P96DRI            8 0
096500091211     c                   endif
096600960709     C*
096700000000     C                   ENDSR
096701150624      /free
096702150624       //--------------------------------------------------------------
096703150624       //?Controllo data se festiva + restituzione data prima data non festiva
096704150624       //--------------------------------------------------------------
096705150624       BEGSR  sr_DataFes;
096706150624       wfes='1';
096707150624       dow wfes='1';
096708150624       chain (0:0:%subdt(datacur:*years):
096709150624                        %subdt(datacur:*MONTHS)) azcln01l;
096710150624          if %found(azcln01l) and mat(%subdt(datacur:*days)) = 'F'
096711150624                               or pom(%subdt(datacur:*days)) = 'F';
096712150624       // data festiva: la incremento di un giorno
096713150624             datacur=datacur+%days(1);
096714150624          else;
096715150624            clear wfes;
096716150624          endif;
096717150624       enddo;
096718150624       endsr;
096719150624      /end-free
096800000000     C*------------------------------------------------------------------------*
096900000000**   CMD -COMANDI CLP-
097000050318CLRPFM FILE(WADLY00F)
097100091211CRTDUPOBJ OBJ(WADLY00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(EDPES)
097200091211CLRPFM FILE(EDPES/WADLY00F)
097300091211OVRDBF FILE(WADLY00F) TOFILE(EDPES/WADLY00F)
