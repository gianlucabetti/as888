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
001400150624     FAZcln01L  IF   E           K DISK
001500051114     FWADLY00F  O    E           K DISK    USROPN
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*-------------------
002000000000     D* COMANDI CLP
002100000000     D*-------------------
002200091211     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
002300960709     D*-------------------
002400980216     D* PROVINCIE
002500960709     D*-------------------
002600051116     d PRV             s                   dim(600)  inz  like(DLYprv)          *PROVINCIA
002700051116     d Pcts            s                   dim(600)  inz  like(§PRcts)          *CODICE TASSAZIONE
002800051116     d Pfcr            s                   dim(600)  inz  like(DLYfcr)          *CAPOLUOGO DI REGION
002900051116     d Prap            s                   dim(600)  inz  like(DLYrap)          *REGIONE APPARTENENZ
003000051116     d Pcor            s                   dim(600)  inz  like(DLYcor)          *ORDINAMENTO
003100010716     D*-------------------
003200010716     D* TERMINAL PARTENZA
003300010716     D*-------------------
003400051116     D OFIL            S              3  0 DIM(250)  inz                        *CODICE
003500051116     D OTFP            S              3  0 DIM(250)  inz                        *CODICE
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900980210     D*-------------------
004000980210     D* DS REPERIMENTO DATI UTENTE
004100980210     D*-------------------
004200020919     D TIBS34DS      E DS                                                       *Profili utente
004300020919     D DDATIUTE      E DS                                                       *Dati utente
004400020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004500000000     D*-------------------
004600980210     D* DS CALCOLA TEMPI DELIVERY
004700000000     D*-------------------
004800980210     D SD99DS        E DS                  EXTNAME(TNSD99DS)
004900971020     D*------------------
005000980210     D* DS REPERIMENTO TERMINALs
005100971020     D*------------------
005200020925     D FNLV55DS      E DS
005300960709     D*-------------------
005400960709     D* DS PROVINCIE
005500960709     D*-------------------
005600960709     D DSPR          E DS
005700000000     D*-------------------
005800000000     D* DS CODICI TASSAZIONE
005900000000     D*-------------------
006000000000     D DSCT          E DS
006100020925     D*-------------------
006200020925     D* DS STATISTICHE DATI DIVERSI
006300020925     D*-------------------
006400020925     D DSDD          E DS
006500980210     D*------------------
006600980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006700980210     D*------------------
006800980210     D WLBDA8          DS
006900980210     D  G08DAT                 1      8  0
007000980210     D  G08INV                 9     16  0
007100980210     D  G08ERR                17     17
007200980210     D  G08TGI                18     22  0
007300000000     D*-------------------
007400000000     D* DS PARAMETRI DI LANCIO
007500000000     D*-------------------
007600980210     D PARAM           DS
007700980210     D  PARLNP                 1      3
007800980210     D  PARTEM                 4      4
007900980210     D  PARTSP                 5      5
008000980210     D  PARISO                 6      6
008100980210     D  PARDIS                 7      7
008200980210     D  PARSSA                 8      8
008300980210     D  PARND1                 9     83
008400980210     D  PARND2                84    158
008500980210     D  PARND3               159    233
008600091204     d  PARlin               234    234
008700091204     d  PARold               235    235
008800091204     d  PARdtEla             236    243
008900091211     d  PARelaprova          244    244
009000980210     D*-------------------
009100980210     D* ARCHITETTURA
009200980210     D*-------------------
009300980210     D KPJBA         E DS
009400020830     D*-------------------
009500020925     D* DS REPERIMENTO P.O. IN GESTIONE
009600020830     D*-------------------
009700020925     D tibs56ds      E DS                                                       *per FILIALE remota
009800020830     D  ski                    7    756  0 dim(250)
009900020925     D trul26ds      E DS                                                       *per SEDE sede
010000020925     D  skpo                  11    760  0 dim(250)
010100020925     D  skpot                761   1510  0 dim(250)
010200150624
010300150624     D clnmat          DS
010400150624     D  mat                    1     31    dim(31)
010500150624     D clnpom          DS
010600150624     D  pom                    1     31    dim(31)
010700051116      *
010800051116      * V A R I A B I L I
010900051116      *
011000051116     d XX              s              3  0 inz
011100150624     d datacur         s               d   datfmt(*iso)
011200150624     d wfes            s              1
011300000000     C*------------------------------------------------------------------------*
011400000000     C* MAIN LINE
011500000000     C*------------------------------------------------------------------------*
011600051116      *
011700051116      * Carica le tabelle occorrenti
011800051116     c                   exsr      CarTAB
011900020925     C*
012000050318     C* PULISCE E RICREA IL FILE SOLO SE:
012100020925     C* data corrente > data ultima creazione workfile
012200020925IF  0c     DATCOR        IFGT      §SDDDLY
012300020925     C*
012400050318     C* OPERAZIONI INIZIALI APERTURA FILE
012500020925     C                   EXSR      OPNFIL
012600000000     C*
012700980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
012800980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
012900000000     C                   EXSR      SETFIL
013000000000     C*
013100000000     C* CICLO FINO A FINE FILE
013200000000DO  2C     $FINE         DOWEQ     '0'
013300980216     C*
013400980216     C* OPERAZIONI PER NUOVO CAP
013500980216     C                   EXSR      NEWCAP
013600980216     C*
013700980216     C* CICLO FINO A ROTTURA CAP
013800980216DO  3C     $FINE         DOWEQ     '0'
013900980216     C     CPCNAR        ANDEQ     DEPNAR
014000980216     C     CPCCAP        ANDEQ     DEPCAP
014100980302     C*
014200980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
014300980302     C                   Z-ADD     CPCLNA        WILNA                          *LINEA ARRIVO
014400121015     C                   Z-ADD     CPClna        deplna                         *LINEA OLTRE
014500980302     C                   Z-ADD     CPCLOL        WILOL                          *LINEA OLTRE
014600121015     C                   Z-ADD     CPCLOL        deplol                         *LINEA OLTRE
014700121012     C                   Z-ADD     CPCLOS        WILOS                          *LINEA sotto
014800121015     C                   Z-ADD     CPCLOS        deplos                         *LINEA sotto
014900980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
015000980302     C                   EXSR      CALLNA
015100980302     C                   Z-ADD     WOLIN         DEPLIN                         *DEPOSITO LNA CAP
015200980302     C*
015300020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
015400980220     C                   MOVEL     'C'           $DLY
015500980216     C                   MOVEL     *BLANKS       WLOC
015600980216     C                   MOVEL     CPCISO        WISO
015700980302     C                   Z-ADD     WOLIN         WLNA
015800980219     C                   MOVEL     'N'           WDLV
015900121015     c
016000020925do  4C     1             DO        250           J
016100020925if  5C                   IF        OTFP(J) > *ZEROS
016200010716     C                   Z-ADD     OTFP(J)       DEPTFP
016300121015     c
016400121012     c* per ogni terminal di partenza verifico se c'e' una eccezione
016500121015     c                   if        cpcese='S'
016600121012     c     keycpe        chain     azcpe01l
016700121012     c                   if        %found(azcpe01l) and cpeatb=' '
016800121012     C                   Z-ADD     CPeLNA        WILNA                          *LINEA ARRIVO
016900121012     C                   Z-ADD     CPeLOL        WILOL                          *LINEA OLTRE
017000121012     C                   Z-ADD     CPeLOS        WILOS                          *LINEA sotto
017100121012     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
017200121012     C                   EXSR      CALLNA
017300121012     C                   Z-ADD     WOLIN         WLNA
017400121015     c                   else
017500121015     C                   Z-ADD     depLNA        WILNA                          *LINEA ARRIVO
017600121015     C                   Z-ADD     depLOL        WILOL                          *LINEA OLTRE
017700121015     C                   Z-ADD     depLOS        WILOS                          *LINEA sotto
017800121015     C                   Z-ADD     deplin        WLNA
017900121012     c                   endif
018000121015     c                   endif
018100121012     c
018200980210     C                   EXSR      CREDLY
018300121012     c
018400040415     C                   Z-ADD     *ZEROS        DEPTIC
018500040415     C                   Z-ADD     *ZEROS        DEPTCC
018600090515     C                   Z-ADD     *ZEROS        DEPTIe
018700090515     C                   Z-ADD     *ZEROS        DEPTCe
018800090921     C                   Z-ADD     *ZEROS        DEPTIh
018900090921     C                   Z-ADD     *ZEROS        DEPTCh
019000020925e   5C                   ENDIF
019100020925e   4C                   ENDDO
019200980216     C*
019300980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
019400980216     C                   EXSR      SETCPL
019500980216     C*
019600980216     C* CICLO FINO A FINE FILE
019700980216DO  4C     $FINL         DOWEQ     '0'
019800980216     C*
019900980216     C* SCRIVE IL RECORD DEL WORKFILE
020000980220     C                   MOVEL     'L'           $DLY
020100980216     C                   MOVEL     CPLLOC        WLOC
020200980220     C                   MOVEL     CPLDLV        WDLV
020300980302     C*
020400980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
020500980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
020600980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
020700980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
020800121012     C                   Z-ADD     CPLLOs        WILOs                          *LINEA sotto
020900980302     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
021000980302     C                   EXSR      CALLNA
021100980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
021200980302     C                   Z-ADD     WOLIN         WLNA
021300980220X   5C                   ELSE                                                   *LIVELLO CAP
021400980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
021500980302     C                   Z-ADD     DEPLIN        WLNA
021600121015     C                   Z-ADD     DEPlna        Wilna
021700121015     C                   Z-ADD     DEPlol        Wilol
021800121015     C                   Z-ADD     DEPlos        Wilos
021900980220E   5C                   ENDIF
022000020925     C*
022100020925     C* SCRIVE IL RECORD DEL WORKFILE per tutti i terminal in gestione
022200020925do  5C     1             DO        250           J
022300020925if  6C                   IF        OTFP(J) > *ZEROS
022400020925     C                   Z-ADD     OTFP(J)       DEPTFP
022500121015     c
022600121015IF  7C     CPLLIV        IFEQ      'C'                                          *LIVELLO LOCALITA'
022700121015     c
022800121015    8c                   if        cpcese='S'
022900121015     c* per ogni terminal di partenza verifico se c'e' una eccezione
023000121015     c     keycpe        chain     azcpe01l
023100121015    9c                   if        %found(azcpe01l) and cpeatb=' '
023200121015     C                   Z-ADD     CPeLNA        WILNA                          *LINEA ARRIVO
023300121015     C                   Z-ADD     CPeLOL        WILOL                          *LINEA OLTRE
023400121015     C                   Z-ADD     CPeLOS        WILOS                          *LINEA sotto
023500121015     C                   Z-ADD     *ZEROS        WOLIN                          *LINEA DA CONSIDERAR
023600121015     C                   EXSR      CALLNA
023700121015     C                   Z-ADD     WOLIN         WLNA
023800121015   x9c                   else
023900121015     C                   Z-ADD     depLNA        WILNA                          *LINEA ARRIVO
024000121015     C                   Z-ADD     depLOL        WILOL                          *LINEA OLTRE
024100121015     C                   Z-ADD     depLOS        WILOS                          *LINEA sotto
024200121015     C                   Z-ADD     deplin        WLNA
024300121015    9c                   endif
024400121015    8c                   endif
024500121015     c
024600121015    7c                   endif
024700121015     c
024800020925     C                   EXSR      CREDLY
024900121015     c
025000040415     C                   Z-ADD     *ZEROS        DEPTIC
025100040415     C                   Z-ADD     *ZEROS        DEPTCC
025200090515     C                   Z-ADD     *ZEROS        DEPTIe
025300090515     C                   Z-ADD     *ZEROS        DEPTCe
025400090921     C                   Z-ADD     *ZEROS        DEPTIh
025500090921     C                   Z-ADD     *ZEROS        DEPTCh
025600020925e   6C                   ENDIF
025700020925e   5C                   ENDDO
025800980216     C*
025900980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
026000980216     C                   EXSR      LETCPL
026100980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
026200000000     C*
026300980216     C* LETTURA RECORD SUCCESSIVO -CAP-
026400000000     C                   EXSR      LETFIL
026500980216E   3C                   ENDDO                                                  *ROTTURA CAP
026600980216E   2C                   ENDDO                                                  *FINE -CAP-
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C* OPERAZIONI DI CHIUSURA
027000971020     C                   EXSR      FINSR
027100020925E   0C                   ENDIF
027200000000     C*
027300000000     C                   SETON                                        LR
027400000000     C*------------------------------------------------------------------------*
027500980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
027600000000     C*------------------------------------------------------------------------*
027700000000     C     SETFIL        BEGSR
027800000000     C*
027900970707     C                   Z-ADD     P96VER        KCCVER
028000970707     C     KEYCPC        SETLL     AZCPC01L                           99
028100000000IF  1C     *IN99         IFEQ      *ON
028200000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
028300000000X   1C                   ELSE
028400000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
028500000000E   1C                   ENDIF
028600000000     C*
028700000000     C                   ENDSR
028800000000     C*------------------------------------------------------------------------*
028900980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
029000000000     C*------------------------------------------------------------------------*
029100000000     C     LETFIL        BEGSR
029200000000     C*
029300000000     C* LEGGE FINO A:
029400000000     C*     - FINE FILE
029500000000     C*     - TROVATO RECORD VALIDO
029600000000     C                   MOVEL     'N'           WRECOK
029700000000DO  1C     $FINE         DOWEQ     '0'
029800000000     C     WRECOK        ANDEQ     'N'
029900970707     C     KEYCPC        READE     AZCPC01L                               99
030000000000IF  2C     *IN99         IFEQ      *ON
030100000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
030200000000X   2C                   ELSE
030300000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
030400000000E   2C                   ENDIF
030500000000E   1C                   ENDDO
030600960701     C*
030700000000     C                   ENDSR
030800000000     C*------------------------------------------------------------------------*
030900980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
031000000000     C*------------------------------------------------------------------------*
031100000000     C     CHKREC        BEGSR
031200000000     C*
031300000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
031400000000     C*
031500000000     C* CAP ESTERO, ESCLUDE
031600970707IF  1C     CPCNAR        IFNE      *BLANKS
031700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031800000000E   1C                   ENDIF
031900000000     C*
032000000000     C* CAP FITTIZIO, ESCLUDE
032100970707IF  1C     CPCFIT        IFEQ      'S'
032200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032300000000E   1C                   ENDIF
032400980217     C*
032500980217     C* CAP OBSOLETO, ESCLUDE
032600980217IF  1C     CPCFI1        IFEQ      'O'
032700980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032800980217E   1C                   ENDIF
032900000000     C*
033000000000     C                   ENDSR
033100980302     C*------------------------------------------------------------------------*
033200980302     C* CALLNA - CALCOLA LA LINEA ARRIVO DA CONSIDERARE
033300980302     C*------------------------------------------------------------------------*
033400980302     C     CALLNA        BEGSR
033500020802     C*
033600980302     C                   Z-ADD     WILNA         WOLIN                          *LINEA STANDARD
033700980302     C*
033800980302     C                   ENDSR
033900980216     C*------------------------------------------------------------------------*
034000980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
034100980216     C*------------------------------------------------------------------------*
034200980216     C     NEWCAP        BEGSR
034300980216     C*
034400980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
034500980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
034600980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
034700980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
034800980220     C*
034900980220     C* AZZERA LE VARIABILI DI LAVORO
035000980220     C                   Z-ADD     *ZEROS        DEPTIC                         *TEMPO INTERNO CORRI
035100980220     C                   Z-ADD     *ZEROS        DEPTCC                         *TEMPO CLIENTE CORRI
035200090921     C                   Z-ADD     *ZEROS        DEPTIE                         *TEMPO INTERNO "E"
035300090921     C                   Z-ADD     *ZEROS        DEPTCE                         *TEMPO CLIENTE "E"
035400090921     C                   Z-ADD     *ZEROS        DEPTIh                         *TEMPO INTERNO "H"
035500090921     C                   Z-ADD     *ZEROS        DEPTCh                         *TEMPO CLIENTE "H"
035600980216     C*
035700980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
035800980216     C                   Z-ADD     1             I
035900980216     C                   SETOFF                                           28
036000980216     C     CPCPRV        LOOKUP    PRV(I)                                 28
036100980216IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
036200980216     C                   MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
036300980216     C                   MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
036400980216     C                   MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
036500980216X   1C                   ELSE                                                   *NON TROVATO
036600980216     C                   MOVEL     *BLANKS       DEPFCR
036700980216     C                   MOVEL     *BLANKS       DEPRAP
036800980216     C                   MOVEL     *BLANKS       DEPCOR
036900980216E   1C                   ENDIF
037000980216     C*
037100980216     C                   ENDSR
037200980216     C*------------------------------------------------------------------------*
037300980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
037400980216     C*------------------------------------------------------------------------*
037500980216     C     SETCPL        BEGSR
037600980216     C*
037700980216     C* REIMPOSTA LE VARIABILI DI LAVORO
037800980216     C                   MOVEL     '0'           $FINL
037900980216     C*
038000980216     C                   Z-ADD     P96VER        KCLVER
038100980216     C                   MOVEL     DEPNAR        KCLNAR
038200980216     C                   MOVEL     DEPCAP        KCLCAP
038300980216     C     KEYCPL        SETLL     AZCPL01L                           98
038400980216IF  1C     *IN98         IFEQ      *ON
038500980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
038600980216X   1C                   ELSE
038700980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
038800980216E   1C                   ENDIF
038900980216     C*
039000980216     C                   ENDSR
039100980216     C*------------------------------------------------------------------------*
039200980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
039300980216     C*------------------------------------------------------------------------*
039400980216     C     LETCPL        BEGSR
039500980216     C*
039600980216     C* LEGGE FINO A:
039700980216     C*     - FINE FILE
039800980216     C*     - TROVATO RECORD VALIDO
039900980216     C                   MOVEL     'N'           WRECOK
040000980216DO  1C     $FINL         DOWEQ     '0'
040100980216     C     WRECOK        ANDEQ     'N'
040200980216     C     KEYCPL        READE     AZCPL01L                               98
040300980216IF  2C     *IN98         IFEQ      *ON
040400980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
040500980216X   2C                   ELSE
040600980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
040700980216E   2C                   ENDIF
040800980216E   1C                   ENDDO
040900980216     C*
041000980216     C                   ENDSR
041100980216     C*------------------------------------------------------------------------*
041200980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
041300980216     C*------------------------------------------------------------------------*
041400980216     C     CHKCPL        BEGSR
041500980216     C*
041600980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
041700980216     C*
041800980217     C* LOCALITA' OBSOLETA, ESCLUDE
041900980217IF  1C     CPLFI1        IFEQ      'O'
042000980217     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042100980217E   1C                   ENDIF
042200980216     C*
042300980216     C                   ENDSR
042400980216     C*------------------------------------------------------------------------*
042500980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
042600980216     C*------------------------------------------------------------------------*
042700980216     C     CREDLY        BEGSR
042800980216     C*
042900050318     C                   CLEAR                   WADLY000
043000980216     C*---
043100980216     C* DATI DEDOTTI
043200980216     C*---
043300980216     C                   MOVEL     DEPTFP        DLYLNP                         *LINEA PARTENZA
043400980216     C                   MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
043500980216     C                   MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
043600980216     C                   MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
043700980216     C*---
043800980216     C* DATI CAPPARIO
043900980216     C*---
044000980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
044100980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
044200980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
044300980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
044400980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
044500980219     C                   MOVEL     WDLV          DLYDLV                         *STAMPA LOCALITA'
044600980216     C*---
044700980216     C* DATI CALCOLATI
044800980216     C*---
044900980220     C                   MOVEL     'C'           DLYTSP                         *TIPO SERVIZIO
045000980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
045100980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
045200040415     C     DEPTIC        ANDNE     *ZEROS                                       *CAMBIO D TP
045300040415     C     DEPTCC        ANDNE     *ZEROS                                       *CAMBIO D TP
045400980220     C                   Z-ADD     DEPTIC        DLYTRI                         *TEMPO INTERNO CORRI
045500980220     C                   Z-ADD     DEPTCC        DLYTRC                         *TEMPO CLIENTE CORRI
045600980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
045700040415     C                                                                          *'L' O NEW TERM.PART
045800980216     C*
045900980216     C* RECUPERA IL TERMINAL ARRIVO DEL CAP
046000980216     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
046100980216     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
046200980216     C                   Z-ADD     WLNA          D55LIN                         *LINEA
046300020527     C                   Z-ADD     P96DRI        D55DRF                         *DATA RIFERIMENTO
046400980216     C                   Z-ADD     DEPTFP        D55LNP                         *LINEA PARTENZA
046500980216     C                   CALL      'FNLV55R'
046600020925     C                   PARM                    FNLV55DS
046700980220IF  2C     D55ERR        IFNE      *BLANKS                                      *ERRORE
046800980216     C                   Z-ADD     WLNA          D55TFA                         *SE STESSA
046900980220E   2C                   ENDIF
047000980216     C                   Z-ADD     D55TFA        DEPTFA                         *TERMINAL ARRIVO
047100090921     C* "C"- esxpress
047200980216     C                   CLEAR                   SD99DS
047300980216     C                   MOVEL     'C'           D98TSP                         *TIPO SERVIZIO
047400020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
047500980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
047600980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
047700980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
047800980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
047900980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
048000980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
048100980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
048200980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
048300980216     C                   Z-ADD     1             D98NCD                         *COLLI
048400980216     C                   CALL      'TNSD99R'
048500980216     C                   PARM                    SD99DS
048600980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
048700980220IF  3C     D98TRI        IFGT      24
048800980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
048900980220X   3C                   ELSE
049000980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
049100980220E   3C                   ENDIF
049200980220IF  3C     D98TRC        IFGT      24
049300980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
049400980220X   3C                   ELSE
049500980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
049600980220E   3C                   ENDIF
049700980220X   2C                   ELSE                                                   *ERRORI
049800980216     C                   Z-ADD     *ZEROS        DLYTRI
049900980216     C                   Z-ADD     *ZEROS        DLYTRC
050000980220E   2C                   ENDIF
050100980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
050200980220     C                   Z-ADD     DLYTRI        DEPTIC                         *TEMPO INTERNO CORRI
050300980220     C                   Z-ADD     DLYTRC        DEPTCC                         *TEMPO CLIENTE CORRI
050400980220E   2C                   ENDIF
050500980220E   1C                   ENDIF
050600090921
050700121012     c* imposto le 3 linee di arrivo previste dal cap/località
050800121012     c                   z-add     wilna         dlylna
050900121012     c                   z-add     wilol         dlylnao
051000121012     c                   z-add     wilos         dlylnas
051100121012     c
051200050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
051300090921     c
051400090921     C* "E"- priority
051500980220     C                   MOVEL     'E'           DLYTSP                         *TIPO SERVIZIO
051600980220IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
051700980220     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
051800090515     C     DEPTIe        ANDNE     *ZEROS                                       *CAMBIO D TP
051900090515     C     DEPTCe        ANDNE     *ZEROS                                       *CAMBIO D TP
052000980220     C                   Z-ADD     DEPTIE        DLYTRI                         *TEMPO INTERNO ESPRE
052100980220     C                   Z-ADD     DEPTCE        DLYTRC                         *TEMPO CLIENTE ESPRE
052200980220X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
052300980216     C                   CLEAR                   SD99DS
052400980216     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO
052500020527     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
052600980216     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
052700980216     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
052800980216     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
052900980216     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
053000980226     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
053100980216     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
053200980216     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
053300980216     C                   MOVEL     WISO          D98FIN                         *INOLTRO
053400980216     C                   Z-ADD     1             D98NCD                         *COLLI
053500980216     C                   CALL      'TNSD99R'
053600980216     C                   PARM                    SD99DS
053700980220IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
053800980220IF  3C     D98TRI        IFGT      24
053900980219     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
054000980220X   3C                   ELSE
054100980219     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
054200980220E   3C                   ENDIF
054300980220IF  3C     D98TRC        IFGT      24
054400980219     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
054500980220X   3C                   ELSE
054600980219     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
054700980220E   3C                   ENDIF
054800980220X   2C                   ELSE                                                   *ERRORI
054900980216     C                   Z-ADD     *ZEROS        DLYTRI
055000980216     C                   Z-ADD     *ZEROS        DLYTRC
055100980220E   2C                   ENDIF
055200980220IF  2C     $DLY          IFEQ      'C'                                          *CAP
055300980220     C                   Z-ADD     DLYTRI        DEPTIE                         *TEMPO INTERNO ESPRE
055400980220     C                   Z-ADD     DLYTRC        DEPTCE                         *TEMPO CLIENTE ESPRE
055500980220E   2C                   ENDIF
055600980220E   1C                   ENDIF
055700050318     C                   WRITE     WADLY000                                     I N S E R I M E N T
055800980216     C*
055900090921     C* "H"- H 10 30
056000090921     C                   MOVEL     'H'           DLYTSP                         *TIPO SERVIZIO
056100090921IF  1C     $DLY          IFEQ      'L'                                          *LOCALITA'
056200090921     C     CPLLIV        ANDEQ     'C'                                          *A LIVELLO DI CAP
056300090921     C     DEPTIh        ANDNE     *ZEROS                                       *CAMBIO D TP
056400090921     C     DEPTCh        ANDNE     *ZEROS                                       *CAMBIO D TP
056500090921     C                   Z-ADD     DEPTIh        DLYTRI                         *TEMPO INTERNO ESPRE
056600090921     C                   Z-ADD     DEPTCh        DLYTRC                         *TEMPO CLIENTE ESPRE
056700090921X   1C                   ELSE                                                   *CAP O LOC A LIVELLO
056800090921     C                   CLEAR                   SD99DS
056900090921     C                   MOVEL     'H'           D98TSP                         *TIPO SERVIZIO
057000090921     C                   Z-ADD     P96DRI        D98DSP                         *DATA SPEDIZIONE
057100090921     C                   Z-ADD     DEPTFP        D98LNP                         *FILIALE PARTENZA
057200090921     C                   Z-ADD     DEPTFP        D98TFP                         *TERMINAL PARTENZA
057300090921     C                   MOVEL     DEPNAR        D98NZD                         *NAZIONE DESTINATARI
057400090921     C                   MOVEL     DEPCAP        D98CAD                         *CAP DESTINATARIO
057500090921     C                   MOVEL     WLOC          D98LOD                         *LOCALITA'
057600090921     C                   Z-ADD     WLNA          D98LNA                         *LINEA DI ARRIVO
057700090921     C                   Z-ADD     DEPTFA        D98TFA                         *TERMINAL ARRIVO
057800090921     C                   MOVEL     WISO          D98FIN                         *INOLTRO
057900090921     C                   Z-ADD     1             D98NCD                         *COLLI
058000090921     C                   CALL      'TNSD99R'
058100090921     C                   PARM                    SD99DS
058200090921IF  2C     D98ERR        IFEQ      '0'                                          *NO ERRORI
058300090921IF  3C     D98TRI        IFGT      24
058400090921     C     D98TRI        SUB       24            DLYTRI                         *TEMPO RESA INTERNO
058500090921X   3C                   ELSE
058600090921     C                   Z-ADD     D98TRI        DLYTRI                         *TEMPO RESA INTERNO
058700090921E   3C                   ENDIF
058800090921IF  3C     D98TRC        IFGT      24
058900090921     C     D98TRC        SUB       24            DLYTRC                         *TEMPO RESA CLIENTE
059000090921X   3C                   ELSE
059100090921     C                   Z-ADD     D98TRC        DLYTRC                         *TEMPO RESA CLIENTE
059200090921E   3C                   ENDIF
059300090921X   2C                   ELSE                                                   *ERRORI
059400090921     C                   Z-ADD     *ZEROS        DLYTRI
059500090921     C                   Z-ADD     *ZEROS        DLYTRC
059600090921E   2C                   ENDIF
059700090921IF  2C     $DLY          IFEQ      'C'                                          *CAP
059800090921     C                   Z-ADD     DLYTRI        DEPTIh                         *TEMPO INTERNO ESPRE
059900090921     C                   Z-ADD     DLYTRC        DEPTCh                         *TEMPO CLIENTE ESPRE
060000090921E   2C                   ENDIF
060100090921E   1C                   ENDIF
060200090921     c
060300090921     C                   WRITE     WADLY000                                     I N S E R I M E N T
060400980216     C                   ENDSR
060500000000     C*------------------------------------------------------------------------*
060600971020     C* FINSR - OPERAZIONI FINALI
060700000000     C*------------------------------------------------------------------------*
060800971020     C     FINSR         BEGSR
060900971020     C*
061000971020     C* CHIUDE I FILE DI LAVORO
061100051114     C                   CLOSE     WADLY00F
061200020925     C*
061300160802     C* AGGIORNA LA DATA ULTIMA CREAZIONE WORKFILE CON LA DATA CORRENTE (SOLO SE LANCIO BUONO)
061400160802     C                   IF        PARelaprova = *blank
061500020925     C                   MOVEL(P)  'SDD'         KBECOD
061600020925     C                   MOVEL(P)  '1'           KBEKE1
061700020925     C     KEYTBE        CHAIN     TNTBE01L
061800020925IF  1C                   IF        %found(TNTBE01L)
061900020925     C                   Z-ADD     DATCOR        §SDDDLY
062000020925     C                   MOVEL     DSDD          TBEUNI
062100020925     C                   UPDATE    TNTBE000
062200020925E   1C                   ENDIF
062300160802     C                   ENDIF
062400971020     C*
062500980211     C* CHIUDE I PGM CHIAMATI
062600980210     C                   CLEAR                   SD99DS                         *CALCOLA DELIVERY
062700000000     C                   MOVEL     'C'           D98TLA
062800971009     C                   CALL      'TNSD99R'
062900980210     C                   PARM                    SD99DS
063000980211     C*
063100020925     C                   CLEAR                   FNLV55DS                       *RECUPERA TERMINALs
063200971020     C                   MOVEL     'C'           D55TLA
063300971020     C                   CALL      'FNLV55R'
063400020925     C                   PARM                    FNLV55DS
063500000000     C*
063600000000     C                   ENDSR
063700000000     C*------------------------------------------------------------------------*
063800050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
063900000000     C*------------------------------------------------------------------------*
064000000000     C     OPNFIL        BEGSR
064100091211     C                   Z-ADD     80            LENGH            15 5
064200091211     c* se si tratta di una prova, creo il file, vuoto in EDPES
064300091211    1c                   if        parelaprova='S'
064400091211     c* creazione file
064500091211     C                   MOVEL(P)  CMD(2)        COMMAN           80
064600091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
064700091211     C                   PARM                    COMMAN
064800091211     C                   PARM                    LENGH
064900091211     c
065000091211     c* Se c'e' già lo pulisco
065100091211    2C                   IF        %error
065200091211     C                   MOVEL(P)  CMD(3)        COMMAN           80
065300091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
065400091211     C                   PARM                    COMMAN
065500091211     c                   PARM                    LENGH
065600091211    3C                   IF        %error
065700091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
065800091211    3C                   ENDIF
065900091211    2C                   ENDIF
066000091211     c
066100091211     c* Eseguo ovrdbf
066200091211     C                   MOVEL(P)  CMD(4)        COMMAN           80
066300091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
066400091211     C                   PARM                    COMMAN
066500091211     c                   PARM                    LENGH
066600091211    3C                   IF        %error
066700091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
066800091211    3C                   ENDIF
066900091214     c
067000091211   x1c                   else
067100020925     c*
067200050318     c* Pulisce il file
067300030304     C                   MOVEL(P)  CMD(1)        COMMAN           80
067400050318     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
067500030304     C                   PARM                    COMMAN
067600000000     C                   PARM                    LENGH
067700091211    2C                   IF        %error
067800021202     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
067900091211    2C                   ENDIF
068000091211    1C                   ENDIF
068100020925     C*
068200050318     C* Apre il file
068300051114     C                   OPEN      WADLY00F                                     *APRE FILE
068400960701     C*
068500000000     C                   ENDSR
068600960709     C*------------------------------------------------------------------------*
068700980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
068800960709     C*------------------------------------------------------------------------*
068900960709     C     CARTAB        BEGSR
069000020925     C*---
069100020925     C* Dati statistiche
069200020925     C*---
069300020925     C                   CLEAR                   DSDD
069400020925     C                   MOVEL(P)  'SDD'         KBECOD
069500020925     C                   MOVEL(P)  '1'           KBEKE1
069600020925     C     KEYTBE        CHAIN(N)  TNTBE01L
069700020925     C                   IF        %found(TNTBE01L)
069800020925     C                   MOVEL     TBEUNI        DSDD
069900020925     C                   ENDIF
070000980211     C*---
070100980211     C* PROVINCIE
070200980211     C*---
070300980216     C                   Z-ADD     *ZEROS        I                 5 0
070400020919     C                   Z-ADD     1             KTBKUT
070500960709     C                   MOVEL     'PR'          KTBCOD
070600960709     C     KE2TAB        SETLL     TABEL00F
070700960709     C     KE2TAB        READE     TABEL00F                               99
070800960709DO  1C     *IN99         DOWEQ     *OFF
070900960709IF  2C     TBLFLG        IFEQ      *BLANKS
071000960709     C                   ADD       1             I
071100960709     C                   MOVEL     TBLUNI        DSPR
071200960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
071300960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
071400051116     c                   movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
071500960709E   2C                   ENDIF
071600960709     C     KE2TAB        READE     TABEL00F                               99
071700960709E   1C                   ENDDO
071800051116      *
071900051116      * Imposta il progressivo per le province (diverse) con lo stesso
072000051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
072100051116      *   - che NON può essere uguale per due province diverse:
072200051116      *   ha causato confusione nel reperimento dei tempi di consegna
072300051116      *   e/o nell'elenco delle località)
072400051116     c                   clear                   xx
072500051116do  1c                   DOW       xx       <  %elem(PRV)
072600051116     c                   add       1             xx
072700051116if  2c                   if        Prv(xx)  =  *blanks
072800051116     c                   leave
072900051116e   2c                   endif
073000051116      * - scarto le province per le quali è già stata impostata la
073100051116      *   "sigla" finale della provincia per il codice ordinamento
073200051116     c                   if        Pcor(xx) <> *blanks
073300051116     c                   iter
073400051116     c                   endif
073500051116      * - cerco le altre province con lo stesso codice tassazione
073600051116     c                   eval      I        =  xx
073700051116do  2c                   DOW       I        <  %elem(PRV)
073800051116     c                   add       1             I
073900051116     c                   eval      *in28    =  *off
074000051116     c     Pcts(xx)      lookup    Pcts(I)                                28
074100051116if  3c                   if        *in28    =  *off
074200051116     c                   leave
074300051116e   3c                   endif
074400051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
074500051116      *   memorizzo la provincia stessa - da posporre al codice
074600051116      *   ordinamento
074700051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
074800051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
074900051116      *   (ora memorizzo "   LO", "   FO", "   RN")
075000051116if  3c                   if        Pfcr(xx) =  *blanks
075100051116     c                   move      Prv(xx)       Pcor(xx)
075200051116e   3c                   endif
075300051116if  3c                   if        Pfcr(I)  =  *blanks
075400051116     c                   move      Prv(I)        Pcor(I)
075500051116e   3c                   endif
075600051116e   2c                   ENDDO
075700051116e   1c                   ENDDO
075800980211     C*---
075900960709     C* DATI CODICI TASSAZIONE
076000980211     C*---
076100020919     C                   Z-ADD     1             KTBKUT
076200960709     C                   MOVEL     'CT'          KTBCOD
076300041213DO  1C     1             DO        600           I
076400960709IF  2C     PRV(I)        IFNE      *BLANKS
076500960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
076600960709     C     KEYTAB        CHAIN     TABEL00F                           99
076700960709IF  3C     *IN99         IFEQ      *OFF
076800960709     C     TBLFLG        ANDEQ     *BLANKS
076900960709     C                   MOVEL     TBLUNI        DSCT
077000960709     C                   MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
077100960709     C                   MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
077200960709E   3C                   ENDIF
077300020925E   3C                   ENDIF
077400960709E   1C                   ENDDO
077500980302     C*---
077600010716     C* TERMINAL SU ELABORATORE
077700020925     c* SEDE    --> TUTTI I TERMINAL PARTENZA
077800020925     c* FILIALE --> TUTTI i TERMINAL PARTENZA DELL'AREA
077900980302     C*---
078000020925     c                   CLEAR                   OFil
078100020925     c                   CLEAR                   OTfp
078200020925IF  1C                   IF        DUTLPO = 'S'                                 TRASMISSIONE IN SEDE
078300020925     C                   clear                   trul26ds
078400020925     C                   EVAL      I26TLA = 'L'
078500020925     C                   EVAL      I26TPT = 'P'
078600160802     C                   EVAL      I26DTA = DATCOR
078700020925     C                   CALL      'TRUL26R'
078800020925     C                   PARM                    trul26ds
078900020925     C                   Z-ADD     skpot         OFil                           *terminal -TUTTI-
079000020925X   1C                   ELSE                                                   RICEZIONE IN FILIALE
079100020925     C                   CLEAR                   tibs56ds
079200020925     C                   EVAL      I56PPO = DUTPOU
079300020925     C                   EVAL      I56MOD = 'POA'
079400020925     C                   CALL      'TIBS56R'
079500020925     C                   PARM                    tibs56ds
079600020925do  2C     1             DO        250           I
079700020925if  3C                   IF        ski(I) > *ZEROS
079800020925     C                   CLEAR                   FNLV55DS
079900020925     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
080000020925     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
080100020925     C                   Z-ADD     ski(I)        D55LIN                         *LINEA
080200020925     C                   Z-ADD     p96dri        D55DRF                         *DATA RIFERIMENTO
080300020925     C                   CALL      'FNLV55R'
080400020925     C                   PARM                    FNLV55DS
080500020925if  4C     D55ERR        IFNE      *BLANKS                                      *ERRORE
080600020925     C                   Z-ADD     dutpou        D55TFP                         *SE STESSA
080700020925e   4C                   ENDIF
080800020925     c                   Z-ADD     d55tfp        ski(I)                         *FIL diventa TFP
080900020925e   3C                   ENDIF
081000020925e   2C                   ENDDO
081100020925     C                   Z-ADD     ski           OFil                           *terminal -AREA-
081200020925e   1C                   ENDIF
081300020925     c*
081400020925     c* Estrapola SOLO i terminal
081500010716     C                   Z-ADD     *ZEROS        J                 5 0
081600020925do  1C     1             DO        250           I
081700020925if  2C                   IF        OFil(I) > *ZEROS
081800020925     C     OFil(i)       LOOKUP    OTfp                                   55
081900020925if  3C                   IF        *IN55 = *OFF                                 *NON TROVATO
082000010716     C                   ADD       1             J
082100020925     C                   Z-ADD     OFil(i)       OTfp(J)
082200020925e   3C                   ENDIF
082300020925e   2C                   ENDIF
082400020925e   1C                   ENDDO
082500960709     C*
082600960709     C                   ENDSR
082700020919     C*--------------------------------------------------------------------------------------------*
082800020919     C* REPERISCE I DATI UTENTE
082900020919     C*--------------------------------------------------------------------------------------------*
083000020919     C     REPDATIUTE    BEGSR
083100020919     C*
083200020919     C* INIZIALIZZA VARIABILI DI WRK
083300020919     C                   CLEAR                   TIBS34DS
083400020919     C                   CLEAR                   AZUTEDS
083500020919     C                   CLEAR                   DDATIUTE
083600020919     C*
083700020919     C     *DTAARA       DEFINE    §azute        azuteds
083800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
083900020919     C                   IN(E)     *DTAARA
084000020919if  1C                   IF        %Error
084100020919     c                   EVAL      I34Tla = 'L'
084200020919     C                   CALL      'TIBS34R'
084300020919     C                   PARM                    Tibs34Ds
084400020919     C                   IN        *DTAARA
084500020919e   1C                   ENDIF
084600020919     C*
084700020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
084800020919     C                   MOVEL(P)  RSUT          DSFIRS           20
084900020919     C*
085000020919     C                   ENDSR
085100000000     C*------------------------------------------------------------------------*
085200000000     C* *INZSR - OPERAZIONI INIZIALI
085300000000     C*------------------------------------------------------------------------*
085400000000     C     *INZSR        BEGSR
085500980211     C*---
085600000000     C* RICEVIMENTO PARAMETRI
085700980211     C*---
085800000000     C     *ENTRY        PLIST
085900000000     C                   PARM                    KPJBA
086000000000     C                   MOVEL     KPJBU         PARAM
086100160802     c*
086200160802     c* Se passata la data elaboro cappario alla data passata altrimenti
086300160802     c*  alla data corrente
086400160802     c                   clear                   wdatavers
086500160802     c                   if        pardtela>*zeros
086600160802     c                   movel     pardtela      wdatavers         8 0
086700160802     c                   endif
086800980211     C*---
086900000000     C* VARIABILI RIFERITE AL DATABASE
087000980211     C*---
087100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
087200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
087300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
087400020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
087500020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
087600020925     C     *LIKE         DEFINE    TBLKEY        KTBKEY
087700980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
087800980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
087900980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
088000980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
088100980216     C     *LIKE         DEFINE    CPCLNA        DEPTFP                         *DEPOSITI
088200980216     C     *LIKE         DEFINE    CPCLNA        DEPTFA
088300980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
088400980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
088500980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
088600980216     C     *LIKE         DEFINE    DLYFCR        DEPFCR
088700980216     C     *LIKE         DEFINE    DLYRAP        DEPRAP
088800980216     C     *LIKE         DEFINE    DLYCOR        DEPCOR
088900121015     C     *LIKE         DEFINE    CPCLNA        DEPLna
089000121015     C     *LIKE         DEFINE    CPCLNA        DEPLIN
089100121015     C     *LIKE         DEFINE    CPCLNA        DEPlol
089200121015     C     *LIKE         DEFINE    CPCLNA        DEPlos
089300980220     C     *LIKE         DEFINE    DLYTRI        DEPTIC
089400980220     C     *LIKE         DEFINE    DLYTRC        DEPTCC
089500980220     C     *LIKE         DEFINE    DLYTRI        DEPTIE
089600980220     C     *LIKE         DEFINE    DLYTRC        DEPTCE
089700090921     C     *LIKE         DEFINE    DLYTRI        DEPTIh
089800090921     C     *LIKE         DEFINE    DLYTRC        DEPTCh
089900980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
090000980216     C     *LIKE         DEFINE    CPCISO        WISO
090100980216     C     *LIKE         DEFINE    CPCLNA        WLNA
090200980219     C     *LIKE         DEFINE    CPLDLV        WDLV
090300980302     C     *LIKE         DEFINE    CPCLNA        WILNA
090400980302     C     *LIKE         DEFINE    CPCLOL        WILOL
090500121012     C     *LIKE         DEFINE    CPCLOS        WILOS
090600980302     C     *LIKE         DEFINE    CPCLNA        WOLIN
090700980211     C*---
090800000000     C* CHIAVI DI LETTURA
090900980211     C*---
091000980211     C     KEYTAB        KLIST                                                  *TABEL00F
091100980211     C                   KFLD                    KTBKUT                          -UTENTE
091200980211     C                   KFLD                    KTBCOD                          -TABELLA
091300980211     C                   KFLD                    KTBKEY                          -CHIAVE
091400980211     C     KE2TAB        KLIST                                                  *TABEL00F
091500980211     C                   KFLD                    KTBKUT                          -UTENTE
091600980211     C                   KFLD                    KTBCOD                          -TEBELLA
091700020925     C     KEYTBE        KLIST                                                  *TNTBE01L
091800020925     C                   KFLD                    KBECOD                          -TABELLA
091900020925     C                   KFLD                    KBEKE1                          -CHIAVE
092000980211     C     KEYCPC        KLIST                                                  *AZCPC00F
092100980211     C                   KFLD                    KCCVER                          -VERSIONE
092200980211     C     KEYCPL        KLIST                                                  *AZCPL00F
092300980211     C                   KFLD                    KCLVER                          -VERSIONE
092400980216     C                   KFLD                    KCLNAR                          -NAZIONE
092500980216     C                   KFLD                    KCLCAP                          -CAP
092600121012     C     KEYCPE        KLIST                                                  *AZCPL00F
092700121012     C                   KFLD                    p96VER                          -VERSIONE
092800121012     C                   KFLD                    depNAR                          -NAZIONE
092900121012     C                   KFLD                    depCAP                          -CAP
093000121012     C                   KFLD                    deptfp                          -TFP
093100980211     C*---
093200000000     C* VARIABILI DI CONTROLLO
093300980211     C*---
093400980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
093500980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
093600980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
093700980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
093800050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
093900020527     C*
094000020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
094100020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
094200020527     C                   Z-ADD     N8            G08DAT
094300020527     C                   Z-ADD     *ZEROS        G08INV
094400020527     C                   MOVEL     '0'           G08ERR
094500020527     C                   CALL      'XSRDA8'
094600020527     C                   PARM                    WLBDA8
094700020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
094800150623     c* se data corrente cade in giorno festivo la incremento fino ad arrivare
094900150623     c* al primo giorno lavorativo
095000150624     c                   eval      datacur=%date(datcor:*iso)
095100150624     c                   exsr      sr_datafes
095200150624     c                   eval      datcor=%dec(datacur)
095300160802     C*
095400160802     C* Se lancio in "prova" overrido in todo la data corrente con la data di riferimento forzata
095500160802     C                   if        PARelaprova <> *blanks AND
095600160802     C                             PARdtela > *zeros
095700160802     C                   movel     PARdtela      DATCOR
095800160802     C                   endif
095900020925     C*
096000020925     C* REPERISCE I DATI UTENTE
096100020925     C                   EXSR      REPDATIUTE
096200970707     C*
096300020527     C* FILIALE --> REPERISCE L'ULTIMA VERSIONE DEL CAPPARIO
096400020527     C* SEDE    --> PEZZA: REPERISCE LA VERSIONE ALLA DATA DEL JOB -COSI' LUCIANO LA PUO' CAMBIARE-
096500020919IF  1C     DUTLPO        IFNE      'S'                                          F I L I A L E
096600030304     C***                Z-ADD     *ZEROS        P96DRI            8 0
096700030304     C                   Z-ADD     DATCOR        P96DRI            8 0
096800020527X   1C                   ELSE                                                   S E D E
096900020527     C                   Z-ADD     DATCOR        P96DRI            8 0
097000020527E   1C                   ENDIF
097100091204     c
097200091204     c* Se passata data uso questa
097300091204     c                   if        wdatavers>0
097400091204     C                   Z-ADD     wdatavers     P96DRI            8 0
097500091204     c                   endif
097600970707     C                   Z-ADD     *ZEROS        P96VER            5 0
097700970707     C                   CALL      'TISI96R'
097800970707     C                   PARM                    P96DRI
097900970707     C                   PARM                    P96VER
098000040415     C*
098100091211     C* REIPOSTO LA DATA CORRENTE (oppure la data passata al pgm ) nella data di riferimento
098200091211     C*  XCHE' IN USCITA il  *PGM TISI96R VALORIZZA IL P96DRI CON LA DATA D DECORRENZA
098300091211     C*  DELLA VERSIONE REPERITA
098400040415     C                   Z-ADD     DATCOR        P96DRI
098500091211     c                   if        wdatavers>0
098600091211     C                   Z-ADD     wdatavers     P96DRI            8 0
098700091211     c                   endif
098800960709     C*
098900000000     C                   ENDSR
099000150624      /free
099100150624       //--------------------------------------------------------------
099200150624       //?Controllo data se festiva + restituzione data prima data non festiva
099300150624       //--------------------------------------------------------------
099400150624       BEGSR  sr_DataFes;
099500150624       wfes='1';
099600150624       dow wfes='1';
099700150624       chain (0:0:%subdt(datacur:*years):
099800150624                        %subdt(datacur:*MONTHS)) azcln01l;
099900150624          if %found(azcln01l) and mat(%subdt(datacur:*days)) = 'F'
100000150624                               or pom(%subdt(datacur:*days)) = 'F';
100100150624       // data festiva: la incremento di un giorno
100200150624             datacur=datacur+%days(1);
100300150624          else;
100400150624            clear wfes;
100500150624          endif;
100600150624       enddo;
100700150624       endsr;
100800150624      /end-free
100900000000     C*------------------------------------------------------------------------*
101000000000**   CMD -COMANDI CLP-
101100050318CLRPFM FILE(WADLY00F)
101200091211CRTDUPOBJ OBJ(WADLY00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(EDPES)
101300091211CLRPFM FILE(EDPES/WADLY00F)
101400091211OVRDBF FILE(WADLY00F) TOFILE(EDPES/WADLY00F)
