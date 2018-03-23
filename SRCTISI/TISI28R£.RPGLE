000100971121     H*------------------------------------------------------------------------*
000200971121     H* AUTOMATISMO TEMPI DI TRAZIONE - ESEGUE                                 *
000300971121     H* PARTENZA - COMBINAZIONE X TUTTE LE FILIALI                             *
000400971121     H* TRAZIONE - COMBINAZIONE X TUTTI I TERMINAL -STANDARD E ECCEZIONI-      *
000500070117     h*            oppure per un solo terminal se richiesto
000600971121     H*------------------------------------------------------------------------*
000700971121     H DECEDIT('0,') DATEDIT(*DMY.)
000800971121     F*------------------------------------------------------------------------*
000900971121     F* DATA BASE
001000971121     F*------------------------------------------------------------------------*
001100971121     FAZORG01L  IF   E           K DISK
001200971121     FTABEL00F  IF   E           K DISK
001300971121     FAZCAE01L  IF   E           K DISK
001400070118     FAZCAE05L  IF   E           K DISK    rename(azcae000:azcae005)
001500971121     FAZCAS01L  UF A E           K DISK
001600971121     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700971121     D*------------------------------------------------------------------------*
001800971121     D* SCHIERE
001900971121     D*------------------------------------------------------------------------*
002000971121     D*-------------------
002100971121     D* RIGHE DI STAMPA
002200971121     D*-------------------
002300971121     D STM             S             66    DIM(5) CTDATA PERRCD(1)
002400971121     D*-------------------
002500971121     D* MESSAGGI DI ERRORE
002600971121     D*-------------------
002700971121     D MSG             S             66    DIM(5) CTDATA PERRCD(1)
002800000620     D*-------------------
002900000620     D* FILIALI POSTE
003000000620     D*-------------------
003100000620     D POS             S              3  0 DIM(500)                             *FILIALI POSTE
003200971121     D*-------------------
003300971121     D* TERMINAL
003400971121     D*-------------------
003500971121     D TFP             S              3  0 DIM(500)                             *FILIALE/TERMINAL
003600971121     D TFA             S              3  0 DIM(500)                             *TERMINAL
003700971121     D*-------------------
003800971121     D* COMBINAZIONI FILIALI/FILIALI
003900971121     D*-------------------
004000971121     D CPA             S              6  0 DIM(2000)
004100971121     D*-------------------
004200971121     D* TIPI SERVIZIO
004300971121     D*-------------------
004400971121     D TSP             S              1    DIM(10)
004500971121     D*------------------------------------------------------------------------*
004600971121     D* INPUT
004700971121     D*------------------------------------------------------------------------*
004800971121     D*-------------------
004900020919     D* DS REPERIMENTO DATI UTENTE
005000971121     D*-------------------
005100020919     D TIBS34DS      E DS                                                       *Profili utente
005200020919     D DDATIUTE      E DS                                                       *Dati utente
005300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005400971121     D*------------------
005500971121     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600971121     D*------------------
005700971121     D WLBDA8          DS
005800971121     D  G08DAT                 1      8  0
005900971121     D  G08INV                 9     16  0
006000971121     D  G08ERR                17     17
006100971121     D  G08TGI                18     22  0
006200000620     D*------------------
006300000620     D* DS LETTURA 143° DESCRIZIO ORGANIGRAMMA (X POSTE)
006400000620     D*------------------
006500000620     D OG143         E DS
006600051028      *
006700051028      * Tab. 5E (Tipi Servizio)
006800051028     d ds5E          e ds                  inz
006900051216     d
007000051216     d pardde          s              8s 0
007100061205     d partfp          s              3s 0
007200061205     d partpcpy        s              3s 0
007300070118     d partfa          s              3s 0
007400070118     d partacpy        s              3s 0
007500051028      *
007600971121     C*------------------------------------------------------------------------*
007700971121     C* MAIN LINE
007800971121     C*------------------------------------------------------------------------*
007900971121     C*
008000971121     C* STAMPA TESTATA
008100971121     C                   EXCEPT    TES
008200971121     C*
008300971121     C* CARICA TUTTI LE COMBINAZIONI DA SVILUPPARE
008400971121IF  1C     PARTEM        IFEQ      'P'                                          *TEMPO PARTENZA
008500971121     C                   EXSR      CARORG                                        -CARICA FILIALI
008600971121X   1C                   ELSE                                                   *TEMPO TRAZIONE
008700971121     C                   EXSR      CARCAE                                        -CARICA TERMINAL
008800971121E   1C                   ENDIF
008900971121     C*
009000971121     C* AGGIORNA IL FILE E STAMPA L'ESITO DELL'ELABORAZIONE
009100971121     C                   EXSR      AGGSTM
009200971121     C*
009300971121     C* STAMPA RIGA DI FINE STAMPA
009400971121     C                   EXCEPT    FIN
009500971121     C*
009600971121     C                   SETON                                        LR
009700971121     C*------------------------------------------------------------------------*
009800971121     C* CARORG - CARICA LE FILIALI PER LO SVILUPPO DEI TEMPI PARTENZA
009900971121     C*------------------------------------------------------------------------*
010000971121     C     CARORG        BEGSR
010100971121     C*
010200971121     C* AZZERA LE VARIABILI DI LAVORO
010300971121     C                   Z-ADD     *ZEROS        I                 3 0
010400971121     C*
010500971121     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
010600971121     C     *LOVAL        SETLL     AZORG01L
010700971121     C                   READ      AZORG01L                               99
010800971121DO  1C     *IN99         DOWEQ     *OFF
010900971121IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDO
011000971121     C     ORGFAG        ANDNE     'V'                                          *FILIALI/AGENZIE
011100971121     C                   ADD       1             I
011200971121     C                   Z-ADD     ORGFIL        TFP(I)
011300971121E   2C                   ENDIF
011400971121     C                   READ      AZORG01L                               99
011500971121E   1C                   ENDDO
011600971121     C                   Z-ADD     I             WUTFP                          *ULTIMO TFP
011700971121     C*
011800971121     C* SVILUPPA LE "COMBINAZIONI" PARTENZA
011900971121     C                   EXSR      SVILUP
012000971121     C*
012100971121     C                   ENDSR
012200971121     C*------------------------------------------------------------------------*
012300971121     C* SVILUP - SVILUPPA LE "COMBINAZIONI" PARTENZA
012400971121     C*------------------------------------------------------------------------*
012500971121     C     SVILUP        BEGSR
012600971121     C*
012700971121     C* AZZERA LE VARIBILI DI LAVORO
012800971121     C                   Z-ADD     *ZEROS        NC                6 0
012900971121     C*
013000971121     C* CICLO FINO A FINE TERMINAL PARTENZA
013100971121DO  1C     1             DO        WUTFP         I
013200971121IF  2C     TFP(I)        IFGT      *ZEROS                                       *SE MEMORIZZATO
013300971121     C*
013400971121     C* MEMORIZZA LA COMBINAZIONE
013500971121     C                   Z-ADD     *ZEROS        WN6               6 0
013600971121     C                   MOVEL     TFP(I)        WN6               6 0
013700971121     C                   SETOFF                                           28
013800971121     C     WN6           LOOKUP    CPA                                    28
013900971121IF  3C     *IN28         IFEQ      *OFF                                         *SE MAI MEMORIZZATA
014000971121     C                   ADD       1             NC
014100971121     C                   Z-ADD     *ZEROS        CPA(NC)
014200971121     C                   MOVEL     TFP(I)        CPA(NC)
014300971121E   3C                   ENDIF
014400971121E   2C                   ENDIF
014500971121E   1C                   ENDDO                                                  *FINE TERM PARTENZA
014600971121     C*
014700971121     C                   Z-ADD     NC            WUCOM                          *ULTIMA COMBINAZIONE
014800971121     C*
014900971121     C                   ENDSR
015000971121     C*------------------------------------------------------------------------*
015100971121     C* CARCAE - CARICA I TERMINAL PER LO SVILUPPO DEI TEMPI TRAZIONE
015200971121     C*------------------------------------------------------------------------*
015300971121     C     CARCAE        BEGSR
015400971121     C*
015500971121     C* AZZERA LE VARIABILI DI LAVORO
015600971121     C                   Z-ADD     *ZEROS        J                 3 0
015700971121     C                   Z-ADD     *ZEROS        I                 3 0
015800971121     C*---
015900971121     C* CARICA I TERMINAL STANDARD
016000971121     C*---
016100971121     C* ARRIVO
016200070117     C* PARTENZA se immesso il terminal, carico solo lui
016300070117     c                   if        partfa>0
016400070117     c                   eval      J=1
016500070117     c                   eval      tfa(1)=partfa
016600070117     c                   else
016700070118     c
016800971121     C                   MOVEL     'A'           KCEEPA                         *ARRIVO
016900070118     C     KCEEPA        SETLL     AZCAE05L
017000070118     C     KCEEPA        READE     AZCAE05L                               99
017100971121DO  1C     *IN99         DOWEQ     *OFF
017200971121IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
017300971121IF  3C     CAETFP        IFEQ      *ZEROS                                       *STANDARD
017400051216IF  4C     pardde        IFGE      CAEDDE                                       *IN DECORRENZA
017500051216     C     pardde        ANDLE     CAEDSC
017600070118     c                   if        J=0 OR  caetfe<>tfa(J)
017700971121     C                   ADD       1             J
017800971121     C                   Z-ADD     CAETFE        TFA(J)
017900971121E   4C                   ENDIF
018000070118E   4C                   ENDIF
018100971121E   3C                   ENDIF
018200971121E   2C                   ENDIF
018300070118     C     KCEEPA        READE     AZCAE05L                               99
018400971121E   1C                   ENDDO
018500070117     c                   endif
018600070117     c
018700070117     C                   Z-ADD     J             WUTFA                          *ULTIMO TFA
018800070117     c
018900061205     C* PARTENZA se immesso il terminal, carico solo lui
019000061205     c                   if        partfp>0
019100061205     c                   eval      I=1
019200061205     c                   eval      tfp(1)=partfp
019300061205     c                   else
019400061205     c
019500971121     C                   MOVEL     'P'           KCEEPA                         *ARRIVO
019600070118     C     KCEEPA        SETLL     AZCAE05L
019700070118     C     KCEEPA        READE     AZCAE05L                               99
019800971121DO  1C     *IN99         DOWEQ     *OFF
019900971121IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
020000971121IF  3C     CAETFA        IFEQ      *ZEROS                                       *STANDARD
020100051216IF  4C     pardde        IFGE      CAEDDE                                       *IN DECORRENZA
020200051216     C     pardde        ANDLE     CAEDSC
020300070118     c                   if        i=0 OR  caetfe<>tfp(I)
020400971121     C                   ADD       1             I
020500971121     C                   Z-ADD     CAETFE        TFP(I)
020600971121E   4C                   ENDIF
020700070118E   4C                   ENDIF
020800971121E   3C                   ENDIF
020900971121E   2C                   ENDIF
021000070118     C     KCEEPA        READE     AZCAE05L                               99
021100971121E   1C                   ENDDO
021200000620     C*
021300000620     C* AGGIUNGE AI TERMINAL PARTENZA LE FILIALI POSTE (ANCHE SE NON SONO TERMINAL)
021400000620DO  1C                   DO        WUPOS         K
021500000620     C                   SETOFF                                           28
021600000620     C     POS(K)        LOOKUP    TFP                                    28
021700000620IF  2C     *IN28         IFEQ      *OFF                                         *NON INSERITA
021800000620     C                   ADD       1             I
021900000620     C                   Z-ADD     POS(K)        TFP(I)                         *FILIALE POSTA=TFP
022000000620E   2C                   ENDIF
022100000620E   1C                   ENDDO
022200061205     c                   endif
022300061205     c
022400000620     C                   Z-ADD     I             WUTFP                          *ULTIMO TFP
022500971121     C*
022600971121     C* SVILUPPA LE COMBINAZIONI TERMINAL/TERMINAL
022700971121     C                   EXSR      SVILUT
022800971121     C*
022900971121     C* CARICA ULTERIORI COMBINAZIONI TERMINAL/TERMINAL DA TERMINAL ECCEZIONI
023000070118     c* non lo faccio se ho richiesto un solo terminal di arrivo
023100070118     c* non serve in quanto per quel temrinal ho giàù creato tutte le
023200070118     c*  combinazioni con i temrinal di partenza
023300070118     c                   if        partfa=0
023400971121     C                   EXSR      CARECC
023500070118     c                   endif
023600971121     C*
023700971121     C                   ENDSR
023800971121     C*------------------------------------------------------------------------*
023900971121     C* SVILUT - SVILUPPA LE COMBINAZIONI TERMINAL/TERMINAL
024000971121     C*------------------------------------------------------------------------*
024100971121     C     SVILUT        BEGSR
024200971121     C*
024300971121     C* CICLO FINO A FINE TERMINAL PARTENZA
024400971121     C                   Z-ADD     *ZEROS        NC
024500971121DO  1C     1             DO        WUTFP         I
024600971121IF  2C     TFP(I)        IFGT      *ZEROS                                       *SE MEMORIZZATO
024700971121     C*
024800971121     C* CICLO FINO A FINE TERMINAL ARRIVO
024900971121DO  3C     1             DO        WUTFA         J
025000971121IF  4C     TFA(J)        IFGT      *ZEROS                                       *SE MEMORIZZATO
025100971121     C*
025200971121     C* MEMORIZZA LA COMBINAZIONE
025300971121     C                   MOVEL     TFP(I)        WN6               6 0
025400971121     C                   MOVE      TFA(J)        WN6               6 0
025500971121     C                   SETOFF                                           28
025600971121     C     WN6           LOOKUP    CPA                                    28
025700971121IF  5C     *IN28         IFEQ      *OFF                                         *SE MAI MEMORIZZATA
025800971121     C                   ADD       1             NC
025900971121     C                   MOVEL     TFP(I)        CPA(NC)
026000971121     C                   MOVE      TFA(J)        CPA(NC)
026100971121E   5C                   ENDIF
026200971121E   4C                   ENDIF
026300971121E   3C                   ENDDO                                                  *FINE TERM ARRIVO
026400971121E   2C                   ENDIF
026500971121E   1C                   ENDDO                                                  *FINE TERM PARTENZA
026600971121     C*
026700971121     C                   Z-ADD     NC            WUCOM                          *ULTIMA COMBINAZIONE
026800971121     C*
026900971121     C                   ENDSR
027000971121     C*------------------------------------------------------------------------*
027100971121     C* CARECC - CARICA COMBINAZIONI TERMINAL/TERMINAL DA TERMINAL ECCEZIONI
027200971121     C*------------------------------------------------------------------------*
027300971121     C     CARECC        BEGSR
027400971121     C*
027500971121     C                   Z-ADD     WUCOM         NC                             *ULTIMA COMBINAZIONE
027600971121     C*---
027700971121     C* CARICA I TERMINAL ECCEZIONE
027800971121     C*---
027900971121     C* ARRIVO
028000971121     C                   MOVEL     'A'           KCEEPA
028100971121     C     KCEEPA        SETLL     AZCAE01L
028200971121     C     KCEEPA        READE     AZCAE01L                               99
028300971121DO  1C     *IN99         DOWEQ     *OFF
028400971121IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
028500051216IF  4C     pardde        IFGE      CAEDDE                                       *IN DECORRENZA
028600051216     C     pardde        ANDLE     CAEDSC
028700061205     c
028800061205IF  3C     CAETFP        IFGT      *ZEROS                                       *ECCEZIONE
028900061205     c     partfp        andeq     0
029000061205     c     caetfp        oreq      partfp
029100061205     c     partfp        andgt     0
029200061205     c
029300971121     C                   MOVEL     CAETFP        WN6               6 0
029400971121     C                   MOVE      CAETFE        WN6               6 0
029500971121     C                   SETOFF                                           28
029600971121     C     WN6           LOOKUP    CPA                                    28
029700971121IF  5C     *IN28         IFEQ      *OFF                                         *SE MAI MEMORIZZATA
029800971121     C                   ADD       1             NC
029900971121     C                   MOVEL     CAETFP        CPA(NC)
030000971121     C                   MOVE      CAETFE        CPA(NC)
030100971121E   5C                   ENDIF
030200971121E   4C                   ENDIF
030300971121E   3C                   ENDIF
030400971121E   2C                   ENDIF
030500971121     C     KCEEPA        READE     AZCAE01L                               99
030600971121E   1C                   ENDDO                                                  *FINE ECCEZIONI
030700971121     C*---
030800971121     C* PARTENZA -> NON ESISTONO
030900971121     C*---
031000971121     C                   Z-ADD     NC            WUCOM                          *ULTIMA COMBINAZIONE
031100971121     C*
031200971121     C                   ENDSR
031300971121     C*------------------------------------------------------------------------*
031400971121     C* AGGSTM - AGGIORNA IL FILE E STAMPA L'ESITO DELL'ELABORAZIONE
031500971121     C*------------------------------------------------------------------------*
031600971121     C     AGGSTM        BEGSR
031700971121     C*
031800971121     C* CICLO PER TUTTE LE COMBINAZIONI MEMORIZZATE
031900971121DO  1C     1             DO        WUCOM         NC                6 0
032000971121IF  2C     CPA(NC)       IFGT      *ZEROS                                       *SE MEMORIZZATA
032100971121     C*
032200971121     C* CICLO PER CIASCUN TIPO SERVIZIO ESISTENTE
032300971121DO  3C     1             DO        WUTSP         TS                6 0
032400971121     C*
032500971121     C* SCRIVE IL TEMPO
032600971121IF  4C     PARTEM        IFEQ      'P'                                          *TEMPO PARTENZA
032700971121     C                   MOVEL     'P'           KCSPTA
032800971121X   4C                   ELSE                                                   *TEMPO TRAZIONE
032900971121     C                   MOVEL     'T'           KCSPTA
033000971121E   4C                   ENDIF
033100971121     C                   MOVEL     CPA(NC)       KCSLNP                         *LINEA PARTENZA
033200971121     C                   MOVE      CPA(NC)       KCSLNA                         *LINEA ARRIVO
033300971121     C                   MOVEL     TSP(TS)       KCSTSP                         *TIPO SERVIZIO
033400971121     C                   MOVEL     'N'           $ESI                           *ESISTENZA TEMPO
033500971121     C     KEYCAS        SETLL     AZCAS01L
033600971121     C     KEYCAS        READE(N)  AZCAS01L                               99
033700971121DO  4C     *IN99         DOWEQ     *OFF
033800971121     C     $ESI          ANDEQ     'N'
033900971121IF  5C     CASATB        IFEQ      *BLANKS                                      *VALIDO
034000051216IF  6C     CASDDE        IFLE      pardde                                       *IN DECORRENZA
034100971121     C                   MOVEL     'S'           $ESI                           *ESISTENZA TEMPO
034200971121E   6C                   ENDIF
034300971121E   5C                   ENDIF
034400971121     C     KEYCAS        READE(N)  AZCAS01L                               99
034500971121E   4C                   ENDDO                                                  *FINE TEMPI
034600971121     C*
034700971127     C* RECORD INESISTENTE, CREA TEMPO E STAMPA
034800971121IF  4C     $ESI          IFEQ      'N'                                          *ESISTE
034900971121     C                   EXSR      CRECAS                                       *CREAZIONE TEMPO
035000971121E   4C                   ENDIF
035100971121E   3C                   ENDDO                                                  *FINE SERVIZI
035200971121E   2C                   ENDIF
035300971121E   1C                   ENDDO                                                  *FINE COMBINAZIONI
035400971121     C*
035500971121     C                   ENDSR
035600971121     C*------------------------------------------------------------------------*
035700971121     C* CRECAS - CREA IL TEMPO
035800971121     C*------------------------------------------------------------------------*
035900971121     C     CRECAS        BEGSR
036000061205     c                   clear                   wtrova            1
036100061205     c* Se c'e' la copia da terminal, chaino il record per duplicarlo
036200061205     c*  altrimenti come ora
036300070118     c* PARTENZA
036400061205    0c                   if        partpcpy>0
036500061205     C                   MOVEL     partpcpy      KCSLNP                         *LINEA PARTENZA
036600061205     C     KEYCAS        SETLL     AZCAS01L
036700061205     C     KEYCAS        READE(N)  AZCAS01L                               99
036800061205DO  4C     *IN99         DOWEQ     *OFF
036900061205     C     $ESI          ANDEQ     'N'
037000061205IF  5C     CASATB        IFEQ      *BLANKS                                      *VALIDO
037100061205IF  6C     CASDDE        IFLE      pardde                                       *IN DECORRENZA
037200061205     C                   MOVEL     'S'           wtrova                         *ESISTENZA TEMPO
037300061205E   6C                   ENDIF
037400061205E   5C                   ENDIF
037500061205     C     KEYCAS        READE(N)  AZCAS01L                               99
037600061205E   4C                   ENDDO                                                  *FINE TEMPI
037700061205     c                   endif
037800070118     c* ARRIVO
037900070118    0c                   if        partacpy>0
038000070118     C                   MOVEL     partacpy      KCSLNa                         *LINEA PARTENZA
038100070118     C     KEYCAS        SETLL     AZCAS01L
038200070118     C     KEYCAS        READE(N)  AZCAS01L                               99
038300070118DO  4C     *IN99         DOWEQ     *OFF
038400070118     C     $ESI          ANDEQ     'N'
038500070118IF  5C     CASATB        IFEQ      *BLANKS                                      *VALIDO
038600070118IF  6C     CASDDE        IFLE      pardde                                       *IN DECORRENZA
038700070118     C                   MOVEL     'S'           wtrova                         *ESISTENZA TEMPO
038800070118E   6C                   ENDIF
038900070118E   5C                   ENDIF
039000070118     C     KEYCAS        READE(N)  AZCAS01L                               99
039100070118E   4C                   ENDDO                                                  *FINE TEMPI
039200070118     c                   endif
039300971121     C*
039400061205    0c                   if        wtrova=' '
039500971121     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
039600971121IF  1C     PARTEM        IFEQ      'P'                                          *TEMPO PARTENZA
039700971121     C                   MOVEL     'P'           CASPTA
039800971121     C                   Z-ADD     *ZEROS        CASTIN                         *TEMPO INTERNO
039900971121     C                   Z-ADD     *ZEROS        CASTCN                         *TEMPO CLIENTE
040000971230IF  2C     TSP(TS)       IFEQ      'D'                                          *DISTRIBUZIONE
040100971230     C                   Z-ADD     24            CASTIN                         *TEMPO INTERNO
040200971230     C                   Z-ADD     24            CASTCN                         *TEMPO CLIENTE
040300971230E   2C                   ENDIF
040400971121X   1C                   ELSE                                                   *TEMPO TRAZIONE
040500971121     C                   MOVEL     'T'           CASPTA
040600971121     C                   Z-ADD     24            CASTIN                         *TEMPO INTERNO
040700971121     C                   Z-ADD     24            CASTCN                         *TEMPO CLIENTE
040800051028      * (Tipo servizio Poste - "P" - non più caricato in TSP...)
040900000620IF  2C     TSP(TS)       IFEQ      'P'                                          *POSTE
041000000620     C                   Z-ADD     *ZEROS        CASTIN                         *TEMPO INTERNO
041100000620     C                   Z-ADD     *ZEROS        CASTCN                         *TEMPO CLIENTE
041200000620E   2C                   ENDIF
041300000620E   1C                   ENDIF
041400061205    0c                   endif
041500971121     C                   MOVEL     CPA(NC)       CASLNP                         *LINEA PARTENZA
041600971121     C                   MOVE      CPA(NC)       CASLNA                         *LINEA ARRIVO
041700971121     C                   MOVEL     TSP(TS)       CASTSP                         *TIPO SERVIZIO
041800061205     c
041900051216     C                   Z-ADD     pardde        CASDDE                         *DECORRENZA
042000971121     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
042100971121     C                   Z-ADD     *ZEROS        CASDTR                         *DATA TRASMISSIONE
042200971121     C                   MOVEL     *BLANKS       CASATB                         *FLAG ANNULLAMENTO
042300971121     C* SCRIVE
042400971121     C                   MOVEL     *BLANKS       STMMSG
042500971121IF  1C     PARSIM        IFEQ      'N'                                          AG G I O R N A
042600971121     C                   WRITE     AZCAS000                             98
042700971121IF  2C     *IN98         IFEQ      *OFF                                         *OK IN SCRITTURA
042800971121     C                   MOVEL(P)  MSG(1)        STMMSG
042900971121X   2C                   ELSE                                                   *ERRORE IN SCRITTURA
043000971121     C                   MOVEL(P)  MSG(2)        STMMSG
043100971121E   2C                   ENDIF
043200971121E   1C                   ENDIF
043300000620     C*
043400000620     C* IMPOSTA ALCUNI DATI PER LA STAMPA
043500000620     C                   Z-ADD     CASDDE        G08INV
043600000620     C                   Z-ADD     *ZEROS        G08DAT
043700000620     C                   MOVEL     '3'           G08ERR
043800000620     C                   CALL      'XSRDA8'
043900000620     C                   PARM                    WLBDA8
044000000620     C                   Z-ADD     G08DAT        DDECAS            8 0          *DATA DECORRENZA
044100971121     C* STAMPA ESITO
044200971121     C                   EXSR      STADET
044300971121     C*
044400971121     C                   ENDSR
044500971121     C*------------------------------------------------------------------------*
044600971121     C* STADET - STAMPA RIGA DI DETTAGLIO
044700971121     C*------------------------------------------------------------------------*
044800971121     C     STADET        BEGSR
044900971121     C*
045000971121     C* FINE PAGINA, STAMPA TESTATA
045100971121IF  1C     *INOF         IFEQ      *ON
045200971121     C                   EXCEPT    TES
045300971121     C                   SETOFF                                       OF
045400971121E   1C                   ENDIF
045500971121     C*
045600971121     C* STAMPA RIGA DI DETTAGLIO
045700971121     C                   EXCEPT    DET
045800971121     C*
045900971121     C                   ENDSR
046000971121     C*------------------------------------------------------------------------*
046100971121     C* CARTAB - CARICA LE TABELLE
046200971121     C*------------------------------------------------------------------------*
046300971121     C     CARTAB        BEGSR
046400971121     C*--------------------
046500971121     C* TIPI SERVIZIO
046600971121     C*--------------------
046700971121     C                   Z-ADD     *ZEROS        I
046800020919     C                   MOVEL     1             KTBKUT
046900971121     C                   MOVEL     '5E'          KTBCOD
047000971121     C     KEYTAB        SETLL     TABEL00F
047100971121     C     KEYTAB        READE     TABEL00F                               99
047200971121DO  1C     *IN99         DOWEQ     *OFF
047300971121IF  2C     TBLFLG        IFEQ      *BLANKS
047400051028      * non carico né annullati né "servizi Poste"
047500051028     c                   movel     TBLuni        ds5E
047600051028     c                   if        §5Euso <> 'P'
047700971121     C                   ADD       1             I
047800971121     C                   MOVEL     TBLKEY        TSP(I)
047900051028     c                   endif
048000971121E   2C                   ENDIF
048100971121     C     KEYTAB        READE     TABEL00F                               99
048200971121E   1C                   ENDDO
048300971121     C                   Z-ADD     I             WUTSP                          *ULTIMO SERVIZIO
048400000620     C*--------------------
048500000620     C* FILIALI POSTE
048600000620     C*--------------------
048700000620     C                   Z-ADD     *ZEROS        K                 3 0
048800000620     C     *LOVAL        SETLL     AZORG01L
048900000620     C                   READ      AZORG01L                               99
049000000620DO  1C     *IN99         DOWEQ     *OFF
049100000620     C                   MOVEL     ORGDE3        OG143
049200000620IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDO
049300000620     C     ORGFAG        ANDNE     'V'                                          *FILIALI/AGENZIE
049400020627     C     §OGNTW        ANDEQ     'PPT'                                        *FILIALI POSTE
049500000620     C                   ADD       1             K
049600000620     C                   Z-ADD     ORGFIL        POS(K)
049700000620E   2C                   ENDIF
049800000620     C                   READ      AZORG01L                               99
049900000620E   1C                   ENDDO
050000000620     C                   Z-ADD     K             WUPOS                          *ULTIMO SERVIZIO
050100971121     C*
050200971121     C                   ENDSR
050300020919     C*--------------------------------------------------------------------------------------------*
050400020919     C* REPERISCE I DATI UTENTE
050500020919     C*--------------------------------------------------------------------------------------------*
050600020919     C     REPDATIUTE    BEGSR
050700020919     C*
050800020919     C* INIZIALIZZA VARIABILI DI WRK
050900020919     C                   CLEAR                   TIBS34DS
051000020919     C                   CLEAR                   AZUTEDS
051100020919     C                   CLEAR                   DDATIUTE
051200020919     C*
051300020919     C     *DTAARA       DEFINE    §azute        azuteds
051400020919     C     *DTAARA       DEFINE    §datiute      ddatiute
051500020919     C                   IN(E)     *DTAARA
051600020919if  1C                   IF        %Error
051700020919     c                   EVAL      I34Tla = 'L'
051800020919     C                   CALL      'TIBS34R'
051900020919     C                   PARM                    Tibs34Ds
052000020919     C                   IN        *DTAARA
052100020919e   1C                   ENDIF
052200020919     C*
052300020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
052400020919     C                   MOVEL(P)  RSUT          DSFIRS           20
052500020919     C*
052600020919     C                   ENDSR
052700971121     C*------------------------------------------------------------------------*
052800971121     C* *INZSR - OPERAZIONI INIZIALI
052900971121     C*------------------------------------------------------------------------*
053000971121     C     *INZSR        BEGSR
053100971121     C*--------------------
053200971121     C* RICEVIMENTO PARAMETRI
053300971121     C*--------------------
053400971121     C     *ENTRY        PLIST
053500971121     C                   PARM                    PARTEM            1
053600971121     C                   PARM                    PARSIM            1
053700051216     C                   PARM                    PARdde
053800061205     C                   PARM                    PARtfp
053900061205     C                   PARM                    PARtpcpy
054000070117     C                   PARM                    PARtfa
054100070117     C                   PARM                    PARtacpy
054200971121     C*--------------------
054300971121     C* VARIABILI RIFERITE AL DATABASE
054400971121     C*--------------------
054500971121     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
054600971121     C     *LIKE         DEFINE    TBLCOD        KTBCOD
054700971121     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
054800971121     C     *LIKE         DEFINE    CASPTA        KCSPTA                         *AZCAS00F
054900971121     C     *LIKE         DEFINE    CASLNP        KCSLNP
055000971121     C     *LIKE         DEFINE    CASLNA        KCSLNA
055100971121     C     *LIKE         DEFINE    CASTSP        KCSTSP
055200971121     C*--------------------
055300971121     C* CHIAVI DI LETTURA
055400971121     C*--------------------
055500971121     C* LETTURA TABEL00F - PARZIALE
055600971121     C     KEYTAB        KLIST
055700971121     C                   KFLD                    KTBKUT                         *UTENTE
055800971121     C                   KFLD                    KTBCOD                         *TABELLA
055900971121     C* LETTURA AZCAS01L - PARZIALE
056000971121     C     KEYCAS        KLIST
056100971121     C                   KFLD                    KCSPTA                         *TIPO TEMPO
056200971121     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
056300971121     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
056400971121     C                   KFLD                    KCSTSP                         *TIPO SERVIZIO
056500971121     C*--------------------
056600971121     C* VARIABILI DI CONTROLLO
056700971121     C*--------------------
056800971121     C                   MOVEL     'N'           $ESI              1            *ESISTENZA TEMPO
056900971121     C                   Z-ADD     *ZEROS        WUTFP             6 0          *ULTIMO TERM PART
057000971121     C                   Z-ADD     *ZEROS        WUTFA             6 0          *ULTIMO TERM ARR
057100971121     C                   Z-ADD     *ZEROS        WUCOM             6 0          *ULTIMA COMBINAZIONE
057200971121     C                   Z-ADD     *ZEROS        WUTSP             6 0          *ULTIMO SERVIZIO
057300000620     C                   Z-ADD     *ZEROS        WUPOS             6 0          *ULTIME FILIALI POST
057400971121     C                   MOVEL     *BLANKS       STMMSG           66            *MESSAGGIO IN STAMPA
057500971121     C*
057600971121     C* REPERIMENTO DATI UTENTE
057700020919     C                   EXSR      REPDATIUTE
057800971121     C*
057900971121     C* CARICA LE TABELLE
058000971121     C                   EXSR      CARTAB
058100971121     C*
058200971121     C* CALCOLA LA DATA CORRENTE
058300051216     C*                  TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
058400051216     C*                  MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
058500051216     C*                  Z-ADD     WNUM8         G08DAT
058600051216     C*                  Z-ADD     *ZEROS        G08INV
058700051216     C*                  MOVEL     '0'           G08ERR
058800051216     C*                  CALL      'XSRDA8'
058900051216     C*                  PARM                    WLBDA8
059000051216     C*                  Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
059100971121     C*
059200971121     C                   ENDSR
059300971121     O*------------------------------------------------------------------------*
059400971121     OQSYSPRT   E            TES               2
059500971121     O                       DSFIRS              20
059600971121     O                                           30 'TISI28R '
059700971121     O                                           62 '*** AUTOMATISMI TERMPI T'
059800971121     O                                           73 'RAZIONE ***'
059900971121     O                       UDATE               95 '  /  /  '
060000971121     O                                          128 'PAG.'
060100971121     O                       PAGE          Z    132
060200000620     O*------------------------
060300971121     O          E            TES               4
060400971121     O                       STM(1)              66
060500971121     O                       STM(1)             132
060600000620     O*------------------------
060700971121     O          E            TES               5
060800971121     O                       STM(2)              66
060900000620     O                       STM(4)             132
061000000620     O*------------------------
061100971121     O          E            TES               6
061200971121     O                       STM(1)              66
061300971121     O                       STM(1)             132
061400971121     O*------------------------------------
061500971121     O          E            DET         1
061600971121     O                       CASPTA               3
061700971121     O                       CASLNP              12
061800971121     O                       CASLNA              22
061900971121     O                       CASTSP              29
062000000620     O                       DDECAS              45
062100000620     O                       CASTIN              51
062200000620     O                       CASTCN              58
062300000620     O                       STMMSG             132
062400971121     O*------------------------------------
062500971121     O          E            FIN         1
062600971121     O                                           76 '*** FINE STAMPA ***'
062700971121     O*------------------------------------------------------------------------*
062800971121** STM - RIGHE
062900971121------------------------------------------------------------------  1
063000000620TEMPO  PARTENZA  ARRIVO  SERVIZIO  DECORRENZA  T.INT  T.CLI         2
063100000620  X      999       999      X      99/99/9999   999    999          3
063200000620ESITO                                                               4
063300000620XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  5
063400971121** MSG - MESSAGGI
063500971121RECORD INSERITO                                                     1
063600971121ERRORE IN SCRITTURA                                                 2
0637009711213
0638009711214
0639009711215
