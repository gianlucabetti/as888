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
002800971121     D*-------------------
002900971121     D* TERMINAL
003000971121     D*-------------------
003100971121     D TFP             S              3  0 DIM(500)                             *FILIALE/TERMINAL
003200971121     D TFA             S              3  0 DIM(500)                             *TERMINAL
003300971121     D*-------------------
003400971121     D* COMBINAZIONI FILIALI/FILIALI
003500971121     D*-------------------
003600971121     D CPA             S              6  0 DIM(2000)
003700971121     D*-------------------
003800971121     D* TIPI SERVIZIO
003900971121     D*-------------------
004000971121     D TSP             S              1    DIM(10)
004100971121     D*------------------------------------------------------------------------*
004200971121     D* INPUT
004300971121     D*------------------------------------------------------------------------*
004400971121     D*-------------------
004500020919     D* DS REPERIMENTO DATI UTENTE
004600971121     D*-------------------
004700020919     D TIBS34DS      E DS                                                       *Profili utente
004800020919     D DDATIUTE      E DS                                                       *Dati utente
004900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005000971121     D*------------------
005100971121     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200971121     D*------------------
005300971121     D WLBDA8          DS
005400971121     D  G08DAT                 1      8  0
005500971121     D  G08INV                 9     16  0
005600971121     D  G08ERR                17     17
005700971121     D  G08TGI                18     22  0
005800051028      *
005900051028      * Tab. 5E (Tipi Servizio)
006000051028     d ds5E          e ds                  inz
006100051216     d
006200051216     d pardde          s              8s 0
006300061205     d partfp          s              3s 0
006400061205     d partpcpy        s              3s 0
006500070118     d partfa          s              3s 0
006600070118     d partacpy        s              3s 0
006700051028      *
006800971121     C*------------------------------------------------------------------------*
006900971121     C* MAIN LINE
007000971121     C*------------------------------------------------------------------------*
007100971121     C*
007200971121     C* STAMPA TESTATA
007300971121     C                   EXCEPT    TES
007400971121     C*
007500971121     C* CARICA TUTTI LE COMBINAZIONI DA SVILUPPARE
007600971121IF  1C     PARTEM        IFEQ      'P'                                          *TEMPO PARTENZA
007700971121     C                   EXSR      CARORG                                        -CARICA FILIALI
007800971121X   1C                   ELSE                                                   *TEMPO TRAZIONE
007900971121     C                   EXSR      CARCAE                                        -CARICA TERMINAL
008000971121E   1C                   ENDIF
008100971121     C*
008200971121     C* AGGIORNA IL FILE E STAMPA L'ESITO DELL'ELABORAZIONE
008300971121     C                   EXSR      AGGSTM
008400971121     C*
008500971121     C* STAMPA RIGA DI FINE STAMPA
008600971121     C                   EXCEPT    FIN
008700971121     C*
008800971121     C                   SETON                                        LR
008900971121     C*------------------------------------------------------------------------*
009000971121     C* CARORG - CARICA LE FILIALI PER LO SVILUPPO DEI TEMPI PARTENZA
009100971121     C*------------------------------------------------------------------------*
009200971121     C     CARORG        BEGSR
009300971121     C*
009400971121     C* AZZERA LE VARIABILI DI LAVORO
009500971121     C                   Z-ADD     *ZEROS        I                 3 0
009600971121     C*
009700971121     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
009800971121     C     *LOVAL        SETLL     AZORG01L
009900971121     C                   READ      AZORG01L                               99
010000971121DO  1C     *IN99         DOWEQ     *OFF
010100971121IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDO
010200971121     C     ORGFAG        ANDNE     'V'                                          *FILIALI/AGENZIE
010300971121     C                   ADD       1             I
010400971121     C                   Z-ADD     ORGFIL        TFP(I)
010500971121E   2C                   ENDIF
010600971121     C                   READ      AZORG01L                               99
010700971121E   1C                   ENDDO
010800971121     C                   Z-ADD     I             WUTFP                          *ULTIMO TFP
010900971121     C*
011000971121     C* SVILUPPA LE "COMBINAZIONI" PARTENZA
011100971121     C                   EXSR      SVILUP
011200971121     C*
011300971121     C                   ENDSR
011400971121     C*------------------------------------------------------------------------*
011500971121     C* SVILUP - SVILUPPA LE "COMBINAZIONI" PARTENZA
011600971121     C*------------------------------------------------------------------------*
011700971121     C     SVILUP        BEGSR
011800971121     C*
011900971121     C* AZZERA LE VARIBILI DI LAVORO
012000971121     C                   Z-ADD     *ZEROS        NC                6 0
012100971121     C*
012200971121     C* CICLO FINO A FINE TERMINAL PARTENZA
012300971121DO  1C     1             DO        WUTFP         I
012400971121IF  2C     TFP(I)        IFGT      *ZEROS                                       *SE MEMORIZZATO
012500971121     C*
012600971121     C* MEMORIZZA LA COMBINAZIONE
012700971121     C                   Z-ADD     *ZEROS        WN6               6 0
012800971121     C                   MOVEL     TFP(I)        WN6               6 0
012900971121     C                   SETOFF                                           28
013000971121     C     WN6           LOOKUP    CPA                                    28
013100971121IF  3C     *IN28         IFEQ      *OFF                                         *SE MAI MEMORIZZATA
013200971121     C                   ADD       1             NC
013300971121     C                   Z-ADD     *ZEROS        CPA(NC)
013400971121     C                   MOVEL     TFP(I)        CPA(NC)
013500971121E   3C                   ENDIF
013600971121E   2C                   ENDIF
013700971121E   1C                   ENDDO                                                  *FINE TERM PARTENZA
013800971121     C*
013900971121     C                   Z-ADD     NC            WUCOM                          *ULTIMA COMBINAZIONE
014000971121     C*
014100971121     C                   ENDSR
014200971121     C*------------------------------------------------------------------------*
014300971121     C* CARCAE - CARICA I TERMINAL PER LO SVILUPPO DEI TEMPI TRAZIONE
014400971121     C*------------------------------------------------------------------------*
014500971121     C     CARCAE        BEGSR
014600971121     C*
014700971121     C* AZZERA LE VARIABILI DI LAVORO
014800971121     C                   Z-ADD     *ZEROS        J                 3 0
014900971121     C                   Z-ADD     *ZEROS        I                 3 0
015000971121     C*---
015100971121     C* CARICA I TERMINAL STANDARD
015200971121     C*---
015300971121     C* ARRIVO
015400070117     C* PARTENZA se immesso il terminal, carico solo lui
015500070117     c                   if        partfa>0
015600070117     c                   eval      J=1
015700070117     c                   eval      tfa(1)=partfa
015800070117     c                   else
015900070118     c
016000971121     C                   MOVEL     'A'           KCEEPA                         *ARRIVO
016100070118     C     KCEEPA        SETLL     AZCAE05L
016200070118     C     KCEEPA        READE     AZCAE05L                               99
016300971121DO  1C     *IN99         DOWEQ     *OFF
016400971121IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
016500971121IF  3C     CAETFP        IFEQ      *ZEROS                                       *STANDARD
016600051216IF  4C     pardde        IFGE      CAEDDE                                       *IN DECORRENZA
016700051216     C     pardde        ANDLE     CAEDSC
016800070118     c                   if        J=0 OR  caetfe<>tfa(J)
016900971121     C                   ADD       1             J
017000971121     C                   Z-ADD     CAETFE        TFA(J)
017100971121E   4C                   ENDIF
017200070118E   4C                   ENDIF
017300971121E   3C                   ENDIF
017400971121E   2C                   ENDIF
017500070118     C     KCEEPA        READE     AZCAE05L                               99
017600971121E   1C                   ENDDO
017700070117     c                   endif
017800070117     c
017900070117     C                   Z-ADD     J             WUTFA                          *ULTIMO TFA
018000070117     c
018100061205     C* PARTENZA se immesso il terminal, carico solo lui
018200061205     c                   if        partfp>0
018300061205     c                   eval      I=1
018400061205     c                   eval      tfp(1)=partfp
018500061205     c                   else
018600061205     c
018700971121     C                   MOVEL     'P'           KCEEPA                         *ARRIVO
018800070118     C     KCEEPA        SETLL     AZCAE05L
018900070118     C     KCEEPA        READE     AZCAE05L                               99
019000971121DO  1C     *IN99         DOWEQ     *OFF
019100971121IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
019200971121IF  3C     CAETFA        IFEQ      *ZEROS                                       *STANDARD
019300051216IF  4C     pardde        IFGE      CAEDDE                                       *IN DECORRENZA
019400051216     C     pardde        ANDLE     CAEDSC
019500070118     c                   if        i=0 OR  caetfe<>tfp(I)
019600971121     C                   ADD       1             I
019700971121     C                   Z-ADD     CAETFE        TFP(I)
019800971121E   4C                   ENDIF
019900070118E   4C                   ENDIF
020000971121E   3C                   ENDIF
020100971121E   2C                   ENDIF
020200070118     C     KCEEPA        READE     AZCAE05L                               99
020300971121E   1C                   ENDDO
020400100806     c
020500061205     c                   endif
020600061205     c
020700000620     C                   Z-ADD     I             WUTFP                          *ULTIMO TFP
020800971121     C*
020900971121     C* SVILUPPA LE COMBINAZIONI TERMINAL/TERMINAL
021000971121     C                   EXSR      SVILUT
021100971121     C*
021200971121     C* CARICA ULTERIORI COMBINAZIONI TERMINAL/TERMINAL DA TERMINAL ECCEZIONI
021300070118     c* non lo faccio se ho richiesto un solo terminal di arrivo
021400070118     c* non serve in quanto per quel temrinal ho giàù creato tutte le
021500070118     c*  combinazioni con i temrinal di partenza
021600070118     c                   if        partfa=0
021700971121     C                   EXSR      CARECC
021800070118     c                   endif
021900971121     C*
022000971121     C                   ENDSR
022100971121     C*------------------------------------------------------------------------*
022200971121     C* SVILUT - SVILUPPA LE COMBINAZIONI TERMINAL/TERMINAL
022300971121     C*------------------------------------------------------------------------*
022400971121     C     SVILUT        BEGSR
022500971121     C*
022600971121     C* CICLO FINO A FINE TERMINAL PARTENZA
022700971121     C                   Z-ADD     *ZEROS        NC
022800971121DO  1C     1             DO        WUTFP         I
022900971121IF  2C     TFP(I)        IFGT      *ZEROS                                       *SE MEMORIZZATO
023000971121     C*
023100971121     C* CICLO FINO A FINE TERMINAL ARRIVO
023200971121DO  3C     1             DO        WUTFA         J
023300971121IF  4C     TFA(J)        IFGT      *ZEROS                                       *SE MEMORIZZATO
023400971121     C*
023500971121     C* MEMORIZZA LA COMBINAZIONE
023600971121     C                   MOVEL     TFP(I)        WN6               6 0
023700971121     C                   MOVE      TFA(J)        WN6               6 0
023800971121     C                   SETOFF                                           28
023900971121     C     WN6           LOOKUP    CPA                                    28
024000971121IF  5C     *IN28         IFEQ      *OFF                                         *SE MAI MEMORIZZATA
024100971121     C                   ADD       1             NC
024200971121     C                   MOVEL     TFP(I)        CPA(NC)
024300971121     C                   MOVE      TFA(J)        CPA(NC)
024400971121E   5C                   ENDIF
024500971121E   4C                   ENDIF
024600971121E   3C                   ENDDO                                                  *FINE TERM ARRIVO
024700971121E   2C                   ENDIF
024800971121E   1C                   ENDDO                                                  *FINE TERM PARTENZA
024900971121     C*
025000971121     C                   Z-ADD     NC            WUCOM                          *ULTIMA COMBINAZIONE
025100971121     C*
025200971121     C                   ENDSR
025300971121     C*------------------------------------------------------------------------*
025400971121     C* CARECC - CARICA COMBINAZIONI TERMINAL/TERMINAL DA TERMINAL ECCEZIONI
025500971121     C*------------------------------------------------------------------------*
025600971121     C     CARECC        BEGSR
025700971121     C*
025800971121     C                   Z-ADD     WUCOM         NC                             *ULTIMA COMBINAZIONE
025900971121     C*---
026000971121     C* CARICA I TERMINAL ECCEZIONE
026100971121     C*---
026200971121     C* ARRIVO
026300971121     C                   MOVEL     'A'           KCEEPA
026400971121     C     KCEEPA        SETLL     AZCAE01L
026500971121     C     KCEEPA        READE     AZCAE01L                               99
026600971121DO  1C     *IN99         DOWEQ     *OFF
026700971121IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
026800051216IF  4C     pardde        IFGE      CAEDDE                                       *IN DECORRENZA
026900051216     C     pardde        ANDLE     CAEDSC
027000061205     c
027100061205IF  3C     CAETFP        IFGT      *ZEROS                                       *ECCEZIONE
027200061205     c     partfp        andeq     0
027300061205     c     caetfp        oreq      partfp
027400061205     c     partfp        andgt     0
027500061205     c
027600971121     C                   MOVEL     CAETFP        WN6               6 0
027700971121     C                   MOVE      CAETFE        WN6               6 0
027800971121     C                   SETOFF                                           28
027900971121     C     WN6           LOOKUP    CPA                                    28
028000971121IF  5C     *IN28         IFEQ      *OFF                                         *SE MAI MEMORIZZATA
028100971121     C                   ADD       1             NC
028200971121     C                   MOVEL     CAETFP        CPA(NC)
028300971121     C                   MOVE      CAETFE        CPA(NC)
028400971121E   5C                   ENDIF
028500971121E   4C                   ENDIF
028600971121E   3C                   ENDIF
028700971121E   2C                   ENDIF
028800971121     C     KCEEPA        READE     AZCAE01L                               99
028900971121E   1C                   ENDDO                                                  *FINE ECCEZIONI
029000971121     C*---
029100971121     C* PARTENZA -> NON ESISTONO
029200971121     C*---
029300971121     C                   Z-ADD     NC            WUCOM                          *ULTIMA COMBINAZIONE
029400971121     C*
029500971121     C                   ENDSR
029600971121     C*------------------------------------------------------------------------*
029700971121     C* AGGSTM - AGGIORNA IL FILE E STAMPA L'ESITO DELL'ELABORAZIONE
029800971121     C*------------------------------------------------------------------------*
029900971121     C     AGGSTM        BEGSR
030000971121     C*
030100971121     C* CICLO PER TUTTE LE COMBINAZIONI MEMORIZZATE
030200971121DO  1C     1             DO        WUCOM         NC                6 0
030300971121IF  2C     CPA(NC)       IFGT      *ZEROS                                       *SE MEMORIZZATA
030400971121     C*
030500971121     C* CICLO PER CIASCUN TIPO SERVIZIO ESISTENTE
030600971121DO  3C     1             DO        WUTSP         TS                6 0
030700971121     C*
030800971121     C* SCRIVE IL TEMPO
030900971121IF  4C     PARTEM        IFEQ      'P'                                          *TEMPO PARTENZA
031000971121     C                   MOVEL     'P'           KCSPTA
031100971121X   4C                   ELSE                                                   *TEMPO TRAZIONE
031200971121     C                   MOVEL     'T'           KCSPTA
031300971121E   4C                   ENDIF
031400971121     C                   MOVEL     CPA(NC)       KCSLNP                         *LINEA PARTENZA
031500971121     C                   MOVE      CPA(NC)       KCSLNA                         *LINEA ARRIVO
031600971121     C                   MOVEL     TSP(TS)       KCSTSP                         *TIPO SERVIZIO
031700971121     C                   MOVEL     'N'           $ESI                           *ESISTENZA TEMPO
031800971121     C     KEYCAS        SETLL     AZCAS01L
031900971121     C     KEYCAS        READE(N)  AZCAS01L                               99
032000971121DO  4C     *IN99         DOWEQ     *OFF
032100971121     C     $ESI          ANDEQ     'N'
032200971121IF  5C     CASATB        IFEQ      *BLANKS                                      *VALIDO
032300051216IF  6C     CASDDE        IFLE      pardde                                       *IN DECORRENZA
032400971121     C                   MOVEL     'S'           $ESI                           *ESISTENZA TEMPO
032500971121E   6C                   ENDIF
032600971121E   5C                   ENDIF
032700971121     C     KEYCAS        READE(N)  AZCAS01L                               99
032800971121E   4C                   ENDDO                                                  *FINE TEMPI
032900971121     C*
033000971127     C* RECORD INESISTENTE, CREA TEMPO E STAMPA
033100971121IF  4C     $ESI          IFEQ      'N'                                          *ESISTE
033200971121     C                   EXSR      CRECAS                                       *CREAZIONE TEMPO
033300971121E   4C                   ENDIF
033400971121E   3C                   ENDDO                                                  *FINE SERVIZI
033500971121E   2C                   ENDIF
033600971121E   1C                   ENDDO                                                  *FINE COMBINAZIONI
033700971121     C*
033800971121     C                   ENDSR
033900971121     C*------------------------------------------------------------------------*
034000971121     C* CRECAS - CREA IL TEMPO
034100971121     C*------------------------------------------------------------------------*
034200971121     C     CRECAS        BEGSR
034300061205     c                   clear                   wtrova            1
034400061205     c* Se c'e' la copia da terminal, chaino il record per duplicarlo
034500061205     c*  altrimenti come ora
034600070118     c* PARTENZA
034700061205    0c                   if        partpcpy>0
034800061205     C                   MOVEL     partpcpy      KCSLNP                         *LINEA PARTENZA
034900061205     C     KEYCAS        SETLL     AZCAS01L
035000061205     C     KEYCAS        READE(N)  AZCAS01L                               99
035100061205DO  4C     *IN99         DOWEQ     *OFF
035200061205     C     $ESI          ANDEQ     'N'
035300061205IF  5C     CASATB        IFEQ      *BLANKS                                      *VALIDO
035400061205IF  6C     CASDDE        IFLE      pardde                                       *IN DECORRENZA
035500061205     C                   MOVEL     'S'           wtrova                         *ESISTENZA TEMPO
035600061205E   6C                   ENDIF
035700061205E   5C                   ENDIF
035800061205     C     KEYCAS        READE(N)  AZCAS01L                               99
035900061205E   4C                   ENDDO                                                  *FINE TEMPI
036000061205     c                   endif
036100070118     c* ARRIVO
036200070118    0c                   if        partacpy>0
036300070118     C                   MOVEL     partacpy      KCSLNa                         *LINEA PARTENZA
036400070118     C     KEYCAS        SETLL     AZCAS01L
036500070118     C     KEYCAS        READE(N)  AZCAS01L                               99
036600070118DO  4C     *IN99         DOWEQ     *OFF
036700070118     C     $ESI          ANDEQ     'N'
036800070118IF  5C     CASATB        IFEQ      *BLANKS                                      *VALIDO
036900070118IF  6C     CASDDE        IFLE      pardde                                       *IN DECORRENZA
037000070118     C                   MOVEL     'S'           wtrova                         *ESISTENZA TEMPO
037100070118E   6C                   ENDIF
037200070118E   5C                   ENDIF
037300070118     C     KEYCAS        READE(N)  AZCAS01L                               99
037400070118E   4C                   ENDDO                                                  *FINE TEMPI
037500070118     c                   endif
037600971121     C*
037700061205    0c                   if        wtrova=' '
037800971121     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
037900971121IF  1C     PARTEM        IFEQ      'P'                                          *TEMPO PARTENZA
038000971121     C                   MOVEL     'P'           CASPTA
038100971121     C                   Z-ADD     *ZEROS        CASTIN                         *TEMPO INTERNO
038200971121     C                   Z-ADD     *ZEROS        CASTCN                         *TEMPO CLIENTE
038300971230IF  2C     TSP(TS)       IFEQ      'D'                                          *DISTRIBUZIONE
038400971230     C                   Z-ADD     24            CASTIN                         *TEMPO INTERNO
038500971230     C                   Z-ADD     24            CASTCN                         *TEMPO CLIENTE
038600971230E   2C                   ENDIF
038700971121X   1C                   ELSE                                                   *TEMPO TRAZIONE
038800971121     C                   MOVEL     'T'           CASPTA
038900971121     C                   Z-ADD     24            CASTIN                         *TEMPO INTERNO
039000971121     C                   Z-ADD     24            CASTCN                         *TEMPO CLIENTE
039100051028      * (Tipo servizio Poste - "P" - non più caricato in TSP...)
039200000620IF  2C     TSP(TS)       IFEQ      'P'                                          *POSTE
039300000620     C                   Z-ADD     *ZEROS        CASTIN                         *TEMPO INTERNO
039400000620     C                   Z-ADD     *ZEROS        CASTCN                         *TEMPO CLIENTE
039500000620E   2C                   ENDIF
039600000620E   1C                   ENDIF
039700061205    0c                   endif
039800971121     C                   MOVEL     CPA(NC)       CASLNP                         *LINEA PARTENZA
039900971121     C                   MOVE      CPA(NC)       CASLNA                         *LINEA ARRIVO
040000971121     C                   MOVEL     TSP(TS)       CASTSP                         *TIPO SERVIZIO
040100061205     c
040200051216     C                   Z-ADD     pardde        CASDDE                         *DECORRENZA
040300971121     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
040400971121     C                   Z-ADD     *ZEROS        CASDTR                         *DATA TRASMISSIONE
040500971121     C                   MOVEL     *BLANKS       CASATB                         *FLAG ANNULLAMENTO
040600971121     C* SCRIVE
040700971121     C                   MOVEL     *BLANKS       STMMSG
040800971121IF  1C     PARSIM        IFEQ      'N'                                          AG G I O R N A
040900971121     C                   WRITE     AZCAS000                             98
041000971121IF  2C     *IN98         IFEQ      *OFF                                         *OK IN SCRITTURA
041100971121     C                   MOVEL(P)  MSG(1)        STMMSG
041200971121X   2C                   ELSE                                                   *ERRORE IN SCRITTURA
041300971121     C                   MOVEL(P)  MSG(2)        STMMSG
041400971121E   2C                   ENDIF
041500971121E   1C                   ENDIF
041600000620     C*
041700000620     C* IMPOSTA ALCUNI DATI PER LA STAMPA
041800000620     C                   Z-ADD     CASDDE        G08INV
041900000620     C                   Z-ADD     *ZEROS        G08DAT
042000000620     C                   MOVEL     '3'           G08ERR
042100000620     C                   CALL      'XSRDA8'
042200000620     C                   PARM                    WLBDA8
042300000620     C                   Z-ADD     G08DAT        DDECAS            8 0          *DATA DECORRENZA
042400971121     C* STAMPA ESITO
042500971121     C                   EXSR      STADET
042600971121     C*
042700971121     C                   ENDSR
042800971121     C*------------------------------------------------------------------------*
042900971121     C* STADET - STAMPA RIGA DI DETTAGLIO
043000971121     C*------------------------------------------------------------------------*
043100971121     C     STADET        BEGSR
043200971121     C*
043300971121     C* FINE PAGINA, STAMPA TESTATA
043400971121IF  1C     *INOF         IFEQ      *ON
043500971121     C                   EXCEPT    TES
043600971121     C                   SETOFF                                       OF
043700971121E   1C                   ENDIF
043800971121     C*
043900971121     C* STAMPA RIGA DI DETTAGLIO
044000971121     C                   EXCEPT    DET
044100971121     C*
044200971121     C                   ENDSR
044300971121     C*------------------------------------------------------------------------*
044400971121     C* CARTAB - CARICA LE TABELLE
044500971121     C*------------------------------------------------------------------------*
044600971121     C     CARTAB        BEGSR
044700971121     C*--------------------
044800971121     C* TIPI SERVIZIO
044900971121     C*--------------------
045000971121     C                   Z-ADD     *ZEROS        I
045100020919     C                   MOVEL     1             KTBKUT
045200971121     C                   MOVEL     '5E'          KTBCOD
045300971121     C     KEYTAB        SETLL     TABEL00F
045400971121     C     KEYTAB        READE     TABEL00F                               99
045500971121DO  1C     *IN99         DOWEQ     *OFF
045600971121IF  2C     TBLFLG        IFEQ      *BLANKS
045700051028      * non carico né annullati né "servizi Poste"
045800051028     c                   movel     TBLuni        ds5E
045900051028     c                   if        §5Euso <> 'P'
046000971121     C                   ADD       1             I
046100971121     C                   MOVEL     TBLKEY        TSP(I)
046200051028     c                   endif
046300971121E   2C                   ENDIF
046400971121     C     KEYTAB        READE     TABEL00F                               99
046500971121E   1C                   ENDDO
046600971121     C                   Z-ADD     I             WUTSP                          *ULTIMO SERVIZIO
046700971121     C*
046800971121     C                   ENDSR
046900020919     C*--------------------------------------------------------------------------------------------*
047000020919     C* REPERISCE I DATI UTENTE
047100020919     C*--------------------------------------------------------------------------------------------*
047200020919     C     REPDATIUTE    BEGSR
047300020919     C*
047400020919     C* INIZIALIZZA VARIABILI DI WRK
047500020919     C                   CLEAR                   TIBS34DS
047600020919     C                   CLEAR                   AZUTEDS
047700020919     C                   CLEAR                   DDATIUTE
047800020919     C*
047900020919     C     *DTAARA       DEFINE    §azute        azuteds
048000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
048100020919     C                   IN(E)     *DTAARA
048200020919if  1C                   IF        %Error
048300020919     c                   EVAL      I34Tla = 'L'
048400020919     C                   CALL      'TIBS34R'
048500020919     C                   PARM                    Tibs34Ds
048600020919     C                   IN        *DTAARA
048700020919e   1C                   ENDIF
048800020919     C*
048900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
049100020919     C*
049200020919     C                   ENDSR
049300971121     C*------------------------------------------------------------------------*
049400971121     C* *INZSR - OPERAZIONI INIZIALI
049500971121     C*------------------------------------------------------------------------*
049600971121     C     *INZSR        BEGSR
049700971121     C*--------------------
049800971121     C* RICEVIMENTO PARAMETRI
049900971121     C*--------------------
050000971121     C     *ENTRY        PLIST
050100971121     C                   PARM                    PARTEM            1
050200971121     C                   PARM                    PARSIM            1
050300051216     C                   PARM                    PARdde
050400061205     C                   PARM                    PARtfp
050500061205     C                   PARM                    PARtpcpy
050600070117     C                   PARM                    PARtfa
050700070117     C                   PARM                    PARtacpy
050800971121     C*--------------------
050900971121     C* VARIABILI RIFERITE AL DATABASE
051000971121     C*--------------------
051100971121     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
051200971121     C     *LIKE         DEFINE    TBLCOD        KTBCOD
051300971121     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
051400971121     C     *LIKE         DEFINE    CASPTA        KCSPTA                         *AZCAS00F
051500971121     C     *LIKE         DEFINE    CASLNP        KCSLNP
051600971121     C     *LIKE         DEFINE    CASLNA        KCSLNA
051700971121     C     *LIKE         DEFINE    CASTSP        KCSTSP
051800971121     C*--------------------
051900971121     C* CHIAVI DI LETTURA
052000971121     C*--------------------
052100971121     C* LETTURA TABEL00F - PARZIALE
052200971121     C     KEYTAB        KLIST
052300971121     C                   KFLD                    KTBKUT                         *UTENTE
052400971121     C                   KFLD                    KTBCOD                         *TABELLA
052500971121     C* LETTURA AZCAS01L - PARZIALE
052600971121     C     KEYCAS        KLIST
052700971121     C                   KFLD                    KCSPTA                         *TIPO TEMPO
052800971121     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
052900971121     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
053000971121     C                   KFLD                    KCSTSP                         *TIPO SERVIZIO
053100971121     C*--------------------
053200971121     C* VARIABILI DI CONTROLLO
053300971121     C*--------------------
053400971121     C                   MOVEL     'N'           $ESI              1            *ESISTENZA TEMPO
053500971121     C                   Z-ADD     *ZEROS        WUTFP             6 0          *ULTIMO TERM PART
053600971121     C                   Z-ADD     *ZEROS        WUTFA             6 0          *ULTIMO TERM ARR
053700971121     C                   Z-ADD     *ZEROS        WUCOM             6 0          *ULTIMA COMBINAZIONE
053800971121     C                   Z-ADD     *ZEROS        WUTSP             6 0          *ULTIMO SERVIZIO
053900971121     C                   MOVEL     *BLANKS       STMMSG           66            *MESSAGGIO IN STAMPA
054000971121     C*
054100971121     C* REPERIMENTO DATI UTENTE
054200020919     C                   EXSR      REPDATIUTE
054300971121     C*
054400971121     C* CARICA LE TABELLE
054500971121     C                   EXSR      CARTAB
054600971121     C*
054700971121     C* CALCOLA LA DATA CORRENTE
054800051216     C*                  TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
054900051216     C*                  MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
055000051216     C*                  Z-ADD     WNUM8         G08DAT
055100051216     C*                  Z-ADD     *ZEROS        G08INV
055200051216     C*                  MOVEL     '0'           G08ERR
055300051216     C*                  CALL      'XSRDA8'
055400051216     C*                  PARM                    WLBDA8
055500051216     C*                  Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
055600971121     C*
055700971121     C                   ENDSR
055800971121     O*------------------------------------------------------------------------*
055900971121     OQSYSPRT   E            TES               2
056000971121     O                       DSFIRS              20
056100971121     O                                           30 'TISI28R '
056200971121     O                                           62 '*** AUTOMATISMI TERMPI T'
056300971121     O                                           73 'RAZIONE ***'
056400971121     O                       UDATE               95 '  /  /  '
056500971121     O                                          128 'PAG.'
056600971121     O                       PAGE          Z    132
056700000620     O*------------------------
056800971121     O          E            TES               4
056900971121     O                       STM(1)              66
057000971121     O                       STM(1)             132
057100000620     O*------------------------
057200971121     O          E            TES               5
057300971121     O                       STM(2)              66
057400000620     O                       STM(4)             132
057500000620     O*------------------------
057600971121     O          E            TES               6
057700971121     O                       STM(1)              66
057800971121     O                       STM(1)             132
057900971121     O*------------------------------------
058000971121     O          E            DET         1
058100971121     O                       CASPTA               3
058200971121     O                       CASLNP              12
058300971121     O                       CASLNA              22
058400971121     O                       CASTSP              29
058500000620     O                       DDECAS              45
058600000620     O                       CASTIN              51
058700000620     O                       CASTCN              58
058800000620     O                       STMMSG             132
058900971121     O*------------------------------------
059000971121     O          E            FIN         1
059100971121     O                                           76 '*** FINE STAMPA ***'
059200971121     O*------------------------------------------------------------------------*
059300971121** STM - RIGHE
059400971121------------------------------------------------------------------  1
059500000620TEMPO  PARTENZA  ARRIVO  SERVIZIO  DECORRENZA  T.INT  T.CLI         2
059600000620  X      999       999      X      99/99/9999   999    999          3
059700000620ESITO                                                               4
059800000620XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  5
059900971121** MSG - MESSAGGI
060000971121RECORD INSERITO                                                     1
060100971121ERRORE IN SCRITTURA                                                 2
0602009711213
0603009711214
0604009711215
