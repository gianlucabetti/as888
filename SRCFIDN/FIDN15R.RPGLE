000100980506      *------------------------------------------------------------------------*
000200980506      * GESTIONE ANAGRAFICA FASI                                               *
000300980506      *------------------------------------------------------------------------*
000400980506     H DECEDIT('0,') DATEDIT(*DMY.)
000500980506      *--------------------------------------------
000600980717     FFIDN15D   CF   E             WORKSTN sfile(fi15s03:nrr1)
000700980506     FFNDFA01L  UF A E           K DISK
000800980717     FFNDFd01L  UF A E           K DISK
000900981030     FTNTBE01L  IF   E           k DISK
001000980508     FPRTF198   O    F  198        PRINTER       oflind(*INOF)   usropn
001100980506      *--------------------------------------------
001200990127     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
001300980508     D KOS             S            100    DIM(6) CTDATA PERRCD(1)              DISEGNO STAMPA
001400980720     D TA              S              2    dim(30)
001500980507      *--------------------------------------------
001600980507     D KPJBA         E DS
001700980508     D CNCR80        E DS
001800980507     D UT§DSE0F      E DS
001900980601     D fidn05        E DS                  extname(fidn05ds)
002000980601     D fndfa         E DS                  extname(fndfa00f)
002100980507      *--------------------------------------------
002200980507     D KFCA            S                   LIKE(dfafca)
002300980508     D KFRA            S                   LIKE(dfafra)
002400980717     D tad             S                   LIKE(dfdtad)
002500980717     D codt            S                   LIKE(tbecod)
002600980717     D key1            S                   LIKE(tbeke1)
002700980717     D key2            S                   LIKE(tbeke2)
002800980717     D Klin            S                   LIKE(tbelin)
002900980507     D TRAT            S            198    INZ(*all'-')
003000980507     D W001A           S              1
003100980507     D W0140           S             14  0
003200980507     D WDTGIO          S              8  0
003300980507     D WORA            S              6  0
003400980506      *--------------------------------------------
003500980908     C*  RIEPILOGO INDICATORI
003600980908      *--------------------------------------------
003700980908      * 01    - IMMISSIONE NUOVA FASE
003800981030      * 02    - LETTURA FILE NUOVA FASE
003900981030      * 03    - FGASE ANNULLATA
004000980908      * 20/22 - Gestione SUBFILE
004100980908      * 28    - ERRORE GENERICO EMISSIONE CAMPO MESSAGGI
004200980908      * 30/34 - di comodo
004300990127      * 40/42 - Errori a video
004400980908      *----------------------------------------------------------
004500980506      *
004600980506      * EMISSIONE PRIMA VIDEATA
004700980506     C     EMD01         TAG
004800980507     C                   EXFMT     FI15D01
004900980506     C                   CLEAR                   V1CMSG
005000980506     C                   eval      *in28 = *OFF
005100980506      * F3=FINE
005200981030     c   KC              goto      fine
005300980506      * F8=STAMPA
005400980508     c                   IF        *INKH =*on
005500980508     c                   exsr      SUB_STAMPA
005600980508     C                   movel     MSG(5)        V1CMSG
005700980508     C                   eval      *in28 = *on
005800980506     c                   goto      EMD01
005900980508     c                   ENDIF
006000980506      * CONTROLLI VIDEATA
006100980506     C                   EXSR      CTRD01
006200980506      *
006300980506     C   28              GOTO      EMD01
006400980506      *
006500980507      * -------------------  PASSAGGIO DA PRIMA A SECONDA VIDEATA
006600980507      *
006700980507      * Se esiste rcd "riempio" la videata
006800980507     C                   MOVEL     V1CFCA        KFCA
006900980507     C                   movel     V1Cfca        V2Cfca
007000980506     c     KFCA          CHAIN     FNDFA000                           01
007100980507     C                   EXSR      INZD02
007200980507     C  N01              EXSR      RIED02
007300981030      *
007400981030      * pulizia del subfile dettaglio anagrafica fase
007500980717     c                   setoff                                       2021
007600981030     c                   eval      *IN22 = *ON
007700981030     c                   WRITE     fi15c03
007800981030     c                   eval      *IN22 = *OFF
007900981030     c                   clear                   nrr1
008000980507      *
008100980507      * -------------------  GESTIONE SECONDA VIDEATA --------------------------
008200980506      *
008300980507      * EMISSIONE
008400980506     C     EMD02         TAG
008500980506     C                   EXFMT     FI15D02
008600980506     C                   CLEAR                   V2CMSG
008700980507     C                   eval      *in28 = *off
008800990127     C                   SETOFF                                       404142
008900980506      * F3=FINE
009000981030     c   KC              goto      fine
009100980507      * F12=RITORNO
009200981030     C   KL              GOTO      EMD01
009300980507      *
009400980506      * CONTROLLI VIDEATA
009500980506     C                   EXSR      CTRD02
009600980717      * errori
009700980717     C   28              GOTO      EMD02
009800981030      *
009900981030      * Per F5 o F16 passo alla conferma
010000981030     c                   IF        *INKE = *ON  or  *INKQ = *ON
010100981030     c                   EXSR      RIEDFA
010200981030     C                   GOTO      EMD01
010300981030     C                   ENDIF
010400981030      * -------------------  GESTIONE TERZA VIDEATA --------------------------
010500981030      *
010600980717      * gestione subfile tipo anomalie valide
010700990127     c                   if        v2ctad <> 'SI'
010800980717     c                   EXSR      SUB_DET
010900980717     c                   end
011000981030      * F12=RITORNO
011100981030     C   KL              GOTO      EMD02
011200981030      *
011300981030      * F6=CONFERMA
011400981030     c                   if        *INKF = *ON
011500981030     c                   EXSR      RIEDFA
011600981030     C                   GOTO      EMD01
011700981030     c                   else
011800981030     C                   GOTO      EMD02
011900981030     c                   endif
012000980506      *
012100980506     C     FINE          TAG
012200980506     C                   EVAL      *INLR = *ON
012300980506      *****************************************************************
012400980506      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
012500980506      *****************************************************************
012600980506     C     CTRD01        BEGSR
012700980508      *
012800980508      * RICERCA
012900980508     C     '?'           SCAN      V1CFCA                                 31
013000980508     C     *IN31         IFEQ      *ON
013100980601     c                   clear                   fidn05
013200980601     c                   move      'R'           I05MOD
013300980601     C                   CALL      'FIDN05R'                            99
013400980601     c                   parm                    kpjba
013500980601     c                   parm                    fidn05
013600980601     c                   if        O05ERR = 'E'
013700980601     c                   movel     O05msg        v1cmsg
013800980601     c                   eval      *in28 = *on
013900980602     c                   else
014000980601     c                   movel     o05rec        fndfa
014100980601     c                   movel     dfafca        v1cfca
014200980602     c                   endif
014300980601     C                   Else
014400980507      *
014500980507     C                   IF        V1CFCA = *zeros  OR  V1CFCA = *blanks
014600980507     C                   movel     MSG(1)        V1CMSG
014700980507     C                   eval      *in28 = *on
014800980507     C                   ELSE
014900980507      *
015000980507     C                   testn                   V1CFCA               31
015100980507      * testo ultimo char
015200980507     C   31              move      V1CFCA        W001A
015300980507     C   31              eval      *in31 = (W001A >= '0')
015400980507      *
015500980507     c                   IF        *IN31 = *OFF
015600980507     C                   movel     MSG(1)        V1CMSG
015700980507     C                   eval      *in28 = *on
015800980507     C                   ENDIF
015900980507     C                   ENDIF
016000980507      *
016100980601     C                   ENDIF
016200980506     C                   ENDSR
016300980507      *****************************************************************
016400980507      *   ROUTINE PER INIZIALIZZAZIONE SECONDA VIDEATA
016500980507      *****************************************************************
016600980507     C     INZD02        BEGSR
016700980507      *
016800981030     c                   eval      *IN03 = *OFF
016900980507     C                   movel     *blanks       V2Cdes
017000980507     C                   movel     *blanks       V2Cobl
017100980507     C                   movel     *blanks       V2Cval
017200980507     C                   movel     *blanks       V2Cene
017300981030     C                   movel     *blanks       V2Cfde
017400980507     C                   movel     *blanks       V2Cgfs
017500981125     C                   movel     *blanks       v2Crud
017600980507     C                   movel     *blanks       V2Cfgc
017700980507     C                   movel     *blanks       V2Cdgc
017800980507     C                   movel     *blanks       V2Cpgm
017900980507     C                   movel     *blanks       V2Cle1
018000980507     C                   movel     *blanks       V2Cle2
018100980507     C                   z-add     *zeros        V2Cgga
018200980507     C                   movel     *blanks       V2Cfra
018300980507     C                   movel     *blanks       V2Dfra
018400981119     C                   movel     *blanks       V2ctad
018500980731     C                   movel     *blanks       V2ccog
018600980507      *
018700980507     C                   ENDSR
018800980506      *****************************************************************
018900980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
019000980506      *****************************************************************
019100980506     C     RIED02        BEGSR
019200980506      *
019300980506     C                   movel     DFAdes        V2Cdes
019400980506     C                   movel     DFAobl        V2Cobl
019500980506     C                   movel     DFAval        V2Cval
019600980506     C                   movel     DFAene        V2Cene
019700981030     C                   movel     DFAfde        V2Cfde
019800980506     C                   movel     DFAgfs        V2Cgfs
019900981125     C                   movel     DFArud        V2Crud
020000980506     C                   movel     DFAfgc        V2Cfgc
020100980506     C                   movel     DFAdgc        V2Cdgc
020200980506     C                   movel     DFApgm        V2Cpgm
020300980506     C                   movel     DFAle1        V2Cle1
020400980506     C                   movel     DFAle2        V2Cle2
020500980506     C                   z-add     DFAgga        V2Cgga
020600980506     C                   movel     DFAfra        V2Cfra
020700981030     C                   eval      *IN03 = (DFAatb <> *blanks)
020800980731     C                   movel     DFAcog        V2ccog
020900980717      * verifico se la fase e' valida per tutti i tipi anomalia oppure solo per alcuni
021000980717      * provo con tipo anomalia a blank  (cioe' valida per tutti)
021100981119     c                   movel     *blanks       v2ctad
021200980717     c                   movel     *blanks       tad
021300980717     c     kdfd          chain     fndfd01l                           30
021400980717     c  n30              movel     'SI'          V2Ctad
021500980717     c   30              movel     'NO'          V2CTAD
021600980507      *
021700980506      * Decodifico fase di riferimento
021800980508     C                   z-add     DFAfra        KFRA
021900980508     c     KFRA          CHAIN (N) FNDFA000                           01
022000980506     C  N01              movel     DFADES        V2Dfra
022100980506      *
022200980506      * Riaggancio fase in elaborazione
022300980506     c     KFCA          CHAIN     FNDFA000                           01
022400980506      *
022500980506     C                   ENDSR
022600980506      *****************************************************************
022700980506      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
022800980506      *****************************************************************
022900980506     C     CTRD02        BEGSR
023000980506      *
023100980507     C                   eval      V2DFRA = *BLANKS
023200980507      *
023300980506      * Descrizione fase
023400980506     C                   IF        V2CDES = *blanks
023500980506     C                   movel     MSG(2)        V2CMSG
023600980506     C                   eval      *in28 = *on
023700980507     C                   eval      *in40 = *on
023800980506     C                   goto      endctrd02
023900980506     C                   ENDIF
024000980508      *
024100980508      * RICERCA FASE PER ATTESA
024200980508     C     '?'           SCAN      V2CFRA                                 31
024300980508     C     *IN31         IFEQ      *ON
024400980602     c                   clear                   fidn05
024500980602     c                   move      'R'           I05MOD
024600980602     C                   CALL      'FIDN05R'                            99
024700980602     c                   parm                    kpjba
024800980602     c                   parm                    fidn05
024900980602     c                   if        O05ERR = 'E'
025000980602     c                   movel     O05msg        v1cmsg
025100980602     c                   eval      *in28 = *on
025200980602     C                   goto      endctrd02
025300980602     c                   endif
025400980602     c                   movel     o05rec        fndfa
025500980602     c                   movel     dfafca        v2cfra
025600980602     C                   movel     DFADES        V2Dfra
025700980602     C                   Endif
025800980506      *
025900980506      * Fase di riferimento per attesa
026000980602     C                   IF        V2CFRA <> *zeros  AND  V2CFRA <> *blanks
026100980507     C                   testn                   V2CFRA               31
026200980507      * testo ultimo char
026300980507     C   31              move      V2CFRA        W001A
026400980507     C   31              eval      *in31 = (W001A >= '0')
026500980507      *
026600980507     c                   IF        *IN31 = *OFF
026700980507     C                   movel     MSG(1)        V2CMSG
026800980507     C                   eval      *in28 = *on
026900980511     C                   eval      *in41 = *on
027000980507     C                   goto      endctrd02
027100980507     C                   ENDIF
027200980507     C                   ENDIF
027300980507      *
027400980507      * Decodifico fase di riferimento
027500980507     C                   IF        V2CFRA > *zeros
027600980508     C                   movel     V2CFRA        KFRA
027700980508     c     KFRA          CHAIN (N) FNDFA000                           28
027800980507     C  N28              movel     DFADES        V2Dfra
027900980507     C   28              movel     MSG(4)        V2CMSG
028000980511     C   28              eval      *in41 = *on
028100980507      *
028200980507      * Riaggancio fase in elaborazione
028300980507     c     KFCA          CHAIN     FNDFA000                           01
028400980507      *
028500980507     C   28              goto      endctrd02
028600980507     C                   ENDIF
028700980507      *
028800980507      * Congruità per gestione attesa esecuzione
028900980507     C                   IF        (V2CGGA = *zeros and
029000980507     C                                V2CFRA <> *blanks and V2CFRA <> *zeros)
029100980507     C                             OR
029200980507     C                             (V2CGGA > *zeros and
029300980507     C                                (V2CFRA = *blanks or V2CFRA = *blanks))
029400981125     c                             OR
029500981125     c                             (v2crud = 'S' and v2cgga = *zeros and
029600981125     C                                (V2CFRA = *blanks or V2CFRA = *blanks))
029700980507     C                   movel     MSG(4)        V2CMSG
029800980507     C                   eval      *in28 = *on
029900980511     C                   eval      *in41 = *on
030000980507     C                   goto      endctrd02
030100980507     C                   ENDIF
030200990127      *
030300990127      * Controllo se indicato la selezione dei tipi anomalie validi
030400990127     C                   IF        V2Ctad = *blanks
030500990127     C                   movel     MSG(6)        V2CMSG
030600990127     C                   eval      *in28 = *on
030700990127     C                   eval      *in42 = *on
030800990127     C                   goto      endctrd02
030900990127     C                   ENDIF
031000980506      *
031100980506     C     ENDCTRD02     ENDSR
031200980507      *****************************************************************
031300980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
031400980507      *****************************************************************
031500980507     C     RIEDFA        BEGSR
031600980507      *
031700980507     C                   movel     V2Cfca        DFAfca
031800980507     C                   movel     V2Cdes        DFAdes
031900980507     C                   movel     V2Cobl        DFAobl
032000980507     C                   movel     V2Cval        DFAval
032100980507     C                   movel     V2Cene        DFAene
032200981030     C                   movel     V2Cfde        DFAfde
032300980507     C                   movel     V2Cgfs        DFAgfs
032400981125     C                   movel     V2Crud        DFArud
032500980507     C                   movel     V2Cfgc        DFAfgc
032600980507     C                   movel     V2Cdgc        DFAdgc
032700980507     C                   movel     V2Cpgm        DFApgm
032800980507     C                   movel     V2Cle1        DFAle1
032900980507     C                   movel     V2Cle2        DFAle2
033000980507     C                   z-add     V2Cgga        DFAgga
033100980507     C                   movel     V2Cfra        DFAfra
033200980507     C                   movel     *blanks       DFAftr
033300980507     C                   z-add     *zeros        DFAdtr
033400981030      *
033500981030     C   KQ              movel     'A'           DFAatb
033600981030     C   KE              movel     *blanks       DFAatb
033700981030      *
033800980731     C                   movel     V2Ccog        DFAcog
033900980717     C   01              WRITE     FNDFA000
034000980717     C  N01              UPDATE    FNDFA000
034100981030      *
034200980717      * verifico il dettaglio
034300981030     c                   IF        v2ctad = 'SI'  or  DFAatb = 'A'              da annullare
034400980717      * tutti i tipi anomalie validi
034500980717     c     dfafca        setll     fndfd01l
034600980717     c     dfafca        reade     fndfd01l                               32
034700980717     c                   dow       *in32 = *off
034800980717     c                   delete    fndfd000
034900980717     c     dfafca        reade     fndfd01l                               32
035000980717     c                   enddo
035100980908      * solo se non c'e' annullamento
035200981030     c                   if        DFAatb <> 'A'
035300980717     C                   movel     V2Cfca        DFDfca
035400980717     C                   movel     *blanks       DFDatb
035500980717     C                   movel     *blank        DFDtad
035600980717     C                   WRITE     FNDFd000
035700980908     c                   endif
035800980908      *
035900981030     c                   ELSE
036000981030      *
036100981030     c                   IF        *INKQ = *OFF  and  *INKE = *OFF
036200981030      *
036300980717     C                   movel     V2Cfca        DFDfca
036400980717     C                   movel     *blank        tad
036500980717     c     kdfd          chain     fndfd01l                           32
036600980717     c  n32              delete    fndfd000
036700980717      * definito il dettaglio tipi anomalie validi
036800980717     c                   z-add     1             nrr1
036900980717     c                   setoff                                       32
037000980717     c                   dow       *in32 = *off
037100980717     c     nrr1          chain     fi15s03                            32
037200980717     c                   if        *in32 = *off   and
037300980717     c                             v3sopz <> *blank
037400980717     C                   movel     V2Cfca        DFDfca
037500980717     C                   movel     *blanks       DFDatb
037600980717     C                   movel     v3stad        DFDtad
037700980908     C                   movel     v3stad        tad
037800980908     c     kdfd          chain     fndfd01l                           34
037900980908     C   34              WRITE     FNDFd000
038000980717     c                   endif
038100980717     c                   if        *in32 = *off   and
038200980717     c                             v3sopz =  *blank
038300980717     C                   movel     V2Cfca        DFDfca
038400980717     C                   movel     v3stad        tad
038500980717     C     kdfd          delete    FNDFd01l                           33
038600980717     c                   endif
038700980717     c                   add       1             nrr1
038800980717     c                   enddo
038900981030     c                   ENDIF
039000981030      *
039100981030     c                   ENDIF
039200980507      *
039300980507     C                   ENDSR
039400980717      *****************************************************************
039500980717      *   ROUTINE DI GESTIONE VIDEATA DEL DETTAGLIO TIPO ANOMALIE VALIDE
039600980717      *****************************************************************
039700980717     C     SUB_DET       BEGSR
039800981030      *
039900980717      * caricamento SUBFILE solo se non e' stato mai caricato
040000980717     c     nrr1          ifeq      *zeros
040100980717     c                   exsr      carsbf
040200980717     c                   endif
040300980717      *
040400980717     c                   exsr      gessbf
040500980717      *
040600980717     c                   endsr
040700980717      *****************************************************************
040800980717      *   ROUTINE DI CARICAMENTI SUBFILE
040900980717      *****************************************************************
041000980717     C     CARSBF        BEGSR
041100980717      * caricamento subfile dei tipi anomalie
041200980717     c                   z-add     *zeros        nrr1              4 0
041300980717     C                   MOVEL     'TAD'         CODT
041400981030     C     KTBE          setll     TNTBE000
041500981030     c     ktbe          reade     tntbe000                               31
041600980717     c                   doW       *in31 = *off
041700980717      * controllo il sistema informativo
041800980717     c                   if        tbesif <> *blanks and
041900980717      * se non e' uguale a blank deve essere uguale al mio
042000980717     c                             tbesif = knsif or
042100980717     c                             tbesif = *blanks
042200980717      *
042300980717     c                   clear                   v3sopz
042400980717      * verifico se era gia' stata selezionata per la fase in gestione in variazione fase
042500980717     c                   if        *in01 = *off
042600980717     c                   movel     tbeke1        tad
042700980717     c     kdfd          chain     fndfd01l                           32
042800980720     c  n32              movel     '1'           v3sopz
042900980717     c                   endif
043000980717     c                   movel     tbeke1        v3stad
043100980717     c                   movel     tbeuni        v3sdta
043200980717     c                   add       1             nrr1
043300980717     c                   write     fi15s03
043400980717     c                   endif
043500980717      *
043600981030     c     ktbe          reade     tntbe000                               31
043700980717      *
043800980717     c                   enddo
043900980717      *
044000980717     c                   endsr
044100980717      *****************************************************************
044200980717      *   ROUTINE DI gestione del subfile
044300980717      *****************************************************************
044400980717     C     GESSBF        BEGSR
044500980717     c     nrr1          comp      *zeros                             2020
044600980717     c                   seton                                        21
044700980717     c                   write     fi15z03
044800980717     c                   dow       *inkl = *off
044900980717     c                             and *inkf = *off
045000980717     c                   exfmt     fi15c03
045100980717      * KL F12 Ritorno alla videata precedente
045200980717     C                   if        *inkl = *off
045300980717     c                   readc     fi15s03                                32
045400980717     c                   end
045500980717      *
045600980717     c                   enddo
045700980717     c                   endsr
045800980717      *
045900980506      *****************************************************************
046000980506      *   ROUTINE DI STAMPA
046100980506      *****************************************************************
046200980506     C     SUB_stampa    BEGSR
046300980506      *
046400980508     C                   OPEN      PRTF198
046500980508     C                   EXCEPT    TESTA
046600980508      *
046700980508     C     *LOVAL        SETLL     FNDFA000
046800980506      *
046900980508     C                   DO        *HIVAL
047000980508     C                   READ      FNDFA000                               02
047100980508     C                   IF        *IN02 = *OFF
047200980508     C   OF              EXCEPT    TESTA
047300980508     C                   EXCEPT    DETTA
047400980720      * stampo il dettaglio se c'e'
047500980720     c                   move      *blanks       tad
047600981109     c                   movel     *blanks       ta
047700980720     c     kdfd          chain     fndfd01l                           30
047800980720     c                   if        *in30 = *on
047900980720     c     dfafca        setll     fndfd01l
048000980720     c     dfafca        reade     fndfd01l                               30
048100980720     c                   z-add     *zeros        x                 2 0
048200980720     c                   dow       *in30 = *off
048300980720     c     x             ifle      9
048400980720     c                   add       1             x
048500980720     c                   movea     dfdtad        ta(X)
048600980720     c                   endif
048700980720     c     dfafca        reade     fndfd01l                               30
048800980720     c                   enddo
048900980720     C                   EXCEPT    DETTd
049000980720     c                   endif
049100980720      *
049200980508     C                   ENDIF
049300980508     C  N02              ENDDO
049400980508      *
049500980508     C                   EXCEPT    CODA
049600980508     C                   CLOSE     PRTF198
049700980508      *
049800980506     C                   ENDSR
049900980506      *****************************************************************
050000980506      *   ROUTINE INIZIALE
050100980506      *****************************************************************
050200980506     C     *INZSR        BEGSR
050300980506      *
050400980506     C     *ENTRY        PLIST
050500980506     C                   PARM                    KPJBA
050600980506      *
050700980506     C                   Z-ADD     1             CODUT
050800980506     C                   CALL      'X§PARUT'
050900980507     C                   PARM                    UT§DSE0F
051000980506     C                   MOVEL     RAGUT         RSUT
051100980508     C                   MOVEL     REC80         CNCR80
051200980506      *
051300980507     C                   TIME                    W0140
051400980507     C                   MOVE      W0140         WDTGIO
051500980507     C                   MOVEL     W0140         WORA
051600980717      * accesso Dettaglio Anagrafica Fasi
051700980717     c     kdfd          klist
051800980717     c                   kfld                    dfafca
051900980717     c                   kfld                    tad
052000980717      * accesso tntbe
052100980717     c     KTBE          KLIST
052200980717     C                   KFLD                    CODt
052300980717      *
052400980506     C                   ENDSR
052500980506      *****************************************************************
052600980507     OPRTF198   E            TESTA             2
052700980507     O                       RSUT              +  0
052800980507     O                                         +  0 '/'
052900980507     O                       SIMFEL            +  0
053000980508     o                                           90 '***  A N A G R A F I C A'
053100980508     o                                         +  3 'F A S I  ***'
053200980508     O                                          155 'FIDN15R'
053300980508     O                       WDTGIO        Y    170
053400980507     O                                          194 'PAG.'
053500980507     O                       PAGE          Z    198
053600980507     O          E            TESTA             3
053700980507     O                       KNSIF             +  0
053800980507     O                       KNMUS             +  1
053900980508     O                       WORA               170 '  :  :  '
054000980507     O          E            TESTA             4
054100980507     O                       TRAT               198
054200980507     O          E            TESTA             5
054300980508     O                                           11 'Codice Fase'
054400980508     O                                           73 'Obbli.   Valida per'
054500980508     O                                           94 'Utilizzo      P.O.'
054600981125     O                                          117 '        Programma'
054700980508     O                                          146 'Descrizione Fase per Gesti'
054800980508     O                                          149 'one'
054900980508     O                                          192 'Cod.Testo   gg e Fase'
055000980508     O                                          198 'A'
055100980508     O          E            TESTA             6
055200980508     O                                           85 'L.Tr/P.As.   Ev/No ev.'
055300980508     O                                           97 'Abilitati'
055400980508     O                                          116 'Gestione'
055500980508     O                                          193 'Lettera    per Attesa'
055600980508     O          E            TESTA          1  7
055700980508     O                       TRAT               198
055800980508      *
055900980508     O          E            DETTA       1
056000980508     O                       DFAFCA               3
056100980508     O                       DFADES              52
056200980508     O                       DFAOBL              57
056300980508     O                       DFAVAL              68
056400980508     O                       DFAENE              80
056500980508     O                       DFAGFS              92
056600980508     O                       DFAPGM             118
056700980508     O                       DFADGC             168
056800980508     O                       DFALE1             174
056900980508     O                                          176 '-'
057000980508     O                       DFALE2             180
057100980508     O                       DFAGGA        Z    186
057200980508     O                                          188 '-'
057300980508     O                       DFAFRA             192
057400980508     O                       DFAATB             198
057500980720     O          E            DETTD       1
057600980720     O                                        +   4 'Valida per le anomalie :'
057700980720     o                       ta(1)            +   1
057800980720     o                                              '-'
057900980720     o                       ta(2)
058000980720     o                                              '-'
058100980720     o                       ta(3)
058200980720     o                                              '-'
058300980720     o                       ta(4)
058400980720     o                                              '-'
058500980720     o                       ta(5)
058600980720     o                                              '-'
058700980720     o                       ta(6)
058800980720     o                                              '-'
058900980720     o                       ta(7)
059000980720     o                                              '-'
059100980720     o                       ta(8)
059200980720     o                                              '-'
059300980720     o                       ta(9)
059400980720     o                                              '-'
059500980720     o                       ta(10)
059600980508      *
059700980508     O          E            CODA        2
059800980508     O                                           94 '***          F I N E'
059900980508     O                                          124 'S T A M P A          ***'
060000980508      *****************************************************************
060100980508** MSG  (Lungh. 78)                                                          *
060200980507Immettere Codice Fase valido                                                       1
060300980507Immettere Descrizione Fase                                                         2
060400980507Controllare scelte per utilizzo in gestione                                        3
060500980507Controllare scelte per attesa esecuzione                                           4
060600980508Eseguita la stampa richiesta                                                       5
060700990127Immissione Obbligatoria                                                            6
060800980508** KOS  (Lungh. 100)   -Disegno stampa-
060900980508Codice fase                                           Obbli.   Valida per   Utilizzo      P.O.
061000980508                                                               L.Tr/P.As.   Ev/No ev.   Abilitati
061100980508XXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX     X         X           X           X
061200980508Inter.  Programma   Descrizione Fase per Gestione                      Cod.Testo   gg e Fase     A
061300980508        Gestione                                                        Lettera    per Attesa
061400980508  X     XXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   XXX - XXX   XXX - XXX     X
