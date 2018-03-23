000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200971015     F* tibs01r * -----------------------------------------*
000300971015     F*          MANUTENZIONE TABELLE
000400000000     F*----------------------------------------------------*
000500971015     Ftntbe01l  UF A E           K DISK
000600971020     Ftntbe02l  iF   E           K DISK
000700971021     F                                     RENAME(tntbe000:tntbe002)
000800991229     Ftntbe03l  iF   E           K DISK
000900991229     F                                     RENAME(tntbe000:tntbe003)
001000971016     Fazorg01l  iF   E           K DISK
001100971016     Fkfsif01l  iF   E           K DISK
001200060427     fazlin02l  if   e           k disk
001300971015     Ftibs01d   CF   E             WORKSTN
001400971021     F                                     SFILE(bs01s02:nrr2)
001500971021     F                                     SFILE(bs01s04:nrr4)
001600090513
001700090513     d c_Minu          c                   const('abcdefghijklm+
001800090513     d                                            nopqrstuvwxyz')
001900090513     d c_Maiu          c                   const('ABCDEFGHIJKLM+
002000090513     d                                            NOPQRSTUVWXYZ')
002100090513
002200060427     D MSG             S             78    DIM(25) CTDATA PERRCD(1)             MSG VIDEO
002300090513
002400000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002500991230     D TIBS06DS      E DS
002600991230     D KPJBA         E DS
002700000000     D  EDP                    1      3
002800060412     d dtbe01        e ds
002900971016     d* passaggio dati a ricerca sistema informativo
003000971016     D param1          DS
003100971016     D  pa1ric                 1      1
003200971016     D  pa1sif                 2     11
003300971030     D v4huni          DS
003400971030     D  v4cun1                 1     70
003500971030     D  v4cun2                71    140
003600971030     D  v4cun3               141    210
003700971030     D  v4cun4               211    256
003800971020     D tbeuni          DS
003900060412     d  wfglin               246    246
004000971020     D  wtbpgm               247    256
004100090513      *
004200090513DAGR d Status         sds
004300090513DAGR d  SDSpgm           *proc
004400090513      *
004500971016     DWDATA            S               D   DATFMT(*EUR)
004600090115     d wforza          s              1
004700971021     F*----------------------------------------------------*
004800971021      * INDICATORI
004900971021     c* 01   - off-inserimento      on-modifica
005000971021     c* 03   - duplica
005100971021     c* 04   - annullamento
005200971028     c* 10   - record tabella annullato
005300971021     c* 11   - protezione key record tabella esistenti
005400971021     c* 25   - rollup
005500971021     c* 28   - emetto campo messaggi
005600971021     c* 30/31- comodo
005700060427     c* 40/48- posizionamento cursore su campi video
005800971021     c* 70   - sfl control
005900971021     c* 71   - sfldsp
006000971021     c* 90   - riemetto videata (presente errore o fatto ?)
006100000000     C*----------------------------------------------------*
006200000000      * CHIAVI
006300971016     C*----------------------------------------------------*
006400971016     C     KTbe1         KLIST                                                  *
006500971016     C                   KFLD                    kcod                           *
006600971016     C                   KFLD                    kke1                           *
006700971016     C     KTbe2         KLIST                                                  *
006800971016     C                   KFLD                    kcod                           *
006900971016     C                   KFLD                    kke1                           *
007000971016     C                   KFLD                    kke2                           *
007100971016     C                   KFLD                    klin                           *
007200971016     C                   KFLD                    ksif                           *
007300971020     C     KTbe3         KLIST                                                  *
007400971020     C                   KFLD                    kcod                           *
007500971020     C                   KFLD                    klin                           *
007600971020     C                   KFLD                    ksif                           *
007700991229     C     KTbe33        KLIST                                                  *
007800991229     C                   KFLD                    kcod                           *
007900991229     C                   KFLD                    klin                           *
008000991229     C     KTbe4         KLIST                                                  *
008100991229     C                   KFLD                    kcod                           *
008200991229     C                   KFLD                    kapl                           *
008300991229     C                   KFLD                    ksif                           *
008400991229     C     KTbe44        KLIST                                                  *
008500991229     C                   KFLD                    kcod                           *
008600991229     C                   KFLD                    kapl                           *
008700971016     c*
008800971016     c     *like         define    tbeke1        kke1
008900971021     c     *like         define    tbeke2        kke2
009000971021     c     *like         define    tbelin        klin
009100971021     c     *like         define    tbesif        ksif
009200991229     c     *like         define    tbeapl        kapl
009300971016     c     *like         define    tbecod        kcod
009400971020     c     *like         define    tbecod        wcod
009500971020     c     *like         define    tbelin        wlin
009600971020     c     *like         define    tbesif        wsif
009700971030     c     *like         define    tbeuni        wuni
009800980626     c     *like         define    nrr4          savnr4
009900971016     C*----------------------------------------------------*
010000000000     C     *ENTRY        PLIST                                                  *
010100000000     C                   PARM                    KPJBA                          *
010200890429     C                   Z-ADD     1             CODUT
010300000000     C                   CALL      'XPARUT'
010400000000     C                   PARM                    PARUT
010500000000     C                   MOVEL     RAGUT         RSUT             20
010600090513     c                   eval      V1Tpgm = SDSpgm
010700971016     C*
010800881025?    C     INIZIO        TAG                                                    *
010900991229     c                   exsr      RICERCA
011000881025     C*
011100971015     C* nessun record caricato
011200971021     c     nrr2          ifeq      0
011300971015     c                   setoff                                       71
011400971015     c                   else
011500971015     c                   seton                                        71
011600971015     c                   endif
011700000000     C*
011800881025?    C     T1TAG         TAG                                                    *
011900971015     C                   WRITE     bs01t01                                      *
012000980626     C                   WRITE     bs01z02                                      *
012100881025?    C     C1TAG         TAG                                                    *
012200971015     C* nessuna riga da visualizzare
012300971021     C  n71              write     bs01d06                                      *
012400971015     c* sfl controlo
012500971015     C                   EXFMT     bs01c02                                      *
012600971015     c**
012700950915     C                   SETOFF                                       632840
012800971015     c* F3 = fine lavoro
012900930820     C   KC              GOTO      FINE                                         *
013000971015     c*
013100971016     C* F6= creazione tabella nuova
013200971016     c     *inkf         ifeq      *on
013300971016     C                   exsr      newttb                                       *
013400971016     c     wfine         ifeq      '1'
013500971016     c                   goto      fine
013600971016     c                   endif
013700971016     C                   GOTO      INIZIO                                       *
013800971016     c                   endif
013900971016     c* GESTIONE OPZIONI
014000971021     c                   exsr      gesop2
014100971020     c     wfine         ifeq      '1'
014200971020     c                   goto      fine
014300991229     c                   endif
014400991229     c
014500971020     c** ricarico le tabelle
014600971020     c                   goto      inizio
014700000000      *
014800881025?    C     FINE          TAG                                                    *
014900000000     C                   SETON                                        LR        *
015000991229     c*--------------------------------------------------------------------
015100991229     c*  GESTIONE RIERCA DI INDICI TABELLA
015200991229     c*--------------------------------------------------------------------
015300991229     C     RICERCA       BEGSR                                                  *
015400991229     c                   EXSR      PULSFL
015500991229
015600991229     c                   clear                   kcod
015700991229     c                   clear                   ksif
015800991229     c                   clear                   klin
015900060518     c**!!!              setoff                                       121314
016000060518     c                   setoff                                       1214
016100991229     c                   setoff                                       15
016200991229     c
016300991229    1c                   if        v2crap<>*blanks or v2crcd<>*blanks  or
016400060518     c**!!!                        v2crsi<>*blanks or v2crli<>' '
016500060518     c                             v2crsi<>*blanks
016600991229     c
016700991229     c* per codice tabella
016800991229    2c                   select
016900991229     c                   when      v2crcd<>*blANKS
017000991229     c
017100991229     c                   MOVEL     *ALL'0'       KKE1
017200991229     c                   move      v2crcd        kke1
017300991229     c     ktbe1         setll     tntbe01l
017400991229     c     kcod          reade(n)  tntbe01l                               30
017500991229     c                   seton                                        12
017600991229     c
017700991229     c** per codice applicazione
017800991229     c                   when      v2crap<>*blanks
017900991229     c                   movel     v2crap        kapl
018000991229     c                   seton                                        14
018100991229     c                   if        v2crsi<>*blanks
018200991229     c                   movel     v2crsi        ksif
018300991229     c     ktbe4         setll     tntbe03l
018400991229     c     kcod          reade     tntbe03l                               30
018500991229     c                   else
018600991229     c     ktbe44        setll     tntbe03l
018700991229     c     kcod          reade     tntbe03l                               30
018800991229     c                   endif
018900991229
019000991229     c* per lingua + eventuale s.i.
019100060518     c**!!!              when      v2crli<>*blanks
019200060518     c**!!!              movel     v2crli        klin
019300060518     c**!!!              seton                                        13
019400060518     c**!!!              if        v2crsi<>*blanks
019500060518     c**!!!              movel     v2crsi        ksif
019600060518     c**!!!ktbe3         setll     tntbe02l
019700060518     c**!!!kcod          reade     tntbe02l                               30
019800060518     c**!!!              else
019900060518     c**!!!ktbe33        setll     tntbe02l
020000060518     c**!!!kcod          reade     tntbe02l                               30
020100060518     c**!!!              endif
020200991229     c
020300991229     c                   other
020400991229     c
020500991229     c* per sistema informativo
020600991229     c                   seton                                        15
020700991229     c     kcod          setll     tntbe03l
020800991229     c     kcod          reade     tntbe03l                               30
020900991229    2c                   endsl
021000991229     c
021100991229    2c                   dow       not *in30
021200991229    3c                   if        tbeatb=' '
021300991229    4c                   if        v2crsi<>*blanks   and v2crsi=tbesif
021400991229     c                             or v2crsi=*blanks
021500991229    5c                   if        v2crap<>*blanks   and v2crap=tbeapl
021600991229     c                             or v2crap=*blanks  or *in14
021700060519    6c**!!!              if        v2crli<>*blanks   and v2crli=tbelin
021800060518     c**!!!                        or v2crli=*blanks  or *in13
021900991229     c
022000991229     c                   exsr      carrec
022100991229     c
022200060519    6c**!!!              endif
022300991229    5c                   endif
022400991229    4c                   endif
022500991229    3c                   endif
022600991229     c
022700991229     c   12kcod          reade(n)  tntbe01l                               30
022800060518     c*!!13kcod          reade     tntbe02l                               30
022900991229     c   14kcod          reade     tntbe03l                               30
023000991229     c   15kcod          reade     tntbe03l                               30
023100991229    2c                   enddo
023200991229     c
023300991229     c                   else
023400991229     c
023500991229     C*-------- CARICA SUBFILE INDICE TABELLA per codice tabella ----------*
023600991229     C*
023700991229     C     Kcod          SETLL     tntbe01l                                     *
023800991229     C     Kcod          READE(n)  tntbe01l                               30
023900991229     C*
024000991229     C     *in30         DOweq     *off
024100991229     c     tbeatb        ifeq      ' '
024200991229     c                   exsr      carrec
024300991229     c                   endif
024400991229     c**
024500991229     C     Kcod          READE(n)  tntbe01l                               30
024600991229     C                   ENDdo                                                  *
024700991229    1c                   endif
024800991229     c                   endsr
024900971015     c*--------------------------------------------------------------------
025000971015     c*  CREAZIONE DI NUOVA TABELLA
025100971015     c*--------------------------------------------------------------------
025200971016     C     newttb        BEGSR                                                  *
025300971015     c                   clear                   bs01d05
025400971016     c                   clear                   wfine             1
025500090115     c                   clear                   wforza
025600971016     c                   setoff                                       01
025700971016     c                   exsr      gesd05
025800971016     c                   endsr
025900991229     c*--------------------------------------------------------------------
026000991229     c*  Carico record di sfl
026100991229     c*--------------------------------------------------------------------
026200991229     c     CARREC        BEGSR
026300090513
026400090513      /free
026500090513         // Ricerca per contenuto nella descrizione della tabella:
026600090513         dTbe01 = TBEuni;
026700090513         if  V2Ccid <> *blank   and
026800090513             %scan( %trim( V2Ccid ) :
026900090513                    %trimr( %xlate( c_Minu : c_Maiu : §TBEdesc ) ) )
027000090513                    = *zero;
027100090513           leavesr;
027200090513         endif;
027300090513      /end-free
027400060518
027500060518      * se ricerca per tabella gestita in lingua carico solo se la tabella
027600060518      * è gestita in lingua
027700060518     c                   if        v2crli <> *blanks and wfglin = *blanks
027800060518     c                   leavesr
027900060518     c                   endif
028000060518
028100991229     C                   ADD       1             nrr2                           *
028200991229     c                   clear                   v2copz
028300991229     C                   MOVE      tbeke1        v2ccod                         *
028400991229     C                   MOVE      tbeapl        v2capl                         *
028500991229     C                   MOVEL     tbeuni        v2cdes                         *
028600991229     C                   MOVEL     tbeuni        v2huni                         *
028700060518     C                   MOVEL     tbelin        v2hlin                         *
028800991229     C                   MOVEL     tbesif        v2hsif                         *
028900991229     C                   MOVE      tbesif        v2ccsf                         *
029000060412     c                   clear                   v2cfglin
029100060412     c                   if        wfglin <> *blanks
029200060412     c                   eval      v2cfglin = 'L'
029300060412     c                   endif
029400991229     C                   MOVE      Wtbpgm        v2hpgm                         *
029500991229     C                   WRITE     bs01s02                                      *
029600991229     c                   ENDSR
029700991229     c*--------------------------------------------------------------------
029800991229     c*  pulizia sfl
029900991229     c*--------------------------------------------------------------------
030000991229     c     PULSFL        BEGSR
030100991229     C                   SETON                                        70        *
030200991229     C                   WRITE     bs01c02                                      *
030300991229     C                   SETOFF                                       70        *
030400991229     C*
030500991229     C                   Z-ADD     0             nrr2              4 0          *
030600991229     C                   Z-ADD     1             CURS2                          *
030700991229     c                   endsr
030800971016     c*--------------------------------------------------------------------
030900971016     c*  gestione foramto 5 - gestione testata tabella
031000971016     c*--------------------------------------------------------------------
031100971016     C     gesd05        BEGSR                                                  *
031200971016     c     for05         tag
031300971015     c                   write     bs01t01
031400971015     c                   exfmt     bs01d05
031500971016     c                   setoff                                       404142
031600971016     c                   setoff                                       439028
031700971020     c                   setoff                                       444546
031800060427     c                   setoff                                       4748
031900971015     c* F3= fine
032000971021     c   kc              move      '1'           wfine
032100971021     c   kc              goto      endnew
032200971015     c* F12=Ritorno
032300971015     c   kl              goto      endnew
032400971020     c* Controllo
032500971020     c                   exsr      ctrttb
032600971015     c  nkf
032700971015     cor 90              goto      for05
032800971015     c** F6=aggiornamento
032900971020     c   kf              exsr      regttb
033000971015     C     endnew        ENDSR
033100971015     c*--------------------------------------------------------------------
033200971015     c*  controllo testata tabella
033300971015     c*--------------------------------------------------------------------
033400971015     C     ctrttb        BEGSR                                                  *
033500971020     c                   movel     v5ccod        wcod
033600971020     c                   movel     v5clin        wlin
033700971020     c                   movel     v5csif        wsif
033800971016     c*******
033900971016     c*   CONTROLLI PER IMMISSIONE/ANNULLAMENTO
034000971016     c*******
034100971016     c* record tabella
034200971016     c     *in01         ifeq      *off
034300971016     c     *in04         oreq      *on
034400971020     c                   exsr      ctrdu2
034500971020     c     *in90         ifeq      *on
034600971020     c                   seton                                        40
034700971020     c                   goto      endttb
034800971020     c                   endif
034900971020     c                   endif
035000971016     c*
035100971016     C*******
035200971016     c* CONTROLLI SOLO IN IMMISSIONE/MODIFICA
035300971016     C*******
035400971016     c     *in02         ifeq      *off
035500971016     c* applicazione: qualsiasi
035600971016     c* Descrizione : obbligatoria
035700971016     c     v5cuni        ifeq      *blanks
035800971016     c                   movel     msg(2)        vidmsg
035900971016     c                   seton                                        412890
036000971016     c                   goto      endttb
036100971016     c                   endif
036200060412     c* lingua:
036300060427      * controllo l'esistenza sul file azlin
036400060427     c     v5clin        chain     azlin02l
036500060427     c                   if        not %found(azlin02l)
036600060427     c                   eval      vidmsg = msg(19)
036700060427     c                   eval      *in28 = *on
036800060427     c                   eval      *in47 = *on
036900060427     c                   eval      *in90 = *on
037000060427     c                   goto      endttb
037100060427     c                   endif
037200060412      * se immessa lingua diversa da italiano (" ") devo per forza indicare anche il flag
037300060412      * di tabella gestita in lingua
037400060427     c                   if        v5clin <> *blanks and v5cfglin = *blanks
037500060412     c                   eval      vidmsg = msg(16)
037600060412     c                   eval      *in28 = *on
037700060412     c                   eval      *in47 = *on
037800060412     c                   eval      *in90 = *on
037900060412     c                   goto      endttb
038000060412     c                   endif
038100971016     c* sistema informativo
038200980626     c                   movel     v5csif        wsif
038300980626     c                   exsr      ctrsif
038400980626     c     *in90         ifeq      *on
038500980626     c                   seton                                        43
038600980626     c                   movel     wsif          v5csif
038700980626     c                   goto      endttb
038800980626     c                   endif
038900971016     c*
039000971016     c* punto operativo trasmissione
039100971016     c                   clear                   v5dflt
039200971016     c     v5cflt        ifgt      0
039300971021     c     v5cflt        chain     azorg01l                           30
039400971016     c     *in30         ifeq      *on
039500971016     c     orgfva        orne      *blanks
039600971016     c     orgfag        orne      'A'
039700971016     c     orgfag        andne     'F'
039800971016     c                   movel     msg(3)        vidmsg
039900971016     c                   seton                                        422890
040000971016     c                   goto      endttb
040100971016     c                   endif
040200971016     c                   movel     orgdes        v5dflt
040300971016     c                   endif
040400971016     c*
040500971016     c                   endif
040600980626     c*******
040700980626     c*   CONTROLLI SOLO PER IMMISSIONE
040800980626     c*******
040900980626     c* se inserimento il codice tabella non deve gia' esistere
041000980626     c     *in01         ifeq      *off
041100980626     c                   movel     'n'           wesist
041200980626     c                   exsr      ctrdu1
041300980626     c     *in90         ifeq      *on
041400980626     c                   seton                                        40
041500980626     c                   goto      endttb
041600980626     c                   endif
041700980626     c                   endif
041800971020     c**********************
041900971020     c* controlli solo in DUPLICA
042000971020     c**********************
042100980626    1c     *in03         ifeq      *on
042200980626    2c     v5cdcd        ifeq      '   '
042300971021     c                   seton                                        449028
042400971021     c                   movel     msg(7)        vidmsg
042500971021     c                   goto      endttb
042600980626    2c                   endif
042700980626     c* sistema informativo
042800980626     c                   movel     v5cdsi        wsif
042900980626     c                   exsr      ctrsif
043000980626    2c     *in90         ifeq      *on
043100980626     c                   seton                                        46
043200980626     c                   movel     wsif          v5cdsi
043300980626     c                   goto      endttb
043400980626    2c                   endif
043500060427      * lingua
043600060427     c                   if        v5cdli <> *blanks
043700060427     c                   seton                                        489028
043800060427     c                   movel     msg(20)       vidmsg
043900060427     c                   goto      endttb
044000060427     c                   endif
044100971021     c**
044200971020     c                   movel     v5cdcd        wcod
044300971020     c                   movel     v5cdli        wlin
044400971020     c                   movel     v5cdsi        wsif
044500971020     c* se inserimento il codice tabella non deve gia' esistere
044600980626     c                   movel     'n'           wesist
044700971020     c* testata tabella
044800971020     c                   exsr      ctrdu1
044900980626    2c     *in90         ifeq      *on
045000971020     c                   seton                                        44
045100971020     c                   goto      endttb
045200980626    2c                   endif
045300971020     c*
045400971020     c                   exsr      ctrdu2
045500980626    2c     *in90         ifeq      *on
045600971020     c                   seton                                        44
045700971020     c                   goto      endttb
045800980626    2c                   endif
045900971020     c* Descrizione : obbligatoria
046000980626    2c     v5cdde        ifeq      *blanks
046100971020     c                   movel     msg(2)        vidmsg
046200971020     c                   seton                                        452890
046300971020     c                   goto      endttb
046400980626    2c                   endif
046500980626    1c                   endif
046600971020     c**
046700971016     C     endttb        ENDSR
046800980626     c*--------------------------------------------------------------------
046900980626     c*  controllo sistema informativo
047000980626     c*--------------------------------------------------------------------
047100980626     c     CTRSIF        begsr
047200980626     c*
047300980626    2c     wsif          ifne      *blanks
047400980626     c     wsif          chain     kfsif01l                           30
047500980626    3c     *in30         ifeq      *on
047600980626     c     atki1         orne      *blanks
047700980626     c                   movel     msg(4)        vidmsg
047800980626     c                   seton                                        2890
047900980626    3c                   endif
048000980626    2c                   endif
048100980626     c                   endsr
048200971020     c*--------------------------------------------------------------------
048300971020     c*  controllo se testata tabella gia' presente
048400971020     c*--------------------------------------------------------------------
048500971020     c     ctrdu1        begsr
048600060209      * per prima cosa controllo che il codice tabella immesso sia di almeno 3 byte
048700060209     c                   if        %subst(wcod:3:1) = *blanks
048800060209     c                   eval      vidmsg = msg(15)
048900060209     c                   seton                                          2890
049000060209     c                   leavesr
049100060209     c                   endif
049200971020     c                   clear                   kcod
049300971020     c                   movel     *zeros        kke1
049400971020     c                   move      wcod          kke1
049500971020     c                   move      wlin          klin
049600971020     c                   move      wsif          ksif
049700971020     c                   clear                   kke2
049800971021     c     ktbe2         chain(n)  tntbe01l                           30
049900971020     c     *in30         ifeq      *off
050000971020     c     tbeatb        andeq     ' '
050100980626     c     wesist        ifeq      'n'
050200971020     c                   movel     msg(5)        vidmsg
050300971020     c                   seton                                          2890
050400971020     c                   endif
050500980626     c                   else
050600980626     c     wesist        ifeq      's'
050700980626     c                   movel     msg(14)       vidmsg
050800980626     c                   seton                                          2890
050900980626     c                   endif
051000980626     c                   endif
051100971020     c                   endsr
051200971020     c*--------------------------------------------------------------------
051300971020     c*  controllo se records tabella gia' present1
051400971020     c*--------------------------------------------------------------------
051500971020     c     ctrdu2        begsr
051600090115     c                   if        wforza=' '
051700971020     c                   movel     wcod          kcod
051800971020     c                   movel     wlin          klin
051900971020     c                   movel     wsif          ksif
052000971020     c     KTBE3         chain     tntbe02l                           30
052100971020     c     *in30         doweq     *off
052200971020     c     tbeatb        ifeq      ' '
052300090115     c                   if        not *in04
052400090115     c                   movel     msg(1)        vidmsg
052500090115     c                   eval      wforza='S'
052600090115     c                   else
052700090115     c                   movel     msg(6)        vidmsg
052800090115     c                   endif
052900090115     c
053000971020     c                   seton                                        289030
053100971020     c                   else
053200971020     c     ktbe3         reade     tntbe02l                               30
053300971020     c                   endif
053400971020     c                   enddo
053500090115
053600090115     c                   endif
053700971020     c                   endsr
053800971016     c*--------------------------------------------------------------------
053900971016     c*  registro  testata tabella
054000971016     c*--------------------------------------------------------------------
054100971016     C     regttb        BEGSR                                                  *
054200971020     c* duplica : duplico prima le righe di dettaglio
054300971020     c     *in03         ifeq      *on
054400971020     c                   move      v5ccod        kcod
054500971020     c                   move      v5csif        ksif
054600971020     c                   move      v5clin        klin
054700971020     c     ktbe3         chain     tntbe02l                           31
054800971020     c     *in31         doweq     *off
054900971020     c     tbeatb        ifeq      ' '
055000971020     c                   move      v5cdcd        tbecod
055100971020     c                   move      v5cdsi        tbesif
055200971020     c                   move      v5cdli        tbelin
055300981203     c                   clear                   tbeftr
055400971020     c                   write     tntbe000
055500971020     c                   endif
055600971020     c**
055700971020     c     ktbe3         reade     tntbe02l                               31
055800971020     c                   enddo
055900971020     c                   endif
056000971020     c**
056100971020     c                   clear                   kcod
056200971020     c                   clear                   kke2
056300971020     c                   movel     *zeros        kke1
056400971021     c* no duplica
056500971021     c     *in03         ifeq      *off
056600971021     c                   move      v5ccod        kke1
056700971021     c                   move      v5csif        ksif
056800971021     c                   move      v5clin        klin
056900971016     c     ktbe2         chain     tntbe01l                           31
057000971021     c                   else
057100971021     c* per duplica controllo se esiste il codice che devo creare
057200971021     c                   move      v5cdcd        kke1
057300971021     c                   move      v5cdsi        ksif
057400971021     c                   move      v5cdli        klin
057500971021     c     ktbe2         chain     tntbe01l                           31
057600971021     c                   endif
057700971021     c**
057800990330     c  n31              movel     tbedtr        wdtr              8 0
057900971020     c                   clear                   tntbe000
058000971020     c                   movel     *zeros        tbeke1
058100971020     c* no duplica
058200971020     c     *in03         ifeq      *off
058300971016     c                   movel     v5csif        tbesif
058400971016     c                   movel     v5clin        tbelin
058500971020     c                   move      v5ccod        tbeke1
058600060412     c**!!!              movel     v5cuni        tbeuni
058700060412     c**!!!              move      v5cpgm        tbeuni
058800060412     c                   clear                   dtbe01
058900060412     c                   eval      §tbedesc = v5cuni
059000060412     c                   eval      §tbefglin = v5cfglin
059100060412     c                   eval      §tbepgm = v5cpgm
059200060412     c                   eval      tbeuni = dtbe01
059300971020     c                   else
059400971020     c*    duplica
059500971020     c                   movel     v5cdsi        tbesif
059600971020     c                   movel     v5cdli        tbelin
059700971020     c                   move      v5cdcd        tbeke1
059800971020     c                   clear                   tbeuni
059900971020     c                   movel     v5cdde        tbeuni
060000971020     c                   endif
060100971020     c**
060200971016     c                   movel     v5capl        tbeapl
060300971016     c                   movel     v5cftt        tbeftt
060400971016     c                   movel     v5cflt        tbeflt
060500971020     c* annullamento
060600971020     c   04              movel     'A'           tbeatb
060700971020     c* immissione /modifica
060800971016     c   31              write     tntbe000
060900990330     c  n31              movel     wdtr          tbedtr
061000971016     c  n31              update    tntbe000
061100971020     c*
061200971016     c
061300971016     c                   endsr
061400971016     c*--------------------------------------------------------------------
061500971016     c*  gestione delle opzioni
061600971016     c*--------------------------------------------------------------------
061700971021     C     gesop2        BEGSR                                                  *
061800971028     c* per 71 on non leggo perche' non ci sono record
061900971021     c   71              readc     bs01s02                                30
062000971021     c   71*in30         doweq     *off
062100971016     c* imposto la key
062200971016     c                   movel     *zeros        kke1
062300971016     c                   move      v2ccod        kke1
062400060518     c                   move      v2hlin        klin
062500971016     c                   movel     v2hsif        ksif
062600971016     c                   clear                   kke2
062700971016     c                   clear                   kcod
062800971020     c** 2-modifica  3-duplica   4-annullamento
062900971016     c     v2copz        ifeq      '2'
063000971016     c     v2copz        oreq      '3'
063100971016     c     v2copz        oreq      '4'
063200971016     c                   exsr      modttb
063300971016     c                   endif
063400971020     c** P-richiamo pgm di manutenzione
063500971020     c     v2copz        ifeq      'P'
063600060412      * se tabella da gestire in lingua e non c'è il programma di gestione emetto errore
063700060412     c                   if        v2cfglin <> *blanks and v2hpgm = *blanks
063800060412     c                   eval      v1cmsg = msg(18)
063900060412     c                   eval      *in28 = *on
064000060412     c                   z-add     nrr2          curs2
064100060412     c                   update    bs01s02
064200060412     c                   goto      t1tag
064300060412     c                   endif
064400100215     c                   IF        V2Hpgm = 'TNTB___R'
064500100215     c                   EVAL      kpjbu = v2ccod + v2hsif
064600100215     c                   ELSE
064700971020     c                   clear                   kpjbu
064800100215     c                   ENDIF
064900981102 AA  C                   CALL      V2Hpgm                               21
065000971020 AA  C                   PARM                    KPJBA                          *
065100971020     C                   END
065200971021     c** 1-gestione record tabella
065300971020     c     v2copz        ifeq      '1'
065400060412      * se tabella da gestire in lingua non posso fare la manutenzione generica
065500060412     c                   if        v2cfglin <> *blanks
065600060412     c                   eval      v1cmsg = msg(17)
065700060412     c                   eval      *in28 = *on
065800060412     c                   z-add     nrr2          curs2
065900060412     c                   update    bs01s02
066000060412     c                   goto      t1tag
066100060412     c                   endif
066200971020 AA  C                   exsr      carsf4
066300971020     C                   END
066400991230     c** 8-stampa tabella
066500991230     c     v2copz        ifeq      '8'
066600991230     c                   clear                   tibs06ds
066700991230     c                   eval      t06cod=v2ccod
066800060518     c                   eval      t06lin=v2hlin
066900991230     c                   eval      t06sif=v2hsif
067000991230     c                   call      'TIBS06R'
067100991230     c                   parm                    kpjba
067200991230     c                   parm                    tibs06ds
067300991230     C                   END
067400971021     c** posizionamento cursore
067500980626     c     wfine         ifeq      ' '
067600971021     c                   z-add     nrr2          curs2
067700980626     c                   seton                                        80
067800980626     c                   update    bs01s02
067900980626     c                   setoff                                       80
068000971021     c                   readc     bs01s02                                30
068100980626     c                   else
068200980626     c                   seton                                        30
068300980626     c                   endif
068400971021     C                   enddo
068500971021     c**
068600971016     c                   endsr
068700971016     c*--------------------------------------------------------------------
068800971016     c*  modifica/annullamento testata tabella
068900971016     c*--------------------------------------------------------------------
069000971016     C     modttb        BEGSR                                                  *
069100971016     c                   setoff                                       040203
069200971016     c                   seton                                        01
069300971016     c*
069400971016     c     ktbe2         chain     tntbe01l                           30
069500971016     c*
069600971016     c     *in30         ifeq      *off
069700971016     c                   movel     tbesif        v5csif
069800971016     c                   movel     tbelin        v5clin
069900971016     c                   move      tbeke1        v5ccod
070000060412     c**!!!              movel     tbeuni        v5cuni
070100060412     c**!!!              move      tbeuni        v5cpgm
070200060412     c                   eval      dtbe01 = tbeuni
070300060412     c                   eval      v5cuni = §tbedesc
070400060412     c                   eval      v5cfglin = §tbefglin
070500060412     c                   eval      v5cpgm = §tbepgm
070600971016     c                   movel     tbeftt        v5cftt
070700971016     c                   movel     tbeflt        v5cflt
070800971021     c                   movel     tbeapl        v5capl
070900971016     c     tbeflt        chain     azorg01l                           30
071000971016     c  n30              movel     orgdes        v5dflt
071100971016     c   30              clear                   v4dflt
071200971016     c                   movel     tbeftr        v5cftr
071300971021     c     tbedtr        ifgt      0
071400971016     c     *iso          movel     tbedtr        wdata
071500971016     c                   movel     wdata         v5cdtr
071600971021     c                   else
071700971021     c                   clear                   v5cdtr
071800971021     c                   endif
071900971016     c** duplica
072000971016     c     v2copz        ifeq      '3'
072100971016     c                   seton                                        0203
072200971021     c* imposto la descrizione della nuova tabella con l'attuale
072300971021     c                   movel     v5cuni        v5cdde
072400971016     c                   endif
072500971016     c** annullamento
072600971016     c     v2copz        ifeq      '4'
072700971016     c                   seton                                        0204
072800971016     c                   endif
072900971016     c**
073000971016     c                   exsr      gesd05
073100971016     c                   endif
073200971016     c                   endsr
073300971021     c*--------------------------------------------------------------------
073400971021     c*  carico sfl 4
073500971021     c*--------------------------------------------------------------------
073600971021     C     carsf4        BEGSR                                                  *
073700971021     c                   seton                                          70
073800971021     c                   write     bs01c04
073900971021     c                   setoff                                         70
074000971021     c                   seton                                          71
074100971021     c                   clear                   nrr4
074200971021     c*
074300971021     c                   movel     v2ccod        kcod
074400971021     c                   movel     v2hsif        ksif
074500060518     c                   movel     v2hlin        klin
074600971021     c* campi formato 3
074700971021     c                   movel     v2ccod        v3ccod
074800971021     c                   movel     v2hsif        v3csif
074900060518     c                   movel     v2hlin        v3clin
075000971021     c                   movel     v2capl        v3capl
075100971021     c                   movel     v2huni        v3cuni
075200971021     c     ktbe3         setll     tntbe02l
075300971021     c     ktbe3         reade     tntbe02l                               30
075400971021     c     *in30         doweq     *off
075500971021     c**
075600971021     c                   clear                   v4copz
075700971021     c                   movel     '1'           v4hi11
075800971021     c                   movel     tbeke1        v4cke1
075900971021     c                   movel     tbeke2        v4cke2
076000001115     c* se si tratta della tabella delle tabelle, memorizzo il cod
076100001115     c*  applicazione
076200001115     c                   if        tbecod=*blanks
076300001115     c                   movel     tbeapl        v4hapl
076400001115     c                   else
076500001115     c                   clear                   v4hapl
076600001115     c                   endif
076700971021     c                   movel     tbeftt        v4cftt
076800971021     c                   movel     tbeflt        v4cflt
076900971028     c                   clear                   v4dflt
077000001115     c                   if        tbeflt>0
077100971021     c     tbeflt        chain     azorg01l                           31
077200971021     c  n31              movel     orgdes        v4dflt
077300001115     c                   endif
077400971021     c                   movel     tbeftr        v4cftr
077500971030     c                   movel     tbeuni        v4cunc
077600971028     c                   movel     tbeuni        v4huni
077700971021     c     tbedtr        ifgt      0
077800971021     c     *iso          movel     tbedtr        wdata
077900971021     c                   movel     wdata         v4cdtr
078000971021     c                   else
078100971021     c                   clear                   v4cdtr
078200971021     c                   endif
078300971021     c* annullamento
078400971021     c     tbeatb        ifne      ' '
078500980624     c                   movel     'ANN'         v4cann
078600971021     c                   else
078700980624     c                   movel     '   '         v4cann
078800971021     c                   endif
078900980626     c**
079000980626     c                   exsr      gesin4
079100980626     c**
079200971021     c                   add       1             nrr4              5 0
079300971021     c                   write     bs01s04
079400971021     c**
079500971021     c     ktbe3         reade     tntbe02l                               30
079600971021     c                   enddo
079700971021     c* vedo quanti record mi mancano per caricare l'intera videata
079800971021     c     nrr4          div       7             w0050             5 0
079900971021     c                   mvr                     resto             1 0
080000971021     c                   add       1             resto
080100971021     c                   setoff                                       1110
080200971021     c**
080300971021     c     resto         do        7
080400971021     c                   clear                   bs01s04
080500971021     c                   add       1             nrr4
080600971021     c                   write     bs01s04
080700971021     c                   enddo
080800971021     c                   z-add     1             curs4
080900980626     c                   z-add     nrr4          savnr4
081000971021     c* emetto sf4
081100971021     c     for04         tag
081200971021     c                   write     bs01t01
081300971021     c                   write     bs01d03
081400971021     c                   write     bs01z04
081500971021     c                   exfmt     bs01c04
081600971027     c                   setoff                                       289040
081700971027     c                   setoff                                       4142
081800980624     c                   clear                   wf06
081900971021     c* F3=Fine
082000971021     c   kc              movel     '1'           wfine
082100971021     c   kc              goto      endsf4
082200971021     c* F12=ritorno
082300971021     c   kl              goto      endsf4
082400980626     c* ROLLUP
082500980626     c     *in25         ifeq      *on
082600980626     c                   z-add     savnr4        nrr4
082700980626     c                   do        7
082800980626     c                   clear                   bs01s04
082900980626     c                   add       1             nrr4
083000980626     c                   write     bs01s04
083100980626     c                   enddo
083200980626     c     savnr4        add       1             curs4
083300980626     c                   z-add     nrr4          savnr4
083400980626     c                   goto      for04
083500980626     c                   endif
083600980624     c* memorizzo se premuto f6
083700980624     c   kf              movel     '1'           wf06              1
083800980624     c* controllo se immesse delle opzioni
083900971028     c                   exsr      gesop4
084000980624     c
084100980626     c     wfine         ifeq      ' '
084200980624     c     *in90         ifeq      *on
084300980624     c     wf06          oreq      ' '
084400980624     c                   goto      for04
084500980624     c                   endif
084600971027     c* aggiornamento
084700971027     c                   exsr      regrtb
084800980626     c                   endif
084900971021     c     endsf4        endsr
085000971030     c*--------------------------------------------------------------------
085100971030     c*  controllo record di tabella inseriti o modificati in sfl
085200971030     c*--------------------------------------------------------------------
085300971030     C     ctrrtb        BEGSR                                                  *
085400971030     c**
085500971030     c* una delle 2 key deve essere piena
085600980624    1c     v4cke1        ifeq      *blanks
085700971030     c     v4cke2        andeq     *blanks
085800971030     c     wuni          andne     *blanks
085900971030     c                   movel     msg(9)        vidmsg
086000971030     c                   seton                                        412890
086100971030     c                   goto      endrtb
086200980624    1c                   endif
086300971030     c**
086400971030     c** non devo inserire un record gia' esistente
086500980624    1c     v4hi11        ifeq      ' '
086600971030     c                   movel     v3ccod        kcod
086700971030     c                   movel     v3clin        klin
086800971030     c                   movel     v3csif        ksif
086900971030     c                   movel     v4cke1        kke1
087000971030     c                   movel     v4cke2        kke2
087100971030     c     ktbe2         setll     tntbe01l                               30
087200980624    2c     *In30         ifeq      *on
087300971030     c                   movel     msg(12)       vidmsg
087400971030     c                   seton                                        412890
087500971030     c                   goto      endrtb
087600980624    2c                   endif
087700980624    1c                   endif
087800971030     c** trasmissione
087900971030    2c     v4cflt        ifgt      0
088000971030    3c     v4cftt        ifeq      ' '
088100971030     c                   movel     msg(10)       vidmsg
088200971030     c                   seton                                        422890
088300971030     c                   goto      endrtb
088400971030    3c                   endif
088500971030     c**
088600971030     c     v4cflt        chain     azorg01l                           31
088700971030    3c     *in31         ifeq      *on
088800971030     c     orgfva        orne      *blanks
088900971030     c     orgfag        orne      'F'
089000971030     c     orgfag        andne     'A'
089100971030     c                   movel     msg(11)       vidmsg
089200971030     c                   seton                                        422890
089300971030     c                   goto      endrtb
089400971030    3c                   endif
089500971030     c                   movel     orgdes        v4dflt
089600971030    2c                   endif
089700980626     c**
089800980626     c** in duplica controllo i campi che voglio scrivere
089900980626     c     *in03         ifeq      *on
090000980626     c* codice tabella inesistente
090100980626     c     v7cdcd        ifeq      *blanks
090200980626     c                   movel     msg(7)        vidmsg
090300980626     c                   seton                                        442890
090400980626     c                   goto      endrtb
090500980626    3c                   endif
090600980626     c* sistema informativo
090700980626     c                   movel     v7cdsi        wsif
090800980626     c                   exsr      ctrsif
090900980626     c     *in90         ifeq      *on
091000980626     c                   seton                                        43
091100980626     c                   movel     wsif          v7cdsi
091200980626     c                   goto      endrtb
091300980626     c                   endif
091400060427      * lingua
091500060427     c                   if        v7cdli <> *blanks
091600060427     c                   movel     msg(20)       vidmsg
091700060427     c                   seton                                        482890
091800060427     c                   goto      endrtb
091900060427    3c                   endif
092000980626     c* la testata tabella deve esistere
092100980626     c                   movel     v7cdcd        wcod
092200980626     c                   movel     v7cdsi        wsif
092300980626     c                   movel     v7cdli        wlin
092400980626     c                   movel     's'           wesist            1
092500980626     c                   exsr      ctrdu1
092600980626     c**
092700980626     c     *in90         ifeq      *on
092800980626     c                   seton                                        44
092900980626     c                   goto      endrtb
093000980626     c                   endif
093100980626     c** obbligatoria la key tabella
093200980626     c     v7cdk1        ifeq      *blanks
093300980626     c     v7cdk2        andeq     *blanks
093400980626     c                   movel     msg(9)        vidmsg
093500980626     c                   seton                                        452890
093600980626     c                   goto      endrtb
093700980626    2c                   endif
093800980626     c**
093900980626     c** in record tabella non deve gia' esistere
094000980626     c                   movel     v7cdcd        kcod
094100980626     c                   movel     v7cdli        klin
094200980626     c                   movel     v7cdsi        ksif
094300980626     c                   movel     v7cdk1        kke1
094400980626     c                   movel     v7cdk2        kke2
094500980626     c     ktbe2         setll     tntbe01l                               30
094600980626    2c     *In30         ifeq      *on
094700980626     c                   movel     msg(12)       vidmsg
094800980626     c                   seton                                        452890
094900980626     c                   goto      endrtb
095000980626    2c                   endif
095100980626     c                   endif
095200971030     c**
095300971030     c     endrtb        endsr
095400971027     c*--------------------------------------------------------------------
095500971027     c*  controllo record di tabella inseriti o modificati in sfl
095600971027     c*--------------------------------------------------------------------
095700971028     C     regrtb        BEGSR                                                  *
095800980626     c                   setoff                                       030405
095900971027     c                   readc     bs01s04                                30
096000971027     c     *in30         doweq     *off
096100980626     c                   movel     v4cunc        v4huni
096200980624     c                   exsr      scrivi
096300971028     c                   readc     bs01s04                                30
096400971028     c                   enddo
096500971028     c                   endsr
096600980624     c*--------------------------------------------------------------------
096700980624     c*  scrivo record di tabella
096800980624     c*--------------------------------------------------------------------
096900980624     c     SCRIVI        begsr
097000980624     c     v4cke1        ifne      *blanks
097100980624     c     v4cke2        orne      *blanks
097200980626     c** no duplica
097300980626     c     *in03         ifeq      *off
097400980624     c                   movel     v3ccod        kcod
097500980624     c                   movel     v3clin        klin
097600980624     c                   movel     v3csif        ksif
097700980624     c                   movel     v4cke1        kke1
097800980624     c                   movel     v4cke2        kke2
097900980626     c                   else
098000980626     c                   movel     v7cdcd        kcod
098100980626     c                   movel     v7cdli        klin
098200980626     c                   movel     v7cdsi        ksif
098300980626     c                   movel     v7cdk1        kke1
098400980626     c                   movel     v7cdk2        kke2
098500980626     c                   endif
098600980624     c     ktbe2         chain     tntbe01l                           30
098700990330     c  n30              movel     tbedtr        wdtr
098800980624     c**
098900980624     c                   clear                   tntbe000
099000980626     c     *in03         ifeq      *off
099100001114     c                   movel     v3capl        tbeapl
099200001114     c                   movel     v3ccod        tbecod
099300980624     c                   movel     v3clin        tbelin
099400980624     c                   movel     v3csif        tbesif
099500980624     c                   movel     v4cke1        tbeke1
099600980624     c                   movel     v4cke2        tbeke2
099700980626     c                   else
099800980626     c                   movel     v7cdcd        tbecod
099900980626     c                   movel     v7cdli        tbelin
100000980626     c                   movel     v7cdsi        tbesif
100100980626     c                   movel     v7cdk1        tbeke1
100200980626     c                   movel     v7cdk2        tbeke2
100300980626     c                   movel     ' '           tbeatb
100400001115     c                   if        v7cdcd=v3ccod
100500001115     c                   movel     v3capl        tbeapl
100600001115     c                   endif
100700980626     c                   endif
100800001115     c* se si tratta della tabella delle tabelle, rimemorizzo il cod applic
100900001115     c                   if        tbecod=*blanks
101000001115     c                   movel     v4hapl        tbeapl
101100001115     c                   endif
101200980626     c                   movel     v4huni        tbeuni
101300980626     c                   movel     v4cftt        tbeftt
101400980626     c                   movel     v4cflt        tbeflt
101500980624     c* annullamento
101600980624     c   04              movel     'A'           tbeatb
101700980624     c* ripristino
101800980624     c   05              movel     ' '           tbeatb
101900980624     c**
102000990330     c  n30              movel     wdtr          tbedtr
102100980624     c  n30              update    tntbe000
102200980624     c   30              write     tntbe000
102300980624     c                   endif
102400980624     c                   endsr
102500971028     c*--------------------------------------------------------------------
102600971028     c*  gestione opzioni sfl 4
102700971028     c*--------------------------------------------------------------------
102800971028     C     gesop4        BEGSR                                                  *
102900971028     c                   readc     bs01s04                                30
103000980624    0c     *in30         doweq     *off
103100980624     c* sflnxtchg
103200980624     c                   seton                                        73
103300980626     c                   setoff                                       030405
103400980626     c                   setoff                                       02
103500980626     c* clear dei campi di duplica
103600980626     c                   clear                   v7cdcd
103700980626     c                   clear                   v7cdsi
103800980626     c                   clear                   v7cdli
103900980626     c                   clear                   v7cdk1
104000980626     c                   clear                   v7cdk2
104100980624     c**
104200980624     c* non controllo se e' annullato
104300980624    1c     v4cann        ifne      'ANN'
104400980624     c* per immissione
104500980624     c* impossibile impostare opzione 3   4 e 5
104600980624    2c     v4hi11        ifeq      ' '
104700980624     c**
104800980624    3c     v4copz        ifeq      '3'
104900980624     c     v4copz        oreq      '4'
105000980624     c     v4copz        oreq      '5'
105100980624     c                   movel     msg(8)        vidmsg
105200980624     c                   seton                                        402890
105300980624     c                   goto      endop4
105400980624    3c                   endif
105500980624    3c                   endif
105600980624   x1c                   else
105700980624     c* e' ammessa solo l'opzione di ripristino
105800980624    2c     v4copz        ifne      ' '
105900980624     c     v4copz        andne     '5'
106000980624     c                   movel     msg(13)       vidmsg
106100980624     c                   seton                                        402890
106200980624     c                   goto      endop4
106300980624    2c                   endif
106400980624    1c                   endif
106500980624     c**
106600980626     c                   exsr      gesin4
106700971028     c* opzioni
106800980624    1c     v4copz        ifne      ' '
106900971028     c                   select
107000971028     c     v4copz        wheneq    '3'
107100971028     c                   seton                                        0302
107200971028     c     v4copz        wheneq    '4'
107300971028     c                   seton                                        0402
107400971028     c     v4copz        wheneq    '5'
107500971028     c                   seton                                        0502
107600971028     c                   endsl
107700980626     c**
107800971030     c                   movel     v4cunc        v4huni
107900971028     c**
108000971030     c     for07         tag
108100971028     c                   write     bs01t01
108200971028     c                   write     bs01d03
108300971028     c                   exfmt     bs01d07
108400971028     c                   setoff                                       289040
108500980626     c                   setoff                                       414243
108600060427     c                   setoff                                       444548
108700971028     c* f3 - fine
108800980624    2c     *inkc         ifeq      *on
108900980619     c                   movel     '1'           wfine
109000980624   x2c                   else
109100971030     c* f12 - ritorno
109200980624    3c     *inkl         ifeq      *off
109300971028     c* controlli
109400971030     c                   movel     v4huni        wuni
109500971030     c                   exsr      ctrrtb
109600971030     c  nkf
109700971030     cor 90              goto      for07
109800980624     c* AGGIORNO TABELLA
109900980624     c                   exsr      scrivi
110000980626     c                   movel     v4huni        v4cunc
110100980624     c                   setoff                                       73
110200980624     c* se annullamento imposto ANN e accendo 10
110300980624     c   04              movel     'ANN'         v4cann
110400980624     c**
110500980624     c   05              movel     '   '         v4cann
110600980624     c**
110700980624     c                   movel     '1'           v4hi11
110800980624    3c                   endif
110900980624    2c                   endif
111000980624     c                   else
111100980624     c                   exsr      ctrrtb
111200980624    1c                   endif
111300971028     c**
111400971028     c** posizionamento cursore
111500980626     c     endop4        tag
111600980626     c** accensione indicatori
111700980626     c                   exsr      gesin4
111800980626     c**
111900971028     c                   z-add     nrr4          curs4
112000971028     c                   clear                   v4copz
112100971028     c                   update    bs01s04
112200980624     c                   seton                                        73
112300971028     c**
112400980619     c     wfine         ifeq      '1'
112500980619     c                   seton                                        30
112600980619     c                   else
112700971028     c                   readc     bs01s04                                30
112800980619     c                   endif
112900971028     C                   enddo
113000971028     c**
113100980619     c                   setoff                                       73
113200980626     c                   endsr
113300980626     c*--------------------------------------------------------------------
113400980626     c*  Accensione indicatori sfl4 e formato video 7
113500980626     c*--------------------------------------------------------------------
113600980626     c     GESIN4        begsr
113700980626     c     v4cann        ifeq      'ANN'
113800980626     c                   seton                                        10
113900980626     c                   else
114000980626     c                   setoff                                       10
114100980626     c                   endif
114200980626     c*
114300980626     c     v4hi11        ifeq      '1'
114400980626     c                   seton                                        11
114500980626     c                   else
114600980626     c                   setoff                                       11
114700980626     c                   endif
114800980626     c                   endsr
114900971016
115000971016**
115100090115Per questa tabella esistono gia' dei records.enter forza
115200971016Descrizione obbligatoria
115300971016Punto operativo trasmissione inesistente
115400971016Sistema Informativo inesistente
115500980626Testata tabella gia' presente                                                                     05
115600971016Impossibile annullare la tabella: presenti records
115700971021Immettere il codice tabella !!
115800971027Opzione non ammessa: record in immissione
115900971027Immettere almeno uno dei due campi chiave
116000980626Il P.O. trasmissione si puo' immettere solo se e' richiesta la trasmissione                       10
116100971030P.O. trasmissione errato o inesistente
116200971028Record  tabella gia' presente
116300971030Per record annullato viene accettata soltanto l'opzione di ripristino
116400980626Testata tabella inesistente                                                                       14
116500060209Attenzione è possibile che la tabella esista già su TABEL00F                                      15
116600060412Se lingua diversa da italiano impostare anche il flag di tabella gestita in lingua                16
116700060412Tabella gestita in lingua non è possibile la manutenzione generica                                17
116800060412Tabella gestita in lingua serve il programma di manutenzione                                      18
116900060427Lingua errata                                                                                     19
117000060427In copia tabella è possibile solo la lingua italiano cioè " "                                     20
