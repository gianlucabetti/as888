000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200060315      * TISIP1R *----------------------------------------------------*
000300060315      *         - Calendario Eventi Particolari
000400940915      *--------------------------------------------------------------*
000500060315     FTIsip1d   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700060327     Fazcep02l  uF a E           K DISK
000800060331     Fazcpc01l  iF   E           K DISK
000900060315     Ftabel00f  IF   E           K DISK
001000020805      *
001100020805      * DEFINIZIONE SCHIERE
001200960302     D*
001300060404     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
001400060315     d
001500040917     D KPJBA         E DS
001600060315     d Tibs02Ds      e ds
001700060315     d Azuteds       e ds                  extname(Azute00f)
001800060315     d dDatiute      e ds
001900060315     d dLat          e ds
002000060315     d dute01        e ds
002100060315     D TIBS34DS      E DS
002200060331     D trul09ds      E DS
002300060331     D  po09                  19    108    dim(30)
002400050103     d Dataiso         s               d   datfmt(*iso)
002500050103     d Dataeur         s               d   datfmt(*eur)
002600050103     d
002700060315     d wabi            s                   like(UteAut)
002800060315     d kcod            s                   Like(tblCod)
002900060315     d kkey            s                   Like(TblKey)
003000060315     d varcap          s                   Like(cepcap)
003100060315     d varloc          s                   Like(ceploc)
003200060330     d wcev            s                   Like(cepcev)
003300060330     d wdev            s                   Like(cepdev)
003400060330     d numfil          s                   Like(cepfil)
003500060331     d alffil          s              3
003600060331     d yy              s              3  0
003700060330     d wfil            s                   Like(cepfil)
003800060330     d kfil            s                   Like(cepfil)
003900060330     d kcap            s                   Like(cepcap)
004000060330     d kloc            s                   Like(ceploc)
004100060330     d knar            s                   Like(cepnar)
004200060330     d kcev            s                   Like(cepcev)
004300060331     d vartfa          s                   Like(v1ctfa)
004400060331     d varcda          s                   Like(v1ccda)
004500060331     d varcal          s                   Like(v1ccda)
004600060404     d varcev          s                   Like(cepcev)
004700050103      * Ds date statistiche
004800060315     d dS2Z          e ds
004900071211     d og148         e ds
005000060315     d tisip0ds      e ds
005100060315     D* DS PER FNLV13R - CONTROLLO DI UN CAP
005200060315     D DSLV13        E DS                  EXTNAME(FNLV13DS)
005300060315     D* DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
005400060315     D DSLV14        E DS                  EXTNAME(FNLV14DS)
005500060315     D* DS PER TISI95R - CONTROLLO DI CAP
005600060315     D DSSI95        E DS                  EXTNAME(TISI95DS)
005700060328     d
005800060328     D facilita        DS
005900060328     D  v1cnda                 1      3
006000060328     D  v1ccda                 4     12
006100060330     D  v1ccal                13     21
006200060330     D  v1ctfa                22     24
006300060330     D  v1cpo0                25     27
006400060331     D  v1cpo1                28     30
006500060330     D  v1cpo2                31     33
006600060330     D  v1cpo3                34     36
006700060330     D  v1cpo4                37     39
006800060330     D  v1cpo5                40     42
006900060330     D  v1cpo6                43     45
007000060330     D  v1cpo7                46     48
007100060330     D  v1cpo8                49     51
007200060330     D  v1cpo9                52     54
007300060330     D  pofac                 25     54    dim(10)
007400060331     d  vuoto                 55     57
007500060331     D  po2la                 58    123    dim(22)
007600060331     D  v1da01                58     60
007700060331     D  v1da02                61     63
007800060331     D  v1da03                64     66
007900060331     D  v1da04                67     69
008000060331     D  v1da05                70     72
008100060331     D  v1da06                73     75
008200060331     D  v1da07                76     78
008300060331     D  v1da08                79     81
008400060331     D  v1da09                82     84
008500060331     D  v1da10                85     87
008600060331     D  v1da11                88     90
008700060331     D  v1da12                91     93
008800060331     D  v1da13                94     96
008900060331     D  v1da14                97     99
009000060331     D  v1da15               100    102
009100060331     D  v1da16               103    105
009200060331     D  v1da17               106    108
009300060331     D  v1da18               109    111
009400060331     D  v1da19               112    114
009500060331     D  v1da20               115    117
009600060331     D  v1da21               118    120
009700060331     D  v1da22               121    123
009800040917     C**********************************************************************
009900060315     c* INDICATORI
010000060315     C**********************************************************************
010100060315     C* 01    - PROTEZIONE CAMPI VIDEATA
010200060315     C* 02    - IMMISSIONE
010300060315     C* 03    - ANNULLAMENTO
010400060315     C* 04    - MODIFICA
010500060315     C* 05    - INTERROGAZIONE
010600060404     C* 06    - evidenzia campo di evento duplicabile  <> da STD
010700050103     C**********************************************************************
010800040917     C     *ENTRY        PLIST
010900040917     C                   PARM                    KPJBA
011000060315     C                   PARM                    tisip0ds
011100060315     c
011200060315     c* Verifico se l'utente  abilitato alla gestione del calendario
011300060315     c                   exsr      CARINIZ
011400060315     c                   if        dp0err='1'
011500060315     c                   goto      fine
011600060315     c                   endif
011700060329     c
011800060329     c* Duplica
011900060329     c                   if        dp0op1='O03'
012000060329     c                   exsr      Duplica
012100060329     c                   goto      fine
012200060329     c                   endif
012300040917     c
012400040917     C     FOR01         TAG
012500000828      *
012600060315     C                   EXFMT     sip1d01
012700060315     C                   SETOFF                                       2890
012800060315     c                   setoff                                       404142
012900060315     c                   setoff                                       434445
013000060315     c                   setoff                                       464748
013100060330     c                   setoff                                       495152
013200060330     c                   setoff                                       535455
013300060330     c                   setoff                                       565758
013400060404     c                   setoff                                       5960
013500040917     C* F3 - FINE
013600060315     C                   IF        *INKC
013700060315     C                   EVAL      DP0F03='1'
013800060315     C                   GOTO      FINE
013900060315     C                   ENDIF
014000060404     c
014100060315     C* F12- RITORNO
014200060315     C                   IF        *INKl
014300060315     C                   EVAL      DP0F12='1'
014400060315     C                   GOTO      FINE
014500060315     C                   ENDIF
014600060404     c
014700060404     C* F8 - Modifica evento duplicabile (dopo il controllo di v1ccep)
014800060404     C                   IF        *INKH
014900060404     c                   if        v1cdup='  '
015000060404     c                   seton                                        479028
015100060404     C                   MOVEL     MSG(16)       V1cMSG
015200060404     c                   goto      for01
015300060404     c                   endif
015400060404     C* TEASTO FUNZIONALE non ammesso se modificato cod.evento
015500060404     c                   if        v1ccev<>varcev
015600060404     c                   seton                                        479028
015700060404     C                   MOVEL     MSG(17)       V1cMSG
015800060404     c                   goto      for01
015900060404     c                   endif
016000060404     c* per il resto cambio
016100060404     c                   if        v1cdup='SI'
016200060404     c                   eval      v1cdup='NO'
016300060404     c                   else
016400060404     c                   eval      v1cdup='SI'
016500060404     c                   endif
016600060404     c
016700060404     c                   if        *in06
016800060404     c                   eval      *in06='0'
016900060404     c                   else
017000060404     c                   eval      *in06='1'
017100060404     c                   endif
017200060404     c
017300060404     C                   GOTO      for01
017400060404     C                   ENDIF
017500060315     c
017600060315      *  CONTROLLI --> non in interrogazione
017700040917     C                   EXSR      CTRD01
017800040917     c   90              goto      for01
017900000825      *
018000060315     c* Per visualizzazione, enter esce
018100060315     c                   if        dp0op1='O05'
018200060315     c                   goto      fine
018300060315     c                   endif
018400060315     c
018500060315     c  nkf              goto      for01
018600060327     c* F06 - conferma
018700060327     c   kf              exsr      AGGIORNA
018800040917     C*
018900900515      *
019000000000     C     FINE          TAG
019100060315     C**
019200060315     C                   CLEAR                   tibs02ds
019300060315     C                   MOVEL     'C'           T02TLA
019400060315     C                   CALL      'TIBS02R'
019500060315     C                   PARM                    KPJBA
019600060315     C                   PARM                    tibs02ds
019700060315     C**
019800060315     c                   clear                   dslv13
019900060315     C                   MOVEL     'C'           I13TLA
020000060315     C                   CALL      'FNLV13R'
020100060315     C                   PARM                    KPJBA
020200060315     C                   PARM                    DSLV13
020300060315     C                   PARM                    DSSI95
020400000000     C                   SETON                                        LR
020500060329      *-----------------------------------------------------***********
020600060329      * Duplica
020700060329      *-----------------------------------------------------***********
020800060329     C     Duplica       BEGSR
020900060329     c
021000060330     C                   write     sip1dw2
021100060329     c     forw2         tag
021200060329     C                   EXFMT     sip1dv2
021300060330     c                   setoff                                       2890
021400060330     c                   setoff                                       4041
021500060329     C* F12- RITORNO
021600060329     C                   IF        *INKl
021700060329     C                   EVAL      DP0F12='1'
021800060329     C                   GOTO      Endduplica
021900060329     C                   ENDIF
022000060329     c* controlli video 2
022100060329     c                   exsr      ctrw2
022200060329     c
022300060330     c  Nkf
022400060330     cor 90              goto      forw2
022500060330     c
022600060330     c   KF              exsr      Aggiorna
022700060329     c
022800060329     c     endduplica    endsr
022900900515      *-----------------------------------------------------***********
023000060329      * CONTROLLI                                           *  CTRD01 *
023100900515      *-----------------------------------------------------***********
023200040917     C     CTRD01        BEGSR
023300060330     c
023400060330     c* Controllo codice evento
023500060330     c                   eval      wcev=v1ccev
023600060330     c                   exsr      CTRCEV
023700060330     c                   eval      v1ccev=wcev
023800060330     c   90              goto      endct1
023900060404     c
024000060404     c                   if        v1ccev<>varcev
024100060404     c                   if        §2zdep='S'
024200060404     c                   eval      v1cdup='SI'
024300060404     c                   else
024400060404     c                   eval      v1cdup='NO'
024500060404     c                   endif
024600060404     C
024700060404     c                   setoff                                       06
024800060404     c                   eval      varcev=v1ccev
024900060404     c                   endif
025000060315     c*
025100060327     c* Data evento  obbligatoria
025200060330     c                   eval      wdev=v1cdev
025300060330     c                   exsr      CTRDEV
025400060330     c                   eval      v1cdev=wdev
025500060330     c   90              goto      endct1
025600060330     c
025700041220     c*
025800060315     c* Annullamento effettuabile soltanto se la data => data corrente
025900060315    1c                   if        dp0op1='O04'   and
026000060315     c                             amgdev<datcor
026100060315     c                   seton                                        419028
026200060315     C                   MOVEL     MSG(4)        V1cMSG
026300060315     c                   goto      endct1
026400060315    1c                   endif
026500060315     c
026600060328     c* Punto operativo  e cap/località: almeno uno valido o facilitaz.
026700060328     c*  di inserimento immesse
026800060331     c                   if        (v1cfil=*zeros or v1cfil=*blanks)  and
026900060331     c                             v1cnar=*blanks and
027000060328     c                             v1cloc=*blanks and facilita=*blanks
027100060328     c                             and v1ccap=*blanks
027200060328     c                   seton                                        429028
027300060328     C                   MOVEL     MSG(5)        V1cMSG
027400060328     c                   goto      endct1
027500060328    1c                   endif
027600060328    c
027700060328     c* Punto operativo  e cap/località non mettibili insieme
027800060327     c                   clear                   wfil
027900060315    1c                   if        v1cfil<>*blanks and v1cfil<>*zeros and
028000060315     c                             (v1cnar<>*blanks or v1cloc<>*blanks
028100060328     c                              or v1ccap<>*blanks or v1cprv<>*blanks)
028200060315     c                   seton                                        429028
028300060315     C                   MOVEL     MSG(5)        V1cMSG
028400060315     c                   goto      endct1
028500060315    1c                   endif
028600060328    c
028700060328    1c                   if        v1cfil<>*blanks and v1cfil<>*zeros  and
028800060328     c                             facilita<>*blanks
028900060328     c                   seton                                        429028
029000060328     C                   MOVEL     MSG(5)        V1cMSG
029100060328     c                   goto      endct1
029200060328     c                   endif
029300060328     c
029400060328    1c                   if        (v1cnar<>*blanks or v1cloc<>*blanks
029500060328     c                             or v1ccap<>*blanks) and facilita<>*blanks
029600060328     c                   seton                                        449028
029700060328     C                   MOVEL     MSG(5)        V1cMSG
029800060328     c                   goto      endct1
029900060328     c                   endif
030000060315     c* controllo p.o.
030100060315    1c                   if        v1cfil<>*blanks and v1cfil<>*zeros
030200060330     c                   eval      alffil=v1cfil
030300060330     c                   exsr      CTRFIL
030400060330     c                   eval      v1cfil=alffil
030500060330     c                   eval      wfil  =numfil
030600060330     c   90              seton                                        42
030700060330     c   90              goto      endct1
030800060315    1C                   ENDIF
030900060315     c
031000060315     c* Nazione/cap/località
031100060315    1c                   if        v1cnar<>*blanks or v1cloc<>*blanks
031200060328     c                             or v1ccap<>*blanks or v1cprv<>*blanks
031300060315     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
031400060315     C                   CLEAR                   DSLV13
031500060315     C                   CLEAR                   DSLV14
031600060315     C                   CLEAR                   DSSI95
031700060315     C*
031800060328     c                   if        v1cloc<>*blanks
031900060315     C                   CLEAR                   I14ISO
032000070207     C                   MOVEL     amgdev        I14DRI
032100060315     c                   movel     'S'           i14cnz
032200060315     c                   movel     'XX'          i14rsc
032300060315     c                   movel     'XX'          i14ind
032400060315     c                   movel     v1cnar        e14nar
032500060315     c                   movel     v1ccap        e14cap
032600060315     c                   movel     v1cloc        e14loc
032700060315     c                   movel     v1cprv        e14prv
032800060315     C                   CALL      'FNLV14R'
032900060315     C                   PARM                    KPJBA
033000060315     C                   PARM                    DSLV14
033100060329     C* CAMPI CHE POSSO AVER IMPOSTATO
033200060329     C                   MOVEL     E14LOC        v1cloc
033300060329     C                   MOVEL     E14PRV        V1CPRv
033400060329     C                   MOVEL     E14NAR        V1Cnar
033500060329     C                   MOVEL     E14CAP        V1Ccap
033600060315     C*
033700060315     C                   MOVEL     O14ERR        WERR              1
033800060315     C                   MOVEL     O14MSG        V1cMSG
033900060315     C**
034000060315    2C     WERR          IFNE      ' '
034100060315    3C     V1cMSG        IFNE      *BLANKS
034200060315     C                   SETON                                        28
034300060315    3c                   endif
034400060315     c                   seton                                        9044
034500060315     c                   goto      endct1
034600060315    2c                   endif
034700060328    2c                   endif
034800060315     C* CONTROLLO CAP -
034900060329     c                   if        v1cloc=*blanks
035000060328     C                   MOVEL     '3'           I95TCN
035100060328     c                   else
035200060328     C                   MOVEL     '7'           I95TCN
035300060329     C                   MOVEL     'S'           I13AF1
035400060329     C                   MOVEL     'S'           I13SZ2
0355000603299/12 C                   MOVEL     'S'           I13LA3
0356000603299/12 C                   MOVEL     'S'           I13SZ3
035700060328     c                   endif
035800060331     C                   MOVEL     v1ccAP        I95CAP
035900060331     C                   MOVEL     v1cLOC        I95LOC
036000060331     C                   MOVEL     v1cPRV        I95PRV
036100060331     C                   MOVEL     v1cNAR        I95NAR
036200060315     C                   MOVEL     amgdev        I95DAT
036300060315     C                   MOVEL     'S'           I13AF0
036400061010     C                   MOVEL     ' '           I13CNV
036500060315     C                   MOVEL     'S'           I13SZV
036600060315c
036700060331    2C     v1cCAP        IFNE      VARcap
036800060331     C                   CLEAR                   wfor1
036900060315     C                   CLEAR                   AAA
037000060315     C                   CLEAR                   AAB
037100060315     C                   CLEAR                   AAD
037200060331     C                   MOVEL     v1ccap        VARCAp
037300060315    2C                   ENDIF
037400060315    c
037500060315     C* MODIFICA LA LOCALITA'
037600060331    2C     v1cLOC        IFNE      VARLOc
037700060315     C                   CLEAR                   AAA
037800060315     C                   CLEAR                   AAB
037900060315     C                   CLEAR                   AAD
038000060331     C                   MOVEL     v1cloc        VARLOc
038100060315    2C                   ENDIF
038200060315     C**
038300060331     C                   MOVEL     wfor1         E13FZV
038400060315     C                   MOVEL     AAA           E13FZ1
038500060315     C                   MOVEL     AAB           E13FZ2
038600060315     C                   MOVEL     AAD           E13FZ3
038700060315     C**
038800060315     C** CALL
038900060315     C                   CALL      'FNLV13R'
039000060315     C                   PARM                    KPJBA
039100060315     C                   PARM                    DSLV13
039200060315     C                   PARM                    DSSI95
039300060315     C*
039400060315     C                   MOVEL     O13ERR        WERR              1
039500060315     C                   MOVEL     O13MSG        v1cMSG
039600060315     C                   EXSR      CTRERR
039700060315     C**
039800060315     C* REIMPOSTO LE FORZATURE
039900060331     C                   MOVEL     E13FZV        wfor1
040000060315     C                   MOVEL     E13FZ1        AAA
040100060315     C                   MOVEL     E13FZ2        AAB
040200060315     C                   MOVEL     E13FZ3        AAD
040300060315     C**
040400060315    1C     WERR          IFNE      ' '
040500060315    2C     V1cMSG        IFNE      *BLANKS
040600060315     C                   SETON                                        28
040700060315     c                   endif
040800060315     c                   seton                                        9044
040900060315     c                   goto      endct1
041000060315     c                   endif
041100060315     c                   endif
041200060327     c
041300060330     c* In caso di modifiche o nuova immiss.verifico se esiste già il record
041400060327     c                   if        wfil<>dp0fil or
041500060327     c                             v1cnar<>dp0nar or
041600060327     c                             v1ccev<>dp0cev or
041700060327     c                             amgdev<>dp0dri or
041800060327     c                             v1ccap<>dp0cap or
041900060330     c                             v1cloc<>dp0loc or dp0op1='F06'
042000060327     c     kcep2         chain(N)  azcep02l
042100060327     c                   if        %found(azcep02l)
042200060327     c                   if        wfil>0
042300060327     C                   MOVEL     msg(9)        V1cMSG
042400060329     c                   seton                                        904228
042500060327     c                   else
042600060327     C                   MOVEL     msg(10)       V1cMSG
042700060329     c                   seton                                        904428
042800060327     c                   endif
042900060327     c                   goto      endct1
043000060327     c                   endif
043100060327     c                   endif
043200060328     c
043300060328     c* Verifico se controllare il numero di eventi inseribili
043400060327     c*  nell'anno
043500060327     c                   if        §2znep<>99
043600060327     c                   if        §2znep=0
043700060327     C                   MOVEL     msg(11)       V1cMSG
043800060327     c                   seton                                        9044
043900060327     c                   endif
044000060328     c* li conto
044100060328     c*
044200060328     c                   endif
044300060330     c*
044400060330     c                   if        v1ctfa=*blanks or v1ctfa=*zeros
044500060330     c                   clear                   po2la
044600060330     c                   clear                   vartfa
044700060330     c                   endif
044800060330     c
044900060330     c* Se nuova immissione controllo facilitazioni di inserimento se immess
045000060330    1c                   if        facilita<>*blanks
045100060330
045200060330     c
045300060331     c* Controllo cap  dal/al
045400060331     c                   if        v1cnda<>*blanks or v1ccda<>*blanks
045500060331     c                             or v1ccal<>*blanks
045600060331     c                   clear                   dssi95
045700060331     c                   clear                   dslv13
045800060331     C                   MOVEL     '3'           I95TCN
045900060331     C                   MOVEL     v1ccda        I95CAP
046000060331     C                   MOVEL     v1cnda        I95NAR
046100060331     C                   MOVEL     amgdev        I95DAT
046200060331     C                   MOVEL     ' '           I13AF0
046300061010     C                   MOVEL     ' '           I13CNV
046400060331     C                   MOVEL     'S'           I13SZV
046500060331     c
046600060331    2C     v1ccda        IFNE      VARcda
046700060331     C                   CLEAR                   wfor2
046800060331     C                   MOVEL     v1ccda        VARCda
046900060331    2C                   ENDIF
047000060331     C**
047100060331     C                   MOVEL     wfor2         E13FZV
047200060331     c
047300060331     C                   CALL      'FNLV13R'
047400060331     C                   PARM                    KPJBA
047500060331     C                   PARM                    DSLV13
047600060331     C                   PARM                    DSSI95
047700060331     c
047800060331     C* REIMPOSTO LE FORZATURE
047900060331     C                   MOVEL     E13FZV        wfor2
048000060331     C*
048100060331     c                   if        o95lid='2' or o95lid=' '
048200060331     C                   MOVEL     O13ERR        WERR              1
048300060331     C                   MOVEL     O13MSG        v1cMSG
048400060331     c
048500060331    1C     WERR          IFNE      ' '
048600060331    2C     V1cMSG        IFNE      *BLANKS
048700060331     C                   SETON                                        28
048800060331     c                   endif
048900060331     c                   seton                                        9048
049000060331     c                   goto      endct1
049100060331     c                   endif
049200060331     c                   endif
049300060331     c
049400060331     c                   clear                   dssi95
049500060331     c                   clear                   dslv13
049600060331     C                   MOVEL     '3'           I95TCN
049700060331     C                   MOVEL     v1ccal        I95CAP
049800060331     C                   MOVEL     v1cnda        I95NAR
049900060331     C                   MOVEL     amgdev        I95DAT
050000060331     C                   MOVEL     ' '           I13AF0
050100061010     C                   MOVEL     ' '           I13CNV
050200060331     C                   MOVEL     'S'           I13SZV
050300060331     c
050400060331    2C     v1ccal        IFNE      VARcal
050500060331     C                   CLEAR                   wfor3
050600060331     C                   MOVEL     v1ccal        VARCal
050700060331    2C                   ENDIF
050800060331     C**
050900060331     C                   MOVEL     wfor3         E13FZV
051000060331     c
051100060331     C                   CALL      'FNLV13R'
051200060331     C                   PARM                    KPJBA
051300060331     C                   PARM                    DSLV13
051400060331     C                   PARM                    DSSI95
051500060331     c
051600060331     C* REIMPOSTO LE FORZATURE
051700060331     C                   MOVEL     E13FZV        wfor3
051800060331     C*
051900060331     c* Se impostato cap generico, non do errore
052000060331     c*  se cap valido do gli errori
052100060331     c                   if        o95lid='2' or o95lid=' '
052200060331     C                   MOVEL     O13ERR        WERR              1
052300060331     C                   MOVEL     O13MSG        v1cMSG
052400060331     c
052500060331    1C     WERR          IFNE      ' '
052600060331    2C     V1cMSG        IFNE      *BLANKS
052700060331     C                   SETON                                        28
052800060331     c                   endif
052900060331     c                   seton                                        9049
053000060331     c                   goto      endct1
053100060331     c                   endif
053200060331     c                   endif
053300060331     c* CAL al > CAP dal
053400060331     c                   if        v1ccal<=v1ccda
053500060331     C                   MOVEL     msg(12)       V1cMSG
053600060331     c                   seton                                        904928
053700060331     c                   goto      endct1
053800060331     c                   endif
053900060331     c
054000060331     c* Il primo numero del CAP deve essere uguale per nazione
054100060331     c*  italia
054200060331     c                   if        o95nit='I'
054300060331     c                   movel     v1ccda        winizioda         1
054400060331     c                   movel     v1ccal        winizioal         1
054500060331     c                   if        winizioda<>winizioal
054600060331     C                   MOVEL     msg(14)       V1cMSG
054700060331     c                   seton                                        904928
054800060331     c                   goto      endct1
054900060331     c                   endif
055000060331     c                   endif
055100060331     c
055200060331     c                   endif
055300060331     c
055400060331     c
055500060330     c                   clear                   pienounpo         1
055600071211     c                   clear                   Tuttipo           1
055700060330    2c                   do        10            xx                3 0
055800060331    3c                   if        pofac(XX)<>*blanks and pofac(xx)<>*zeros
055900060331    cc                   eval      pienounpo='S'
056000071211     c
056100060330     c                   eval      alffil=pofac(xx)
056200071211     c* Se immesso 999 non controllo ma nessun altro p.o. inseribile
056300071211   3ac                   if        alffil='999'
056400071211     c                   eval      Tuttipo='S'
056500071211   3ac                   else
056600060330     c                   exsr      ctrfil
056700060331     c                   clear                   v1dfil
056800060330     c
056900060330     c                   eval      pofac(xx)=alffil
057000071211     c* Verifica se record doppio
057100060330    4c                   if        not *in90
057200060330     c                   movel     pofac(xx)     kfil
057300060330     c                   clear                   kcap
057400060330     c                   clear                   kloc
057500060330     c                   clear                   knar
057600060330     c                   eval      kcev=v1ccev
057700060330     c     kcep21        chain(N)  azcep02l
057800060330    5c                   if        %found(azcep02l)
057900060330     C                   MOVEL     msg(9)        V1cMSG
058000060330     c                   seton                                        9028
058100060330    5c                   endif
058200060330    4c                   endif
058300071211     c* Errore o rivisualizzazione
058400060330    4c                   if        *in90
058500060330     c                   eval      yy=xx+20
058600060330     c                   eval      *in(YY)='1'
058700060330     c                   goto      endct1
058800060330    4c                   endif
058900071211   3ac                   endif
059000060330    3c                   endif
059100060330    2c                   enddo
059200060330     c
059300060330     c* Controllo il terminal di arrivo inseribile solo se non immessi
059400060330     c*  i p.o.
059500060331    2c                   if        pienounpo='S'  and v1ctfa<>*blanks
059600060331     c                             and v1ctfa<>*zeros
059700060331     C                   MOVEL     msg(13)       V1cMSG
059800060330     c                   seton                                        902850
059900060331    2c                   endif
060000060330     c
060100060331    2c                   if        v1ctfa<>*blanks and v1ctfa<>*zeros
060200060330     c                   eval      alffil=v1ctfa
060300060330     c                   exsr      ctrfil
060400060330     c                   eval      v1ctfa=alffil
060500060331    3c                   if        *in90
060600060330     c                   seton                                        50
060700060330     c                   goto      endct1
060800060331    3c                   endif
060900060331     c
061000060330     c* visualizzo i p.o. del terminal
061100060331    3c                   if        v1ctfa<>vartfa
061200060330     c                   clear                   po2la
061300060331    3c                   endif
061400060331     c
061500060331     c* Richiamo pgm che mi carica i p.o. del terminal di arrivo
061600060331    3c                   if        po2la(1)=*blanks
061700060331     c                   clear                   trul09ds
061800060331     c                   eval      i09mod='TFA'
061900060331     c                   movel     v1ctfa        i09tfa
062000060331     c                   eval      i09dta=amgdev
062100060331     c                   call      'TRUL09R'
062200060331     c                   parm                    trul09ds
062300060331    4c                   if        o09err<>' '
062400060331     c                   movel     o09msg        v1cmsg
062500060331     c                   seton                                        289050
062600060331     c                   goto      endct1
062700060331    4c                   endif
062800060331     c
062900060331     c                   z-add     1             xx
063000060331     c                   z-add     1             yy
063100071211     c
063200060331    4c                   dow       po09(xx)>*zeros
063300071211     c* Carico solo se operativo  o 102
063400071211    5c                   if        po09(xx)<>v1ctfa
063500071211     c                   movel     po09(xx)      numfil
063600071211     c     numfil        chain     azorg01l
063700071211    6c                   if        %found(azorg01l)
063800071211     c                   movel     orgde8        og148
063900071211    7c                   if        §oglpo<>' ' or numfil=102
064000071211     c
064100060331     c                   eval      po2la(yy)=po09(xx)
064200060331     c                   add       1             yy
064300071211    7c                   endif
064400071211    6c                   endif
064500071211    5c                   endif
064600060331     c                   add       1             xx
064700060331    4c                   enddo
064800060331     c
064900060331     c                   goto      endct1
065000060331    3c                   endif
065100060331    2c                   endif
065200060331    1c                   endif
065300060315     C**
065400040917     C     ENDCT1        ENDSR
065500060330      *-----------------------------------------------------***********
065600060330      * CONTROLLo codice Evento
065700060330      *-----------------------------------------------------***********
065800060330     c     CTRcev        BEGSR
065900060330     c                   clear                   v1dcev
066000060330     c                   clear                   vvdcev
066100060330      * codice evento
066200060330     c     '?'           scan      wcev                                   90
066300060330    1c                   if        *in90
066400060330     C                   movel     '2A'          cod
066500060330     C                   CLEAR                   §KEY
066600060330     C                   CLEAR                   §des
066700060330     C                   CALL      'X§TABER'
066800060330     C                   PARM                    CODUT
066900060330     C                   PARM                    COD               2
067000060330     C                   PARM                    §KEY              8
067100060330     C                   PARM                    §DES             25
067200060330     c                   movel     §key          wcev
067300060330     c                   movel     §des          v1dcev
067400060330     c                   movel     §des          vvdcev
067500060330     c                   goto      endctcev
067600060330    1c                   endif
067700060330     c
067800060330     c                   eval      kcod='2Z'
067900060330     c                   movel(p)  wcev          kkey
068000060330     c     ktab          chain     tabel00f
068100060330    1c                   if        not %found(tabel00f) or tblflg<>' '
068200060330     c                   seton                                        409028
068300060330     C                   MOVEL     MSG(2)        V1cMSG
068400060330     c                   goto      endctcev
068500060330   x1c                   else
068600060330     c                   movel     tbluni        ds2z
068700060330    2c                   if        §2zcep=' '
068800060330     c                   seton                                        409028
068900060330     C                   MOVEL     MSG(2)        V1cMSG
069000060330     c                   goto      endctcev
069100060330    2C                   ENDIF
069200060330     c
069300060330     c                   eval      kcod='2A'
069400060330     c                   movel(p)  wcev          kkey
069500060330     c     ktab          chain     tabel00f
069600060330    2c                   if        not %found(tabel00f) or tblflg<>' '
069700060330     c                   seton                                        409028
069800060330     C                   MOVEL     MSG(2)        V1cMSG
069900060330     c                   goto      endctcev
070000060330   x2c                   else
070100060330     c                   movel     tbluni        v1dcev
070200060330     c                   movel     tbluni        vvdcev
070300060331    3c                   if        §2zfep='G'
070400060331     c                   eval      v1dtaf='GIUSTIFICA'
070500060331     C                   ELSE
070600060331     C                   EVAL      V1DTAF='ALLUNGA'
070700060331    3c                   endif
070800060331     c
070900060331    2c                   endif
071000060330    1c                   endif
071100060330     c     endctcev      ENDSR
071200060330      *-----------------------------------------------------***********
071300060330      * CONTROLLo Data   Evento
071400060330      *-----------------------------------------------------***********
071500060330     c     CTRDEV        BEGSR
071600060330    1c                   if        wdev>0
071700060331     c                   movel     wdev          w002a             2
071800060330     c     *eur          test(d)                 wdev                   30
071900060331    2c                   if        *in30  and w002a='00'
072000060330     c     *dmy          test(d)                 wdev                   30
072100060331     c                   endif
072200060331     c* Errore
072300060330    3c                   if        *in30
072400060330     c                   seton                                        419028
072500060330     C                   MOVEL     MSG(3)        V1cMSG
072600060330     c                   goto      endctdev
072700060331     c                   endif
072800060330     c
072900060331     c                   if        w002a='00'
073000060330     c     *dmy          move      wdev          dataeur
073100060330     c                   else
073200060330     c     *eur          move      wdev          dataeur
073300060330     c                   endif
073400060330     c
073500060330     c                   else
073600060330     c                   seton                                        419028
073700060330     C                   MOVEL     MSG(3)        V1cMSG
073800060330     c                   goto      endctdev
073900060330     c                   endif
074000060330     c* la trasformo in AAAAAMMGG - iso
074100060330     c                   move      dataeur       wdev
074200060330     c                   movel     dataeur       dataiso
074300060330     c     *iso          movel     dataiso       amgdev            8 0
074400060331     c
074500060331     c* L'anno può essere solo l'anno in corso o + 1
074600060331     c                   movel     datcor        annocor           4 0
074700060331     c                   movel     amgdev        annodev           4 0
074800060331     C                   SELECT
074900060331     c
075000060331     c                   WHEN      annodev=ANNOCOR
075100060331     c
075200060331     c                   WHEN      annodev<ANNOCOR
075300060331     c                   seton                                        419028
075400060331     C                   MOVEL     MSG(15)       V1cMSG
075500060331     c                   goto      endctdev
075600060331     c                   WHEN      annodev>ANNOCOR
075700060331     C                   SUB       1             ANNODEV
075800060331     c
075900060331     C                   IF        ANNODEV>ANNOCOR
076000060331     c                   seton                                        419028
076100060331     C                   MOVEL     MSG(15)       V1cMSG
076200060331     c                   goto      endctdev
076300060331     c                   endif
076400060331     c                   endsl
076500060331     c
076600060330     c
076700060330     c     endctdev      ENDSR
076800060329      *-----------------------------------------------------***********
076900060329      * CONTROLLI x deplica
077000060329      *-----------------------------------------------------***********
077100060329     C     CTRw2         BEGSR
077200060330     c* Controllo nuovo codice evento
077300060330     c                   eval      wcev=vvccev
077400060330     c                   exsr      CTRCEV
077500060330     c                   eval      vvccev=wcev
077600060330     c   90              goto      endct2
077700060404     c                   if        §2zdep='S'
077800060404     c                   eval      v1cdup='SI'
077900060404     c                   else
078000060404     c                   eval      v1cdup='NO'
078100060404     c                   endif
078200060330     c*
078300060330     c* Data evento  obbligatoria
078400060330     c                   eval      wdev=vvcdev
078500060330     c                   exsr      CTRDEV
078600060330     c                   eval      vvcdev=wdev
078700060330     c   90              goto      endct2
078800060330     c
078900060330     c* Verifico se già esiste il record
079000060330     c                   eval      kfil=dp0fil
079100060330     c                   eval      kcap=dp0cap
079200060330     c                   eval      knar=dp0nar
079300060330     c                   eval      kloc=dp0loc
079400060330     c                   eval      kcev=vvccev
079500060330     c     kcep21        chain(N)  azcep02l
079600060330     c                   if        %found(azcep02l)
079700060330     c                   if        dp0fil>0
079800060330     C                   MOVEL     msg(9)        V1cMSG
079900060330     c                   seton                                        904228
080000060330     c                   else
080100060330     C                   MOVEL     msg(10)       V1cMSG
080200060330     c                   seton                                        904428
080300060330     c                   endif
080400060330     c                   goto      endct2
080500060330     c                   endif
080600060330     c
080700060330     c*
080800060329     C     ENDCT2        ENDSR
080900060315     C*
081000060315     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
081100060315     C     CTRERR        BEGSR
081200060315     C* IMPOSTO CAMPI PASSATI
081300060315     C     O13RON        IFEQ      'S'
081400060315     C                   MOVEL     O95NAR        v1cNAR
081500060315     C                   ENDIF
081600060315     C     O13ROC        IFEQ      'S'
081700060315     C                   MOVEL     O95CAP        v1cCAP
081800060315     C                   ENDIF
081900060315     C     O13ROP        IFEQ      'S'
082000060315     C                   MOVEL     O95PRV        v1cPRV
082100060315     C                   ENDIF
082200060315     C     O13ROL        IFEQ      'S'
082300060315     C                   MOVEL     O95LOC        v1cLOC
082400060315     C                   ENDIF
082500060315     c
082600060315    2C                   ENDSR
082700951025     C**************************************************************************
082800951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
082900951025     C**************************************************************************
083000951025     C     CTRFIL        BEGSR
083100060330     c                   clear                   numfil
083200060330     c
083300060330     c     '?'           scan      alffil                                 90
083400060330    2c                   if        *in90
083500060331     C                   MOVE      *BLANKS       §des
083600060331     C                   MOVE      *BLANKS       §COD1
083700060330     C                   MOVE      *BLANKS       v1cfil
083800060330     C                   CALL      'TNSD24R'
083900060330     C                   PARM                    §COD1             3
084000060330     C                   PARM                    §TIP              1
084100060330     C                   PARM                    §DES
084200060330     C                   PARM                    §pra              3
084300060330     C                   MOVEL     §COD1         alffil
084400060330     C                   MOVEL     §DES          V1Dfil
084500060330     c                   goto      endctfil
084600060330    2c                   endif
084700060330     C*
084800060330     C                   TESTN                   alffil               30
084900060330     C                   MOVE      alffil        W001A             1
085000060330    2C     *IN30         IFEQ      *OFF
085100060330     C     W001A         ORLT      '0'
085200060330     C                   MOVEL     msg(6)        V1cMSG
085300060330     C                   SETON                                        9028
085400060330     C                   GOTO      endctfil
085500060330    2C                   ENDIF
085600060330     c
085700060330     c                   movel     alffil        numfil
085800060330
085900060330     C     numfil        CHAIN     AZORG01L                           34
086000060330    1C  N34ORGFVA        IFNE      ' '
086100060330     C     ORGFAG        ORNE      'A'
086200060330     C     ORGFAG        ANDNE     'F'
086300060330     C                   SETON                                        34
086400060330    1C                   ENDIF
086500060330    2c                   if        *in34
086600060330     C                   MOVEL     msg(6)        V1cMSG
086700060330     C                   SETON                                        9028
086800060330     C                   GOTO      endctfil
086900060330    2C                   ENDIF
087000951025     C*
087100060330     C     ENDCTFIL      ENDSR
087200050214     C**************************************************************************
087300060315     C*  Carico dati utente per vedere se abilitato alla gestione      cal
087400050214     C**************************************************************************
087500060315     C     CARiniz       BEGSR
087600060327     c     kcep          klist
087700060327     c                   kfld                    dp0dri
087800060327     c                   kfld                    dp0fil
087900060327     c                   kfld                    dp0nar
088000060327     c                   kfld                    dp0cap
088100060327     c                   kfld                    dp0loc
088200060327     c                   kfld                    dp0cev
088300060327     c
088400060328     c     kcep2         klist
088500060327     c                   kfld                    amgdev
088600060327     c                   kfld                    wfil
088700060327     c                   kfld                    v1cnar
088800060327     c                   kfld                    v1ccap
088900060327     c                   kfld                    v1cloc
089000060327     c                   kfld                    v1ccev
089100060330     c     kcep21        klist
089200060330     c                   kfld                    amgdev
089300060330     c                   kfld                    kfil
089400060330     c                   kfld                    knar
089500060330     c                   kfld                    kcap
089600060330     c                   kfld                    kloc
089700060330     c                   kfld                    kcev
089800060330     c     Ktab          Klist
089900060330     c                   Kfld                    codut             1 0
090000060330     c                   Kfld                    kCod
090100060330     c                   Kfld                    kKey
090200060331     c     kcpc          klist
090300060331     c                   kfld                    o95ver
090400060331     c                   kfld                    v1cnda
090500060331     c                   kfld                    v1ccda
090600060331     c     kcpc2         klist
090700060331     c                   kfld                    o95ver
090800060331     c                   kfld                    v1cnda
090900060331     c
091000060331     c                   z-add     1             codut
091100060327     c
091200060315     c                   clear                   dute01
091300060315      *
091400060315     c     *dtaara       define    §azute        azuteds
091500060315     c     *dtaara       define    §datiute      ddatiute
091600060315     c                   in(E)     *dtaara
091700060315    1c                   If        %error  or RSUT = *blanks
091800060315     c                   Clear                   Tibs34ds
091900060315     c                   Call      'TIBS34R'
092000060315     c                   Parm                    Tibs34ds
092100060315     c                   In        *dtaara
092200060315    1c                   EndIf
092300060404    cc                   movel     rsut          v1crsu
092400060315
092500060315     c                   Clear                   wabi
092600060315     c                   Clear                   dp0msg
092700060315     c                   Clear                   dLat
092800060315
092900060315     c* Vado avanti nei caricamenti non in interrogazione
093000060403    0c                   if        dp0op1<>'O05'
093100060315     c
093200060315      * Verifica errori e autorità profilo
093300060315s   1c                   Select
093400060315      * se ho errori nei dati utente esco dal pgm
093500060315w   1c                   When      DutErr = 'E'
093600060315     c                   GoTo      ENDINIZ
093700060315      * se non c'è l'abilitazione
093800060315      * --> se 1° livello, abilitazioni al terminal
093900060315      *     se 2° livello, abilitazioni al punto operativo
094000060315      *     se sede è impossibile ma se capita mando a fine il pgm
094100060315w   1c                   When      UteAut = *Blanks
094200060315if  2c                   If        DutLpo = '1'
094300060315     c                   Eval      wabi   = 'TP'
094400060315e   2c                   EndIf
094500060315if  2c                   If        DutLpo = '2'
094600060315     c                   Eval      wabi   = 'PO'
094700060315e   2c                   EndIf
094800060315if  2c                   If        DutLpo = 'S'
094900060315     c                   GoTo      ENDINIZ
095000060315e   2c                   EndIf
095100060315      * carica le abilitazioni del profilo
095200060315x   1c                   Other
095300060315     c                   Movel     UteFaf        Dute01
095400060315     c                   If        §Utembf <> *Blanks
095500060315     c                   Eval      wabi = §Utembf
095600060315     c                   Else
095700060315     c                   Eval      wabi = UteAut
095800060315     c                   EndIf
095900060315e   1c                   EndSl
096000060315
096100060315      * controllo se ok l'abilitazione dell'utente
096200060315     c                   Clear                   Tibs02ds
096300060315     c                   Eval      T02Mod = 'C'
096400060315     c                   Eval      T02Sif = knsif
096500060315     c                   Eval      T02Cod = 'LAT'
096600060315     c                   Movel(p)  wabi          T02Ke1
096700060315     c                   Call      'TIBS02R'
096800060315     c                   Parm                    kpjba
096900060315     c                   Parm                    Tibs02ds
097000060315if  1c                   If        T02Err = *Blanks
097100060315     c                   Eval      dLat = T02Uni
097200060315e   1c                   EndIf
097300060315     c
097400060315      * errore o non abilitato
097500060315if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
097600060315     C                   movel     msg(1)        dp0msg
097700060315     c                   eval      dp0err='1'
097800060315   x1c                   Else
097900060315     c* controllo se può gestire il calendario
098000060315    2c                   if        §utecep<>'S'
098100060315     C                   movel     msg(1)        dp0msg
098200060315     c                   eval      dp0err='1'
098300060315    2c                   endif
098400060315    1c                   endif
098500060315    0c                   endif
098600060315     c
098700060315     c* Data corrente
098800060315     c                   time                    w0140            14 0
098900060315     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
099000060315     c     *eur          movel     w0080         dataeur
099100060315     c                   movel     dataeur       dataiso
099200060315     c     *iso          movel     dataiso       datcor            8 0
099300060315     c
099400060315     C*
099500060315     C* VEDO QUALE OPZINE SCELTA
099600060315     c*
099700060315     C                   CLEAR                   V1Cfil
099800060315     C                   CLEAR                   V1Dfil
099900060331     C                   CLEAR                   V1Dtaf
100000060315     C                   CLEAR                   V1Cdev
100100060315     C                   CLEAR                   V1Ccev
100200060404     C                   CLEAR                   V1Cdup
100300060315     C                   CLEAR                   V1Dcev
100400060329     C                   CLEAR                   V2Dcev
100500060329     C                   CLEAR                   V2Duni
100600060329     C                   CLEAR                   V2Ddec
100700060329     c                   clear                   vvccev
100800060329     c                   clear                   vvdcev
100900060329     c                   clear                   vvcdev
101000060315     C                   CLEAR                   V1cnar
101100060315     C                   CLEAR                   V1dnar
101200060315     C                   CLEAR                   V1cloc
101300060315     C                   CLEAR                   V1cprv
101400060315     C                   CLEAR                   V1ccap
101500060315     C                   CLEAR                   V1cnda
101600060315     C                   CLEAR                   v1ccda
101700060315     C                   CLEAR                   v1ccal
101800060315     c                   clear                   aaa               1
101900060315     c                   clear                   aab               1
102000060315     c                   clear                   aad               1
102100060331     c                   clear                   wfor1             1
102200060331     c                   clear                   wfor2             1
102300060331     c                   clear                   wfor3             1
102400060331     c                   clear                   vartfa
102500060331     c                   clear                   varcda
102600060331     c                   clear                   varcal
102700060331     c                   clear                   varcap
102800060331     c                   clear                   varloc
102900060404     c                   clear                   varcev
103000060315     c                   setoff                                       404142
103100060315     c                   setoff                                       434445
103200060315     c                   setoff                                       464748
103300060315     c                   setoff                                       49
103400060315     c                   setoff                                       010203
103500060315     c                   setoff                                       0405
103600060315     c* Imposto i campi
103700060331     c                   if        dp0fil>0
103800060315     C                   MOVEL     DP0FIL        V1CFIL
103900060331     c                   endif
104000060315     C                   MOVEL     DP0NAR        V1CNAR
104100060315     C                   MOVEL     DP0LOC        V1CLOC
104200060315     C                   MOVEL     DP0CAP        V1CCAP
104300060315     C                   MOVEL     DP0PRV        V1CPRV
104400060315     C                   MOVEL     DP0CEV        V1CCEV
104500060404     c                   if        dp0dup<>' '
104600060404     C                   MOVEL     DP0CEV        varCEV
104700060404     c                   endif
104800060404     c                   if        dp0dup='S'
104900060404     C                   eval      V1Cdup='SI'
105000060404     c                   endif
105100060404     c                   if        dp0dup='N'
105200060404     C                   eval      V1Cdup='NO'
105300060404     c                   endif
105400060404     c
105500060315     C                   IF        DP0DRI>0
105600060315     C     *ISO          MOVEL     DP0DRI        DATAISO
105700060315     C                   MOVEL     DATAISO       DATAEUR
105800060315     C     *EUR          MOVEL     DATAEUR       V1CDEV
105900060315     C                   ENDIF
106000060315     C*
106100060315     C                   IF        DP0FIL>0
106200060315     C     DP0FIL        CHAIN     AZORG01L
106300060315     c                   if        %found(azorg01l)
106400060315     c                   movel     orgdes        v1dfil
106500060331     c                   movel     dp0fil        alffil
106600060331     c                   eval      v2duni=alffil+'-'+orgdes
106700060329     c                   eval      v2ddec='P.OPERATIVO'
106800060315     c                   endif
106900060315     c                   endif
107000060315     c
107100060327     c                   if        dp0loc<>*blanks or dp0cap<>*blanks
107200060315     c                   eval      kcod='15'
107300060315     c                   movel(p)  dp0nar        kkey
107400060315     c     ktab          chain     tabel
107500060327     c                   if        %found(tabel00f)
107600060315     c                   movel     tbluni        v1dnar
107700060315     c                   endif
107800060329     C                   EVAL      V2DDEC='NAZIONE/LOCALITA'''
107900060329     c                   eval      v2duni=DP0NAR+%subst(dp0loc:1:25)+' '
108000060329     c                             +dp0prv
108100060327     c                   endif
108200060315     c                   if        dp0cev<>*blanks
108300060315     c                   eval      kcod='2A'
108400060315     c                   movel(p)  dp0cev        kkey
108500060315     c     ktab          chain     tabel
108600060327     c                   if        %found(tabel00f)
108700060315     c                   movel     tbluni        v1dcev
108800060329     c                   movel     tbluni        v2dcev
108900060315     c                   endif
109000060404     c* chain su sa2z per prendere §2zdep
109100060404     c                   eval      kcod='2Z'
109200060404     c                   movel(p)  dp0cev        kkey
109300060404     c     ktab          chain     tabel
109400060404     c                   if        %found(tabel00f)
109500060404     c                   movel     tbluni        ds2z
109600060404     c                   else
109700060404     c                   clear                   ds2z
109800060404     c                   endif
109900060404     c
110000060404     c                   if        §2zdep<>dp0dup
110100060404     c                   seton                                        06
110200060404     c                   endif
110300060404     c
110400060315     c                   endif
110500060404     c
110600060331     c                   if        dp0taf<>' '
110700060331     c                   if        dp0taf='G'
110800060331     c                   eval      v1dtaf='GIUSTIFICA'
110900060331     C                   ELSE
111000060331     c                   eval      v1dtaf='ALLUNGA   '
111100060331     C                   ENDIF
111200060331     C                   ENDIF
111300060315     c* IMMISSIONE
111400060315     c                   select
111500060315     C                   WHEN      DP0OP1='F06'
111600060315     C                   SETON                                        02
111700060315     C* MANUTENZIONE
111800060315     C                   WHEN      DP0OP1='O02'
111900060315     C                   SETON                                        04
112000060315     C* ANNULLAMENTO
112100060315     C                   WHEN      DP0OP1='O04'
112200060315     C                   SETON                                        0301
112300060315     C* VISULIZZA
112400060315     C                   WHEN      DP0OP1='O05'
112500060315     C                   SETON                                        0501
112600060315     C                   ENDSL
112700060315     C*
112800060315     C
112900060315     C     ENDINIZ       ENDSR
113000060327     C**************************************************************************
113100060327     C*  Aggiornamento dati su azcep
113200060327     C**************************************************************************
113300060327     C     Aggiorna      BEGSR
113400060327     c* Annullamento
113500060327     c                   select
113600060328     c                   when      dp0op1='O04'
113700060327     c     kcep          delete    azcep02l
113800060327     c                   if        %error
113900060327     c                   eval      dp0msg=msg(7)
114000060327     c                   eval      dp0err='1'
114100060327     c                   endif
114200060327     c
114300060328     c                   when      dp0op1='O02'
114400060327     c* Visto che tutti i campi sono in key, annullo e riscrivo
114500060327     c     kcep          delete    azcep02l
114600060327     c                   if        %error
114700060327     c                   eval      dp0msg=msg(8)
114800060327     c                   eval      dp0err='1'
114900060327     c                   else
115000060327     c                   exsr      Scrivi
115100060327     c                   endif
115200060327     c
115300060330     c* Duplica
115400060330     c                   when      dp0op1='O03'
115500060404     c                   exsr      SCRIVI
115600060330     c* Inserimento unico
115700060330     c                   when      dp0op1='F06'  and facilita=*blanks
115800060327     c                   exsr      SCRIVI
115900060330     c* Inserimento con facilitazioni
116000060330     c                   when      dp0op1='F06'  and facilita<>*blanks
116100060330     c                   exsr      SCRIVIFAC
116200060327     c                   ENDSL
116300060327     c
116400060327     c
116500060327     c                   ENDSR
116600060327     C**************************************************************************
116700060327     C*  Scrittura nuovo record
116800060327     C**************************************************************************
116900060327     C     Scrivi        BEGSR
117000060327     c                   clear                   azcep000
117100060327     c                   eval      cepdup=dp0dup
117200060330     c                   eval      cepdev=amgdev
117300060404     c                   movel     v1cdup        cepdup
117400060330     c                   if        dp0op1='O03'
117500060330     c                   eval      cepcev=vvccev
117600060331     c                   MOVEL     v1dtaf        ceptaf
117700060330     c                   eval      cepfil=dp0fil
117800060330     c                   eval      cepnar=dp0nar
117900060330     c                   eval      ceploc=dp0loc
118000060330     c                   eval      cepprv=dp0prv
118100060330     c                   eval      cepcap=dp0cap
118200060330     c                   else
118300060327     c                   eval      cepfil=wfil
118400060327     c                   eval      cepnar=v1cnar
118500060327     c                   eval      ceploc=v1cloc
118600060327     c                   eval      cepprv=v1cprv
118700060328     c                   eval      cepcap=v1ccap
118800060330     c                   eval      cepcev=v1ccev
118900060331     c                   MOVEL     v1dtaf        ceptaf
119000060330     c                   endif
119100060327     c                   write     azcep000
119200060327     c                   endsr
119300060330     C**************************************************************************
119400060330     C*  Scrittura nuovi record con facilitazioni di inserimento
119500060330     C**************************************************************************
119600060330     C     Scrivifac     BEGSR
119700060330     c                   clear                   azcep000
119800060330     c                   eval      cepdup=dp0dup
119900060330     c                   eval      cepdev=amgdev
120000060330     c                   eval      cepcev=v1ccev
120100060404     c                   movel     v1cdup        cepdup
120200060331     c                   MOVEL     v1dtaf        ceptaf
120300060330     c
120400071211    1c                   do        10            xx
120500071211    2c                   if        pofac(xx)>*zeros
120600071211    3c                   if        pofac(xx)='999'
120700071211     c                   exsr      CreaTutti
120800071211   x3c                   else
120900060330     c                   movel     pofac(xx)     cepfil
121000061212     c                   write     azcep000                             99
121100071211    3c                   endif
121200071211    2c                   endif
121300071211    1c                   enddo
121400060331     c
121500060331     c                   if        v1ctfa>*zeros
121600060331     c                   do        22            xx
121700060331     c                   if        po2la(xx)>*zeros
121800060331     c                   movel     po2la(xx)     cepfil
121900061212     c                   write     azcep000                             99
122000060331     c                   endif
122100060331     c                   enddo
122200060331     c
122300060331     c                   movel     v1ctfa        cepfil
122400061212     c                   write     azcep000                             99
122500060331     c                   endif
122600060331     c
122700060331     c                   if        v1ccda<>*blanks
122800060331     c     kcpc          setll     azcpc01l
122900060331     c     kcpc2         reade     azcpc01l
123000060331     c                   dow       not %eof(azcpc01l) and cpccap<=v1ccal
123100060331     c
123200060331     c* Escludo i fittizi e i cap con viario e obsoleti
123300060331     c                   if        cpcflg='N' and cpcfit='N' and cpcfi1=' '
123400060331     c                   movel     cpcnar        cepnar
123500060331     c                   movel     cpccap        cepcap
123600060331     c                   movel     cpcprv        cepprv
123700061212     c                   write     azcep000                             99
123800060331     c                   endif
123900060331     c
124000060331     c     kcpc2         reade     azcpc01l
124100060331     c                   enddo
124200060331     c                   endif
124300060330     c
124400060330     c                   endsr
124500071211     C**************************************************************************
124600071211     C*  Scrittura evento per tutti i punti operativi
124700071211     C**************************************************************************
124800071211     C     Creatutti     BEGSR
124900071211     c     *loval        setll     azorg01l
125000071211     c                   read      azorg01l
125100071211     c                   dow       not %eof(azorg01l)
125200071211     c                   if        orgfva=' ' and
125300071211     c                             (orgfag='F' or orgfag='A')
125400071211     c                   movel     orgde8        og148
125500071211     c                   if        §oglpo<>' '   or orgfil=102
125600071211     c                   movel     orgfil        cepfil
125700071211     c                   write     azcep000                             99
125800071211     c                   endif
125900071211     c                   endif
126000071211     c
126100071211     c                   read      azorg01l
126200071211     c                   enddo
126300071211     c
126400071211     c                   endsr
126500020805      **************************************************************************
126600960302** SCHIERA MSG - MESSAGGI DI ERRORE
126700060328Utente non abilitato alla Gestione del Calendario Eventi particolari           01
126800060328Codice Evento errato o non utilizzabile nel calendario                         02
126900060328Data Evento errata                                                             03
127000060328Annullamento non effettuabile: data già trascorsa                              04
127100071211Immettere la Filiale   o il CAP/Località o una facilitaz.di inserimento        05
127200071211Filiale errata                                                                 06
127300060328Record al momento non annullabile: in uso!!!! Verificare e riprovare           07
127400060328Record al momento non modificabile: in uso!!!! Verificare e riprovare          08
127500060328Evento (codice+data) già presente per questo P.Operativo                       09
127600060328Evento (codice+data) già presente per questo CAP/Località                      00
127700060328Per questo codice evento sono ammessi nello stesso anno xx eventi              11
127800060331CAP  AL deve essere maggiore  del CAP  DAL                                     12
127900071211Immettere le singole Filiali  oppure il Terminal di Arrivo                     13
128000060331Per evitare errori, CAP DAL e CAP AL devono iniziare con lo stesso numero      14
128100060331Data evento DEVE essere dell'anno corrente o dell'anno prossimo                15
128200060404Tipo evento duplicabile S/N non ancora impostato: tasto funzionale non ammesso 16
128300060404F8 non ammesso: è stato modificato il codice evento                            17
