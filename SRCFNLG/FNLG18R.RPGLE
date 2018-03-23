000100060421     H DECEDIT('0,') DATEDIT(*YMD.)
000200050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300050930
000400060421     Ffnlg18d   CF   E             WORKSTN
000500060421     Ffnblp01l  IF   E           K DISK
000600060421     Ftitgc01l  IF   E           K DISK
000700050930
000800060802     D kpjba         E DS
000900060421     D fnlg18ds      E DS
001000160301     D trul27ds1     E DS
001100160301     D trulc7ds      E DS
001200150514     D fnlv59ds      E DS
001300160301     d  KS_CT                 74    118    dim(15)
001400160301     d  KS_pr                119    163    dim(15)
001500160301     d  KS_ie                164    178    dim(15)
001600060802      *ds per richiamo interrogazione tariffe giacenza
001700060802     D PARAM           DS
001800060802     D* CODICE CLIENTE
001900060802     D  PASCLI                10     16
002000060802     D* DECODIFICA CODICE TARIFFA
002100060802     D  PASDCT                17     26
002200060802     D* DESCRIZIONE TARIFFA
002300060802     D  PASDCV                27     41
002400060802     D* CODICE DIVISA
002500060802     D  VICDIV                43     45
002600060802     D* CODICE TARIFFA E PROGRESSIVO
002700060802     D  PASCTR               125    127  0
002800060802     D  PASPRG               128    130  0
002900060906      * - Ds per ricerca tariffe
003000060906     d parmta36        ds
003100060906     d  ta36ksc                6     12  0
003200060906     d  ta36ctr               13     15
003300060906     d  ta36flg               16     16
003400060906     d  ta36vpr               17     17
003500060906     d  ta36kcc               18     21  0
003600060906     d  ta36prg               37     39
003700060802      *
003800060503     D tgs             s              3  0 dim(3)
003900060503     D tts             s             11  3 dim(3)
004000060503     D sgg             s              3  0 dim(3)
004100060503     D stu             s             11  3 dim(3)
004200060503     D stt             s             11  3 dim(3)
004300061106      * P.O. gestibili dall'utente
004400061106     d fig             s              3    dim(250) inz(*Zeros)
004500061106
004600061106     d wabi            s                   like(UteAut)
004700061106     d Azuteds       e ds                  extname(Azute00f)
004800061106     d dDatiute      e ds
004900061106     d dLat          e ds
005000061106     d dute01        e ds
005100061106     d Tibs02ds      e ds
005200061106     d Tibs34ds      e ds
005300061106     d Trul31ds      e ds
005400050930
005500060503     Ititgc000
005600060503     I              tgcSG1                      tgs(1)
005700060503     I              tgcSG2                      tgs(2)
005800060503     I              tgcSG3                      tgs(3)
005900060503     I              tgcST1                      tts(1)
006000060503     I              tgcST2                      tts(2)
006100060503     I              tgcST3                      tts(3)
006200060302      *--------------------------------------------------------------------
006300060906     c                   if        damenu = *blank
006400060421     C     kblp          chain     fnblp01l
006500060421     c                   if        %found(fnblp01l)
006600060809     c                   z-add     blpksc        vksc
006700060913     c                   move      blpfin        vcit
006800060906     c                   endif
006900060906     c                   endif
007000060503     c                   do        *hival
007100060302     c                   exsr      elabora
007200060421     c                   exfmt     f01
007300060503     c   kc              leave
007400061106     c                   setoff                                       33
007500061106      * verifico se utente abilitato all'interrogazione
007600061106     c                   movel     vksc          w3ksc             3
007700061106     c     w3ksc         lookup    fig                                    32
007800061106      *se non abilitato riemette il video
007900061106     c                   if        not *in32
008000061106     c                   seton                                        33
008100061106     c                   iter
008200061106     c                   endif
008300061106
008400060802      *richiamo interrogazione tariffe giacenza
008500060802     c                   if        *inkn
008600060802     c                   clear                   param
008700060802     c                   move      tgcksc        pascli
008800060906     c                   z-add     vctr          pasctr
008900060802     c                   z-add     tgcprg        pasprg
009000060802     c                   move      'EUR'         vicdiv
009100060802     c                   movel     param         kpjbu
009200060802     c                   call      'TNTA15R'
009300060802     c                   parm                    kpjba
009400060802     c                   endif
009500060802
009600060503     c                   enddo
009700060906     c                   movel(p)  fnlg18ds      kpjbu
009800160301
009900160301     C                   CLEAR                   Fnlv59DS
010000160301     C                   MOVEL     'C'           ilv59tla
010100160301     C                   CALL      'FNLV59R'
010200160301     C                   PARM                    Fnlv59DS
010300160301     c*
010400160301     C                   CLEAR                   trul27ds1
010500160301     C                   MOVEL     'C'           I27TLA
010600160301     C                   CALL      'TRUL27R1'
010700160301     C                   PARM                    TRUL27DS1
010800160301     c*
010900160301     C                   CLEAR                   trulc7ds
011000160301     C                   MOVEL     'C'           Ic7TLA
011100160301     C                   CALL      'TRULC7R'
011200160301     C                   PARM                    TRULc7DS
011300160301     c
011400060302     c                   seton                                        lr
011500050930
011600060302      *___________________________________________________                  ****
011700060302     C     elabora       BEGSR
011800060302      *___________________________________________________                  ****
011900060421      *
012000060421      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
012100060906     c                   if        damenu = *blank
012200060421     C                   clear                   wtrova            1
012300160301     C                   clear                   trul27ds1
012400060421     C                   move      blpctr        I27ctb
012500060421     C                   move      blptsp        I27tsp
012600060421     C                   move      blplna        I27lna
012700060421     C                   move      blplnp        I27lnp
012800060809     C                   move      vksc          I27cli
012900160301     C                   CALL      'TRUL27R1'
013000160301     C                   parm                    trul27ds1
013100060809     C                   MOVE      vksc          §quatr            4 0
013200160301     c
013300160301     C                   clear                   fnlv59ds
013400060502     C* Se cliente cui fatturare è 8888 prendo tariffa Std.
013500160301    1C     §quatr        ifne      *all'8'
013600060421     C     §quatr        andne     *all'9'
013700060421     C     §quatr        andne     *zeros
013800060421     C                   move      blpctr        codctr
013900150514     C                   move      blpctr        ilv59ctr
014000150514     C                   moveL     ' '           ilv59tbo
014100160301     c* uso sempre la data bolla per caricare le tariffe
014200160301     C                   moveL     blpaas        ilv59dsp
014300160301     C                   move      blpmgs        Ilv59dsp
014400160301     c
014500160301     C*****              z-add     i18disp       ilv59dsp
014600150514     C                   move      vksc          ilv59ksc
014700150514     C                   z-add     151           ilv59kci
014800150514     C                   move      o27fie        ilv59fie
014900150514     C                   movel     blptsp        ilv59tsp
015000150514     C                   MOVEL     'S'           ilv59CON
015100150514     c                   eval      ilv59ta2='B'
015200150514     c                   eval      ilv59blo='S'
015300150514     C                   call      'FNLV59R'
015400150514     C                   parm                    fnlv59ds
015500060421     C*
015600160301    2C     olv59ERR      ifeq      *blanks
015700150514     C     olv59tks      andeq     'S'
015800060421     C*  Se ho trovato tariffa in vigore mi posiziono sul file
015900060421     C*  delle tariffe giacenze con il proggressivo giusto
016000150514     C                   move      olv59prg      kprg
016100150514     C                   MOVE      olv59ctr      codctr
016200150514     C                   MOVE      ilv59ksc      kksc
016300060421     C     Ktgc          chain     titgc01l                           31
016400160301    3C     *in31         ifeq      '0'
016500060421     C     tgcatb        andeq     *blanks
016600060421     C                   movel     'S'           wtrova
016700160301    3C                   endif
016800160301    2C                   endif
016900160301    1C                   endif
017000160301     c
017100060421     C*  Se non ho trovato tariffa cliente cerco su tar. cartello
017200160301    1C     wtrova        ifne      'S'
017300160301     c* Carico la cartello :
017400160301     c*  - per 8888 col fie della bolla
017500160301     c*  - per tariffe cliente esistente col fie della tariffa
017600160301     c                   clear                   trulc7ds
017700160301    2C     olv59ERR      ifeq      *blanks
017800160301     C     olv59tks      andeq     'S'
017900160301     c                   z-add     1             indx              3 0
018000160301     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
018100160301    3c                   if        indx>0 and %editc(olv59prg:'X')=
018200160301     c                             ks_pr(indx)
018300160301     c                   eval      ic7tfie=ks_ie(indx)
018400160301   x3c                   else
018500160302     c                   eval      ic7tntw=o27ntw
018600160301    3c                   endif
018700160301     c
018800160301   x2c                   else
018900160302     c                   eval      ic7tntw=o27ntw
019000160301    2c                   endif
019100160301     c
019200160301     C                   moveL     blpaas        Ic7dta
019300160301     C                   move      blpmgs        Ic7dta
019400160301     C                   move      blpctr        Ic7ctb
019500160301     c                   call      'TRULC7R'
019600160301     c                   parm                    trulc7ds
019700160301     c
019800060421     C*  Se ho trovato tariffa in vigore mi posiziono sul file
019900060421     C*  delle tariffe giacenze con il proggressivo giusto
020000160301     C                   move      Oc7prgc       kprg
020100160301     C                   move      Oc7ctrc       codctr
020200160301     C                   z-add     Oc7kscc       kksc
020300160301
020400060421     C     Ktgc          chain     titgc01l                           31
020500060421     C     tgcatb        ifeq      *blankS
020600060421     C  N31              movel     'S'           wtrova
020700060503     C                   endif
020800060503     C                   endif
020900060906     C*da video
021000060906     c                   else
021100060906      * richiamo da menu cerca tariffa tramite pgm tnta36
021200060906     c                   if        vksc = 0 or vksc <> exksc
021300060906     c                   Clear                   parmta36
021400060906     c                   Eval      ta36ksc = vksc
021500060906     c                   Eval      exksc = vksc
021600060906     c                   Eval      ta36flg = '1'
021700060906     c                   Eval      ta36vpr = '1'
021800060906     c                   Eval      ta36kcc = 151
021900060906     c                   Eval      kpjbu = parmta36
022000060906     c                   Call      'TNTA36R'
022100060906     c                   Parm                    kpjba
022200060906     c                   Eval      parmta36 = kpjbu
022300060906      *non trovato errore
022400060906     c                   setoff                                       91
022500060906     c                   if        ta36ctr = *blank
022600060906     c                   seton                                        91
022700060906     c                   goto      endrut
022800060906     c                   endif
022900060906      *
023000060913     C                   move      ta36ctr       vctr
023100060906     C                   move      ta36prg       kprg
023200060906     C                   move      ta36ctr       codctr
023300060906     C                   z-add     ta36ksc       kksc
023400060906     C     Ktgc          chain     titgc01l                           31
023500060906     C     tgcatb        ifeq      *blankS
023600060906     C  N31              movel     'S'           wtrova
023700060906     C                   endif
023800060906     c                   endif
023900060906      *
024000060906     c                   endif
024100060906      *
024200060503     C                   z-add     0             xp                1 0
024300060503     C                   move      *zeros        sgg
024400060503     C                   move      *zeros        stu
024500060503     C                   move      *zeros        stt
024600060503     C*
024700060503     C                   z-add     vgior         §ggpar            3 0
024800060503     C                   do        3             x                 1 0
024900060503     C     vgior         ifgt      tgs(x)
025000060503     C     vgior         sub       tgs(X)        §ggpar
025100060503     C     x             ifeq      1
025200060503     C                   z-add     tgs(x)        sgg(x)
025300060503     C                   else
025400060503     C     tgs(X)        sub       tgs(xp)       sgg(x)
025500060503     C                   endif
025600060503     C                   z-add     tts(x)        stu(x)
025700060503     C                   else
025800060503     C                   z-add     §ggpar        sgg(x)
025900060503     C                   z-add     tts(x)        stu(x)
026000060503     C                   z-add     0             §ggpar
026100060503     C                   endif
026200060503     C                   add       1             xp
026300060503     C                   enddo
026400060421      * calcolo peso tassabile
026500060906     c                   if        damenu = *blank
026600060421     C     tgcrpv        mult      blpvlf        §pesvo           13 3
026700060421     C     §pesvo        mult      100           §pesvo
026800060421     C     §pesvo        ifgt      blppkf
026900060421     C     §pesvo        div       100           §pes§            13 3
027000060421     C                   else
027100060421     C     blppkf        div       100           §pes§
027200060503     C                   endif
027300060906      * da video
027400060906     c                   else
027500060906     C     tgcrpv        mult      vvol          §pesvo
027600060906     C     §pesvo        mult      100           §pesvo
027700060906     C     §pesvo        ifgt      v§pes§
027800060906     C     §pesvo        div       100           §pes§
027900060906     C                   else
028000060906     C                   z-add     v§pes§        §pes§
028100060906     C                   endif
028200060906     c                   endif
028300060421     C     §pes§         add       0,99          §pesca            6 0
028400060503     C     sgg           mult      stu           stt
028500060503     C     stt           mult      §pesca        stt
028600060503     C                   xfoot     stt           vtsgs
028700060503     C* MINIMO TASSABILE se maggiore di spese di sosta
028800060503     C     vtsgs         iflt      tgcstm
028900060503     C                   z-add     tgcstm        vtsgs
029000060503     C                   endif
029100060421     C* SPESE RICONSEGNA
029200060913 5   C     vcit          ifne      'C'
029300110623 5   C     vcit          andne     'T'
029400060421     C                   z-add     tgcsgp        §sgr             13 3
029500060421x5   C                   else
029600060421     C                   z-add     tgcsgr        §sgr
029700060503e5   C                   endif
029800060421
029900060421     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
030000060421     C     §sgr          mult      §pesca        §sgr
030100060503     C     §sgr          add       tgcsgv        §sgr
030200060503     C     §sgr          mult      vric          vsgr
030300060421     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
030400060503     C     tgcsgd        mult      vape          vsgd
030500060801     c                   z-add     tgcrpv        vrpv
030600060906     c                   if        damenu = *blank
030700060801     c                   z-add     blpvlf        vvol
030800060906     c                   z-add     §pes§         v§pes§
030900060906     c                   endif
031000060809      * verifica applicazione franchigia se giorni da calcolare superano la
031100060809      * franchigia non la applico altrimenti seguo le regole del flag
031200060809     c                   if        tgcsgf >= vgior
031300060809     c                   select
031400060809      * sosta
031500060809     c                   when      tgcfaf = '1'
031600060809     c                   z-add     0             vtsgs
031700060809      * tutte
031800060809     c                   when      tgcfaf = '2'
031900060809     c                   z-add     0             vtsgs
032000060809     c                   z-add     0             vsgd
032100060809     c                   z-add     0             vsgr
032200060809      * sosta e dossier
032300060809     c                   when      tgcfaf = '3'
032400060809     c                   z-add     0             vtsgs
032500060809     c                   z-add     0             vsgd
032600100714     C     tgcsgv        mult      vric          comovar          13 3
032700100714     c                   sub       comovar       vsgr
032800060809     c
032900060809     c                   endsl
033000060809     c                   endif
033100060421     C*
033200060906     C     endrut        endsr
033300060302      *___________________________________________________                  ****
033400060421     C     *inzsr        begsr
033500060302      *___________________________________________________                  ****
033600060421     C     *entry        plist
033700060906     C                   parm                    kpjba
033800061106      * se richiamato da menu non ho parametri
033900061106     c                   if        kpjbu = *blank
034000061106     c                   clear                   fnlg18ds
034100061106     c                   move      '1'           damenu            1
034200061106     c                   else
034300061106      * 90*on protegge campi se chiamato da pgm e non si variano dati bolla
034400061106     c                   seton                                        90
034500061106     c                   clear                   damenu
034600061106     C                   movel(p)  kpjbu         fnlg18ds
034700061106     c                   endif
034800061106      *definizione utente e verifica autorizzazione visualizzazione tariffe
034900061106     c     *dtaara       define    §azute        azuteds
035000061106     c     *dtaara       define    §datiute      ddatiute
035100061106     c                   in(E)     *dtaara
035200061106     c                   If        %error  or RSUT = *blanks
035300061106     c                   Clear                   Tibs34ds
035400061106     c                   Call      'TIBS34R'
035500061106     c                   Parm                    Tibs34ds
035600061106     c                   In        *dtaara
035700061106     c                   EndIf
035800061106
035900061106     c                   Clear                   wabi
036000061106     c                   Clear                   dLat
036100061106
036200061106      * Verifica errori e autorità profilo
036300061106s   1c                   Select
036400061106      * se ho errori nei dati utente esco dal pgm
036500061106w   1c                   When      DutErr = 'E'
036600061106     c                   GoTo      EndAbi
036700061106      * se non c'è l'abilitazione
036800061106      * --> se 1° livello, abilitazioni al terminal
036900061106      *     se 2° livello, abilitazioni al punto operativo
037000061106      *     se sede è impossibile ma se capita mando a fine il pgm
037100061106w   1c                   When      UteAut = *Blanks
037200061106if  2c                   If        DutLpo = '1'
037300061106     c                   Eval      wabi   = 'TP'
037400061106e   2c                   EndIf
037500061106if  2c                   If        DutLpo = '2'
037600061106     c                   Eval      wabi   = 'PO'
037700061106e   2c                   EndIf
037800061106if  2c                   If        DutLpo = 'S'
037900061106     c                   GoTo      EndAbi
038000061106e   2c                   EndIf
038100061106      * carica le abilitazioni del profilo
038200061106x   1c                   Other
038300061106     c                   Movel     UteFaf        Dute01
038400061106     c                   If        §UteGtc <> *Blanks
038500061106     c                   Eval      wabi = §UteGtc
038600061106     c                   Else
038700061106     c                   Eval      wabi = UteAut
038800061106     c                   EndIf
038900061106e   1c                   EndSl
039000061106
039100061106      * controllo se ok l'abilitazione dell'utente
039200061106     c                   Clear                   Tibs02ds
039300061106     c                   Eval      T02Mod = 'C'
039400061106     c                   Eval      T02Sif = knsif
039500061106     c                   Eval      T02Cod = 'LAT'
039600061106     c                   Movel(p)  wabi          T02Ke1
039700061106     c                   Call      'TIBS02R'
039800061106     c                   Parm                    kpjba
039900061106     c                   Parm                    Tibs02ds
040000061106if  1c                   If        T02Err = *Blanks
040100061106     c                   Eval      dLat = T02Uni
040200061106e   1c                   EndIf
040300061106      * errore o non abilitato
040400061106if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
040500061106     c                   GoTo      EndAbi
040600061106e   1c                   EndIf
040700061106
040800061106      * Se l'utente ha autorizzazione 'AZ' è di sede
040900061106      *
041000061106      * Reperimento dei P.O. gestibili dall'utente
041100061106     c                   clear                   TRUL31DS
041200061106     c                   eval      I31abi = wabi
041300061106     c                   eval      I31cdi = DUTdis
041400061106     c                   eval      I31car = DUTare
041500061106     c                   eval      I31cpo = DUTpou
041600061106     c                   call      'TRUL31R'
041700061106     c                   parm                    KPJBA
041800061106     c                   parm                    TRUL31DS
041900061106if  2c                   if        O31pog > *zeros
042000061106     c                   movea     O31pog        fig
042100061106e   2c                   endif
042200061106      * Aggiungo 888 nella sk fig x poter interrogare le tariffe di cartello
042300061106     c                   Z-add     1             xx                3 0
042400061106     c     *zeros        Lookup    fig(xx)                                30
042500061106     c                   If        *In30
042600061106     c                   Move      '888'         fig(xx)
042700061106     c                   EndIf
042800061106     c     EndAbi        tag
042900061106
043000060502      * imposta parametri dft se non passati valori nella ds
043100060421      * data disposizioni a 0 imposto udate
043200060502     c                   if        i18disp = 0
043300060421     c                   move      *date         i18disp
043400060421     c                   endif
043500060302
043600060503      * numero giorni su cui calcolare
043700060823     c                   if        i18gior = 0 and i18nsp = 0
043800060503     c                   z-add     1             vgior
043900060503     c                   else
044000060503     c                   z-add     i18gior       vgior
044100060503     c                   endif
044200060502
044300060503      * Numero aperture
044400060823     c                   if        i18ape = 0  and i18nsp = 0
044500060503     c                   z-add     1             vape
044600060503     c                   else
044700060503     c                   z-add     i18ape        vape
044800060503     c                   endif
044900060502
045000060503      * Numero riconsegne
045100060823     c                   if        i18ric = 0  and i18nsp = 0
045200060503     c                   z-add     1             vric
045300060503     c                   else
045400060503     c                   z-add     i18ric        vric
045500060503     c                   endif
045600060503
045700060421     C     kblp          klist
045800060421     C                   kfld                    i18AAS
045900060421     C                   kfld                    i18LNP
046000060421     C                   kfld                    i18NRS
046100060421     C                   kfld                    i18NSP
046200060421
046300060421     C     Ktgc          klist
046400060421     C                   kfld                    kksc              7 0
046500060421     C                   kfld                    codctr            3 0
046600060421     C                   Kfld                    kprg
046700060421     C     *like         define    tgcprg        kprg
046800060906     C     *like         define    vksc          exksc
046900060302
047000060421     C                   endsr
047100060302
