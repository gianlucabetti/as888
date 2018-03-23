000100060421     H DECEDIT('0,') DATEDIT(*YMD.)
000200050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300050930
000400060421     Ffnlg18d   CF   E             WORKSTN
000500060421     Ffnblp01l  IF   E           K DISK
000600060421     Ftitgc01l  IF   E           K DISK
000700050930
000800060802     D kpjba         E DS
000900060421     D fnlg18ds      E DS
001000060421     D trul27ds      E DS
001100150514     D fnlv59ds      E DS
001200060802      *ds per richiamo interrogazione tariffe giacenza
001300060802     D PARAM           DS
001400060802     D* CODICE CLIENTE
001500060802     D  PASCLI                10     16
001600060802     D* DECODIFICA CODICE TARIFFA
001700060802     D  PASDCT                17     26
001800060802     D* DESCRIZIONE TARIFFA
001900060802     D  PASDCV                27     41
002000060802     D* CODICE DIVISA
002100060802     D  VICDIV                43     45
002200060802     D* CODICE TARIFFA E PROGRESSIVO
002300060802     D  PASCTR               125    127  0
002400060802     D  PASPRG               128    130  0
002500060906      * - Ds per ricerca tariffe
002600060906     d parmta36        ds
002700060906     d  ta36ksc                6     12  0
002800060906     d  ta36ctr               13     15
002900060906     d  ta36flg               16     16
003000060906     d  ta36vpr               17     17
003100060906     d  ta36kcc               18     21  0
003200060906     d  ta36prg               37     39
003300060802      *
003400060503     D tgs             s              3  0 dim(3)
003500060503     D tts             s             11  3 dim(3)
003600060503     D sgg             s              3  0 dim(3)
003700060503     D stu             s             11  3 dim(3)
003800060503     D stt             s             11  3 dim(3)
003900061106      * P.O. gestibili dall'utente
004000061106     d fig             s              3    dim(250) inz(*Zeros)
004100061106
004200061106     d wabi            s                   like(UteAut)
004300061106     d Azuteds       e ds                  extname(Azute00f)
004400061106     d dDatiute      e ds
004500061106     d dLat          e ds
004600061106     d dute01        e ds
004700061106     d Tibs02ds      e ds
004800061106     d Tibs34ds      e ds
004900061106     d Trul31ds      e ds
005000050930
005100060503     Ititgc000
005200060503     I              tgcSG1                      tgs(1)
005300060503     I              tgcSG2                      tgs(2)
005400060503     I              tgcSG3                      tgs(3)
005500060503     I              tgcST1                      tts(1)
005600060503     I              tgcST2                      tts(2)
005700060503     I              tgcST3                      tts(3)
005800060302      *--------------------------------------------------------------------
005900060906     c                   if        damenu = *blank
006000060421     C     kblp          chain     fnblp01l
006100060421     c                   if        %found(fnblp01l)
006200060809     c                   z-add     blpksc        vksc
006300060913     c                   move      blpfin        vcit
006400060906     c                   endif
006500060906     c                   endif
006600060503     c                   do        *hival
006700060302     c                   exsr      elabora
006800060421     c                   exfmt     f01
006900060503     c   kc              leave
007000061106     c                   setoff                                       33
007100061106      * verifico se utente abilitato all'interrogazione
007200061106     c                   movel     vksc          w3ksc             3
007300061106     c     w3ksc         lookup    fig                                    32
007400061106      *se non abilitato riemette il video
007500061106     c                   if        not *in32
007600061106     c                   seton                                        33
007700061106     c                   iter
007800061106     c                   endif
007900061106
008000060802      *richiamo interrogazione tariffe giacenza
008100060802     c                   if        *inkn
008200060802     c                   clear                   param
008300060802     c                   move      tgcksc        pascli
008400060906     c                   z-add     vctr          pasctr
008500060802     c                   z-add     tgcprg        pasprg
008600060802     c                   move      'EUR'         vicdiv
008700060802     c                   movel     param         kpjbu
008800060802     c                   call      'TNTA15R'
008900060802     c                   parm                    kpjba
009000060802     c                   endif
009100060802
009200060503     c                   enddo
009300060906     c                   movel(p)  fnlg18ds      kpjbu
009400060302     c                   seton                                        lr
009500050930
009600060302      *___________________________________________________                  ****
009700060302     C     elabora       BEGSR
009800060302      *___________________________________________________                  ****
009900060421      *
010000060421      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
010100060906     c                   if        damenu = *blank
010200060421     C                   clear                   wtrova            1
010300060421     C                   clear                   trul27ds
010400060421     C                   move      blpctr        I27ctb
010500060421     C                   move      blptsp        I27tsp
010600060421     C                   move      blplna        I27lna
010700060421     C                   move      blplnp        I27lnp
010800060809     C                   move      vksc          I27cli
010900060421     C                   moveL     blpaas        I27dta
011000060421     C                   move      blpmgs        I27dta
011100060421     C                   CALL      'TRUL27R'
011200060421     C                   parm                    trul27ds
011300060809     C                   MOVE      vksc          §quatr            4 0
011400060502     C* Se cliente cui fatturare è 8888 prendo tariffa Std.
011500060421     C     §quatr        ifne      *all'8'
011600060421     C     §quatr        andne     *all'9'
011700060421     C     §quatr        andne     *zeros
011800060421     C                   move      blpctr        codctr
011900150514     C                   clear                   fnlv59ds
012000150514     C                   move      blpctr        ilv59ctr
012100150514     C                   moveL     ' '           ilv59tbo
012200150514     C                   z-add     i18disp       ilv59dsp
012300150514     C                   move      vksc          ilv59ksc
012400150514     C                   z-add     151           ilv59kci
012500150514     C                   move      o27fie        ilv59fie
012600150514     C                   movel     blptsp        ilv59tsp
012700150514     C                   MOVEL     'S'           ilv59CON
012800150514     c                   eval      ilv59ta2='B'
012900150514     c                   eval      ilv59blo='S'
013000150514     C                   call      'FNLV59R'
013100150514     C                   parm                    fnlv59ds
013200060421     C*
013300150514     C     olv59ERR      ifeq      *blanks
013400150514     C     olv59tks      andeq     'S'
013500060421     C*  Se ho trovato tariffa in vigore mi posiziono sul file
013600060421     C*  delle tariffe giacenze con il proggressivo giusto
013700150514     C                   move      olv59prg      kprg
013800150514     C                   MOVE      olv59ctr      codctr
013900150514     C                   MOVE      ilv59ksc      kksc
014000060421     C     Ktgc          chain     titgc01l                           31
014100060503     C     *in31         ifeq      '0'
014200060421     C     tgcatb        andeq     *blanks
014300060421     C                   movel     'S'           wtrova
014400060503     C                   endif
014500060503     C                   endif
014600060503     C                   endif
014700060421     C*  Se non ho trovato tariffa cliente cerco su tar. cartello
014800060421     C     wtrova        ifne      'S'
014900060421     C*  Se ho trovato tariffa in vigore mi posiziono sul file
015000060421     C*  delle tariffe giacenze con il proggressivo giusto
015100060421     C                   move      O27prg        kprg
015200060421     C                   move      O27ctr        codctr
015300060421     C                   z-add     O27ksc        kksc
015400060421     C     Ktgc          chain     titgc01l                           31
015500060421     C     tgcatb        ifeq      *blankS
015600060421     C  N31              movel     'S'           wtrova
015700060503     C                   endif
015800060503     C                   endif
015900060906     C*da video
016000060906     c                   else
016100060906      * richiamo da menu cerca tariffa tramite pgm tnta36
016200060906     c                   if        vksc = 0 or vksc <> exksc
016300060906     c                   Clear                   parmta36
016400060906     c                   Eval      ta36ksc = vksc
016500060906     c                   Eval      exksc = vksc
016600060906     c                   Eval      ta36flg = '1'
016700060906     c                   Eval      ta36vpr = '1'
016800060906     c                   Eval      ta36kcc = 151
016900060906     c                   Eval      kpjbu = parmta36
017000060906     c                   Call      'TNTA36R'
017100060906     c                   Parm                    kpjba
017200060906     c                   Eval      parmta36 = kpjbu
017300060906      *non trovato errore
017400060906     c                   setoff                                       91
017500060906     c                   if        ta36ctr = *blank
017600060906     c                   seton                                        91
017700060906     c                   goto      endrut
017800060906     c                   endif
017900060906      *
018000060913     C                   move      ta36ctr       vctr
018100060906     C                   move      ta36prg       kprg
018200060906     C                   move      ta36ctr       codctr
018300060906     C                   z-add     ta36ksc       kksc
018400060906     C     Ktgc          chain     titgc01l                           31
018500060906     C     tgcatb        ifeq      *blankS
018600060906     C  N31              movel     'S'           wtrova
018700060906     C                   endif
018800060906     c                   endif
018900060906      *
019000060906     c                   endif
019100060906      *
019200060503     C                   z-add     0             xp                1 0
019300060503     C                   move      *zeros        sgg
019400060503     C                   move      *zeros        stu
019500060503     C                   move      *zeros        stt
019600060503     C*
019700060503     C                   z-add     vgior         §ggpar            3 0
019800060503     C                   do        3             x                 1 0
019900060503     C     vgior         ifgt      tgs(x)
020000060503     C     vgior         sub       tgs(X)        §ggpar
020100060503     C     x             ifeq      1
020200060503     C                   z-add     tgs(x)        sgg(x)
020300060503     C                   else
020400060503     C     tgs(X)        sub       tgs(xp)       sgg(x)
020500060503     C                   endif
020600060503     C                   z-add     tts(x)        stu(x)
020700060503     C                   else
020800060503     C                   z-add     §ggpar        sgg(x)
020900060503     C                   z-add     tts(x)        stu(x)
021000060503     C                   z-add     0             §ggpar
021100060503     C                   endif
021200060503     C                   add       1             xp
021300060503     C                   enddo
021400060421      * calcolo peso tassabile
021500060906     c                   if        damenu = *blank
021600060421     C     tgcrpv        mult      blpvlf        §pesvo           13 3
021700060421     C     §pesvo        mult      100           §pesvo
021800060421     C     §pesvo        ifgt      blppkf
021900060421     C     §pesvo        div       100           §pes§            13 3
022000060421     C                   else
022100060421     C     blppkf        div       100           §pes§
022200060503     C                   endif
022300060906      * da video
022400060906     c                   else
022500060906     C     tgcrpv        mult      vvol          §pesvo
022600060906     C     §pesvo        mult      100           §pesvo
022700060906     C     §pesvo        ifgt      v§pes§
022800060906     C     §pesvo        div       100           §pes§
022900060906     C                   else
023000060906     C                   z-add     v§pes§        §pes§
023100060906     C                   endif
023200060906     c                   endif
023300060421     C     §pes§         add       0,99          §pesca            6 0
023400060503     C     sgg           mult      stu           stt
023500060503     C     stt           mult      §pesca        stt
023600060503     C                   xfoot     stt           vtsgs
023700060503     C* MINIMO TASSABILE se maggiore di spese di sosta
023800060503     C     vtsgs         iflt      tgcstm
023900060503     C                   z-add     tgcstm        vtsgs
024000060503     C                   endif
024100060421     C* SPESE RICONSEGNA
024200060913 5   C     vcit          ifne      'C'
024300110623 5   C     vcit          andne     'T'
024400060421     C                   z-add     tgcsgp        §sgr             13 3
024500060421x5   C                   else
024600060421     C                   z-add     tgcsgr        §sgr
024700060503e5   C                   endif
024800060421
024900060421     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
025000060421     C     §sgr          mult      §pesca        §sgr
025100060503     C     §sgr          add       tgcsgv        §sgr
025200060503     C     §sgr          mult      vric          vsgr
025300060421     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
025400060503     C     tgcsgd        mult      vape          vsgd
025500060801     c                   z-add     tgcrpv        vrpv
025600060906     c                   if        damenu = *blank
025700060801     c                   z-add     blpvlf        vvol
025800060906     c                   z-add     §pes§         v§pes§
025900060906     c                   endif
026000060809      * verifica applicazione franchigia se giorni da calcolare superano la
026100060809      * franchigia non la applico altrimenti seguo le regole del flag
026200060809     c                   if        tgcsgf >= vgior
026300060809     c                   select
026400060809      * sosta
026500060809     c                   when      tgcfaf = '1'
026600060809     c                   z-add     0             vtsgs
026700060809      * tutte
026800060809     c                   when      tgcfaf = '2'
026900060809     c                   z-add     0             vtsgs
027000060809     c                   z-add     0             vsgd
027100060809     c                   z-add     0             vsgr
027200060809      * sosta e dossier
027300060809     c                   when      tgcfaf = '3'
027400060809     c                   z-add     0             vtsgs
027500060809     c                   z-add     0             vsgd
027600100714     C     tgcsgv        mult      vric          comovar          13 3
027700100714     c                   sub       comovar       vsgr
027800060809     c
027900060809     c                   endsl
028000060809     c                   endif
028100060421     C*
028200060906     C     endrut        endsr
028300060302      *___________________________________________________                  ****
028400060421     C     *inzsr        begsr
028500060302      *___________________________________________________                  ****
028600060421     C     *entry        plist
028700060906     C                   parm                    kpjba
028800061106      * se richiamato da menu non ho parametri
028900061106     c                   if        kpjbu = *blank
029000061106     c                   clear                   fnlg18ds
029100061106     c                   move      '1'           damenu            1
029200061106     c                   else
029300061106      * 90*on protegge campi se chiamato da pgm e non si variano dati bolla
029400061106     c                   seton                                        90
029500061106     c                   clear                   damenu
029600061106     C                   movel(p)  kpjbu         fnlg18ds
029700061106     c                   endif
029800061106      *definizione utente e verifica autorizzazione visualizzazione tariffe
029900061106     c     *dtaara       define    §azute        azuteds
030000061106     c     *dtaara       define    §datiute      ddatiute
030100061106     c                   in(E)     *dtaara
030200061106     c                   If        %error  or RSUT = *blanks
030300061106     c                   Clear                   Tibs34ds
030400061106     c                   Call      'TIBS34R'
030500061106     c                   Parm                    Tibs34ds
030600061106     c                   In        *dtaara
030700061106     c                   EndIf
030800061106
030900061106     c                   Clear                   wabi
031000061106     c                   Clear                   dLat
031100061106
031200061106      * Verifica errori e autorità profilo
031300061106s   1c                   Select
031400061106      * se ho errori nei dati utente esco dal pgm
031500061106w   1c                   When      DutErr = 'E'
031600061106     c                   GoTo      EndAbi
031700061106      * se non c'è l'abilitazione
031800061106      * --> se 1° livello, abilitazioni al terminal
031900061106      *     se 2° livello, abilitazioni al punto operativo
032000061106      *     se sede è impossibile ma se capita mando a fine il pgm
032100061106w   1c                   When      UteAut = *Blanks
032200061106if  2c                   If        DutLpo = '1'
032300061106     c                   Eval      wabi   = 'TP'
032400061106e   2c                   EndIf
032500061106if  2c                   If        DutLpo = '2'
032600061106     c                   Eval      wabi   = 'PO'
032700061106e   2c                   EndIf
032800061106if  2c                   If        DutLpo = 'S'
032900061106     c                   GoTo      EndAbi
033000061106e   2c                   EndIf
033100061106      * carica le abilitazioni del profilo
033200061106x   1c                   Other
033300061106     c                   Movel     UteFaf        Dute01
033400061106     c                   If        §UteGtc <> *Blanks
033500061106     c                   Eval      wabi = §UteGtc
033600061106     c                   Else
033700061106     c                   Eval      wabi = UteAut
033800061106     c                   EndIf
033900061106e   1c                   EndSl
034000061106
034100061106      * controllo se ok l'abilitazione dell'utente
034200061106     c                   Clear                   Tibs02ds
034300061106     c                   Eval      T02Mod = 'C'
034400061106     c                   Eval      T02Sif = knsif
034500061106     c                   Eval      T02Cod = 'LAT'
034600061106     c                   Movel(p)  wabi          T02Ke1
034700061106     c                   Call      'TIBS02R'
034800061106     c                   Parm                    kpjba
034900061106     c                   Parm                    Tibs02ds
035000061106if  1c                   If        T02Err = *Blanks
035100061106     c                   Eval      dLat = T02Uni
035200061106e   1c                   EndIf
035300061106      * errore o non abilitato
035400061106if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
035500061106     c                   GoTo      EndAbi
035600061106e   1c                   EndIf
035700061106
035800061106      * Se l'utente ha autorizzazione 'AZ' è di sede
035900061106      *
036000061106      * Reperimento dei P.O. gestibili dall'utente
036100061106     c                   clear                   TRUL31DS
036200061106     c                   eval      I31abi = wabi
036300061106     c                   eval      I31cdi = DUTdis
036400061106     c                   eval      I31car = DUTare
036500061106     c                   eval      I31cpo = DUTpou
036600061106     c                   call      'TRUL31R'
036700061106     c                   parm                    KPJBA
036800061106     c                   parm                    TRUL31DS
036900061106if  2c                   if        O31pog > *zeros
037000061106     c                   movea     O31pog        fig
037100061106e   2c                   endif
037200061106      * Aggiungo 888 nella sk fig x poter interrogare le tariffe di cartello
037300061106     c                   Z-add     1             xx                3 0
037400061106     c     *zeros        Lookup    fig(xx)                                30
037500061106     c                   If        *In30
037600061106     c                   Move      '888'         fig(xx)
037700061106     c                   EndIf
037800061106     c     EndAbi        tag
037900061106
038000060502      * imposta parametri dft se non passati valori nella ds
038100060421      * data disposizioni a 0 imposto udate
038200060502     c                   if        i18disp = 0
038300060421     c                   move      *date         i18disp
038400060421     c                   endif
038500060302
038600060503      * numero giorni su cui calcolare
038700060823     c                   if        i18gior = 0 and i18nsp = 0
038800060503     c                   z-add     1             vgior
038900060503     c                   else
039000060503     c                   z-add     i18gior       vgior
039100060503     c                   endif
039200060502
039300060503      * Numero aperture
039400060823     c                   if        i18ape = 0  and i18nsp = 0
039500060503     c                   z-add     1             vape
039600060503     c                   else
039700060503     c                   z-add     i18ape        vape
039800060503     c                   endif
039900060502
040000060503      * Numero riconsegne
040100060823     c                   if        i18ric = 0  and i18nsp = 0
040200060503     c                   z-add     1             vric
040300060503     c                   else
040400060503     c                   z-add     i18ric        vric
040500060503     c                   endif
040600060503
040700060421     C     kblp          klist
040800060421     C                   kfld                    i18AAS
040900060421     C                   kfld                    i18LNP
041000060421     C                   kfld                    i18NRS
041100060421     C                   kfld                    i18NSP
041200060421
041300060421     C     Ktgc          klist
041400060421     C                   kfld                    kksc              7 0
041500060421     C                   kfld                    codctr            3 0
041600060421     C                   Kfld                    kprg
041700060421     C     *like         define    tgcprg        kprg
041800060906     C     *like         define    vksc          exksc
041900060302
042000060421     C                   endsr
042100060302
