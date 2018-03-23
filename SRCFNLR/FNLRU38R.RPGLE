000100120830     h dftactgrp(*no) actgrp(*caller)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300160919      * FNLRU38R *----------------------------------------------------*
000400120906      *      VISUALIZZAZIONE DETTAGLIO SEGNACOLLI BOLLE - unico
000500940720      *--------------------------------------------------------------*
000600160919     FFNLRu38D  CF   E             WORKSTN
000700120618     F                                     SFILE(LR38S01:NRR1)
000800120830     F                                     USROPN
000900160919     FFNLRu38D1 CF   E             WORKSTN
001000120830     F                                     SFILE(LR38S011:NRR1)
001100120830     F                                     USROPN
001200911024     FTABEL00F  IF   E           K DISK
001300090226     FFIART01L  IF   E           K DISK
001400161122     FFnART01L  IF   E           K DISK    prefix(old:3)
001500051013     FFIARS01L  IF   E           K DISK
001600980312     FAZORG01L  IF   E           K DISK
001700970805      *--------------------------------------------------------------*
001800970805     D* Schiera codici consegne anomale + relativi flag
001900970805     D C7O             S              1    DIM(200)                             cod.cons.anom
002000970805     D F7O             S              1    DIM(200)                             flg cons.anom
002100120620     D TIPars          S              1    DIM(3)
002200120620     D NOTars          S             35    DIM(3)
002300120831     D  VAR            S              1    dim(32)
002400120620     D FUNF11          S             38    DIM(2) CTDATA PERRCD(1)
002500970805      *--------------------------------------------------------------*
002600911024     D PARAM           DS
002700940720     D  PARAAS                14     17  0
002800940720     D  PARLNP                18     20  0
002900940720     D  PARNRS                21     22  0
003000940720     D  PARNSP                23     29  0
003100940720     D  PARF03                30     30
003200940720     D  PARFLG                31     31
003300940720     D  PARPGM                32     41
003400940720     D  PARDSA                42     61
003500940720     D  PARDSP                63     70  0
003600940720     D  PARRMN                71     85  0
003700940720     D  PARLNA                86     88  0
003800940720     D  PADLNA                89    108
003900090226     D  PARRSc               109    143
004000150619     D  PARTBO               148    148
004100150619     D  PARcod               149    155
004200160916     D  PARTFP               156    158  0
004300160916     D  PARTFA               159    161  0
004400120620     d
004500941230     D PARAM1          DS                  INZ
004600920803     D  VI1NFV                 1      5  0
004700920803     D  VI1LNP                 6      8  0
004800920803     D  VI1LNA                 9     11  0
004900920803     D  VI1NRS                12     13  0
005000920803     D  VI1NCD                14     20  0
005100920803     D  VI1NCA                21     27  0
005200920803     D  VI1NPS                28     29  0
005300920803     D  VI1NPG                30     30  0
005400920813     D  VI1FLG                31     31
005500920803     D  VI1PGM                32     38
005600941230     D  VI1FGS                39     41  0
005700120621     d* bolla
005800120621     d  vi1bolp               42     44  0
005900120621     d  vi1bors               45     46  0
006000120621     d  vi1bosp               47     53  0
006100120621     d  vi1boan               54     57  0
006200120621     D  PA1ERR                58     58
006300940720     D WLBDAT          DS                  INZ
006400940720     D  G02DAT                 1      8  0
006500940720     D  G02INV                 9     16  0
006600940720     D  G02ERR                17     17
006700940720     D  G02TGI                18     22  0
006800120620     D
006900160919     d fnlru39ds     e ds
007000161122     d fiartds       e ds                  extname(fiart00f)
007100161122     d s_fiartds     e ds                  extname(fiart00F) prefix(s_)
007200961122     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
007300961122     D DSLR45        E DS                  EXTNAME(FNLR45DS)
007400120620     d
007500900517     D KPJBA         E DS
007600120711     D DS3E          E DS
007700120711     D DS7O          E DS
007800000911     D DS7H          E DS
007900020121     D OG143         E DS
008000090226     d wdcm            s              6  0
008100161122     d Kdcm            s              8  0
008200120620     D WNUME           s              8
008300120620     D XX              s              3  0
008400120830     d dsp_mod         s             10I 0
008500120830
008600120830     D* PROTOTIPI
008700120830      /copy gaitrasrc/srcprotopr,QsnQryModS
008800940720     C*-------------------------------------------------------------------------
008900941110     C     *ENTRY        PLIST
009000941110     C                   PARM                    KPJBA
009100941110     C*
009200910219     C                   MOVEL     KPJBU         PARAM
009300090226     c* Imposto campi testata
009400160916     c                   exsr      Dati_tes
009500020226      *
009600900509     C     FOR01         TAG
009700120830     C                   IF        dsp_mod = 1
009800120830     C                   WRITE     LR38T011                                     TESTATA
009900120830     C                   WRITE     LR38Z011                                     TASTI FUNZIONALI
010000120830     C                   EXFMT     LR38C011                                     SFLCTL
010100120830     C                   ELSE
010200120830     C                   WRITE     LR38T01                                      TESTATA
010300120830     C                   WRITE     LR38Z01                                      TASTI FUNZIONALI
010400120830     C                   EXFMT     LR38C01                                      SFLCTL
010500120830     C                   ENDIF
010600940802     C*
010700980316     C                   SETOFF                                       11
010800950220     C                   CLEAR                   PARF03                         NO PREMUTO F3=FINE
010900940802     C   KC              MOVEL     '1'           PARF03                         PREMUTO F3=FINE
011000920803     C*
011100120620     C* F19 - INTERROGAZIONE SPUNTE
011200950125     C   KT              DO
011300950125     C                   MOVEL     PARAM1        KPJBU
011400950125     C                   CALL      'FNLR04R'                            99
011500920803     C                   PARM                    KPJBA
011600950125     C                   GOTO      FOR01
011700950125     C                   ENDDO
011800910219     C*
011900120620     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
012000961122     C     *INKH         IFEQ      *ON
012100961122     C                   CLEAR                   DSLR45
012200961122     C                   MOVE      PARAAS        D45AAS
012300961122     C                   MOVE      PARLNP        D45FIL
012400961122     C                   MOVE      PARNRS        D45NRS
012500961122     C                   MOVE      PARNSP        D45NSP
012600961122     C                   MOVEL     DSLR45        KPJBU
012700961122     C                   CALL      'FNLR45R'
012800961122     C                   PARM                    KPJBA
012900961122     C                   GOTO      FOR01
013000961122     C                   ENDIF
013100020226      *
013200120620     C* F11 - INTERROGAZIONE VOLUME/SEGNACOLLO EUROEXPRESS
013300980312     C     *INKK         IFEQ      *ON
013400020226      *
013500980316     C                   SETON                                        11
013600980312     C                   EXSR      UPDS01
013700020226      *
013800980312     C                   GOTO      FOR01
013900980312     C                   END
014000961122     C*
014100000000     C     FINE          TAG
014200150618     c                   movel     'C'           i38tla
014300160919     c                   call      'FNLRU39R'
014400160919     c                   parm                    fnlru39ds
014500150618     c                   parm                    s_fiartds
014600950125     C                   MOVEL     PARAM         KPJBU
014700150618
014800150618
014900000000     C                   SETON                                        LR
015000940720     C**************************************************************************
015100940720     C* CARICO DETTAGLIO SEGNACOLLI
015200940720     C**************************************************************************
015300090226     C     CARART        BEGSR
015400940720     C*
015500910205     C                   EXSR      PULSF
015600120620     c
015700940720     C                   Z-ADD     0             NRR1              4 0
015800160922     c
015900160922     C                   SETOFF                                       301213
016000090226     C     KART1         SETLL     FiART01L
016100090226     C     KART1         READE     FiART01L                               30
016200940720     C  N30              MOVEL     ARTNSC        VI1NCD
016300950116     C  N30              MOVEL     ARTFLS        VI1LNP
016400090226     C  N30              MOVEL     ARTFLS        Vccfls
016500910220     C*
016600161115     c* richiamo soltanto se i dati sono vuoti
016700910806     C     *IN30         DOWEQ     '0'
016800161122     c                   eval      kdcm=artdcm
016900180124     c                   setoff                                       14
017000161122     c
017100161122     c                   if        artdcm>0
017200161122     C     KARTold       chain     FnART01L
017300161122     c                   if        %found(fnart01l) and olddcm=0
017400161122     c                   clear                   kdcm
017500161122     c                   endif
017600161122     c                   endif
017700161122     c
017800161122     c                   if        artdcm=0 or kdcm=0
017900160919     c                   clear                   fnlru39ds
018000150618     c                   eval      i38tbo=partbo
018100150618     c                   eval      i38aas=artaas
018200150618     c                   eval      i38lnp=artlnp
018300150618     c                   eval      i38nrs=artnrs
018400150618     c                   eval      i38nsp=artnsp
018500150618     c                   eval      i38nsc=artnsc
018600160919     c                   call      'FNLRU39R'
018700160919     c                   parm                    fnlru39ds
018800150618     c                   parm                    s_fiartds
018900161115     c
019000150618     c                   eval      fiartds=s_fiartds
019100161115     c                   endif
019200160922     c
019300160922     c* Se la bolla è "da partire" lo visualizzo anche nel dett colli
019400160922     c                   if        artbga=' '
019500160922     c                   seton                                        12
019600160922     c                   eval      vccdcl1='DaPartire'
019700160922     c* In questo caso devo reimpostare il destinatario perchè non ho lo spazio per
019800160922     c*  scrivere il "da partire" in un altro posto
019900160922     c                   eval      vcdcod1='Dest.'+parrsc
020000160922     c                   endif
020100160922     c
020200160922     c* in R.I se arrivato almeno un collo
020300160922     c                   if        artdam>0
020400160922     c                   seton                                        13
020500160922     c                   endif
020600160922     c
020700150318
020800120620     c                   clear                   tipars
020900120620     c                   clear                   notars
021000120620     c                   clear                   XX
021100920813     C*
021200940720     C                   MOVEL     ARTNSC        VSCNSC                         NUMERO SEGNACOLLO
021300940720     C                   MOVEL     ARTFLP        VSCFLP                         FILIALE TRANSITO
021400120619     C* cod anomalia
021500940720     C                   MOVEL     *BLANKS       VSCCAN
021600160922     C                   MOVEL     *BLANKS       VSdCAN
021700090226     C                   MOVEL     *BLANKS       VSCFTA
021800920813     C     ARTCAN        IFNE      ' '
021900940720     C                   MOVEL     '3E'          KCOD
022000940720     C                   MOVEL     *BLANKS       KKEY
022100940720     C                   MOVEL     ARTCAN        KKEY
022200940720     C     KTAB1         CHAIN     TABEL00F                           31
022300120711     c  n31              movel     tbluni        ds3e
022400120711     c   31              clear                   ds3e
022500120711     c
022600160922     c                   eval      vsccan=artcan+'-'+
022700160922     c                                    %subst(§3ede12:1:9)+'  '+artfta
022800160922     c                   eval      vsdcan=artcan+'-'+
022900160922     c                                    %subst(§3ede12:1:11)
023000090226     c* FTA
023100120618     c                   eval      vscfta=artfta
023200920813     C                   END
023300910912     C* GIRO DATE:
023400090226     C* entrata
023500160922     c                   clear                   vscdse
023600160922     c                   clear                   vsddse
023700160922     c                   clear                   vscnpe
023800160922     c                   clear                   vsdnpe
023900160922     c                   clear                   vschse
024000160922     c
024100090226     C                   MOVEL     ARTDse        G02INV
024200090226     C                   Z-ADD     *ZEROS        G02DAT
024300090226     C                   MOVEL     '3'           G02ERR
024400090226     C                   CALL      'XSRDA8'
024500090226     C                   PARM                    WLBDAT
024600160920     C                   MOVEL     G02DAT        VSCDse            6 0
024700120621     C                   MOVE      G02DAT        due
024800120621     C                   MOVE      due           VSCDse
024900160919     c
025000090226     C     ARTdse        IFGT      0
025100160919     c                   eval      vsdnpe=%editc(artnpe:'X')+'|'
025200160920     c                   eval      vscnpe=%editc(artnpe:'X')
025300090226     C                   ELSE
025400160919     C                   MOVEL     '  |'         VSdNPe
025500160920     C                   MOVEL     '  '          VScNPe
025600160920     C                   END
025700120619     c* Orario
025800120621     c                   if        arthse>0
025900120621     c                   eval      vschse=%editw(arthse:'0 :  ')
026000120621     c                   endif
026100160919     c                   eval      vsddse ='|        '
026200160919     c                   select
026300160919     c                   when      artdse>0 and (artdie=0  or artdie=artdse)
026400160919     c                   eval      vsddse=%trim(vsddse) +
026500160919     c                                    %editw(vscdse:'  /  /  ')
026600160919
026700160919     c* se data immissione <> data arrivo la visualizzo
026800160919     c                   when      artdie>0 and artdse>0 and artdie<>artdse
026900160919     c                   movel     g02dat        wggmm             4 0
027000160919     c                   eval      vsddse=%trim(vsddse) +
027100160919     c                                    %editc(wggmm:'X')
027200160919
027300160919     C                   MOVEL     ARTDie        G02INV
027400160919     C                   Z-ADD     *ZEROS        G02DAT
027500160919     C                   MOVEL     '3'           G02ERR
027600160919     C                   CALL      'XSRDA8'
027700160919     C                   PARM                    WLBDAT
027800160919     c                   movel     g02dat        wggmm             4 0
027900160920     c                   eval      vsddse=%trim(vsddse) +'|' +%editc(wggmm:'X')
028000160919     c                   endsl
028100910912     C* PARTENZA MERCE
028200160922     c                   clear                   vscdfv
028300160922     c                   clear                   vsddfv
028400160922     c                   clear                   vsdnpp
028500160922     c                   clear                   vscnpp
028600160922     c
028700940720     C                   MOVEL     ARTDFV        G02INV
028800940720     C                   Z-ADD     *ZEROS        G02DAT
028900940720     C                   MOVEL     '3'           G02ERR
029000940720     C                   CALL      'XSRDA8'
029100940720     C                   PARM                    WLBDAT
029200940720     C                   MOVEL     G02DAT        VSCDFV
029300120620     C                   MOVE      G02DAT        DUE               2 0          ANNO 2 CIFRE
029400950116     C                   MOVE      DUE           VSCDFV
029500160920     c                   eval      vsddfv ='|        '
029600160920     c                   if        artdfv>0
029700160920     c                   eval      vsddfv=%trim(vsddfv) +
029800160920     c                                    %editw(vscdfv:'  /  /  ')
029900160920     c                   endif
030000160920
030100921231     C     ARTDFV        IFGT      0
030200940720     C                   MOVEL     ARTNPP        VSCNPP
030300160920     c                   eval      vsdnpp= %editc(artnpp:'X')+'|'
030400921231     C                   ELSE
030500940720     C                   MOVEL     '  '          VSCNPP
030600160920     C                   MOVEL     '  |'         VSdNPP
030700921231     C                   END
030800921230     C* ENTRATA AL TRANSITO
030900160922     c                   clear                   vscdet
031000160922     c                   clear                   vsc6det
031100160922     c                   clear                   vsdpet
031200160922     c                   clear                   vscpet
031300160922     c
031400941219     C                   MOVEL     ARTDET        G02INV                         AAAA/MM/GG
031500940720     C                   Z-ADD     *ZEROS        G02DAT
031600940720     C                   MOVEL     '3'           G02ERR
031700940720     C                   CALL      'XSRDA8'
031800940720     C                   PARM                    WLBDAT
031900160922     C                   MOVEL     G02DAT        VSCDET                         GG/MM
032000160922     C                   MOVEL     G02DAT        VSC6DET                        GG/MM/aa
032100160920     C                   MOVE      G02DAT        due
032200160922     C                   MOVE      due           vsc6det                        GG/MM/AA
032300921231     C     ARTDET        IFGT      0
032400940720     C                   MOVEL     ARTPET        VSCPET
032500160920     C                   EVAL      VSDPET=%editc(artpet:'X')+'|'
032600921231     C                   ELSE
032700940720     C                   MOVEL     '  '          VSCPET
032800160920     C                   MOVEL     '  |'         VSdPET
032900921231     C                   END
033000910912     C* USCITA DAL TRANSITO
033100160922     c                   clear                   vscdut
033200160922     c                   clear                   vscput
033300160922     c                   clear                   vsdput
033400160922     c
033500940720     C                   MOVEL     ARTDUT        G02INV
033600940720     C                   Z-ADD     *ZEROS        G02DAT
033700940720     C                   MOVEL     '3'           G02ERR
033800940720     C                   CALL      'XSRDA8'
033900940720     C                   PARM                    WLBDAT
034000940720     C                   MOVEL     G02DAT        VSCDUT
034100120619     C                   MOVE      G02DAT        due
034200120619     C                   MOVE      due           vscdut
034300921231     C     ARTDUT        IFGT      0
034400160920     C                   MOVEL     ARTPUT        VSCPUT            2
034500160920     c                   eval      vsdput=%editc(artput:'X')+'|'
034600921231     C                   ELSE
034700940720     C                   MOVEL     '  '          VSCPUT
034800160920     C                   MOVEL     '  |'         VSdPUT
034900921231     C                   END
035000160922
035100910912     C* ARRIVO MERCE ALLA FIL ARRIVO
035200160922     c                   clear                   vsddam
035300160922     c                   clear                   vsdnap
035400160922     c                   clear                   vscnap
035500160922     c                   clear                   vscham
035600160916     c                   eval      vscflnap=artflnap
035700160916     c
035800940720     C                   MOVEL     ARTDAM        G02INV
035900940720     C                   Z-ADD     *ZEROS        G02DAT
036000940720     C                   MOVEL     '3'           G02ERR
036100940720     C                   CALL      'XSRDA8'
036200940720     C                   PARM                    WLBDAT
036300160920     C                   MOVEL     G02DAT        VSCDAM            6 0
036400950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
036500950116     C                   MOVE      DUE           VSCDAM
036600921231     C     ARTDAM        IFGT      0
036700940720     C                   MOVEL     ARTNAP        VSCNAP
036800160920     c                   eval      vsdnap=%editc(artnap:'X') + '|'
036900921231     C                   ELSE
037000940720     C                   MOVEL     '  '          VSCNAP
037100160920     C                   MOVEL     '  |'         VSdNAP
037200921231     C                   END
037300120621     c                   if        artham>0
037400120621     c                   eval      vscham=%editw(artham:'0 :  ')
037500120621     c                   endif
037600160916
037700160916     c                   select
037800160916     c                   when      artdam>0 and (artdia=0  or artdia=artdam)
037900160920     c                   eval      vsddam=%editw(g02dat:'  /  /    ')
038000160916
038100160916     c* se data immissione <> data arrivo la visualizzo
038200160916     c                   when      artdia>0 and artdam>0 and artdia<>artdam
038300160916     c                   movel     g02dat        wggmm             4 0
038400160920     c                   eval      vsddam=%editw(wggmm:'  /  ')
038500160916
038600160916     C                   MOVEL     ARTDia        G02INV
038700160916     C                   Z-ADD     *ZEROS        G02DAT
038800160916     C                   MOVEL     '3'           G02ERR
038900160916     C                   CALL      'XSRDA8'
039000160916     C                   PARM                    WLBDAT
039100160916     c                   movel     g02dat        wggmm             4 0
039200160920     c                   eval      vsddam=%trim(vsddam) +'|' +
039300160920     c                                    %editw(wggmm:'  /  ')
039400160916     c                   endsl
039500160916     c
039600910912     C* CONSEGNA MERCE REALE
039700090226     c                   clear                   vsccon
039800090226     c                   setoff                                       01
039900090226     c                   if        artdcm>0
040000940720     C                   MOVEL     ARTDCM        G02INV
040100940720     C                   Z-ADD     *ZEROS        G02DAT
040200940720     C                   MOVEL     '3'           G02ERR
040300940720     C                   CALL      'XSRDA8'
040400940720     C                   PARM                    WLBDAT
040500090226     C                   MOVEL     G02DAT        wdcm
040600950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
040700090226     C                   MOVE      DUE           wDCM
040800120618     c                   eval      vsccon= %editw(wdcm: '  /  /  ')
040900120618     c                   if        arthcm>0
041000160922     c                   eval      vsccon= %editw(wdcm: '  /  /  ')+' '+
041100120618     c                             %editw(arthcm: '  :  ')
041200090226     c                   endif
041300120618     c                   endif
041400120619     c
041500120619     c* ARRIVO FINALE
041600160920     c                   clear                   vscdamf           6 0
041700160922     c                   clear                   vsddamf
041800120619     c                   clear                   vschamf
041900120619     c                   clear                   vscnapf
042000160920     c                   clear                   vsdnapf
042100161020     c                   clear                   vsccan_p
042200160922
042300120619     c* Se non si tratta di bolla export, imposto la data all'arrivo finale
042400160920    2c                   if        not *in15
042500120619     C                   MOVEL     ARTDAMF       G02INV
042600120619     C                   Z-ADD     *ZEROS        G02DAT
042700120619     C                   MOVEL     '3'           G02ERR
042800120619     C                   CALL      'XSRDA8'
042900120619     C                   PARM                    WLBDAT
043000120619     C                   MOVEL     G02DAT        VSCDAMf
043100120619     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
043200120619     C                   MOVE      DUE           VSCDAMf
043300160920     c
043400160920    3C     ARTnapf       IFGT      0
043500120619     C                   MOVEL     ARTNAPf       VSCNAPf
043600160920     c                   eval      vsdnapf=%editc(artnapf:'X')+ '|'
043700120619     C                   ELSE
043800120619     C                   MOVEL     '  '          VSCNAPf
043900160920     C                   MOVEL     '  |'         VSdNAPf
044000160920    3C                   END
044100160920    3c                   if        arthamf>0
044200120621     c                   eval      vschamf=%editw(arthamf:'0 :  ')
044300160920    3c                   endif
044400160916     c
044500160920    3c                   select
044600160916     c                   when      artdamf>0 and (artdiaf=0  or artdiaf=artdamf)
044700160920     c                   eval      vsddamf=%editw(g02dat:'  /  /    ')
044800160916
044900160916     c* se data immissione <> data arrivo la visualizzo
045000160916     c                   when      artdiaf>0 and artdamf>0 and artdiaf<>artdamf
045100160916     c                   movel     g02dat        wggmm             4 0
045200160916     c                   eval      vsddamf=%trim(vsddamf) +
045300160920     c                                     %editw(wggmm :'  /  ')
045400160916
045500160916     C                   MOVEL     ARTDiaf       G02INV
045600160916     C                   Z-ADD     *ZEROS        G02DAT
045700160916     C                   MOVEL     '3'           G02ERR
045800160916     C                   CALL      'XSRDA8'
045900160916     C                   PARM                    WLBDAT
046000160916     c                   movel     g02dat        wggmm             4 0
046100160920     c                   eval      vsddamf=%trim(vsddamf) +'|' +
046200160920     c                                    %editw(wggmm:'  /  ')
046300160920    3c                   endsl
046400120619     c
046500160920    2c                   endif
046600910912     C*
046700090226     C* SOLO PER LE BOLLE EXPORT (15 ON) VISUALIZZO LA DATA manifest
046800161019     c* nel campo DAMF
046900961112     C     *IN15         IFEQ      *ON
047000161019     c                   clear                   vschamf
047100161019     c                   clear                   vsddamf
047200161019     C                   MOVEL     '  '          VSCNAPf
047300161019     c                   if        artdmf>0
047400090226     C                   MOVEL     ARTDmf        G02INV
047500961112     C                   Z-ADD     *ZEROS        G02DAT
047600961112     C                   MOVEL     '3'           G02ERR
047700961112     C                   CALL      'XSRDA8'
047800961112     C                   PARM                    WLBDAT
047900090226     C                   MOVEL     G02DAT        VSCDmf
048000961112     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
048100090226     C                   MOVE      DUE           VSCDmf
048200160920     c                   eval      vsddamf=%editw(g02dat:'  /  /    ')
048300161019     c
048400161019     c* il campo ORA contiene la pistola di spunta dell'invio al partner per video a 80
048500161019     C                   IF        dsp_mod = 0
048600161019     C                   MOVE      ARTNpdmf      VSChamf
048700161019     c                   endif
048800161019     c
048900160920     c                   eval      vsdnapf=%editc(artnpdmf:'X')+ '|'
049000160920     C                   ELSE
049100160920     C                   MOVEL     '  |'         VSdNAPf
049200961112     C                   ENDIF
049300160920     C                   ENDIF
049400000211      *
049500000211      * Aggancio estensione dettaglio segnacolli
049600020226      * A seconda della linea di arrivo aggancio prima tipo righe E o F
049700020226     C     §OGNTW        IFEQ      'FED'
049800020226     C                   MOVE      'F'           WTRC
049900020226     C                   ELSE
050000020226     C                   MOVE      'E'           WTRC
050100020226     C                   ENDIF
050200051013     C     KARS1         CHAIN     FIARS01L                           31
050300120619    1C     *IN31         IFEQ      *OFF
050400000911      *
050500020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
050600120619    2C     ARSNOT        IFNE      *BLANKS
050700120620     c                   add       1             xx
050800120620     c                   movel     wtrc          tipars(xx)
050900120620     C                   MOVEL     ARSNOT        NOTars(xx)
051000120619    2C                   ENDIF
051100011031      *
051200011031      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
051300121107     c                   if        *in15
051400161019     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
051500161019     c* nel campo PISTOLTa di spunta lna finale
051600161020     c* video 80
051700161019     C                   MOVEL     ARSCAN        VSCnapf
051800161020     c* video 132
051900161020     C                   MOVEL     ARSCAN        VSCcan_P
052000121107     c
052100161019     c                   if        arsdat>0
052200011031     C                   MOVEL     ARSDAT        G02INV            8 0
052300011031     C                   Z-ADD     *ZEROS        G02DAT
052400011031     C                   MOVEL     '3'           G02ERR
052500011031     C                   CALL      'XSRDA8'
052600011031     C                   PARM                    WLBDAT
052700161019     C                   MOVEL     G02DAT        w0040             4 0
052800161019     c                   eval      vsddamp=%editw(g02dat:'  /  /    ')+
052900161019     c                             ' ' + arscan
053000161019     c                   if        dsp_mod=0
053100161019     c                   clear                   vsccan
053200161019     c                   eval      vsccan='ArrP '+%editw(w0040: '  /  ')+
053300161019     c                             '   ' + artcan
053400121107     c                   endif
053500161019     c                   endif
053600161019     c                   endif
053700011031      *
053800120620   x1C                   ELSE
053900000911     C                   CLEAR                   VSCVA2
054000120619    1C                   ENDIF
054100000911      *
054200000911     C                   MOVEL     'C'           WTRC
054300051013     C     KARS1         CHAIN     FIARS01L                           31
054400120620    1C     *IN31         IFEQ      *OFF
054500000211      *
054600000911      * VERIFICO PRESENZA CODICE "CHI SONO"
054700120620    2C     ARSNOT        IFNE      *BLANKS
054800120620     c                   add       1             xx
054900120620     c                   movel     wtrc          tipars(xx)
055000120620     C                   MOVEL     ARSNOT        NOTars(xx)
055100120620    2C                   ENDIF
055200000211      *
055300120620    1C                   endif
055400180124      *
055500180124      * Verifico presenza indicazione incompatibile
055600180124     c                   clear                   vscinc
055700180124     C                   MOVEL     'J'           WTRC
055800180124     C     KARS1         CHAIN     FIARS01L
055900180124    1C                   if        %found(fiars01l) and arsnot<>*blanks
056000180124     c                   eval      vscinc='INC'
056100180124     c                   seton                                        14
056200180124    1C                   ENDIF
056300180124      *
056400000211      *
056500090226      * SE NON C'È FIARS VISUALIZZO IL VOLUME VDL
056600980312     C                   CLEAR                   VAR
056700120831     C                   MOVEA     'Vol'         VAR(19)
056800120620    2C     ARTVUC        IFGT      0
056900120620     c                   eval      wnume=%editc(artvuc:'4')
057000120831     C                   MOVEA     WNUME         VAR(23)
057100120831     C                   MOVEA     ARTFVC        VAR(32)
057200120620    2C                   END
057300120831     C                   MOVEA     'Peso'        VAR(1)
057400120620    2C     ARTPUC        IFGT      0
057500120620     c                   eval      wnume=%editc(artpuc:'4')
057600120831     C                   MOVEA     WNUME         VAR(6)
057700120831     C                   MOVEA     ARTFPC        VAR(16)
057800120620    2C                   END
057900120620     c
058000120620     c                   add       1             xx
058100120620     c                   movel     ' '           tipars(xx)
058200120620     C                   MOVEa     var           NOTars(xx)
058300000211      *
058400090226      * SEGNALO SE SEGNACOLLO con consegna anomala
058500090226     c                   if        artcca<>' '
058600970805     C                   Z-ADD     1             X
058700090226     C     ARTcca        LOOKUP    C7O(X)                                 34
058800970805     C     *IN34         IFEQ      '1'
058900970805     C     F7O(X)        ANDNE     *BLANKS
059000160922     c                   eval      %subst(vsccon:10:5)=' IDD '
059100160922     c                   seton                                        01
059200970805     C                   END
059300090226     c
059400090226     c                   else
059500090226     c* Se disabilitato visualizzo
059600090226     c                   if        artdcm=0 and artdab='S'
059700090226     c                   eval      vsccon='MANCANTE'
059800090226     c                   seton                                        01
059900090226     c                   endif
060000090226     c                   endif
060100961112     C*
060200120620     c                   exsr      AbiF11
060300160920     c* se il campo vsc7h1 vuoto, metto una | di separazione
060400160920     c                   if        vsc7h1=*blanks
060500160920     c                   eval      vsc7h1='        |  '
060600160920     c                   endif
060700120620     c
060800910205     C* SCRIVO SUBFILE
060900940720     C                   ADD       1             NRR1
061000120830     C                   IF        dsp_mod = 1
061100120830     C                   WRITE     LR38S011
061200120830     C                   ELSE
061300120830     C                   WRITE     LR38S01
061400120830     C                   ENDIF
061500910912     C*
061600090226     C     KART1         READE     FiART01L                               30
061700910205     C                   END
061800920803     C*
061900920803     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
062000160609     c     nrr1          ifgt      0
062100160609     C     ARTNSC        andne     VI1NCD
062200940720     C                   MOVEL     ARTNSC        VI1NCA
062300920803     C                   END
062400910205     C*
062500940720     C                   Z-ADD     NRR1          SAVNRR            4 0
062600921104     C*
062700940720     C     NRR1          IFEQ      0
062800120830     C                   IF        dsp_mod = 1
062900120830     c                   clear                   lr38s011
063000120830     C                   ELSE
063100120830     c                   clear                   lr38s01
063200120830     C                   ENDIF
063300940720     C                   ADD       1             NRR1
063400120830     C                   IF        dsp_mod = 1
063500120830     C                   WRITE     LR38S011
063600120830     C                   ELSE
063700120830     C                   WRITE     LR38S01
063800120830     C                   ENDIF
063900921104     C                   END
064000910205     C*
064100910205     C                   ENDSR
064200020226     C**************************************************************************
064300020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11
064400020226     C**************************************************************************
064500020226     C     ABIF11        BEGSR
064600120620     c                   clear                   vsc7h1
064700120620     c                   clear                   vsh7h2
064800120620     c                   clear                   vsh7h3
064900120830     C                   IF        dsp_mod = 1
065000120830     c                   clear                   vsc7h11
065100120830     c                   clear                   vsh7h31
065200120830     C                   ENDIF
065300120620     c                   clear                   vscvar
065400120620     c                   clear                   vscva2
065500120620     c                   clear                   vshnot2
065600120620     c                   clear                   vshnot3
065700120620     c*
065800120620     c* se  presenti tutti e 3 accendo F11
065900120620     c                   if        xx=3
066000020226     C                   seton                                        17
066100120620     C                   movel     FunF11(1)     VZCF11
066200120620     C                   MOVEL     'S'           WINFO             1
066300120620     c                   endif
066400020226      *
066500120620     c* Visualizzo i primi due
066600120620     c                   eval      vscva2=notars(1)
066700120621    1c                   if        tipars(1)<>*blanks
066800120620     c
066900120620     C                   MOVE      '7H'          KCOD
067000120620     C                   MOVEL(P)  tipars(1)     KKEY
067100120620     C     KTAB1         CHAIN     TABEL00F                           38
067200120620     C  N38              MOVEL     TBLUNI        DS7H
067300120620     C   38              clear                   DS7H
067400120620    2c                   if        tipars(2)=' '
067500120620     C                   MOVEL     §7HDEB        VSc7H1
067600120620   x2c                   else
067700120830     C                   IF        dsp_mod = 1
067800120830     C                   MOVEL     §7HDEB        VSc7H1
067900120830     C                   ELSE
068000120620     c                   eval      vsc7h1=§7HDEB6+'/'
068100120830     C                   ENDIF
068200120620     c* prima descrizione completa
068300120620     C                   MOVEL     §7HDEB        VSh7H2
068400120620    2c                   endif
068500120620    1c                   endif
068600120620     c
068700120620     c                   eval      vscvar=notars(2)
068800120621    1c                   if        tipars(2)<>*blanks
068900120620     C                   MOVE      '7H'          KCOD
069000120620     C                   MOVEL(P)  tipars(2)     KKEY
069100120620     C     KTAB1         CHAIN     TABEL00F                           38
069200120620     C  N38              MOVEL     TBLUNI        DS7H
069300120620     C   38              clear                   DS7H
069400120620    2c                   if        tipars(1)=' '
069500120620     C                   MOVEL     §7HDEB        VSc7H1
069600120620     c                   else
069700120830     C                   IF        dsp_mod = 1
069800120830     C                   MOVEL     §7HDEB        VSc7H11
069900120830     c* 2 descrizioni perché 132 col.  (le salvo entrambe)
070000120830     c                   eval      vsh7h3=vsc7h1
070100120830     c                   eval      vsh7h31=vsc7h11
070200120830     C                   ELSE
070300120620     c                   eval      vsc7h1=%trim(vsc7h1)+§7HDEB6
070400120830     c* 1 descrizione doppia perché 80 col. (ne salvo una sola)
070500120620     c                   eval      vsh7h3=vsc7h1
070600120830     C                   ENDIF
070700120620    2c                   endif
070800120620    1c                   endif
070900120620     c
071000120620     c                   eval      vshnot2=notars(2)
071100120620     c                   eval      vshnot3=notars(3)
071200020226      *
071300020226     C                   ENDSR
071400980312     C**************************************************************************
071500120620     C* AGGIORNAMENTO SUBFILE PER INTERROGAZIONE ALTRI DATI
071600980312     C**************************************************************************
071700980312     C     UPDS01        BEGSR
071800980312     C                   Z-ADD     1             NRR1
071900120830     C                   IF        dsp_mod = 1
072000120830     C     NRR1          CHAIN     LR38S011                           31
072100120830     C                   ELSE
072200120830     C     NRR1          CHAIN     LR38S01                            31
072300120830     C                   ENDIF
072400980312     C     *IN31         DOWEQ     *OFF
072500120620     c
072600120620     C* VISUALIZZO dati vdl al posto del chi sono --> record 3 col record 2
072700120620     C                   if        WINFO='S'
072800120620     c                   eval      vsc7h1=vsh7h2
072900120830     C                   IF        dsp_mod = 1
073000120830     c                   eval      vsc7h11=*blank
073100120830     C                   ENDIF
073200120620     c                   eval      vscvar=vshnot3
073300120620     c
073400980312     C                   ELSE
073500120620     C* VISUALIZZO "CHI SONO" al posto di dati VDL --> record 2 al posto di record 3
073600120620     c                   eval      vsc7h1=vsh7h3
073700120830     C                   IF        dsp_mod = 1
073800120830     c                   eval      vsc7h11=vsh7h31
073900120830     C                   ENDIF
074000120620     c                   eval      vscvar=vshnot2
074100980312     C                   END
074200020226      *
074300120830     C                   IF        dsp_mod = 1
074400120830     C                   UPDATE    LR38S011
074500120830     C                   ELSE
074600120830     C                   UPDATE    LR38S01
074700120830     C                   ENDIF
074800980312     C                   ADD       1             NRR1
074900120830     C                   IF        dsp_mod = 1
075000120830     C     NRR1          CHAIN     LR38S011                           31
075100120830     C                   ELSE
075200120830     C     NRR1          CHAIN     LR38S01                            31
075300120830     C                   ENDIF
075400980312     C                   ENDDO
075500120620     c
075600120620     c                   if        WINFO='S'
075700120620     C                   movel     FunF11(2)     VZCF11
075800120620     c                   eval      winfo='V'
075900120620     c                   else
076000120620     C                   movel     FunF11(1)     VZCF11
076100120620     c                   eval      winfo='S'
076200120620     c                   endif
076300120620     c
076400980312     C                   ENDSR
076500940720     C**************************************************************************
076600940720     C* PULIZIA SUBFILE
076700940720     C**************************************************************************
076800910205     C     PULSF         BEGSR
076900940720     C*
077000910205     C                   SETON                                        35
077100120830     C                   IF        dsp_mod = 1
077200120830     C                   WRITE     LR38C011                                     SFLCTL
077300120830     C                   ELSE
077400120830     C                   WRITE     LR38C01                                      SFLCTL
077500120830     C                   ENDIF
077600910205     C                   SETOFF                                       35
077700940720     C*
077800910205     C                   ENDSR
077900940720     C**************************************************************************
078000940720     C*  ROUTINE  INIZIALE
078100940720     C**************************************************************************
078200940720     C     *INZSR        BEGSR
078300940720     C*
078400090226     C     KART1         KLIST                                                  FiiRT01L
078500940720     C                   KFLD                    PARAAS                         ..ANNO
078600940720     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
078700940720     C                   KFLD                    PARNRS                         ..NUMERO SERIE
078800940720     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
078900161122     C     KARTold       KLIST                                                  FiiRT01L
079000161122     C                   KFLD                    PARAAS                         ..ANNO
079100161122     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
079200161122     C                   KFLD                    PARNRS                         ..NUMERO SERIE
079300161122     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
079400161122     C                   KFLD                    artnsc
079500150326     C     KART27        KLIST                                                  FiiRT01L
079600150326     C                   KFLD                    artfls                         ..ANNO
079700150326     C                   KFLD                    artlna                         ..LINEA PARTENZA
079800150326     C                   KFLD                    artNRS                         ..NUMERO SERIE
079900150326     C                   KFLD                    artnsc                         ..NUMERO SPEDIZIONE
080000940720     C     KTAB1         KLIST                                                  TABEL00F
080100940720     C                   KFLD                    KKUT                           ..CODICE UTENTE
080200940720     C                   KFLD                    KCOD                           ..CODICE TABELLA
080300940720     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
080400970805     C     KTAB2         KLIST                                                  TABEL00F
080500970805     C                   KFLD                    KKUT                           ..CODICE UTENTE
080600970805     C                   KFLD                    KCOD                           ..CODICE TABELLA
080700090226     C     KARS1         KLIST                                                  FiART01L
080800980312     C                   KFLD                    ARTFLS
080900980312     C                   KFLD                    ARTLNA                                     ENZA
081000980312     C                   KFLD                    ARTNRS                                     IE
081100980312     C                   KFLD                    ARTNSC                                     DIZIONE
081200980312     C                   KFLD                    WTRC                                       DIZIONE
081300940720     C*
081400940720     C     *LIKE         DEFINE    TBLKUT        KKUT                           CODICE UTENTE
081500940720     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
081600940720     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
081700980312     C     *LIKE         DEFINE    ARSTRC        WTRC                                       NTO
081800940720     C*
081900940720     C                   Z-ADD     1             KKUT
082000970805     C*
082100970805     C* Eseguo caricamento della tabella dei codici consegna anomala
082200970805     C                   Z-ADD     0             X                 3 0
082300970805     C                   MOVEL     '7O'          KCOD
082400970805     C     KTAB2         CHAIN     TABEL                              34
082500970805     C     *IN34         DOWEQ     '0'
082600970805     C     X             ANDLT     100
082700970805     C     TBLFLG        IFEQ      *BLANKS
082800970805     C                   ADD       1             X
082900970805     C                   MOVEL     TBLKEY        C7O(X)
083000970805     C                   MOVEL     TBLUNI        DS7O
083100970805     C                   MOVEL     §7OIDD        F7O(X)
083200970805     C                   END
083300970805     C     KTAB2         READE     TABEL                                  34
083400970805     C                   END
083500120830     C*
083600120830     C* definisco se lo schermo dell'utente supporta o meno il 27X132
083700160919     C                   IF        not %open(FNLRU38D) and
083800160919     C                             not %open(FNLRU38D1)
083900120830     C                   EVAL      dsp_mod = QueryDisplayModeSupport
084000120830     C                              ('4':*omit :*omit :*omit)
084100120830     C                   IF        dsp_mod=0
084200120830     C***                EVAL      k_sflpag=17
084300160919     C                   OPEN      FNLRU38D
084400120830     C                   ELSE
084500120830     C***                EVAL      k_sflpag=17
084600160919     C                   OPEN      FNLRU38D1
084700120830     C                   ENDIF
084800120830     C                   ENDIF
084900940720     C*
085000940720     C                   ENDSR
085100160916     C**************************************************************************
085200160916     c     DATI_TES      BEGSR
085300160916     c* Trimmo rif mittente numerico
085400160916     c                   eval      vcdrmn=%trim(%editc(parrmn:'Z'))
085500160916
085600160916     C                   MOVEL     PARDSA        V1CDSA
085700160916     C                   MOVEL     PADLNA        VCDLNA                         DECODIFICA FILIALE
085800160916
085900160916     c* Destinatario (mittente solo nella vecchia int bolle part)
086000160916     c                   clear                   vcdcod
086100160916     c                   clear                   vcdcod1
086200160916     c                   if        parcod>*zeros
086300160916     c                   eval      vcdcod =parcod+'-'+parrsc
086400160916     c                   eval      vcdcod1=parcod+'-'+parrsc
086500160916     c                   else
086600160916     C                   MOVEL     PARRSc        VCDcod
086700160916     C                   MOVEL     PARRSc        VCDcod1
086800160916     c                   endif
086900160916     c                   if        partbo='A'
087000160922     c                   eval      vccdcl1='Destinat.'
087100160916     c                   eval      vccdcl ='Destinatario'
087200160916     c                   else
087300160922     c                   eval      vccdcl1 ='Mittente '
087400160922     c                   eval      vccdcl  ='Mittente    '
087500160916     c                   endif
087600160916     c*
087700160916     c* terminal
087800160916     c                   eval      vcdter=%editc(partfp:'X')+'/'+
087900160916     c                                    %editc(partfa:'X')
088000160916     c
088100160916     c                   eval      vcdpar=%editc(partfp:'X')
088200160916     c                   clear                   orgde5
088300160916     c                   clear                   orgdes
088400160916     C     PARtfa        CHAIN     AZORG01L
088500160916     c                   if        orgde5<>*blanks
088600160916     C                   eval      vcdarr=%editc(partfa:'X')+'-' +orgde5
088700160916     c                   else
088800160916     C                   eval      vcdarr=%editc(partfa:'X')+'-'+orgdes
088900160916     c                   endif
089000160916     C*
089100160916     C* 15 ON  - SI TRATTA DI BOLLA EXPORT
089200160916     C     PARLNA        CHAIN     AZORG01L
089300160916     C                   if        %found
089400160916     C                   movel     ORGde3        og143
089500160916     C                   eval      *in15 = (§OgNtw = 'EEX' or §OgNtw ='EUP' or
089600160916     C                                     §OgNtw ='FED' or §OgNtw = 'DPD')
089700160916     C                   endif
089800160916     C*
089900160916     C* IMPOSTA CAMPI PER INT SPUNTE
090000160916     c                   clear                   param1
090100160916     C                   Z-ADD     0             VI1NFV
090200160916     C                   MOVEL     *ZEROS        VI1LNP
090300160916     C                   MOVEL     PARLNA        VI1LNA
090400160916     C                   MOVEL     PARNRS        VI1NRS
090500160916     C                   MOVEL     9             VI1NPG
090600160916     C                   Z-ADD     0             VI1NCD
090700160916     C                   Z-ADD     0             VI1NCA
090800160916     C                   MOVEL     99            VI1NPS
090900160916     C                   MOVEL     'FNLR38R'     VI1PGM
091000160916     C                   MOVEL     '1'           VI1FLG
091100160916     C                   MOVEL     PARNRS        VI1bors
091200160916     C                   MOVEL     PARlnp        VI1bolp
091300160916     C                   MOVEL     PARnsp        VI1bosp
091400160916     C                   MOVEL     PARaas        VI1boan
091500160922     c*
091600160916     C*
091700160916     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI
091800160916     C                   EXSR      CARART
091900160916     c                   ENDSR
092000020225**
092100051129F11=Dati VDL
092200020226F11=CHI SONO
