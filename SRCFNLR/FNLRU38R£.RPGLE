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
016900161122     c
017000161122     c                   if        artdcm>0
017100161122     C     KARTold       chain     FnART01L
017200161122     c                   if        %found(fnart01l) and olddcm=0
017300161122     c                   clear                   kdcm
017400161122     c                   endif
017500161122     c                   endif
017600161122     c
017700161122     c                   if        artdcm=0 or kdcm=0
017800160919     c                   clear                   fnlru39ds
017900150618     c                   eval      i38tbo=partbo
018000150618     c                   eval      i38aas=artaas
018100150618     c                   eval      i38lnp=artlnp
018200150618     c                   eval      i38nrs=artnrs
018300150618     c                   eval      i38nsp=artnsp
018400150618     c                   eval      i38nsc=artnsc
018500160919     c                   call      'FNLRU39R'
018600160919     c                   parm                    fnlru39ds
018700150618     c                   parm                    s_fiartds
018800161115     c
018900150618     c                   eval      fiartds=s_fiartds
019000161115     c                   endif
019100160922     c
019200160922     c* Se la bolla è "da partire" lo visualizzo anche nel dett colli
019300160922     c                   if        artbga=' '
019400160922     c                   seton                                        12
019500160922     c                   eval      vccdcl1='DaPartire'
019600160922     c* In questo caso devo reimpostare il destinatario perchè non ho lo spazio per
019700160922     c*  scrivere il "da partire" in un altro posto
019800160922     c                   eval      vcdcod1='Dest.'+parrsc
019900160922     c                   endif
020000160922     c
020100160922     c* in R.I se arrivato almeno un collo
020200160922     c                   if        artdam>0
020300160922     c                   seton                                        13
020400160922     c                   endif
020500160922     c
020600150318
020700120620     c                   clear                   tipars
020800120620     c                   clear                   notars
020900120620     c                   clear                   XX
021000920813     C*
021100940720     C                   MOVEL     ARTNSC        VSCNSC                         NUMERO SEGNACOLLO
021200940720     C                   MOVEL     ARTFLP        VSCFLP                         FILIALE TRANSITO
021300120619     C* cod anomalia
021400940720     C                   MOVEL     *BLANKS       VSCCAN
021500160922     C                   MOVEL     *BLANKS       VSdCAN
021600090226     C                   MOVEL     *BLANKS       VSCFTA
021700920813     C     ARTCAN        IFNE      ' '
021800940720     C                   MOVEL     '3E'          KCOD
021900940720     C                   MOVEL     *BLANKS       KKEY
022000940720     C                   MOVEL     ARTCAN        KKEY
022100940720     C     KTAB1         CHAIN     TABEL00F                           31
022200120711     c  n31              movel     tbluni        ds3e
022300120711     c   31              clear                   ds3e
022400120711     c
022500160922     c                   eval      vsccan=artcan+'-'+
022600160922     c                                    %subst(§3ede12:1:9)+'  '+artfta
022700160922     c                   eval      vsdcan=artcan+'-'+
022800160922     c                                    %subst(§3ede12:1:11)
022900090226     c* FTA
023000120618     c                   eval      vscfta=artfta
023100920813     C                   END
023200910912     C* GIRO DATE:
023300090226     C* entrata
023400160922     c                   clear                   vscdse
023500160922     c                   clear                   vsddse
023600160922     c                   clear                   vscnpe
023700160922     c                   clear                   vsdnpe
023800160922     c                   clear                   vschse
023900160922     c
024000090226     C                   MOVEL     ARTDse        G02INV
024100090226     C                   Z-ADD     *ZEROS        G02DAT
024200090226     C                   MOVEL     '3'           G02ERR
024300090226     C                   CALL      'XSRDA8'
024400090226     C                   PARM                    WLBDAT
024500160920     C                   MOVEL     G02DAT        VSCDse            6 0
024600120621     C                   MOVE      G02DAT        due
024700120621     C                   MOVE      due           VSCDse
024800160919     c
024900090226     C     ARTdse        IFGT      0
025000160919     c                   eval      vsdnpe=%editc(artnpe:'X')+'|'
025100160920     c                   eval      vscnpe=%editc(artnpe:'X')
025200090226     C                   ELSE
025300160919     C                   MOVEL     '  |'         VSdNPe
025400160920     C                   MOVEL     '  '          VScNPe
025500160920     C                   END
025600120619     c* Orario
025700120621     c                   if        arthse>0
025800120621     c                   eval      vschse=%editw(arthse:'0 :  ')
025900120621     c                   endif
026000160919     c                   eval      vsddse ='|        '
026100160919     c                   select
026200160919     c                   when      artdse>0 and (artdie=0  or artdie=artdse)
026300160919     c                   eval      vsddse=%trim(vsddse) +
026400160919     c                                    %editw(vscdse:'  /  /  ')
026500160919
026600160919     c* se data immissione <> data arrivo la visualizzo
026700160919     c                   when      artdie>0 and artdse>0 and artdie<>artdse
026800160919     c                   movel     g02dat        wggmm             4 0
026900160919     c                   eval      vsddse=%trim(vsddse) +
027000160919     c                                    %editc(wggmm:'X')
027100160919
027200160919     C                   MOVEL     ARTDie        G02INV
027300160919     C                   Z-ADD     *ZEROS        G02DAT
027400160919     C                   MOVEL     '3'           G02ERR
027500160919     C                   CALL      'XSRDA8'
027600160919     C                   PARM                    WLBDAT
027700160919     c                   movel     g02dat        wggmm             4 0
027800160920     c                   eval      vsddse=%trim(vsddse) +'|' +%editc(wggmm:'X')
027900160919     c                   endsl
028000910912     C* PARTENZA MERCE
028100160922     c                   clear                   vscdfv
028200160922     c                   clear                   vsddfv
028300160922     c                   clear                   vsdnpp
028400160922     c                   clear                   vscnpp
028500160922     c
028600940720     C                   MOVEL     ARTDFV        G02INV
028700940720     C                   Z-ADD     *ZEROS        G02DAT
028800940720     C                   MOVEL     '3'           G02ERR
028900940720     C                   CALL      'XSRDA8'
029000940720     C                   PARM                    WLBDAT
029100940720     C                   MOVEL     G02DAT        VSCDFV
029200120620     C                   MOVE      G02DAT        DUE               2 0          ANNO 2 CIFRE
029300950116     C                   MOVE      DUE           VSCDFV
029400160920     c                   eval      vsddfv ='|        '
029500160920     c                   if        artdfv>0
029600160920     c                   eval      vsddfv=%trim(vsddfv) +
029700160920     c                                    %editw(vscdfv:'  /  /  ')
029800160920     c                   endif
029900160920
030000921231     C     ARTDFV        IFGT      0
030100940720     C                   MOVEL     ARTNPP        VSCNPP
030200160920     c                   eval      vsdnpp= %editc(artnpp:'X')+'|'
030300921231     C                   ELSE
030400940720     C                   MOVEL     '  '          VSCNPP
030500160920     C                   MOVEL     '  |'         VSdNPP
030600921231     C                   END
030700921230     C* ENTRATA AL TRANSITO
030800160922     c                   clear                   vscdet
030900160922     c                   clear                   vsc6det
031000160922     c                   clear                   vsdpet
031100160922     c                   clear                   vscpet
031200160922     c
031300941219     C                   MOVEL     ARTDET        G02INV                         AAAA/MM/GG
031400940720     C                   Z-ADD     *ZEROS        G02DAT
031500940720     C                   MOVEL     '3'           G02ERR
031600940720     C                   CALL      'XSRDA8'
031700940720     C                   PARM                    WLBDAT
031800160922     C                   MOVEL     G02DAT        VSCDET                         GG/MM
031900160922     C                   MOVEL     G02DAT        VSC6DET                        GG/MM/aa
032000160920     C                   MOVE      G02DAT        due
032100160922     C                   MOVE      due           vsc6det                        GG/MM/AA
032200921231     C     ARTDET        IFGT      0
032300940720     C                   MOVEL     ARTPET        VSCPET
032400160920     C                   EVAL      VSDPET=%editc(artpet:'X')+'|'
032500921231     C                   ELSE
032600940720     C                   MOVEL     '  '          VSCPET
032700160920     C                   MOVEL     '  |'         VSdPET
032800921231     C                   END
032900910912     C* USCITA DAL TRANSITO
033000160922     c                   clear                   vscdut
033100160922     c                   clear                   vscput
033200160922     c                   clear                   vsdput
033300160922     c
033400940720     C                   MOVEL     ARTDUT        G02INV
033500940720     C                   Z-ADD     *ZEROS        G02DAT
033600940720     C                   MOVEL     '3'           G02ERR
033700940720     C                   CALL      'XSRDA8'
033800940720     C                   PARM                    WLBDAT
033900940720     C                   MOVEL     G02DAT        VSCDUT
034000120619     C                   MOVE      G02DAT        due
034100120619     C                   MOVE      due           vscdut
034200921231     C     ARTDUT        IFGT      0
034300160920     C                   MOVEL     ARTPUT        VSCPUT            2
034400160920     c                   eval      vsdput=%editc(artput:'X')+'|'
034500921231     C                   ELSE
034600940720     C                   MOVEL     '  '          VSCPUT
034700160920     C                   MOVEL     '  |'         VSdPUT
034800921231     C                   END
034900160922
035000910912     C* ARRIVO MERCE ALLA FIL ARRIVO
035100160922     c                   clear                   vsddam
035200160922     c                   clear                   vsdnap
035300160922     c                   clear                   vscnap
035400160922     c                   clear                   vscham
035500160916     c                   eval      vscflnap=artflnap
035600160916     c
035700940720     C                   MOVEL     ARTDAM        G02INV
035800940720     C                   Z-ADD     *ZEROS        G02DAT
035900940720     C                   MOVEL     '3'           G02ERR
036000940720     C                   CALL      'XSRDA8'
036100940720     C                   PARM                    WLBDAT
036200160920     C                   MOVEL     G02DAT        VSCDAM            6 0
036300950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
036400950116     C                   MOVE      DUE           VSCDAM
036500921231     C     ARTDAM        IFGT      0
036600940720     C                   MOVEL     ARTNAP        VSCNAP
036700160920     c                   eval      vsdnap=%editc(artnap:'X') + '|'
036800921231     C                   ELSE
036900940720     C                   MOVEL     '  '          VSCNAP
037000160920     C                   MOVEL     '  |'         VSdNAP
037100921231     C                   END
037200120621     c                   if        artham>0
037300120621     c                   eval      vscham=%editw(artham:'0 :  ')
037400120621     c                   endif
037500160916
037600160916     c                   select
037700160916     c                   when      artdam>0 and (artdia=0  or artdia=artdam)
037800160920     c                   eval      vsddam=%editw(g02dat:'  /  /    ')
037900160916
038000160916     c* se data immissione <> data arrivo la visualizzo
038100160916     c                   when      artdia>0 and artdam>0 and artdia<>artdam
038200160916     c                   movel     g02dat        wggmm             4 0
038300160920     c                   eval      vsddam=%editw(wggmm:'  /  ')
038400160916
038500160916     C                   MOVEL     ARTDia        G02INV
038600160916     C                   Z-ADD     *ZEROS        G02DAT
038700160916     C                   MOVEL     '3'           G02ERR
038800160916     C                   CALL      'XSRDA8'
038900160916     C                   PARM                    WLBDAT
039000160916     c                   movel     g02dat        wggmm             4 0
039100160920     c                   eval      vsddam=%trim(vsddam) +'|' +
039200160920     c                                    %editw(wggmm:'  /  ')
039300160916     c                   endsl
039400160916     c
039500910912     C* CONSEGNA MERCE REALE
039600090226     c                   clear                   vsccon
039700090226     c                   setoff                                       01
039800090226     c                   if        artdcm>0
039900940720     C                   MOVEL     ARTDCM        G02INV
040000940720     C                   Z-ADD     *ZEROS        G02DAT
040100940720     C                   MOVEL     '3'           G02ERR
040200940720     C                   CALL      'XSRDA8'
040300940720     C                   PARM                    WLBDAT
040400090226     C                   MOVEL     G02DAT        wdcm
040500950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
040600090226     C                   MOVE      DUE           wDCM
040700120618     c                   eval      vsccon= %editw(wdcm: '  /  /  ')
040800120618     c                   if        arthcm>0
040900160922     c                   eval      vsccon= %editw(wdcm: '  /  /  ')+' '+
041000120618     c                             %editw(arthcm: '  :  ')
041100090226     c                   endif
041200120618     c                   endif
041300120619     c
041400120619     c* ARRIVO FINALE
041500160920     c                   clear                   vscdamf           6 0
041600160922     c                   clear                   vsddamf
041700120619     c                   clear                   vschamf
041800120619     c                   clear                   vscnapf
041900160920     c                   clear                   vsdnapf
042000161020     c                   clear                   vsccan_p
042100160922
042200120619     c* Se non si tratta di bolla export, imposto la data all'arrivo finale
042300160920    2c                   if        not *in15
042400120619     C                   MOVEL     ARTDAMF       G02INV
042500120619     C                   Z-ADD     *ZEROS        G02DAT
042600120619     C                   MOVEL     '3'           G02ERR
042700120619     C                   CALL      'XSRDA8'
042800120619     C                   PARM                    WLBDAT
042900120619     C                   MOVEL     G02DAT        VSCDAMf
043000120619     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
043100120619     C                   MOVE      DUE           VSCDAMf
043200160920     c
043300160920    3C     ARTnapf       IFGT      0
043400120619     C                   MOVEL     ARTNAPf       VSCNAPf
043500160920     c                   eval      vsdnapf=%editc(artnapf:'X')+ '|'
043600120619     C                   ELSE
043700120619     C                   MOVEL     '  '          VSCNAPf
043800160920     C                   MOVEL     '  |'         VSdNAPf
043900160920    3C                   END
044000160920    3c                   if        arthamf>0
044100120621     c                   eval      vschamf=%editw(arthamf:'0 :  ')
044200160920    3c                   endif
044300160916     c
044400160920    3c                   select
044500160916     c                   when      artdamf>0 and (artdiaf=0  or artdiaf=artdamf)
044600160920     c                   eval      vsddamf=%editw(g02dat:'  /  /    ')
044700160916
044800160916     c* se data immissione <> data arrivo la visualizzo
044900160916     c                   when      artdiaf>0 and artdamf>0 and artdiaf<>artdamf
045000160916     c                   movel     g02dat        wggmm             4 0
045100160916     c                   eval      vsddamf=%trim(vsddamf) +
045200160920     c                                     %editw(wggmm :'  /  ')
045300160916
045400160916     C                   MOVEL     ARTDiaf       G02INV
045500160916     C                   Z-ADD     *ZEROS        G02DAT
045600160916     C                   MOVEL     '3'           G02ERR
045700160916     C                   CALL      'XSRDA8'
045800160916     C                   PARM                    WLBDAT
045900160916     c                   movel     g02dat        wggmm             4 0
046000160920     c                   eval      vsddamf=%trim(vsddamf) +'|' +
046100160920     c                                    %editw(wggmm:'  /  ')
046200160920    3c                   endsl
046300120619     c
046400160920    2c                   endif
046500910912     C*
046600090226     C* SOLO PER LE BOLLE EXPORT (15 ON) VISUALIZZO LA DATA manifest
046700161019     c* nel campo DAMF
046800961112     C     *IN15         IFEQ      *ON
046900161019     c                   clear                   vschamf
047000161019     c                   clear                   vsddamf
047100161019     C                   MOVEL     '  '          VSCNAPf
047200161019     c                   if        artdmf>0
047300090226     C                   MOVEL     ARTDmf        G02INV
047400961112     C                   Z-ADD     *ZEROS        G02DAT
047500961112     C                   MOVEL     '3'           G02ERR
047600961112     C                   CALL      'XSRDA8'
047700961112     C                   PARM                    WLBDAT
047800090226     C                   MOVEL     G02DAT        VSCDmf
047900961112     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
048000090226     C                   MOVE      DUE           VSCDmf
048100160920     c                   eval      vsddamf=%editw(g02dat:'  /  /    ')
048200161019     c
048300161019     c* il campo ORA contiene la pistola di spunta dell'invio al partner per video a 80
048400161019     C                   IF        dsp_mod = 0
048500161019     C                   MOVE      ARTNpdmf      VSChamf
048600161019     c                   endif
048700161019     c
048800160920     c                   eval      vsdnapf=%editc(artnpdmf:'X')+ '|'
048900160920     C                   ELSE
049000160920     C                   MOVEL     '  |'         VSdNAPf
049100961112     C                   ENDIF
049200160920     C                   ENDIF
049300000211      *
049400000211      * Aggancio estensione dettaglio segnacolli
049500020226      * A seconda della linea di arrivo aggancio prima tipo righe E o F
049600020226     C     §OGNTW        IFEQ      'FED'
049700020226     C                   MOVE      'F'           WTRC
049800020226     C                   ELSE
049900020226     C                   MOVE      'E'           WTRC
050000020226     C                   ENDIF
050100051013     C     KARS1         CHAIN     FIARS01L                           31
050200120619    1C     *IN31         IFEQ      *OFF
050300000911      *
050400020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
050500120619    2C     ARSNOT        IFNE      *BLANKS
050600120620     c                   add       1             xx
050700120620     c                   movel     wtrc          tipars(xx)
050800120620     C                   MOVEL     ARSNOT        NOTars(xx)
050900120619    2C                   ENDIF
051000011031      *
051100011031      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
051200121107     c                   if        *in15
051300161019     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
051400161019     c* nel campo PISTOLTa di spunta lna finale
051500161020     c* video 80
051600161019     C                   MOVEL     ARSCAN        VSCnapf
051700161020     c* video 132
051800161020     C                   MOVEL     ARSCAN        VSCcan_P
051900121107     c
052000161019     c                   if        arsdat>0
052100011031     C                   MOVEL     ARSDAT        G02INV            8 0
052200011031     C                   Z-ADD     *ZEROS        G02DAT
052300011031     C                   MOVEL     '3'           G02ERR
052400011031     C                   CALL      'XSRDA8'
052500011031     C                   PARM                    WLBDAT
052600161019     C                   MOVEL     G02DAT        w0040             4 0
052700161019     c                   eval      vsddamp=%editw(g02dat:'  /  /    ')+
052800161019     c                             ' ' + arscan
052900161019     c                   if        dsp_mod=0
053000161019     c                   clear                   vsccan
053100161019     c                   eval      vsccan='ArrP '+%editw(w0040: '  /  ')+
053200161019     c                             '   ' + artcan
053300121107     c                   endif
053400161019     c                   endif
053500161019     c                   endif
053600011031      *
053700120620   x1C                   ELSE
053800000911     C                   CLEAR                   VSCVA2
053900120619    1C                   ENDIF
054000000911      *
054100000911     C                   MOVEL     'C'           WTRC
054200051013     C     KARS1         CHAIN     FIARS01L                           31
054300120620    1C     *IN31         IFEQ      *OFF
054400000211      *
054500000911      * VERIFICO PRESENZA CODICE "CHI SONO"
054600120620    2C     ARSNOT        IFNE      *BLANKS
054700120620     c                   add       1             xx
054800120620     c                   movel     wtrc          tipars(xx)
054900120620     C                   MOVEL     ARSNOT        NOTars(xx)
055000120620    2C                   ENDIF
055100000211      *
055200120620    1C                   endif
055300000211      *
055400090226      * SE NON C'È FIARS VISUALIZZO IL VOLUME VDL
055500980312     C                   CLEAR                   VAR
055600120831     C                   MOVEA     'Vol'         VAR(19)
055700120620    2C     ARTVUC        IFGT      0
055800120620     c                   eval      wnume=%editc(artvuc:'4')
055900120831     C                   MOVEA     WNUME         VAR(23)
056000120831     C                   MOVEA     ARTFVC        VAR(32)
056100120620    2C                   END
056200120831     C                   MOVEA     'Peso'        VAR(1)
056300120620    2C     ARTPUC        IFGT      0
056400120620     c                   eval      wnume=%editc(artpuc:'4')
056500120831     C                   MOVEA     WNUME         VAR(6)
056600120831     C                   MOVEA     ARTFPC        VAR(16)
056700120620    2C                   END
056800120620     c
056900120620     c                   add       1             xx
057000120620     c                   movel     ' '           tipars(xx)
057100120620     C                   MOVEa     var           NOTars(xx)
057200000211      *
057300090226      * SEGNALO SE SEGNACOLLO con consegna anomala
057400090226     c                   if        artcca<>' '
057500970805     C                   Z-ADD     1             X
057600090226     C     ARTcca        LOOKUP    C7O(X)                                 34
057700970805     C     *IN34         IFEQ      '1'
057800970805     C     F7O(X)        ANDNE     *BLANKS
057900160922     c                   eval      %subst(vsccon:10:5)=' IDD '
058000160922     c                   seton                                        01
058100970805     C                   END
058200090226     c
058300090226     c                   else
058400090226     c* Se disabilitato visualizzo
058500090226     c                   if        artdcm=0 and artdab='S'
058600090226     c                   eval      vsccon='MANCANTE'
058700090226     c                   seton                                        01
058800090226     c                   endif
058900090226     c                   endif
059000961112     C*
059100120620     c                   exsr      AbiF11
059200160920     c* se il campo vsc7h1 vuoto, metto una | di separazione
059300160920     c                   if        vsc7h1=*blanks
059400160920     c                   eval      vsc7h1='        |  '
059500160920     c                   endif
059600120620     c
059700910205     C* SCRIVO SUBFILE
059800940720     C                   ADD       1             NRR1
059900120830     C                   IF        dsp_mod = 1
060000120830     C                   WRITE     LR38S011
060100120830     C                   ELSE
060200120830     C                   WRITE     LR38S01
060300120830     C                   ENDIF
060400910912     C*
060500090226     C     KART1         READE     FiART01L                               30
060600910205     C                   END
060700920803     C*
060800920803     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
060900160609     c     nrr1          ifgt      0
061000160609     C     ARTNSC        andne     VI1NCD
061100940720     C                   MOVEL     ARTNSC        VI1NCA
061200920803     C                   END
061300910205     C*
061400940720     C                   Z-ADD     NRR1          SAVNRR            4 0
061500921104     C*
061600940720     C     NRR1          IFEQ      0
061700120830     C                   IF        dsp_mod = 1
061800120830     c                   clear                   lr38s011
061900120830     C                   ELSE
062000120830     c                   clear                   lr38s01
062100120830     C                   ENDIF
062200940720     C                   ADD       1             NRR1
062300120830     C                   IF        dsp_mod = 1
062400120830     C                   WRITE     LR38S011
062500120830     C                   ELSE
062600120830     C                   WRITE     LR38S01
062700120830     C                   ENDIF
062800921104     C                   END
062900910205     C*
063000910205     C                   ENDSR
063100020226     C**************************************************************************
063200020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11
063300020226     C**************************************************************************
063400020226     C     ABIF11        BEGSR
063500120620     c                   clear                   vsc7h1
063600120620     c                   clear                   vsh7h2
063700120620     c                   clear                   vsh7h3
063800120830     C                   IF        dsp_mod = 1
063900120830     c                   clear                   vsc7h11
064000120830     c                   clear                   vsh7h31
064100120830     C                   ENDIF
064200120620     c                   clear                   vscvar
064300120620     c                   clear                   vscva2
064400120620     c                   clear                   vshnot2
064500120620     c                   clear                   vshnot3
064600120620     c*
064700120620     c* se  presenti tutti e 3 accendo F11
064800120620     c                   if        xx=3
064900020226     C                   seton                                        17
065000120620     C                   movel     FunF11(1)     VZCF11
065100120620     C                   MOVEL     'S'           WINFO             1
065200120620     c                   endif
065300020226      *
065400120620     c* Visualizzo i primi due
065500120620     c                   eval      vscva2=notars(1)
065600120621    1c                   if        tipars(1)<>*blanks
065700120620     c
065800120620     C                   MOVE      '7H'          KCOD
065900120620     C                   MOVEL(P)  tipars(1)     KKEY
066000120620     C     KTAB1         CHAIN     TABEL00F                           38
066100120620     C  N38              MOVEL     TBLUNI        DS7H
066200120620     C   38              clear                   DS7H
066300120620    2c                   if        tipars(2)=' '
066400120620     C                   MOVEL     §7HDEB        VSc7H1
066500120620   x2c                   else
066600120830     C                   IF        dsp_mod = 1
066700120830     C                   MOVEL     §7HDEB        VSc7H1
066800120830     C                   ELSE
066900120620     c                   eval      vsc7h1=§7HDEB6+'/'
067000120830     C                   ENDIF
067100120620     c* prima descrizione completa
067200120620     C                   MOVEL     §7HDEB        VSh7H2
067300120620    2c                   endif
067400120620    1c                   endif
067500120620     c
067600120620     c                   eval      vscvar=notars(2)
067700120621    1c                   if        tipars(2)<>*blanks
067800120620     C                   MOVE      '7H'          KCOD
067900120620     C                   MOVEL(P)  tipars(2)     KKEY
068000120620     C     KTAB1         CHAIN     TABEL00F                           38
068100120620     C  N38              MOVEL     TBLUNI        DS7H
068200120620     C   38              clear                   DS7H
068300120620    2c                   if        tipars(1)=' '
068400120620     C                   MOVEL     §7HDEB        VSc7H1
068500120620     c                   else
068600120830     C                   IF        dsp_mod = 1
068700120830     C                   MOVEL     §7HDEB        VSc7H11
068800120830     c* 2 descrizioni perché 132 col.  (le salvo entrambe)
068900120830     c                   eval      vsh7h3=vsc7h1
069000120830     c                   eval      vsh7h31=vsc7h11
069100120830     C                   ELSE
069200120620     c                   eval      vsc7h1=%trim(vsc7h1)+§7HDEB6
069300120830     c* 1 descrizione doppia perché 80 col. (ne salvo una sola)
069400120620     c                   eval      vsh7h3=vsc7h1
069500120830     C                   ENDIF
069600120620    2c                   endif
069700120620    1c                   endif
069800120620     c
069900120620     c                   eval      vshnot2=notars(2)
070000120620     c                   eval      vshnot3=notars(3)
070100020226      *
070200020226     C                   ENDSR
070300980312     C**************************************************************************
070400120620     C* AGGIORNAMENTO SUBFILE PER INTERROGAZIONE ALTRI DATI
070500980312     C**************************************************************************
070600980312     C     UPDS01        BEGSR
070700980312     C                   Z-ADD     1             NRR1
070800120830     C                   IF        dsp_mod = 1
070900120830     C     NRR1          CHAIN     LR38S011                           31
071000120830     C                   ELSE
071100120830     C     NRR1          CHAIN     LR38S01                            31
071200120830     C                   ENDIF
071300980312     C     *IN31         DOWEQ     *OFF
071400120620     c
071500120620     C* VISUALIZZO dati vdl al posto del chi sono --> record 3 col record 2
071600120620     C                   if        WINFO='S'
071700120620     c                   eval      vsc7h1=vsh7h2
071800120830     C                   IF        dsp_mod = 1
071900120830     c                   eval      vsc7h11=*blank
072000120830     C                   ENDIF
072100120620     c                   eval      vscvar=vshnot3
072200120620     c
072300980312     C                   ELSE
072400120620     C* VISUALIZZO "CHI SONO" al posto di dati VDL --> record 2 al posto di record 3
072500120620     c                   eval      vsc7h1=vsh7h3
072600120830     C                   IF        dsp_mod = 1
072700120830     c                   eval      vsc7h11=vsh7h31
072800120830     C                   ENDIF
072900120620     c                   eval      vscvar=vshnot2
073000980312     C                   END
073100020226      *
073200120830     C                   IF        dsp_mod = 1
073300120830     C                   UPDATE    LR38S011
073400120830     C                   ELSE
073500120830     C                   UPDATE    LR38S01
073600120830     C                   ENDIF
073700980312     C                   ADD       1             NRR1
073800120830     C                   IF        dsp_mod = 1
073900120830     C     NRR1          CHAIN     LR38S011                           31
074000120830     C                   ELSE
074100120830     C     NRR1          CHAIN     LR38S01                            31
074200120830     C                   ENDIF
074300980312     C                   ENDDO
074400120620     c
074500120620     c                   if        WINFO='S'
074600120620     C                   movel     FunF11(2)     VZCF11
074700120620     c                   eval      winfo='V'
074800120620     c                   else
074900120620     C                   movel     FunF11(1)     VZCF11
075000120620     c                   eval      winfo='S'
075100120620     c                   endif
075200120620     c
075300980312     C                   ENDSR
075400940720     C**************************************************************************
075500940720     C* PULIZIA SUBFILE
075600940720     C**************************************************************************
075700910205     C     PULSF         BEGSR
075800940720     C*
075900910205     C                   SETON                                        35
076000120830     C                   IF        dsp_mod = 1
076100120830     C                   WRITE     LR38C011                                     SFLCTL
076200120830     C                   ELSE
076300120830     C                   WRITE     LR38C01                                      SFLCTL
076400120830     C                   ENDIF
076500910205     C                   SETOFF                                       35
076600940720     C*
076700910205     C                   ENDSR
076800940720     C**************************************************************************
076900940720     C*  ROUTINE  INIZIALE
077000940720     C**************************************************************************
077100940720     C     *INZSR        BEGSR
077200940720     C*
077300090226     C     KART1         KLIST                                                  FiiRT01L
077400940720     C                   KFLD                    PARAAS                         ..ANNO
077500940720     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
077600940720     C                   KFLD                    PARNRS                         ..NUMERO SERIE
077700940720     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
077800161122     C     KARTold       KLIST                                                  FiiRT01L
077900161122     C                   KFLD                    PARAAS                         ..ANNO
078000161122     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
078100161122     C                   KFLD                    PARNRS                         ..NUMERO SERIE
078200161122     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
078300161122     C                   KFLD                    artnsc
078400150326     C     KART27        KLIST                                                  FiiRT01L
078500150326     C                   KFLD                    artfls                         ..ANNO
078600150326     C                   KFLD                    artlna                         ..LINEA PARTENZA
078700150326     C                   KFLD                    artNRS                         ..NUMERO SERIE
078800150326     C                   KFLD                    artnsc                         ..NUMERO SPEDIZIONE
078900940720     C     KTAB1         KLIST                                                  TABEL00F
079000940720     C                   KFLD                    KKUT                           ..CODICE UTENTE
079100940720     C                   KFLD                    KCOD                           ..CODICE TABELLA
079200940720     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
079300970805     C     KTAB2         KLIST                                                  TABEL00F
079400970805     C                   KFLD                    KKUT                           ..CODICE UTENTE
079500970805     C                   KFLD                    KCOD                           ..CODICE TABELLA
079600090226     C     KARS1         KLIST                                                  FiART01L
079700980312     C                   KFLD                    ARTFLS
079800980312     C                   KFLD                    ARTLNA                                     ENZA
079900980312     C                   KFLD                    ARTNRS                                     IE
080000980312     C                   KFLD                    ARTNSC                                     DIZIONE
080100980312     C                   KFLD                    WTRC                                       DIZIONE
080200940720     C*
080300940720     C     *LIKE         DEFINE    TBLKUT        KKUT                           CODICE UTENTE
080400940720     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
080500940720     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
080600980312     C     *LIKE         DEFINE    ARSTRC        WTRC                                       NTO
080700940720     C*
080800940720     C                   Z-ADD     1             KKUT
080900970805     C*
081000970805     C* Eseguo caricamento della tabella dei codici consegna anomala
081100970805     C                   Z-ADD     0             X                 3 0
081200970805     C                   MOVEL     '7O'          KCOD
081300970805     C     KTAB2         CHAIN     TABEL                              34
081400970805     C     *IN34         DOWEQ     '0'
081500970805     C     X             ANDLT     100
081600970805     C     TBLFLG        IFEQ      *BLANKS
081700970805     C                   ADD       1             X
081800970805     C                   MOVEL     TBLKEY        C7O(X)
081900970805     C                   MOVEL     TBLUNI        DS7O
082000970805     C                   MOVEL     §7OIDD        F7O(X)
082100970805     C                   END
082200970805     C     KTAB2         READE     TABEL                                  34
082300970805     C                   END
082400120830     C*
082500120830     C* definisco se lo schermo dell'utente supporta o meno il 27X132
082600160919     C                   IF        not %open(FNLRU38D) and
082700160919     C                             not %open(FNLRU38D1)
082800120830     C                   EVAL      dsp_mod = QueryDisplayModeSupport
082900120830     C                              ('4':*omit :*omit :*omit)
083000120830     C                   IF        dsp_mod=0
083100120830     C***                EVAL      k_sflpag=17
083200160919     C                   OPEN      FNLRU38D
083300120830     C                   ELSE
083400120830     C***                EVAL      k_sflpag=17
083500160919     C                   OPEN      FNLRU38D1
083600120830     C                   ENDIF
083700120830     C                   ENDIF
083800940720     C*
083900940720     C                   ENDSR
084000160916     C**************************************************************************
084100160916     c     DATI_TES      BEGSR
084200160916     c* Trimmo rif mittente numerico
084300160916     c                   eval      vcdrmn=%trim(%editc(parrmn:'Z'))
084400160916
084500160916     C                   MOVEL     PARDSA        V1CDSA
084600160916     C                   MOVEL     PADLNA        VCDLNA                         DECODIFICA FILIALE
084700160916
084800160916     c* Destinatario (mittente solo nella vecchia int bolle part)
084900160916     c                   clear                   vcdcod
085000160916     c                   clear                   vcdcod1
085100160916     c                   if        parcod>*zeros
085200160916     c                   eval      vcdcod =parcod+'-'+parrsc
085300160916     c                   eval      vcdcod1=parcod+'-'+parrsc
085400160916     c                   else
085500160916     C                   MOVEL     PARRSc        VCDcod
085600160916     C                   MOVEL     PARRSc        VCDcod1
085700160916     c                   endif
085800160916     c                   if        partbo='A'
085900160922     c                   eval      vccdcl1='Destinat.'
086000160916     c                   eval      vccdcl ='Destinatario'
086100160916     c                   else
086200160922     c                   eval      vccdcl1 ='Mittente '
086300160922     c                   eval      vccdcl  ='Mittente    '
086400160916     c                   endif
086500160916     c*
086600160916     c* terminal
086700160916     c                   eval      vcdter=%editc(partfp:'X')+'/'+
086800160916     c                                    %editc(partfa:'X')
086900160916     c
087000160916     c                   eval      vcdpar=%editc(partfp:'X')
087100160916     c                   clear                   orgde5
087200160916     c                   clear                   orgdes
087300160916     C     PARtfa        CHAIN     AZORG01L
087400160916     c                   if        orgde5<>*blanks
087500160916     C                   eval      vcdarr=%editc(partfa:'X')+'-' +orgde5
087600160916     c                   else
087700160916     C                   eval      vcdarr=%editc(partfa:'X')+'-'+orgdes
087800160916     c                   endif
087900160916     C*
088000160916     C* 15 ON  - SI TRATTA DI BOLLA EXPORT
088100160916     C     PARLNA        CHAIN     AZORG01L
088200160916     C                   if        %found
088300160916     C                   movel     ORGde3        og143
088400160916     C                   eval      *in15 = (§OgNtw = 'EEX' or §OgNtw ='EUP' or
088500160916     C                                     §OgNtw ='FED' or §OgNtw = 'DPD')
088600160916     C                   endif
088700160916     C*
088800160916     C* IMPOSTA CAMPI PER INT SPUNTE
088900160916     c                   clear                   param1
089000160916     C                   Z-ADD     0             VI1NFV
089100160916     C                   MOVEL     *ZEROS        VI1LNP
089200160916     C                   MOVEL     PARLNA        VI1LNA
089300160916     C                   MOVEL     PARNRS        VI1NRS
089400160916     C                   MOVEL     9             VI1NPG
089500160916     C                   Z-ADD     0             VI1NCD
089600160916     C                   Z-ADD     0             VI1NCA
089700160916     C                   MOVEL     99            VI1NPS
089800160916     C                   MOVEL     'FNLR38R'     VI1PGM
089900160916     C                   MOVEL     '1'           VI1FLG
090000160916     C                   MOVEL     PARNRS        VI1bors
090100160916     C                   MOVEL     PARlnp        VI1bolp
090200160916     C                   MOVEL     PARnsp        VI1bosp
090300160916     C                   MOVEL     PARaas        VI1boan
090400160922     c*
090500160916     C*
090600160916     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI
090700160916     C                   EXSR      CARART
090800160916     c                   ENDSR
090900020225**
091000051129F11=Dati VDL
091100020226F11=CHI SONO
