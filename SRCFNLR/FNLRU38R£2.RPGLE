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
001400051013     FFIARS01L  IF   E           K DISK
001500980312     FAZORG01L  IF   E           K DISK
001600970805      *--------------------------------------------------------------*
001700970805     D* Schiera codici consegne anomale + relativi flag
001800970805     D C7O             S              1    DIM(200)                             cod.cons.anom
001900970805     D F7O             S              1    DIM(200)                             flg cons.anom
002000120620     D TIPars          S              1    DIM(3)
002100120620     D NOTars          S             35    DIM(3)
002200120831     D  VAR            S              1    dim(32)
002300120620     D FUNF11          S             38    DIM(2) CTDATA PERRCD(1)
002400970805      *--------------------------------------------------------------*
002500911024     D PARAM           DS
002600940720     D  PARAAS                14     17  0
002700940720     D  PARLNP                18     20  0
002800940720     D  PARNRS                21     22  0
002900940720     D  PARNSP                23     29  0
003000940720     D  PARF03                30     30
003100940720     D  PARFLG                31     31
003200940720     D  PARPGM                32     41
003300940720     D  PARDSA                42     61
003400940720     D  PARDSP                63     70  0
003500940720     D  PARRMN                71     85  0
003600940720     D  PARLNA                86     88  0
003700940720     D  PADLNA                89    108
003800090226     D  PARRSc               109    143
003900150619     D  PARTBO               148    148
004000150619     D  PARcod               149    155
004100160916     D  PARTFP               156    158  0
004200160916     D  PARTFA               159    161  0
004300120620     d
004400941230     D PARAM1          DS                  INZ
004500920803     D  VI1NFV                 1      5  0
004600920803     D  VI1LNP                 6      8  0
004700920803     D  VI1LNA                 9     11  0
004800920803     D  VI1NRS                12     13  0
004900920803     D  VI1NCD                14     20  0
005000920803     D  VI1NCA                21     27  0
005100920803     D  VI1NPS                28     29  0
005200920803     D  VI1NPG                30     30  0
005300920813     D  VI1FLG                31     31
005400920803     D  VI1PGM                32     38
005500941230     D  VI1FGS                39     41  0
005600120621     d* bolla
005700120621     d  vi1bolp               42     44  0
005800120621     d  vi1bors               45     46  0
005900120621     d  vi1bosp               47     53  0
006000120621     d  vi1boan               54     57  0
006100120621     D  PA1ERR                58     58
006200940720     D WLBDAT          DS                  INZ
006300940720     D  G02DAT                 1      8  0
006400940720     D  G02INV                 9     16  0
006500940720     D  G02ERR                17     17
006600940720     D  G02TGI                18     22  0
006700120620     D
006800160919     d fnlru39ds     e ds
006900160916     d fiartds       e ds                  extname(fiart50f)
007000160916     d s_fiartds     e ds                  extname(fiart50F) prefix(s_)
007100961122     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
007200961122     D DSLR45        E DS                  EXTNAME(FNLR45DS)
007300120620     d
007400900517     D KPJBA         E DS
007500120711     D DS3E          E DS
007600120711     D DS7O          E DS
007700000911     D DS7H          E DS
007800020121     D OG143         E DS
007900090226     d wdcm            s              6  0
008000120620     D WNUME           s              8
008100120620     D XX              s              3  0
008200120830     d dsp_mod         s             10I 0
008300120830
008400120830     D* PROTOTIPI
008500120830      /copy gaitrasrc/srcprotopr,QsnQryModS
008600150618     i*fnart000
008700150618     i*              artnfv                      artnfv1
008800150318     i
008900940720     C*-------------------------------------------------------------------------
009000941110     C     *ENTRY        PLIST
009100941110     C                   PARM                    KPJBA
009200941110     C*
009300910219     C                   MOVEL     KPJBU         PARAM
009400090226     c* Imposto campi testata
009500160916     c                   exsr      Dati_tes
009600020226      *
009700900509     C     FOR01         TAG
009800120830     C                   IF        dsp_mod = 1
009900120830     C                   WRITE     LR38T011                                     TESTATA
010000120830     C                   WRITE     LR38Z011                                     TASTI FUNZIONALI
010100120830     C                   EXFMT     LR38C011                                     SFLCTL
010200120830     C                   ELSE
010300120830     C                   WRITE     LR38T01                                      TESTATA
010400120830     C                   WRITE     LR38Z01                                      TASTI FUNZIONALI
010500120830     C                   EXFMT     LR38C01                                      SFLCTL
010600120830     C                   ENDIF
010700940802     C*
010800980316     C                   SETOFF                                       11
010900950220     C                   CLEAR                   PARF03                         NO PREMUTO F3=FINE
011000940802     C   KC              MOVEL     '1'           PARF03                         PREMUTO F3=FINE
011100920803     C*
011200120620     C* F19 - INTERROGAZIONE SPUNTE
011300950125     C   KT              DO
011400950125     C                   MOVEL     PARAM1        KPJBU
011500950125     C                   CALL      'FNLR04R'                            99
011600920803     C                   PARM                    KPJBA
011700950125     C                   GOTO      FOR01
011800950125     C                   ENDDO
011900910219     C*
012000120620     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
012100961122     C     *INKH         IFEQ      *ON
012200961122     C                   CLEAR                   DSLR45
012300961122     C                   MOVE      PARAAS        D45AAS
012400961122     C                   MOVE      PARLNP        D45FIL
012500961122     C                   MOVE      PARNRS        D45NRS
012600961122     C                   MOVE      PARNSP        D45NSP
012700961122     C                   MOVEL     DSLR45        KPJBU
012800961122     C                   CALL      'FNLR45R'
012900961122     C                   PARM                    KPJBA
013000961122     C                   GOTO      FOR01
013100961122     C                   ENDIF
013200020226      *
013300120620     C* F11 - INTERROGAZIONE VOLUME/SEGNACOLLO EUROEXPRESS
013400980312     C     *INKK         IFEQ      *ON
013500020226      *
013600980316     C                   SETON                                        11
013700980312     C                   EXSR      UPDS01
013800020226      *
013900980312     C                   GOTO      FOR01
014000980312     C                   END
014100961122     C*
014200000000     C     FINE          TAG
014300150618     c                   movel     'C'           i38tla
014400160919     c                   call      'FNLRU39R'
014500160919     c                   parm                    fnlru39ds
014600150618     c                   parm                    s_fiartds
014700950125     C                   MOVEL     PARAM         KPJBU
014800150618
014900150618
015000000000     C                   SETON                                        LR
015100940720     C**************************************************************************
015200940720     C* CARICO DETTAGLIO SEGNACOLLI
015300940720     C**************************************************************************
015400090226     C     CARART        BEGSR
015500940720     C*
015600910205     C                   EXSR      PULSF
015700120620     c
015800940720     C                   Z-ADD     0             NRR1              4 0
015900160922     c
016000160922     C                   SETOFF                                       301213
016100090226     C     KART1         SETLL     FiART01L
016200090226     C     KART1         READE     FiART01L                               30
016300940720     C  N30              MOVEL     ARTNSC        VI1NCD
016400950116     C  N30              MOVEL     ARTFLS        VI1LNP
016500090226     C  N30              MOVEL     ARTFLS        Vccfls
016600910220     C*
016700910806     C     *IN30         DOWEQ     '0'
016800160919     c                   clear                   fnlru39ds
016900150618     c                   eval      i38tbo=partbo
017000150618     c                   eval      i38aas=artaas
017100150618     c                   eval      i38lnp=artlnp
017200150618     c                   eval      i38nrs=artnrs
017300150618     c                   eval      i38nsp=artnsp
017400150618     c                   eval      i38nsc=artnsc
017500160919     c                   call      'FNLRU39R'
017600160919     c                   parm                    fnlru39ds
017700150618     c                   parm                    s_fiartds
017800150618     c* In base ai campi di fiart già aggiornati dai pgm e che
017900150618     c* quindi non devo reperire valorizzo solo i campi di fiart che mi interessano
018000150618     c* ora li valorizzo tutti
018100150618     c                   eval      fiartds=s_fiartds
018200160922     c
018300160922     c* Se la bolla è "da partire" lo visualizzo anche nel dett colli
018400160922     c                   if        artbga=' '
018500160922     c                   seton                                        12
018600160922     c                   eval      vccdcl1='DaPartire'
018700160922     c* In questo caso devo reimpostare il destinatario perchè non ho lo spazio per
018800160922     c*  scrivere il "da partire" in un altro posto
018900160922     c                   eval      vcdcod1='Dest.'+parrsc
019000160922     c                   endif
019100160922     c
019200160922     c* in R.I se arrivato almeno un collo
019300160922     c                   if        artdam>0
019400160922     c                   seton                                        13
019500160922     c                   endif
019600160922     c
019700150318
019800120620     c                   clear                   tipars
019900120620     c                   clear                   notars
020000120620     c                   clear                   XX
020100920813     C*
020200940720     C                   MOVEL     ARTNSC        VSCNSC                         NUMERO SEGNACOLLO
020300940720     C                   MOVEL     ARTFLP        VSCFLP                         FILIALE TRANSITO
020400120619     C* cod anomalia
020500940720     C                   MOVEL     *BLANKS       VSCCAN
020600160922     C                   MOVEL     *BLANKS       VSdCAN
020700090226     C                   MOVEL     *BLANKS       VSCFTA
020800920813     C     ARTCAN        IFNE      ' '
020900940720     C                   MOVEL     '3E'          KCOD
021000940720     C                   MOVEL     *BLANKS       KKEY
021100940720     C                   MOVEL     ARTCAN        KKEY
021200940720     C     KTAB1         CHAIN     TABEL00F                           31
021300120711     c  n31              movel     tbluni        ds3e
021400120711     c   31              clear                   ds3e
021500120711     c
021600160922     c                   eval      vsccan=artcan+'-'+
021700160922     c                                    %subst(§3ede12:1:9)+'  '+artfta
021800160922     c                   eval      vsdcan=artcan+'-'+
021900160922     c                                    %subst(§3ede12:1:11)
022000090226     c* FTA
022100120618     c                   eval      vscfta=artfta
022200920813     C                   END
022300910912     C* GIRO DATE:
022400090226     C* entrata
022500160922     c                   clear                   vscdse
022600160922     c                   clear                   vsddse
022700160922     c                   clear                   vscnpe
022800160922     c                   clear                   vsdnpe
022900160922     c                   clear                   vschse
023000160922     c
023100090226     C                   MOVEL     ARTDse        G02INV
023200090226     C                   Z-ADD     *ZEROS        G02DAT
023300090226     C                   MOVEL     '3'           G02ERR
023400090226     C                   CALL      'XSRDA8'
023500090226     C                   PARM                    WLBDAT
023600160920     C                   MOVEL     G02DAT        VSCDse            6 0
023700120621     C                   MOVE      G02DAT        due
023800120621     C                   MOVE      due           VSCDse
023900160919     c
024000090226     C     ARTdse        IFGT      0
024100160919     c                   eval      vsdnpe=%editc(artnpe:'X')+'|'
024200160920     c                   eval      vscnpe=%editc(artnpe:'X')
024300090226     C                   ELSE
024400160919     C                   MOVEL     '  |'         VSdNPe
024500160920     C                   MOVEL     '  '          VScNPe
024600160920     C                   END
024700120619     c* Orario
024800120621     c                   if        arthse>0
024900120621     c                   eval      vschse=%editw(arthse:'0 :  ')
025000120621     c                   endif
025100160919     c                   eval      vsddse ='|        '
025200160919     c                   select
025300160919     c                   when      artdse>0 and (artdie=0  or artdie=artdse)
025400160919     c                   eval      vsddse=%trim(vsddse) +
025500160919     c                                    %editw(vscdse:'  /  /  ')
025600160919
025700160919     c* se data immissione <> data arrivo la visualizzo
025800160919     c                   when      artdie>0 and artdse>0 and artdie<>artdse
025900160919     c                   movel     g02dat        wggmm             4 0
026000160919     c                   eval      vsddse=%trim(vsddse) +
026100160919     c                                    %editc(wggmm:'X')
026200160919
026300160919     C                   MOVEL     ARTDie        G02INV
026400160919     C                   Z-ADD     *ZEROS        G02DAT
026500160919     C                   MOVEL     '3'           G02ERR
026600160919     C                   CALL      'XSRDA8'
026700160919     C                   PARM                    WLBDAT
026800160919     c                   movel     g02dat        wggmm             4 0
026900160920     c                   eval      vsddse=%trim(vsddse) +'|' +%editc(wggmm:'X')
027000160919     c                   endsl
027100910912     C* PARTENZA MERCE
027200160922     c                   clear                   vscdfv
027300160922     c                   clear                   vsddfv
027400160922     c                   clear                   vsdnpp
027500160922     c                   clear                   vscnpp
027600160922     c
027700940720     C                   MOVEL     ARTDFV        G02INV
027800940720     C                   Z-ADD     *ZEROS        G02DAT
027900940720     C                   MOVEL     '3'           G02ERR
028000940720     C                   CALL      'XSRDA8'
028100940720     C                   PARM                    WLBDAT
028200940720     C                   MOVEL     G02DAT        VSCDFV
028300120620     C                   MOVE      G02DAT        DUE               2 0          ANNO 2 CIFRE
028400950116     C                   MOVE      DUE           VSCDFV
028500160920     c                   eval      vsddfv ='|        '
028600160920     c                   if        artdfv>0
028700160920     c                   eval      vsddfv=%trim(vsddfv) +
028800160920     c                                    %editw(vscdfv:'  /  /  ')
028900160920     c                   endif
029000160920
029100921231     C     ARTDFV        IFGT      0
029200940720     C                   MOVEL     ARTNPP        VSCNPP
029300160920     c                   eval      vsdnpp= %editc(artnpp:'X')+'|'
029400921231     C                   ELSE
029500940720     C                   MOVEL     '  '          VSCNPP
029600160920     C                   MOVEL     '  |'         VSdNPP
029700921231     C                   END
029800921230     C* ENTRATA AL TRANSITO
029900160922     c                   clear                   vscdet
030000160922     c                   clear                   vsc6det
030100160922     c                   clear                   vsdpet
030200160922     c                   clear                   vscpet
030300160922     c
030400941219     C                   MOVEL     ARTDET        G02INV                         AAAA/MM/GG
030500940720     C                   Z-ADD     *ZEROS        G02DAT
030600940720     C                   MOVEL     '3'           G02ERR
030700940720     C                   CALL      'XSRDA8'
030800940720     C                   PARM                    WLBDAT
030900160922     C                   MOVEL     G02DAT        VSCDET                         GG/MM
031000160922     C                   MOVEL     G02DAT        VSC6DET                        GG/MM/aa
031100160920     C                   MOVE      G02DAT        due
031200160922     C                   MOVE      due           vsc6det                        GG/MM/AA
031300921231     C     ARTDET        IFGT      0
031400940720     C                   MOVEL     ARTPET        VSCPET
031500160920     C                   EVAL      VSDPET=%editc(artpet:'X')+'|'
031600921231     C                   ELSE
031700940720     C                   MOVEL     '  '          VSCPET
031800160920     C                   MOVEL     '  |'         VSdPET
031900921231     C                   END
032000910912     C* USCITA DAL TRANSITO
032100160922     c                   clear                   vscdut
032200160922     c                   clear                   vscput
032300160922     c                   clear                   vsdput
032400160922     c
032500940720     C                   MOVEL     ARTDUT        G02INV
032600940720     C                   Z-ADD     *ZEROS        G02DAT
032700940720     C                   MOVEL     '3'           G02ERR
032800940720     C                   CALL      'XSRDA8'
032900940720     C                   PARM                    WLBDAT
033000940720     C                   MOVEL     G02DAT        VSCDUT
033100120619     C                   MOVE      G02DAT        due
033200120619     C                   MOVE      due           vscdut
033300921231     C     ARTDUT        IFGT      0
033400160920     C                   MOVEL     ARTPUT        VSCPUT            2
033500160920     c                   eval      vsdput=%editc(artput:'X')+'|'
033600921231     C                   ELSE
033700940720     C                   MOVEL     '  '          VSCPUT
033800160920     C                   MOVEL     '  |'         VSdPUT
033900921231     C                   END
034000160922
034100910912     C* ARRIVO MERCE ALLA FIL ARRIVO
034200160922     c                   clear                   vsddam
034300160922     c                   clear                   vsdnap
034400160922     c                   clear                   vscnap
034500160922     c                   clear                   vscham
034600160916     c                   eval      vscflnap=artflnap
034700160916     c
034800940720     C                   MOVEL     ARTDAM        G02INV
034900940720     C                   Z-ADD     *ZEROS        G02DAT
035000940720     C                   MOVEL     '3'           G02ERR
035100940720     C                   CALL      'XSRDA8'
035200940720     C                   PARM                    WLBDAT
035300160920     C                   MOVEL     G02DAT        VSCDAM            6 0
035400950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
035500950116     C                   MOVE      DUE           VSCDAM
035600921231     C     ARTDAM        IFGT      0
035700940720     C                   MOVEL     ARTNAP        VSCNAP
035800160920     c                   eval      vsdnap=%editc(artnap:'X') + '|'
035900921231     C                   ELSE
036000940720     C                   MOVEL     '  '          VSCNAP
036100160920     C                   MOVEL     '  |'         VSdNAP
036200921231     C                   END
036300120621     c                   if        artham>0
036400120621     c                   eval      vscham=%editw(artham:'0 :  ')
036500120621     c                   endif
036600160916
036700160916     c                   select
036800160916     c                   when      artdam>0 and (artdia=0  or artdia=artdam)
036900160920     c                   eval      vsddam=%editw(g02dat:'  /  /    ')
037000160916
037100160916     c* se data immissione <> data arrivo la visualizzo
037200160916     c                   when      artdia>0 and artdam>0 and artdia<>artdam
037300160916     c                   movel     g02dat        wggmm             4 0
037400160920     c                   eval      vsddam=%editw(wggmm:'  /  ')
037500160916
037600160916     C                   MOVEL     ARTDia        G02INV
037700160916     C                   Z-ADD     *ZEROS        G02DAT
037800160916     C                   MOVEL     '3'           G02ERR
037900160916     C                   CALL      'XSRDA8'
038000160916     C                   PARM                    WLBDAT
038100160916     c                   movel     g02dat        wggmm             4 0
038200160920     c                   eval      vsddam=%trim(vsddam) +'|' +
038300160920     c                                    %editw(wggmm:'  /  ')
038400160916     c                   endsl
038500160916     c
038600910912     C* CONSEGNA MERCE REALE
038700090226     c                   clear                   vsccon
038800090226     c                   setoff                                       01
038900090226     c                   if        artdcm>0
039000940720     C                   MOVEL     ARTDCM        G02INV
039100940720     C                   Z-ADD     *ZEROS        G02DAT
039200940720     C                   MOVEL     '3'           G02ERR
039300940720     C                   CALL      'XSRDA8'
039400940720     C                   PARM                    WLBDAT
039500090226     C                   MOVEL     G02DAT        wdcm
039600950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
039700090226     C                   MOVE      DUE           wDCM
039800120618     c                   eval      vsccon= %editw(wdcm: '  /  /  ')
039900120618     c                   if        arthcm>0
040000160922     c                   eval      vsccon= %editw(wdcm: '  /  /  ')+' '+
040100120618     c                             %editw(arthcm: '  :  ')
040200090226     c                   endif
040300120618     c                   endif
040400120619     c
040500120619     c* ARRIVO FINALE
040600160920     c                   clear                   vscdamf           6 0
040700160922     c                   clear                   vsddamf
040800120619     c                   clear                   vschamf
040900120619     c                   clear                   vscnapf
041000160920     c                   clear                   vsdnapf
041100161020     c                   clear                   vsccan_p
041200160922
041300120619     c* Se non si tratta di bolla export, imposto la data all'arrivo finale
041400160920    2c                   if        not *in15
041500120619     C                   MOVEL     ARTDAMF       G02INV
041600120619     C                   Z-ADD     *ZEROS        G02DAT
041700120619     C                   MOVEL     '3'           G02ERR
041800120619     C                   CALL      'XSRDA8'
041900120619     C                   PARM                    WLBDAT
042000120619     C                   MOVEL     G02DAT        VSCDAMf
042100120619     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
042200120619     C                   MOVE      DUE           VSCDAMf
042300160920     c
042400160920    3C     ARTnapf       IFGT      0
042500120619     C                   MOVEL     ARTNAPf       VSCNAPf
042600160920     c                   eval      vsdnapf=%editc(artnapf:'X')+ '|'
042700120619     C                   ELSE
042800120619     C                   MOVEL     '  '          VSCNAPf
042900160920     C                   MOVEL     '  |'         VSdNAPf
043000160920    3C                   END
043100160920    3c                   if        arthamf>0
043200120621     c                   eval      vschamf=%editw(arthamf:'0 :  ')
043300160920    3c                   endif
043400160916     c
043500160920    3c                   select
043600160916     c                   when      artdamf>0 and (artdiaf=0  or artdiaf=artdamf)
043700160920     c                   eval      vsddamf=%editw(g02dat:'  /  /    ')
043800160916
043900160916     c* se data immissione <> data arrivo la visualizzo
044000160916     c                   when      artdiaf>0 and artdamf>0 and artdiaf<>artdamf
044100160916     c                   movel     g02dat        wggmm             4 0
044200160916     c                   eval      vsddamf=%trim(vsddamf) +
044300160920     c                                     %editw(wggmm :'  /  ')
044400160916
044500160916     C                   MOVEL     ARTDiaf       G02INV
044600160916     C                   Z-ADD     *ZEROS        G02DAT
044700160916     C                   MOVEL     '3'           G02ERR
044800160916     C                   CALL      'XSRDA8'
044900160916     C                   PARM                    WLBDAT
045000160916     c                   movel     g02dat        wggmm             4 0
045100160920     c                   eval      vsddamf=%trim(vsddamf) +'|' +
045200160920     c                                    %editw(wggmm:'  /  ')
045300160920    3c                   endsl
045400120619     c
045500160920    2c                   endif
045600910912     C*
045700090226     C* SOLO PER LE BOLLE EXPORT (15 ON) VISUALIZZO LA DATA manifest
045800161019     c* nel campo DAMF
045900961112     C     *IN15         IFEQ      *ON
046000161019     c                   clear                   vschamf
046100161019     c                   clear                   vsddamf
046200161019     C                   MOVEL     '  '          VSCNAPf
046300161019     c                   if        artdmf>0
046400090226     C                   MOVEL     ARTDmf        G02INV
046500961112     C                   Z-ADD     *ZEROS        G02DAT
046600961112     C                   MOVEL     '3'           G02ERR
046700961112     C                   CALL      'XSRDA8'
046800961112     C                   PARM                    WLBDAT
046900090226     C                   MOVEL     G02DAT        VSCDmf
047000961112     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
047100090226     C                   MOVE      DUE           VSCDmf
047200160920     c                   eval      vsddamf=%editw(g02dat:'  /  /    ')
047300161019     c
047400161019     c* il campo ORA contiene la pistola di spunta dell'invio al partner per video a 80
047500161019     C                   IF        dsp_mod = 0
047600161019     C                   MOVE      ARTNpdmf      VSChamf
047700161019     c                   endif
047800161019     c
047900160920     c                   eval      vsdnapf=%editc(artnpdmf:'X')+ '|'
048000160920     C                   ELSE
048100160920     C                   MOVEL     '  |'         VSdNAPf
048200961112     C                   ENDIF
048300160920     C                   ENDIF
048400000211      *
048500000211      * Aggancio estensione dettaglio segnacolli
048600020226      * A seconda della linea di arrivo aggancio prima tipo righe E o F
048700020226     C     §OGNTW        IFEQ      'FED'
048800020226     C                   MOVE      'F'           WTRC
048900020226     C                   ELSE
049000020226     C                   MOVE      'E'           WTRC
049100020226     C                   ENDIF
049200051013     C     KARS1         CHAIN     FIARS01L                           31
049300120619    1C     *IN31         IFEQ      *OFF
049400000911      *
049500020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
049600120619    2C     ARSNOT        IFNE      *BLANKS
049700120620     c                   add       1             xx
049800120620     c                   movel     wtrc          tipars(xx)
049900120620     C                   MOVEL     ARSNOT        NOTars(xx)
050000120619    2C                   ENDIF
050100011031      *
050200011031      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
050300121107     c                   if        *in15
050400161019     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
050500161019     c* nel campo PISTOLTa di spunta lna finale
050600161020     c* video 80
050700161019     C                   MOVEL     ARSCAN        VSCnapf
050800161020     c* video 132
050900161020     C                   MOVEL     ARSCAN        VSCcan_P
051000121107     c
051100161019     c                   if        arsdat>0
051200011031     C                   MOVEL     ARSDAT        G02INV            8 0
051300011031     C                   Z-ADD     *ZEROS        G02DAT
051400011031     C                   MOVEL     '3'           G02ERR
051500011031     C                   CALL      'XSRDA8'
051600011031     C                   PARM                    WLBDAT
051700161019     C                   MOVEL     G02DAT        w0040             4 0
051800161019     c                   eval      vsddamp=%editw(g02dat:'  /  /    ')+
051900161019     c                             ' ' + arscan
052000161019     c                   if        dsp_mod=0
052100161019     c                   clear                   vsccan
052200161019     c                   eval      vsccan='ArrP '+%editw(w0040: '  /  ')+
052300161019     c                             '   ' + artcan
052400121107     c                   endif
052500161019     c                   endif
052600161019     c                   endif
052700011031      *
052800120620   x1C                   ELSE
052900000911     C                   CLEAR                   VSCVA2
053000120619    1C                   ENDIF
053100000911      *
053200000911     C                   MOVEL     'C'           WTRC
053300051013     C     KARS1         CHAIN     FIARS01L                           31
053400120620    1C     *IN31         IFEQ      *OFF
053500000211      *
053600000911      * VERIFICO PRESENZA CODICE "CHI SONO"
053700120620    2C     ARSNOT        IFNE      *BLANKS
053800120620     c                   add       1             xx
053900120620     c                   movel     wtrc          tipars(xx)
054000120620     C                   MOVEL     ARSNOT        NOTars(xx)
054100120620    2C                   ENDIF
054200000211      *
054300120620    1C                   endif
054400000211      *
054500090226      * SE NON C'È FIARS VISUALIZZO IL VOLUME VDL
054600980312     C                   CLEAR                   VAR
054700120831     C                   MOVEA     'Vol'         VAR(19)
054800120620    2C     ARTVUC        IFGT      0
054900120620     c                   eval      wnume=%editc(artvuc:'4')
055000120831     C                   MOVEA     WNUME         VAR(23)
055100120831     C                   MOVEA     ARTFVC        VAR(32)
055200120620    2C                   END
055300120831     C                   MOVEA     'Peso'        VAR(1)
055400120620    2C     ARTPUC        IFGT      0
055500120620     c                   eval      wnume=%editc(artpuc:'4')
055600120831     C                   MOVEA     WNUME         VAR(6)
055700120831     C                   MOVEA     ARTFPC        VAR(16)
055800120620    2C                   END
055900120620     c
056000120620     c                   add       1             xx
056100120620     c                   movel     ' '           tipars(xx)
056200120620     C                   MOVEa     var           NOTars(xx)
056300000211      *
056400090226      * SEGNALO SE SEGNACOLLO con consegna anomala
056500090226     c                   if        artcca<>' '
056600970805     C                   Z-ADD     1             X
056700090226     C     ARTcca        LOOKUP    C7O(X)                                 34
056800970805     C     *IN34         IFEQ      '1'
056900970805     C     F7O(X)        ANDNE     *BLANKS
057000160922     c                   eval      %subst(vsccon:10:5)=' IDD '
057100160922     c                   seton                                        01
057200970805     C                   END
057300090226     c
057400090226     c                   else
057500090226     c* Se disabilitato visualizzo
057600090226     c                   if        artdcm=0 and artdab='S'
057700090226     c                   eval      vsccon='MANCANTE'
057800090226     c                   seton                                        01
057900090226     c                   endif
058000090226     c                   endif
058100961112     C*
058200120620     c                   exsr      AbiF11
058300160920     c* se il campo vsc7h1 vuoto, metto una | di separazione
058400160920     c                   if        vsc7h1=*blanks
058500160920     c                   eval      vsc7h1='        |  '
058600160920     c                   endif
058700120620     c
058800910205     C* SCRIVO SUBFILE
058900940720     C                   ADD       1             NRR1
059000120830     C                   IF        dsp_mod = 1
059100120830     C                   WRITE     LR38S011
059200120830     C                   ELSE
059300120830     C                   WRITE     LR38S01
059400120830     C                   ENDIF
059500910912     C*
059600090226     C     KART1         READE     FiART01L                               30
059700910205     C                   END
059800920803     C*
059900920803     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
060000160609     c     nrr1          ifgt      0
060100160609     C     ARTNSC        andne     VI1NCD
060200940720     C                   MOVEL     ARTNSC        VI1NCA
060300920803     C                   END
060400910205     C*
060500940720     C                   Z-ADD     NRR1          SAVNRR            4 0
060600921104     C*
060700940720     C     NRR1          IFEQ      0
060800120830     C                   IF        dsp_mod = 1
060900120830     c                   clear                   lr38s011
061000120830     C                   ELSE
061100120830     c                   clear                   lr38s01
061200120830     C                   ENDIF
061300940720     C                   ADD       1             NRR1
061400120830     C                   IF        dsp_mod = 1
061500120830     C                   WRITE     LR38S011
061600120830     C                   ELSE
061700120830     C                   WRITE     LR38S01
061800120830     C                   ENDIF
061900921104     C                   END
062000910205     C*
062100910205     C                   ENDSR
062200020226     C**************************************************************************
062300020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11
062400020226     C**************************************************************************
062500020226     C     ABIF11        BEGSR
062600120620     c                   clear                   vsc7h1
062700120620     c                   clear                   vsh7h2
062800120620     c                   clear                   vsh7h3
062900120830     C                   IF        dsp_mod = 1
063000120830     c                   clear                   vsc7h11
063100120830     c                   clear                   vsh7h31
063200120830     C                   ENDIF
063300120620     c                   clear                   vscvar
063400120620     c                   clear                   vscva2
063500120620     c                   clear                   vshnot2
063600120620     c                   clear                   vshnot3
063700120620     c*
063800120620     c* se  presenti tutti e 3 accendo F11
063900120620     c                   if        xx=3
064000020226     C                   seton                                        17
064100120620     C                   movel     FunF11(1)     VZCF11
064200120620     C                   MOVEL     'S'           WINFO             1
064300120620     c                   endif
064400020226      *
064500120620     c* Visualizzo i primi due
064600120620     c                   eval      vscva2=notars(1)
064700120621    1c                   if        tipars(1)<>*blanks
064800120620     c
064900120620     C                   MOVE      '7H'          KCOD
065000120620     C                   MOVEL(P)  tipars(1)     KKEY
065100120620     C     KTAB1         CHAIN     TABEL00F                           38
065200120620     C  N38              MOVEL     TBLUNI        DS7H
065300120620     C   38              clear                   DS7H
065400120620    2c                   if        tipars(2)=' '
065500120620     C                   MOVEL     §7HDEB        VSc7H1
065600120620   x2c                   else
065700120830     C                   IF        dsp_mod = 1
065800120830     C                   MOVEL     §7HDEB        VSc7H1
065900120830     C                   ELSE
066000120620     c                   eval      vsc7h1=§7HDEB6+'/'
066100120830     C                   ENDIF
066200120620     c* prima descrizione completa
066300120620     C                   MOVEL     §7HDEB        VSh7H2
066400120620    2c                   endif
066500120620    1c                   endif
066600120620     c
066700120620     c                   eval      vscvar=notars(2)
066800120621    1c                   if        tipars(2)<>*blanks
066900120620     C                   MOVE      '7H'          KCOD
067000120620     C                   MOVEL(P)  tipars(2)     KKEY
067100120620     C     KTAB1         CHAIN     TABEL00F                           38
067200120620     C  N38              MOVEL     TBLUNI        DS7H
067300120620     C   38              clear                   DS7H
067400120620    2c                   if        tipars(1)=' '
067500120620     C                   MOVEL     §7HDEB        VSc7H1
067600120620     c                   else
067700120830     C                   IF        dsp_mod = 1
067800120830     C                   MOVEL     §7HDEB        VSc7H11
067900120830     c* 2 descrizioni perché 132 col.  (le salvo entrambe)
068000120830     c                   eval      vsh7h3=vsc7h1
068100120830     c                   eval      vsh7h31=vsc7h11
068200120830     C                   ELSE
068300120620     c                   eval      vsc7h1=%trim(vsc7h1)+§7HDEB6
068400120830     c* 1 descrizione doppia perché 80 col. (ne salvo una sola)
068500120620     c                   eval      vsh7h3=vsc7h1
068600120830     C                   ENDIF
068700120620    2c                   endif
068800120620    1c                   endif
068900120620     c
069000120620     c                   eval      vshnot2=notars(2)
069100120620     c                   eval      vshnot3=notars(3)
069200020226      *
069300020226     C                   ENDSR
069400980312     C**************************************************************************
069500120620     C* AGGIORNAMENTO SUBFILE PER INTERROGAZIONE ALTRI DATI
069600980312     C**************************************************************************
069700980312     C     UPDS01        BEGSR
069800980312     C                   Z-ADD     1             NRR1
069900120830     C                   IF        dsp_mod = 1
070000120830     C     NRR1          CHAIN     LR38S011                           31
070100120830     C                   ELSE
070200120830     C     NRR1          CHAIN     LR38S01                            31
070300120830     C                   ENDIF
070400980312     C     *IN31         DOWEQ     *OFF
070500120620     c
070600120620     C* VISUALIZZO dati vdl al posto del chi sono --> record 3 col record 2
070700120620     C                   if        WINFO='S'
070800120620     c                   eval      vsc7h1=vsh7h2
070900120830     C                   IF        dsp_mod = 1
071000120830     c                   eval      vsc7h11=*blank
071100120830     C                   ENDIF
071200120620     c                   eval      vscvar=vshnot3
071300120620     c
071400980312     C                   ELSE
071500120620     C* VISUALIZZO "CHI SONO" al posto di dati VDL --> record 2 al posto di record 3
071600120620     c                   eval      vsc7h1=vsh7h3
071700120830     C                   IF        dsp_mod = 1
071800120830     c                   eval      vsc7h11=vsh7h31
071900120830     C                   ENDIF
072000120620     c                   eval      vscvar=vshnot2
072100980312     C                   END
072200020226      *
072300120830     C                   IF        dsp_mod = 1
072400120830     C                   UPDATE    LR38S011
072500120830     C                   ELSE
072600120830     C                   UPDATE    LR38S01
072700120830     C                   ENDIF
072800980312     C                   ADD       1             NRR1
072900120830     C                   IF        dsp_mod = 1
073000120830     C     NRR1          CHAIN     LR38S011                           31
073100120830     C                   ELSE
073200120830     C     NRR1          CHAIN     LR38S01                            31
073300120830     C                   ENDIF
073400980312     C                   ENDDO
073500120620     c
073600120620     c                   if        WINFO='S'
073700120620     C                   movel     FunF11(2)     VZCF11
073800120620     c                   eval      winfo='V'
073900120620     c                   else
074000120620     C                   movel     FunF11(1)     VZCF11
074100120620     c                   eval      winfo='S'
074200120620     c                   endif
074300120620     c
074400980312     C                   ENDSR
074500940720     C**************************************************************************
074600940720     C* PULIZIA SUBFILE
074700940720     C**************************************************************************
074800910205     C     PULSF         BEGSR
074900940720     C*
075000910205     C                   SETON                                        35
075100120830     C                   IF        dsp_mod = 1
075200120830     C                   WRITE     LR38C011                                     SFLCTL
075300120830     C                   ELSE
075400120830     C                   WRITE     LR38C01                                      SFLCTL
075500120830     C                   ENDIF
075600910205     C                   SETOFF                                       35
075700940720     C*
075800910205     C                   ENDSR
075900940720     C**************************************************************************
076000940720     C*  ROUTINE  INIZIALE
076100940720     C**************************************************************************
076200940720     C     *INZSR        BEGSR
076300940720     C*
076400090226     C     KART1         KLIST                                                  FiiRT01L
076500940720     C                   KFLD                    PARAAS                         ..ANNO
076600940720     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
076700940720     C                   KFLD                    PARNRS                         ..NUMERO SERIE
076800940720     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
076900150326     C     KART27        KLIST                                                  FiiRT01L
077000150326     C                   KFLD                    artfls                         ..ANNO
077100150326     C                   KFLD                    artlna                         ..LINEA PARTENZA
077200150326     C                   KFLD                    artNRS                         ..NUMERO SERIE
077300150326     C                   KFLD                    artnsc                         ..NUMERO SPEDIZIONE
077400940720     C     KTAB1         KLIST                                                  TABEL00F
077500940720     C                   KFLD                    KKUT                           ..CODICE UTENTE
077600940720     C                   KFLD                    KCOD                           ..CODICE TABELLA
077700940720     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
077800970805     C     KTAB2         KLIST                                                  TABEL00F
077900970805     C                   KFLD                    KKUT                           ..CODICE UTENTE
078000970805     C                   KFLD                    KCOD                           ..CODICE TABELLA
078100090226     C     KARS1         KLIST                                                  FiART01L
078200980312     C                   KFLD                    ARTFLS
078300980312     C                   KFLD                    ARTLNA                                     ENZA
078400980312     C                   KFLD                    ARTNRS                                     IE
078500980312     C                   KFLD                    ARTNSC                                     DIZIONE
078600980312     C                   KFLD                    WTRC                                       DIZIONE
078700940720     C*
078800940720     C     *LIKE         DEFINE    TBLKUT        KKUT                           CODICE UTENTE
078900940720     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
079000940720     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
079100980312     C     *LIKE         DEFINE    ARSTRC        WTRC                                       NTO
079200940720     C*
079300940720     C                   Z-ADD     1             KKUT
079400970805     C*
079500970805     C* Eseguo caricamento della tabella dei codici consegna anomala
079600970805     C                   Z-ADD     0             X                 3 0
079700970805     C                   MOVEL     '7O'          KCOD
079800970805     C     KTAB2         CHAIN     TABEL                              34
079900970805     C     *IN34         DOWEQ     '0'
080000970805     C     X             ANDLT     100
080100970805     C     TBLFLG        IFEQ      *BLANKS
080200970805     C                   ADD       1             X
080300970805     C                   MOVEL     TBLKEY        C7O(X)
080400970805     C                   MOVEL     TBLUNI        DS7O
080500970805     C                   MOVEL     §7OIDD        F7O(X)
080600970805     C                   END
080700970805     C     KTAB2         READE     TABEL                                  34
080800970805     C                   END
080900120830     C*
081000120830     C* definisco se lo schermo dell'utente supporta o meno il 27X132
081100160919     C                   IF        not %open(FNLRU38D) and
081200160919     C                             not %open(FNLRU38D1)
081300120830     C                   EVAL      dsp_mod = QueryDisplayModeSupport
081400120830     C                              ('4':*omit :*omit :*omit)
081500120830     C                   IF        dsp_mod=0
081600120830     C***                EVAL      k_sflpag=17
081700160919     C                   OPEN      FNLRU38D
081800120830     C                   ELSE
081900120830     C***                EVAL      k_sflpag=17
082000160919     C                   OPEN      FNLRU38D1
082100120830     C                   ENDIF
082200120830     C                   ENDIF
082300940720     C*
082400940720     C                   ENDSR
082500160916     C**************************************************************************
082600160916     c     DATI_TES      BEGSR
082700160916     c* Trimmo rif mittente numerico
082800160916     c                   eval      vcdrmn=%trim(%editc(parrmn:'Z'))
082900160916
083000160916     C                   MOVEL     PARDSA        V1CDSA
083100160916     C                   MOVEL     PADLNA        VCDLNA                         DECODIFICA FILIALE
083200160916
083300160916     c* Destinatario (mittente solo nella vecchia int bolle part)
083400160916     c                   clear                   vcdcod
083500160916     c                   clear                   vcdcod1
083600160916     c                   if        parcod>*zeros
083700160916     c                   eval      vcdcod =parcod+'-'+parrsc
083800160916     c                   eval      vcdcod1=parcod+'-'+parrsc
083900160916     c                   else
084000160916     C                   MOVEL     PARRSc        VCDcod
084100160916     C                   MOVEL     PARRSc        VCDcod1
084200160916     c                   endif
084300160916     c                   if        partbo='A'
084400160922     c                   eval      vccdcl1='Destinat.'
084500160916     c                   eval      vccdcl ='Destinatario'
084600160916     c                   else
084700160922     c                   eval      vccdcl1 ='Mittente '
084800160922     c                   eval      vccdcl  ='Mittente    '
084900160916     c                   endif
085000160916     c*
085100160916     c* terminal
085200160916     c                   eval      vcdter=%editc(partfp:'X')+'/'+
085300160916     c                                    %editc(partfa:'X')
085400160916     c
085500160916     c                   eval      vcdpar=%editc(partfp:'X')
085600160916     c                   clear                   orgde5
085700160916     c                   clear                   orgdes
085800160916     C     PARtfa        CHAIN     AZORG01L
085900160916     c                   if        orgde5<>*blanks
086000160916     C                   eval      vcdarr=%editc(partfa:'X')+'-' +orgde5
086100160916     c                   else
086200160916     C                   eval      vcdarr=%editc(partfa:'X')+'-'+orgdes
086300160916     c                   endif
086400160916     C*
086500160916     C* 15 ON  - SI TRATTA DI BOLLA EXPORT
086600160916     C     PARLNA        CHAIN     AZORG01L
086700160916     C                   if        %found
086800160916     C                   movel     ORGde3        og143
086900160916     C                   eval      *in15 = (§OgNtw = 'EEX' or §OgNtw ='EUP' or
087000160916     C                                     §OgNtw ='FED' or §OgNtw = 'DPD')
087100160916     C                   endif
087200160916     C*
087300160916     C* IMPOSTA CAMPI PER INT SPUNTE
087400160916     c                   clear                   param1
087500160916     C                   Z-ADD     0             VI1NFV
087600160916     C                   MOVEL     *ZEROS        VI1LNP
087700160916     C                   MOVEL     PARLNA        VI1LNA
087800160916     C                   MOVEL     PARNRS        VI1NRS
087900160916     C                   MOVEL     9             VI1NPG
088000160916     C                   Z-ADD     0             VI1NCD
088100160916     C                   Z-ADD     0             VI1NCA
088200160916     C                   MOVEL     99            VI1NPS
088300160916     C                   MOVEL     'FNLR38R'     VI1PGM
088400160916     C                   MOVEL     '1'           VI1FLG
088500160916     C                   MOVEL     PARNRS        VI1bors
088600160916     C                   MOVEL     PARlnp        VI1bolp
088700160916     C                   MOVEL     PARnsp        VI1bosp
088800160916     C                   MOVEL     PARaas        VI1boan
088900160922     c*
089000160916     C*
089100160916     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI
089200160916     C                   EXSR      CARART
089300160916     c                   ENDSR
089400020225**
089500051129F11=Dati VDL
089600020226F11=CHI SONO
