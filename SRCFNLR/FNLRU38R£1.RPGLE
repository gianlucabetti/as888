000100120830     h dftactgrp(*no) actgrp(*caller)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300120618      * FNLR38R *----------------------------------------------------*
000400120906      *      VISUALIZZAZIONE DETTAGLIO SEGNACOLLI BOLLE - unico
000500940720      *--------------------------------------------------------------*
000600120618     FFNLR38D   CF   E             WORKSTN
000700120618     F                                     SFILE(LR38S01:NRR1)
000800120830     F                                     USROPN
000900120830     FFNLR38D1  CF   E             WORKSTN
001000120830     F                                     SFILE(LR38S011:NRR1)
001100120830     F                                     USROPN
001200911024     FTABEL00F  IF   E           K DISK
001300090226     FFIART01L  IF   E           K DISK
001400051013     FFIARS01L  IF   E           K DISK
001500980312     FAZORG01L  IF   E           K DISK
001600150618     F*FnART27L  IF   E           K DISK
001700150618     F*Fnblt27L  IF   E           K DISK
001800970805      *--------------------------------------------------------------*
001900970805     D* Schiera codici consegne anomale + relativi flag
002000970805     D C7O             S              1    DIM(200)                             cod.cons.anom
002100970805     D F7O             S              1    DIM(200)                             flg cons.anom
002200120620     D TIPars          S              1    DIM(3)
002300120620     D NOTars          S             35    DIM(3)
002400120831     D  VAR            S              1    dim(32)
002500120620     D FUNF11          S             38    DIM(2) CTDATA PERRCD(1)
002600970805      *--------------------------------------------------------------*
002700911024     D PARAM           DS
002800940720     D  PARAAS                14     17  0
002900940720     D  PARLNP                18     20  0
003000940720     D  PARNRS                21     22  0
003100940720     D  PARNSP                23     29  0
003200940720     D  PARF03                30     30
003300940720     D  PARFLG                31     31
003400940720     D  PARPGM                32     41
003500940720     D  PARDSA                42     61
003600940720     D  PARDSP                63     70  0
003700940720     D  PARRMN                71     85  0
003800940720     D  PARLNA                86     88  0
003900940720     D  PADLNA                89    108
004000090226     D  PARRSc               109    143
004100150619     D  PARTBO               148    148
004200150619     D  PARcod               149    155
004300160916     D  PARTFP               156    158  0
004400160916     D  PARTFA               159    161  0
004500120620     d
004600941230     D PARAM1          DS                  INZ
004700920803     D  VI1NFV                 1      5  0
004800920803     D  VI1LNP                 6      8  0
004900920803     D  VI1LNA                 9     11  0
005000920803     D  VI1NRS                12     13  0
005100920803     D  VI1NCD                14     20  0
005200920803     D  VI1NCA                21     27  0
005300920803     D  VI1NPS                28     29  0
005400920803     D  VI1NPG                30     30  0
005500920813     D  VI1FLG                31     31
005600920803     D  VI1PGM                32     38
005700941230     D  VI1FGS                39     41  0
005800120621     d* bolla
005900120621     d  vi1bolp               42     44  0
006000120621     d  vi1bors               45     46  0
006100120621     d  vi1bosp               47     53  0
006200120621     d  vi1boan               54     57  0
006300120621     D  PA1ERR                58     58
006400940720     D WLBDAT          DS                  INZ
006500940720     D  G02DAT                 1      8  0
006600940720     D  G02INV                 9     16  0
006700940720     D  G02ERR                17     17
006800940720     D  G02TGI                18     22  0
006900120620     D
007000150618     d fnlr38uds     e ds
007100150618     d fiartds       e ds                  extname(fiart00f)
007200150618     d s_fiartds     e ds                  extname(fiart00F) prefix(s_)
007300961122     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
007400961122     D DSLR45        E DS                  EXTNAME(FNLR45DS)
007500120620     d
007600900517     D KPJBA         E DS
007700120711     D DS3E          E DS
007800120711     D DS7O          E DS
007900000911     D DS7H          E DS
008000020121     D OG143         E DS
008100090226     d wdcm            s              6  0
008200120620     D WNUME           s              8
008300120620     D XX              s              3  0
008400120830     d dsp_mod         s             10I 0
008500120830
008600120830     D* PROTOTIPI
008700120830      /copy gaitrasrc/srcprotopr,QsnQryModS
008800150618     i*fnart000
008900150618     i*              artnfv                      artnfv1
009000150318     i
009100940720     C*-------------------------------------------------------------------------
009200941110     C     *ENTRY        PLIST
009300941110     C                   PARM                    KPJBA
009400941110     C*
009500910219     C                   MOVEL     KPJBU         PARAM
009600090226     c* Imposto campi testata
009700160916     c                   exsr      Dati_tes
009800020226      *
009900900509     C     FOR01         TAG
010000120830     C                   IF        dsp_mod = 1
010100120830     C                   WRITE     LR38T011                                     TESTATA
010200120830     C                   WRITE     LR38Z011                                     TASTI FUNZIONALI
010300120830     C                   EXFMT     LR38C011                                     SFLCTL
010400120830     C                   ELSE
010500120830     C                   WRITE     LR38T01                                      TESTATA
010600120830     C                   WRITE     LR38Z01                                      TASTI FUNZIONALI
010700120830     C                   EXFMT     LR38C01                                      SFLCTL
010800120830     C                   ENDIF
010900940802     C*
011000980316     C                   SETOFF                                       11
011100950220     C                   CLEAR                   PARF03                         NO PREMUTO F3=FINE
011200940802     C   KC              MOVEL     '1'           PARF03                         PREMUTO F3=FINE
011300920803     C*
011400120620     C* F19 - INTERROGAZIONE SPUNTE
011500950125     C   KT              DO
011600950125     C                   MOVEL     PARAM1        KPJBU
011700950125     C                   CALL      'FNLR04R'                            99
011800920803     C                   PARM                    KPJBA
011900950125     C                   GOTO      FOR01
012000950125     C                   ENDDO
012100910219     C*
012200120620     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
012300961122     C     *INKH         IFEQ      *ON
012400961122     C                   CLEAR                   DSLR45
012500961122     C                   MOVE      PARAAS        D45AAS
012600961122     C                   MOVE      PARLNP        D45FIL
012700961122     C                   MOVE      PARNRS        D45NRS
012800961122     C                   MOVE      PARNSP        D45NSP
012900961122     C                   MOVEL     DSLR45        KPJBU
013000961122     C                   CALL      'FNLR45R'
013100961122     C                   PARM                    KPJBA
013200961122     C                   GOTO      FOR01
013300961122     C                   ENDIF
013400020226      *
013500120620     C* F11 - INTERROGAZIONE VOLUME/SEGNACOLLO EUROEXPRESS
013600980312     C     *INKK         IFEQ      *ON
013700020226      *
013800980316     C                   SETON                                        11
013900980312     C                   EXSR      UPDS01
014000020226      *
014100980312     C                   GOTO      FOR01
014200980312     C                   END
014300961122     C*
014400000000     C     FINE          TAG
014500150618     c                   movel     'C'           i38tla
014600150618     c                   call      'FNLR38UR'
014700150618     c                   parm                    fnlr38uds
014800150618     c                   parm                    s_fiartds
014900950125     C                   MOVEL     PARAM         KPJBU
015000150618
015100150618
015200000000     C                   SETON                                        LR
015300940720     C**************************************************************************
015400940720     C* CARICO DETTAGLIO SEGNACOLLI
015500940720     C**************************************************************************
015600090226     C     CARART        BEGSR
015700940720     C*
015800910205     C                   EXSR      PULSF
015900120620     c
016000940720     C                   Z-ADD     0             NRR1              4 0
016100910806     C                   SETOFF                                       30
016200090226     C     KART1         SETLL     FiART01L
016300090226     C     KART1         READE     FiART01L                               30
016400940720     C  N30              MOVEL     ARTNSC        VI1NCD
016500950116     C  N30              MOVEL     ARTFLS        VI1LNP
016600090226     C  N30              MOVEL     ARTFLS        Vccfls
016700910220     C*
016800910806     C     *IN30         DOWEQ     '0'
016900150318     c* per il momento visto che su fiart non i dati ancora non ci sono li
017000150318     c* vado a prendere dal fnart e fnblt
017100150618     c***  kart27        chain     fnart27l
017200150618     c***                if        artdse=0
017300150618     c***  kart27        chain     fnblt27l
017400150618     c***                if        %found(fnblt27l)
017500150618     c***                z-add     bltdse        artdse
017600150618     c****               z-add     bltnpe        artnpe
017700150618     c****               endif
017800150618     c****               endif
017900150618     c                   clear                   fnlr38uds
018000150618     c                   eval      i38tbo=partbo
018100150618     c                   eval      i38aas=artaas
018200150618     c                   eval      i38lnp=artlnp
018300150618     c                   eval      i38nrs=artnrs
018400150618     c                   eval      i38nsp=artnsp
018500150618     c                   eval      i38nsc=artnsc
018600150618     c                   call      'FNLR38UR'
018700150618     c                   parm                    fnlr38uds
018800150618     c                   parm                    s_fiartds
018900150618     c* In base ai campi di fiart già aggiornati dai pgm e che
019000150618     c* quindi non devo reperire valorizzo solo i campi di fiart che mi interessano
019100150618     c* ora li valorizzo tutti
019200150618     c                   eval      fiartds=s_fiartds
019300150318
019400120620     c                   clear                   tipars
019500120620     c                   clear                   notars
019600120620     c                   clear                   XX
019700920813     C*
019800940720     C                   MOVEL     ARTNSC        VSCNSC                         NUMERO SEGNACOLLO
019900940720     C                   MOVEL     ARTFLP        VSCFLP                         FILIALE TRANSITO
020000120619     C* cod anomalia
020100940720     C                   MOVEL     *BLANKS       VSCCAN
020200090226     C                   MOVEL     *BLANKS       VSCFTA
020300920813     C     ARTCAN        IFNE      ' '
020400120711     C                   MOVEL     ' -'          VSCCAN
020500120711     C                   MOVEL     ARTCAN        VSCCAN
020600940720     C                   MOVEL     '3E'          KCOD
020700940720     C                   MOVEL     *BLANKS       KKEY
020800940720     C                   MOVEL     ARTCAN        KKEY
020900940720     C     KTAB1         CHAIN     TABEL00F                           31
021000120711     c  n31              movel     tbluni        ds3e
021100120711     c   31              clear                   ds3e
021200120711     c
021300120711     c                   eval      %subst(vsccan:3:10)=§3ede12
021400120619     c
021500120619     C**N31              MOVEL     TBLUNI        ALF13            13
021600120619     C** 31              MOVEL     *BLANKS       ALF13
021700120619     C**                 MOVE      ALF13         VSCCAN
021800120619     c
021900090226     c* FTA
022000120618     c                   eval      vscfta=artfta
022100120618     c**                 select
022200120618     c**                 when      artfta='P'
022300120618     c**                 eval      vscfta='inPAR'
022400120618     c**                 when      artfta='A'
022500120618     c**                 eval      vscfta='inARR'
022600120618     c**                 when      artfta='T'
022700120618     c**                 eval      vscfta='inTRA'
022800120618     c**                 ENDSL
022900920813     C                   END
023000910912     C* GIRO DATE:
023100090226     C* entrata
023200090226     C                   MOVEL     ARTDse        G02INV
023300090226     C                   Z-ADD     *ZEROS        G02DAT
023400090226     C                   MOVEL     '3'           G02ERR
023500090226     C                   CALL      'XSRDA8'
023600090226     C                   PARM                    WLBDAT
023700090226     C                   MOVEL     G02DAT        VSCDse
023800120621     C                   MOVE      G02DAT        due
023900120621     C                   MOVE      due           VSCDse
024000090226     C     ARTdse        IFGT      0
024100090226     C                   MOVEL     ARTNPe        VSCNPe
024200090226     C                   ELSE
024300090226     C                   MOVEL     '  '          VSCNPe
024400090226     C                   END
024500120619     c* Orario
024600120621     c                   clear                   vschse
024700120621     c                   if        arthse>0
024800120621     c                   eval      vschse=%editw(arthse:'0 :  ')
024900120621     c                   endif
025000910912     C* PARTENZA MERCE
025100940720     C                   MOVEL     ARTDFV        G02INV
025200940720     C                   Z-ADD     *ZEROS        G02DAT
025300940720     C                   MOVEL     '3'           G02ERR
025400940720     C                   CALL      'XSRDA8'
025500940720     C                   PARM                    WLBDAT
025600940720     C                   MOVEL     G02DAT        VSCDFV
025700120620     C                   MOVE      G02DAT        DUE               2 0          ANNO 2 CIFRE
025800950116     C                   MOVE      DUE           VSCDFV
025900921231     C     ARTDFV        IFGT      0
026000940720     C                   MOVEL     ARTNPP        VSCNPP
026100921231     C                   ELSE
026200940720     C                   MOVEL     '  '          VSCNPP
026300921231     C                   END
026400921230     C* ENTRATA AL TRANSITO
026500941219     C                   MOVEL     ARTDET        G02INV                         AAAA/MM/GG
026600940720     C                   Z-ADD     *ZEROS        G02DAT
026700940720     C                   MOVEL     '3'           G02ERR
026800940720     C                   CALL      'XSRDA8'
026900940720     C                   PARM                    WLBDAT
027000941219     C                   MOVEL     G02DAT        VSCDET                         GG/MM/AA (6)
027100921231     C     ARTDET        IFGT      0
027200940720     C                   MOVEL     ARTPET        VSCPET
027300921231     C                   ELSE
027400940720     C                   MOVEL     '  '          VSCPET
027500921231     C                   END
027600910912     C* USCITA DAL TRANSITO
027700940720     C                   MOVEL     ARTDUT        G02INV
027800940720     C                   Z-ADD     *ZEROS        G02DAT
027900940720     C                   MOVEL     '3'           G02ERR
028000940720     C                   CALL      'XSRDA8'
028100940720     C                   PARM                    WLBDAT
028200940720     C                   MOVEL     G02DAT        VSCDUT
028300120619     C                   MOVE      G02DAT        due
028400120619     C                   MOVE      due           vscdut
028500921231     C     ARTDUT        IFGT      0
028600940720     C                   MOVEL     ARTPUT        VSCPUT
028700921231     C                   ELSE
028800940720     C                   MOVEL     '  '          VSCPUT
028900921231     C                   END
029000910912     C* ARRIVO MERCE ALLA FIL ARRIVO
029100940720     C                   MOVEL     ARTDAM        G02INV
029200940720     C                   Z-ADD     *ZEROS        G02DAT
029300940720     C                   MOVEL     '3'           G02ERR
029400940720     C                   CALL      'XSRDA8'
029500940720     C                   PARM                    WLBDAT
029600940720     C                   MOVEL     G02DAT        VSCDAM
029700950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
029800950116     C                   MOVE      DUE           VSCDAM
029900921231     C     ARTDAM        IFGT      0
030000940720     C                   MOVEL     ARTNAP        VSCNAP
030100921231     C                   ELSE
030200940720     C                   MOVEL     '  '          VSCNAP
030300921231     C                   END
030400120621     c                   clear                   vscham
030500120621     c                   if        artham>0
030600120621     c                   eval      vscham=%editw(artham:'0 :  ')
030700120621     c                   endif
030800910912     C* CONSEGNA MERCE REALE
030900090226     c                   clear                   vsccon
031000090226     c                   setoff                                       01
031100090226     c                   if        artdcm>0
031200940720     C                   MOVEL     ARTDCM        G02INV
031300940720     C                   Z-ADD     *ZEROS        G02DAT
031400940720     C                   MOVEL     '3'           G02ERR
031500940720     C                   CALL      'XSRDA8'
031600940720     C                   PARM                    WLBDAT
031700090226     C                   MOVEL     G02DAT        wdcm
031800950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
031900090226     C                   MOVE      DUE           wDCM
032000120618     c                   eval      vsccon= %editw(wdcm: '  /  /  ')
032100120618     c                   if        arthcm>0
032200120618     c                   eval      vsccon=%trim(vsccon) + ' ' +
032300120618     c                             %editw(arthcm: '  :  ')
032400090226     c                   endif
032500120618     c                   endif
032600120619     c
032700120619     c* ARRIVO FINALE
032800120620     c****               eval      vscarf='A'+%editc(artlna:'X')
032900120619     c                   clear                   vscdamf
033000120619     c                   clear                   vschamf
033100120619     c                   clear                   vscnapf
033200120619     c* Se non si tratta di bolla export, imposto la data all'arrivo finale
033300120619     c                   if        not *in15
033400120619     C                   MOVEL     ARTDAMF       G02INV
033500120619     C                   Z-ADD     *ZEROS        G02DAT
033600120619     C                   MOVEL     '3'           G02ERR
033700120619     C                   CALL      'XSRDA8'
033800120619     C                   PARM                    WLBDAT
033900120619     C                   MOVEL     G02DAT        VSCDAMf
034000120619     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
034100120619     C                   MOVE      DUE           VSCDAMf
034200120620     C     ARTnapf       IFGT      0
034300120619     C                   MOVEL     ARTNAPf       VSCNAPf
034400120619     C                   ELSE
034500120619     C                   MOVEL     '  '          VSCNAPf
034600120619     C                   END
034700120621     c                   clear                   vschamf
034800120621     c                   if        arthamf>0
034900120621     c                   eval      vschamf=%editw(arthamf:'0 :  ')
035000120621     c                   endif
035100120619     c
035200120619     c                   endif
035300910912     C*
035400090226     C* SOLO PER LE BOLLE EXPORT (15 ON) VISUALIZZO LA DATA manifest
035500961112     C     *IN15         IFEQ      *ON
035600090226     C                   MOVEL     ARTDmf        G02INV
035700961112     C                   Z-ADD     *ZEROS        G02DAT
035800961112     C                   MOVEL     '3'           G02ERR
035900961112     C                   CALL      'XSRDA8'
036000961112     C                   PARM                    WLBDAT
036100090226     C                   MOVEL     G02DAT        VSCDmf
036200961112     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
036300090226     C                   MOVE      DUE           VSCDmf
036400961112     C                   ENDIF
036500000211      *
036600000211      * Aggancio estensione dettaglio segnacolli
036700020226      * A seconda della linea di arrivo aggancio prima tipo righe E o F
036800020226     C     §OGNTW        IFEQ      'FED'
036900020226     C                   MOVE      'F'           WTRC
037000020226     C                   ELSE
037100020226     C                   MOVE      'E'           WTRC
037200020226     C                   ENDIF
037300051013     C     KARS1         CHAIN     FIARS01L                           31
037400120619    1C     *IN31         IFEQ      *OFF
037500000911      *
037600020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
037700120619    2C     ARSNOT        IFNE      *BLANKS
037800120620     c                   add       1             xx
037900120620     c                   movel     wtrc          tipars(xx)
038000120620     C                   MOVEL     ARSNOT        NOTars(xx)
038100120619    2C                   ENDIF
038200011031      *
038300011031      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
038400121107     c                   if        *in15
038500121107     c                   clear                   vschamf
038600121107     c
038700011031     C                   MOVEL     ARSDAT        G02INV            8 0
038800011031     C                   Z-ADD     *ZEROS        G02DAT
038900011031     C                   MOVEL     '3'           G02ERR
039000011031     C                   CALL      'XSRDA8'
039100011031     C                   PARM                    WLBDAT
039200120619     C                   MOVEL     G02DAT        VSCdamf
039300120621     C                   MOVE      G02DAT        due
039400120621     C                   MOVE      due           vscdamf
039500120619     c
039600011031     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
039700120619     C                   MOVEL     ARSCAN        VSCnapf
039800121107     c                   endif
039900011031      *
040000120620   x1C                   ELSE
040100000911     C                   CLEAR                   VSCVA2
040200120619    1C                   ENDIF
040300000911      *
040400000911     C                   MOVEL     'C'           WTRC
040500051013     C     KARS1         CHAIN     FIARS01L                           31
040600120620    1C     *IN31         IFEQ      *OFF
040700000211      *
040800000911      * VERIFICO PRESENZA CODICE "CHI SONO"
040900120620    2C     ARSNOT        IFNE      *BLANKS
041000120620     c                   add       1             xx
041100120620     c                   movel     wtrc          tipars(xx)
041200120620     C                   MOVEL     ARSNOT        NOTars(xx)
041300120620    2C                   ENDIF
041400000211      *
041500120620    1C                   endif
041600000211      *
041700090226      * SE NON C'È FIARS VISUALIZZO IL VOLUME VDL
041800980312     C                   CLEAR                   VAR
041900120831     C                   MOVEA     'Vol'         VAR(19)
042000120620    2C     ARTVUC        IFGT      0
042100120620     c                   eval      wnume=%editc(artvuc:'4')
042200120831     C                   MOVEA     WNUME         VAR(23)
042300120831     C                   MOVEA     ARTFVC        VAR(32)
042400120620    2C                   END
042500120831     C                   MOVEA     'Peso'        VAR(1)
042600120620    2C     ARTPUC        IFGT      0
042700120620     c                   eval      wnume=%editc(artpuc:'4')
042800120831     C                   MOVEA     WNUME         VAR(6)
042900120831     C                   MOVEA     ARTFPC        VAR(16)
043000120620    2C                   END
043100120620     c
043200120620     c                   add       1             xx
043300120620     c                   movel     ' '           tipars(xx)
043400120620     C                   MOVEa     var           NOTars(xx)
043500000211      *
043600090226      * SEGNALO SE SEGNACOLLO con consegna anomala
043700090226     c                   if        artcca<>' '
043800970805     C                   Z-ADD     1             X
043900090226     C     ARTcca        LOOKUP    C7O(X)                                 34
044000970805     C     *IN34         IFEQ      '1'
044100970805     C     F7O(X)        ANDNE     *BLANKS
044200120618     c                   eval      %subst(vsccon:10:5)=' IDD '
044300970805     C                   END
044400090226     c
044500090226     c                   else
044600090226     c* Se disabilitato visualizzo
044700090226     c                   if        artdcm=0 and artdab='S'
044800090226     c                   eval      vsccon='MANCANTE'
044900090226     c                   seton                                        01
045000090226     c                   endif
045100090226     c                   endif
045200961112     C*
045300120620     c                   exsr      AbiF11
045400120620     c
045500910205     C* SCRIVO SUBFILE
045600940720     C                   ADD       1             NRR1
045700120830     C                   IF        dsp_mod = 1
045800120830     C                   WRITE     LR38S011
045900120830     C                   ELSE
046000120830     C                   WRITE     LR38S01
046100120830     C                   ENDIF
046200910912     C*
046300090226     C     KART1         READE     FiART01L                               30
046400910205     C                   END
046500920803     C*
046600920803     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
046700160609     c     nrr1          ifgt      0
046800160609     C     ARTNSC        andne     VI1NCD
046900940720     C                   MOVEL     ARTNSC        VI1NCA
047000920803     C                   END
047100910205     C*
047200940720     C                   Z-ADD     NRR1          SAVNRR            4 0
047300921104     C*
047400940720     C     NRR1          IFEQ      0
047500120830     C                   IF        dsp_mod = 1
047600120830     c                   clear                   lr38s011
047700120830     C                   ELSE
047800120830     c                   clear                   lr38s01
047900120830     C                   ENDIF
048000940720     C                   ADD       1             NRR1
048100120830     C                   IF        dsp_mod = 1
048200120830     C                   WRITE     LR38S011
048300120830     C                   ELSE
048400120830     C                   WRITE     LR38S01
048500120830     C                   ENDIF
048600921104     C                   END
048700910205     C*
048800910205     C                   ENDSR
048900020226     C**************************************************************************
049000020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11
049100020226     C**************************************************************************
049200020226     C     ABIF11        BEGSR
049300120620     c                   clear                   vsc7h1
049400120620     c                   clear                   vsh7h2
049500120620     c                   clear                   vsh7h3
049600120830     C                   IF        dsp_mod = 1
049700120830     c                   clear                   vsc7h11
049800120830     c                   clear                   vsh7h31
049900120830     C                   ENDIF
050000120620     c                   clear                   vscvar
050100120620     c                   clear                   vscva2
050200120620     c                   clear                   vshnot2
050300120620     c                   clear                   vshnot3
050400120620     c*
050500120620     c* se  presenti tutti e 3 accendo F11
050600120620     c                   if        xx=3
050700020226     C                   seton                                        17
050800120620     C                   movel     FunF11(1)     VZCF11
050900120620     C                   MOVEL     'S'           WINFO             1
051000120620     c                   endif
051100020226      *
051200120620     c* Visualizzo i primi due
051300120620     c                   eval      vscva2=notars(1)
051400120621    1c                   if        tipars(1)<>*blanks
051500120620     c
051600120620     C                   MOVE      '7H'          KCOD
051700120620     C                   MOVEL(P)  tipars(1)     KKEY
051800120620     C     KTAB1         CHAIN     TABEL00F                           38
051900120620     C  N38              MOVEL     TBLUNI        DS7H
052000120620     C   38              clear                   DS7H
052100120620    2c                   if        tipars(2)=' '
052200120620     C                   MOVEL     §7HDEB        VSc7H1
052300120620   x2c                   else
052400120830     C                   IF        dsp_mod = 1
052500120830     C                   MOVEL     §7HDEB        VSc7H1
052600120830     C                   ELSE
052700120620     c                   eval      vsc7h1=§7HDEB6+'/'
052800120830     C                   ENDIF
052900120620     c* prima descrizione completa
053000120620     C                   MOVEL     §7HDEB        VSh7H2
053100120620    2c                   endif
053200120620    1c                   endif
053300120620     c
053400120620     c                   eval      vscvar=notars(2)
053500120621    1c                   if        tipars(2)<>*blanks
053600120620     C                   MOVE      '7H'          KCOD
053700120620     C                   MOVEL(P)  tipars(2)     KKEY
053800120620     C     KTAB1         CHAIN     TABEL00F                           38
053900120620     C  N38              MOVEL     TBLUNI        DS7H
054000120620     C   38              clear                   DS7H
054100120620    2c                   if        tipars(1)=' '
054200120620     C                   MOVEL     §7HDEB        VSc7H1
054300120620     c                   else
054400120830     C                   IF        dsp_mod = 1
054500120830     C                   MOVEL     §7HDEB        VSc7H11
054600120830     c* 2 descrizioni perché 132 col.  (le salvo entrambe)
054700120830     c                   eval      vsh7h3=vsc7h1
054800120830     c                   eval      vsh7h31=vsc7h11
054900120830     C                   ELSE
055000120620     c                   eval      vsc7h1=%trim(vsc7h1)+§7HDEB6
055100120830     c* 1 descrizione doppia perché 80 col. (ne salvo una sola)
055200120620     c                   eval      vsh7h3=vsc7h1
055300120830     C                   ENDIF
055400120620    2c                   endif
055500120620    1c                   endif
055600120620     c
055700120620     c                   eval      vshnot2=notars(2)
055800120620     c                   eval      vshnot3=notars(3)
055900020226      *
056000020226     C                   ENDSR
056100980312     C**************************************************************************
056200120620     C* AGGIORNAMENTO SUBFILE PER INTERROGAZIONE ALTRI DATI
056300980312     C**************************************************************************
056400980312     C     UPDS01        BEGSR
056500980312     C                   Z-ADD     1             NRR1
056600120830     C                   IF        dsp_mod = 1
056700120830     C     NRR1          CHAIN     LR38S011                           31
056800120830     C                   ELSE
056900120830     C     NRR1          CHAIN     LR38S01                            31
057000120830     C                   ENDIF
057100980312     C     *IN31         DOWEQ     *OFF
057200120620     c
057300120620     C* VISUALIZZO dati vdl al posto del chi sono --> record 3 col record 2
057400120620     C                   if        WINFO='S'
057500120620     c                   eval      vsc7h1=vsh7h2
057600120830     C                   IF        dsp_mod = 1
057700120830     c                   eval      vsc7h11=*blank
057800120830     C                   ENDIF
057900120620     c                   eval      vscvar=vshnot3
058000120620     c
058100980312     C                   ELSE
058200120620     C* VISUALIZZO "CHI SONO" al posto di dati VDL --> record 2 al posto di record 3
058300120620     c                   eval      vsc7h1=vsh7h3
058400120830     C                   IF        dsp_mod = 1
058500120830     c                   eval      vsc7h11=vsh7h31
058600120830     C                   ENDIF
058700120620     c                   eval      vscvar=vshnot2
058800980312     C                   END
058900020226      *
059000120830     C                   IF        dsp_mod = 1
059100120830     C                   UPDATE    LR38S011
059200120830     C                   ELSE
059300120830     C                   UPDATE    LR38S01
059400120830     C                   ENDIF
059500980312     C                   ADD       1             NRR1
059600120830     C                   IF        dsp_mod = 1
059700120830     C     NRR1          CHAIN     LR38S011                           31
059800120830     C                   ELSE
059900120830     C     NRR1          CHAIN     LR38S01                            31
060000120830     C                   ENDIF
060100980312     C                   ENDDO
060200120620     c
060300120620     c                   if        WINFO='S'
060400120620     C                   movel     FunF11(2)     VZCF11
060500120620     c                   eval      winfo='V'
060600120620     c                   else
060700120620     C                   movel     FunF11(1)     VZCF11
060800120620     c                   eval      winfo='S'
060900120620     c                   endif
061000120620     c
061100980312     C                   ENDSR
061200940720     C**************************************************************************
061300940720     C* PULIZIA SUBFILE
061400940720     C**************************************************************************
061500910205     C     PULSF         BEGSR
061600940720     C*
061700910205     C                   SETON                                        35
061800120830     C                   IF        dsp_mod = 1
061900120830     C                   WRITE     LR38C011                                     SFLCTL
062000120830     C                   ELSE
062100120830     C                   WRITE     LR38C01                                      SFLCTL
062200120830     C                   ENDIF
062300910205     C                   SETOFF                                       35
062400940720     C*
062500910205     C                   ENDSR
062600940720     C**************************************************************************
062700940720     C*  ROUTINE  INIZIALE
062800940720     C**************************************************************************
062900940720     C     *INZSR        BEGSR
063000940720     C*
063100090226     C     KART1         KLIST                                                  FiiRT01L
063200940720     C                   KFLD                    PARAAS                         ..ANNO
063300940720     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
063400940720     C                   KFLD                    PARNRS                         ..NUMERO SERIE
063500940720     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
063600150326     C     KART27        KLIST                                                  FiiRT01L
063700150326     C                   KFLD                    artfls                         ..ANNO
063800150326     C                   KFLD                    artlna                         ..LINEA PARTENZA
063900150326     C                   KFLD                    artNRS                         ..NUMERO SERIE
064000150326     C                   KFLD                    artnsc                         ..NUMERO SPEDIZIONE
064100940720     C     KTAB1         KLIST                                                  TABEL00F
064200940720     C                   KFLD                    KKUT                           ..CODICE UTENTE
064300940720     C                   KFLD                    KCOD                           ..CODICE TABELLA
064400940720     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
064500970805     C     KTAB2         KLIST                                                  TABEL00F
064600970805     C                   KFLD                    KKUT                           ..CODICE UTENTE
064700970805     C                   KFLD                    KCOD                           ..CODICE TABELLA
064800090226     C     KARS1         KLIST                                                  FiART01L
064900980312     C                   KFLD                    ARTFLS
065000980312     C                   KFLD                    ARTLNA                                     ENZA
065100980312     C                   KFLD                    ARTNRS                                     IE
065200980312     C                   KFLD                    ARTNSC                                     DIZIONE
065300980312     C                   KFLD                    WTRC                                       DIZIONE
065400940720     C*
065500940720     C     *LIKE         DEFINE    TBLKUT        KKUT                           CODICE UTENTE
065600940720     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
065700940720     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
065800980312     C     *LIKE         DEFINE    ARSTRC        WTRC                                       NTO
065900940720     C*
066000940720     C                   Z-ADD     1             KKUT
066100970805     C*
066200970805     C* Eseguo caricamento della tabella dei codici consegna anomala
066300970805     C                   Z-ADD     0             X                 3 0
066400970805     C                   MOVEL     '7O'          KCOD
066500970805     C     KTAB2         CHAIN     TABEL                              34
066600970805     C     *IN34         DOWEQ     '0'
066700970805     C     X             ANDLT     100
066800970805     C     TBLFLG        IFEQ      *BLANKS
066900970805     C                   ADD       1             X
067000970805     C                   MOVEL     TBLKEY        C7O(X)
067100970805     C                   MOVEL     TBLUNI        DS7O
067200970805     C                   MOVEL     §7OIDD        F7O(X)
067300970805     C                   END
067400970805     C     KTAB2         READE     TABEL                                  34
067500970805     C                   END
067600120830     C*
067700120830     C* definisco se lo schermo dell'utente supporta o meno il 27X132
067800120830     C                   IF        not %open(FNLR38D) and
067900120830     C                             not %open(FNLR38D1)
068000120830     C                   EVAL      dsp_mod = QueryDisplayModeSupport
068100120830     C                              ('4':*omit :*omit :*omit)
068200120830     C                   IF        dsp_mod=0
068300120830     C***                EVAL      k_sflpag=17
068400120830     C                   OPEN      FNLR38D
068500120830     C                   ELSE
068600120830     C***                EVAL      k_sflpag=17
068700120830     C                   OPEN      FNLR38D1
068800120830     C                   ENDIF
068900120830     C                   ENDIF
069000940720     C*
069100940720     C                   ENDSR
069200160916     C**************************************************************************
069300160916     c     DATI_TES      BEGSR
069400160916     c* Trimmo rif mittente numerico
069500160916     c                   eval      vcdrmn=%trim(%editc(parrmn:'Z'))
069600160916
069700160916     C                   MOVEL     PARDSA        V1CDSA
069800160916     C                   MOVEL     PADLNA        VCDLNA                         DECODIFICA FILIALE
069900160916
070000160916     c* Destinatario (mittente solo nella vecchia int bolle part)
070100160916     c                   clear                   vcdcod
070200160916     c                   clear                   vcdcod1
070300160916     c                   if        parcod>*zeros
070400160916     c                   eval      vcdcod =parcod+'-'+parrsc
070500160916     c                   eval      vcdcod1=parcod+'-'+parrsc
070600160916     c                   else
070700160916     C                   MOVEL     PARRSc        VCDcod
070800160916     C                   MOVEL     PARRSc        VCDcod1
070900160916     c                   endif
071000160916     c                   if        partbo='A'
071100160916     c                   eval      vccdcl1='Destinat'
071200160916     c                   eval      vccdcl ='Destinatario'
071300160916     c                   else
071400160916     c                   eval      vccdcl1 ='Mittente'
071500160916     c                   eval      vccdcl  ='Mittente   '
071600160916     c                   endif
071700160916     c*
071800160916     c* terminal
071900160916     c                   eval      vcdter=%editc(partfp:'X')+'/'+
072000160916     c                                    %editc(partfa:'X')
072100160916     c
072200160916     c                   eval      vcdpar=%editc(partfp:'X')
072300160916     c                   clear                   orgde5
072400160916     c                   clear                   orgdes
072500160916     C     PARtfa        CHAIN     AZORG01L
072600160916     c                   if        orgde5<>*blanks
072700160916     C                   eval      vcdarr=%editc(partfa:'X')+'-' +orgde5
072800160916     c                   else
072900160916     C                   eval      vcdarr=%editc(partfa:'X')+'-'+orgdes
073000160916     c                   endif
073100160916     C*
073200160916     C* 15 ON  - SI TRATTA DI BOLLA EXPORT
073300160916     C     PARLNA        CHAIN     AZORG01L
073400160916     C                   if        %found
073500160916     C                   movel     ORGde3        og143
073600160916     C                   eval      *in15 = (§OgNtw = 'EEX' or §OgNtw ='EUP' or
073700160916     C                                     §OgNtw ='FED' or §OgNtw = 'DPD')
073800160916     C                   endif
073900160916     C*
074000160916     C* IMPOSTA CAMPI PER INT SPUNTE
074100160916     c                   clear                   param1
074200160916     C                   Z-ADD     0             VI1NFV
074300160916     C                   MOVEL     *ZEROS        VI1LNP
074400160916     C                   MOVEL     PARLNA        VI1LNA
074500160916     C                   MOVEL     PARNRS        VI1NRS
074600160916     C                   MOVEL     9             VI1NPG
074700160916     C                   Z-ADD     0             VI1NCD
074800160916     C                   Z-ADD     0             VI1NCA
074900160916     C                   MOVEL     99            VI1NPS
075000160916     C                   MOVEL     'FNLR38R'     VI1PGM
075100160916     C                   MOVEL     '1'           VI1FLG
075200160916     C                   MOVEL     PARNRS        VI1bors
075300160916     C                   MOVEL     PARlnp        VI1bolp
075400160916     C                   MOVEL     PARnsp        VI1bosp
075500160916     C                   MOVEL     PARaas        VI1boan
075600160916     C*
075700160916     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI
075800160916     C                   EXSR      CARART
075900160916     c                   ENDSR
076000020225**
076100051129F11=Dati VDL
076200020226F11=CHI SONO
