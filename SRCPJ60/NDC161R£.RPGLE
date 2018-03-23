000100990309     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200130625>>>>>H*PARMS BNDDIR(PJXBND PJCBND YCO)
000300990427     H*PARMS CVTOPT(*NONE)
000400950608     H DECEDIT('0,') DATEDIT(*DMY.)
000500990427     F*-------------
000600900731     F*
000700950627     F*  PILOTA STAMPA LIBRI IVA
000800900731     F*
000900980126     F*-------------
001000950627    >FANDLI01L  IF   E           K DISK
001100090120C2106F*------------
001200090120  "  FANAIV01L  IF   E           K DISK
001300090120  "  F*------------
001400090120  "  FANAIL01L  IF   E           K DISK
001500090120C2106F*------------
001600020429C1550 * Soc/TpLiq/Anno/Mese/TipTot
001700981112    >FNDIVT01L  UF A E           K DISK
001800950627     D*-------------
001900900731     D KPJBA         E DS
002000950605     D*-------------
002100950130     D* Parametri del PGM
002200950627     D C161DS        E DS                  EXTNAME(NDC161DS)
002300950405     D*-------------
002400950628     D* Parametri del PGM stampa anomalie
002500950628     D C162DS        E DS                  EXTNAME(NDC162DS)
002600950628     D*-------------
002700950628     D* Parametri del PGM stampa libro IVA
002800950628     D C163DS        E DS                  EXTNAME(NDC163DS)
002900030508C1651D                                     INZ
003000950628     D*-------------
003100950703     D* Parametri del PGM cancella riepiloghi
003200950703     D C165DS        E DS                  EXTNAME(NDC165DS)
003300950703     D*-------------
003400000511C1357D* Parametri del pgm NDC262R (stampa prospetto ventilazione)
003500000511  "  D NDC262DS      E DS                  Inz
003600000511C1357D*-------------
003700941103     D* Dati per pgm XSOC
003800941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
003900981112     D    XscIvaRgm                   1    overlay(XscFgo:9)
004000000511C1357D    XScIvaVent                  1A   Overlay(XscFgo:4)
004100941103     D*-------------
004200950605     D* DS Interna per dati utente
004300941103     D XSOCDS          DS          1000
004400981112     D*-------------
004500981112     D* Decodifica periodo
004600981112     D X62IVALQDS    E DS
004700950703     D*-------------
004800950703     D xdtdat          S               D
004900981112     D periodo         S                   like(X62perliq)
005000090120C1651d savtpreg        S                   like(dlitpreg)
005100090120C2106d AILTPREGW       S                   LIKE(AILTPREG)
005200090120     D*-------------
005300090120  "  D* Parametro modulo IVA
005400090120C2106D ANP045DS      E DS
005500130625C2106D*-------------
005600130625>>>>>D YCmdStr         S             64
005700130625>>>>>D YCmdLen         S             15  5
005800130625>>>>>D                                     INZ(%SIZE(YCmdStr))
005900130625     D YSpoolNbr       S              5I 0
006000130625     D YAnno           S              4
006100130625     D YPeriodo        S              2
006200130625     D YRegistro       S              1
006300950405     C*----------------------------------------------------*
006400950405     C*                MAIN LINE PROGRAM
006500950405     C*----------------------------------------------------*
006600990427     C*
006700950606     C* Inizializzazione variabili
006800950606     C                   EXSR      INZVAR
006900950703     C*
007000990427     C* Se regime trasportatori e richiesto aggiornamento
007100990427     C*  cancello record di iva slittata
007200990427     C                   If        XscIvaRgm = '1'
007300990309     C                             and agr161 = *on
007400981112     C                   EXSR      CANIVT
007500990427     C                   EndIf
007600990427     C*
007700090120C2106C                   If         GesPlur045 = *on
007800090120  "  C                   Move      '1'           ailtpregw
007900090120  "  C                   Exsr      REDPLUR
008000090120C2106C                   ElSe
008100090120     C                   Move      soc161        dlisocieta
008200950627     C*
008300950627     C* Registro acquisti
008400950627     C     acq161        ifeq      *on
008500950627     C                   move      '1'           dlitpreg
008600950703     C                   move      'IVAACQ'      XDTTIP
008700950628     C                   EXSR      REDDLI
008800130625>>>>>** Creazione file database per CD.
008900130625>>>>>C                   CALL(E)   'YCO1611R'
009000130625>>>>>C                   PARM                    Soc161
009100130625>>>>>C                   PARM                    Bol161
009200130625>>>>>C                   PARM                    DtI161
009300130625>>>>>C                   PARM                    DtF161
009400130625>>>>>C                   PARM      '*'           YJob             26
009500130625>>>>>C                   PARM      'I'           YTpStampa         1
009600130625>>>>>C                   PARM      *BLANK        YEsito            1
009700130625     C                   PARM                    YSpoolNbr
009800130625     C                   PARM                    YAnno
009900130625     C                   PARM                    YPeriodo
010000130625     C                   PARM      '1'           YRegistro
010100950627     C                   endif
010200950627     C*
010300950627     C* Registro vendite
010400950627     C     ven161        ifeq      *on
010500950627     C                   move      '2'           dlitpreg
010600950703     C                   move      'IVAVEN'      XDTTIP
010700950628     C                   EXSR      REDDLI
010800130625>>>>>** Creazione file database per CD.
010900130625>>>>>C                   CALL(E)   'YCO1611R'
011000130625>>>>>C                   PARM                    Soc161
011100130625>>>>>C                   PARM                    Bol161
011200130625>>>>>C                   PARM                    DtI161
011300130625>>>>>C                   PARM                    DtF161
011400130625>>>>>C                   PARM      '*'           YJob             26
011500130625>>>>>C                   PARM      'I'           YTpStampa         1
011600130625>>>>>C                   PARM      *BLANK        YEsito            1
011700130625     C                   PARM                    YSpoolNbr
011800130625     C                   PARM                    YAnno
011900130625     C                   PARM                    YPeriodo
012000130625     C                   PARM      '2'           YRegistro
012100950627     C                   endif
012200950627     C*
012300950627     C* Registro corrispettivi
012400950627     C     cor161        ifeq      *on
012500950627     C                   move      '3'           dlitpreg
012600950703     C                   move      'IVACOR'      XDTTIP
012700950628     C                   EXSR      REDDLI
012800950627     C                   endif
012900090120C2106C                   endif
013000000511C1357C*
013100000511  "  C* Se la società gestisce la ventilazione ...
013200000511  "  C                   If        XScIvaVent = '1'
013300000511  "  C* Stampa prospetto ventilazione
013400000511  "  C                   ExSr      StpProspV
013500000511C1357C                   EndIf
013600900731     C*
013700950606     C* Fine programma
013800950606     C                   EXSR      ENDPGM
013900950606     C*
014000950606     C/EJECT
014100950606     C************************************************************
014200950606     C* FINE PROGRAMMA
014300950606     C************************************************************
014400950606     C     ENDPGM        BEGSR
014500950606     C*
014600950627     C                   MOVEL     C161DS        KPJBU
014700950606     C                   SETON                                        LR
014800950606     C                   RETURN
014900950606     C*
015000950606     C                   ENDSR
015100950606     C/EJECT
015200950628     C************************************************************
015300950628     C* LETTURA ANDLI
015400950628     C************************************************************
015500950628     C     REDDLI        BEGSR
015600950703     C*
015700981022     C* Cancellazione riepiloghi se richiesto aggiornamento
015800981022     C     agr161        ifeq      *on
015900950703     C                   EXSR      CANRIE
016000981112     C                   endif
016100000517C1357C*
016200000517  "  C* Aggiorno data ultima stampa definitiva
016300000517  "  C     bol161        ifeq      *on
016400000517  "  C                   move      dtf161        XDTDAT
016500000517  "  C                   EXSR      AGGDAT
016600000517C1357C                   endif
016700950628     C*
016800950703     C     k2dli1        SETLL     andli01l
016900950703     C     k2dli1        READE     andli01l                               21
017000950628     C*
017100950628   B1C     *IN21         DOWEQ     *OFF
017200980113     C*
017300950628     C* Stampa anomalie
017400130625>>>>>C**   anm161        ifeq      *on
017500130625>>>>>C**                 EXSR      STPANM
017600130625>>>>>C**                 endif
017700950628     C*
017800950628     C* Stampa libro IVA
017900950628     C                   EXSR      STPIVA
018000130625>>>>>C*
018100130625>>>>>C* Stampa anomalie
018200130625>>>>>C     anm161        ifeq      *on
018300130625>>>>>C                   EXSR      STPANM
018400130625>>>>>C                   endif
018500980113     C*
018600950703     C     k2dli1        READE     andli01l                               21
018700950628     C*
018800950628   E2C                   ENDDO
018900000517C1357X*
019000000517  "  X****** Aggiorno data ultima stampa definitiva
019100000517  "  X*****bol161        ifeq      *on
019200000517  "  X*****              move      dtf161        XDTDAT
019300000517  "  X*****              EXSR      AGGDAT
019400000517C1357X*****              endif
019500950628     C*
019600950628     C                   ENDSR
019700950628     C/EJECT
019800950703     C************************************************************
019900950703     C* CANCELLAZIONE RIEPILOGHI
020000950703     C************************************************************
020100950703     C     CANRIE        BEGSR
020200950703     C*
020300980826     C                   extrct    dti161:*m     mesei             2 0
020400980825     C                   extrct    dti161:*y     anno              4
020500980826     C                   extrct    dtf161:*m     mesef             2 0
020600980825     C*
020700980825     C                   move      soc161        soc165
020800980825     C                   move      anno          ann165
020900980825     C                   move      dlitpreg      tpr165
021000990427     C                   move      mesei         mese              2 0
021100980825     C*
021200990427     C                   DOU       mese >mesef
021300990427     C                   move      mese          mes165
021400980825    >C                   MOVEL     C165DS        KPJBU
021500980825    >C                   CALL      'NDC165R'
021600980825     C                   PARM                    KPJBA
021700980825    >C                   MOVEL     KPJBU         C165DS
021800990427     C                   eval      mese =mese+1
021900990427     C                   ENDDO
022000990427     C*
022100950703     C                   ENDSR
022200950703     C/EJECT
022300981112     C************************************************************
022400981113     C* CANCELLAZIONE TOTALE IVA SLITTATA SU NDIVT00F
022500981112     C************************************************************
022600981112     C     CANIVT        BEGSR
022700981112     C*
022800981112     C                   extrct    dti161:*m     mesei
022900981112     C                   extrct    dti161:*y     anno
023000990427     C*
023100990427     C* Ricerco periodo di riferimento da data protocollo inizio
023200981112     C                   reset                   x62ivalqds
023300981112     C                   move      soc161        x62societa
023400981112     C                   move      anno          x62anno
023500981112     C                   move      mesei         x62mese
023600981112     C                   call      'X62IVALQ'
023700981112     C                   parm                    x62ivalqds
023800981112   B3C     x62esito      ifeq      '0'
023900981112     C                   move      x62perliq     periodo
024000981112   E3C                   endif
024100981112     C*
024200981112     C                   move      soc161        ivtsoc
024300981112     C                   move      anno          ivtanno
024400981112     C                   move      periodo       ivtmese
024500981112     C                   move      'I'           ivttiptot
024600990427     C*
024700090120C2106C                   If         GesPlur045 = *on
024800090120  "  C     k5ivt1        setll     ndivt01l                           23
024900090120  "  C     *in23         doweq     *off
025000090120  "  C     K5ivt1        reade     ndivt01l                               23
025100090120  "  C     *in23         ifeq      *off
025200090120  "  C                   exsr      updivt
025300090120  "  C                   endif
025400090120  "  C                   enddo
025500090120C2106C                   else
025600020429C1550X**** k4ivt1        chain     ndivt01l                           23
025700020429C1550 * solo in regime iva traportat. normale il tipo liq è valorizzata
025800020429C1550C                   move      *blank        IVTTpLiq
025900020429C1550C     k5ivt1        chain     ndivt01l                           23
026000981112     C     *in23         ifeq      *off
026100090120C2106C                   exsr      updivt
026200090120  "  C***                z-add     0             ivttotale
026300090120  "  C***                move      '0'           ivtdare
026400090120  "  C***                move      '0'           ivtavere
026500090120  "  C***                z-add     0             ivtalmesal
026600090120  "  C***                move      *off          ivtdare
026700090120  "  C***                move      *off          ivtavere
026800090120C2106C***                update    ndivt000
026900981112     C                   endif
027000090120C2106C                   endif
027100990427     C*
027200981112     C                   ENDSR
027300981112     C/EJECT
027400090120C2106C************************************************************
027500090120  "  C* AZZERA TOTALI SU NDIVT
027600090120  "  C************************************************************
027700090120  "  C     UPDIVT        BEGSR
027800090120  "  C*
027900090120  "  C                   z-add     0             ivttotale
028000090120  "  C                   move      '0'           ivtdare
028100090120  "  C                   move      '0'           ivtavere
028200090120  "  C                   z-add     0             ivtalmesal
028300090120  "  C                   move      *off          ivtdare
028400090120  "  C                   move      *off          ivtavere
028500090120  "  C                   update    ndivt000
028600090120  "  C*
028700090120  "  C                   ENDSR
028800090120C2106C/EJECT
028900950628     C************************************************************
029000950628     C* STAMPA ANOMALIE
029100950628     C************************************************************
029200950628     C     STPANM        BEGSR
029300950628     C*
029400950628     C                   move      soc161        soc162
029500960515     C                   move      ctb161        ctb162
029600950628     C                   move      dlitpreg      tpr162
029700950628     C                   move      dlilibro      lib162
029800950628     C                   move      dlidesbrev    dbr162
029900950628     C                   move      dti161        dti162
030000950628     C                   move      dtf161        dtf162
030100030325D1300C                   move      RSTA161       RSTA162
030200030325  "  C                   move      RSTV161       RSTV162
030300030325D1300C                   move      RSTC161       RSTC162
030400090612D2460c                   move      Bol161        Bol162
030500950628     C*
030600950628    >C                   MOVEL     C162DS        KPJBU
030700950628    >C                   CALL      'NDC162R'
030800950628     C                   PARM                    KPJBA
030900950628    >C                   MOVEL     KPJBU         C162DS
031000950628     C*
031100950628     C                   ENDSR
031200950628     C/EJECT
031300950628     C************************************************************
031400950628     C* STAMPA LIBRO IVA
031500950628     C************************************************************
031600950628     C     STPIVA        BEGSR
031700950628     C*
031800950628     C                   move      soc161        soc163
031900960515     C                   move      ctb161        ctb163
032000950628     C                   move      dlitpreg      tpr163
032100950628     C                   move      dlilibro      lib163
032200950628     C                   move      dlidesbrev    dbr163
032300950628     C                   move      dti161        dti163
032400950628     C                   move      dtf161        dtf163
032500950628     C                   move      tot161        tot163
032600950628     C                   move      bol161        bol163
032700960705     C                   move      dlisospens    sos163
032800980126     C                   move      ind161        ind163
032900980723     C                   move      agr161        agr163
033000011214c1529C                   move      int161        int163
033100030310D1300C                   move      RSTA161       RSTA163
033200030310  "  C                   move      RSTV161       RSTV163
033300030310D1300C                   move      RSTC161       RSTC163
033400030508c1651c* Pulisco numero di pagina al cambio registro
033500030508  "  c                   if        DliTpreg <>SavTpReg
033600030508  "  c                   clear                   pag163
033700030508c1651c                   endif
033800950628     C*
033900950628    >C                   MOVEL     C163DS        KPJBU
034000950628    >C                   CALL      'NDC163R'
034100950628     C                   PARM                    KPJBA
034200950628    >C                   MOVEL     KPJBU         C163DS
034300030508c1651c* salvo tipo registro
034400030508c1651c                   eval      SavTpreg = DliTpReg
034500950628     C*
034600950628     C                   ENDSR
034700950628     C/EJECT
034800950703     C************************************************************
034900950703     C* AGGIORNA DATE ULTIME STAMPE DEFINITIVE
035000950703     C************************************************************
035100950703     C     AGGDAT        BEGSR
035200950703     C*
035300950703     C* Aggiorno data
035400950703     C                   MOVE      '4'           XDTRIC
035500950703     C                   MOVE      soc161        XDTSOC
035600950703     C                   MOVE      *blanks       XDTUNI
035700950703     C                   MOVE      ctb161        XDTCTB
035800950703     C                   MOVE      *BLANKS       XDTKEY
035900950703     C                   MOVE      *OFF          XDTCMT
036000950703     C                   MOVE      *on           XDTALC
036100950703     C*
036200950703     C                   CALLB     'XDATR'       PDAT
036300950703     C*
036400950703     C                   ENDSR
036500950703     C/EJECT
036600000511C1357C************************************************************
036700000511  "  C* Stampa prospetto ventilazione
036800000511C1357C************************************************************
036900000511     C     StpProspV     BEGSR
037000000511     C*
037100000511     C                   Clear                   NDC262DS
037200000511     C                   Eval      Societa262 = Soc161
037300000511     C                   Eval      Ctb262 = Ctb161
037400000511     C                   Eval      DataI262 = DtI161
037500000511     C                   Eval      DataF262 = DtF161
037600000511     C                   Eval      Bollato262 = Bol161
037700000511     C*
037800000511     C                   Call      'NDC262R'
037900000511     C                   Parm                    KPJBA
038000000511     C                   Parm                    NDC262DS
038100000511     C*
038200000511C1357C                   ENDSR
038300950405     C************************************************************
038400900731     C* Definizioni e inizializzazioni
038500950405     C************************************************************
038600940831     C     *INZSR        BEGSR
038700900731     C*
038800900731     C     *ENTRY        PLIST
038900900731     C                   PARM                    KPJBA
039000941104     C*
039100940831     C                   ENDSR
039200990427     C/EJECT
039300950606     C************************************************************
039400950606     C* INIZIALIZZAZIONE VARIABILI
039500950606     C************************************************************
039600950606     C     INZVAR        BEGSR
039700950606     C*
039800950627     C                   MOVEL     KPJBU         C161DS
039900950627     C*
040000950627     C* KLIST per ANDLI
040100950627     C     K2DLI1        KLIST
040200950627     C                   KFLD                    DLISOCIETA
040300950630     C                   KFLD                    DLITPREG
040400950703     C*
040500981112     C* klist per NDIVT
040600020429C1550X**** K4IVT1        KLIST
040700020429C1550C     K5IVT1        KLIST
040800981112     C                   KFLD                    ivtsoc
040900020429C1550C                   KFLD                    IVTTpLiq
041000981112     C                   KFLD                    ivtanno
041100981112     C                   KFLD                    ivtmese
041200981112     C                   KFLD                    ivttiptot
041300090120C2106C* KLIST per ANAIL
041400090120  "  C     K2AIL1        KLIST
041500090120  "  C                   KFLD                    AIVSOCIETA
041600090120  "  C                   KFLD                    AIVCDATT
041700090120  "  C* KLIST per ANDLI
041800090120  "  C     K3DLI1        KLIST
041900090120  "  C                   KFLD                    AILSOCIETA
042000090120  "  C                   KFLD                    AILTPREG
042100090120C2106C                   KFLD                    AILLIBRO
042200990427     C*
042300950703     C* Plist per XDATR
042400950703     C     PDAT          PLIST
042500950703     C                   PARM                    XDTRIC            1
042600950703     C                   PARM                    XDTSOC            3
042700950703     C                   PARM                    XDTUNI            8
042800950703     C                   PARM                    XDTCTB            2
042900950703     C                   PARM                    XDTKEY            8
043000950703     C                   PARM                    XDTALC            1
043100950703     C                   PARM                    XDTCMT            1
043200950703     C                   PARM                    XDTDAT
043300950703     C                   PARM                    XDTTIP            6
043400950703     C                   PARM                    XDTERR            1
043500950703     C                   PARM                    XDTTST            1
043600990427     C*
043700950606     C* Reperimento azienda
043800950627     C                   MOVE      SOC161        SOCXSC
043900950606     C                   MOVEL     'SOC001'      TIPXSC
044000950606     C                   EXSR      REPSOC
044100950606     C     RTNXSC        IFNE      '1'
044200950606     C                   MOVEL     XSOCDS        SOC001
044300950606     C                   ELSE
044400950627     C                   MOVE      *ON           ERR161
044500950606     C                   EXSR      ENDPGM
044600950606     C                   ENDIF
044700090120C2106C*
044800090120  "  C* Reperimento parametro IVA (anagrafiche iva)
044900090120  "  C                   MOVEL     'IVA     '    xpapar
045000090120  "  C                   MOVEL     *OFF          xpaerr
045100090120  "  C                   MOVEL     ' '           xparic
045200090120  "  C                   CALLb     'XPAR'
045300090120  "  C                   PARM                    xscsoc
045400090120  "  C                   PARM                    xpapar            8
045500090120  "  C                   PARM                    xpaout         2000
045600090120  "  C                   PARM                    xpaerr            1
045700090120  "  C                   PARM                    xparic            1
045800090120  "  C     xpaerr        ifeq      *ON
045900090120  "  C                   MOVE      *ON           ERR161
046000090120  "  C                   EXSR      ENDPGM
046100090120  "  C                   else
046200090120  "  C                   movel     xpaout        ANP045DS
046300090120C2106C                   endif
046400950606     C*
046500950606     C                   ENDSR
046600950606     C/EJECT
046700990427     C************************************************************
046800990427     C* Reperimento dati società
046900990427     C************************************************************
047000941103     C     REPSOC        BEGSR
047100990427     C*
047200950630     C                   CALLB     'XSOC'
047300941103     C                   PARM                    TIPXSC            6
047400941103     C                   PARM                    SOCXSC            3
047500941103     C                   PARM                    CDSXSC            9 0
047600941103     C                   PARM                    MODXSC            3
047700941103     C                   PARM      *BLANKS       RTNXSC            1
047800941103     C                   PARM                    XSOCDS
047900941103     C                   PARM                    KPJBA
048000990427     C*
048100941103     C                   ENDSR
048200090120C2106C******************************************************
048300090120  "  C* Lettura ANAIV01L e ANAIL01L
048400090120  "  C******************************************************
048500090120  "  C     REDPLUR       BEGSR
048600090120  "  C* Cancellazione riepiloghi se richiesto aggiornamento
048700090120  "  C     agr161        ifeq      *on
048800090120  "  C* Registro acquisti
048900090120  "  C     acq161        ifeq      *on
049000090120  "  C                   move      '1'           dlitpreg
049100090120  "  C                   EXSR      CANRIE
049200090120  "  C                   endif
049300090120  "  C* Registro vendite
049400090120  "  C     ven161        ifeq      *on
049500090120  "  C                   move      '2'           dlitpreg
049600090120  "  C                   EXSR      CANRIE
049700090120  "  C                   endif
049800090120  "  C* Registro corrispettivi
049900090120  "  C     cor161        ifeq      *on
050000090120  "  C                   move      '3'           dlitpreg
050100090120  "  C                   EXSR      CANRIE
050200090120  "  C                   endif
050300090120  "  C                   endif
050400090120  "  C*
050500090120  "  C* Aggiorno data ultima stampa definitiva
050600090120  "  C     bol161        ifeq      *on
050700090120  "  C                   move      dtf161        XDTDAT
050800090120  "  C* Registro acquisti
050900090120  "  C     acq161        ifeq      *on
051000090120  "  C                   move      'IVAACQ'      XDTTIP
051100090120  "  C                   EXSR      AGGDAT
051200090120  "  C                   endif
051300090120  "  C* Registro vendite
051400090120  "  C     ven161        ifeq      *on
051500090120  "  C                   move      'IVAVEN'      XDTTIP
051600090120  "  C                   EXSR      AGGDAT
051700090120  "  C                   endif
051800090120  "  C* Registro corrispettivi
051900090120  "  C     cor161        ifeq      *on
052000090120  "  C                   move      'IVACOR'      XDTTIP
052100090120  "  C                   EXSR      AGGDAT
052200090120  "  C                   endif
052300090120  "  C                   endif
052400090120  "  C* Lettura ANAIV01L
052500090120  "  C     XSCSOC        SETLL     ANAIV01L                           33
052600090120  "  C     *IN33         DOWEQ     *OFF
052700090120  "  C     XSCSOC        READE     ANAIV01L                               33
052800090120  "  C                   IF        (*IN33 = *OFF)
052900090120  "  C* Lettura ANAIL01L per soc./cod.attività
053000090120  "  C     K2AIL1        SETLL     ANAIL01L                           34
053100090120  "  C     *IN34         DOWEQ     *OFF
053200090120  "  C     K2AIL1        READE     ANAIL01L                               34
053300090120  "  C                   IF        (*IN34 = *OFF)
053400090120  "  C                   move      *blank        wokrec            1
053500090120  "  C* Registro acquisti
053600090120  "  C     acq161        ifeq      *on
053700090120  "  C     ailtpreg      andeq     '1'
053800090120  "  C                   eval      wokrec = 'Y'
053900090120  "  C                   endif
054000090120  "  C* Registro vendite
054100090120  "  C     ven161        ifeq      *on
054200090120  "  C     ailtpreg      andeq     '2'
054300090120  "  C                   eval      wokrec = 'Y'
054400090120  "  C                   endif
054500090120  "  C* Registro corrispettivi
054600090120  "  C     cor161        ifeq      *on
054700090120  "  C     ailtpreg      andeq     '3'
054800090120  "  C                   eval      wokrec = 'Y'
054900090120  "  C                   endif
055000090120  "  C* Per ogni combinazione tipo reg./libro dell'attività esegue
055100090120  "  C* attività richiesta
055200090120  "  C                   if        (wokrec = 'Y')
055300090120  "  C     k3dli1        CHAIN     andli01l                           21
055400090120  "  C                   IF        (*IN21 = *OFF)
055500090120  "  C* Stampa anomalie
055600090120  "  C     anm161        ifeq      *on
055700090120  "  C                   EXSR      STPANM
055800090120  "  C                   endif
055900090120  "  C* Stampa libro IVA
056000090120  "  C                   EXSR      STPIVA
056100090120  "  C                   ENDIF
056200090120  "  C                   ENDIF
056300090120  "  C                   ENDIF
056400090120  "  C                   ENDDO
056500090120  "  C                   ENDIF
056600090120  "  C                   ENDDO
056700090120  "  C*
056800090120C2106C                   ENDSR
