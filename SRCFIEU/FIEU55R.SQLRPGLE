000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300051010     Ffieu55d   CF   E             WORKSTN
000400060926     f                                     sfile(VIDSFL:snrr1)
000500060928     F                                     INFDS(infDspF)
000600061023     Ffidpo00f  uf   E             disk
000700061023     Ffidpo01l  if   E           k disk    rename(fidpo000:fidpo1)
000800061023     F                                     INFDS(FIdpo1Ds)
000900051011     Ffidpo03l  if   E           k disk    rename(fidpo000:fidpo3)
001000061023     F                                     INFDS(FIdpo3Ds)
001100061003     Fazorg01l  if   E           k disk
001200051010     Ftntbe01l  if   E           k disk
001300061003      *
001400061204     Ffnorm01l  if   E           k disk    rename(fnorm000:fnorm1)
001500061005     Ffnorm07l  if   E           k disk
001600061003     Ffnblp01l  if   E           k disk
001700061003     Ffiar403l  if   E           k disk
001800061023      *
001900120216     Ffnorf01l  if   E           k disk
002000120216      *
002100061023     D FIdpo1DS        DS
002200061023     D  nrrecDPO1            397    400I 0
002300061023      *
002400061023     D FIdpo3DS        DS
002500061023     D  nrrecDPO3            397    400I 0
002600061023      *
002700020103     D Psds           SDS
002800020103     D  PgmName          *PROC
002900060928     D infDspF         DS
003000060928     D  tasto                369    369
003100060928     D  act_row              382    382
003200060928     D  act_col              383    383
003300060928     D                 DS
003400060928     D  rowInt                 1      2I 0
003500060928     D                                     INZ
003600060928     D  rowChr                 2      2
003700060928     D  colInt                 3      4I 0
003800060928     D                                     INZ
003900060928     D  colChr                 4      4
004000011228     D Kpjba         E DS
004100060927     D savKpjbu        s                   like(kpjbu)
004200011228      *
004300070612     D dsbl4M        E DS
004400030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004500030604     D CNCR80        E DS
004600080527     D TRUL06DS      E DS
004700080527     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
004800080527     D*
004900060926     D fieu55ds      E DS
005000061002     D dmrd          E DS
005100061003      *
005200061003     D*VISUALIZZAZIONE ORM - FIOR07R
005300061003     D Parm01          DS
005400061003     D  ppoe                          3  0
005500061003     D  pnor                          7  0
005600061003     D  pnsr                          2  0
005700061003     D  pnrv                          2  0
005800061003     D  psce                          1
005900061003     D  pfgs                          3  0
006000061003     D  ppor                          3  0
006100061003     D  pdtr                          8  0
006200061003     D  pmdb                         10
006300061003     D  pprb                         10
006400061003     D  pdts                          8  0
006500061003     D  prmp                          1
006600061003     D  pbrc                          1
006700061003     D  pref                          2
006800061003     D  pmio                          1
006900060928
007000061003     D* DATI DA PGM RICHIAMANTE               - FNLS04R - FNLR36R -
007100170124     D*PARAM0          DS
007200170124     D* PA0AAS                 1      4  0
007300170124     D* PA0LNP                 5      7  0
007400170124     D* PA0NRS                 8      9  0
007500170124     D* PA0NSP                10     16  0
007600170124     D* PA0F03                17     17
007700170124     D* PA0FLG                18     18
007800170124     D* PA0A9                 19     19
007900170124     D* PA0B5                 20     20
008000170124     D* PA075                 21     21
008100170124     D* PA098                 22     22
008200170124     D* PA0RSU                23     42
008300170124     D* PA0CML                43     43
008400170124     D* PA0SIM                44     46  0
008500061003      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
008600170124     D* PA0CLI                47     47
008700061003      * PA0GIA indica se il pgm e' richiamato dalle giacenze
008800170124     D* PA0GIA                48     48
008802170124      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
008803170124     D PARAMu6ds       DS
008804170124     d* CAMPI DI OUTPUT:
008805170124     d* . Key spedizione
008806170124     D  pa1AAS                14     17  0
008807170124     D  pa1LNP                18     20  0
008808170124     D  pa1NRS                21     22  0
008809170124     D  pa1NSP                23     29  0
008810170124     d* . Flag '1'=Premuto F3=Fine
008811170124     D  PA1F03                30     30
008812170124     d* CAMPI DI INPUT:
008813170124     d* . Modalità di richiamo
008814170124     D  PA1FLG                31     31
008815170124     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
008816170124     d*                                in interrogazione della bolla ed evitare
008817170124     d*                                errore di chiamata ricorsiva
008818170124     D  PA1GIA               144    144
008819170124     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
008820170124     d  pa1rma               149    163
008821170124     d  pa1xco               164    164
008822170124     d* non utilizzato
008823170124     d  pa1flo               165    256
008824170124      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
008825170124     D PARAMU6ds1      DS
008826170124     d** INPUT/OUTPUT (se richiesta la selezione bolla)
008827170124     D  pa1AAS_               14     17  0
008828170124     D  pa1LNP_               18     20  0
008829170124     D  pa1NRS_               21     22  0
008830170124     D  pa1NSP_               23     29  0
008831170124     d** OUTPUT
008832170124     D  PA1F03_               30     30
008833170124     d** INPUT
008834170124     D  PA1FLG_               31     31
008835170124     D*    SE RICH DA PGM GIAC. = 1
008836170124     D  PA1GIA_              144    144
008837170124     d** OUTPUT
008838170124     d  pa1rma_              149    163
008839170124     d  pa1xco_              164    164
008840170124     d** INPUT
008841170124     d*   Tipo lancio: "C" -           CHIUSO CON LR
008842170124     d*                "L" - ELABORO E CHIUDO CON LR
008843170124     d*                " " - ELABORO E CHIUDO IN RETRN
008844170124     d  pa1tla_              165    165
008845170124     d** OUTPUT - Codice di errore 1=Sped. non trovata
008846170124     d  pa1err_              166    166
008847170124     d  pa1flo_              167    256
008900061003      *
009000011227     d data            ds            10
009100011228     d aa                      3      4
009200011228     d mm                      6      7
009300011228     d gg                      9     10
009400011228      *
009500030604     D x               S              4  0
009600011227     D WrkEofS01       S              1
009700011227     D WrkCarS01       S              1
009800011228     D $VIDEO          S             10
009900030604     D snrr1           S              5i 0
010000030604     D SavOpzione      S                   like(v1csce)
010100030606     d dataiso         s               d   datfmt(*iso)
010200030606     d dataeur         s               d   datfmt(*eur)
010300030613     D WLBDA8          DS
010400030613     D  G02DAT                 1      8  0
010500030613     D  G02INV                 9     16  0
010600030613     D  G02ERR                17     17
010700030613     D  G02TGI                18     22  0
010800061005
010900120216     d Linea_di_altra_HUB...
011000120216     d                 s              3
011100061005     D alfa10          s             10
011200061005     D alfa15          s             15
011300080509      *
011400080509     D ChiaveORM       s             14
011500080509     D Pacchi_x_ORM    s              5  0
011600060926      *
011700060928     D Enter           C                   X'F1'
011800060928     D F1              C                   X'31'
011900060928     D F3              C                   X'33'
012000060928     D F4              C                   X'34'
012100060928     D F5              C                   X'35'
012200060928     D F7              C                   X'37'
012300060928     D F6              C                   X'36'
012400060928     D F8              C                   X'38'
012500060928     D F12             C                   X'3C'
012600060928     D F14             C                   X'B2'
012700060928     D F18             C                   X'B6'
012800060926      *---------------- ingresso a pgm (attenzione se richiamato x interrogazione)
012900060926     C     *ENTRY        PLIST
013000060926     C                   PARM                    Kpjba
013100060927      *   se richiamato
013200060927     c                   exsr      test_call
013300060927      *
013400030604      *---------------- Gestione formato video video01----------------------
013500060926     C                   EXSR      InzWinSel
013600060927     C                   EXSR      ChkWinSel
013700060926      **
013800060927     C                   DOu       $Video <>'WINSEL'
013900060925      **
014000000608      *----------- Visualizzo dati di output in caso di errori.-------------
014100000608     C                   IF        *IN99
014200060926     C                   setoff                                       99
014300060926     C                   WRITE     winsel
014400060926     C                   seton                                        99
014500000608     C                   ENDIF
014600000608      *
014700051010     c     emetti        tag
014800060927      * solo se deve emettere il primo video di selezione
014900060927     C                   if        chiamato = *blank
015000060926     C                   EXFMT     winsel
015100060927     c                   end
015200000608      *
015300011227     C                   SELECT
015400060928      *   int.ORM se non è chiamato
015500060928     c                   when      tasto = F8 and *in21 = *off
015600060928     c                   movel     kpjbu         savkpjbu
015700061024     c                   movel(p)  'ID'          kpjbu
015800150923     C                   CALL      'FIOR010R'
015900060928     C                   PARM                    KPJBA
016000060928     c                   movel     savkpjbu      kpjbu
016100060928     c*
016200060928     C                   WHEN      tasto = F4
016300060927     C                   Exsr      Search
016400060927     C                   eval      $Video = 'WINSEL'
016500060927     C                   EXSR      ChkWinSel
016600060927
016700060928     C                   WHEN      tasto = F3 or tasto = F12
016800011227     C                   EVAL      $Video = *BLANKS
016900011227
017000060928     C                   WHEN      tasto <>F6 and chiamato= *BLANK
017100060926     C                   eval      $Video = 'WINSEL'
017200060926     C                   EXSR      ChkWinSel
017300060926
017400060926     C                   Other
017500060926     C                   EXSR      ChkWinSel
017600051010     c
017700060926      *  emissione della lista in base alle selezioni fatte
017800060926      *    se selezionato un solo record viene bypassato il SFL ed emesso
017900060926      *   esclusivamente la finestra contenente il dettaglio del singolo record.
018000060926     C  N99              EXSR      WrkVIDSFLc
018100060926      *
018200031020     C     OK            CABEQ     0             EMETTI
018300011227     C                   ENDSL
018400011227      *
018500011227     C                   ENDDO
018600011227      *
018700060926     C                   seton                                        LR
018800060926     C                   RETURN
018900060926     c**********************************************************************
019000060927     c* Controllo se il pgm è richiamato
019100060926     c**********************************************************************
019200060927     C     test_call     BEGSR
019300060927     **
019400060927     c                   if        kpjbu = *blank
019500060927     C                   clear                   fieu55ds
019600060927     c                   else
019700060927     C                   movel     Kpjbu         fieu55ds
019800060927     c                   end
019900060927
020000060927     c                   clear                   chiamato          1
020100060927     c                   if        i55IE  <> *blank or
020200060927     c                             i55SCA <> *blank or
020300060927     c                             i55VIS <> *blank or
020400060927     c                             i55MAR <> *blank or
020500060927     c                             i55NRP <> *blank or
020600060927     c                             i55DEP <> *blank or
020700060927     c                             i55NOR <> *zero  or
020800060927     c                             i55POE <> *zero  or
020900060927     c                             i55NRS <> *zero  or
021000060927     c                             i55NRO <> *zero  or
021100060927     c                             i55NRV <> *zero  or
021200060927     c                             i55DTR <> *zero  or
021300060927     c                             i55DRC <> *zero  or
021400060927     c                             i55STS <> *zero  or
021500060927     c                             i55ORD <> *zero
021600060927     c                   move      'S'           chiamato
021700060927     c                   end
021800060927      **
021900060926     C                   ENDSR
022000060927     c**********************************************************************
022100060927     c* inizializza r01
022200060927     c**********************************************************************
022300060927     C     InzWinSel     BEGSR
022400060927
022500060927     C                   eval      $Video = 'WINSEL'
022600060927     c                   clear                   winsel
022700060929     c                   move      '2'           vsordina
022800061006     c                   move      'I'           vsIE
022900061006     c                   move      'N'           vsVIS
023000060927     c                   move      knsif         vknsif
023100060927     c                   move      knmus         vknmus
023200060927      *** se richiamato
023300060927     c                   if        chiamato <> *blank
023400060927
023500060927     C                   EVAL      $Video = 'VIDSFLC'
023600060927
023700060927     c                   eval      VSIE    =  i55IE
023800060927     c                   eval      VSSCA   =  i55SCA
023900060927     c                   eval      VSVIS   =  i55VIS
024000060927     c                   eval      VSMAR   =  i55MAR
024100060927     c                   eval      VSNRP   =  i55NRP
024200060927     c                   eval      VSDEP   =  i55DEP
024300060927     c                   eval      VSNOR   =  i55NOR
024400060927     c                   eval      VSPOE   =  i55POE
024500060927     c                   eval      VSNRS   =  i55NRS
024600060927     c                   eval      VSNRORD =  i55NRO
024700060927     c                   eval      VSNRVIA =  i55NRV
024800060927     c                   eval      VSDTRS  =  i55DTR
024900080611     c                   eval      VSDTRSa =  i55DTR
025000060927     c                   eval      VSDRIC  =  i55DRC
025100080611     c                   eval      VSDRICa =  i55DRC
025200060927     c                   eval      VSSTS   =  i55STS
025300060927     c                   eval      VSORDINA=  i55ORD
025400060928     c                   if        VSORDINA < 1 or VSORDINA > 2
025500060928     c                   z-add     1             VSORDINA
025600060928     c                   end
025700060929     c                   if        VSie = *blank
025800060929     c                   if        VsDEP='0837' or VsPOE <> 190 and VsPOE <> 0
025900060929     c                   move      'E'           VSie
026000060929     c                   end
026100060929     c                   if        VSie =*blank and VsDEP <> *blank and
026200060929     c                             VsDEP <>'0837' or VsPOE = 190 and VsPOE <>0
026300060929     c                   move      'I'           VSie
026400060929     c                   end
026500060927     c                   end
026600060929     c                   end
026700060927
026800060927     C                   EXSR      ChkWinSel
026900060927
027000060927     C                   ENDSR
027100060926     c**********************************************************************
027200060927     c* Ricerche sui campi nella window di selezione
027300060926     c**********************************************************************
027400060927     C     Search        BEGSR
027500060927      *
027600060927      * ricerche sui campi:
027700060927      *      STATUS
027800060927     c                   select
027900060927     c* riporta il valore selezionato
028000060927     C                   when      rtncsrfld = 'VSSTS'
028100060927     c                   movel     kpjbu         savkpjbu
028200060927     c                   clear                   kpjbu
028300060927     c                   call      'FIEU56R'
028400060927     c                   parm                    kpjba
028500060927     c                   movel     kpjbu         VSSTS
028600060927     c                   movel     savkpjbu      kpjbu
028700060927      *
028800060927     c                   endsl
028900060927
029000060927     C                   ENDSR
029100060927     c**********************************************************************
029200060927     c* Controlli video R01.
029300060927     c**********************************************************************
029400060927     C     ChkWinSel     BEGSR
029500060927      *
029600080611      * Data ORM
029700060926     C                   EVAL      *IN99 = *OFF
029800080529      *
029900080529     C                   Z-ADD     0             DTORMVS           8 0
030000080529     C     VSDTORM       IFGT      0
030100080529     C                   MOVE      VSDTORM       G02DAT
030200080529     C                   Z-ADD     0             G02INV
030300080529     C                   MOVEL     *BLANKS       G02ERR
030400080529     C                   CALL      'XSRDA8'
030500080529     C                   PARM                    WLBDA8
030600080529     C     G02ERR        IFEQ      '1'
030700080529     C                   SETON                                          0299
030800080529     C                   ELSE
030900080529     C                   Z-ADD     G02INV        DTORMVS
031000080529     C                   MOVEL     G02DAT        VSDTORM
031100080529     C                   END
031200080529     C                   END
031300080611     c* Date trasmissione
031400060926     C                   Z-ADD     0             DTRSVS            8 0
031500060926     C     VSDTRS        IFGT      0
031600060926     C                   MOVE      VSDTRS        G02DAT
031700060926     C                   Z-ADD     0             G02INV
031800060926     C                   MOVEL     *BLANKS       G02ERR
031900060926     C                   CALL      'XSRDA8'
032000060926     C                   PARM                    WLBDA8
032100060926     C     G02ERR        IFEQ      '1'
032200060926     C                   SETON                                          0199
032300060926     C                   ELSE
032400060927     C                   Z-ADD     G02INV        DTRSVS
032500060926     C                   MOVEL     G02DAT        VSDTRS
032600060926     C                   END
032700060926     C                   END
032800080611      *
032900080611     C                   Z-ADD     0             DTRSVSa           8 0
033000080611     C     VSDTRSa       IFGT      0
033100080611     C                   MOVE      VSDTRSa       G02DAT
033200080611     C                   Z-ADD     0             G02INV
033300080611     C                   MOVEL     *BLANKS       G02ERR
033400080611     C                   CALL      'XSRDA8'
033500080611     C                   PARM                    WLBDA8
033600080611     C     G02ERR        IFEQ      '1'
033700080611     C                   SETON                                          0499
033800080611     C                   ELSE
033900080611     C                   Z-ADD     G02INV        DTRSVSa
034000080611     C                   MOVEL     G02DAT        VSDTRSa
034100080611     C                   END
034200080611     C                   ELSE
034300080611     C     VSDTRS        IFGT      0
034400080611     C                   Z-ADD     DTRSVS        DTRSVSa
034500080611     C                   MOVEL     VSDTRS        VSDTRSa
034600080611     C                   END
034700080611     C                   END
034800080611     c* controllo ordinamento fra le 2 date
034900080611     C     DTRSVSa       ifGT      0
035000080611     C     DTRSVS        andGT     DTRSVSa
035100080611     C                   SETON                                          0699
035200080611     C                   END
035300080611     c* Date ricezione
035400060926     C                   Z-ADD     0             DRICVS            8 0
035500060926     C     VSDRIC        IFGT      0
035600060926     C                   MOVE      VSDRIC        G02DAT
035700060926     C                   Z-ADD     0             G02INV
035800060926     C                   MOVEL     *BLANKS       G02ERR
035900060926     C                   CALL      'XSRDA8'
036000060926     C                   PARM                    WLBDA8
036100060926     C     G02ERR        IFEQ      '1'
036200060926     C                   SETON                                          0399
036300060926     C                   ELSE
036400060927     C                   Z-ADD     G02INV        DRICVS
036500060926     C                   MOVEL     G02DAT        VSDRIC
036600060926     C                   END
036700060926     C                   END
036800080611     c*
036900080611     C                   Z-ADD     0             DRICVSa           8 0
037000080611     C     VSDRICa       IFGT      0
037100080611     C                   MOVE      VSDRICa       G02DAT
037200080611     C                   Z-ADD     0             G02INV
037300080611     C                   MOVEL     *BLANKS       G02ERR
037400080611     C                   CALL      'XSRDA8'
037500080611     C                   PARM                    WLBDA8
037600080611     C     G02ERR        IFEQ      '1'
037700080611     C                   SETON                                          0599
037800080611     C                   ELSE
037900080611     C                   Z-ADD     G02INV        DRICVSa
038000080611     C                   MOVEL     G02DAT        VSDRICa
038100080611     C                   END
038200080611     C                   ELSE
038300080611     C     VSDRIC        IFGT      0
038400080611     C                   Z-ADD     DRICVS        DRICVSa
038500080611     C                   MOVEL     VSDRIC        VSDRICa
038600080611     C                   END
038700080611     C                   END
038800080611     c* controllo ordinamento fra le 2 date
038900080611     C     DRICVSa       ifGT      0
039000080611     C     DRICVS        andGT     DRICVSa
039100080618     C                   SETON                                          0899
039200080611     C                   END
039300060926      *
039400060926     C                   ENDSR
039500011227     c**********************************************************************
039600011227     c* gestione sfl
039700011227     c**********************************************************************
039800060926     C     WrkVIDSFLc    BEGSR
039900000627      *
040000011227     C                   EVAL      WrkCarS01 = *ON
040100060926     C                   EVAL      $Video = 'VIDSFLC'
040200011227      *
040300060926     C                   DOU       $Video <> 'VIDSFLC'
040400060926      *
040500060926      * caricamento del SFL
040600060926     C                   EXSR      CarSFL
040700000627      *
040800060926      * se non trovati dati in base alle selezioni
040900060926     C     OK            ifeq      0
041000060926     c                   exfmt     winvuota
041100060926     C                   goto      TORNA
041200060926     c                   end
041300060926      *
041400060926      ** visualizza il sfl se ha più di 1 record
041500060926     c                   if        unSoloREC = 'N'
041600060927     c                   move      knsif         vknsif
041700060927     c                   move      knmus         vknmus
041800060926     C                   WRITE     VIDSFLz
041900060926     C                   EXFMT     VIDSFLc
042000060926     c                   end
042100060926      *
042200011227     C                   SELECT
042300011228     c* fine
042400060928     C                   WHEN      tasto = F3
042500011227     C                   EVAL      $VIDEO=*BLANKS
042600011228     c* guida
042700060928     C                   WHEN      tasto = F12
042800060926     C                   EVAL      $VIDEO='WINSEL'
042900030714
043000011227     C                   OTHER
043100011227
043200011227     C                   EXSR      GestioneSFL
043300011227     C                   ENDSL
043400011228
043500011227     C                   ENDDO
043600060926
043700031020     C     torna         ENDSR
043800060926     c**********************************************************************
043900060926     c* carica sfl
044000060926     c**********************************************************************
044100060926     C     CarSFL        BEGSR
044200060926      **
044300060926     C                   IF        WrkCarS01 = *ON
044400060926     C                   EVAL      snrr1   = 0
044500060926     C                   EVAL      *IN90 = *ON
044600060926     C                   EVAL      *IN91 = *OFF
044700060926     C                   WRITE     VIDSFLc
044800060926     C                   EVAL      *IN90 = *OFF
044900060926      *
045000060926      * Analisi Import/Export
045100060926     c                   if        vSie ='I'
045200060926     c                   eval      impexp = 'I M P O R T'
045300060927     c                   end
045400060927     c                   if        vSie ='E'
045500060926     c                   eval      impexp = 'E X P O R T'
045600060926     c                   end
045700060927     c                   if        vSie = *blank
045800060927     c                   eval      impexp = ' T U T T I '
045900060927     c                   end
046000060926      *ordinamento
046100060926     c                   if        vsordina = 1
046200060926     c     *loval        setll     fidpo01l
046300060926     c                   else
046400060926     c     *loval        setll     fidpo03l
046500060926     c                   end
046600060926      **
046700060926     c                   do        *hival
046800060926      *   lettura
046900060926     c                   if        vsordina = 1
047000060926     c                   read      fidpo01l                               97
047100060926     c                   else
047200060926     c                   read      fidpo03l                               97
047300060926     c                   endif
047400060926      * x EoF
047500060926     c   97              leave
047600060926
047700060926      * ----------------------*
047800060926      * in base alle selezioni:
047900060926      * generali:
048000060926      * ----------------------*
048100060926      * verifico se IMPORT / EXPORT (depot "0837" x convenzione)
048200060926     c                   if        vSie = 'I' and dpoDEP = '0837' or
048300060926     c                             vSie = 'E' and dpoDEP <>'0837'
048400060926     c                   iter
048500060926     c                   endif
048600060926
048700151215      ** è stato deciso di togliere il controllo di appartenenza e che tutti possono
048800151215      ** vedere tutto senza controllo sulle linee di appartenenza come x gli ORM
048900151215      **  Quindi saltiamo questi controlli.
049000151215      **
049100151215     c                   goto      NON_FARE_PIU
049200151215      **
049300080527      * verifico se IMPORT gli ORM gestiti da Verona/Torino in base
049400080527      * alla filiale di Emissione
049500080527     c                   if        vSie = 'I'
049600080527     c     dpoPOE        lookup    LIN                                    25
049700080527     c  n25              iter
049800080527     c                   endif
049900080527
050000080527      * verifico se EXPORT gli ORM gestiti da Verona/Torino in base
050100080527      *  alla filiale di Ritiro presa sull'ORM
050200120216     c                   clear                   altra_HUB         1
050300120216     c                   clear                   HormPOR
050400080527     c                   if        vSie = 'E'
050500080527     c     Korm1_Dpo     chain     fnorm01l
050600080527     c                   if        %Found(fnorm01l)
050700080527     c     ormPOR        lookup    LIN                                    25
050800120216      * se non appartiene alla propria Area di influenza
050900120216      * occorre verificare se c'è stato un dirottamento perchè DPD ha inviato il pacco
051000120216      * fisicamente ad un'altra nostra HUB. (caso frequente fra VERONA e GORIZIA)
051100120216      *   a questo punto occorre verificare sul FNORF00F, x l'ORM in questione, se
051200120216      *   andando cronologicamente indietro esiste una fase 200 che come ORFPOG
051300120216      *   un linea appartenente alla propria area di influenza...allora l'ORM era
051400120216      *   mio e posso vederlo.
051500120216     c**n25*******       iter
051600120216     c                   if        not *in25
051700120216      **
051800120216     C     Korm1_Dpo     setGT     fnORF01l
051900120216     C     Korm1_Dpo     readpe    fnORF01l
052000120216     c                   dow       not %eof
052100120216      *** Dirottamento  (necessario per poter bollettare poichè il collo è arrivato
052200120216      *       fisicamente all'altra HUB)
052300120216     c                   if        orfFAR = 200
052400120216      *  quindi controlla se appartiene all'area di propria appartenenza
052500120216     c     orfPOG        lookup    LIN                                    25
052600120216     c   25              move      'S'           altra_HUB
052700120216     c   25              move      ormPOR        HormPOR
052800120216     c   25              leave
052900120216     C                   end
053000120216     C     Korm1_Dpo     readpe    fnORF01l
053100120216     c                   enddo
053200120216      ** Se altrimenti non c'è comunque riuscito allora lo scarta rileggendo nuovo record
053300120216     c  n25              iter
053400120216     c                   end
053500080527     c                   endif
053600080527     c                   endif
053700151215      **
053800151215     c     NON_FARE_PIU  tag
053900151215      **
054000080527
054100060926      * verifico se richiesti gli Scartati
054200060926     c                   if        vSsca = 'N' and dpoCODER <> *blank or
054300060926     c                             vSsca = 'S' and dpoCODER = *blank
054400060926     c                   iter
054500060926     c                   endif
054600060926
054700060926      * verifico se richiesti i Visionati
054800060926     c                   if        vSvis = 'N' and dpoann <> *blank or
054900060926     c                             vSvis = 'S' and dpoann = *blank
055000060926     c                   iter
055100060926     c                   endif
055200060926
055300060926      * verifico se richiesti i MAncati Ritiri
055400060926     c                   if        vSmar = 'N' and dpoCDMR <> *blank or
055500060926     c                             vSmar = 'S' and dpoCDMR = *blank
055600060926     c                   iter
055700060926     c                   endif
055800060926      * ----------------------*
055900060926      * identificative 1 record:
056000060926      * ----------------------*
056100060926      * verifico se richiesto un parcel specifico
056200060926     c                   if        VSNRP <> *blank and
056300060926     c                             VSNRP <> dpoNRP
056400060926     c                   iter
056500060926     c                   endif
056600060926
056700060926      * verifico se richiesto un depot  specifico
056800060926     c                   if        VSDEP <> *blank and
056900060926     c                             VSDEP <> dpoDEP
057000060926     c                   iter
057100060926     c                   endif
057200060926
057300060926      * verifico se richiesto un ordine specifico
057400060926     c                   if        VSNOR <> *zero  and
057500060926     c                             VSNOR <> dpoORD
057600060926     c                   iter
057700060926     c                   endif
057800060926
057900060926      * verifico se richiesto un P.O.   specifico
058000060926     c                   if        VSPOE <> *zero and
058100060926     c                             VSPOE <> dpoPOE
058200060926     c                   iter
058300060926     c                   endif
058400060926
058500060926      * verifico se richiesto un NR.SERIE  specifico
058600060926     c                   if        VSNRS <> *zero  and
058700060926     c                             VSNRS <> dpoNRS
058800060926     c                   iter
058900060926     c                   endif
059000060926
059100060926      * verifico se richiesto un NR.ORDINE specifico
059200060926     c                   if        VSNRORD <> *zero  and
059300060926     c                             VSNRORD <> dpoNOR
059400060926     c                   iter
059500060926     c                   endif
059600060926
059700060926      * verifico se richiesto un NR.VIAGGIO specifico
059800060926     c                   if        VSNRVIA <> *zero  and
059900060926     c                             VSNRVIA <> dpoNRV
060000060926     c                   iter
060100060926     c                   endif
060200080529
060300080529      * verifico se richiesto una data  ORM specifica
060400080529     c                   if        DTORMVS   <> *zero  and
060500080529     c                             DTORMVS   <> dpodtORM
060600080529     c                   iter
060700080529     c                   endif
060800060926
060900080611      * verifico se richiesto un range date specifico
061000060926     c                   if        DTRSVS   <> *zero  and
061100080611     c                             (dpodtr < DTRSVS  or dpodtr > DTRSVSa)
061200060926     c                   iter
061300060926     c                   endif
061400060926
061500080611      * verifico se richiesto un range date specifico
061600060926     c                   if        DRICVS   <> *zero  and
061700080611     c                             (dpoDTRIC < DRICVS  or dpoDTRIC > DRICVSa)
061800060926     c                   iter
061900060926     c                   endif
062000060926
062100060926      * verifico se richiesto uno STATUS    specifico
062200080529     c                   move      dpoSTS        Status_1          1 0
062300060926     c                   if        VSSTS   <> *zero  and
062400080529     c                             VSSTS   <> Status_1
062500060926     c                   iter
062600060926     c                   endif
062700060926
062800060926      * ----------------------*
062900060926      * Carica Dati
063000120216      *
063100120216      *  se l'ORM è passato sotto un'altra HUB (x segnalarlo a video)
063200120216     c                   eval      Linea_di_altra_HUB = *blank
063300120216      *
063400120216      **   imposta la LINEA dell'altra HUB ma solo se ha trovato un solo record.
063500120216      *    Se ci sono più righe per la selezione NON DEVE mostrare nulla.
063600120216     c                   if          altra_HUB = 'S' and snrr1 = 0
063700120216     c                   eval      Linea_di_altra_HUB = %editc(ORMPOR:'X')
063800120216     c                   end
063900120216      **
064000060926     c                   move      *blanks       v1csce
064100060926     c                   move      dpoDEP        vdpoDEP
064200060926     c                   z-add     dpoORD        vdpoORD
064300060926     c                   z-add     dpoPOE        vdpoPOE
064400060926     c                   z-add     dpoNRS        vdpoNRS
064500060926     c                   z-add     dpoNOR        vdpoNOR
064600060926     c                   z-add     dpoNRV        vdpoNRV
064700060926     c                   movel     dpoNRP        vdpoNRP
064800061120     c                   movel     dpoMANC       vdpoMANC
064900060926     c                   move      dpoSTS        vdpoSTS1
065000060927      * decodifica stato
065100060927     c                   clear                   vdpostsD
065200080611     c                   clear                   ManRit
065300060927     c                   movel(p)  vdpoSTS1      kke1
065400060927     C                   move      'SOD'         ktip
065500060927     c     ktab          chain     tntbe01l
065600060927     c                   if        %found(tntbe01l)
065700060927     c                   movel     tbeuni        vdpostsD
065800060927     c                   endif
065900060926     c                   z-add     dpoNTR        vdpoNTR
066000080611      *
066100060926     c                   move      dpoCDMR       vdpoCDMR
066200080611      *
066300080611      * decodifica mancato ritiro
066400080611     c                   setoff                                       26
066500080611     c                   move      dpoCDMR       vdpoCMR
066600080611     c                   clear                   vdpoDMR
066700080611     c                   if        vdpoCMR  <> *blank
066800080611     c                   movel     'man.Rit'     ManRit
066900080611     C                   move      'MRD'         ktip
067000080611     c     ktip          setll     tntbe01l
067100080611     c     ktip          reade     tntbe01l
067200080611     c                   dow       not %Eof(tntbe01l)
067300080611     c                   seton                                        26
067400080611     c                   movel     tbeuni        dmrd
067500080611     c                   movel(p)  §MRDCAR       §MRDCARalfa       3
067600080611     c                   if        vdpoCMR = §MRDCARalfa
067700080611     c                   movel     §MRDDESD      vdpoDMR
067800080611     c                   setoff                                       26
067900080611     c                   leave
068000080611     c                   end
068100080611     c     ktip          reade     tntbe01l
068200080611     c                   end
068300080611     c                   end
068400060926     c                   move      dpoCODER      vdpoCODER
068500060926     c                   move      dpoANN        vdpoann
068600060926      * decodifica errore
068700060926     c                   clear                   vdpocoderd
068800060926     c                   movel(p)  dpocoder      kke1
068900060927     C                   move      'OER'         ktip
069000060926     c     ktab          chain     tntbe01l
069100060926     c                   if        %found(tntbe01l)
069200060926     c                   movel     tbeuni        vdpocoderd
069300060926     c                   endif
069400060926      * decodifica p.o. emissione
069500060926     c     dpopoe        chain     azorg01l                           95
069600060926     c  n95              movel     orgdes        vdpopoed
069700060926     c   95              clear                   vdpopoed
069800060926      *date
069900060926     c                   if        dpodtric > 0
070000060926     c                   move      dpodtric      dataiso
070100060926     c                   move      dataiso       dataeur
070200060926     c                   move      dataeur       vdpodtric
070300060926     c                   else
070400060926     c                   clear                   vdpodtric
070500060926     c                   endif
070600060926      *date
070700060926     c                   if        dpodtorm > 0
070800060926     c                   move      dpodtorm      dataiso
070900060926     c                   move      dataiso       dataeur
071000060926     c                   move      dataeur       vdpodtorm
071100060926     c                   else
071200060926     c                   clear                   vdpodtorm
071300060926     c                   endif
071400060926
071500080611      *date
071600080611     c                   clear                   UltRic
071700080611     c                   if        DPODTRIC > 0
071800080611     c                   movel     'ult.ric'     UltRic
071900080611     c                   move      DPODTRIC      dataiso
072000080611     c                   move      dataiso       dataeur
072100080611     c                   move      dataeur       vdpoulRX
072200080611     c                   else
072300080611     c                   clear                   vdpoulRX
072400080611     c                   endif
072500080611      *date
072600080611     c                   clear                   UltTrs
072700080611     c                   if        DPODTR   > 0
072800080611     c                   movel     'ult.trs'     UltTrs
072900080611     c                   move      DPODTR        dataiso
073000080611     c                   move      dataiso       dataeur
073100080611     c                   move      dataeur       vdpoulTX
073200080611     c                   else
073300080611     c                   clear                   vdpoulTX
073400080611     c                   endif
073500080611
073600060926      *esco per raggiunta massima capacità sfl
073700060926     c                   if        snrr1 > 9990
073800060926     c                   leave
073900060926     c                   endif
074000061002      **
074100061002      * evidenzia l'errore da Vistare
074200061002     c                   setoff                                       24
074300061002     c                   if        VDPOCODER <> *blank and VDPOANN = *blank
074400061002     c                   seton                                        24
074500061002     c                   endif
074600061002
074700061023     c                   if        vsordina = 1
074800061023     c                   z-add     nrrecDPO1     VrecDPO
074900061023     c                   else
075000061023     c                   z-add     nrrecDPO3     VrecDPO
075100061023     c                   end
075200060926     C                   EVAL      snrr1 = snrr1 + 1
075300060926     C                   WRITE     VIDSFL
075400060926     C                   ENDDO
075500060926
075600060926      *-------------------
075700060926      *** quanti records sono stati caricati:
075800060926      * se solo uno si può passare automaticamente alla finestra finale con
075900060926      * tutti i dati.
076000060926     c                   move      'N'           unsolorec         1
076100060926      * se trovato un solo record dalla selezione imposto il flag
076200060926     c                   if        snrr1 = 1
076300060926     c                   move      'S'           unsolorec
076400060926     c                   end
076500060926      *
076600060926     C                   IF        snrr1  > 0
076700060926     C                   EVAL      *IN91 = *ON
076800060926     C                   MOVE      1             OK                1 0
076900060926     C                   ELSE
077000060926     C                   MOVE      0             OK                1 0
077100060926     C                   ENDIF
077200060926
077300060926     C                   EVAL      nrr1  = 1
077400060926     C                   ENDIF
077500060926
077600060926     C                   EVAL      WrkCarS01 = *OFF
077700060926     C                   EVAL      snrr1  = 1
077800060926     C                   EVAL      csrrrn = 1
077900120216      **
078000060926
078100060926     C                   ENDSR
078200060926     c**********************************************************************
078300060926     c* gestione sfl
078400060926     c**********************************************************************
078500060926     C     GestioneSFL   BEGSR
078600060926      *
078700060926     C                   EVAL      WrkEofS01 = *OFF
078800060926     c*
078900060926      * Elaborazione opzioni.
079000060926     c                   do        *hival        X                 4 0
079100060926     c     X             CHAIN     VIDSFL                             50
079200060926     C   50              LEAVE
079300060926      ***
079400060926     c                   if        unSolorec = 'S'
079500061003     c                   move      '5'           v1csce
079600060926     c                   end
079700060926      ***
079800060927     c                   IF        v1csce <> *blanks
079900060926      ***
080000080529      ***  Visualizza la finestra con il dettaglio
080100080529     c                   IF        v1csce = '5'
080200080529      ***
080300060926      *** Emissione Dettaglio della riga del SFL selezionata
080400060926      ***  mediante una window
080500060926     c                   exsr      WinDati
080600060928     c                   if        *in21 = *on
080700060928     c                   clear                   $msg
080800060928     c                   eval      *in21 = *off
080900060928     c                   end
081000060926
081100080529     c                   end
081200080529
081300080529      ***  Rende il record VISIONATO o se già Visionato lo riATTIVA
081400080529     c                   IF        v1csce = 'V'
081500080529     c                   z-add     vRECdpo       vrelREC
081600080529      *
081700080529     c     vrelREC       chain     fidpo00f
081800080529     c                   if        %Found(fidpo00f)
081900080529     c                   if        dpoANN = 'V'
082000080529     c                   clear                   dpoANN
082100080529     c                   elseif    dpoANN = ' '
082200080529     c                   move      'V'           dpoANN
082300080529     c                   end
082400080529     c                   update    fidpo000
082500080529     C                   eval      WrkCarS01 = *ON
082600080529     c                   end
082700080529      *
082800080529     c                   end
082900080529
083000060926     c                   move      *blank        v1csce
083100060926     c                   update    VIDSFL
083200060926
083300060926     C                   ENDIF
083400060926     C                   enddo
083500060926      *
083600060926     C                   ENDSR
083700030605     c**********************************************************************
083800060926      *  Emissione della Window di dettaglio
083900060926     c**********************************************************************
084000060926     C     WinDati       BEGSR
084100060927     c                   move      knsif         vknsif
084200060927     c                   move      knmus         vknmus
084300061023     c                   z-add     vRECdpo       vrelREC
084400120216      * riporta la LINEA memorizzata in Hidden sul SFL solo se NON è della
084500120216      *   propria area di appartenenza
084600120216     c                   eval      Linea_di_altra_HUB = *blank
084700120216     c                   if        HormPOR <> *blank
084800120216     c                   eval      Linea_di_altra_HUB = HormPOR
084900120216     c                   end
085000060926      *
085100051010     c     kdpo1         chain     fidpo01l                           98
085200030610     c                   if        not *in98
085300060810     c                   move      dpoDEP        vdpoDEP
085400051010     c                   z-add     dpoORD        vdpoORD
085500051010     c                   z-add     dpoPOE        vdpoPOE
085600051010     c                   z-add     dpoNRS        vdpoNRS
085700051010     c                   z-add     dpoNOR        vdpoNOR
085800051010     c                   z-add     dpoNRV        vdpoNRV
085900060810     c                   movel     dpoNRP        vdpoNRP
086000060915     c                   move      dpoSTS        vdpoSTS1
086100060927      * decodifica stato
086200060927     c                   clear                   vdpostsD
086300060927     c                   movel(p)  vdpoSTS1      kke1
086400060927     C                   move      'SOD'         ktip
086500060927     c     ktab          chain     tntbe01l
086600060927     c                   if        %found(tntbe01l)
086700060927     c                   movel     tbeuni        vdpostsD
086800060927     c                   endif
086900051010     c                   z-add     dpoNTR        vdpoNTR
087000051010     c                   move      dpoCDMR       vdpoCDMR
087100061002      * decodifica mancato ritiro
087200061204     c                   clear                   vdpoCDMD
087300061002     c                   if        vdpoCDMR <> *blank
087400061002     C                   move      'MRD'         ktip
087500061002     c     ktip          setll     tntbe01l
087600061002     c     ktip          reade     tntbe01l
087700061002     c                   dow       not %Eof(tntbe01l)
087800061002     c                   movel     tbeuni        dmrd
087900061002     c                   movel(p)  §MRDCAR       §MRDCARalfa       3
088000061002     c                   if        vdpoCDMR = §MRDCARalfa
088100061002     c                   movel     §MRDDESD      vdpoCDMD
088200061002     c                   leave
088300061002     c                   end
088400061002     c     ktip          reade     tntbe01l
088500061002     c                   end
088600061002     c                   end
088700060926     c                   move      dpoANN        vdpoann
088800051010     c                   move      dpoCODER      vdpoCODER
088900051010      * decodifica errore
089000060928     c                   clear                   vdpocoded
089100051010     c                   movel(p)  dpocoder      kke1
089200060927     C                   move      'OER'         ktip
089300051010     c     ktab          chain     tntbe01l
089400051010     c                   if        %found(tntbe01l)
089500060928     c                   movel     tbeuni        vdpocoded
089600051010     c                   endif
089700051010      * decodifica p.o. emissione
089800051010     c     dpopoe        chain     azorg01l                           95
089900051010     c  n95              movel     orgdes        vdpopoed
090000051010     c   95              clear                   vdpopoed
090100051011      *date
090200051011     c                   if        dpodtric > 0
090300051011     c                   move      dpodtric      dataiso
090400051011     c                   move      dataiso       dataeur
090500051011     c                   move      dataeur       vdpodtric
090600051011     c                   else
090700051011     c                   clear                   vdpodtric
090800051011     c                   endif
090900051011     c                   if        dpodtorm > 0
091000051011     c                   move      dpodtorm      dataiso
091100051011     c                   move      dataiso       dataeur
091200051011     c                   move      dataeur       vdpodtorm
091300051011     c                   else
091400051011     c                   clear                   vdpodtorm
091500051011     c                   endif
091600051011     c                   if        dpodtr > 0
091700051011     c                   move      dpodtr        dataiso
091800051011     c                   move      dataiso       dataeur
091900051011     c                   move      dataeur       vdpodtr
092000051011     c                   else
092100051011     c                   clear                   vdpodtr
092200051011     c                   endif
092300060927      ***
092400060927     c                   clear                   visto
092500060927     c                   if        dpoANN = 'V'
092600070104     c                   eval      visto = 'VISIONATO'
092700060927     c                   endif
092800080506      *
092900080509     c                   clear                   vMSG01
093000080506      *  Controlla esistenza di + colli su stesso ORM
093100080509     c                   if        dpoPOE > 0
093200080509     c                   eval      chiaveORM = %Editc(dpoPOE:'X') +
093300080509     c                                         %Editc(dpoNRS:'X') +
093400080509     c                                         %Editc(dpoNOR:'X') +
093500080509     c                                         %Editc(dpoNRV:'X')
093600080509     c/exec sql
093700080509     C+ SELECT COUNT(*) INTO :Pacchi_x_ORM FROM Fiar400F WHERE ar4trc =
093800080509     C+ 'M' AND substr(ar4NOT, 1, 14) = :chiaveORM
093900080509     c/end-exec
094000080509     c                   if        Pacchi_x_ORM > 1
094100080509     c                   eval      vMSG01  = 'NB: ci sono + Parcels su stess-
094200120305     c                             o ORM BRT  '
094300080509     c                   end
094400080509     c                   end
094500080506      *
094600060926      ***
094700060926     c                   exsr      GesWinDati
094800060926      ***
094900030605     c                   endif
095000060926
095100030605     C                   ENDSR
095200030605     c**********************************************************************
095300060926     c**********************************************************************
095400060926     C     GesWinDati    BEGSR
095500060926      *
095600030605     c                   do        *hival
095700060927      *
095800060927     c                   setoff                                       55
095900060927     c                   if        vdpoCODER <> *blank
096000060927     c                   seton                                        55
096100060927     c                   end
096200061204
096300061120     c                   clear                   VMANCDPO
096400061204
096500061120     c                   if        vdpoCODER =  '82'
096600061120     c                   eval      VMANCDPO = 'I Parcels in + da ritirare ' +
096700061120     c                                    'sono : ' + %char(VdpoManc)
096800061120     c                   end
096900060927      *
097000060928     c                   eval      *in21 = *off
097100060928     c                   eval      *in22 = *off
097200061204      *
097300060927      *  se il pgm è richiamato oppure non ha errori
097400060928     c                   if        chiamato <> *blank
097500060928     c                   eval      *in21 = *on
097600060927     c                   end
097700060928     c                   if        vdpoCODER = *blank
097800060928     c                   eval      *in22 = *on
097900060928     c                   end
098000080529      *
098100080529      *  Abilita il tasto F6 a eliminare il Visionato
098200080529     c                   setoff                                       23
098300080529     c                   if        visto = 'VISIONATO'
098400080529     c                   setON                                        23
098500080529     c                   end
098600120216      * Se c'è stato un dirottamento e la bolla è arrivata e bollettata con
098700120216      * un'altra linea di partenza, Viene emesso un messaggio di avviso
098800120216     c                   clear                   VLNPALT
098900120216     c                   if        Linea_di_altra_HUB <> *blank
099000120216     c                   eval      VLNPALT = '** Linea Partenza Bolla ' +
099100120216     c                             Linea_di_altra_HUB + ' **'
099200120216     c                   end
099300120216      *
099400060926     c                   exfmt     window1
099500080505      *
099600061003      * riposizionamento sul sfl
099700061003     c                   if        snrr1> 0
099800061003     C                   EVAL      nrr1  = snrr1
099900061003     c                   end
100000060928      *   int.Bolle
100100060928     c                   if        tasto = F7
100200061003     C*
100300061003     C                   MOVE      'N'           INT_ORD           1
100400061003     C* se è impostato l'ORM visualizza direttamente la sua bolla se c'è
100500061003     c                   if        vdpoPOE > 0 and vdpoNOR > 0
100600061003     c                   eval      ar4trc ='M'
100700061003     c                   move      vdpoPOE       vdpoPOEa          3
100800061003     c                   move      vdpoNRS       vdpoNRSa          2
100900061003     c                   move      vdpoNOR       vdpoNORa          7
101000061003     c                   move      vdpoNRV       vdpoNRVa          2
101100070612     c******             eval      ar4n30 = %trim(vdpopoeA) + %trim(vdponrsA) +
101200070612     c******                                %trim(vdponorA) + %trim(vdponrvA)
101300070612     c                   move      vdpopoeA      §B4POE
101400070612     c                   move      vdponrsA      §B4NSR
101500070612     c                   move      vdponorA      §B4NOR
101600070612     c                   move      vdponrvA      §B4NRV
101700080527     C                   If        vsIE = 'I'
101800080527     c                   eval      §B4CLG = 'S'
101900080527     C                   Else
102000080527     c                   eval      §B4CLG = *blank
102100080527     C                   End
102200070612     c                   eval      ar4n30 = dsbl4m
102300070613     c                   move      'N'           Sped_xOrm         1
102400070613     c     kar43         chain     fiar403l
102500070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
102600070613     c                   dow       not %Eof(fiar403l)
102700070613     c                   movel     ar4N30        dsbl4M
102800080527     C                   If        vsIE = 'E' or
102900080527     c                             (§B4CLG = 'S' and vsIE = 'I')
103000070613     c                   move      'S'           Sped_xOrm         1
103100070613     c                   leave
103200070613     c                   end
103300070613     c     kar43         reade     fiar403l
103400070613     c                   enddo
103500070613      ***
103600070613      *? se trovato la spedizione abbinata procede...  ?
103700070613     c                   if        Sped_xOrm = 'S'
103800070613     c********           if        %Found(fiar403l)
103900070613      *
104000061003     C     Kblp1         chain     fnblp01l
104100061003     c                   if        %Found(fnblp01l)
104200061003     C                   MOVE      'S'           INT_ORD
104300061003     c                   movel     kpjbu         savkpjbu
104400170124     C***                clear                   PARAM0
104500170124     C***                move      ar4AAS        PA0AAS
104600170124     C***                move      ar4LNP        PA0LNP
104700170124     C***                move      ar4NRS        PA0NRS
104800170124     C****               move      ar4NSP        PA0NSP
104900170124     C****               MOVEL     RAGUT         PA0RSU
105000170124     C****               MOVE      SIMFEL        PA0SIM
105100170124     C****               movel(p)  PARAM0        KPJBU
105200170124     C***                call      'FNLS05R'                            99
105300170124     C****               parm                    KPJBA
105301170124     C                   CLEAR                   paramu6ds1
105302170124     C                   Z-ADD     ar4AAS        pa1AAS_
105303170124     C                   Z-ADD     ar4LNP        Pa1LNP_
105304170124     C                   Z-ADD     ar4NRS        Pa1NRS_
105305170124     C                   Z-ADD     ar4NSP        Pa1NSP_
105306170124     C                   eval      PA1tla_='L'
105307170124     C                   MOVEL     paramu6ds1    KPJBU
105308170124     C                   CALL      'FNLRU6R1'
105309170124     C                   PARM                    KPJBA
105400061003     c                   movel     savkpjbu      kpjbu
105500061003     c                   end
105600061003     c                   end
105700061003     c                   end
105800061003      * altrimenti genericamente:
105900061003     C                   IF        INT_ORD   = 'N'
106000060929     c                   movel     kpjbu         savkpjbu
106100060929     c                   clear                   kpjbu
106200170124     C***                CALL      'FNLS04R'
106300170124     c***                parm                    kpjba
106301170124     C                   CLEAR                   PARAmu6ds
106303170124     C                   MOVEL     '4'           Pa1FLG
106304170124     C                   MOVEL     PARAmu6ds     KPJBU
106310170124     C                   CALL      'FNLRU6R'
106311170124     C                   PARM                    KPJBA
106400060929     c                   movel     savkpjbu      kpjbu
106500060928     c                   end
106600061003     c                   end
106700060928      *
106800060928      *   int.ORM se non è chiamato
106900060928     c                   if        tasto = F8 and *in21 = *off
107000061003     C*
107100061003     C                   MOVE      'N'           INT_ORM           1
107200061204     C* x import
107300061204     c                   if        vsIE ='I'
107400061204     C*
107500061005     C* se trova l'ORM visualizza direttamente la sua interrogazione
107600061005      * prepara la chiave per agganciare l'orm
107700061005     c                   clear                   alfa10
107800061005     c                   movel     vdpoDEP       alfa10
107900061005     c                   move      vdpoORD       alfa10
108000061005     c                   eval      alfa15 = 'DPD'+ %Trim(alfa10)
108100061005      * aggancia l'orm Bartolini
108200061005     c     alfa15        chain     fnorm07l
108300061204     C* x export
108400061204     c                   else
108500061204     c     Korm1         chain     fnorm01l
108600061204     c                   end
108700061204     C* se agganciato l'ORM
108800061005      **?           ---------------
108900080904     c                   if        (%Found(fnORM07l) and vsIE ='I') or
109000080904     c                             (%Found(fnORM01l) and vsIE ='E')
109100061005      **?           ---------------
109200061003     C                   MOVE      'S'           INT_ORM           1
109300061003     c                   movel     kpjbu         savkpjbu
109400061003     C                   clear                   PARM01
109500061005     C                   move      ormpoe        ppoe
109600061005     C                   move      ormnor        pnor
109700061005     C                   move      ormnsr        pnsr
109800061005     C                   move      ormnrv        pnrv
109900061024     C                   move      'D'           psce
110000080505     C                   move      simPOU        pfgs
110100061003     C                   movel(p)  PARM01        KPJBU
110200061003     C                   call      'FIOR07R'                            99
110300061003     C                   parm                    KPJBA
110400061003     c                   movel     savkpjbu      kpjbu
110500061003     c                   end
110600061003      * altrimenti genericamente:
110700061003     C                   IF        INT_ORM   = 'N'
110800060928     c                   movel     kpjbu         savkpjbu
110900061024     c                   movel(p)  'ID'          kpjbu
111000150923     C                   CALL      'FIOR010R'
111100060928     C                   PARM                    KPJBA
111200060928     c                   movel     savkpjbu      kpjbu
111300061003     c                   end
111400061003
111500061003     c                   endif
111600060928      *
111700060928     c                   if        tasto = F3
111800060928     C                   EVAL      $Video = *BLANKS
111900060928     c                   end
112000060928      *
112100060928     c                   if        tasto = F3 or tasto = F12
112200060928     c                   except    libera
112300060928     c                   leave
112400060928     c                   end
112500060926      *
112600060928     c                   if        tasto = F6
112700060926      *  Visiona solo eventuali errori
112800080529      *     No permette di visionare
112900080529     c******             if        dpoann = *blank and dpoCODER <> *blank
113000061023     c     vrelREC       chain     fidpo00f
113100080529     c                   if        dpoAnn = ' '
113200060926     c                   move      'V'           dpoann
113300080529     c                   else
113400080529     c                   move      ' '           dpoann
113500080529     c                   end
113600060926     c                   update    fidpo000
113700080529     c**********         end
113800060926     c                   eval      wrkcars01 = *on
113900030702     c                   leave
114000030702     c                   endif
114100030605
114200030605     c                   enddo
114300060926
114400030605     C                   ENDSR
114500011227     c**********************************************************************
114600060928      *  Routine iniziale
114700011227     c**********************************************************************
114800060926     C     *INZSR        BEGSR
114900060926      *
115000061206      *  sistema di test
115100061206     c                   clear                   sistema_test      1
115200061206     c                   clear                   sede_test         1
115300061206     c                   setoff                                       07
115400061206     c                   if        %subst(Knsif:7:1) = 'P'
115500061206     c                   move      'S'           sistema_test      1
115600061206     c                   end
115700061206     c                   if        sistema_test = *blank and
115800061206     c                             knsiF='GAITRA201'
115900061206     c                   move      'S'           sede_test
116000061206     c                   seton                                        07
116100061206     c                   end
116200061206     c                   if        sistema_test = 'S' and
116300061206     c                             knsiF='GAITRAPSP'
116400061206     c                   move      'S'           sede_test
116500061206     c                   seton                                        07
116600061206     c                   end
116700080527     c                   movel     kpjbu         savkpjbu
116800061206      *
116900060926     C                   Z-ADD     1             CODUT
117000060926     C                   CALL      'X§PARUT'
117100060926     C                   PARM                    UTEDSE
117200060926     C                   MOVEL     REC80         CNCR80
117300080527      *
117400080527      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
117500080527     C                   CLEAR                   TRUL06DS
117600080527     C                   MOVE      '£1'          D06COD
117700080527     C                   MOVEL     SIMFEL        D06KEY
117800080527     C                   MOVEL     'L'           D06TLA
117900080527     C                   MOVEL     TRUL06DS      KPJBU
118000080527     C                   CALL      'TRUL06R'
118100080527     C                   PARM                    KPJBA
118200080527     c                   MOVEL     KPJBU         TRUL06DS
118300080527     c                   movel     savkpjbu      kpjbu
118400080527
118500060926     C                   MOVEL     Ragut         rsut             20
118600060926      *reperimento data
118700060926     C                   TIME                    W0120            14 0
118800060926     C                   MOVE      W0120         WDAT              8 0
118900060926     C*
119000060926     C                   Z-ADD     WDAT          G02DAT
119100060926     C                   MOVEL     *BLANK        G02ERR
119200060926     C                   CALL      'XSRDA8'
119300060926     C                   PARM                    WLBDA8
119400060926     C* UDATE A 8 IN AAAA/MM/GG
119500060926     C                   Z-ADD     G02INV        WUDA              8 0
119600060926      *
119700060926     C     Kdpo1         KLIST
119800060926     C                   KFLD                    vDPOPOE
119900060926     C                   KFLD                    vDPONRS
120000060926     C                   KFLD                    vDPONOR
120100060926     C                   KFLD                    vDPONRV
120200060926     C                   KFLD                    vDPODEP
120300060926     C                   KFLD                    vDPOORD
120400060926      *
120500060926     C     Ktab          KLIST
120600060926     C                   KFLD                    ktip              3
120700060926     C                   KFLD                    kke1             15
120800061003      *
120900061003     C     Kar43         KLIST
121000061003     C                   KFLD                    ar4trc
121100061003     C                   KFLD                    ar4n30
121200061003      *
121300061003     C     Kblp1         KLIST
121400061003     C                   KFLD                    ar4AAS
121500061003     C                   KFLD                    ar4LNP
121600061003     C                   KFLD                    ar4NRS
121700061003     C                   KFLD                    ar4NSP
121800061204      *
121900061204     C     Korm1         KLIST
122000061204     C                   KFLD                    vDPOPOE
122100061204     C                   KFLD                    vDPONRS
122200061204     C                   KFLD                    vDPONOR
122300061204     C                   KFLD                    vDPONRV
122400060926
122500080527     C     Korm1_Dpo     KLIST
122600080527     C                   KFLD                    DPOPOE
122700080527     C                   KFLD                    DPONRS
122800080527     C                   KFLD                    DPONOR
122900080527     C                   KFLD                    DPONRV
123000080527
123100011227     C                   ENDSR
123200060928     ***************************************************************************
123300051011     OFidpo000  E            libera
