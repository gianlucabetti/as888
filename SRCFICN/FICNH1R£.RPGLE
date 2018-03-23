000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200150605     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000300150914     Fycostp4d  CF   E             WORKSTN
000400150727     FTntbe01l  IF   E           K DISK
000500150914     Fazorg01l  IF   E           K DISK
000501150928     FTabel00f  IF   E           K DISK
000502150928     Fkpprf01l  IF   E           K DISK
000600150529     D DTAcor          S               D   INZ
000700150914     D viddt1g         S               D   INZ
000800150914     D viddt2g         S               D   INZ
000801151006     D viddt3g         S               D   INZ
000802151006     D viddt4g         S               D   INZ
000900011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001000000000     D KPJBA         E DS
001001150917     d  param          ds
001002150917     d   parrich                      9s 0
001003150917     d   paropz                       1
001100150807     d dtce          e ds
001101150928     d ds05          e ds
001200150914     D ycostp4i      E DS
001300150807     D TIBS02DS      E DS
001400150529     D TIBS34DS      E DS                                                       *Profili utente
001500150529     D DDATIUTE      E DS                                                       *Dati utente
001600150529     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001700150811     D dute01        E DS                                                       *Utenti
001800150811     d dLat          e ds
001900150914     D TRUL31DS      E DS
002000150914     D TRUL31DS2     E DS
002100150811     d wabi            s                   like(UteAut)
002200150914     d* p.o. abilitati
002300150914     D $pogn           s              3  0 dim(250)
002400150914     D $pog            s              3    dim(250)
002500150914     D $DIG            s              1    dim(20)
002600150914     D $ARG            s              3    dim(50)
002601150928     D §cod            s              3
002602150928     D §tip            s              1
002603150928     D §des            s             25
002604150928     D §pra            s              3
002700150914      *------------------------------------------------
002800150914     D KSC             S              7  0 dim(30)
002900150914     D KSA             S              4    dim(30)
003000150914     D*------------------
003100150914     D DSXC          E DS                  extname(XCLIRDS)
003200150914     D*------------------
003300150914     D                 DS
003400150914     D DSKSN                   1      4p 0
003500150914     D DSKSA                   1      4
003600150914      *
003700150914     d Tibs69DS      E DS                  INZ
003800150914     d DS_cnaco      E DS                  extname(CNACO00F)
003900150914     d DS_cnind      E DS                  extname(CNIND00F)
004000150914     d DS_cnclp      E DS                  extname(CNCLP00F)
004100150914     d DS_fncls      E DS                  extname(FNCLS00F)
004200150914      *
004300150914     D*------------------
004400150914     D WLBDA8          DS                  INZ
004500150914     D  G08DAT                 1      8  0
004600150914     D  G08INV                 9     16  0
004700150914     D  G08ERR                17     17
004800150914     D  G08TGI                18     22  0
004900150928     C*_______________________________________________________________
004902150928     c                   move      'N'           vidann
005000011130     C  nlr              DO        *HIVAL
005100011130     c                   EXSR      CLR00
005200150529     C                   EXFMT     ycostp0
005300011130     C* FINE LAVORO
005400011130     C   KC              LEAVE
005500011130     C* RICERCA
005501151006     c                   if        *inkd
005600151006     C                   EXSR      RIC01
005601151006     c                   iter
005602151006     c                   endif
005603151006      * inserimento
005604151006     c                   if        *inkj
005605151006     c                   clear                   param
005606151006     c                   movel     param         kpjbu
005607151006     c                   call      'YCOSTP5R'
005608151006     c                   parm                    kpjba
005609151007     c                   iter
005610151006     c                   endif
005700000000     C* CONTROLLI
005800151006     C                   EXSR      CTRL01
005801150924     c   kd
005900150924     COR 99              ITER
006400150914     c                   clear                   ycostp4i
006500150914     c                   move      vtipo         STPTIPO
006600150914     c                   move      vidDt1g       STPDAT1
006700150914     c                   move      viddt2g       STPDAT2
006701151006     c                   move      vidDt3g       STPDAT3
006702151006     c                   move      viddt4g       STPDAT4
006800150914     c                   z-add     vidfi1        STPFILA
006900150914     c                   z-add     vidfi2        STPFILD
006901150928     c                   z-add     vidar1        STPareA
006902150928     c                   z-add     vidar2        STPareD
007000150914     c                   z-add     vidFAS        STPFAS
007100150914     c                   move      vidks2        STPKSC
007200150914     c                   move      vidTSR        STPTSR
007201150928     c                   move      vidann        STPatb
007300150914     c                   move      vidute        STPUTEN
007400150914     c                   movel     ycostp4i      kpjbu
007500150914     c                   call      'YCOSTP3R'
007600150605     c                   parm                    kpjba
007700150914     c                   movel     kpjbu         ycostp4i
007800011130     C                   ENDDO
007900011130     c*
008000000000     C                   SETON                                        LR
008100011130     c**********************************************************************
008200011130     c* pulisce 1° formato
008300011130     c**********************************************************************
008400011130     C     clr00         BEGSR
008500011130     C                   Z-ADD     0             §RIGA
008600011130     C                   Z-ADD     0             §COLON
008700011130     C                   CLEAR                   §FLD
008800011130     C                   ENDSR
008900000000     C******************************************************
009000000000     C** CONTROLLI FORMATO ENTRATA
009100011130     C******************************************************
009200011130     C     CTRL01        BEGSR
009201151006     C                   SETOFF                                       28
009300150914     C                   SETOFF                                       998081
009400150914     C                   SETOFF                                       828384
009500151006     C                   SETOFF                                       858687
009501151006     C                   SETOFF                                       888990
009600011130     C                   DO
009700150727      * CONTROLLO il tipo
009800150916     c                   if        vtipo  <> *blank
009801150924     c                   clear                   tibs02ds
009802150924     c                   eval      t02mod = 'C'
009803150924     c                   eval      t02ke1 = vtipo
009900151103     c                   exsr      srtab
009901150930     c                   if        t02err <> *blank
009902150930     C                   SETON                                        992880
009903150930     c                   eval      $msg = 'Causale non valida in filiale'
009904150930     c                   leavesr
009905150930     c                   endif
010000150916     c                   if        §tceute = 'S'
010100150916     C                   SETON                                        992880
010200150916     c                   eval      $msg = 'Causale non valida in filiale'
010300150916     c                   leavesr
010400150916     c                   endif
010401151103     c                   else
010402151103     c                   clear                   dtipo
010500150727     c                   endif
010600150914      * date
010700151006      * dal PRESTAZIONE
010800150914     c                   clear                   viddt1g
010900150914     c                   if        viddt1 > 0
011000150914     c                   move      viddt1        g08dat
011100150914     c                   move      *blank        g08err
011200150914     C                   CALL      'XSRDA8'
011300150914     C                   PARM                    WLBDA8
011400150914     C     G08ERR        IFEQ      '1'
011500150914     C                   SETON                                        992882
011600150914     c                   eval      $msg = 'Data errata '
011700150914     c                   leavesr
011800150914     C                   endif
011900150914     c                   move      g08inv        viddt1g
011901150929     c                   move      g08dat        viddt1
012000150914     C                   endif
012100150914      * al
012200150914     c                   clear                   viddt2g
012300150914     c                   if        viddt2 > 0
012400150914     c                   move      viddt2        g08dat
012500150914     c                   move      *blank        g08err
012600150914     C                   CALL      'XSRDA8'
012700150914     C                   PARM                    WLBDA8
012800150914     C     G08ERR        IFEQ      '1'
012900150914     C                   SETON                                        992881
013000150914     c                   eval      $msg = 'Data errata '
013100150914     c                   leavesr
013200150914     C                   endif
013300150914     c                   move      g08inv        viddt2g
013301150929     c                   move      g08dat        viddt2
013400150914     C                   endif
013500150914     c                   if        viddt1g <> *loval
013600150914     c                   if        viddt2g = *loval
013700150914     c                   move      viddt1g       viddt2g
013800150914     c                   move      viddt1        viddt2
013900150914     c                   endif
014000150914     c                   if        viddt1g > viddt2g
014100150914     C                   SETON                                        992881
014200150914     c                   eval      $msg  = 'Data dal maggiore di data al '
014300150914     c                   endif
014400150914     c                   endif
014401151006      * dal INSERIMENTO
014402151006     c                   clear                   viddt3g
014403151006     c                   if        viddt3 > 0
014404151006     c                   move      viddt3        g08dat
014405151006     c                   move      *blank        g08err
014406151006     C                   CALL      'XSRDA8'
014407151006     C                   PARM                    WLBDA8
014408151006     C     G08ERR        IFEQ      '1'
014409151006     C                   SETON                                        992889
014410151006     c                   eval      $msg = 'Data errata '
014411151006     c                   leavesr
014412151006     C                   endif
014413151006     c                   move      g08inv        viddt3g
014414151006     c                   move      g08dat        viddt3
014415151006     C                   endif
014416151006      * al
014417151006     c                   clear                   viddt4g
014418151006     c                   if        viddt4 > 0
014419151006     c                   move      viddt4        g08dat
014420151006     c                   move      *blank        g08err
014421151006     C                   CALL      'XSRDA8'
014422151006     C                   PARM                    WLBDA8
014423151006     C     G08ERR        IFEQ      '1'
014424151006     C                   SETON                                        992890
014425151006     c                   eval      $msg = 'Data errata '
014426151006     c                   leavesr
014427151006     C                   endif
014428151006     c                   move      g08inv        viddt4g
014429151006     c                   move      g08dat        viddt4
014430151006     C                   endif
014431151006     c                   if        viddt3g <> *loval
014432151006     c                   if        viddt4g = *loval
014433151006     c                   move      viddt3g       viddt4g
014434151006     c                   move      viddt3        viddt4
014435151006     c                   endif
014436151006     c                   if        viddt3g > viddt4g
014437151006     C                   SETON                                        992890
014438151006     c                   eval      $msg  = 'Data dal maggiore di data al '
014439151006     c                   endif
014440151006     c                   endif
014441151006     c                   if        viddt3 = 0 and  viddt1 = 0
014442151006     C                   SETON                                        992889
014443151006     c                   eval      $msg  = 'Inserimento obbligatorio fra -
014444151006     c                             date prestazione o date inserimento'
014445151006     c                   leavesr
014447151006     c                   endif
014448150928     c* area richiedente
014449150929     c                   clear                   vidar1d
014450150928     c                   if        vidar1 > *zero
014451150928     C                   MOVEL     *blanks       tbluni
014452150928     C                   MOVEL     '05'          COD
014453150928     C                   MOVEL(p)  vidar1        KEY
014454150928     C     KTAB          CHAIN     TABEL00f
014455150928     c                   if        not %found(tabel00f)
014456150928     C                   seton                                        872899
014457150928     C                   eval      $msg = 'Area richiedente inesistente'
014458150928     c                   leavesr
014459150928     C                   Else
014460150928     c                   movel     tbluni        ds05
014461150928     c                   movel     §05des        vidar1d
014462150928     c                   endif
014463150928     c                   endif
014464150928     c                   if        vidar1 > *zero and vidfi1 > 0
014465150928     C                   seton                                        872899
014466150928     C                   eval      $msg = 'Inserire Area o filiale non entrambe'
014467150928     c                   leavesr
014468150928     c                   endif
014469150928     c* area addebito
014470150929     c                   clear                   vidar2d
014471150928     c                   if        vidar2 > *zero
014472150928     C                   MOVEL     *blanks       tbluni
014473150928     C                   MOVEL     '05'          COD
014474150928     C                   MOVEL(p)  vidar2        KEY
014475150928     C     KTAB          CHAIN     TABEL00f
014476150928     c                   if        not %found(tabel00f)
014477150928     C                   seton                                        882899
014478150928     C                   eval      $msg = 'Area addebito inesistente'
014479150928     c                   leavesr
014480150928     C                   Else
014481150928     c                   movel     tbluni        ds05
014482150928     c                   movel     §05des        vidar2d
014483150928     c                   endif
014484150928     c                   endif
014485150928     c                   if        vidar2 > *zero and vidfi2 > 0
014486150928     C                   seton                                        882899
014487150928     C                   eval      $msg = 'Inserire Area o filiale non entrambe'
014488150928     c                   leavesr
014489150928     c                   endif
014500150914     c* filiale richiedente
014501150929     c                   clear                   vidfi1d
014600150914     c                   if        vidfi1 > *zero
014601150929     c     vidfi1        chain     azorg01l
014602150929     c                   movel     orgdes        vidfi1d
014700150914     C     vidfi1        lookup    $pogN                                  79
014800151217     c                   if        not *in79 and vidfi2 = 0
014900150914     C                   seton                                        832899
015000150914     C                   eval      $msg = 'Filiale richiedente non in gestione'
015001150928     c                   leavesr
015100150914     C                   endif
015200150914     c                   endif
015300150914     c* filiale addebito
015301150929     c                   clear                   vidfi2d
015400150929     c                   if        vidfi2 > *zero
015401150929     c     vidfi2        chain     azorg01l
015402150929     c                   movel     orgdes        vidfi2d
015403151217     c                   if        not %found(azorg01l)
015404151217     C                   seton                                        842899
015405151217     C                   eval      $msg = 'Filiale inesistente'
015406151217     C                   endif
015407151217     c                   if        not *in79 and vidfi1 > 0
015408151217     C     vidfi2        lookup    $pogN                                  79
015409151217     c                   if        not *in79
015410151217     C                   seton                                        842899
015411151217     C                   eval      $msg = 'Una delle due filiali deve essere -
015412151217     c                             una filiale in gestione'
015413151217     c                   leavesr
015414151217     C                   endif
015415151217     C                   endif
015416151217      *
016000150929     c                   endif
016100150914     c* codice cliente
016200150914     c                   if        vidks2 <> *blank
016300150914     c                   move      vidks2        chkks2            7
016400150914     c                   exsr      dectib
016500150914     c                   endif
016600150727      *
016608150928     c* codice cliente
016609150928     c                   clear                   viduted
016610150928     c                   if        vidute <> *blank
016611150928     c     vidute        chain     kpprf01l
016612150928     c                   if        not %found(kpprf01l)
016613150928     C                   seton                                        862899
016614150928     C                   eval      $msg = 'Profilo non trovato '
016616150928     c                   leavesr
016617150928     c                   else
016618150928     c                   movel     prftxt        viduted
016619150928     c                   endif
016620150928     c                   endif
016700011130     C                   ENDDO
016800000000     C                   ENDSR
016900011130     C******************************************************
017000011130     C* RICERCA PRIMO VIDEO
017100011130     C******************************************************
017200011130     C     RIC01         BEGSR
017300011130     c                   select
017301150928      * ricerca causale
017400150529     C                   when      §fld = 'VTIPO'
017500150529     C                   exsr      srtipo
017501150928      * ricerca cliente
017600150914     C                   when      §fld = 'VIDKS2'
017700150914     c                   move      vidks2        chkks2
017800150914     C                   exsr      srclie
017801150928      * ricerca area richiedente
017802150928     C                   when      §fld = 'VIDAR1'
017804150928     C                   exsr      srarea
017805150928     c                   movel     parkey        vidar1
017807150928      * ricerca filiale richiedente
017808150928     C                   when      §fld = 'VIDFI1'
017809150928     C                   exsr      srfil
017810150928     c                   if        §cod <> ' '
017811150928     c                   move      §cod          vidfi1
017812150928     c                   movel(p)  §des          vidfi1d
017813150928     c                   end
017814150928      * ricerca area addebito
017815150928     C                   when      §fld = 'VIDAR2'
017816150928     C                   exsr      srarea
017817150928     c                   movel     parkey        vidar2
017819150928      * ricerca filiale richiedente
017820150928     C                   when      §fld = 'VIDFI2'
017821150928     C                   exsr      srfil
017822150928     c                   if        §cod <> ' '
017823150928     c                   move      §cod          vidfi2
017824150928     c                   movel(p)  §des          vidfi2d
017825150928     c                   end
017900011130     C                   ENDsl
018000011130     C* POSIZIONAMENTO CURSORE
018100011130     C                   Z-ADD     §RIGA         §RIG
018200011130     C                   Z-ADD     §COLON        §COL
018300011130     C                   ENDSR
018400011130     c**********************************************************************
018500150529     C** tipo registrazione
018600011130     c**********************************************************************
018700150529     C     srtipo        BEGSR
018800011130     c*
019902151103     c                   clear                   vtipor
019903151103     c                   call      'TRTBTCER'
019904151103     c                   parm      'F'           tipo              1
019905151103     c                   parm                    vtipor            3
019907151105     c                   if        vtipor <> *blank
019908151105     c                   movel     vtipor        vtipo
019909151103     c                   exsr      srtab
019910151103     c                   eval      dtce = t02uni
019911151103     C                   MOVEL     §TCEDES       dtipo
019912151103     c                   endif
020000011130     c*
020100011130     C                   ENDSR
020200011130     C**********************************************************************
020300150529     C** Controlli e decodifiche campi con tntbe00f
020400011130     C**********************************************************************
020401151103     C     srtab         BEGSR
020402151103     c                   clear                   tibs02ds
020404151103     c                   if        vtipo <> *blank
020405151103     c                   eval      t02mod = 'C'
020406151103     c                   eval      t02ke1 = vtipo
020407151103     c                   endif
020408151103     c                   eval      t02sif = knsif
020409151103     c                   eval      t02cod = 'TCE'
020410151103     c                   call      'TIBS02R'
020411151103     c                   parm                    kpjba
020412151103     c                   parm                    tibs02ds
021700011130     c*
021800011130     C                   ENDSR
021900150914     C**********************************************************************
022000150914     C** richiamo  routine ricerca clienti
022100150914     C**********************************************************************
022200150914     C     srclie        BEGSR
022300150914     C* RICHIESTO ELENCO CLIENTI
022400150914     C                   CLEAR                   DSXC
022500150914     C                   Z-ADD     DUTKCI        DXCCAP
022600150914     C                   CALL      'XCLIR'
022700150914     C                   PARM                    DSXC
022800150914     C* SCOMPATTA
022900150914     C                   MOVEA     DXCKSC        KSA
023000150914     C     1             DO        30            J                 3 0
023100150914     C                   MOVE      KSA(J)        DSKSA
023200150914     C                   Z-ADD     DSKSN         KSC(J)
023300150914     C                   ENDDO
023400150914     C                   IF        KSC(1) > *ZEROS
023500150914     C                   MOVE(P)   KSC(1)        CHKKS2
023501150929     C                   MOVE      chkks2        vidks2
023600150914     C                   ELSE
023700150914     C                   MOVEL     *BLANKS       CHKKS2
023800150914     C                   ENDIF
023900150914     C*
024000150914     C* CODICE CLIENTE NUMERICO
024100150914     C     CHKKS2        IFLT      *ZEROS
024200150914     C                   seton                                        859928
024300150914     C                   ELSE
024400150914     C*
024500150914     C* SOLO CARATTERI NUMERICI
024600150914     C   29              MOVE      *all'0'       CHKKS2
024700150914     C   29              MOVE      VIDKS2        CHKKS2
024800150914     C                   SETOFF                                       29
024900150914     C                   TESTN                   CHKKS2               29
025000150914     C     *IN29         IFEQ      *OFF                                         *NON E'NUMERO
025100150914     C                   seton                                        859928
025200150914     C                   ELSE                                                   *E'NUMERO DA TESTN
025300150914     C                   MOVE      CHKKS2        W001              1
025400150914     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
025500150914     C                   seton                                        859928
025600150914     C                   ELSE                                                   *NUMERO
025700150914     C*
025800150914     C* CONTROLLA ESISTENZA CLIENTE
025900150914     C                   EXSR      DECTIB
026000150914     C                   ENDIF
026100150914     C                   ENDIF
026200150914     C                   ENDIF
026300150914     c   85              eval      $msg = 'Cliente errato o non in gestione'
026400150914     C*
026500150914     C                   ENDSR
026600150914     C*------------------------------------------------------------------------*
026700150914     C* dectib - decodifica ragione sociale per vedere se compreso in scansione
026800150914     C*------------------------------------------------------------------------*
026900150914     C     dectib        BEGSR
027000150914     C                   CLEAR                   tibs69ds
027100150914     C                   CLEAR                   ds_cnaco
027200150914     C                   MOVEL     KNSIF         I69SIF
027300150914     C                   MOVE      chkks2        I69KAC
027400150914     C                   CALL      'TIBS69R'
027500150914     C                   PARM                    tibs69ds
027600150914     C                   PARM                    DS_cnaco
027700150914     C                   PARM                    DS_cnind
027800150914     C                   PARM                    DS_cnclp
027900150914     C                   PARM                    DS_fncls
028000150914     C     O69ERR        IFNE      '1'
028100150914     C                   MOVEL     ACORAG        vidks2d
028200150914     C                   ELSE
028300150914     C                   MOVEL     *ALL'*'       vidks2d
028400150914     C                   ENDIF
028500150914     C                   endsr
028600150914     c*
028601150928     C**********************************************************************
028602150928     C** Controlli e decodifiche campi con TABEL00f
028603150928     C**********************************************************************
028604150928     C     srarea        BEGSR
028605150928     c                   clear                   ds05
028606150928     c                   movel     parkey        parkeys           8
028607150928     C                   CALL      'TRUL19R'
028608150928     c                   parm      '05'          parcod            2
028609150928     c                   parm      '1'           parord            1
028610150928     c                   parm      *blanks       parkey            8
028611150928     c                   parm      *blanks       cmd               1
028612150928     c* ritorno senza scelta
028613150928     c                   if        cmd = 'L'
028614150928     c                   movel     parkeys       parkey
028617150928     c                   endif
028623150928     c*
028624150928     C                   ENDSR
028625150928     C*----------------------------------------------------*
028626150928     C* ricerca filiale
028627150928     C*----------------------------------------------------*
028628150928     C     srfil         begsr
028629150928     c                   clear                   §cod
028630150928     c                   eval      §tip = 'F'
028631150928     c                   clear                   §des
028632150928     c                   clear                   §pra
028633150928     c                   call      'TNSD24R'
028634150928     c                   parm                    §cod
028635150928     c                   parm                    §tip
028636150928     c                   parm                    §des
028637150928     c                   parm                    §pra
028642150928     C                   ENDsr
028700011130     C*----------------------------------------------------*
028800011130     c     *inzsr        begsr
028900011130     C*----------------------------------------------------*
029000011130     C     *ENTRY        PLIST                                                  *
029100011130     C                   PARM                    KPJBA                          *
029200150529     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
029300150529     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
029400150529     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
029500150529     C                   move      WN8           dtaeur
029600150529     C                   move      dtaeur        DtaCOR                         *DATA CORRENTE AA/M/
029601151020     C                   move      dtacor        wn8g              8 0          *DATA CORRENTE AA/M/
029700150529      * tabelle
029800150529     C                   EXSR      REPDATIUTE
029900150727     C* POSIZIONAMENTO TABELLE                             *
030000150727     C     KTbe          KLIST                                                  *
030100150727     C                   KFLD                    TBECOD                         *
030200150727     C                   KFLD                    TBEKE1                         *
030300150727     C                   KFLD                    TBEKE2                         *
030301150928     C     Ktab          KLIST                                                  *
030302150928     C                   KFLD                    codut1            1 0          *
030303150928     C                   KFLD                    cod               2            *
030304150928     C                   KFLD                    key               8            *
030305150928     C                   z-add     1             codut1            1 0          *
030400150529     c                   endsr
030500150529     C*--------------------------------------------------------------------------------------------*
030600150529     C* REPERISCE I DATI UTENTE
030700150529     C*--------------------------------------------------------------------------------------------*
030800150529     C     REPDATIUTE    BEGSR
030900150529     C*
031000150529     C* INIZIALIZZA VARIABILI DI WRK
031100150529     C                   CLEAR                   TIBS34DS
031200150529     C                   CLEAR                   AZUTEDS
031300150529     C                   CLEAR                   DDATIUTE
031400150529     C*
031500150529     C     *DTAARA       DEFINE    §azute        azuteds
031600150529     C     *DTAARA       DEFINE    §datiute      ddatiute
031700150529     C                   IN(E)     *DTAARA
031800150529     C                   IF        %Error
031900150529     C                   EVAL      I34Tla = 'L'
032000150529     C                   CALL      'TIBS34R'
032100150529     C                   PARM                    Tibs34Ds
032200150529     C                   IN        *DTAARA
032300150529     C                   ENDIF
032400150811     c                   Clear                   wabi
032500150811     c                   Clear                   dLat
032600150811
032700150811      * Verifica errori e autorità profilo
032800150811s   1c                   Select
032900150811      * se ho errori nei dati utente esco dal pgm
033000150811w   1c                   When      DutErr = 'E'
033100150811     c                   seton                                        lr
033200150811     c                   return
033300150811      * se non c'è l'abilitazione
033400150811      * --> se 1° livello, abilitazioni al terminal
033500150811      *     se 2° livello, abilitazioni al punto operativo
033600150811w   1c                   When      UteAut = *Blanks
033700150811if  2c                   If        DutLpo = '1'
033800150811     c                   Eval      wabi   = 'TP'
033900150811e   2c                   EndIf
034000150811if  2c                   If        DutLpo = '2'
034100150811     c                   Eval      wabi   = 'PO'
034200150811e   2c                   EndIf
034300150811if  2c                   If        DutLpo = 'S'
034400150811     c                   Eval      wabi   = 'AZ'
034500150811e   2c                   EndIf
034600150811      * carica le abilitazioni del profilo
034700150811x   1c                   Other
034800150811     c                   Movel     UteFaf        Dute01
034900150811     c                   If        §Uteadd <> *Blanks
035000150811     c                   Eval      wabi = §Uteadd
035100150811     c                   Else
035200150811     c                   Eval      wabi = UteAut
035300150811     c                   EndIf
035400150811e   1c                   EndSl
035500150811      * controllo se ok l'abilitazione dell'utente
035600150811     c                   Clear                   Tibs02ds
035700150811     c                   Eval      T02Mod = 'C'
035800150811     c                   Eval      T02Sif = knsif
035900150811     c                   Eval      T02Cod = 'LAT'
036000150811     c                   Movel(p)  wabi          T02Ke1
036100150811     c                   Call      'TIBS02R'
036200150811     c                   Parm                    kpjba
036300150811     c                   Parm                    Tibs02ds
036400150811if  1c                   If        T02Err = *Blanks
036500150811     c                   Eval      dLat = T02Uni
036600150811e   1c                   EndIf
036601151020      * verifica se filiale profilo partita con nuova procedura
036602151021      * controllo effettuato solo per profili di filiale
036603151021     c                   if        dutpou <> 046
036604151020     c                   exsr      repVpo
036605151020      * filiale non ancora partita da tabella VPO
036606151020if  1c                   If        dataVpo > wn8g
036607151020     c                   seton                                        lr
036608151020     c                   return
036609151020   x1c                   endif
036610151021   x1c                   endif
036700151007      * errore o non abilitato
036800150914if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
036801151028if  1c                             or §uteadd = 'NO'
036900150811     c                   seton                                        98
037000150914     c                   call      'XUTENOAB'
037001151028     c                   parm                    kpjba
037100150811     c                   seton                                        lr
037200150811     c                   return
037300150811   x1c                   endif
037400150914      * Reperimento dei P.O. gestibili dall'utente
037500150914     c                   clear                   TRUL31DS
037600150914     c                   clear                   TRUL31DS2
037700150914     c                   eval      I31abi = wabi
037800150914     c                   eval      I31cdi = DUTdis
037900150914     c                   eval      I31car = DUTare
038000150914     c                   eval      I31cpo = DUTpou
038100150914     c                   call      'TRUL31R'
038200150914     c                   parm                    KPJBA
038300150914     c                   parm                    TRUL31DS
038400150914     c                   parm                    TRUL31DS2
038500150914if  2c                   if        O31pog > *zeros
038600150914     c                   movea     O31pog        $pog
038700150914     c                   movea     O31arg        $arg
038800150914     c                   movea     O31dig        $dig
038900150914x   2c                   else
039000150914     c* impossibile perchè controllato nel filtrp
039100150914     c                   seton                                        lr
039200150914     c                   return
039300150914    1c                   endif
039400150914     c                   do        250           g                 3 0
039500150914     c                   if        $pog(g) > *zero
039600150914     c                   move      $pog(g)       $pogn(g)
039700150914     c                   else
039800150914     c                   move      999           $pogn(g)
039900150914     c                   endif
040000150914     c                   enddo
040100150914     c                   if        §uteaddt <> *blank
040200150914     c                   seton                                        20
040300150914     c                   endif
040400011130     c                   endsr
040500151020     c*____________________________________________________________
040600151020     c     repVpo        begsr
040700151020     c*____________________________________________________________
040800151020     c                   clear                   dataVpo           8 0
040900151020      *reperisco limiti da tabella per switch su nuove stime
041000151020     c                   movel(p)  'FICN34R'     tbeke1
041100151020     c                   movel(p)  dutpou        tbeke2
041200151020     c                   movel     'VPO'         tbecod
041300151020     c     ktbe          chain     tntbe01l
041400151020     c                   if        %found(tntbe01l)
041500151020     c                   movel     tbeuni        dataVpo
041600151020     C                   Z-ADD     dataVpo       G08inv
041700151020     C                   MOVEL     '3'           G08ERR
041800151020     C                   CALL      'XSRDA8'
041900151020     C                   PARM                    WLBDA8
042000151020     c                   if        g08err <> '0'
042100151020     c                   move      20391231      dataVpo
042200151020     c                   end
042300151020     c                   else
042400151020      *ricerco 999 abilitazione per tutti
042500151020     c                   if        dataVpo999 = 0
042600151020     c                   movel(p)  '999'         tbeke2
042700151020     c                   movel     'VPO'         tbecod
042800151020     c     ktbe          chain     tntbe01l
042900151020     c                   if        %found(tntbe01l)
043000151020     c                   movel     tbeuni        dataVpo
043100151020     c                   movel     tbeuni        dataVpo999        8 0
043200151020     C                   Z-ADD     dataVpo       G08inv
043300151020     C                   MOVEL     '3'           G08ERR
043400151020     C                   CALL      'XSRDA8'
043500151020     C                   PARM                    WLBDA8
043600151020     c                   if        g08err <> '0'
043700151020     c                   move      20391231      dataVpo
043800151020     c                   move      20391231      dataVpo999
043900151020     c                   end
044000151020     c                   endif
044100151020     c                   else
044200151020     c                   z-add     dataVpo999    dataVpo
044300151020     c                   endif
044400151020     c                   endif
044500151020     c                   endsr
