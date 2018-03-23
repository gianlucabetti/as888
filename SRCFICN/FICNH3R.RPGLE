000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200150918     H*
000300160112     Fficnh3d   CF   E             WORKSTN
000400150727     FTntbe01l  IF   E           K DISK
000502160112     Ftnsre02l  uf a E           K DISK
000503160112     Fanfrn01l  IF   E           K DISK
000504160112     Fansog01l  IF   E           K DISK
000505160121     Fansif01l  IF   E           K DISK
000506160112     Fndmov08l  IF   E           K DISK
000600150916     D DTAcor          S               D   INZ
000700160112     D vdat1g          S              8s 0 INZ
000701160112     D vdat2g          S              8s 0 INZ
000900011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
000901160112     D DTAiso          S               D
001000000000     D KPJBA         E DS
001001150921     d  savkpjbu       s                   like(kpjbu)
001002160112     d  param          ds
001003160112     d   parsoc                       3
001004160112     d   parfor                       8
001005160112     d   parcau                       3
001006160112     d   pardat                       8s 0
001007160112     d   paropz                       1
001008160112     d ficnh1i       E DS                  INZ
001009160112     d dsre          E DS                  INZ
001010160114     d dtsr          E DS                  INZ
001011160114     d dsrexaf       E DS                  INZ
001300150807     D TIBS02DS      E DS
001400150529     D TIBS34DS      E DS                                                       *Profili utente
001500150529     D DDATIUTE      E DS                                                       *Dati utente
001600150529     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001700150811     D dute01        E DS                                                       *Utenti
001800150811     d dLat          e ds
001900150914     D TRUL31DS      E DS
002000150914     D TRUL31DS2     E DS
002001160112     D ana400ds      E DS
002002160121     D ana113ds      E DS
002100150811     d wabi            s                   like(UteAut)
002200150914     d* p.o. abilitati
002300150914     D $pogn           s              3  0 dim(250)
002400150914     D $pog            s              3    dim(250)
002500150914     D $DIG            s              1    dim(20)
002600150914     D $ARG            s              3    dim(50)
004300150914     D*------------------
004400150914     D WLBDA8          DS                  INZ
004500150914     D  G08DAT                 1      8  0
004600150914     D  G08INV                 9     16  0
004700150914     D  G08ERR                17     17
004800150914     D  G08TGI                18     22  0
004900000000     C*
004901150930     d Wdata8          DS
004902150930     d  dadata                 1      8  0
004903150930     d  adata                  9     16  0
004904150930     d  GioLav                17     21  0
004905160112      *_______________________________________________________________________________
005000011130     C  nlr              DO        *HIVAL
005100011130     c                   EXSR      CLR00
005101150916      * se passato rekord esistente carica i dati solo al primo giro altrimenti immissione
005102160112     c                   if        parcau <> *blank and caricato = *blank
005103150916     c                   exsr      imposta
005104150916     C                   EXSR      CTRL01
005105150917     c                   move      'X'           caricato          1
005106150916     c                   seton                                        70
005112150916     c                   endif
005113150916      *
005114151105     c                   if        paropz = '5'
005115151105     c                   setoff                                       2899
005116151105     c                   endif
005117151105      *
005200160112     C                   EXFMT     ficnh3
005300011130     C* FINE LAVORO
005400150929     C   Kl              LEAVE
005500011130     C* RICERCA
005501160121     c                   if        *inkd
005600160121     C  n70              EXSR      RIC01
005601151006     c                   iter
005602151006     c                   end
005603150921     C* RICERCA
005700000000     C* CONTROLLI
005800000000     C                   EXSR      CTRL01
005900011130     C   99              ITER
006000150916     c                   if        *inkf
006001160112     c                   if        parcau = *blank
006100150918     c                   exsr      scrive
006102151123     C   99              ITER
006103150918     c                   else
006104160112     c                   if        paropz = '2'
006105150918     c                   exsr      aggiorna
006106160112     c                   endif
006107160112     c                   if        paropz = '4'
006108160112     c                   exsr      cancella
006109160112     c                   endif
006110151123     C   99              ITER
006111150918     c                   endif
006112150916     c                   leave
006300150914     c                   endif
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
009300150914     C                   SETOFF                                       998081
009400150914     C                   SETOFF                                       828384
009500160121     C                   SETOFF                                       852886
009600011130     C                   DO
009700160112      * CONTROLLO causale
009800160112     c                   if        vcau   <> *blank
009900151103     c                   exsr      srtab
009901150930     c                   if        t02err <> *blank
009902160112     C                   clear                   vcaud
009903150930     C                   SETON                                        992880
009904150930     c                   eval      $msg = 'Causale errata'
009905151105     c  n77              leavesr
009906150930     c                   endif
010401160107     c                   else
010402160107     C                   SETON                                        992880
010403160107     c                   eval      $msg = 'Causale obbligatoria'
010404160107     c  n77              leavesr
010405160107     c                   endif
010406160115      * CONTROLLO tipo servizio
010407160115     c                   if        vtsr   <> *blank
010408160115     c                   exsr      srtabb
010409160115     c                   if        t02err <> *blank
010410160115     C                   clear                   vtsrd
010411160115     C                   SETON                                        992885
010412160115     c                   eval      $msg = 'Tipo servizio errato'
010413160115     c  n77              leavesr
010414160115     c                   endif
010415160115     c                   else
010416160115     C                   SETON                                        992885
010417160115     c                   eval      $msg = 'Tipo servizio obbligatorio'
010418160115     c  n77              leavesr
010419160115     c                   endif
010420160121     c* codice società
010422160121     c                   if        vsoc <> *blank
010423160121     C     vsoc          CHAIN     ansif01l
010424160121     c                   if        not%found(ansif01l)
010425160121     C                   SETON                                        992886
010426160121     c                   eval      $msg = 'Società errata'
010427160121     c                   leavesr
010428160121     C                   endif
010429160211     c                   else
010430160211     C                   seton                                        862899
010431160211     C                   eval      $msg = 'Società obbligatoria'
010432160211     c  n77              leavesr
010433160121     C                   endif
010434160112      *fornitore
010435160112     c                   clear                   vford
010436160121     c                   if        vfor <> *blank
010437160112     C     Kfrn          CHAIN     anfrn01l
010438160112     c                   if        %found(anfrn01l)
010439160112     c     frnsogg       chain     ansog01l
010440160112     c                   if        %found(ansog01l)
010441160112     c                   movel     sogdes        vford
010442160112     c                   else
010443160112     C                   seton                                        832899
010444160112     C                   eval      $msg = 'Fornitore non trovato'
010445160112     c  n77              leavesr
010446160112     c                   endif
010447160211     c                   else
010448160211     C                   seton                                        832899
010449160211     C                   eval      $msg = 'Fornitore non trovato'
010450160211     c  n77              leavesr
010451160112     c                   endif
010452160112     c                   else
010453160112     C                   seton                                        832899
010454160211     C                   eval      $msg = 'Fornitore obbligatorio'
010455160112     c  n77              leavesr
010456160112     c                   endif
010600160112      * data
010800160112     c                   clear                   vdat1g
010900160112     c                   if        vdat > 0
011000160112     c                   move      vdat          g08dat
011100150914     c                   move      *blank        g08err
011200150914     C                   CALL      'XSRDA8'
011300150914     C                   PARM                    WLBDA8
011400150914     C     G08ERR        IFEQ      '1'
011500150914     C                   SETON                                        992882
011600150914     c                   eval      $msg = 'Data errata '
011700151105     c  n77              leavesr
011800150914     C                   endif
011900160112     c                   move      g08inv        vdat1g
011901160112     c                   move      g08dat        vdat
011902160112     c                   else
011903160112     C                   SETON                                        992882
011904160112     c                   eval      $msg = 'Data obbligatoria'
011905160112     c  n77              leavesr
011906160112     c                   endif
011907160112     c* importo
011908160114     c                   if        vimp <> *zero
011909160115     c                   if        §sresegno = '+' and vimp < 0
011910160115     C                   seton                                        812899
011911160115     C                   eval      $msg = 'Importo negativo non ammesso -
011912160115     c                             per la causale'
011913160115     c  n77              leavesr
011914160115     c                   endif
011915160115     c                   if        §sresegno = '-' and vimp > 0
011916160115     C                   seton                                        812899
011917160115     C                   eval      $msg = 'Importo positivo non ammesso -
011918160115     c                             per la causale'
011919160115     c  n77              leavesr
011920160115     c                   endif
011921160112     c                   else
011923160112     C                   seton                                        812899
011924160112     C                   eval      $msg = 'Importo obbligatorio'
011925160112     c  n77              leavesr
011926160112     c                   endif
011927160115     c                   if        parcau = *blank
011928160115     c     ksrev         chain     tnsre02l
011929160115     c                   if        %found(tnsre02l)
011930160115     C                   SETON                                        992882
011931160115     C                   SETON                                        8380
011932160115     c                   eval      $msg = 'Attenzione rettifica già presente -
011933160115     c                             modificare quella esistente'
011934160115     c  n77              leavesr
011935160115     c                   endif
011936160115     c                   endif
011937160112     c                   select
011938160114     c                   when      vcau = 'RAF'
011939160114     c                   exsr      ctrRAF
011940160112     c                   endsl
011941160112     c                   enddo
016800000000     C                   ENDSR
016801160112     c*_________________________________________________________
016802160115     c     ctrRAF        begsr
016803160112     c*_________________________________________________________
016804160114     c                   clear                   vdat2g
016805160114     c                   if        vdfta > 0
016806160114     c                   move      vdfta         g08dat
016807160112     c                   move      *blank        g08err
016808160112     C                   CALL      'XSRDA8'
016809160112     C                   PARM                    WLBDA8
016810160112     C     G08ERR        IFEQ      '1'
016811160211     C                   SETON                                        992884
016812160112     c                   eval      $msg = 'Data errata '
016813160112     c  n77              leavesr
016814160112     C                   endif
016815160114     c                   move      g08inv        vdat2g
016816160114     c                   move      g08dat        vdfta
016817160112     c                   else
016818160211     C                   SETON                                        992884
016819160112     c                   eval      $msg = 'Data obbligatoria'
016820160112     c  n77              leavesr
016821160112     c                   endif
016822160112      * verifico se data anticipo maggiore di data rettifica
016823160112     c                   if        vdat2g > vdat1g
016824160211     C                   SETON                                        992884
016825160112     c                   eval      $msg = 'Data anticipo maggiore di data -
016826160112     c                             rettifica'
016827160112     c  n77              leavesr
016828160112     c                   endif
016830160112     c                   move      vdat2g        movdtdoc
016834160112     c     kndmov        chain     ndmov08l
016835160112     c                   if        not %found(ndmov08l)
016836160211     C                   SETON                                        992884
016837160112     c                   eval      $msg = 'Documento indicato come riferim-
016838160112     c                             ento anticipo non trovato'
016839160112     c  n77              leavesr
016840160112     c                   endif
016841160112     c* codice fornitore
016842160112      *
016843160112     C                   ENDSR
016900011130     C******************************************************
017000011130     C* RICERCA PRIMO VIDEO
017100011130     C******************************************************
017200011130     C     RIC01         BEGSR
017300011130     c                   select
017400160112     C                   when      §fld = 'VCAU'
017500160112     C                   exsr      srtab
017600160112     C                   when      §fld = 'VFOR'
017800160112     C                   exsr      call400
017801160121     C                   when      §fld = 'VSOC'
017802160121     C                   exsr      call113
017803160114     C                   when      §fld = 'VTSR'
017804160114     C                   exsr      srtabb
017900011130     C                   ENDsl
018000011130     C* POSIZIONAMENTO CURSORE
018100150930     C*                  Z-ADD     §RIGA         §RIG
018200150930     C*                  Z-ADD     §COLON        §COL
018201150930     C                   Z-ADD     0             §RIG
018202150930     C                   Z-ADD     0             §COL
018300011130     C                   ENDSR
018301160112     C**********************************************************************
018302160112     C** richiamo  routine ricerca clienti
018303160112     C**********************************************************************
018304160112     C     call400       BEGSR
018305160112     C*
018306160112     C                   RESET                   ana400ds
018307160112     C                   MOVEL     '01'          OPZ400
018308160112     C                   MOVEL     ana400ds      KPJBU
018309160112     C                   CALL      'ANA400R'
018310160112     C                   PARM                    KPJBA
018311160112     C                   MOVEL     KPJBU         ana400ds
018312160112     C     KSC400        IFNE      *BLANK
018313160112     C                   MOVEL     ksc400        vfor
018314160112     C                   ELSE
018315160112     C                   MOVEL     *blanks       vfor
018316160112     C                   ENDIF
018317160112     C*
018318160112     C                   ENDSR
018319160121     C**********************************************************************
018320160121     C** richiamo  routine ricerca società
018321160121     C**********************************************************************
018322160121     C     call113       BEGSR
018323160121     C*
018324160121     C                   RESET                   ana113ds
018325160121     C                   MOVEL     '01'          OPZ113
018326160121     C                   MOVEL     '1'           inz113
018327160121     C                   MOVEL     ana113ds      KPJBU
018328160121     C                   CALL      'ANA113R'
018329160121     C                   PARM                    KPJBA
018330160121     C                   MOVEL     KPJBU         ana113ds
018331160121     C     societa113    IFNE      *BLANK
018332160121     C                   MOVEL     societa113    vsoc
018333160121     C                   ELSE
018334160121     C                   MOVEL     *blanks       vsoc
018335160121     C                   ENDIF
018336160121     C*
018337160121     C                   ENDSR
020200011130     C**********************************************************************
020300150529     C** Controlli e decodifiche campi con tntbe00f
020400011130     C**********************************************************************
020500011130     C     srtab         BEGSR
020600150807     c                   clear                   tibs02ds
020800160112     c                   if        vcau  <> *blank
020900150807     c                   eval      t02mod = 'C'
021000160112     c                   eval      t02ke1 = vcau
021100150807     c                   endif
021200150807     c                   eval      t02sif = knsif
021300160112     c                   eval      t02cod = 'SRE'
021400150807     c                   call      'TIBS02R'
021500150807     c                   parm                    kpjba
021600150807     c                   parm                    tibs02ds
021601160112     c                   eval      dsre = t02uni
021602160112     C                   MOVEL     §sreDES       vcaud
021603160112     C  n70              MOVEL     t02ke1        vcau
021700011130     c*
021800011130     C                   ENDSR
021801160114     C**********************************************************************
021802160114     C** Controlli e decodifiche campi con tntbe00f
021803160114     C**********************************************************************
021804160114     C     srtabb        BEGSR
021805160114     c                   clear                   tibs02ds
021806160114     c                   if        vtsr  <> *blank
021807160114     c                   eval      t02mod = 'C'
021808160114     c                   eval      t02ke1 = vtsr
021809160114     c                   endif
021810160114     c                   eval      t02sif = knsif
021811160114     c                   eval      t02cod = 'TSR'
021812160114     c                   call      'TIBS02R'
021813160114     c                   parm                    kpjba
021814160114     c                   parm                    tibs02ds
021815160115     c                   eval      dtsr = t02uni
021816160114     C                   MOVEL     §tsrDES       vtsrd
021817160114     C  n70              MOVEL     t02ke1        vtsr
021818160114     c*
021819160114     C                   ENDSR
021900150916     C**********************************************************************
021901150916     C** Controlli e decodifiche campi con tntbe00f
021902150916     C**********************************************************************
021903150916     C     imposta       BEGSR
021904160112     c     ksre          chain     tnsre02l
021905160112     c                   if        %found(tnsre02l)
021906160112     c                   move      sresre        vcau
021907160114     c                   move      sretsr        vtsr
021908160112     c                   z-add     sretim        vimp
021909160112     c                   move      sresoc        vsoc
021910160112     c                   move      srecdf        vfor
021911160112     c                   move      sreddc        vdat1g
021912160112     c                   move      sreddc        dtaiso
021913160112     c                   move      dtaiso        dtaeur
021914160112     c                   move      dtaeur        vdat
021915160112     c                   clear                   vdat2g
021916160112     c                   clear                   vdfta
021917160112     c                   clear                   vnfta
021918160112     c                   select
021919160114     c                   when      vcau = 'RAF'
021920160114     c                   if        sreuni <> *blank
021921160114     c                   movel     sreuni        dsrexaf
021922160114     c                   else
021923160114     c                   clear                   dsrexaf
021924160114     c                   endif
021926160114     c                   move      §SREXAFDF     dtaiso
021927160112     c                   move      dtaiso        dtaeur
021928160112     c                   move      dtaeur        vdfta
021929160114     c                   move      dtaiso        vdat2g
021930160114     c                   move      §SREXAFNF     vnfta
021932160112     c                   endsl
021941151006     c                   endif
021965150916     C                   ENDSR
021966150916     C**********************************************************************
021967150918     C** scrittura file
021968150916     C**********************************************************************
021969150918     C     scrive        BEGSR
021970160112     c                   clear                   tnsre000
021971160112     c                   move      vcau          sresre
021973160112     c                   move      vsoc          sresoc
021974160112     c                   move      vfor          srecdf
021975160112     c                   move      vdat1g        sreddc
021976160112     c                   z-add     vimp          sretim
021977160114     c                   move      vtsr          sretsr
021978160114     c                   move      §tsrtip       sretip
021979160115     c                   move      knmus         sreute
021980160115     c                   move      dtacor        sredvr
021981160112      *
021982160112     c                   select
021983160114     c                   when      vcau = 'RAF'
021984160114     c                   move      vdat2g        §SREXAFDF
021985160114     c                   move      vnfta         §SREXAFNF
021986160114     c                   movel     dsrexaf       sreuni
021987160112     c                   endsl
021996160112     c                   write     tnsre000
022015150916     C                   endsr
022016150918     C**********************************************************************
022017150918     C** Conferma dati scrittura o aggiornamento
022018150918     C**********************************************************************
022019150918     C     aggiorna      BEGSR
022020160112     c                   z-add     vimp          sretim
022021160115     c                   move      knmus         sreute
022022160115     c                   move      dtacor        sredvr
022023160112      *
022024160112     c                   select
022025160114     c                   when      vcau = 'RAF'
022026160114     c                   move      vdat2g        §SREXAFDF
022027160114     c                   move      vnfta         §SREXAFNF
022028160114     c                   movel     dsrexaf       sreuni
022029160112     c                   endsl
022057160112     c                   update    tnsre000
022058150918     C                   endsr
022059160112     C**********************************************************************
022060160112     C** Cancella dati
022061160112     C**********************************************************************
022062160112     C     cancella      BEGSR
022071160112     c                   delete    tnsre000
022072160112     C                   endsr
028700011130     C*----------------------------------------------------*
028800011130     c     *inzsr        begsr
028900011130     C*----------------------------------------------------*
029000011130     C     *ENTRY        PLIST                                                  *
029100011130     C                   PARM                    KPJBA                          *
029101150916     c                   movel     kpjbu         param
029102150916      *
029200150529     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
029300150529     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
029400150529     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
029500150529     C                   move      WN8           dtaeur
029600150529     C                   move      dtaeur        DtaCOR                         *DATA CORRENTE AA/M/
029700150529      * tabelle
029701151202     c                   if        paropz = '5'
029702151202     c                   seton                                        7778
029703151202     c                   endif
029800150529     C                   EXSR      REPDATIUTE
029801150924     c                   eval      vidopz = 'Inserimento'
029802150924     c                   select
029803150924     c                   when      paropz = '2'
029804150924     c                   eval      vidopz = 'Modifica'
029805150924     c                   when      paropz = '4'
029806160211     c                   seton                                        77
029807160112     c                   eval      vidopz = 'Cancella'
029811150924     c                   when      paropz = '5'
029812150924     c                   eval      vidopz = 'Visualizza'
029813150924     c                   endsl
029900150727     C* POSIZIONAMENTO TABELLE                             *
030000150727     C     KTbe          KLIST                                                  *
030100150727     C                   KFLD                    TBECOD                         *
030200150727     C                   KFLD                    TBEKE1                         *
030300150727     C                   KFLD                    TBEKE2                         *
030301160112      *
030302160112     C     Kfrn          KLIST
030303160112     C                   KFLD                    vsoc
030304160112     C                   KFLD                    vfor
030305160112      *
030306160112     C     Ksre          KLIST
030307160112     C                   KFLD                    parsoc
030308160112     C                   KFLD                    parfor
030309160112     C                   KFLD                    parcau
030310160112     C                   KFLD                    pardat
030311160112      *
030312160115     C     Ksrev         KLIST
030313160115     C                   KFLD                    vsoc
030314160115     C                   KFLD                    vfor
030315160115     C                   KFLD                    vcau
030316160115     C                   KFLD                    vdat1g
030317160115      *
030318160112     C     Kndmov        KLIST                                                  *
030319160112     C                   KFLD                    vsoc                           *
030320160112     C                   KFLD                    movctb                         *
030321160112     C                   KFLD                    movkcc                         *
030322160112     C                   KFLD                    vfor                           *
030323160112     C                   KFLD                    movdtdoc                       *
030324160112     C                   KFLD                    vnfta                          *
030325160112     C                   move      'CG'          movctb                         *
030326160112     C                   move      '000400'      movkcc                         *
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
040400011130     c                   endsr
