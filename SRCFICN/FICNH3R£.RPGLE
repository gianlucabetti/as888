000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200150918     H*
000300150916     Fycostp5d  CF   E             WORKSTN
000400150727     FTntbe01l  IF   E           K DISK
000500150914     Fazorg01l  IF   E           K DISK
000501150921     Fanpdc01l  IF   E           K DISK
000502150916     Ffntna01l  uf a E           K DISK
000503150923     Ffnrna02l  uf a E           K DISK
000504150918     Ffnfna00f  o    E             DISK
000600150916     D DTAcor          S               D   INZ
000700150914     D viddt1g         S               D   INZ
000900011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001000000000     D KPJBA         E DS
001001150921     d  savkpjbu       s                   like(kpjbu)
001002150916     d  param          ds
001003150916     d   parrich                      9s 0
001004150916     d   paropz                       1
001005150916      *
001100150807     d dtce          e ds
001200150914     D ycostp4i      E DS
001300150807     D TIBS02DS      E DS
001400150529     D TIBS34DS      E DS                                                       *Profili utente
001500150529     D DDATIUTE      E DS                                                       *Dati utente
001600150529     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001700150811     D dute01        E DS                                                       *Utenti
001800150811     d dLat          e ds
001801150916     D TRUL33DS      E DS
001900150914     D TRUL31DS      E DS
002000150914     D TRUL31DS2     E DS
002001150921     D ana330ds      E DS
002100150811     d wabi            s                   like(UteAut)
002200150914     d* p.o. abilitati
002300150914     D $pogn           s              3  0 dim(250)
002400150914     D $pog            s              3    dim(250)
002500150914     D $DIG            s              1    dim(20)
002600150914     D $ARG            s              3    dim(50)
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
004900000000     C*
004901150930     d Wdata8          DS
004902150930     d  dadata                 1      8  0
004903150930     d  adata                  9     16  0
004904150930     d  GioLav                17     21  0
004905150930      *
005000011130     C  nlr              DO        *HIVAL
005100011130     c                   EXSR      CLR00
005101150916      * se passato rekord esistente carica i dati solo al primo giro altrimenti immissione
005102150916     c                   if        parrich > 0 and caricato = *blank
005103150916     c                   exsr      imposta
005104150916     C                   EXSR      CTRL01
005105150917     c                   move      'X'           caricato          1
005106150916     c                   seton                                        70
005107150916     c     tnafase       comp      20                                     71
005108151105     c                   else
005109151105     c                   if        vidfi1 = 0
005110151105     c                   move      dutpou        vidfi1
005111151105     c                   endif
005112150916     c                   endif
005113150916      *
005114151105     c                   if        paropz = '5'
005115151105     c                   setoff                                       2899
005116151105     c                   endif
005117151105      *
005200150916     C                   EXFMT     ycostp5
005300011130     C* FINE LAVORO
005400150929     C   Kl              LEAVE
005401151028     C* visualizza fasi
005402151028     c                   if        *inkB
005405151028     C                   movel     param         kpjbu                          *RICARICA SFILE
005406151028     C                   CALL      'YCOSTP7R'
005407151028     C                   PARM                    KPJBA
005411151028     c                   iter
005412151028     c                   end
005500011130     C* RICERCA
005501151006     c                   if        *inkd
005600151006     C                   EXSR      RIC01
005601151006     c                   iter
005602151006     c                   end
005603150921     C* RICERCA
005700000000     C* CONTROLLI
005800000000     C                   EXSR      CTRL01
005900011130     C   99              ITER
006000150916     c                   if        *inkf
006001150918     c                   if        vidric = 0
006100150918     c                   exsr      scrive
006101151123     c   99              z-add     0             vidric
006102151123     C   99              ITER
006103150918     c                   else
006104150918     c                   exsr      aggiorna
006105151123     C   99              ITER
006106150918     c                   endif
006107150916     c                   leave
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
009500150916     C                   SETOFF                                       852886
009600011130     C                   DO
009700150727      * CONTROLLO il tipo
009800150916     c                   if        vtipo  <> *blank
009900151103     c                   exsr      srtab
009901150930     c                   if        t02err <> *blank
009902150930     C                   clear                   dtipo
009903150930     C                   SETON                                        992880
009904150930     c                   eval      $msg = 'Causale errata'
009905151105     c  n77              leavesr
009906150930     c                   endif
010000150916     c                   if        §tceute = 'S'
010100150916     C                   SETON                                        992880
010200150916     c                   eval      $msg = 'Causale non valida in filiale'
010300151105     c  n77              leavesr
010400150916     c                   endif
010401160107     c                   else
010402160107     C                   SETON                                        992880
010403160107     c                   eval      $msg = 'Causale obbligatoria'
010404160107     c  n77              leavesr
010405160107     c                   endif
010600150914      * date
010700150914      * dal
010800150914     c                   clear                   viddt1g
010900150914     c                   if        viddt1 > 0
011000150914     c                   move      viddt1        g08dat
011100150914     c                   move      *blank        g08err
011200150914     C                   CALL      'XSRDA8'
011300150914     C                   PARM                    WLBDA8
011400150914     C     G08ERR        IFEQ      '1'
011500150914     C                   SETON                                        992882
011600150914     c                   eval      $msg = 'Data errata '
011700151105     c  n77              leavesr
011800150914     C                   endif
011900150914     c                   move      g08inv        viddt1g
011901150924     c                   move      g08dat        viddt1
011902150921     c                   clear                   diffe             3 0
011903150921      * verifico se data nei limiti più meno un giorno rispetto a data corrente
011904150922     c                   if        vidric = 0
011905150921     c                   if        viddt1g > dtacor
011907150930     c                   move      dtacor        dadata
011908150930     c                   move      viddt1g       adata
011909150930     c                   CALL      'XSRLAV8'
011910150930     c                   PARM                    Wdata8
011911150930      * se corrisponde ho trovato il D-Day
011912150930     c                   if        giolav > 2
011914150921     C                   SETON                                        992882
011915150930     c                   eval      $msg = 'Data oltre il limite ammesso'
011917151105     c  n77              leavesr
011918150921     c                   endif
011919150921     c                   endif
011920150921     c                   if        dtacor > viddt1g
011921150930     c*    dtacor        subdur    viddt1g       diffe:*d
011922150930     c*                  if        diffe > 1
011923150930     c                   move      dtacor        adata
011924150930     c                   move      viddt1g       dadata
011925150930     c                   CALL      'XSRLAV8'
011926150930     c                   PARM                    Wdata8
011927150930      * se corrisponde ho trovato il D-Day
011928150930     c                   if        giolav > 2
011929150921     C                   SETON                                        992882
011930150930     c                   eval      $msg = 'Data minore del limite ammesso'
011932151105     c  n77              leavesr
011933150921     c                   endif
011934150921     c                   endif
011935150922     c                   endif
011936150916     c                   else
011937150916     C                   SETON                                        992882
011938150916     c                   eval      $msg = 'Data Obbligatoria'
011939151105     c  n77              leavesr
012000150914     C                   endif
014500150914     c* filiale richiedente
014504150918     c                   clear                   vidfi1d
014600150914     c                   if        vidfi1 > *zero
014601151105     c                   if        paropz <> 'C'
014700150914     C     vidfi1        lookup    $pogN                                  79
014800150914     c                   if        not *in79
014900150914     C                   seton                                        832899
015000150914     C                   eval      $msg = 'Filiale richiedente non in gestione'
015001151105     c  n77              leavesr
015100150914     C                   endif
015101150918     c     vidfi1        chain     azorg01l
015102150918     c                   if        %found(azorg01l)
015103150918     c                   movel     orgdes        vidfi1d
015104150918     c                   endif
015105150917     C                   endif
015106150916     c                   else
015108151105     C                   seton                                        832899
015109151105     C                   eval      $msg = 'Filiale richiedente obbligatoria'
015110151105     c  n77              leavesr
015200150914     c                   endif
015201151211     c                   if        §tctsr = 'D' and %subst(orgde8:1:1) <> '1'
015202151211     c                   seton                                        832899
015203151211     c                   eval      $msg = 'Per le causali di affluenza/defluenz-
015204151211     c                             a la richiedente deve essere un terminal'
015205151211     c  n77              leavesr
015206151211     c                   endif
015300150914     c* filiale addebito
015301150918     c                   clear                   vidfi2d
015400150914     c                   if        vidfi2 > *zero
015401151105     c                   if        paropz = 'C'
015500150914     C     vidfi2        lookup    $pogN                                  79
015600150914     c                   if        not *in79
015700150914     C                   seton                                        842899
015800150914     C                   eval      $msg = 'Filiale richiedente non in gestione'
015900150914     C                   endif
015905150917     C                   endif
015906150918     c     vidfi2        chain     azorg01l
015907150918     c                   if        %found(azorg01l)
015908150918     c                   movel     orgdes        vidfi2d
015909150918     c                   endif
015910150916     c                   else
015911150916     C                   seton                                        842899
015912150916     C                   eval      $msg = 'Filiale addebito obbligatoria'
015913151105     c  n77              leavesr
016000150914     c                   endif
016001151211     c                   if        §tcecon = 'S' and vidfi1 <> vidfi2
016002151211     c                   seton                                        842899
016003151211     c                   eval      $msg = 'Se la causale è una stima le filiali-
016004151211     c                              richiedente e addebito devono essere uguali'
016005151211     c  n77              leavesr
016006151211     c                   endif
016007150921     c* importo
016008150916     c                   if        vidimp > *zero
016009150916     c                   if        vidimp > §tceimp
016010150916     C                   seton                                        812899
016011150916     C                   eval      $msg = 'Importo superiore al massimo -
016012150916     c                             consentito'
016013151105     c  n77              leavesr
016014150916     c                   endif
016015150916     c                   else
016016150916     C                   seton                                        812899
016017150916     C                   eval      $msg = 'Importo obbligatorio'
016018151105     c  n77              leavesr
016019150916     c                   endif
016100150914     c* codice cliente
016101151007     c                   if        vidks2 =  *zero
016102150917     c                   move      *blank        vidks2
016103150917     c                   endif
016104150917      *
016200150917     c                   if        vidks2 <> *blank
016201150921      * testo se ammesso
016202150921     c                   if        §tccli <> 'C'
016203150921     C                   seton                                        852899
016204150921     C                   eval      $msg = 'Cliente non valido con la causale'
016205151105     c  n77              leavesr
016206150921     c                   endif
016207150921      * verifico il codice
016300150914     c                   move      vidks2        chkks2            7
016400150914     c                   exsr      dectib
016401151029     C     O69ERR        IFNE      '1'
016402151029     C                   MOVEL     ACORAG        vidks2d
016403151211     c                   if        acoabl = '8'
016404151211     C                   eval      $msg = 'Attenzione Cliente bloccato '
016405151211     C                   seton                                        28
016406151211     c                   endif
016407151029     C                   ELSE
016408151029     C                   seton                                        852899
016409151029     C                   eval      $msg = 'Cliente Inesistente  '
016410151029     C                   MOVEL     *ALL'*'       vidks2d
016411151105     c  n77              leavesr
016412151029     C                   ENDIF
016413150921     c                   move      *zeros        wpdcksc
016414150921     c                   move      chkks2        wpdcksc
016419150916     c                   else
016420150921     c                   if        §tccli = 'C'
016421150916     C                   seton                                        852899
016422150916     C                   eval      $msg = 'Cliente obbligatorio '
016423151105     c  n77              leavesr
016424150916     c                   endif
016500150914     c                   endif
016600150727      *
016700011130     C                   ENDDO
016800000000     C                   ENDSR
016900011130     C******************************************************
017000011130     C* RICERCA PRIMO VIDEO
017100011130     C******************************************************
017200011130     C     RIC01         BEGSR
017300011130     c                   select
017400150529     C                   when      §fld = 'VTIPO'
017500160107     C  n70              exsr      srtipo
017600150914     C                   when      §fld = 'VIDKS2'
017700150914     c                   move      vidks2        chkks2
017701151007     c                   setoff                                       85
017800150914     C                   exsr      srclie
017900011130     C                   ENDsl
018000011130     C* POSIZIONAMENTO CURSORE
018100150930     C*                  Z-ADD     §RIGA         §RIG
018200150930     C*                  Z-ADD     §COLON        §COL
018201150930     C                   Z-ADD     0             §RIG
018202150930     C                   Z-ADD     0             §COL
018300011130     C                   ENDSR
018400011130     c**********************************************************************
018500150529     C** tipo registrazione
018600011130     c**********************************************************************
018700150529     C     srtipo        BEGSR
019901151103     c                   clear                   vtipor
019902151103     c                   call      'TRTBTCER'
019903151103     c                   parm      'F'           tipo              1
019904151103     c                   parm                    vtipor            3
019906151105     c                   if        vtipor <> *blank
019907151105     c                   movel     vtipor        vtipo
019908151103     c                   exsr      srtab
019913151103     c                   endif
020000011130     c*
020100011130     C                   ENDSR
020200011130     C**********************************************************************
020300150529     C** Controlli e decodifiche campi con tntbe00f
020400011130     C**********************************************************************
020500011130     C     srtab         BEGSR
020600150807     c                   clear                   tibs02ds
020800150807     c                   if        vtipo <> *blank
020900150807     c                   eval      t02mod = 'C'
021000150807     c                   eval      t02ke1 = vtipo
021100150807     c                   endif
021200150807     c                   eval      t02sif = knsif
021300150807     c                   eval      t02cod = 'TCE'
021400150807     c                   call      'TIBS02R'
021500150807     c                   parm                    kpjba
021600150807     c                   parm                    tibs02ds
021601151103     c                   eval      dtce = t02uni
021602151103     C                   MOVEL     §TCEDES       dtipo
021603151103     C                   MOVEL     §TCrec        vidrec
021604151103     C                   MOVEL     §TCadd        viddel
021605151103     C                   MOVEL     §TCtsr        vidtsr
021700011130     c*
021800011130     C                   ENDSR
021801150916     C**********************************************************************
021802150916     C** richiamo  routine ricerca clienti
021803150916     C**********************************************************************
021804150916     C     srclie        BEGSR
021805150916     C* RICHIESTO ELENCO CLIENTI
021806150916     C                   CLEAR                   DSXC
021807150916     C                   Z-ADD     DUTKCI        DXCCAP
021808150916     C                   CALL      'XCLIR'
021809150916     C                   PARM                    DSXC
021810150916     C* SCOMPATTA
021811150916     C                   MOVEA     DXCKSC        KSA
021812150916     C     1             DO        30            J                 3 0
021813150916     C                   MOVE      KSA(J)        DSKSA
021814150916     C                   Z-ADD     DSKSN         KSC(J)
021815150916     C                   ENDDO
021816150916     C                   IF        KSC(1) > *ZEROS
021817150916     C                   MOVE(P)   KSC(1)        CHKKS2
021818150923     C                   MOVE(P)   KSC(1)        vidKS2
021819150916     C                   ELSE
021820150916     C                   MOVEL     *BLANKS       CHKKS2
021821150916     C                   ENDIF
021822150916     C*
021823150916     C* CODICE CLIENTE NUMERICO
021824150916     C     CHKKS2        IFLT      *ZEROS
021825150916     C                   seton                                        859928
021826150916     C                   ELSE
021827150916     C*
021828150916     C* SOLO CARATTERI NUMERICI
021829150916     C   29              MOVE      *all'0'       CHKKS2
021830150916     C   29              MOVE      VIDKS2        CHKKS2
021831150916     C                   SETOFF                                       29
021832150916     C                   TESTN                   CHKKS2               29
021833150916     C     *IN29         IFEQ      *OFF                                         *NON E'NUMERO
021834150916     C                   seton                                        859928
021835150916     C                   ELSE                                                   *E'NUMERO DA TESTN
021836150916     C                   MOVE      CHKKS2        W001              1
021837150916     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
021838150916     C                   seton                                        859928
021839150916     C                   ELSE                                                   *NUMERO
021840150916     C*
021841150916     C* CONTROLLA ESISTENZA CLIENTE
021842150916     C                   EXSR      DECTIB
021843150916     C                   ENDIF
021844150916     C                   ENDIF
021845150916     C                   ENDIF
021846150916     c   85              eval      $msg = 'Cliente errato o non in gestione'
021856150916     C*
021857150916     C                   ENDSR
021858150916     C*------------------------------------------------------------------------*
021859150916     C* dectib - decodifica ragione sociale per vedere se compreso in scansione
021860150916     C*------------------------------------------------------------------------*
021861150916     C     dectib        BEGSR
021862150916     C                   CLEAR                   tibs69ds
021863150916     C                   CLEAR                   ds_cnaco
021864150916     C                   MOVEL     KNSIF         I69SIF
021865150916     C                   MOVE      chkks2        I69KAC
021866150916     C                   CALL      'TIBS69R'
021867150916     C                   PARM                    tibs69ds
021868150916     C                   PARM                    DS_cnaco
021869150916     C                   PARM                    DS_cnind
021870150916     C                   PARM                    DS_cnclp
021871150916     C                   PARM                    DS_fncls
021877150916     C                   endsr
021878150916     c*
021879150921     C*------------------------------------------------------------------------*
021880150921     C* Richpdc- richiesta codice su piano dei conti
021881150921     C*------------------------------------------------------------------------*
021882150921     C     RichPdc       BEGSR
021883150921     C                   movel     kpjbu         savkpjbu
021884150921     C                   CLEAR                   ana330ds
021886150921     C                   MOVE      chkks2        ksc330
021887151123     C     ' ':'0'       XLATE     KSC330        ksc330
021888150921     c                   movel     ana330ds      kpjbu
021889150921     C                   CALL      'YCO102R'
021890150921     C                   PARM                    kpjba
021891151123     C                   movel     kpjbu         ANA330DS
021892150921     C                   movel     savkpjbu      kpjbu
021893151123     C                   IF        ERR330 <> '0'
021894151123     C                   SETON                                        2899
021895151123     C                   EVAL      $MSG = 'Inserimento impossibile cliente non -
021896151123     c                             inserito nel piano dei conti'
021897151123     C                   ENDIF
021899150921     C                   endsr
021900150916     C**********************************************************************
021901150916     C** Controlli e decodifiche campi con tntbe00f
021902150916     C**********************************************************************
021903150916     C     imposta       BEGSR
021904150916     c     parrich       chain     fntna01l
021905150916     c                   if        %found(fntna01l)
021906150916     c                   z-add     parrich       vidric
021907150916     c                   z-add     TNAFILA       vidFI1
021908150916     c                   z-add     TNAFILD       vidFI2
021910150916     c                   move      TNACAUS       vtipo
021911151007     c                   move      TNAksc        vidks2
021912150916     c                   move      TNADTAc       dtaeur
021913150916     c                   move      dtaeur        viddt1
021914150917     c                   move      TNANOTA       vidNOT
021915150916     c                   z-add     TNAIMP        vidIMP
021916150929     c                   move      TNAann        vidann
021917150929      *annullato
021918150929     c                   if        vidann <> *blank
021919150929     c                   eval      vidannd = 'Annullato'
021920150929     c                   else
021921150929     c                   clear                   vidannd
021922150929     c                   endif
021923150929      *fase
021924150929     c                   z-add     TNAfase       vidfase
021925150924     c                   select
021926150924     c                   when      vidfase = 10
021927150924     c                   eval      vidfased  = 'Inserita'
021928150924     c                   when      vidfase = 20
021929151105     c                   eval      vidfased  = 'Contestata'
021930150924     c                   when      vidfase = 30
021931151028     c                   eval      vidfased  = 'Verificata'
021932150924     c                   when      vidfase = 100
021933150924     c                   eval      vidfased  = 'Confermata'
021934150924     c                   endsl
021935151006     c                   endif
021965150916     C                   ENDSR
021966150916     C**********************************************************************
021967150918     C** scrittura file
021968150916     C**********************************************************************
021969150918     C     scrive        BEGSR
021970150918     c                   exsr      repnum
021971150916     c                   z-add     vidFI2        TNAFILD
021972150916     c                   move      dtacor        tnadtaf
021973150917     c                   move      vidNOT        TNANOTA
021974150916     c                   z-add     vidIMP        TNAIMP
021975150916     c                   move      vidTSR        TNATSR
021976150916     c                   move      vidks2        TNAKSC
021977151029     c                   move      vidrec        TNARECUP
021978150917     c                   move      viddel        TNAADDEB
021979150918     c                   z-add     10            tnafase
021980150917     c                   z-add     vidric        tnanric
021981150917     c                   z-add     vidFI1        TNAFILA
021982150917     c                   move      vtipo         TNACAUS
021983150917     c                   move      viddt1g       TNADTAc
021984150917     c                   move      knmus         TNAuten
021985151029      *testa se esiste cliente sul piano dei conti
021986151029     c                   if        vidks2 <> *blank
021987151029     c     kpdc          chain     anpdc01l
021988151029     c                   if        not %found(anpdc01l)
021989151029     C                   EXSR      RichPdc
021990151123     c   99              leavesr
021991151029     c                   endif
021992151029     c                   endif
021993150917     c                   write     fntna000
021994150918     c                   exsr      storicofasi
021995150923     c                   if        §tcece = 'S'
021996151211     c                   z-add     tnanric       rnanric
021997151211     c                   move      tnadtac       rnadtac
021998151211     c                   move      tnatsr        rnatsr
021999151211     c                   if        §tcecon = 'N'
022000150923      * scrittura record con importo negativo su filiale richiedente
022001150923     c                   eval      rnaimp = (tnaimp * -1)
022002150923     c                   move      tnafila       rnafil
022003150923     c                   write     fnrna000
022004151211     c                   endif
022005150923      * scrittura record con importo positivo su filiale addebito
022006150923     c                   if        §tctpa = 'C'
022007150923     c                   eval      rnaimp = tnaimp
022008150923     c                   move      tnafild       rnafil
022009150923     c                   write     fnrna000
022010150923     c                   endif
022011150923     c                   endif
022012150916     C                   endsr
022013150918     C**********************************************************************
022014150918     C** Conferma dati scrittura o aggiornamento
022015150918     C**********************************************************************
022016150918     C     aggiorna      BEGSR
022017150923      *verifico se variato l'importo nel caso aggiorno il dettaglio legato
022018150923      * perchè è lunico campo che può essere variato
022019150923     c                   if        vidimp <> tnaimp
022020150923     c                   exsr      AggDetta
022021150923     c                   endif
022022150923      *
022023150918     c                   z-add     vidFI2        TNAFILD
022024150918     c                   move      dtacor        tnadtaf
022025150918     c                   move      vidNOT        TNANOTA
022026150918     c                   z-add     vidIMP        TNAIMP
022027150918     c                   move      vidTSR        TNATSR
022028150918     c                   move      vidks2        TNAKSC
022029150918     c                   move      vidrec        TNARECUP
022030150918     c                   move      viddel        TNAADDEB
022031151123     c                   if        vidks2 <> *blank
022032151123     c     kpdc          chain     anpdc01l
022033151123     c                   if        not %found(anpdc01l)
022034151123     C                   EXSR      RichPdc
022035151123     c   99              leavesr
022036151123     c                   endif
022037151123     c                   endif
022038150918     c                   select
022039151105     c                   when      paropz = 'C'
022040150918     c                   z-add     20            tnafase
022041150918     c                   exsr      storicofasi
022042151029     c                   when      paropz = 'V'
022043150918     c                   z-add     30            tnafase
022044150918     c                   exsr      storicofasi
022045150922     c                   when      paropz = '4'
022046150925     c                   move      'A'           tnaann
022047151202     c                   exsr      Anndetta
022048151006     c                   if        tnafase = 10
022049151006     c                   move      knmus         tnauten
022050151006     c                   endif
022051151006     c                   when      paropz = '2'
022052151006     c                   move      knmus         tnauten
022053150918     c                   endsl
022054150918     c                   update    fntna000
022055150918     C                   endsr
022056150923      *_________________________________________________________
022057150923     c     AggDetta      begsr
022058150923      *_________________________________________________________
022059150923     c                   if        §tcece = 'S'
022060150923     c                   z-add     tnanric       rnanric
022061150923     c                   move      tnafila       rnafil
022062150923     c     krna          chain     fnrna02l
022063150923     c                   if        %found(fnrna02l)
022064150923     c                   eval      rnaimp = (vidimp * -1)
022065150923     c                   update    fnrna000
022066150923     c                   endif
022067150923      * scrittura record con importo positivo su filiale addebito
022068150923     c                   if        §tctpa = 'C'
022069150923     c                   z-add     tnanric       rnanric
022070150923     c                   move      tnafild       rnafil
022071150923     c     krna          chain     fnrna02l
022072150923     c                   if        %found(fnrna02l)
022073150923     c                   eval      rnaimp = vidimp
022074150923     c                   update    fnrna000
022075150923     c                   endif
022076150923     c                   endif
022077150923     c                   endif
022078150923     C                   endsr
022079151202      *_________________________________________________________
022080151202     c     AnnDetta      begsr
022081151202      *_________________________________________________________
022082151202     c     tnanric       setll     fnrna02l
022083151202     c                   do        *hival
022084151202     c     tnanric       reade     fnrna02l
022085151202     c                   if        %eof(fnrna02l)
022086151202     c                   leave
022087151202     c                   endif
022088151202     c                   move      'A'           rnaann
022089151202     c                   update    fnrna000
022090151202     c                   enddo
022102151202     C                   endsr
022103150917     C**********************************************************************
022104150917     C** inserisce lo storico delle fasi
022105150917     C**********************************************************************
022106150917     C     storicofasi   BEGSR
022107150918     c                   z-add     tNANRIC       FNANRIC
022108151105     c                   if        paropz = 'C'
022109150918     c                   move      tNAFILd       FNAFIL
022110150918     c                   else
022111150918     c                   move      tNAFILa       FNAFIL
022112150918     c                   endif
022113150918     c                   move      tNAFASE       FNAFASE
022114150918     c                   move      dtacor        FNADTAAG
022115150918     c                   z-add     oracor        FNAORAAG
022116150918     c                   move      tNANOTA       FNANOTA
022117150918     c                   move      knmus         FNAUTE
022118150918     c                   write     fnfna000
022119150917     C                   endsr
022120150916     c**********************************************************************
022121150916     c* reperimento numero viaggio
022122150916     c**********************************************************************
022123150916     C     repnum        BEGSR
022124150916     c                   clear                   trul33ds
022125150916     c                   move      'L'           i33tla
022126150916     c                   z-add     90            i33cnu
022127150916     c                   z-add     1             i33num
022128150916     c                   movel     trul33ds      kpjbu
022129150916     c                   call      'TRUL33R'
022130150916     c                   PARM                    kpjba
022131150916     c                   movel     kpjbu         trul33ds
022132150916     c                   z-add     o33nrf        vidric
022133150916     C                   ENDSR
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
029704151202     c                   if        paropz = 'C'
029705151202     c                   seton                                        77
029706151202     c                   endif
029708151005     c     paropz        comp      ' '                                    76
029800150529     C                   EXSR      REPDATIUTE
029801150924     c                   eval      vidopz = 'Inserimento'
029802150924     c                   select
029803150924     c                   when      paropz = '2'
029804150924     c                   eval      vidopz = 'Modifica'
029805150924     c                   when      paropz = '4'
029806150924     c                   eval      vidopz = 'Annulla'
029807151105     c                   when      paropz = 'C'
029808151105     c                   eval      vidopz = 'Contesta'
029809151028     c                   when      paropz = 'V'
029810151028     c                   eval      vidopz = 'Verifica'
029811150924     c                   when      paropz = '5'
029812150924     c                   eval      vidopz = 'Visualizza'
029813150924     c                   endsl
029900150727     C* POSIZIONAMENTO TABELLE                             *
030000150727     C     KTbe          KLIST                                                  *
030100150727     C                   KFLD                    TBECOD                         *
030200150727     C                   KFLD                    TBEKE1                         *
030300150727     C                   KFLD                    TBEKE2                         *
030301150923     C* file dettaglio addebiti                            *
030302150923     C     Krna          KLIST                                                  *
030303150923     C                   KFLD                    rnanric                        *
030304150923     C                   KFLD                    rnafil                         *
030305150921     C* Piano dei conti                                    *
030306150921     C     KPdc          KLIST                                                  *
030307150921     C                   KFLD                    wPDCSOC           3             *
030308150921     C                   KFLD                    wPDCKCC           6             *
030309150921     C                   KFLD                    wPDCKSC           8             *
030310150921     C                   move      '201'         wPDCSOC
030311150921     C                   move      '008888'      wPDCKCC
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
036700150811      * errore o non abilitato (impossibile xchè controllato nel filtro)
036800151028if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
036801151028     c                             §Uteadd = 'NO'
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
