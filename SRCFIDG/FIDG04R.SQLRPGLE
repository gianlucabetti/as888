000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000400120723      *----------------------------------------------------------------------------
000500171219     Ffidg04d   CF   E             WORKSTN
000600080930     f                                     sfile(video2:snrr1)
000800030701     Fazorg01l  iF   E           k disk
000801171222     Ffiapd01l  iF   E           k disk
002000011228      *
002001171219     ffipdc03t  uf a e           k disk
002002171219     ffipdd01t  uf a e           k disk
002003171222     ffipdo01t  uf a e           k disk
002004171219     ffipdc60t  uf a e           k disk
002005171219      *inserimenti
002006171219     ffdidc00t  if   e             disk
002007171219     ffdidd00t  if   e             disk
002008171219     ffdido00t  if   e             disk
002009171219     ffdidc60t  if   e             disk
002010180103      *updateioni
002011171219     ffdudc00t  if   e             disk
002012171219     ffdudd00t  if   e             disk
002013171219     ffdudo00t  if   e             disk
002015171219      *_______________________________________________________________________________
002016171221     D fipdcds       E DS                  EXTNAME(fipdc00t)
002017171221     D fipddds       E DS                  EXTNAME(fipdd00t)
002018171221     D fipdods       E DS                  EXTNAME(fipdo00t)
002019171221     D fipdc6ds      E DS                  EXTNAME(fipdc60t)
002020171221      *
002021171221     D fdidcds       E DS                  EXTNAME(fdidc00t)
002022171221     D fdiddds       E DS                  EXTNAME(fdidd00t)
002023171221     D fdidods       E DS                  EXTNAME(fdido00t)
002024171221     D fdidc6ds      E DS                  EXTNAME(fdidc60t)
002025171221      *
002026171222     D fdudcds       E DS                  EXTNAME(fdudc00t)
002027171222     D fduddds       E DS                  EXTNAME(fdudd00t)
002028171222     D fdudods       E DS                  EXTNAME(fdudo00t)
002030171221      *_____________________________________________________________
002100020103     D Psds           SDS
002200020103     D  PgmName          *PROC
002300011228     D Kpjba         E DS
002400011228      *
002500030721     D fnlv24ds      E DS
002600030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002700030604     D CNCR80        E DS
002800131003     D tibs42ds      E DS
002900131003     D uteautds      E DS
003500030617     d Tibs36Ds      e ds
003600090402     d dapdflr       e ds
003900011227     d data            ds            10
004000011228     d aa                      3      4
004100011228     d mm                      6      7
004200011228     d gg                      9     10
004300011228      *
004301180109     D Savidrow        S                   like(pdcidrow)
004302180108     D giorno          S              1  0
004304180108     D si              S              1
004700030604     D x               S              4  0
004800011227     D WrkEofS01       S              1
004900011227     D WrkCarS01       S              1
005000011228     D $VIDEO          S             10
005100030604     D snrr1           S              5i 0
005600030721     D Savkpjbu        S                   like(kpjbu)
005700080930     D SavOpzione      S                   like(v1csce)
006000030606     d dataiso         s               d   datfmt(*iso)
006100030606     d dataeur         s               d   datfmt(*eur)
006101180111     d data60          s               d   datfmt(*iso)
006102171221     d tmp             s             17
006103180130     d dat             s             17
006104171221     d wpdr            s              7  0
006105171222     d wdata           s              8  0
006106171222     d wdist           s              6  0 inz(99999)
006300030613     D WLBDA8          DS
006400030613     D  G02DAT                 1      8  0
006500030613     D  G02INV                 9     16  0
006600030613     D  G02ERR                17     17
006700030613     D  G02TGI                18     22  0
006701171219     c*
006702171219     d trul06ds      e ds
006703171222     d  lint                   1     90  0 dim(30)
009600030604      *---------------- Gestione formato video video01----------------------
010200131014     C                   EXSR      Inzvideo1
010400030604     C                   DOU       $Video <> 'VIDEO1'
010500000608      *----------- Visualizzo dati di output in caso di errori.-------------
010600000608     C                   IF        *IN99
010700000608     C                   EVAL      *IN99 = *OFF
010800030604     C                   WRITE     video1
010900000608     C                   EVAL      *IN99 = *ON
011000000608     C                   ENDIF
011100000608      *
011200030702     c     emetti        tag
011300030604     C                   EXFMT     video1
011301180104     c                   setoff                                       289684
011302180104     c                   clear                   $msg
011400000608      *
011500011227     C                   SELECT
011600030612     C                   WHEN      *INKC
011900011227     C                   EVAL      $Video = *BLANKS
012000011227
012100171219     C                   WHEN      *INKs
012200171219     C                   setoff                                       22
012201171219     c                   iter
012202180109      * inserimento da prima videata ora eliminato per nuova finestra
012300180109     C*m                 WHEN      *INKj
012301180109     C*m                 EXSR      Chkvideo1
012400180109     C*m                 EXSR      Inserimento
012500011227     C                   OTHER
012600030604     C                   EXSR      Chkvideo1
012700040728     c   84              goto      emetti
012800180103     C                   EXSR      Wrkvideo2c
012900011227     C                   ENDSL
013000011227      *
013100011227     C                   ENDDO
013200011227      *
013300011227     C                   EXSR      Uscita
013400011227     c**********************************************************************
013500011227     c* uscita
013600011227     c**********************************************************************
013700011227     C     Uscita        BEGSR
013800011227      *
013900011227     C                   EVAL      *INLR = *ON
014000011227     C                   RETURN
014100011227      *
014200011227     C                   ENDSR
014300011227     c**********************************************************************
014400011227     c* gestione sfl
014500011227     c**********************************************************************
014600030604     C     Wrkvideo2c    BEGSR
014700000627      *
014800011227     C                   EVAL      WrkCarS01 = *ON
014900030604     C                   EVAL      $Video = 'VIDEO2C'
015000011227      *
015100030604     C                   DOU       $Video <> 'VIDEO2C'
015200030604     C                   EXSR      Carvideo2
015300011228     c*Descrizione della ricerca
015400000627      *
015500030604     C                   WRITE     video2z
015600030604     C                   EXFMT     video2c
015700090429     c                   setoff                                       2896
015800090429     c                   clear                   $msg
015900011227     C                   SELECT
016300011228     c* guida
016400011227     C                   WHEN      *INKL=*ON
016500030604     C                   EVAL      $VIDEO='VIDEO1'
016501180103      *
016801180103     C                   WHEN      *INKF = *ON
016802180103     C                   EXSR      GestioneSFL
016900011227
016901180109     C                   WHEN      *INKj
016902180109     c                   do        *hival
016903180109     c                   exfmt     video3w
016904180109     c                   setoff                                       2984
016905180109     c   kl              leave
016906180109     C                   EXSR      Chkvideo3
016907180109     c   84              iter
016908180109      * per conferma inserimento esce dalla window e ricarica
016909180109     c                   if        *inkf
016910180109     C                   EXSR      Inserimento
016911180109     c   84              iter
016912180109     C                   EVAL      WrkCarS01 = *ON
016913180109     c                   leave
016914180109     c                   endif
016915180109     c                   enddo
016916180109      *
017300011227     C                   ENDSL
017400011228
017500011227     C                   ENDDO
017600031027     C                   ENDSR
017700011227     c**********************************************************************
017800011227     c* Controlli video R01.
017900011227     c**********************************************************************
018000030604     C     Chkvideo1     BEGSR
018100000609      *
018200000608     C                   EVAL      *IN99 = *OFF
018300040728     C                   EVAL      *IN84 = *OFF
018301171219     C                   EVAL      *IN83 = *OFF
018400081009     C                   EVAL      *IN40 = *OFF
018401171219      *verifico se filiale in gestione
018402171219     C     Vidfil        LOOKUP    Lint                                   80
018403171219     c                   if        not *in80
018404171219     c                   seton                                        8384
018405171219     c                   leavesr
018406171219     c                   endif
018407171219      *
018500080930     c     vidfil        chain     azorg01l
018600080930     c                   movel     orgdes        vidfild
020200080930     C****  CODICE PADRONCINO  ****
020300080930     C     '?'           SCAN      vidpdr                                 55
020400080930     C*
020500080930     C     *IN55         IFEQ      *ON
020600080930     C                   MOVEL     KPJBU         SAVKPJBU
020700080930     C                   Z-ADD     Vidfil        d24FIL
020800080930     C                   Z-ADD     0             d24pdr
020900080930     C                   MOVEL     'R'           d24FLG
021000180103     C                   MOVEL     'A'           d24tip
021100080930     C                   MOVEL     *BLANKS       d24rsc
021200080930     C                   MOVEL(p)  fnlv24ds      KPJBU
021300080930     C                   CALL      'FNLV24R'
021400080930     C                   PARM                    KPJBA
021500080930     C                   MOVEL     KPJBU         fnlv24ds
021600080930     C                   MOVEL     savKPJBU      KPJBU
021700080930     C*
021800080930     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
021900080930     C     d24pdr        IFNE      0
022000080930     C                   MOVEL     d24pdr        Vidfil
022100080930     C                   MOVE      d24pdr        Vidpdr
022200080930     C                   MOVEL     d24rsc        despdr
022300080930     c                   else
022400080930     C                   clear                   Vidpdr
022500171222     C                   clear                   wpdr
022600080930     C                   clear                   despdr
022700081009     c                   seton                                        4084
022800080930     C                   ENDIF
022900080930     C*
023000080930     C                   ENDIF
023001171222      * inserimento autista obbligatorio
023002171222     c                   if        *inkj and vidfil <> *zeros and
023101171222     c                             (vidpdr = *blank  or vidpdr = '0000')
023400081009     c                   seton                                        4084
023500081009     c                   endif
023501171222      *
023600080930     c                   if        vidfil  <> *zeros and vidpdr <> *blank
023700080930     c                             and vidpdr > '0000'
023800171222     c                   movel     vidfil        wpdr
023900171222     c                   move      vidpdr        wpdr
024000081001     c                   clear                   despdr
024100081001     c     kapd          chain     fiapd01l
024200090720     c                   if        %found(fiapd01l)
024300171219     c                   if        apdatb = *blank
024500171222     c                   movel     apdrsc        despdr
024600081001     c                   else
024700081009     c                   seton                                        4084
024800081001     c                   endif
024900091109     c                   else
025000091109     c                   seton                                        4084
025100090720     c                   endif
025200080930     c                   endif
025300000608      *
025400000608     C                   ENDSR
025401180109     c**********************************************************************
025402180109     c* Controlli video R03.
025403180109     c**********************************************************************
025404180109     C     Chkvideo3     BEGSR
025405180109      *
025407180109     C                   EVAL      *IN84 = *OFF
025409180109     C                   EVAL      *IN23 = *OFF
025419180109     C****  CODICE PADRONCINO  ****
025420180109     C     '?'           SCAN      vidpdr3                                55
025421180109     C*
025422180109     C     *IN55         IFEQ      *ON
025423180109     C                   MOVEL     KPJBU         SAVKPJBU
025424180109     C                   Z-ADD     Vidfil        d24FIL
025425180109     C                   Z-ADD     0             d24pdr
025426180109     C                   MOVEL     'R'           d24FLG
025427180109     C                   MOVEL     'A'           d24tip
025428180109     C                   MOVEL     *BLANKS       d24rsc
025429180109     C                   MOVEL(p)  fnlv24ds      KPJBU
025430180109     C                   CALL      'FNLV24R'
025431180109     C                   PARM                    KPJBA
025432180109     C                   MOVEL     KPJBU         fnlv24ds
025433180109     C                   MOVEL     savKPJBU      KPJBU
025434180109     C*
025435180109     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
025436180109     C     d24pdr        IFNE      0
025437180109     C                   MOVEL     d24pdr        Vidfil
025438180109     C                   MOVE      d24pdr        Vidpdr3
025439180109     C                   MOVEL     d24rsc        despdr3
025440180109     c                   else
025441180109     C                   clear                   Vidpdr3
025442180109     C                   clear                   wpdr
025443180109     C                   clear                   despdr3
025444180109     c                   eval      $msgw = 'Codice autista non valido'
025445180109     c                   leavesr
025446180109     c                   seton                                        2984
025447180109     C                   ENDIF
025448180109     C*
025449180109     C                   ENDIF
025450180109      * inserimento autista obbligatorio
025451180109     c                   if        *inkj and vidfil <> *zeros and
025452180109     c                             (vidpdr3 = *blank  or vidpdr3 = '0000')
025453180109     c                   eval      $msgw = 'Codice autista obbligatorio'
025454180109     c                   seton                                        2984
025455180109     c                   leavesr
025456180109     c                   endif
025457180109      *
025458180109     c                   if        vidfil  <> *zeros and vidpdr3<> *blank
025459180109     c                             and vidpdr3> '0000'
025460180109     c                   movel     vidfil        wpdr
025461180109     c                   move      vidpdr3       wpdr
025462180109     c                   clear                   despdr3
025463180109     c     kapd          chain     fiapd01l
025464180109     c                   if        %found(fiapd01l)
025465180109     c                   if        apdatb = *blank
025466180109     c                   movel     apdrsc        despdr3
025467180109     c                   else
025468180109     c                   eval      $msgw = 'Codice autista non valido'
025469180109     c                   seton                                        2984
025470180109     c                   leavesr
025472180109     c                   endif
025473180109     c                   else
025474180109     c                   eval      $msgw = 'Codice autista obbligatorio'
025475180109     c                   seton                                        2984
025476180109     c                   leavesr
025478180109     c                   endif
025479180109     c                   endif
025480180109      *
025481180109     C                   ENDSR
025500011227     c**********************************************************************
025600011227     c* inizializza r01
025700011227     c**********************************************************************
025800030604     C     Inzvideo1     BEGSR
025900000608
026000080930     c                   move      o36pou        vidfil
026001180105     c                   eval      $video = 'VIDEO1'
026004180104     c                   seton                                        22
026100030604     C                   EXSR      Chkvideo1
026200000609
026300000608     C                   ENDSR
026400030604     c**********************************************************************
026500030605     C     opzione       BEGSR
026600030605     c**********************************************************************
026700171222     c                   move      v1csce        modalita          1
027900030605     c                   select
028200171219     c                   when      v1csce = '2'
028300171219     c                   exsr      Variazioni
029400171219     c                   when      v1csce = '4'
029600171219     c                   exsr      Elimina
029700081013      *non spengo indicatore se sono in interrogazione
030600030605     c                   endsl
030700030604     C                   ENDSR
030701171219     c**********************************************************************
030702171219     C     inserimento   BEGSR
030703171219     c**********************************************************************
030704171221     c     kpdc3         chain     fipdc03t
030705171221     c                   if        %found(fipdc03t)
030706180109     c                   seton                                        2984
030707180109     c                   eval      $msgw = 'Esistono già i dati Demo'
030709171221     c                   leavesr
030710171221     c                   endif
030711171222     c                   move      '1'           modalita
030712171221      *scrittura FIPDC
030713171221     c     *start        setll     fdidc00t
030714171221     c                   do        *hival
030715171221     c                   read      fdidc00t
030716171221     c                   if        %eof(fdidc00t)
030717171221     c                   leave
030718171221     c                   endif
030719171221     c                   eval      fipdcds = fdidcds
030720171221     c                   eval      tmp= %char(%timestamp: *iso0)
030721171222     c                   eval      PDcDATORA =   tmp
030722171221     c                   eval      pdccodaut = wpdr
030723171222     c                   eval      pdcnsp    = wpdr
030724180208     C                   IF        pdcnor <> 0
030725180208     c                   eval      pdcnor    = wpdr
030726180208     c                   endif
030727171221     c                   eval      pdcfgs    = vidfil
030728171221     c                   write     fipdc000
030729171221     c                   enddo
030730171221      *scrittura FIPDD
030731171221     c     *start        setll     fdidd00t
030732171221     c                   do        *hival
030733171221     c                   read      fdidd00t
030734171221     c                   if        %eof(fdidd00t)
030735171221     c                   leave
030736171221     c                   endif
030737171221     c                   eval      fipddds = fdiddds
030738171221     c                   eval      tmp= %char(%timestamp: *iso0)
030739171221     c                   eval      PDdDATORA =   tmp
030740171221     c                   eval      pddcodaut = wpdr
030741171221     c                   eval      pddfgs    = vidfil
030742171222     c                   eval      pddnsp    = wpdr
030743180103     c                   write     fipdd000
030744171221     c                   enddo
030745171221      *scrittura FIPDO
030746171221     c     *start        setll     fdido00t
030747171221     c                   do        *hival
030748171221     c                   read      fdido00t
030749171221     c                   if        %eof(fdido00t)
030750171221     c                   leave
030751171221     c                   endif
030752171221     c                   eval      fipdods = fdidods
030753171221     c                   eval      tmp= %char(%timestamp: *iso0)
030754171222     c                   eval      PDoDATORA =   tmp
030755171221     c                   eval      pdocodaut = wpdr
030756171221     c                   eval      pdofgs    = vidfil
030757171222     c                   eval      pdonor    = wpdr
030758180103     c                   write     fipdo000
030759171221     c                   enddo
030760171221      *scrittura FIPDC60
030761180108      * ricavo prima giorno della settimana da impostare per giro
030762180108     C/EXEC SQL
030763180111     C+ SET :giorno = DayOfWeek(:data60)
030764180108     C/END-EXEC
030765171221     c     *start        setll     fdidc60t
030766171221     c                   do        *hival
030767171221     c                   read      fdidc60t
030768171221     c                   if        %eof(fdidc60t)
030769171221     c                   leave
030770171221     c                   endif
030771171221     c                   eval      fipdc6ds = fdidc6ds
030772171221     c                   eval      tmp= %char(%timestamp: *iso0)
030773171221     c                   eval      pdc6pdr = wpdr
030774180108     c                   eval      pdc6gg  = giorno
030775171221     c                   eval      pdc6fgsg   = vidfil
030776171221     c                   write     fipdc600
030777171221     c                   enddo
030778180109     c                   clear                   vidpdr3
030779180109     c                   clear                   despdr3
030781171219     C                   ENDSR
030782171219     c**********************************************************************
030783171219     C     variazioni    BEGSR
030784171219     c**********************************************************************
030785171222      *scrittura FIPDC
030786171222     c     *start        setll     fdudc00t
030787171222     c                   do        *hival
030788171222     c                   read      fdudc00t
030789171222     c                   if        %eof(fdudc00t)
030790171222     c                   leave
030791171222     c                   endif
030792180109     c                   clear                   savidrow
030793171222     c     kpdct         chain     fipdc03t
030794180109     c                   if        %found(fipdc03t)
030795180109     c                   eval      savidrow = pdcidrow
030796180109     c                   endif
030797180103     c                   eval      fipdcds = fdudcds
030798180103     c                   eval      tmp= %char(%timestamp: *iso0)
030799180103     c                   eval      PDcDATORA =   tmp
030800180103     c                   eval      pdccodaut = vaut
030801180103     c                   eval      pdcnsp    = vaut
030802180103     c                   eval      pdcfgs    = vidfil
030803180208     c                   if        pdcnor <> 0
030804180208     c                   eval      pdcNOR    = vaut
030805180208     C                   ENDIF
030806171222     c                   if        %found(fipdc03t)
030807180109     c                   eval      pdcidrow = savidrow + 1
030808171222     c                   update    fipdc000
030809171222     c                   else
030810171222     c                   write     fipdc000
030811171222     c                   endif
030812171222     c                   enddo
030813171222      *scrittura FIPDD
030814171222     c     *start        setll     fdudd00t
030815171222     c                   do        *hival
030816171222     c                   read      fdudd00t
030817171222     c                   if        %eof(fdudd00t)
030818171222     c                   leave
030819171222     c                   endif
030820180103     c     kpdd          chain     fipdd01t
030821171222     c                   eval      fipddds = fduddds
030822171222     c                   eval      tmp= %char(%timestamp: *iso0)
030823171222     c                   eval      PDdDATORA =   tmp
030824171222     c                   eval      pddfgs    = vidfil
030825171222     c                   eval      pddnsp    = vaut
030826180105     c                   eval      pddcodaut = vaut
030827180103     c                   if        %found(fipdd01t)
030828171222     c                   update    fipdd000
030829180103     c                   else
030830180103     c                   write     fipdd000
030831180103     c                   endif
030832171222     c                   enddo
030833171222      *scrittura FIPDO
030834180109     c                   clear                   savidrow
030835171222     c     *start        setll     fdudo00t
030836171222     c                   do        *hival
030837171222     c                   read      fdudo00t
030838171222     c                   if        %eof(fdudo00t)
030839171222     c                   leave
030840171222     c                   endif
030841180103     c     kpdo          chain     fipdo01t
030842180109     c                   if        %found(fipdo01t)
030843180109     c                   eval      savidrow = pdoidrow
030844180109     c                   endif
030845171222     c                   eval      fipdods = fdudods
030846171222     c                   eval      tmp= %char(%timestamp: *iso0)
030847171222     c                   eval      PDoDATORA =   tmp
030848171222     c                   eval      pdofgs    = vidfil
030849171222     c                   eval      pdonor    = vaut
030850180105     c                   eval      pdocodaut = vaut
030851180103     c                   if        %found(fipdo01t)
030852180109     c                   eval      pdoidrow = savidrow + 1
030853171222     c                   update    fipdo000
030854180103     c                   else
030855180103     c                   write     fipdo000
030856180103     c                   endif
030857171222     c                   enddo
030858171219     C                   ENDSR
030859030605     c**********************************************************************
030900171219     C     elimina       BEGSR
031000030605     c**********************************************************************
031001171221      *
031002171221      *eliminazione testate
031003171221     C/EXEC SQL
031004180130     C+ delete from FIpdc00t where PDCCODAUT = :vaut and pdcndc = 99999
031005171221     C/END-EXEC
031006171221      *
031007171221      *eliminazione dettagli
031008171221     C/EXEC SQL
031009180130     C+ delete from FIpdd00t where PDdCODAUT = :vaut and pddndc = 99999
031010171221     C/END-EXEC
031011171221      *
031012171221      *eliminazione ritiri
031013171221     C/EXEC SQL
031014180130     C+ delete from FIpdo00t where PDoCODAUT = :vaut and pdondc = 99999
031015171221     C/END-EXEC
031016171221      *
046500030605     C                   ENDSR
050200090324     c**********************************************************************
050300090324     C     stampa        BEGSR
050400090324     c**********************************************************************
051100090324     c
051200090324     C                   ENDSR
093900011228     c**********************************************************************
094000011228     c* carica sfl
094100011228     c**********************************************************************
094200030604     C     Carvideo2     BEGSR
094300011228     C                   IF        WrkCarS01 = *ON
094301180104     c                   clear                   vaut
094400030604     C                   EVAL      snrr1   = 0
094500011228     C                   EVAL      *IN90 = *ON
094600020219     C                   EVAL      *IN91 = *OFF
094700030604     C                   WRITE     video2c
094800011228     C                   EVAL      *IN90 = *OFF
095100171221     c     kpdc2         setll     fipdc03t
095200030701     c                   do        *hival
095300171221     c     kpdc2         reade(n)  fipdc03t                               97
095400030701     c   97              leave
095401180109     c                   clear                   vstat
095402180104      * se inserito autista completo carico solo quello
095403180104     c                   if        vidpdr > '0000' and pdccodaut <> wpdr
095404180104     c                   iter
095405180104     c                   endif
095406180104      * se inserita solo la filiale carico tutti gli autisti con dati demo
095500171221     c                   if        pdccodaut = vaut
095600080930     c                   iter
095700080930     c                   endif
095701180108      * scrittura record autista distinta
095702180108     c                   move      pdccodaut     vaut
095703180112     c                   move      pdccodaut     vauta
095704180108     c                   clear                   si
095705180109      * stato distinta
095706180108     C/EXEC SQL
095707180108     C+ SELECT '1' into :si
095708180109     C+  FROM fipdc00t WHERE pdccodaut =:vaut and  pdcidrow >= 2
095709180112     C+  and  pdcndc = 99999
095710180108     C+  FETCH first row only
095711180108     C/END-EXEC
095712180108     c                   if        si = '1'
095713180109     c                   eval      vstat = 'Variata'
095714180109     c                   else
095715180109     c                   eval      vstat = 'Creata'
095716180108     c                   endif
095717180109      *
095718180130     C/EXEC SQL
095719180130     C+ SELECT  min(pdcdatora) into :dat
095720180130     C+ FROM fipdc00t WHERE pdcndc = 99999 and pdccodaut = :vaut
095721180130     C+ FETCH first row only
095722180130     C/END-EXEC
095900180130     c                   movel     dat           wdata             8 0
096000171222     c                   move      wdata         dataiso
096001171222     c                   move      dataiso       dataeur
096002171222     c                   move      dataeur       vdatad
096003171222     c                   clear                   vautd
096004171222     c                   move      vaut          wpdr
096005171222     c     kapd          chain     fiapd01l
096006171222     c                   if        %found(fiapd01l)
096007171222     c                   movel     apdrsc        vautd
096008171222     c                   endif
100900030701      *esco per raggiunta massima capacità sfl
101000030617     c                   if        snrr1 > 9990
101100030617     c                   leave
101200030617     c                   endif
101300030617
101400030604     C                   EVAL      snrr1 = snrr1 + 1
101500030604     C                   WRITE     video2
101600011228     C                   ENDDO
101700180111      * forzatura per cancellazione
101701180112     c                   if        modalita = '4' and
101702180112     C                             snrr1  = 0
101703180112     c                   z-add     1             snrr1
101704180112     c                   clear                   video2
101705180112     C                   WRITE     video2
101706180112     c                   endif
101707180111      *
101800030604     C                   IF        snrr1  > 0
101900020219     C                   EVAL      *IN91 = *ON
102000020219     C                   MOVE      1             OK                1 0
102100020219     C                   ELSE
102200020219     C                   MOVE      0             OK                1 0
102300030701     C                   ENDIF
102400030612     C                   EVAL      nrr1  = 1
102500011228     C                   ENDIF
102600020219     C                   EVAL      WrkCarS01 = *OFF
102700030604     C                   EVAL      snrr1  = 1
102800030604     C                   EVAL      csrrrn = 1
102900011228     C                   ENDSR
103000011227     c**********************************************************************
103100011227     c* gestione sfl
103200011227     c**********************************************************************
103300000613     C     GestioneSFL   BEGSR
103400000627      *
103500000627     C                   EVAL      WrkEofS01 = *OFF
109500030605      * Elaborazione opzioni.
109600011228     c*
109700030605     c                   do        *hival        X                 4 0
109800030605     c     X             CHAIN     video2                             50
109900030605     C   50              LEAVE
110000030604     C                   IF        v1csce <> *blanks
110100030605     c                   exsr      opzione
110200030611
110300030611     c                   move      *blank        v1csce
110400030605     c                   update    video2
110500030611
110600011228     C                   ENDIF
110700030605     C                   enddo
110800040728      *
110900171219     c                   if        modalita = '2' or modalita = '4'
111000171219     c                             or modalita = '1'
111200040728     c                   eval      wrkcars01 = *on
111300040728     c                   endif
111400000627      *
111500180111     C                   ENDSR
119500011227     c**********************************************************************
119600000607     C     *INZSR        BEGSR
119700011227     c**********************************************************************
119800000607      *
119900000607     C     *ENTRY        PLIST
120000000607     C                   PARM                    Kpjba
120100030604     C                   Z-ADD     1             CODUT
120200030604     C                   CALL      'X§PARUT'
120300030604     C                   PARM                    UTEDSE
120400030604     C                   MOVEL     REC80         CNCR80
120500030703     C                   MOVEL     Ragut         rsut
120600030703     C                   MOVEL     knsif         vknsif
120700030703     C                   MOVEL     knmus         vknmus
120800030702      *reperimento data
120900030702     C                   TIME                    W0120            14 0
121000030702     C                   MOVE      W0120         WDAT              8 0
121100030702     C*
121200030702     C                   Z-ADD     WDAT          G02DAT
121300030702     C                   MOVEL     *BLANK        G02ERR
121400030702     C                   CALL      'XSRDA8'
121500030702     C                   PARM                    WLBDA8
121600030702     C* UDATE A 8 IN AAAA/MM/GG
121700030702     C                   Z-ADD     G02INV        WUDA              8 0
121701180111     c                   move      g02inv        data60
121800080930     C                   CLEAR                   Tibs36Ds
121900080930     C                   EVAL      I36ute = knmus
122000080930     C                   CALL      'TIBS36R'
122100080930     C                   PARM                    tibs36ds
122101171219      *
122102171219     c                   clear                   trul06ds
122103171219     c                   eval      d06cod = '£1'
122104171219     c                   movel     o36pou        d06key
122105171219     c                   move      'S'           d06key
122106171219     C                   Z-ADD     G02INV        d06drf
122107171219     c                   movel     trul06ds      kpjbu
122108171219     c                   call      'TRUL06R'
122109171219     c                   parm                    kpjba
122110171219     c                   movel     kpjbu         trul06ds
122111171222     c                   movel     o36pou        vidfil
122112180110      *
122113180110      **provvisorio da eliminare serve per test a 028
122114180110      *
122115180111     c*m                 if        o36pou <> 028
122116180111     c*m                 call      'XUTENOAB'
122117180111     c*m                 parm                    kpjba
122118180111     c*m                 return
122119180111     c*m                 endif
122200030718
122300030718     C     Kapd          KLIST
122400030718     C                   KFLD                    tipoa             1
122500171222     C                   KFLD                    wpdr
122600171222     C                   move      'A'           tipoa
123800090525      *
123900171222     C     Kpdc2         KLIST
124000171222     C                   KFLD                    vidfil
124100171222     C                   KFLD                    wdist
124201171222     C     Kpdc3         KLIST
124202171222     C                   KFLD                    vidfil
124203171222     C                   KFLD                    wdist
124204171222     C                   KFLD                    wpdr
124205171222     C     Kpdct         KLIST
124206171222     C                   KFLD                    vidfil
124207171222     C                   KFLD                    wdist
124208171222     C                   KFLD                    vaut
124209171222     C                   KFLD                    uDCAAS
124210171222     C                   KFLD                    uDCLNP
124211171222     C                   KFLD                    uDCNRS
124212171222     C                   KFLD                    vaut
124213171222     C     Kpdd          KLIST
124214171222     C                   KFLD                    vidfil
124215171222     C                   KFLD                    wdist
124217171222     C                   KFLD                    uDdAAS
124218171222     C                   KFLD                    uDdLNP
124219171222     C                   KFLD                    uDdNRS
124220171222     C                   KFLD                    vaut
124221171222     C                   KFLD                    uddnsc
124222171222     C     Kpdo          KLIST
124223171222     C                   KFLD                    vidfil
124224171222     C                   KFLD                    wdist
124225171222     C                   KFLD                    uDOPOE
124226171222     C                   KFLD                    uDONSR
124227171222     C                   KFLD                    vaut
124228171222     C                   KFLD                    uDONRV
129400090417      *
129500090417     C                   ENDSR
129600000607      *---------------------------------------------------------------------
