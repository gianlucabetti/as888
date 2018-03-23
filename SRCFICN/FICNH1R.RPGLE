000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200150605     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000300160108     Fficnh1d   CF   E             WORKSTN
000301160108     Fanfrn01l  IF   E           K DISK
000302160121     Fansif01l  IF   E           K DISK
000303160108     Fansog01l  IF   E           K DISK
000400150727     FTntbe01l  IF   E           K DISK
000600150529     D DTAcor          S               D   INZ
000700160111     D viddt1g         S              8s 0 INZ
000800160111     D viddt2g         S              8s 0 INZ
000900011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001000000000     D KPJBA         E DS
001001150917     d  param          ds
001002160114     d   parsoc                       3
001003160114     d   parfor                       8
001004160114     d   parcau                       3
001005160114     d   pardat                       8s 0
001006160114     d   paropz                       1
001100160108     d dsre          e ds
001101160114     d dtsr          e ds
001200160108     D ficnh1i       E DS
001201160108     D ana400ds      E DS
001202160121     D ana113ds      E DS
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
004300150914     D*------------------
004400150914     D WLBDA8          DS                  INZ
004500150914     D  G08DAT                 1      8  0
004600150914     D  G08INV                 9     16  0
004700150914     D  G08ERR                17     17
004800150914     D  G08TGI                18     22  0
004900150928     C*_______________________________________________________________
004901160112     c                   move      'N'           vcont
005000011130     C  nlr              DO        *HIVAL
005100011130     c                   EXSR      CLR00
005200160108     C                   EXFMT     ficnh0
005300011130     C* FINE LAVORO
005400011130     C   KC              LEAVE
005500011130     C* RICERCA
005501151006     c                   if        *inkd
005600151006     C                   EXSR      RIC01
005601151006     c                   iter
005602151006     c                   endif
005603160201      * controllo contabile
005604160201     c                   if        *inki
005607160201     c                   call      'FICNH4R'
005608160201     c                   parm                    kpjba
005609160201     c                   iter
005610160201     c                   endif
005611151006      * inserimento
005612151006     c                   if        *inkj
005613151006     c                   clear                   param
005614151006     c                   movel     param         kpjbu
005615160108     c                   call      'FICNH3R'
005616151006     c                   parm                    kpjba
005617151007     c                   iter
005618151006     c                   endif
005700000000     C* CONTROLLI
005800151006     C                   EXSR      CTRL01
005801150924     c   kd
005900150924     COR 99              ITER
006400160108     c                   clear                   FICNH1I
006500160108     c                   move      vtipo         CNHCAU
006600160108     c                   move      vidDt1g       CNHDAT1
006700160108     c                   move      viddt2g       CNHDAT2
007100160108     c                   move      vidFOR        CNHFOR
007200160108     c                   move      vidSOC        CNHSOC
007201160111     c                   move      vcont         CNHcont
007202160114     c                   move      vtsr          CNHtsr
007400160108     c                   movel     FICNH1I       kpjbu
007500160111     c                   call      'FICNH2R'
007600160111     c                   parm                    kpjba
007700160108     c                   movel     kpjbu         FICNH1I
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
009201160211     C                   SETOFF                                       288586
009300150914     C                   SETOFF                                       998081
009400150914     C                   SETOFF                                       828384
009600011130     C                   DO
009601160211     c* codice societa
009602160211     c                   if        vidsoc <> *blank
009603160211     C     vidsoc        CHAIN     ansif01l
009604160211     c                   if        not%found(ansif01l)
009605160211     C                   SETON                                        992883
009606160211     c                   eval      $msg = 'Società errata'
009607160211     c                   leavesr
009608160211     C                   endif
009609160211     C                   endif
009610160211     c*
009611160211     c* codice fornitore
009612160211     c                   clear                   vidford
009613160211     c                   if        vidfor <> *blank
009614160211     C     Kfrn          CHAIN     anfrn01l
009615160211     c                   if        %found(anfrn01l)
009616160211     c     frnsogg       chain     ansog01l
009617160211     c                   if        %found(ansog01l)
009618160211     c                   movel     sogdes        vidford
009619160211     c                   else
009620160211     C                   SETON                                        992884
009621160211     c                   eval      $msg = 'Fornitore non trovato'
009622160211     c                   leavesr
009623160211     c                   endif
009624160211     c                   else
009625160211     C                   SETON                                        992884
009626160211     c                   eval      $msg = 'Fornitore non trovato'
009627160211     c                   leavesr
009628160211     c                   endif
009629160211     c                   endif
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
013500160111     c                   if        viddt1g <> *zero
013600160111     c                   if        viddt2g = *zero
013700150914     c                   move      viddt1g       viddt2g
013800150914     c                   move      viddt1        viddt2
013900150914     c                   endif
013901160211     c                   endif
013902160211     c                   if        viddt1g =  *zero
013903160211     c                   if        viddt2g <> *zero
013904160211     c                   move      viddt2g       viddt1g
013905160211     c                   move      viddt2        viddt1
013906160211     c                   endif
013907160211     c                   endif
014000150914     c                   if        viddt1g > viddt2g
014100150914     C                   SETON                                        992881
014200150914     c                   eval      $msg  = 'Data dal maggiore di data al '
014400150914     c                   endif
014401160211      * CONTROLLO il tipo
014402160211     c                   if        vtipo  <> *blank
014403160211     c                   clear                   tibs02ds
014404160211     c                   eval      t02mod = 'C'
014405160211     c                   eval      t02ke1 = vtipo
014406160211     c                   exsr      srtab
014407160211     c                   if        t02err <> *blank
014408160211     C                   SETON                                        992880
014409160211     c                   eval      $msg = 'Causale non valida'
014410160211     c                   leavesr
014411160211     c                   endif
014412160211     c                   else
014413160211     c                   clear                   vtipod
014414160211     c                   endif
016501160114      * CONTROLLO il tipo servisio
016502160114     c                   if        vtsr  <> *blank
016503160114     c                   clear                   tibs02ds
016504160114     c                   eval      t02mod = 'C'
016505160114     c                   eval      t02ke1 = vtsr
016506160114     c                   exsr      srtabb
016507160114     c                   if        t02err <> *blank
016508160114     C                   SETON                                        992885
016509160114     c                   eval      $msg = 'Tipo servizio non valido'
016510160114     c                   leavesr
016511160114     c                   endif
016512160114     c                   else
016513160114     c                   clear                   vtsrd
016514160114     c                   endif
016600150727      *
016700011130     C                   ENDDO
016800000000     C                   ENDSR
016900011130     C******************************************************
017000011130     C* RICERCA PRIMO VIDEO
017100011130     C******************************************************
017200011130     C     RIC01         BEGSR
017300011130     c                   select
017304150928      * ricerca causale
017400150529     C                   when      §fld = 'VTIPO'
017500160108     C                   exsr      srtab
017501160121      * ricerca società
017502160121     C                   when      §fld = 'VIDSOC'
017503160121     C                   exsr      call113
017504150928      * ricerca cliente
017600160108     C                   when      §fld = 'VIDFOR'
017800160108     C                   exsr      call400
017801160114      * ricerca causale
017802160114     C                   when      §fld = 'VTSR'
017803160114     C                   exsr      srtabb
017900011130     C                   ENDsl
018000011130     C* POSIZIONAMENTO CURSORE
018100011130     C                   Z-ADD     §RIGA         §RIG
018200011130     C                   Z-ADD     §COLON        §COL
018300011130     C                   ENDSR
018301160114     C**********************************************************************
018302160114     C** Controlli e decodifiche campi con tntbe00f
018303160114     C**********************************************************************
018304160114     C     srtab         BEGSR
018305160114     c                   clear                   tibs02ds
018306160114     c                   eval      t02mod = 'R'
018307160114     c                   if        vtipo <> *blank
018308160114     c                   eval      t02mod = 'C'
018309160114     c                   eval      t02ke1 = vtipo
018310160114     c                   endif
018311160114     c                   eval      t02sif = knsif
018312160114     c                   eval      t02cod = 'SRE'
018313160114     c                   call      'TIBS02R'
018314160114     c                   parm                    kpjba
018315160114     c                   parm                    tibs02ds
018316160114     c                   if        t02err = *blank
018317160114     c                   Eval      dsre = T02Uni
018318160114     c                   Eval      vtipod = §sredes
018319160114     c                   Eval      vtipo  = t02ke1
018320160114     c                   endif
018321160114     c*
018322160114     C                   ENDSR
020200011130     C**********************************************************************
020300150529     C** Controlli e decodifiche campi con tntbe00f
020400011130     C**********************************************************************
020401160114     C     srtabb        BEGSR
020402160108     c                   clear                   tibs02ds
020403160108     c                   eval      t02mod = 'R'
020404160114     c                   if        vtsr <> *blank
020405160108     c                   eval      t02mod = 'C'
020406160115     c                   eval      t02ke1 = vtsr
020407160108     c                   endif
020408160108     c                   eval      t02sif = knsif
020409160114     c                   eval      t02cod = 'TSR'
020410160108     c                   call      'TIBS02R'
020411160108     c                   parm                    kpjba
020412160108     c                   parm                    tibs02ds
020413160108     c                   if        t02err = *blank
020414160114     c                   Eval      dtsr = T02Uni
020415160114     c                   Eval      vtsrd = §tsrdes
020416160114     c                   Eval      vtsr  = t02ke1
020417160108     c                   endif
020418160108     c*
021800011130     C                   ENDSR
021900150914     C**********************************************************************
022000150914     C** richiamo  routine ricerca clienti
022100150914     C**********************************************************************
022200160108     C     call400       BEGSR
022201160108     C*
022202160108     C                   RESET                   ana400ds
022204160108     C                   MOVEL     '01'          OPZ400
022205160108     C                   MOVEL     ana400ds      KPJBU
022206160108     C                   CALL      'ANA400R'
022207160108     C                   PARM                    KPJBA
022208160108     C                   MOVEL     KPJBU         ana400ds
022209160108     C     KSC400        IFNE      *BLANK
022210160108     C                   MOVEL     ksc400        vidfor
022211160108     C                   ELSE
022212160108     C                   MOVEL     *blanks       vidfor
022213160108     C                   ENDIF
022214160108     C*
026500150914     C                   ENDSR
026501160121     C**********************************************************************
026502160121     C** richiamo  routine ricerca società
026503160121     C**********************************************************************
026504160121     C     call113       BEGSR
026505160121     C*
026506160121     C                   RESET                   ana113ds
026507160121     C                   MOVEL     '01'          OPZ113
026508160121     C                   MOVEL     '1'           inz113
026509160121     C                   MOVEL     ana113ds      KPJBU
026510160121     C                   CALL      'ANA113R'
026511160121     C                   PARM                    KPJBA
026512160121     C                   MOVEL     KPJBU         ana113ds
026513160121     C     societa113    IFNE      *BLANK
026514160121     C                   MOVEL     societa113    vidsoc
026515160121     C                   ELSE
026516160121     C                   MOVEL     *blanks       vidsoc
026517160121     C                   ENDIF
026518160121     C*
026519160121     C                   ENDSR
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
030301160108     C     Kfrn          KLIST
030302160108     C                   KFLD                    vidsoc
030303160108     C                   KFLD                    vidfor
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
