000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200150605     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000300160304     Fficn47d   CF   E             WORKSTN
000400160304     Fazorg01l  IF   E           K DISK
000500150727     FTntbe01l  IF   E           K DISK
000600150529     D DTAcor          S               D   INZ
000700160111     D viddt1g         S              8s 0 INZ
000800160111     D viddt2g         S              8s 0 INZ
000900011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001000000000     D KPJBA         E DS
001100160304     d dcre          e ds
001200160309     d dtsr          e ds
001300160304     D ficn47i       E DS
001400150807     D TIBS02DS      E DS
001500150529     D TIBS34DS      E DS                                                       *Profili utente
001600150529     D DDATIUTE      E DS                                                       *Dati utente
001700150529     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001800150811     D dute01        E DS                                                       *Utenti
001900150811     d dLat          e ds
002000150914     D TRUL31DS      E DS
002100150914     D TRUL31DS2     E DS
002200150811     d wabi            s                   like(UteAut)
002300150914     d* p.o. abilitati
002400150914     D $pogn           s              3  0 dim(250)
002500150914     D $pog            s              3    dim(250)
002600150914     D $DIG            s              1    dim(20)
002700150914     D $ARG            s              3    dim(50)
002800150928     D §cod            s              3
002900150928     D §tip            s              1
003000150928     D §des            s             25
003100150928     D §pra            s              3
003200150914     D*------------------
003300150914     D WLBDA8          DS                  INZ
003400150914     D  G08DAT                 1      8  0
003500150914     D  G08INV                 9     16  0
003600150914     D  G08ERR                17     17
003700150914     D  G08TGI                18     22  0
003800150928     C*_______________________________________________________________
003900011130     C  nlr              DO        *HIVAL
004000011130     c                   EXSR      CLR00
004100160304     C                   EXFMT     ficn47
004200011130     C* FINE LAVORO
004300011130     C   KC              LEAVE
004400011130     C* RICERCA
004500151006     c                   if        *inkd
004600151006     C                   EXSR      RIC01
004700151006     c                   endif
004800000000     C* CONTROLLI
004900151006     C                   EXSR      CTRL01
005000150924     c   kd
005100150924     COR 99              ITER
005200160304     c                   clear                   FICN47I
005300160304     c                   move      vtipo         C47CAU
005400160304     c                   move      vidDt1g       C47DAT1
005500160304     c                   move      viddt2g       C47DAT2
005600160304     c                   move      vidfil        C47fil
005700160309     c                   move      vidana        C47ana
005800160309     c                   move      vtsr          C47tsr
005900160304     c                   movel     FICN47I       kpjbu
006000160304     c                   call      'FICN47R1'
006100160111     c                   parm                    kpjba
006200160304     c                   movel     kpjbu         FICN47I
006300011130     C                   ENDDO
006400011130     c*
006500000000     C                   SETON                                        LR
006600011130     c**********************************************************************
006700011130     c* pulisce 1° formato
006800011130     c**********************************************************************
006900011130     C     clr00         BEGSR
007000011130     C                   Z-ADD     0             §RIGA
007100011130     C                   Z-ADD     0             §COLON
007200011130     C                   CLEAR                   §FLD
007300011130     C                   ENDSR
007400000000     C******************************************************
007500000000     C** CONTROLLI FORMATO ENTRATA
007600011130     C******************************************************
007700011130     C     CTRL01        BEGSR
007800160304     C                   SETOFF                                       998081
007900160304     C                   SETOFF                                       828328
008000011130     C                   DO
008100160304     c* codice filiale
008200160407     c                   clear                   vidfild
008300160304     c                   if        vidfil <> *zero
008400160304     C     vidfil        CHAIN     azorg01l
008500160304     c                   if        not%found(azorg01l)
008600160211     C                   SETON                                        992883
008700160304     c                   eval      $msg = 'Filiale errata'
008800160211     c                   leavesr
008900160304     c                   else
009000160304     c                   move      orgdes        vidfild
009100160304     C                   endif
009200160211     C                   endif
009300150914      * date
009400151006      * dal PRESTAZIONE
009500150914     c                   clear                   viddt1g
009600150914     c                   if        viddt1 > 0
009700150914     c                   move      viddt1        g08dat
009800150914     c                   move      *blank        g08err
009900150914     C                   CALL      'XSRDA8'
010000150914     C                   PARM                    WLBDA8
010100150914     C     G08ERR        IFEQ      '1'
010200150914     C                   SETON                                        992882
010300150914     c                   eval      $msg = 'Data errata '
010400150914     c                   leavesr
010500160307     C                   endif
010600160307     c                   move      g08inv        viddt1g
010700160307     c                   move      g08dat        viddt1
010800160307     C                   else
010900160307     C                   SETON                                        992882
011000160307     c                   eval      $msg = 'Data Obbligatoria '
011100160307     c                   leavesr
011200150914     C                   endif
011300150914      * al
011400150914     c                   clear                   viddt2g
011500150914     c                   if        viddt2 > 0
011600150914     c                   move      viddt2        g08dat
011700150914     c                   move      *blank        g08err
011800150914     C                   CALL      'XSRDA8'
011900150914     C                   PARM                    WLBDA8
012000150914     C     G08ERR        IFEQ      '1'
012100150914     C                   SETON                                        992881
012200150914     c                   eval      $msg = 'Data errata '
012300150914     c                   leavesr
012400150914     C                   endif
012500150914     c                   move      g08inv        viddt2g
012600150929     c                   move      g08dat        viddt2
012700150914     C                   endif
012800160111     c                   if        viddt1g <> *zero
012900160111     c                   if        viddt2g = *zero
013000150914     c                   move      viddt1g       viddt2g
013100150914     c                   move      viddt1        viddt2
013200150914     c                   endif
013300160211     c                   endif
013400160211     c                   if        viddt1g =  *zero
013500160211     c                   if        viddt2g <> *zero
013600160211     c                   move      viddt2g       viddt1g
013700160211     c                   move      viddt2        viddt1
013800160211     c                   endif
013900160211     c                   endif
014000150914     c                   if        viddt1g > viddt2g
014100150914     C                   SETON                                        992881
014200150914     c                   eval      $msg  = 'Data dal maggiore di data al '
014300150914     c                   endif
014400160309      * CONTROLLO la causale
014500160211     c                   if        vtipo  <> *blank
014600160211     c                   clear                   tibs02ds
014700160211     c                   eval      t02mod = 'C'
014800160211     c                   eval      t02ke1 = vtipo
014900160211     c                   exsr      srtab
015000160211     c                   if        t02err <> *blank
015100160211     C                   SETON                                        992880
015200160211     c                   eval      $msg = 'Causale non valida'
015300160211     c                   leavesr
015400160211     c                   endif
015500160211     c                   else
015600160211     c                   clear                   vtipod
015700160211     c                   endif
015800160309      * CONTROLLO il tipo servizio
015900160309     c                   if        vtsr  <> *blank
016000160309     c                   clear                   tibs02ds
016100160309     c                   eval      t02mod = 'C'
016200160309     c                   eval      t02ke1 = vtsr
016300160309     c                   exsr      srtabb
016400160309     c                   if        t02err <> *blank or
016500160309     c                             (§tsrtip <> 'A' and §tsrtip <> 'C')
016600160309     C                   SETON                                        992880
016700160309     c                   eval      $msg = 'Tipo prestazione non valido'
016800160309     c                   leavesr
016900160309     c                   endif
017000160309     c                   if        vidana <> *blank and
017100160309     c                             §tsrtip <> vidana
017200160309     C                   SETON                                        992880
017300160309     c                   eval      $msg = 'Tipo prestazione incongruente con -
017400160309     c                             tipo anagrafica selezionata'
017500160309     c                   leavesr
017600160309     c                   endif
017700160309     c                   else
017800160407     c                   clear                   vtsrd
017900160309     c                   endif
018000150727      *
018100011130     C                   ENDDO
018200000000     C                   ENDSR
018300011130     C******************************************************
018400011130     C* RICERCA PRIMO VIDEO
018500011130     C******************************************************
018600011130     C     RIC01         BEGSR
018700011130     c                   select
018800150928      * ricerca causale
018900150529     C                   when      §fld = 'VTIPO'
019000160108     C                   exsr      srtab
019100160121      * ricerca società
019200160304     C                   when      §fld = 'VIDFIL'
019300160304     C  n79              exsr      srfil
019400160304     c                   if        §cod <> ' '
019500160304     c                   move      §cod          vidfil
019600160304     c                   movel(p)  §des          vidfild
019700160304     c                   end
019800160309      * ricerca tipo servizio
019900160309     C                   when      §fld = 'VTSR' and not *in11
020000160309     C                   exsr      srtabb
020100011130     C                   ENDsl
020200011130     C* POSIZIONAMENTO CURSORE
020300011130     C                   Z-ADD     §RIGA         §RIG
020400011130     C                   Z-ADD     §COLON        §COL
020500011130     C                   ENDSR
020600160304     C*----------------------------------------------------*
020700160304     C* ricerca filiale
020800160304     C*----------------------------------------------------*
020900160304     C     srfil         begsr
021000160304     c                   clear                   §cod
021100160304     c                   eval      §tip = 'F'
021200160304     c                   clear                   §des
021300160304     c                   clear                   §pra
021400160304     c                   call      'TNSD24R'
021500160304     c                   parm                    §cod
021600160304     c                   parm                    §tip
021700160304     c                   parm                    §des
021800160304     c                   parm                    §pra
021900160304     C                   ENDsr
022000160114     C**********************************************************************
022100160114     C** Controlli e decodifiche campi con tntbe00f
022200160114     C**********************************************************************
022300160114     C     srtab         BEGSR
022400160114     c                   clear                   tibs02ds
022500160114     c                   eval      t02mod = 'R'
022600160114     c                   if        vtipo <> *blank
022700160114     c                   eval      t02mod = 'C'
022800160114     c                   eval      t02ke1 = vtipo
022900160114     c                   endif
023000160114     c                   eval      t02sif = knsif
023100160304     c                   eval      t02cod = 'CRE'
023200160114     c                   call      'TIBS02R'
023300160114     c                   parm                    kpjba
023400160114     c                   parm                    tibs02ds
023500160114     c                   if        t02err = *blank
023600160304     c                   Eval      dcre = T02Uni
023700160304     c                   Eval      vtipod = §crdes
023800160114     c                   Eval      vtipo  = t02ke1
023900160114     c                   endif
024000160114     c*
024100160114     C                   ENDSR
024200160309     C**********************************************************************
024300160309     C** Controlli e decodifiche campi con tntbe00f
024400160309     C**********************************************************************
024500160309     C     srtabb        BEGSR
024600160309     c                   clear                   tibs02ds
024700160309     c                   eval      t02mod = 'R'
024800160309     c                   if        vtsr <> *blank
024900160309     c                   eval      t02mod = 'C'
025000160309     c                   eval      t02ke1 = vtsr
025100160309     c                   endif
025200160309     c                   eval      t02sif = knsif
025300160309     c                   eval      t02cod = 'TSR'
025400160309     c                   call      'TIBS02R'
025500160309     c                   parm                    kpjba
025600160309     c                   parm                    tibs02ds
025700160309     c                   if        t02err = *blank
025800160309     c                   Eval      dtsr = T02Uni
025900160309     c                   Eval      vtsrd  = §tsrdes
026000160309     c                   Eval      vtsr   = t02ke1
026100160309     c                   endif
026200160309     c*
026300160309     C                   ENDSR
026400011130     C*----------------------------------------------------*
026500011130     c     *inzsr        begsr
026600011130     C*----------------------------------------------------*
026700011130     C     *ENTRY        PLIST                                                  *
026800011130     C                   PARM                    KPJBA                          *
026900150529     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
027000150529     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
027100150529     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
027200150529     C                   move      WN8           dtaeur
027300150529     C                   move      dtaeur        DtaCOR                         *DATA CORRENTE AA/M/
027400151020     C                   move      dtacor        wn8g              8 0          *DATA CORRENTE AA/M/
027500160309      * forzatura 11 acceso per proteggere e oscurare i campi di selezione relativi
027600160309      * al  tipo servizio
027700160309     c                   seton                                        11
027800150529      * tabelle
027900150529     C                   EXSR      REPDATIUTE
028000160304     c                   if        dutpou <> 046
028100160304     c                   move      dutpou        vidfil
028200160304     c                   seton                                        79
028300160304     c                   endif
028400150727     C* POSIZIONAMENTO TABELLE                             *
028500150727     C     KTbe          KLIST                                                  *
028600150727     C                   KFLD                    TBECOD                         *
028700150727     C                   KFLD                    TBEKE1                         *
028800150727     C                   KFLD                    TBEKE2                         *
028900150529     c                   endsr
029000150529     C*--------------------------------------------------------------------------------------------*
029100150529     C* REPERISCE I DATI UTENTE
029200150529     C*--------------------------------------------------------------------------------------------*
029300150529     C     REPDATIUTE    BEGSR
029400150529     C*
029500150529     C* INIZIALIZZA VARIABILI DI WRK
029600150529     C                   CLEAR                   TIBS34DS
029700150529     C                   CLEAR                   AZUTEDS
029800150529     C                   CLEAR                   DDATIUTE
029900150529     C*
030000150529     C     *DTAARA       DEFINE    §azute        azuteds
030100150529     C     *DTAARA       DEFINE    §datiute      ddatiute
030200150529     C                   IN(E)     *DTAARA
030300150529     C                   IF        %Error
030400150529     C                   EVAL      I34Tla = 'L'
030500150529     C                   CALL      'TIBS34R'
030600150529     C                   PARM                    Tibs34Ds
030700150529     C                   IN        *DTAARA
030800150529     C                   ENDIF
030900011130     c                   endsr
