000100080206      //--------------------------------------------------------------
000200130820      //?TISIO2R - Cappario: gestione orari servizi
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100130820     fAZcpl01l  if   e           k disk
001200130821     fAZcpc01l  if   e           k disk
001300170120     fAZcpr01l  if   e           k disk
001400130827     fAZcpe01l  if   e           k disk
001500131031     fAZcpt01l  if   e           k disk
001600131213     fAZCOS03l  uf a e           k disk
001700130820     fAZORG01L  if   e           k disk
001800130821     ftabel00f  if   e           k disk
001900130820     fTISIO2D   cf   e             workstn indds(IndDspF)
002000080206     f                                     infds(InfDspF)
002100130821     f                                     sfile(sio2S01 : S01nrr)
002200080206      //---------------------------------------------------------------
002300080207      // - Tasti funzionali a video
002400080207     d c_F01           c                   const(x'31')
002500080207     d c_F02           c                   const(x'32')
002600080207     d c_F03           c                   const(x'33')
002700080207     d c_F05           c                   const(x'35')
002800080207     d c_F06           c                   const(x'36')
002900080207     d c_F07           c                   const(x'37')
003000080207     d c_F08           c                   const(x'38')
003100080207     d c_F09           c                   const(x'39')
003200080207     d c_F10           c                   const(x'3A')
003300080207     d c_F12           c                   const(x'3C')
003400080207     d c_F13           c                   const(x'B1')
003500080207     d c_F14           c                   const(x'B2')
003600080207     d c_F15           c                   const(x'B3')
003700080207     d c_F16           c                   const(x'B4')
003800080207     d c_F17           c                   const(x'B5')
003900080207     d c_F18           c                   const(x'B6')
004000080207     d c_F19           c                   const(x'B7')
004100080207     d c_F20           c                   const(x'B8')
004200080207     d c_F21           c                   const(x'B9')
004300080207     d c_F22           c                   const(x'BA')
004400080207     d c_F23           c                   const(x'BB')
004500080207     d c_F24           c                   const(x'BC')
004600080207     d c_Enter         c                   const(x'F1')
004700080207     d c_RollDown      c                   const(x'F4')
004800080207     d c_RollUp        c                   const(x'F5')
004900080214
005000080214      // - Attributi di visualizzazione
005100080215      //  -  DspAtr() - Normale
005200080214     d C_dspatr_Norm   c                   const(x'20')
005300080215      //  -  DspAtr(RI)
005400080214     d C_dspatr_RI     c                   const(x'21')
005500080215      //  -  DspAtr(ND)
005600080214     d C_dspatr_ND     c                   const(x'27')
005700080215      //  -  DspAtr(BL) / Color(Red)
005800080214     d C_dspatr_BL     c                   const(x'28')
005900080206
006000080206      //---------------------------------------------------------------
006100080206      //?Definizione schiere.
006200080206      //---------------------------------------------------------------
006300080206
006400080206      // - Messaggi di errore
006500130821     d MSG             s             78    dim(25) ctdata perrcd(1)
006600130905     d lna_ES          s              1    dim(14)
006700131213     d lna_SOG         s              1    dim(14)
006800140210     d lna_LOL         s              1    dim(14)
006900140206     d                 ds
007000140206     d lnaprevuni                    42  0
007100140206     d lna_prev                       3  0 dim(14)overlay(lnaprevuni)
007200140206     d*lna_prev        s              3  0 dim(14)
007300081126
007400080206      //---------------------------------------------------------------
007500080206      //?Definizione aree dati.
007600080206      //---------------------------------------------------------------
007700100226
007800080206      // - Dati utente
007900080206     d §AzUte        e ds                  extname(AZUTE00F)
008000080206     d                                     dtaara
008100080206     d §DatiUte      e ds                  extname(dDatiUte)
008200080206     d                                     dtaara
008300080206
008400080206      //---------------------------------------------------------------
008500080206      //?Definizione strutture dati.
008600080206      //---------------------------------------------------------------
008700080619     D CPO01           DS
008800080619     D  cp1NRR               397    400B 0
008900080206
009000080206      // - Status
009100080206     d Psds           sds
009200080206     d   SDSpgm          *proc
009300080206
009400080206      // - InfDS
009500080206     d InfDspF         ds
009600080207     d  dsp_aid              369    369a                                        AID byte
009700080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
009800080207     d  min_nrr              378    379i 0                                      Subfile min rrn
009900080207     d  num_rcds             380    381i 0                                      Subfile num rcds
010000080206
010100080206      // - Indicatori su DspF
010200080208     d IndDspF         ds
010300080206        // - Indicatori di gestione del subfile
010400080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010500080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010600080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010700080206     d  SflEnd                        1n   overlay(IndDspF : 33)
010800080206        // - Indicatori di errore
010900080206     d  errMessage                    1n   overlay(IndDspF : 28)
011000080606     d  errGenerico                   1n   overlay(IndDspF : 99)
011100080627        // - Indicatori vari
011200130829     d  protriga                      1n   overlay(IndDspF : 01)
011300130829     d  orariINT                      1n   overlay(IndDspF : 02)
011400130820     d  Immiss                        1n   overlay(IndDspF : 03)
011500081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
011600120111     d  F03Attivo                     1n   overlay(IndDspF : 05)
011700120111     d  F12Attivo                     1n   overlay(IndDspF : 06)
011800130828     d  Vis_CAP                       1n   overlay(IndDspF : 08)
011900130828     d  Vis_LOC                       1n   overlay(IndDspF : 09)
012000130829     d  Vis_rifer                     1n   overlay(IndDspF : 10)
012100131003     d  prot_tempo                    1n   overlay(IndDspF : 11)
012200131003     d  abilF7                        1n   overlay(IndDspF : 12)
012300131213     d  abilF9                        1n   overlay(IndDspF : 13)
012400130828     d  HI_dlna                       1n   overlay(IndDspF : 14)
012500130828     d  ProtLNA                       1n   overlay(IndDspF : 15)
012600131213     d  indLna9988                    1n   overlay(IndDspF : 16)
012700131213     d  protSoglia                    1n   overlay(IndDspF : 17)
012800130820     d  ErrOSTIS                      1n   overlay(IndDspF : 40)
012900130821     d  ErrOSTFS                      1n   overlay(IndDspF : 41)
013000130820     d  ErrOMIIS                      1n   overlay(IndDspF : 42)
013100130820     d  ErrOMXFS                      1n   overlay(IndDspF : 43)
013200130820     d  ErrTRZS                       1n   overlay(IndDspF : 44)
013300130821     d  ErrLNA                        1n   overlay(IndDspF : 45)
013400131010     d  ErrOlrsc                      1n   overlay(IndDspF : 47)
013500131010     d  ErrOlrnc                      1n   overlay(IndDspF : 48)
013600131010     d  ErrOlrnk                      1n   overlay(IndDspF : 49)
013700130828     d  BL_OSTIS                      1n   overlay(IndDspF : 50)
013800130828     d  BL_OSTFS                      1n   overlay(IndDspF : 51)
013900130828     d  BL_OMIIS                      1n   overlay(IndDspF : 52)
014000130828     d  BL_OMXFS                      1n   overlay(IndDspF : 53)
014100131010     d  BL_OLRSC                      1n   overlay(IndDspF : 55)
014200131010     d  BL_OLRNC                      1n   overlay(IndDspF : 56)
014300131010     d  BL_OLRNK                      1n   overlay(IndDspF : 57)
014400131213     d  BL_TRZS                       1n   overlay(IndDspF : 58)
014500080206
014600080206      // - Parametri ricevuti
014700080206     d KPJBA         e ds
014800080702
014900130820     d tisio0ds      e ds
015000080206
015100080206      // - Reperimento dati utente
015200080206     d TIBS34ds      e ds
015300080206     d dUte01        e ds
015400080702     d DLAT          e ds
015500130821     d ds15          e ds
015600131031     d ds5e          e ds
015700080702
015800080206
015900080206      // - Ricerca/Controllo tabelle
016000080206     d TIBS02ds      e ds                  inz
016100080627
016200080206      //---------------------------------------------------------------
016300080206      //?Definizione variabili globali.
016400080206      //---------------------------------------------------------------
016500080206
016600080206      // - Flags booleani
016700080208     d $Fine           s               n   inz(*off)
016800080208     d $InzS01         s               n   inz(*on)
016900120105     d wAumenta10      s               n   inz(*off)
017000120510     d wAumenta10f     s               n   inz(*off)
017100120110     d wAzzera10       s               n   inz(*off)
017200120105     d wDiminuisci10   s               n   inz(*off)
017300120109     d wForzaAgg       s               n   inz(*off)
017400080206
017500080627     d $Video          s              2    inz('S1')
017600080208     d S01nrr          s              4  0 inz
017700080627     d Indx            s              3  0 inz
017800081126     d yy              s              3  0 inz
017900130821     d Wrighe          s              3  0 inz
018000120109     d wOggi           s              8  0 inz
018100130820     d Tlna            s              1    inz
018200130821     d Wrecord         s              1    inz
018300131213     d Wsoglia         s              1    inz
018400151209     d Wdoppiag        s              1    inz
018500131003     d W003A           s              3    inz
018600130821     d klna            s                   like(coslna)
018700130827     d lna999          s                   like(coslna) inz(999)
018800130821     d savlna          s                   like(coslna)
018900130820     d Elalna          s                   like(coslna)
019000130820     d Elalol          s                   like(coslna)
019100130820     d Elalos          s                   like(coslna)
019200130820     d kloc            s                   like(cosloc)
019300130827     d kcap            s                   like(coscap)
019400130820     d ktser           s                   like(costser)
019500131213     d ksoglia         s                   like(cosserv)
019600130821     d ktbkut          s                   like(tblkut)
019700130821     d ktbcod          s                   like(tblcod)
019800130821     d ktbkey          s                   like(tblkey)
019900080414
020000080605     d Dataiso         s               d   datfmt(*iso)
020100130821     d Datasys         s               d   datfmt(*iso) inz(*sys)
020200080605     d Dataymd         s               d   datfmt(*ymd)
020300080605     d Datadmy         s               d   datfmt(*dmy)
020400130904     d C88ostis        s                   like(cosostis)
020500130904     d C88ostfs        s                   like(cosostfs)
020600130904     d C88omiis        s                   like(cosomiis)
020700130904     d C88omxfs        s                   like(cosomxfs)
020800131010     d C88olrsc        s                   like(cosolrsc)
020900131010     d C88olrnc        s                   like(cosolrnc)
021000131010     d C88olrnk        s                   like(cosolrnk)
021100131216     d C88trzs         s                   like(costrzs)
021200131216     d
021300131216     d C88Postis       s                   like(cosostis)
021400131216     d C88Postfs       s                   like(cosostfs)
021500131216     d C88Pomiis       s                   like(cosomiis)
021600131216     d C88Pomxfs       s                   like(cosomxfs)
021700131216     d C88Polrsc       s                   like(cosolrsc)
021800131216     d C88Polrnc       s                   like(cosolrnc)
021900131216     d C88Polrnk       s                   like(cosolrnk)
022000131216     d C88Ptrzs        s                   like(costrzs)
022100131010     d
022200130904     d R88ostis        s                   like(cosostis)
022300130904     d R88ostfs        s                   like(cosostfs)
022400130904     d R88omiis        s                   like(cosomiis)
022500130904     d R88omxfs        s                   like(cosomxfs)
022600131010     d R88olrsc        s                   like(cosolrsc)
022700131010     d R88olrnc        s                   like(cosolrnc)
022800131010     d R88olrnk        s                   like(cosolrnk)
022900131010     d
023000130904     d W88ostis        s                   like(cosostis)
023100130904     d W88ostfs        s                   like(cosostfs)
023200130904     d W88omiis        s                   like(cosomiis)
023300130904     d W88omxfs        s                   like(cosomxfs)
023400131010     d W88olrsc        s                   like(cosolrsc)
023500131010     d W88olrnc        s                   like(cosolrnc)
023600131010     d W88olrnk        s                   like(cosolrnk)
023700131216     d W88trzs         s                   like(costrzs)
023800131031
023900131031     d wttc            s                   like(cpcttc)
024000131031     d wcpt0in         s                   like(cpt0in)
024100131031     d wcpt0in4        s                   like(cosostis)
024200131031     d wdtsp           s                   like(§5ed08)
024300131031     d wtsp            s                   like(cpttsp)
024400170120     d wabiute         s              1
024500131031
024600131031
024700080604     d
024800080605     d                 DS
024900080605     d  AA                     1      4  0
025000080605     d  MM                     5      6  0
025100080605     d  GG                     7      8  0
025200080605     d DATA                    1      8  0
025300130821     d                 DS
025400130821     d  wHH                    1      2  0
025500130821     d  wMM                    3      4  0
025600130821     d WORA                    1      4  0
025700081009     d
025800080206
025900080206      //---------------------------------------------------------------
026000080206      //?Definizione procedure usate.
026100080206      //---------------------------------------------------------------
026200080414      /copy gaitrasrc/srcprotopr,tibs02r
026300080414      /copy gaitrasrc/srcprotopr,tibs34r
026400080206
026500080206      //---------------------------------------------------------------
026600080206      //?Riepilogo indicatori.
026700080206      //---------------------------------------------------------------
026800080207      // - Comuni
026900080207      // 28    : Emissione messaggio di errore a video
027000080207      // - C01/S01
027100080207      // 30    : Condiziona SFLDSP    (*not)
027200080207      // 31    : Condiziona SFLDSPCTL (*not)
027300080207      // 30+31 : Condiziona SFLCLR
027400080207      // 32    : Condiziona SFLNXTCHG in S01
027500080207      // 50    : Errore: Opzione errata
027600080207      // - D01
027700080207      // - Comuni
027800080207      // 99    : Generico di Errore
027900080206      //---------------------------------------------------------------
028000080206
028100080206      //---------------------------------------------------------------
028200080206      //?M A I N - L I N E
028300080206      //---------------------------------------------------------------
028400080206
028500080627     c     *Entry        plist
028600080206     c                   parm                    KPJBA
028700130820     C                   PARM                    tiSIO0ds
028800130820     c                   clear                   do0err
028900130820     c                   clear                   do0msg
029000080702
029100080206      /free
029200080925
029300080206       // Operazioni iniziali
029400080206       exsr RoutInz;
029500080206
029600130820 2      if  Do0Err=' '    ;
029700081215
029800080206       // Gestione video
029900081215 3     DOW $Fine = *off;
030000081215 4       select;
030100080530
030200080530         // Videata di selezioni
030300080627           when $Video = 'S1';
030400080627             exsr GesS01;
030500080530
030600080702           other   ;
030700080206             $Fine = *on;
030800081215 4       endsl;
030900081215 3     ENDDO;
031000081215
031100081215 2     endif    ;
031200080206
031300080206       // Operazioni finali
031400080206       exsr RoutEnd;
031500080206
031600080206       //--------------------------------------------------------------
031700080206       //?Operazioni iniziali.
031800080206       //--------------------------------------------------------------
031900080206       BEGSR RoutInz;
032000170116      /end-free
032100170116     c* Se la versione non è stata passata uso la data di riferimento per calcolarla
032200170116     c                   if        do0ver=0
032300170116     c
032400170116     c                   if        do0dri=0
032500170116     c                   movel     datasys       do0dri
032600170116     c                   endif
032700170116     C                   Z-ADD     Do0DRI        P96DRI            8 0
032800170116     C                   Z-ADD     *ZEROS        P96VER            5 0
032900170116     C                   CALL      'TISI96R'
033000170116     C                   PARM                    P96DRI
033100170116     C                   PARM                    P96VER
033200170116     c
033300170116     c                   eval      do0ver=p96ver
033400170116     c                   endif
033500170116     c                   movel     datasys       datcor            8 0
033600170116
033700170116      /free
033800131003
033900131003       AbilF7=*off  ;
034000131213       AbilF9=*off  ;
034100131213       IndLna9988=*off  ;
034200080627       $inzs01=*on;
034300080929       clear vsctes2         ;
034400131213       clear Wsoglia         ;
034500080703
034600080206         // Reperimento dati job
034700080206         exsr DatiJob;
034800170120         wabiute=§utecap ;
034900170120         if wabiute='V'    ;
035000170120         chain do0ver  azcpr01l   ;
035100170116          if %found(azcpr01l) and cprdde > datcor  ;
035200170116          else  ;
035300170116          wabiute='I'  ;
035400170116          endif  ;
035500170116         endif  ;
035600081003
035700081003         // Se sono EDP accendo indicatore
035800081003         if %subst(knmus:1:3)<>'EDP'    ;
035900081003         UTENonEDP=*on   ;
036000130820         endif  ;
036100080627
036200080704         $Fine=*off   ;
036300080703
036400080702         // Vedo se modo gestione  o interrogazione
036500080703
036600130820         vscpgm='TISIO2R'   ;
036700080703
036800170120 1       if do0tric='G' and wabiute<>'I' ;
036900130830          vsctes2='  VARIAZIONE  '    ;
037000080929 1       endif    ;
037100130830
037200130830 1       if vsctes2= *blanks    ;
037300130830          orariInt =*on  ;
037400130830          vsctes2='INTERROGAZIONE'    ;
037500130830 1       endif;
037600130821
037700130821      /end-free
037800130820
037900130821     c*     Decodifico la nazione
038000130821     C                   MOVEL     1             KTBKUT
038100130821     C                   MOVEL     '15'          KTBCOD
038200130821     C                   MOVEL(P)  do0nar        KTBKEY
038300130821     C     KEYTAB        CHAIN     TABEL00F                           99
038400130821IF  1C     *IN99         IFEQ      *OFF
038500130821     C     TBLFLG        ANDEQ     *BLANKS
038600130821     C                   MOVEL     TBLUNI        DS15
038700130821     C                   MOVEL     §15DES        v1dnar
038800130821     c                   endif
038900130820
039000130821     C     KEYTAB        KLIST
039100130821     C                   KFLD                    KTBKUT                         *CODICE UTENTE
039200130821     C                   KFLD                    KTBCOD                         *TABELLA
039300130821     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
039400130820     C     KEYCPL        KLIST
039500130820     C                   KFLD                    Do0VER                         *VERSIONE
039600130820     C                   KFLD                    Do0NAR                         *NAZIONE
039700130820     C                   KFLD                    Do0CAP                         *CAP
039800130820     C                   KFLD                    KLOC                           *LOCALITA'
039900130820     C     KEYCPC        KLIST
040000130820     C                   KFLD                    Do0VER                         *VERSIONE
040100130820     C                   KFLD                    Do0NAR                         *NAZIONE
040200130820     C                   KFLD                    Do0CAP                         *CAP
040300130820
040400130822     c                   clear                   elalna
040500130822     c                   clear                   elalol
040600130822     c                   clear                   elalos
040700130827     c                   clear                   lna_prev
040800131213     c                   clear                   lna_es
040900131213     c                   clear                   lna_sog
041000140210     c                   clear                   lna_lol
041100130827     c*
041200130821     c* Caricamento dei dati della località passata
041300130827    0c                   if        Do0LOc<>*blanks
041400130828     c                   eval      Vis_LOC=*on
041500130828     c                   eval      Vis_CAP=*on
041600130820     C                   MOVEL     Do0LOC        KLOC
041700130820     C     KEYCPL        CHAIN     AZCPL01L
041800130820IF  1C                   IF        not %found(azcpl01l)
041900130820     C                   MOVEL     MSG(1)        Do0MSG                         *MESSAGGIO
042000130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
042100130827     c                   leavesr
042200130827   x1c                   else
042300130821     c* Se il livello di gestione è CAP--> prendo i dati da questo
042400130827    2c                   if        cplliv='L' and cpllna>0
042500130820     c                   movel     cpllna        elalna
042600140210    3c                   if        cpllos<>cpllna
042700130820     c                   movel     cpllos        elalos
042800130827    3c                   endif
042900140210    3c                   if        cpllol<>cpllna and cpllos<>cpllol
043000140210     c                   movel     cpllol        elalol
043100140210    3c                   endif
043200130821     c
043300130827     c                   exsr      car_lna
043400131031     c                   eval      wttc=cplttc
043500130827     c
043600130827    2c                   endif
043700130827    1C                   ENDIF
043800130820     c
043900130827    0c                   endif
044000130820
044100130827    0c                   if        (do0loc=*blanks and do0cap<>*blanks) or
044200130827     c                              cplliv='C' or (cplliv='L' and cpllna=0)
044300130828     c                   eval      Vis_CAP=*on
044400130820     C     KEYCPC        CHAIN     AZCPC01L
044500130821IF  1C                   IF        not %found(azcpc01l)
044600130821     C                   MOVEL     MSG(10)       Do0MSG                         *MESSAGGIO
044700130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
044800130827     c                   leavesr
044900130827    1c                   endif
045000130827
045100130820     c                   movel     cpclna        elalna
045200140210    1c                   if        cpclos<>cpclna
045300140210     c                   movel     cpclos        elalos
045400130827    1c                   endif
045500140210    1c                   if        cpclol<>cpclna and cpclos<>cpclol
045600140210     c                   movel     cpclol        elalol
045700130827    1C                   ENDIF
045800130827     c*
045900130827     c                   exsr      car_lna
046000131031     c                   eval      wttc=cpcttc
046100130827     c
046200130827     c* Carico eventuali eccezioni di linea se presenti
046300130827     C     KEYCPC        setll     AZCPe01L
046400130827     C     KEYCPC        reade     AZCPe01L
046500130827     c
046600130827    1c                   dow       not %eof(azcpe01l)
046700130827    2c                   if        cpelna>0
046800130827     c                   eval      indx=%lookup(cpelna:lna_prev)
046900130827    3c                   if        Indx=0
047000130905     c                   add       1             yy
047100130827     c                   z-add     cpelna        lna_prev(yy)
047200130827    3c                   endif
047300130827    2c                   endif
047400130827     c
047500130827    2c                   if        cpelos>0
047600130827     c                   eval      indx=%lookup(cpelos:lna_prev)
047700130827    3c                   if        Indx=0
047800130905     c                   add       1             yy
047900130827     c                   z-add     cpelos        lna_prev(yy)
048000130827    3c                   endif
048100130827    2c                   endif
048200140210     c
048300140210    2c                   if        cpelol>0
048400140210    3c                   eval      indx=%lookup(cpelol:lna_prev)
048500140210     c                   if        Indx=0
048600140210     c                   add       1             yy
048700140210     c                   z-add     cpelol        lna_prev(yy)
048800140210     c                   movel     'S'           lna_lol(yy)
048900140210    3c                   endif
049000140210    2c                   endif
049100130827     c
049200130827     C     KEYCPC        reade     AZCPe01L
049300130827    1c                   enddo
049400130827     c
049500130827    0C                   ENDIF
049600130827     c
049700130827     c* Se non passato il CAP ci deve essere la linea di arrivo (inserimento di filiale)
049800130827    0c                   if        do0loc=*blanks and do0cap=*blanks
049900130827     c* Se passato 999 --> orari standard
050000130827    1c                   if        do0lna=0
050100130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
050200130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
050300130827     c                   leavesr
050400130827    1c                   endif
050500131003
050600131003     c                   eval      AbilF7=*on
050700130827     c
050800130904    1c                   if        do0lna=999 or do0lna=888
050900130904     c                   eval      elalna=888
051000130904     c                   eval      elalol=999
051100131010     c
051200130827   x1c                   else
051300130827     c
051400130827     c     do0lna        chain     azorg01l
051500130827    2c                   if        not %found(azorg01l)
051600130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
051700130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
051800130827     c                   leavesr
051900130827    2c                   endif
052000130827     c                   eval      elalna=do0lna
052100131213     c
052200131213     c* Abilito tasto F9 se non presente orario soglia da filiale
052300131213      /free
052400131213         chain(n)  (do0ver:do0nar:kcap:kloc:elalna:'C':'P ') azcos03l ;
052500131213      /end-free
052600131213     c                   if        not %found(azcos03l)
052700140109     c* non abilito in interrogazione
052800140109     c                   if        OrariINT=*off
052900131213     c                   eval      AbilF9=*on
053000140109     c                   endif
053100140109     c
053200131213     c                   else
053300131213     c                   eval      wsoglia='S'
053400131213     c                   endif
053500131213     c
053600130827    1c                   endif
053700131213
053800130827     c                   exsr      car_lna
053900130827    0c                   endif
054000131213
054100130904     c* carico il record 888 per i controlli
054200130904     c                   clear                   kloc
054300130904     c                   clear                   kcap
054400130904      /free
054500130904         ktser='C'       ;
054600131216         clear ksoglia   ;
054700131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
054800131213         if %found(azcos03l)  ;
054900130904
055000130904         C88ostis=cosostis  ;
055100130904         C88ostfs=cosostfs  ;
055200130904         C88omiis=cosomiis  ;
055300130904         C88omxfs=cosomxfs  ;
055400131010         C88olrsc=cosolrsc  ;
055500131010         C88olrnc=cosolrnc  ;
055600131010         C88olrnk=cosolrnk  ;
055700131216         C88trzs =costrzs   ;
055800130904         endif ;
055900131216
056000131216         ksoglia='P ' ;
056100131216         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
056200131216         if %found(azcos03l)  ;
056300131216
056400131216         C88Postis=cosostis  ;
056500131216         C88Postfs=cosostfs  ;
056600131216         C88Pomiis=cosomiis  ;
056700131216         C88Pomxfs=cosomxfs  ;
056800131216         C88Polrsc=cosolrsc  ;
056900131216         C88Polrnc=cosolrnc  ;
057000131216         C88Polrnk=cosolrnk  ;
057100131216         C88Ptrzs =costrzs   ;
057200131216         endif ;
057300130904
057400131011         ktser='R'       ;
057500131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser)  azcos03l   ;
057600131213         if %found(azcos03l)  ;
057700130904
057800130904         R88ostis=cosostis  ;
057900130904         R88ostfs=cosostfs  ;
058000130904         R88omiis=cosomiis  ;
058100130904         R88omxfs=cosomxfs  ;
058200131010         R88olrsc=cosolrsc  ;
058300131010         R88olrnc=cosolrnc  ;
058400131010         R88olrnK=cosolrnK  ;
058500130904         endif ;
058600131003
058700131003       // Tolgo dalle linee previste le linee non ancora partite
058800131003         w003a='999'  ;
058900131003
059000140318 1       if   do0cap<>*blanks ;
059100131003         clear                   kloc       ;
059200131003         clear                   kcap       ;
059300131003
059400131003         yy =  1   ;
059500140318 2       dow lna_prev(yy)>0  ;
059600131003         // escludo linee 888 e 999  ;
059700140318 3       if lna_prev(YY) <> 888 and lna_prev(yy) <> 999  ;
059800131003
059900131216         chain(n)  (do0ver:do0nar:kcap:kloc:lna_prev(YY):'C':'P ') azcos03l ;
0600001403185              if  %found(azcos03l)  ;
060100131216                lna_sog(yy) ='S' ;
0602001403185              endif  ;
060300131216
0604001403183         endif  ;
060500131003
060600131003         yy=yy+1  ;
0607001403182        enddo  ;
0608001403181         endif  ;
060900131003
061000130904      /end-free
061100130820     c                   ENDSR
061200130827     c*--------------------------------------------------------------
061300130827     c* Carico le 3 linee principali
061400130827     c*--------------------------------------------------------------
061500130827     c     Car_lna       BEGSR
061600130827     c                   z-add     1             yy                3 0
061700130827     c                   z-add     elalna        lna_prev(yy)
061800131213     c* imposto soglia presente per 888, 9999 e per linea
061900131213     c                   if        elalna=888 or elalna=999
062000131213     c                             or wsoglia='S'
062100131213     c                   eval      lna_sog(yy)='S'
062200131213     c                   endif
062300131213
062400130827     c                   if        elalol>0
062500130827     c                   add       1             yy
062600130827     c                   z-add     elalol        lna_prev(yy)
062700140210     c                   movel     'S'           lna_lol(yy)
062800131213     c                   if        elalol=888 or elalol=999
062900131213     c                   eval      lna_sog(yy)='S'
063000131213     c                   endif
063100130827     c                   endif
063200130827     c                   if        elalos>0
063300130827     c                   add       1             yy
063400130827     c                   z-add     elalos        lna_prev(yy)
063500131213     c                   if        elalos=888 or elalos=999
063600131213     c                   eval      lna_sog(yy)='S'
063700131213     c                   endif
063800130827     c                   endif
063900131213
064000131213     c                   clear                   wsoglia
064100130827     c                   ENDSR
064200080703
064300130820      /free
064400130820
064500080206       //--------------------------------------------------------------
064600080206       //?Reperimento Dati del job (Utente/Operativi).
064700080206       //--------------------------------------------------------------
064800080206       BEGSR DatiJob;
064900080206
065000080206         in(E) §AzUte;
065100080206         if NOT %error;
065200080206           in(E) §DatiUte;
065300080206         endif;
065400080206         if %error or RSut = *blanks;
065500080206           clear TIBS34ds;
065600080206           tibs34r(tibs34ds);
065700080206           in §AzUte;
065800080206           in §DatiUte;
065900080206         endif;
066000140206         dute01=utefaf  ;
066100080206
066200080206       ENDSR;
066300130820       //--------------------------------------------------------------
066400130820       //?Carica AZCOS00F dalla località o dal cap se non presente
066500130820       //--------------------------------------------------------------
066600130820       BEGSR Carica_AZCOS ;
066700130821
066800130821        clear CAPostis;
066900130821        clear CAPostfs;
067000130821        clear cAPomiis;
067100130821        clear CAPomxfs;
067200131010        clear CAPolrsc;
067300131010        clear CAPolrnc;
067400131010        clear CAPolrnk;
067500130829        clear CAPprop ;
067600130829        clear CAPrifer;
067700130829        clear CAPvisual;
067800131213        clear CAPtrzs  ;
067900130821        clear v1himm  ;
068000130828        clear v1hforz ;
068100130829        clear v1hforzlna;
068200131213        protsoglia=*off  ;
068300131016        v1cprop='   |'   ;
068400130821
068500130827         if   tlna='?'   ;
068600130827         v1himm='E'  ;
068700130827         endif           ;
068800130827
068900130821         if klna= savlna;
069000130821         endif  ;
069100130821         savlna=klna  ;
069200130820
069300130820       v1clna=klna  ;
069400130821       v1hlna=klna  ;
069500130828
069600130828       exsr Decodi_lna  ;
069700130820
069800130821       if ktser  ='R'   ;
069900131216          v1dtser='RITIRI' ;
070000130828          HI_dlna=*on  ;
070100130820       else  ;
070200131216          v1dtser='CONSEG' ;
070300130828          HI_dlna=*off ;
070400130820       endif  ;
070500130822       v1htser=ktser  ;
070600151204       v1hdoppiag=wdoppiag  ;
070700131213
070800131213       // Soglia
070900131213       if ksoglia='P'  ;
071000151204         if v1hdoppiag='S'  ;
071100151211           v1csogl  ='2Fa'  ;
071200151204            else             ;
071300151204           v1csogl  ='Pom'  ;
071400151204         endif  ;
071500151204
071600131213         else  ;
071700131213         clear v1csogl    ;
071800131213         endif  ;
071900131213
072000130820
072100130827       if Wrecord= 'N'    ;
072200130820        clear  v1costis  ;
072300130820        clear  v1costfs  ;
072400131016        clear  v1comiis  ;
072500131016        clear  v1comxfs  ;
072600131010        clear  v1colrsc  ;
072700131010        clear  v1colrnc  ;
072800131010        clear  v1colrnk  ;
072900130820        clear  v1ctrzs   ;
073000130820       else  ;
073100130820        v1costis=cosostis  ;
073200130820        v1costfs=cosostfs  ;
073300131016        v1comiis=cosomiis  ;
073400131016        v1comxfs=cosomxfs  ;
073500131010        v1colrsc=cosolrsc  ;
073600131010        v1colrnc=cosolrnc  ;
073700131010        v1colrnk=cosolrnk  ;
073800131213        v1ctrzs =costrzs   ;
073900131010
074000130827        select  ;
074100130827        when wrecord='C'  ;
074200131016        v1cprop='cap|'   ;
074300130827        when wrecord='F'  ;
074400131016        v1cprop='fil|'   ;
074500130827        when wrecord='9'  ;
074600131016        v1cprop='std|'   ;
074700130827        endsl  ;
074800130821
074900130820       endif ;
075000130820
075100130829       if Immiss    =*on  ;
075200130829        v1himm='S'  ;
075300130829       else  ;
075400130821       v1cute=cosute  ;
075500130821        if cosduv>0  ;
075600130821        dataiso=%date(cosduv)  ;
075700130821        datadmy=dataiso ;
075800130821        v1dduv=%dec(datadmy) ;
075900130821        endif  ;
076000130828
076100130902        // visto che la linea on è prevista e mi si sposiziona nella lettura del file
076200130902        // non eseguo la ricerca del riferimento
076300130902         if   tlna<>'?'   ;
076400130902          exsr cerca_rif ;
076500130902          else  ;
076600130902          wrecord='N' ;
076700130902         endif  ;
076800130902        endif  ;
076900130828
077000130902       // Se ho trovato il record memorizzo i dati
077100130828        if wrecord<>'N'    ;
077200130828        CAPostis=cosostis  ;
077300130828        CAPostfs=cosostfs  ;
077400130828        cAPomiis=cosomiis  ;
077500130828        CAPomxfs=cosomxfs  ;
077600131010        CAPolrsc=cosolrsc  ;
077700131010        CAPolrnc=cosolrnc  ;
077800131010        CAPolrnk=cosolrnk  ;
077900130828        CAPrifer=wrecord   ;
078000131213        CAPtrzs =costrzs   ;
078100130829        if wrecord='9'  ;
078200131213         CAPprop='OrRif:STD'  ;
078300130829         endif ;
078400130829        if wrecord='F'  ;
078500131213         CAPprop='OrRif:FIL'  ;
078600130829         endif ;
078700130829        if wrecord='C'  ;
078800131213         CAPprop='OrRif:CAP'  ;
078900130829         endif ;
079000130829
079100130828        else  ;
079200130828        clear CAPostis;
079300130828        clear CAPostfs;
079400130828        clear CAPomiis;
079500130828        clear CAPomxfs;
079600131010        clear CAPolrsc;
079700131010        clear CAPolrnc;
079800131010        clear CAPolrnk;
079900130829        clear CAPprop ;
080000131213        clear CAPtrzs ;
080100130828        CAPrifer='N'  ;
080200130828        endif  ;
080300130821
080400130829       exsr Protcampi    ;
080500130829
080600130820       s01nrr=s01nrr + 1 ;
080700130820       write sio2s01     ;
080800130820
080900130820       ENDSR;
081000080206
081100130828       //--------------------------------------------------------------
081200130828       //?Decodifica linea di arrivo
081300130828       //--------------------------------------------------------------
081400130828       BEGSR Decodi_lna  ;
081500151209       clear wdoppiag ;
081600130828
081700130904       select  ;
081800130904       when klna=999  ;
081900131213        v1dlna='Orari Std   '  ;
082000130904       when klna=888  ;
082100131213        v1dlna='OrariContr.'  ;
082200130904        other ;
082300130828       chain klna  azorg01l  ;
082400130828       if %found(azorg01l)  ;
082500131216        if orgde5<>*blanks   ;
082600131216          v1dlna=orgde5  ;
082700131216          else  ;
082800131216          v1dlna=orgdes  ;
082900131216          endif  ;
083000131216
083100130828       else  ;
083200130828         v1dlna=*all'?'  ;
083300130828       endif  ;
083400151204
083500151204       // Verifico se invece che avere soglia POM ha soglia giorno "B/C"
083600151204          clear tibs02ds  ;
083700151204          t02tla=' '      ;
083800151204          t02mod='C'      ;
083900151204          t02cod='FLO'    ;
084000151209          t02ke1=%editc(klna:'X') ;
084100151204          TNTBE_RicercaControllo  (kpjba : tibs02ds);
084200151209          if t02err=' '   and t02atb=' '  ;
084300151204          wdoppiag='S'  ;
084400151204          endif  ;
084500151204
084600130904       endsl  ;
084700130828       ENDSR  ;
084800080206       //--------------------------------------------------------------
084900080627       //?Gestione SFL 01
085000080206       //--------------------------------------------------------------
085100080409       BEGSR GesS01;
085200080207
085300080207         // Inizializzazione videata
085400080409         if  $InzS01 = *on;
085500080409            exsr InzS01;
085600080703         // Posizionamento cursore
085700100507         // C01csr = 1;
085800080703           s01nrr = 1;
085900080703           sfldsp_n=*off;
086000080409            $InzS01  = *off;
086100080207         endif;
086200080207
086300080207
086400080207         // Emissione Testata e Piede con tasti funzionali abilitati
086500080207         if  errGenerico = *off;
086600131003         select   ;
086700131003         when §15ita<>'I'  ;
086800130822           sfldsp_n=*on      ;
086900130822           write  sio2D02;
087000131003
087100140206         when lnaprevuni=0 ;
087200131003           sfldsp_n=*on      ;
087300131003           write  sio2D03;
087400131003         other   ;
087500131003           write  sio2Z01;
087600131003           endsl  ;
087700080207         endif;
087800080703
087900080703         // Posizionamento cursore
088000080703         if  C01csr  > *zero;
088100080703           C01rcd = C01csr;
088200080703         endif  ;
088300080207
088400080207         // Emissione videata
088500130820         exfmt  sio2C01;
088600080410
088700080207         reset errMessage;
088800080207         reset errGenerico;
088900080702         clear Vscmsg;
089000130820         clear Errostis  ;
089100130820         clear Errostfs  ;
089200130820         clear Erromiis  ;
089300130820         clear Erromxfs  ;
089400131010         clear Errolrsc  ;
089500131010         clear Errolrnc  ;
089600131010         clear Errolrnk  ;
089700130820         clear ErrTrzs   ;
089800130821         clear Errlna    ;
089900130828         clear BL_ostis  ;
090000130828         clear BL_ostfs  ;
090100130828         clear BL_omiis  ;
090200130828         clear BL_omxfs  ;
090300131010         clear BL_olrsc  ;
090400131010         clear BL_olrnc  ;
090500131010         clear BL_olrnk  ;
090600131213         clear BL_trzs   ;
090700080207
090800080523 1       SELECT;
090900080207
091000080207         // - F3=Fine
091100080523 1         WHEN  dsp_aid = c_F03;
091200080409            $Fine = *on;
091300130820            Do0f03='1'   ;
091400130828
091500130829         // - F5=Ripristina orari di riferimento (disabilitato)
091600130829 1         // WHEN  dsp_aid = c_F05;
091700130829           // chain    c01csr sio2s01  ;
091800130829           // if %found     ;
091900130829           //  if v1costis<>CAPostis  ;
092000130829           //  v1costis=CAPostis  ;
092100130829           //  BL_ostis=*on  ;
092200130829           // endif  ;
092300130829           // if v1costfs<>CAPostfs  ;
092400130829           //  v1costfs=CAPostfs  ;
092500130829           //  BL_ostfs=*on  ;
092600130829           // endif  ;
092700130829           // if v1comiis<>CAPomiis  ;
092800130829           //  v1comiis=CAPomiis  ;
092900130829           //  BL_omiis=*on  ;
093000130829           // endif  ;
093100130829           // if v1comxfs<>CAPomxfs  ;
093200130829           //  v1comxfs=CAPomxfs  ;
093300130829           //  BL_omxfs=*on  ;
093400130829           // endif  ;
093500130829           // if v1ctrzs <>CAPtrzs   ;
093600130829           //  v1ctrzs =CAPtrzs   ;
093700130829           //  BL_trzs =*on  ;
093800130829           // endif  ;
093900130829           //  else  ;
094000130829            // cursore non ben posizionato : errore
094100130829           //  s01nrr=1  ;
094200130829           // chain    s01nrr sio2s01  ;
094300130829           // errlna  =*on ;
094400130829           // vscmsg = Msg(07);
094500130829           // endif         ;
094600130829
094700130829           // exsr aggiosfl      ;
094800130829
094900130829         // - F7=Visualizza orari di riferimento per tutte le righe
095000131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F07;
095100130829            yy=1           ;
095200130829            s01nrr=1  ;
095300130829            chain s01nrr    sio2s01    ;
095400130829
095500130829 0          dow %found    ;
095600130829
095700130829            if capvisual<>'S'  ;
095800130829            capvisual='S'  ;
095900130829            else  ;
096000130829            capvisual=' '  ;
096100130829            endif  ;
096200130829
096300130829            exsr aggiosfl      ;
096400130829
096500130829            s01nrr=s01nrr+1     ;
096600130829
096700130829            chain s01nrr    sio2s01    ;
096800130829           enddo  ;
096900080207
097000131213         // - F09=Abilita riga soglia
097100131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F09;
097200131213            yy=1           ;
097300131213            s01nrr=1  ;
097400131213            chain s01nrr    sio2s01    ;
097500131213
097600131213 0          dow %found    ;
097700131213
097800151209            if v1csogl<>'   '    ;
097900131213            exsr protcampi  ;
098000131213
098100131213            exsr aggiosfl      ;
098200131216            AbilF9=*off        ;
098300131213            endif  ;
098400131213
098500131213            s01nrr=s01nrr+1     ;
098600131213
098700131213            chain s01nrr    sio2s01    ;
098800131213           enddo  ;
098900080207         // - F12=Ritorno
099000080523 1         WHEN  dsp_aid = c_F12;
099100080702            $Fine = *on;
099200130820            Do0f12='1'   ;
099300080414
099400131003         // - F16=Annullamento  (disabilito)
099500130829 1         WHEN  dsp_aid = c_F16;
099600130829         //  exsr annull_rec    ;
099700130829         //  $Fine = *on;
099800130829
099900080530 x1      // Invio / F6
100000080207           OTHER;
100100081006
100200081006        // Non effettuo controlli in interrogazione
100300131003 1           if orariInt=*off and sfldsp_n=*off ;
100400081006               exsr ContrS01 ;
100500081006 2             if  errGenerico = *on;
100600081006                 leavesr;
100700081006 2             endif;
100800081006 2           endif;
100900080530
101000081023         // F6=conferma Aggiornamento  F10=anche stampa
101100081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
101200131003             if sfldsp_n=*off  ;
101300131003              exsr   ConfAggio ;
101400131003             endif  ;
101500081023
101600080704           $Fine = *on;
101700080530           endif  ;
101800080207
101900080523 1       ENDSL;
102000080207
102100080207       ENDSR;
102200080207
102300080526       //--------------------------------------------------------------
102400080702       //?controlli SFL01
102500080409       //--------------------------------------------------------------
102600080409       BEGSR ContrS01;
102700130821
102800130821       yy=1  ;
1029001308291      dow yy<=12;
103000130821       if lna_es(yy)='R'  ;
103100130821       clear lna_es(YY)  ;
103200130821       endif  ;
103300130821
103400130821       yy=yy+1  ;
1035001308291      enddo    ;
103600130821
103700081126       yy=1           ;
103800080703       s01nrr=1  ;
103900130820       chain s01nrr    sio2s01    ;
104000080703
104100081006 0     dow %found    ;
104200130820
104300130821       clear  v1dlna  ;
104400130821
104500130828       klna=v1clna ;
104600130828       exsr decodi_lna  ;
104700130828 1     if v1dlna=*all'?'        ;
104800130821          errlna  =*on ;
104900130821          vscmsg = Msg(08);
105000130821          EXSR AggioSFL   ;
105100130821          leavesr;
105200130821 1     endif  ;
105300130821
105400130828       // La linea deve essere una di quelle previste
105500130829       //  se non prevista dico che la annullerò
105600130828         Indx=%lookup(v1clna:lna_prev)  ;
1057001308291        if Indx=0  and v1hforzlna=' '  ;
105800130828
105900130821          errlna  =*on ;
106000130821          vscmsg = Msg(04);
106100130829          v1hforzlna='S'  ;
106200130821
106300130821          EXSR AggioSFL   ;
106400130821          leavesr;
106500130821 1     endif  ;
106600130821
106700130822 1     if v1hlna<>v1clna ;
106800130821
106900130821       // Se la riga è RITIRO  controllo il linea doppia e la memorizzo
107000130822  2     if v1htser='R'  ;
107100130821
107200130828         if lna_es(indx)<>' '  ;
107300130821          errlna  =*on ;
107400130821          vscmsg = Msg(05);
107500130821          EXSR AggioSFL   ;
107600130821          leavesr;
107700130821  x3      else  ;
107800130828          lna_es(indx)='R'   ;
107900130821   3      endif  ;
108000130821
108100130821        savlna=v1clna  ;
108200130821
108300130821  x2   else  ;
108400130821   3   if v1clna  <> savlna  ;
108500130821          errlna  =*on ;
108600130821          vscmsg = Msg(13);
108700130821          EXSR AggioSFL   ;
108800130821          leavesr;
108900130821  3    endif  ;
109000130821  2    endif  ;
109100130822  1    endif  ;
109200130821
109300130822       // O R A R I
109400130829       //  li controllo solo per le linee valide
109500130829
109600130829  1a   if indx>0 ;
109700131216
109800131216       // controllo Soglia solo se l'ho abilitata ;
109900131216  1b   if v1csogl= *blanks   or AbilF9=*off  ;
110000131216
110100130829       // Se per la linea di arrivo manca il record di filiale --> non posso inserire
110200130829       if do0cap<>*blanks ;
110300130829       clear kloc  ;
110400130829       clear kcap  ;
110500130829       ktser=v1htser  ;
110600131213           chain(n)  (do0ver:do0nar:kcap:kloc:v1clna:ktser)  azcos03l   ;
110700131213           if not %found(azcos03l)  ;
110800130829          errlna  =*on ;
110900130829          vscmsg = Msg(15);
111000130829          %subst(vscmsg:40:3)=%editc(v1clna:'X')  ;
111100130829          EXSR AggioSFL   ;
111200130829          leavesr;
111300130829           endif  ;
111400130829       endif  ;
111500130829
111600130829
111700130904       // Imposto gli orari in base al servizio
111800130904       if v1clna<>888     ;
111900130904       if v1htser='C'   ;
112000151209       if v1csogl<>'   '  ;
112100131216       W88ostis=C88Postis ;
112200131216       W88ostfs=C88Postfs ;
112300131216       W88omiis=C88Pomiis ;
112400131216       W88omxfs=C88Pomxfs ;
112500131216       W88olrsc=C88Polrsc ;
112600131216       W88olrNc=C88Polrnc ;
112700131216       W88olrNk=C88Polrnk ;
112800131216       W88trzs =C88Ptrzs  ;
112900131216       else  ;
113000130904       W88ostis=C88ostis ;
113100130904       W88ostfs=C88ostfs ;
113200130904       W88omiis=C88omiis ;
113300130904       W88omxfs=C88omxfs ;
113400131010       W88olrsc=C88olrsc ;
113500131010       W88olrNc=C88olrnc ;
113600131010       W88olrNk=C88olrnk ;
113700131216       W88trzs =C88trzs  ;
113800131216       endif  ;
113900131216
114000130904       else  ;
114100130904       W88ostis=r88ostis ;
114200130904       W88ostfs=r88ostfs ;
114300130904       W88omiis=r88omiis ;
114400130904       W88omxfs=r88omxfs ;
114500131010       W88olrsc=r88olrsc ;
114600131010       W88olrnc=r88olrnc ;
114700131010       W88olrnk=r88olrnk ;
114800130904       endif  ;
114900131010       endif  ;
115000131010
115100131011       if v1clna= 888 or w88ostfs=0000  ;
115200130904       W88ostis=0000 ;
115300130904       W88ostfs=2400 ;
115400130904       W88omiis=0000 ;
115500130904       W88omxfs=2400 ;
115600131010       W88olrsc=2400 ;
115700131010       W88olrnc=2400 ;
115800131010       W88olrnk=2400 ;
115900131216       W88trzs =0000 ;
116000130904       endif  ;
116100130904
116200131216
116300131011       // Orario inizio
116400130829 1     if  v1costis=0  ;
116500130820          errostis=*on ;
116600130820          vscmsg = Msg(02);
116700081003          EXSR AggioSFL   ;
116800081003          leavesr;
116900130820 1     endif  ;
117000130821
117100130821       wora=v1costis ;
117200130821       exsr contr_ora ;
117300130829 1     if vscmsg<>*blanks ;
117400130821          errostis=*on ;
117500130821          EXSR AggioSFL   ;
117600130821          leavesr;
117700130821 1     endif  ;
117800130904
117900130904       // no minore del limite previsto
118000130904       if v1costis<w88ostis  ;
118100130904          errostis=*on ;
118200130904          vscmsg = Msg(16);
118300130904          %subst(vscmsg:37:5)=%editw(w88ostis: '0 :  ')    ;
118400130904          EXSR AggioSFL   ;
118500130904          leavesr;
118600130904 1     endif  ;
118700131031       // Orario inizio consegne no maggiore tempo consegna per tipo servizio
118800140210       // non controllo per orari del pomeriggio  e se si tratta di linea oltre
118900140210       if v1htser='C' and do0cap<>*blanks and lna_lol(indx)<>'S'
119000140110          and v1csogl=*blanks              ;
119100140110
119200131031          exsr car_cpt;
119300131031          if v1costis>wcpt0in4;
119400131031             errostis=*on ;
119500131031             vscmsg = Msg(21);
119600131031             vscmsg=%trim(vscmsg)+' ' + %trim(wdtsp);
119700131031             EXSR AggioSFL   ;
119800131031             leavesr;
119900131031          endif;
120000131031       endif;
120100130821
120200130821
120300131011       // Orario fine
120400130820 1     if  v1costfs=0     ;
120500130820          errostfs=*on ;
120600130820          vscmsg = Msg(02);
120700130820          EXSR AggioSFL   ;
120800130820          leavesr;
120900130820 1     endif  ;
121000130821
121100130821       wora=v1costfs ;
121200130821       exsr contr_ora ;
121300130829 1     if vscmsg<>*blanks ;
121400130821          errostfs=*on ;
121500130821          EXSR AggioSFL   ;
121600130821          leavesr;
121700130821 1     endif  ;
121800130904       // no maggiore del limite previsto
121900130904       if v1costfs>w88ostfs  ;
122000130904          errostfs=*on ;
122100130904          vscmsg = Msg(17);
122200130904          %subst(vscmsg:37:5)=%editw(w88ostfs: '0 :  ')    ;
122300130904          EXSR AggioSFL   ;
122400130904          leavesr;
122500130904 1     endif  ;
122600130820
122700130820 1     if  v1costfs<v1costis  ;
122800130820          errostfs=*on ;
122900130820          vscmsg = Msg(03);
123000130820          EXSR AggioSFL   ;
123100130820          leavesr;
123200130820 1     endif  ;
123300131011
123400131016       if v1htser='C'  ;
123500131216       // Tempo oraganizzazione --> solo per CONSEGNE
123600131216 1     if  v1ctrzs<w88trzs   or v1ctrzs>1200 ;
123700131216          Errtrzs =*on ;
123800131216          vscmsg = Msg(22);
123900131216          %subst(vscmsg:37:5)=%editw(w88trzs:'0 :  ')  ;
124000131216          EXSR AggioSFL   ;
124100131216          leavesr;
124200131216 1     endif  ;
124300131216
124400131016       // Orario MINIMO  solo per CONSEGNE
124500131016 1     if  v1comiis=0  ;
124600131016          erromiis=*on ;
124700131016          vscmsg = Msg(02);
124800131016          EXSR AggioSFL   ;
124900131016          leavesr;
125000131016 1     endif  ;
125100131016
125200131016       wora=v1comiis ;
125300131016       exsr contr_ora ;
125400131016 1     if vscmsg<>*blanks ;
125500131016          erromiis=*on ;
125600131016          EXSR AggioSFL   ;
125700131016          leavesr;
125800131016 1     endif  ;
125900131016
126000131016       // no minore del limite previsto
126100131016       if v1comiis<w88omiis  ;
126200131016          erromiis=*on ;
126300131016          vscmsg = Msg(16);
126400131016          %subst(vscmsg:37:5)=%editw(w88omiis: '0 :  ')    ;
126500131016          EXSR AggioSFL   ;
126600131016          leavesr;
126700131016 1     endif  ;
126800131016
126900131016       // no maggiore dell'orario di inizio
127000131016       if v1comiis > v1costis  ;
127100131016          erromiis=*on ;
127200131016          vscmsg = Msg(11);
127300131016          EXSR AggioSFL   ;
127400131016          leavesr;
127500131016 1     endif  ;
127600131016
127700131016       // Orario MASSIMO
127800131016 1     if  v1comxfs=0     ;
127900131016          erromxfs=*on ;
128000131016          vscmsg = Msg(02);
128100131016          EXSR AggioSFL   ;
128200131016          leavesr;
128300131016 1     endif  ;
128400131016
128500131016       wora=v1comxfs ;
128600131016       exsr contr_ora ;
128700131016 1     if vscmsg<>*blanks ;
128800131016          erromxfs=*on ;
128900131016          EXSR AggioSFL   ;
129000131016          leavesr;
129100131016 1     endif  ;
129200131016       // no maggiore del limite previsto
129300131016       if v1comxfs>w88omxfs  ;
129400131016          erromxfs=*on ;
129500131016          vscmsg = Msg(17);
129600131016          %subst(vscmsg:37:5)=%editw(w88omxfs: '0 :  ')    ;
129700131016          EXSR AggioSFL   ;
129800131016          leavesr;
129900131016 1     endif  ;
130000131016
130100131016 1     if  v1comxfs<v1comiis  ;
130200131016          erromxfs=*on ;
130300131016          vscmsg = Msg(03);
130400131016          EXSR AggioSFL   ;
130500131016          leavesr;
130600131016 1     endif  ;
130700131016       // no minore   dell'orario di fine
130800131016       if v1comxfs < v1costfs  ;
130900131016          erromxfs=*on ;
131000131016          vscmsg = Msg(12);
131100131016          EXSR AggioSFL   ;
131200131016          leavesr;
131300131016 1     endif  ;
131400131016 1     endif  ;
131500131016
131600131011       if v1htser='R'  ;
131700131011
131800131011       // Orari limite ritiri: COMMISSIONATI
131900131011 1     if  v1colrsc=0     ;
132000131011          erroLRSC=*on ;
132100131011          vscmsg = Msg(02);
132200131011          EXSR AggioSFL   ;
132300131011          leavesr;
132400131011 1     endif  ;
132500131011
132600131011       wora=v1coLRSC ;
132700131011       exsr contr_ora ;
132800131011 1     if vscmsg<>*blanks ;
132900131011          erroLRSC=*on ;
133000131011          EXSR AggioSFL   ;
133100131011          leavesr;
133200131011 1     endif  ;
133300131011       // no maggiore del limite previsto
133400131011       if v1coLRSC>w88oLRSC  ;
133500131011          erroLRSC=*on ;
133600131011          vscmsg = Msg(17);
133700131011          %subst(vscmsg:37:5)=%editw(w88oLRSC: '0 :  ')    ;
133800131011          EXSR AggioSFL   ;
133900131011          leavesr;
134000131011 1     endif  ;
134100131011       // Orari limite ritiri: NON COMMISSIONATI
134200131011 1     if  v1colrNc=0     ;
134300131011          erroLRNC=*on ;
134400131011          vscmsg = Msg(02);
134500131011          EXSR AggioSFL   ;
134600131011          leavesr;
134700131011 1     endif  ;
134800131011
134900131011       wora=v1coLRNC ;
135000131011       exsr contr_ora ;
135100131011 1     if vscmsg<>*blanks ;
135200131011          erroLRNC=*on ;
135300131011          EXSR AggioSFL   ;
135400131011          leavesr;
135500131011 1     endif  ;
135600131011       // no maggiore del limite previsto
135700131011       if v1coLRNC>w88oLRNC  ;
135800131011          erroLRNC=*on ;
135900131011          vscmsg = Msg(17);
136000131011          %subst(vscmsg:37:5)=%editw(w88oLRNC: '0 :  ')    ;
136100131011          EXSR AggioSFL   ;
136200131011          leavesr;
136300131011 1     endif  ;
136400131011       // Orari limite ritiri: NON COMMISSIONATI CODIFICATI
136500131011 1     if  v1colrNK=0     ;
136600131011          erroLRNK=*on ;
136700131011          vscmsg = Msg(02);
136800131011          EXSR AggioSFL   ;
136900131011          leavesr;
137000131011 1     endif  ;
137100131011
137200131011       wora=v1coLRNK ;
137300131011       exsr contr_ora ;
137400131011 1     if vscmsg<>*blanks ;
137500131011          erroLRNK=*on ;
137600131011          EXSR AggioSFL   ;
137700131011          leavesr;
137800131011 1     endif  ;
137900131011       // no maggiore del limite previsto
138000131011       if v1coLRNK>w88oLRNK  ;
138100131011          erroLRNK=*on ;
138200131011          vscmsg = Msg(17);
138300131011          %subst(vscmsg:37:5)=%editw(w88oLRNK: '0 :  ')    ;
138400131011          EXSR AggioSFL   ;
138500131011          leavesr;
138600131011 1     endif  ;
138700131011
138800131011       // secondo orario >= al primo
138900131011       if v1colrnc<v1colrsc  ;
139000131011          erroLRNc=*on ;
139100131011          vscmsg = Msg(18);
139200131011          EXSR AggioSFL   ;
139300131011          leavesr;
139400131011       endif  ;
139500131011       // terzo orario >= al secondo
139600131011       if v1colrnk<v1colrnc  ;
139700131011          erroLRNk=*on ;
139800131011          vscmsg = Msg(19);
139900131011          EXSR AggioSFL   ;
140000131011          leavesr;
140100131011       endif  ;
140200131011
140300131011       endif  ;
140400130820
140500131216 1b    endif  ;
140600131216 1a    endif  ;
140700130821
140800130821       EXSR AggioSFL   ;
140900080703
141000080703       s01nrr=1+s01nrr  ;
141100130820       chain s01nrr    sio2s01    ;
141200081006 0     enddo         ;
141300081006
141400080409       ENDSR;
141500080409
141600130829       //--------------------------------------------------------------
141700130829       //?Annullamento record
141800130829       //--------------------------------------------------------------
141900130829       //BEGSR Annull_rec  ;
142000130829
142100130829       // Elaboro la schiera delle linee e deleto
142200130829       //yy=1   ;
142300130829       //dow lna_prev(YY)>0     ;
142400130829
142500130829       //klna=lna_prev(yy)     ;
142600130829       //ktser='R'  ;
142700131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
142800130829       //ktser='C'  ;
142900131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
143000130829
143100130829       //yy=yy+1  ;
143200130829       //enddo   ;
143300130829
143400130829       //ENDSR;
143500080703       //--------------------------------------------------------------
143600080703       //?Aggiornamento sfl
143700080703       //--------------------------------------------------------------
143800080703       BEGSR AGGIOSFL  ;
143900080703       if vscmsg<>*blanks   ;
144000080703       errMessage  = *on;
144100080703       errGenerico = *on;
144200080704       c01csr=s01nrr    ;
144300080703       endif    ;
144400080703
144500080703       exsr ProtCampi   ;
144600080703
144700130820       update  sio2s01  ;
144800080703       ENDSR   ;
144900130821       //--------------------------------------------------------------
145000130821       //?Controllo campo ORA
145100130821       //--------------------------------------------------------------
145200130821       BEGSR Contr_ORA ;
145300130821       if whh < 0  or whh > 24  ;
145400130821          vscmsg = Msg(09);
145500130821       endif  ;
145600130821       if wmm < 0  or wmm > 59  ;
145700130821          vscmsg = Msg(09);
145800130821       endif  ;
145900131011
146000131011       // Minuti arrotondati ai 15 minuti
146100131011       if wmm<>00 and wmm<>15 and wmm<>30 and wmm<>45  ;
146200131011          vscmsg = Msg(20);
146300131011       endif  ;
146400130821
146500130821       ENDSR ;
146600080207       //--------------------------------------------------------------
146700080627       //?Inizializzazione SFL01
146800080207       //--------------------------------------------------------------
146900080409       BEGSR InzS01;
147000080207
147100080207       // Pulizia subfile
147200080207         SflDsp_N    = *on;
147300080207         SflDspCtl_N = *on;
147400130820         write  sio2C01;
147500080207         SflDspCtl_N = *off;
147600080207         SflEnd      = *off;
147700080530
147800080530         clear C01rcd;
147900100507         C01csr=1;
148000080627         S01nrr=0 ;
148100080702         clear Vscmsg;
148200080207         errMessage  = *off;
148300080207         errGenerico = *off;
148400130820         immiss    =*off ;
148500130821         clear savlna ;
148600130821         clear yy    ;
148700130821         clear lna_es;
148800130821         clear Wrecord ;
148900130821         clear v1dduv  ;
149000130821         clear v1cute  ;
149100130821
149200130822         // Se la nazione no è italia --> non carico nulla e avviso che non esistono
149300130822         //  orari
149400130822
149500130827 1       if §15ita= 'I'  ;
149600130820
149700130821         // 1)
149800130821         // Leggo e carico eventuali linee non più previste dal cap/località
149900130821
150000130828 1a    if   do0cap<>*blanks   ;
150100130828
150200130829       Protlna=*off ;
150300131213       setll  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
150400131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
150500131213 2      dow  not %eof(azcos03l)  ;
150600130821          klna=coslna ;
150700130827          Indx=%lookup(coslna:lna_prev)  ;
150800130827 3      if indx=0 ;
150900130821          tlna='?'   ;
151000130829           ktser=costser   ;
151100130821          exsr   Leggi_cos ;
1512001308273       endif  ;
151300130821
151400131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
1515001308272      enddo  ;
1516001308281a     endif  ;
151700130821
151800130821         // 2)
151900130821         // Carico i record  già presenti
152000130829         Protlna=*on  ;
152100130902         clear tlna   ;
152200130820
152300130827           YY=1  ;
152400140206 2     //  dow lna_prev(yy)>0  ;
152500140206           for  yy=1 to %elem(lna_prev);
152600140206           if lna_prev(yy)>0;
152700130827
152800130827           klna=lna_prev(YY) ;
152900131213           chain  (do0ver:do0nar:do0cap:do0loc:klna)  azcos03l   ;
153000131213 3         if %found(azcos03l)  ;
153100130827           lna_es(YY)='S'  ;
153200130830           endif  ;
153300131010
153400131213           if klna=999  or klna=888 ;
153500131213            indlna9988=*on  ;
153600131010            else  ;
153700131213            indlna9988=*off ;
153800131010            endif  ;
153900130830
154000131213           ksoglia=*blanks ;
154100131213           ktser='R'  ;
154200130821           exsr Leggi_cos  ;
154300130821           ktser='C'  ;
154400130821           exsr Leggi_cos  ;
154500131213           // Per consegne vedo se presente anche soglia "P"
154600131213
154700131216           if lna_sog(yy)='S'  or abilF9=*on  ;
154800131213           ksoglia='P'  ;
154900131213           exsr Leggi_cos  ;
155000131213           endif  ;
155100130827
155200140206       //  yy=yy+1  ;
155300140206           endif;
155400140206           endfor;
155500140206 2     //  enddo    ;
155600130821
1557001308271        endif  ;
155800130827
155900080702         ENDSR    ;
156000130821       //--------------------------------------------------------------
156100130821       //?Lettura record di AZCOS
156200130821       //--------------------------------------------------------------
156300130821           BEGSR Leggi_cos  ;
156400131213           chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia) azcos03l  ;
156500131213           if %found(azcos03l)  ;
156600130821           Immiss=*off ;
156700130821           wrecord='S'  ;
156800130828           else  ;
156900130821
157000130821           Immiss=*on  ;
157100130828           // Cerca il record di riferimento
157200130828           exsr cerca_rif  ;
157300130828
157400130828           endif  ;
157500130821
157600130821           exsr      Carica_AZCOS ;
157700131213
157800130821           ENDSR  ;
157900130828       //--------------------------------------------------------------
158000130828       //?Cerco record di riferimento per vedere se dati uguali o diversi
158100130828       //--------------------------------------------------------------
158200130828           BEGSR   Cerca_rif  ;
158300130828           wrecord='N'  ;
158400130828
158500130904           if klna<>999  and klna<>888 ;
158600130828
158700130829           if do0cap<>*blanks    ;
158800130829
158900130828 1         if do0loc<>*blanks   ;
159000130828            clear kloc  ;
159100131213           chain(n)  (do0ver:do0nar:do0cap:kloc:klna:ktser:ksoglia) azcos03l ;
159200131213           if %found(azcos03l)  ;
159300130828           wrecord='C'  ;
159400130828           endif  ;
159500130828 1         endif  ;
159600130828
159700130828           // Se non presente a livello di CAP provo a livello di filiale
159800130828 1         if wrecord='N'  ;
159900130828           clear kcap  ;
160000131213           chain(n)  (do0ver:do0nar:kcap:kloc:klna:ktser:ksoglia)  azcos03l   ;
160100131213           if %found(azcos03l)  ;
160200130828           wrecord='F'  ;
160300130828           endif  ;
160400130828 1         endif  ;
160500130829
160600130829 1         endif  ;
160700130828
160800131213           // standard aziendale
160900130828 1         if wrecord='N'  ;
161000130828           clear kcap  ;
161100131213           chain(n)  (do0ver:do0nar:kcap:kloc:lna999:ktser:ksoglia) azcos03l ;
161200131213           if %found(azcos03l)  ;
161300130828           wrecord='9'  ;
161400130828           endif  ;
161500130828 1         endif  ;
161600130828
161700130828 1         endif  ;
161800130828           ENDSR  ;
161900080702       //--------------------------------------------------------------
162000080702       //?Gestione protezione campi del sfl
162100080702       //--------------------------------------------------------------
162200080702          BEGSR   ProtCampi   ;
162300130820           Immiss  =*off ;
162400130829           vis_rifer=*off      ;
162500130829           BL_ostis=*off ;
162600130829           BL_ostfs=*off ;
162700130829           BL_omiis=*off ;
162800130829           BL_omxfs=*off ;
162900131010           BL_olrsc=*off ;
163000131010           BL_olrnc=*off ;
163100131010           BL_olrnK=*off ;
163200131213           BL_trzs =*off ;
163300130829           Protriga=*off ;
163400130829           Protlna=*on  ;
163500131216           Protsoglia=*off   ;
163600131213           IndLna9988=*off ;
163700080703
163800081007           // Se riga di categoria accendo solo protezione e sottolineatura
163900130821          if v1himm='S'    ;
164000130820           Immiss  =*on    ;
164100130820          endif            ;
164200130829
164300130829          // Protezine per linea di arrivo non prevista
164400130829          // Sempre protezione per interrogazione
164500130829          if v1himm='E'    or orariint=*on  ;
164600130829           Protriga=*on  ;
164700130821          endif            ;
164800130829          if v1himm='E' ;
164900130829           Protlna=*off ;
165000130829          endif            ;
165100130829
165200130828          if v1htser='R'  ;
165300130828          HI_dlna=*on  ;
165400130828          else  ;
165500130828          HI_dlna=*off ;
165600130828          endif  ;
165700131003
165800131010          // Se tipo servizio  ritiro non faccio vedere il tempo organizz
165900131003          if v1htser='R'  ;
166000131003          prot_tempo=*on  ;
166100131003          else ;
166200131003          prot_tempo=*off ;
166300131003          endif  ;
166400130821
166500130829          if CAPvisual= 'S'  ;
166600130829          vis_rifer=*on  ;
166700130829          endif  ;
166800131213
166900131213       // Se il record soglia c'è disabilito F9
167000151204       if v1csogl<>'   ' ;
167100131216       if CAPrifer='S' or dsp_aid = c_F09 or AbilF9=*off ;
167200131213        abilF9=*off ;
167300131213        protsoglia=*off   ;
167400131213       else   ;
167500131213        abilF9=*on  ;
167600131213        protsoglia=*on    ;
167700131213       endif   ;
167800131213       endif   ;
167900131010
168000131213          //if v1clna=999  ;
168100131213          // indlna999=*on  ;
168200131213          //endif  ;
168300130829
168400130829          // Evidenzio i campi diversi dal riferimento
168500131003          // solo se sono a livello di CAP/località
168600131003          // non per filiale o standard
168700131003          if do0cap<>*blanks    ;
168800131003
168900130829          if v1costis<>capostis ;
169000130829           BL_ostis=*on ;
169100130829          endif  ;
169200130829          if v1costfs<>capostfs ;
169300130829           BL_ostfs=*on ;
169400130829          endif  ;
169500131016          if v1comiis<>capomiis ;
169600131016           BL_omiis=*on ;
169700131016          endif  ;
169800131016          if v1comxfs<>capomxfs ;
169900131016           BL_omxfs=*on ;
170000131016          endif  ;
170100131010          if v1colrsc<>capolrsc ;
170200131010           BL_olrsc=*on ;
170300131010          endif  ;
170400131010          if v1colrnc<>capolrnc ;
170500131010           BL_olrnc=*on ;
170600131010          endif  ;
170700131010          if v1colrnk<>capolrnk ;
170800131010           BL_olrnk=*on ;
170900131010          endif  ;
171000131213          if v1ctrzs <>captrzs  ;
171100131213           BL_trzs =*on ;
171200131213          endif  ;
171300131010
171400131010
171500131003          endif  ;
171600130829
171700080702          ENDSR    ;
171800080702       //--------------------------------------------------------------
171900080702       //?conferma aggiornamento
172000080702       //--------------------------------------------------------------
172100080702        BEGSR ConfAGGIO              ;
172200081003
172300080704       s01nrr=1  ;
172400130820       chain s01nrr    sio2s01    ;
172500080704
172600080704 1     dow %found    ;
172700130821
172800130821 2     if  v1clna>0  ;
172900130821
173000140116       // Elaboro record soglia solo se
173100140116 2a    if v1csogl=*blanks or (abilF9=*off or caprifer='S')   ;
173200140116       Klna=v1hlna  ;
173300130828       ktser=v1htser  ;
173400151209       if v1csogl<>'   '  ;
173500151209       ksoglia='P'  ;
173600151209       else  ;
173700151209       clear ksoglia  ;
173800151209       endif   ;
173900151209       //   ksoglia=%subst(v1csogl:1:1)  ;
174000130829
174100131216       // Se la linea on è prevista--> la deleto
174200130829 3     if v1himm='E'   ;
174300131216         delete (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
174400130829 x3    else    ;
174500130829
174600131216         chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
174700130829       // se il record  è diventato uguale al record di riferimento
174800130829       //  lo deleto ma non per filiale
174900130829  4       if  CAPostis= v1costis   and
175000130828              CAPostfs= v1costfs   and
175100131016              CAPomiis= v1comiis   and
175200131016              CAPomxfs= v1comxfs   and
175300131010              CAPolrsc= v1colrsc   and
175400131010              CAPolrnc= v1colrnc   and
175500131010              CAPolrnk= v1colrnk   and
175600131216              CAPtrzs = v1ctrzs    and
175700130829              do0cap<>*blanks           ;
175800131213  5           if %found(azcos03l)  ;
175900130821               delete azcos000   ;
176000130829  5           endif  ;
176100130829  x4        else  ;
176200131213  5           if not %found(azcos03l)  ;
176300130821              clear azcos000  ;
176400130821              cosver=do0ver ;
176500130821              cosnar=do0nar ;
176600130821              coscap=do0cap ;
176700130821              cosloc=do0loc ;
176800130829  5           endif  ;
176900130821              cosostis= v1costis      ;
177000130821              cosostfs= v1costfs      ;
177100131016              cosomiis= v1comiis      ;
177200131016              cosomxfs= v1comxfs      ;
177300131010              cosolrsc= v1colrsc   ;
177400131010              cosolrnc= v1colrnc   ;
177500131010              cosolrnk= v1colrnk   ;
177600130821              costrzs = v1ctrzs         ;
177700130821              cosute  =knmus       ;
177800130821              cosduv=%dec(datasys)  ;
177900130821              costser=ktser  ;
178000130821              coslna=v1clna  ;
178100131216              cosserv=ksoglia ;
178200131213  5           if %found(azcos03l)  ;
178300130821               update azcos000   ;
178400130821               else  ;
178500130821               write  azcos000   ;
178600130829  5           endif  ;
178700130829  4        endif  ;
178800130829  3        endif  ;
178900140116 2a      endif  ;
179000140116 2       endif  ;
179100080704
179200080704        s01nrr=s01nrr+1 ;
179300130820        chain s01nrr    sio2s01    ;
179400080704  1     enddo           ;
179500080704
179600080702        ENDSR;
179700131031       //--------------------------------------------------------------
179800131031       //?Caricamento orario più basso del tipo tempo consegna
179900131031       //--------------------------------------------------------------
180000131031        BEGSR car_cpt                ;
180100131031        clear wcpt0in;
180200131031        clear wcpt0in4;
180300131031        clear wdtsp  ;
180400131031        clear wtsp   ;
180500131031        setll (do0ver:wttc) azcpt01l   ;
180600131031        reade (do0ver:wttc) azcpt01l   ;
180700131031        dow not %eof(azcpt01l);
180800131031           if cpt0in<wcpt0in or wcpt0in=0;
180900131031              wcpt0in=cpt0in;
181000131031              wtsp   =cpttsp;
181100131031           endif;
181200131031           reade (do0ver:wttc) azcpt01l   ;
181300131031        enddo;
181400131031        // decodifico il tipo servizio memorizzato
181500131031        clear ds5e;
181600131031        chain (1:'5E':wtsp) tabel00f;
181700131031        if %found(tabel00f);
181800131031           ds5e=tbluni;
181900131031        endif;
182000131031        wdtsp=§5ed08;
182100131031        wcpt0in4=wcpt0in*100;
182200131031        endsr;
182300080702
182400080206       //--------------------------------------------------------------
182500080206       //?Operazioni finali.
182600080206       //--------------------------------------------------------------
182700080206       BEGSR RoutEnd;
182800080627
182900080704         // Chiusura pgm   ;
183000151209          clear tibs02ds  ;
183100151209          t02tla='C'      ;
183200151209          TNTBE_RicercaControllo  (kpjba : tibs02ds);
183300080704
183400080206         *inLR = *on;
183500080704
183600080206         return;
183700080206
183800080206       ENDSR;
183900080206
184000131003
184100080206      /end-free
184200080206
184300080206       //--------------------------------------------------------------
184400080206       //?Schiere a tempo di compilazione.
184500080206       //--------------------------------------------------------------
184600080206
184700080410** - MSG ------------------------------------------------------------------ -*
184800130821Non trovata località: impossibile caricare gli orari servizi!!                            1
184900130821Immettere un orario formalmente corretto                                                  2
185000130821L'orario di fine deve essere maggiore dell'orario iniziale                                3
185100130829Linea non prevista dal cap/località. Verrà eliminata. Enter per continuare                4
185200130829Linea già immessa                                                                         5
185300130828L'inserimento non è ECCEZIONE:orari = a quelli
185400130829Posizionarsi sulla riga di cui si voglio ripristinare gli orari e premere F05             7
185500130821Linea inesistente                                                                         8
185600130821Orario impostato errato                                                                   9
185700130821Non trovato CAP: impossibile caricare gli orari servizi!!                                10
185800130821L'orario minimo non può essere maggiore dell'orario di inizio standard                   11
185900130821L'orario massimo non può essere minore dell'orario di fine standard                      12
186000130822La linea servizio CONSEGNE deve essere uguale alla precedente servizio RITIRO            13
186100130827Non trovata FILIALE: impossibile caricare gli orari servizi!!                            14
186200130904Immettere PRIMA gli orari PER la LINEA xxx, prima di inserire per cap/loc                15
186300130904L'orario non può essere inferiore a xx:xx                                                16
186400130904L'orario non può essere maggiore di xx:xx                                                17
186500131011Orario limite ritiri NON commissionati DEVE essere > = al limite dei commissionati       18
186600131011Orario limite ritiri Cli Codificati DEVE essere > = al limite dei NON commissionati      19
186700131011Si possono inserire soltanto orari arrotondati ai quarti d'ora                           20
186800131031Orario inizio consegna non congruente con tempo di consegna del serv.                    21
186900131216Tempo oraganizzazione non minore di xx:xx  e non maggiore di 12:00                       22
