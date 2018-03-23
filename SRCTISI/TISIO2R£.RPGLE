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
001300130827     fAZcpe01l  if   e           k disk
001400131031     fAZcpt01l  if   e           k disk
001500131213     fAZCOS03l  uf a e           k disk
001600130820     fAZORG01L  if   e           k disk
001700130821     ftabel00f  if   e           k disk
001800130820     fTISIO2D   cf   e             workstn indds(IndDspF)
001900080206     f                                     infds(InfDspF)
002000130821     f                                     sfile(sio2S01 : S01nrr)
002100080206      //---------------------------------------------------------------
002200080207      // - Tasti funzionali a video
002300080207     d c_F01           c                   const(x'31')
002400080207     d c_F02           c                   const(x'32')
002500080207     d c_F03           c                   const(x'33')
002600080207     d c_F05           c                   const(x'35')
002700080207     d c_F06           c                   const(x'36')
002800080207     d c_F07           c                   const(x'37')
002900080207     d c_F08           c                   const(x'38')
003000080207     d c_F09           c                   const(x'39')
003100080207     d c_F10           c                   const(x'3A')
003200080207     d c_F12           c                   const(x'3C')
003300080207     d c_F13           c                   const(x'B1')
003400080207     d c_F14           c                   const(x'B2')
003500080207     d c_F15           c                   const(x'B3')
003600080207     d c_F16           c                   const(x'B4')
003700080207     d c_F17           c                   const(x'B5')
003800080207     d c_F18           c                   const(x'B6')
003900080207     d c_F19           c                   const(x'B7')
004000080207     d c_F20           c                   const(x'B8')
004100080207     d c_F21           c                   const(x'B9')
004200080207     d c_F22           c                   const(x'BA')
004300080207     d c_F23           c                   const(x'BB')
004400080207     d c_F24           c                   const(x'BC')
004500080207     d c_Enter         c                   const(x'F1')
004600080207     d c_RollDown      c                   const(x'F4')
004700080207     d c_RollUp        c                   const(x'F5')
004800080214
004900080214      // - Attributi di visualizzazione
005000080215      //  -  DspAtr() - Normale
005100080214     d C_dspatr_Norm   c                   const(x'20')
005200080215      //  -  DspAtr(RI)
005300080214     d C_dspatr_RI     c                   const(x'21')
005400080215      //  -  DspAtr(ND)
005500080214     d C_dspatr_ND     c                   const(x'27')
005600080215      //  -  DspAtr(BL) / Color(Red)
005700080214     d C_dspatr_BL     c                   const(x'28')
005800080206
005900080206      //---------------------------------------------------------------
006000080206      //?Definizione schiere.
006100080206      //---------------------------------------------------------------
006200080206
006300080206      // - Messaggi di errore
006400130821     d MSG             s             78    dim(25) ctdata perrcd(1)
006500130905     d lna_ES          s              1    dim(14)
006600131213     d lna_SOG         s              1    dim(14)
006700140210     d lna_LOL         s              1    dim(14)
006800140206     d                 ds
006900140206     d lnaprevuni                    42  0
007000140206     d lna_prev                       3  0 dim(14)overlay(lnaprevuni)
007100140206     d*lna_prev        s              3  0 dim(14)
007200081126
007300080206      //---------------------------------------------------------------
007400080206      //?Definizione aree dati.
007500080206      //---------------------------------------------------------------
007600100226
007700080206      // - Dati utente
007800080206     d §AzUte        e ds                  extname(AZUTE00F)
007900080206     d                                     dtaara
008000080206     d §DatiUte      e ds                  extname(dDatiUte)
008100080206     d                                     dtaara
008200080206
008300080206      //---------------------------------------------------------------
008400080206      //?Definizione strutture dati.
008500080206      //---------------------------------------------------------------
008600080619     D CPO01           DS
008700080619     D  cp1NRR               397    400B 0
008800080206
008900080206      // - Status
009000080206     d Psds           sds
009100080206     d   SDSpgm          *proc
009200080206
009300080206      // - InfDS
009400080206     d InfDspF         ds
009500080207     d  dsp_aid              369    369a                                        AID byte
009600080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
009700080207     d  min_nrr              378    379i 0                                      Subfile min rrn
009800080207     d  num_rcds             380    381i 0                                      Subfile num rcds
009900080206
010000080206      // - Indicatori su DspF
010100080208     d IndDspF         ds
010200080206        // - Indicatori di gestione del subfile
010300080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010400080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010500080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010600080206     d  SflEnd                        1n   overlay(IndDspF : 33)
010700080206        // - Indicatori di errore
010800080206     d  errMessage                    1n   overlay(IndDspF : 28)
010900080606     d  errGenerico                   1n   overlay(IndDspF : 99)
011000080627        // - Indicatori vari
011100130829     d  protriga                      1n   overlay(IndDspF : 01)
011200130829     d  orariINT                      1n   overlay(IndDspF : 02)
011300130820     d  Immiss                        1n   overlay(IndDspF : 03)
011400081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
011500120111     d  F03Attivo                     1n   overlay(IndDspF : 05)
011600120111     d  F12Attivo                     1n   overlay(IndDspF : 06)
011700130828     d  Vis_CAP                       1n   overlay(IndDspF : 08)
011800130828     d  Vis_LOC                       1n   overlay(IndDspF : 09)
011900130829     d  Vis_rifer                     1n   overlay(IndDspF : 10)
012000131003     d  prot_tempo                    1n   overlay(IndDspF : 11)
012100131003     d  abilF7                        1n   overlay(IndDspF : 12)
012200131213     d  abilF9                        1n   overlay(IndDspF : 13)
012300130828     d  HI_dlna                       1n   overlay(IndDspF : 14)
012400130828     d  ProtLNA                       1n   overlay(IndDspF : 15)
012500131213     d  indLna9988                    1n   overlay(IndDspF : 16)
012600131213     d  protSoglia                    1n   overlay(IndDspF : 17)
012700130820     d  ErrOSTIS                      1n   overlay(IndDspF : 40)
012800130821     d  ErrOSTFS                      1n   overlay(IndDspF : 41)
012900130820     d  ErrOMIIS                      1n   overlay(IndDspF : 42)
013000130820     d  ErrOMXFS                      1n   overlay(IndDspF : 43)
013100130820     d  ErrTRZS                       1n   overlay(IndDspF : 44)
013200130821     d  ErrLNA                        1n   overlay(IndDspF : 45)
013300131010     d  ErrOlrsc                      1n   overlay(IndDspF : 47)
013400131010     d  ErrOlrnc                      1n   overlay(IndDspF : 48)
013500131010     d  ErrOlrnk                      1n   overlay(IndDspF : 49)
013600130828     d  BL_OSTIS                      1n   overlay(IndDspF : 50)
013700130828     d  BL_OSTFS                      1n   overlay(IndDspF : 51)
013800130828     d  BL_OMIIS                      1n   overlay(IndDspF : 52)
013900130828     d  BL_OMXFS                      1n   overlay(IndDspF : 53)
014000131010     d  BL_OLRSC                      1n   overlay(IndDspF : 55)
014100131010     d  BL_OLRNC                      1n   overlay(IndDspF : 56)
014200131010     d  BL_OLRNK                      1n   overlay(IndDspF : 57)
014300131213     d  BL_TRZS                       1n   overlay(IndDspF : 58)
014400080206
014500080206      // - Parametri ricevuti
014600080206     d KPJBA         e ds
014700080702
014800130820     d tisio0ds      e ds
014900080206
015000080206      // - Reperimento dati utente
015100080206     d TIBS34ds      e ds
015200080206     d dUte01        e ds
015300080702     d DLAT          e ds
015400130821     d ds15          e ds
015500131031     d ds5e          e ds
015600080702
015700080206
015800080206      // - Ricerca/Controllo tabelle
015900080206     d TIBS02ds      e ds                  inz
016000080627
016100080206      //---------------------------------------------------------------
016200080206      //?Definizione variabili globali.
016300080206      //---------------------------------------------------------------
016400080206
016500080206      // - Flags booleani
016600080208     d $Fine           s               n   inz(*off)
016700080208     d $InzS01         s               n   inz(*on)
016800120105     d wAumenta10      s               n   inz(*off)
016900120510     d wAumenta10f     s               n   inz(*off)
017000120110     d wAzzera10       s               n   inz(*off)
017100120105     d wDiminuisci10   s               n   inz(*off)
017200120109     d wForzaAgg       s               n   inz(*off)
017300080206
017400080627     d $Video          s              2    inz('S1')
017500080208     d S01nrr          s              4  0 inz
017600080627     d Indx            s              3  0 inz
017700081126     d yy              s              3  0 inz
017800130821     d Wrighe          s              3  0 inz
017900120109     d wOggi           s              8  0 inz
018000130820     d Tlna            s              1    inz
018100130821     d Wrecord         s              1    inz
018200131213     d Wsoglia         s              1    inz
018300151209     d Wdoppiag        s              1    inz
018400131003     d W003A           s              3    inz
018500130821     d klna            s                   like(coslna)
018600130827     d lna999          s                   like(coslna) inz(999)
018700130821     d savlna          s                   like(coslna)
018800130820     d Elalna          s                   like(coslna)
018900130820     d Elalol          s                   like(coslna)
019000130820     d Elalos          s                   like(coslna)
019100130820     d kloc            s                   like(cosloc)
019200130827     d kcap            s                   like(coscap)
019300130820     d ktser           s                   like(costser)
019400131213     d ksoglia         s                   like(cosserv)
019500130821     d ktbkut          s                   like(tblkut)
019600130821     d ktbcod          s                   like(tblcod)
019700130821     d ktbkey          s                   like(tblkey)
019800080414
019900080605     d Dataiso         s               d   datfmt(*iso)
020000130821     d Datasys         s               d   datfmt(*iso) inz(*sys)
020100080605     d Dataymd         s               d   datfmt(*ymd)
020200080605     d Datadmy         s               d   datfmt(*dmy)
020300130904     d C88ostis        s                   like(cosostis)
020400130904     d C88ostfs        s                   like(cosostfs)
020500130904     d C88omiis        s                   like(cosomiis)
020600130904     d C88omxfs        s                   like(cosomxfs)
020700131010     d C88olrsc        s                   like(cosolrsc)
020800131010     d C88olrnc        s                   like(cosolrnc)
020900131010     d C88olrnk        s                   like(cosolrnk)
021000131216     d C88trzs         s                   like(costrzs)
021100131216     d
021200131216     d C88Postis       s                   like(cosostis)
021300131216     d C88Postfs       s                   like(cosostfs)
021400131216     d C88Pomiis       s                   like(cosomiis)
021500131216     d C88Pomxfs       s                   like(cosomxfs)
021600131216     d C88Polrsc       s                   like(cosolrsc)
021700131216     d C88Polrnc       s                   like(cosolrnc)
021800131216     d C88Polrnk       s                   like(cosolrnk)
021900131216     d C88Ptrzs        s                   like(costrzs)
022000131010     d
022100130904     d R88ostis        s                   like(cosostis)
022200130904     d R88ostfs        s                   like(cosostfs)
022300130904     d R88omiis        s                   like(cosomiis)
022400130904     d R88omxfs        s                   like(cosomxfs)
022500131010     d R88olrsc        s                   like(cosolrsc)
022600131010     d R88olrnc        s                   like(cosolrnc)
022700131010     d R88olrnk        s                   like(cosolrnk)
022800131010     d
022900130904     d W88ostis        s                   like(cosostis)
023000130904     d W88ostfs        s                   like(cosostfs)
023100130904     d W88omiis        s                   like(cosomiis)
023200130904     d W88omxfs        s                   like(cosomxfs)
023300131010     d W88olrsc        s                   like(cosolrsc)
023400131010     d W88olrnc        s                   like(cosolrnc)
023500131010     d W88olrnk        s                   like(cosolrnk)
023600131216     d W88trzs         s                   like(costrzs)
023700131031
023800131031     d wttc            s                   like(cpcttc)
023900131031     d wcpt0in         s                   like(cpt0in)
024000131031     d wcpt0in4        s                   like(cosostis)
024100131031     d wdtsp           s                   like(§5ed08)
024200131031     d wtsp            s                   like(cpttsp)
024300131031
024400131031
024500080604     d
024600080605     d                 DS
024700080605     d  AA                     1      4  0
024800080605     d  MM                     5      6  0
024900080605     d  GG                     7      8  0
025000080605     d DATA                    1      8  0
025100130821     d                 DS
025200130821     d  wHH                    1      2  0
025300130821     d  wMM                    3      4  0
025400130821     d WORA                    1      4  0
025500081009     d
025600080206
025700080206      //---------------------------------------------------------------
025800080206      //?Definizione procedure usate.
025900080206      //---------------------------------------------------------------
026000080414      /copy gaitrasrc/srcprotopr,tibs02r
026100080414      /copy gaitrasrc/srcprotopr,tibs34r
026200080206
026300080206      //---------------------------------------------------------------
026400080206      //?Riepilogo indicatori.
026500080206      //---------------------------------------------------------------
026600080207      // - Comuni
026700080207      // 28    : Emissione messaggio di errore a video
026800080207      // - C01/S01
026900080207      // 30    : Condiziona SFLDSP    (*not)
027000080207      // 31    : Condiziona SFLDSPCTL (*not)
027100080207      // 30+31 : Condiziona SFLCLR
027200080207      // 32    : Condiziona SFLNXTCHG in S01
027300080207      // 50    : Errore: Opzione errata
027400080207      // - D01
027500080207      // - Comuni
027600080207      // 99    : Generico di Errore
027700080206      //---------------------------------------------------------------
027800080206
027900080206      //---------------------------------------------------------------
028000080206      //?M A I N - L I N E
028100080206      //---------------------------------------------------------------
028200080206
028300080627     c     *Entry        plist
028400080206     c                   parm                    KPJBA
028500130820     C                   PARM                    tiSIO0ds
028600130820     c                   clear                   do0err
028700130820     c                   clear                   do0msg
028800080702
028900080206      /free
029000080925
029100080206       // Operazioni iniziali
029200080206       exsr RoutInz;
029300080206
029400130820 2      if  Do0Err=' '    ;
029500081215
029600080206       // Gestione video
029700081215 3     DOW $Fine = *off;
029800081215 4       select;
029900080530
030000080530         // Videata di selezioni
030100080627           when $Video = 'S1';
030200080627             exsr GesS01;
030300080530
030400080702           other   ;
030500080206             $Fine = *on;
030600081215 4       endsl;
030700081215 3     ENDDO;
030800081215
030900081215 2     endif    ;
031000080206
031100080206       // Operazioni finali
031200080206       exsr RoutEnd;
031300080206
031400080206       //--------------------------------------------------------------
031500080206       //?Operazioni iniziali.
031600080206       //--------------------------------------------------------------
031700080206       BEGSR RoutInz;
031800131003
031900131003       AbilF7=*off  ;
032000131213       AbilF9=*off  ;
032100131213       IndLna9988=*off  ;
032200080627       $inzs01=*on;
032300080929       clear vsctes2         ;
032400131213       clear Wsoglia         ;
032500080703
032600080206         // Reperimento dati job
032700080206         exsr DatiJob;
032800081003
032900081003         // Se sono EDP accendo indicatore
033000081003         if %subst(knmus:1:3)<>'EDP'    ;
033100081003         UTENonEDP=*on   ;
033200130820         endif  ;
033300080627
033400080704         $Fine=*off   ;
033500080703
033600080702         // Vedo se modo gestione  o interrogazione
033700080703
033800130820         vscpgm='TISIO2R'   ;
033900080703
034000140206 1       if do0tric='G' and §utecap<>'I' ;
034100130830          vsctes2='  VARIAZIONE  '    ;
034200080929 1       endif    ;
034300130830
034400130830 1       if vsctes2= *blanks    ;
034500130830          orariInt =*on  ;
034600130830          vsctes2='INTERROGAZIONE'    ;
034700130830 1       endif;
034800130821
034900130821      /end-free
035000130820
035100130821     c*     Decodifico la nazione
035200130821     C                   MOVEL     1             KTBKUT
035300130821     C                   MOVEL     '15'          KTBCOD
035400130821     C                   MOVEL(P)  do0nar        KTBKEY
035500130821     C     KEYTAB        CHAIN     TABEL00F                           99
035600130821IF  1C     *IN99         IFEQ      *OFF
035700130821     C     TBLFLG        ANDEQ     *BLANKS
035800130821     C                   MOVEL     TBLUNI        DS15
035900130821     C                   MOVEL     §15DES        v1dnar
036000130821     c                   endif
036100130820
036200130821     C     KEYTAB        KLIST
036300130821     C                   KFLD                    KTBKUT                         *CODICE UTENTE
036400130821     C                   KFLD                    KTBCOD                         *TABELLA
036500130821     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
036600130820     C     KEYCPL        KLIST
036700130820     C                   KFLD                    Do0VER                         *VERSIONE
036800130820     C                   KFLD                    Do0NAR                         *NAZIONE
036900130820     C                   KFLD                    Do0CAP                         *CAP
037000130820     C                   KFLD                    KLOC                           *LOCALITA'
037100130820     C     KEYCPC        KLIST
037200130820     C                   KFLD                    Do0VER                         *VERSIONE
037300130820     C                   KFLD                    Do0NAR                         *NAZIONE
037400130820     C                   KFLD                    Do0CAP                         *CAP
037500130820
037600130822     c                   clear                   elalna
037700130822     c                   clear                   elalol
037800130822     c                   clear                   elalos
037900130827     c                   clear                   lna_prev
038000131213     c                   clear                   lna_es
038100131213     c                   clear                   lna_sog
038200140210     c                   clear                   lna_lol
038300131010     c* e la versione non è stata passata uso la data di riferimento per calcolarla
038400130828     c                   if        do0ver=0
038500130828     c
038600130828     c                   if        do0dri=0
038700130828     c                   movel     datasys       do0dri
038800130828     c                   endif
038900130828     C                   Z-ADD     Do0DRI        P96DRI            8 0
039000130828     C                   Z-ADD     *ZEROS        P96VER            5 0
039100130828     C                   CALL      'TISI96R'
039200130828     C                   PARM                    P96DRI
039300130828     C                   PARM                    P96VER
039400130828     c
039500130828     c                   eval      do0ver=p96ver
039600130828     c                   endif
039700130827     c*
039800130821     c* Caricamento dei dati della località passata
039900130827    0c                   if        Do0LOc<>*blanks
040000130828     c                   eval      Vis_LOC=*on
040100130828     c                   eval      Vis_CAP=*on
040200130820     C                   MOVEL     Do0LOC        KLOC
040300130820     C     KEYCPL        CHAIN     AZCPL01L
040400130820IF  1C                   IF        not %found(azcpl01l)
040500130820     C                   MOVEL     MSG(1)        Do0MSG                         *MESSAGGIO
040600130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
040700130827     c                   leavesr
040800130827   x1c                   else
040900130821     c* Se il livello di gestione è CAP--> prendo i dati da questo
041000130827    2c                   if        cplliv='L' and cpllna>0
041100130820     c                   movel     cpllna        elalna
041200140210    3c                   if        cpllos<>cpllna
041300130820     c                   movel     cpllos        elalos
041400130827    3c                   endif
041500140210    3c                   if        cpllol<>cpllna and cpllos<>cpllol
041600140210     c                   movel     cpllol        elalol
041700140210    3c                   endif
041800130821     c
041900130827     c                   exsr      car_lna
042000131031     c                   eval      wttc=cplttc
042100130827     c
042200130827    2c                   endif
042300130827    1C                   ENDIF
042400130820     c
042500130827    0c                   endif
042600130820
042700130827    0c                   if        (do0loc=*blanks and do0cap<>*blanks) or
042800130827     c                              cplliv='C' or (cplliv='L' and cpllna=0)
042900130828     c                   eval      Vis_CAP=*on
043000130820     C     KEYCPC        CHAIN     AZCPC01L
043100130821IF  1C                   IF        not %found(azcpc01l)
043200130821     C                   MOVEL     MSG(10)       Do0MSG                         *MESSAGGIO
043300130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
043400130827     c                   leavesr
043500130827    1c                   endif
043600130827
043700130820     c                   movel     cpclna        elalna
043800140210    1c                   if        cpclos<>cpclna
043900140210     c                   movel     cpclos        elalos
044000130827    1c                   endif
044100140210    1c                   if        cpclol<>cpclna and cpclos<>cpclol
044200140210     c                   movel     cpclol        elalol
044300130827    1C                   ENDIF
044400130827     c*
044500130827     c                   exsr      car_lna
044600131031     c                   eval      wttc=cpcttc
044700130827     c
044800130827     c* Carico eventuali eccezioni di linea se presenti
044900130827     C     KEYCPC        setll     AZCPe01L
045000130827     C     KEYCPC        reade     AZCPe01L
045100130827     c
045200130827    1c                   dow       not %eof(azcpe01l)
045300130827    2c                   if        cpelna>0
045400130827     c                   eval      indx=%lookup(cpelna:lna_prev)
045500130827    3c                   if        Indx=0
045600130905     c                   add       1             yy
045700130827     c                   z-add     cpelna        lna_prev(yy)
045800130827    3c                   endif
045900130827    2c                   endif
046000130827     c
046100130827    2c                   if        cpelos>0
046200130827     c                   eval      indx=%lookup(cpelos:lna_prev)
046300130827    3c                   if        Indx=0
046400130905     c                   add       1             yy
046500130827     c                   z-add     cpelos        lna_prev(yy)
046600130827    3c                   endif
046700130827    2c                   endif
046800140210     c
046900140210    2c                   if        cpelol>0
047000140210    3c                   eval      indx=%lookup(cpelol:lna_prev)
047100140210     c                   if        Indx=0
047200140210     c                   add       1             yy
047300140210     c                   z-add     cpelol        lna_prev(yy)
047400140210     c                   movel     'S'           lna_lol(yy)
047500140210    3c                   endif
047600140210    2c                   endif
047700130827     c
047800130827     C     KEYCPC        reade     AZCPe01L
047900130827    1c                   enddo
048000130827     c
048100130827    0C                   ENDIF
048200130827     c
048300130827     c* Se non passato il CAP ci deve essere la linea di arrivo (inserimento di filiale)
048400130827    0c                   if        do0loc=*blanks and do0cap=*blanks
048500130827     c* Se passato 999 --> orari standard
048600130827    1c                   if        do0lna=0
048700130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
048800130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
048900130827     c                   leavesr
049000130827    1c                   endif
049100131003
049200131003     c                   eval      AbilF7=*on
049300130827     c
049400130904    1c                   if        do0lna=999 or do0lna=888
049500130904     c                   eval      elalna=888
049600130904     c                   eval      elalol=999
049700131010     c
049800130827   x1c                   else
049900130827     c
050000130827     c     do0lna        chain     azorg01l
050100130827    2c                   if        not %found(azorg01l)
050200130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
050300130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
050400130827     c                   leavesr
050500130827    2c                   endif
050600130827     c                   eval      elalna=do0lna
050700131213     c
050800131213     c* Abilito tasto F9 se non presente orario soglia da filiale
050900131213      /free
051000131213         chain(n)  (do0ver:do0nar:kcap:kloc:elalna:'C':'P ') azcos03l ;
051100131213      /end-free
051200131213     c                   if        not %found(azcos03l)
051300140109     c* non abilito in interrogazione
051400140109     c                   if        OrariINT=*off
051500131213     c                   eval      AbilF9=*on
051600140109     c                   endif
051700140109     c
051800131213     c                   else
051900131213     c                   eval      wsoglia='S'
052000131213     c                   endif
052100131213     c
052200130827    1c                   endif
052300131213
052400130827     c                   exsr      car_lna
052500130827    0c                   endif
052600131213
052700130904     c* carico il record 888 per i controlli
052800130904     c                   clear                   kloc
052900130904     c                   clear                   kcap
053000130904      /free
053100130904         ktser='C'       ;
053200131216         clear ksoglia   ;
053300131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
053400131213         if %found(azcos03l)  ;
053500130904
053600130904         C88ostis=cosostis  ;
053700130904         C88ostfs=cosostfs  ;
053800130904         C88omiis=cosomiis  ;
053900130904         C88omxfs=cosomxfs  ;
054000131010         C88olrsc=cosolrsc  ;
054100131010         C88olrnc=cosolrnc  ;
054200131010         C88olrnk=cosolrnk  ;
054300131216         C88trzs =costrzs   ;
054400130904         endif ;
054500131216
054600131216         ksoglia='P ' ;
054700131216         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
054800131216         if %found(azcos03l)  ;
054900131216
055000131216         C88Postis=cosostis  ;
055100131216         C88Postfs=cosostfs  ;
055200131216         C88Pomiis=cosomiis  ;
055300131216         C88Pomxfs=cosomxfs  ;
055400131216         C88Polrsc=cosolrsc  ;
055500131216         C88Polrnc=cosolrnc  ;
055600131216         C88Polrnk=cosolrnk  ;
055700131216         C88Ptrzs =costrzs   ;
055800131216         endif ;
055900130904
056000131011         ktser='R'       ;
056100131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser)  azcos03l   ;
056200131213         if %found(azcos03l)  ;
056300130904
056400130904         R88ostis=cosostis  ;
056500130904         R88ostfs=cosostfs  ;
056600130904         R88omiis=cosomiis  ;
056700130904         R88omxfs=cosomxfs  ;
056800131010         R88olrsc=cosolrsc  ;
056900131010         R88olrnc=cosolrnc  ;
057000131010         R88olrnK=cosolrnK  ;
057100130904         endif ;
057200131003
057300131003       // Tolgo dalle linee previste le linee non ancora partite
057400131003         w003a='999'  ;
057500131003
057600140318 1       if   do0cap<>*blanks ;
057700131003         clear                   kloc       ;
057800131003         clear                   kcap       ;
057900131003
058000131003         yy =  1   ;
058100140318 2       dow lna_prev(yy)>0  ;
058200131003         // escludo linee 888 e 999  ;
058300140318 3       if lna_prev(YY) <> 888 and lna_prev(yy) <> 999  ;
058400131003
058500131216         chain(n)  (do0ver:do0nar:kcap:kloc:lna_prev(YY):'C':'P ') azcos03l ;
0586001403185              if  %found(azcos03l)  ;
058700131216                lna_sog(yy) ='S' ;
0588001403185              endif  ;
058900131216
0590001403183         endif  ;
059100131003
059200131003         yy=yy+1  ;
0593001403182        enddo  ;
0594001403181         endif  ;
059500131003
059600130904      /end-free
059700130820     c                   ENDSR
059800130827     c*--------------------------------------------------------------
059900130827     c* Carico le 3 linee principali
060000130827     c*--------------------------------------------------------------
060100130827     c     Car_lna       BEGSR
060200130827     c                   z-add     1             yy                3 0
060300130827     c                   z-add     elalna        lna_prev(yy)
060400131213     c* imposto soglia presente per 888, 9999 e per linea
060500131213     c                   if        elalna=888 or elalna=999
060600131213     c                             or wsoglia='S'
060700131213     c                   eval      lna_sog(yy)='S'
060800131213     c                   endif
060900131213
061000130827     c                   if        elalol>0
061100130827     c                   add       1             yy
061200130827     c                   z-add     elalol        lna_prev(yy)
061300140210     c                   movel     'S'           lna_lol(yy)
061400131213     c                   if        elalol=888 or elalol=999
061500131213     c                   eval      lna_sog(yy)='S'
061600131213     c                   endif
061700130827     c                   endif
061800130827     c                   if        elalos>0
061900130827     c                   add       1             yy
062000130827     c                   z-add     elalos        lna_prev(yy)
062100131213     c                   if        elalos=888 or elalos=999
062200131213     c                   eval      lna_sog(yy)='S'
062300131213     c                   endif
062400130827     c                   endif
062500131213
062600131213     c                   clear                   wsoglia
062700130827     c                   ENDSR
062800080703
062900130820      /free
063000130820
063100080206       //--------------------------------------------------------------
063200080206       //?Reperimento Dati del job (Utente/Operativi).
063300080206       //--------------------------------------------------------------
063400080206       BEGSR DatiJob;
063500080206
063600080206         in(E) §AzUte;
063700080206         if NOT %error;
063800080206           in(E) §DatiUte;
063900080206         endif;
064000080206         if %error or RSut = *blanks;
064100080206           clear TIBS34ds;
064200080206           tibs34r(tibs34ds);
064300080206           in §AzUte;
064400080206           in §DatiUte;
064500080206         endif;
064600140206         dute01=utefaf  ;
064700080206
064800080206       ENDSR;
064900130820       //--------------------------------------------------------------
065000130820       //?Carica AZCOS00F dalla località o dal cap se non presente
065100130820       //--------------------------------------------------------------
065200130820       BEGSR Carica_AZCOS ;
065300130821
065400130821        clear CAPostis;
065500130821        clear CAPostfs;
065600130821        clear cAPomiis;
065700130821        clear CAPomxfs;
065800131010        clear CAPolrsc;
065900131010        clear CAPolrnc;
066000131010        clear CAPolrnk;
066100130829        clear CAPprop ;
066200130829        clear CAPrifer;
066300130829        clear CAPvisual;
066400131213        clear CAPtrzs  ;
066500130821        clear v1himm  ;
066600130828        clear v1hforz ;
066700130829        clear v1hforzlna;
066800131213        protsoglia=*off  ;
066900131016        v1cprop='   |'   ;
067000130821
067100130827         if   tlna='?'   ;
067200130827         v1himm='E'  ;
067300130827         endif           ;
067400130827
067500130821         if klna= savlna;
067600130821         endif  ;
067700130821         savlna=klna  ;
067800130820
067900130820       v1clna=klna  ;
068000130821       v1hlna=klna  ;
068100130828
068200130828       exsr Decodi_lna  ;
068300130820
068400130821       if ktser  ='R'   ;
068500131216          v1dtser='RITIRI' ;
068600130828          HI_dlna=*on  ;
068700130820       else  ;
068800131216          v1dtser='CONSEG' ;
068900130828          HI_dlna=*off ;
069000130820       endif  ;
069100130822       v1htser=ktser  ;
069200151204       v1hdoppiag=wdoppiag  ;
069300131213
069400131213       // Soglia
069500131213       if ksoglia='P'  ;
069600151204         if v1hdoppiag='S'  ;
069700151211           v1csogl  ='2Fa'  ;
069800151204            else             ;
069900151204           v1csogl  ='Pom'  ;
070000151204         endif  ;
070100151204
070200131213         else  ;
070300131213         clear v1csogl    ;
070400131213         endif  ;
070500131213
070600130820
070700130827       if Wrecord= 'N'    ;
070800130820        clear  v1costis  ;
070900130820        clear  v1costfs  ;
071000131016        clear  v1comiis  ;
071100131016        clear  v1comxfs  ;
071200131010        clear  v1colrsc  ;
071300131010        clear  v1colrnc  ;
071400131010        clear  v1colrnk  ;
071500130820        clear  v1ctrzs   ;
071600130820       else  ;
071700130820        v1costis=cosostis  ;
071800130820        v1costfs=cosostfs  ;
071900131016        v1comiis=cosomiis  ;
072000131016        v1comxfs=cosomxfs  ;
072100131010        v1colrsc=cosolrsc  ;
072200131010        v1colrnc=cosolrnc  ;
072300131010        v1colrnk=cosolrnk  ;
072400131213        v1ctrzs =costrzs   ;
072500131010
072600130827        select  ;
072700130827        when wrecord='C'  ;
072800131016        v1cprop='cap|'   ;
072900130827        when wrecord='F'  ;
073000131016        v1cprop='fil|'   ;
073100130827        when wrecord='9'  ;
073200131016        v1cprop='std|'   ;
073300130827        endsl  ;
073400130821
073500130820       endif ;
073600130820
073700130829       if Immiss    =*on  ;
073800130829        v1himm='S'  ;
073900130829       else  ;
074000130821       v1cute=cosute  ;
074100130821        if cosduv>0  ;
074200130821        dataiso=%date(cosduv)  ;
074300130821        datadmy=dataiso ;
074400130821        v1dduv=%dec(datadmy) ;
074500130821        endif  ;
074600130828
074700130902        // visto che la linea on è prevista e mi si sposiziona nella lettura del file
074800130902        // non eseguo la ricerca del riferimento
074900130902         if   tlna<>'?'   ;
075000130902          exsr cerca_rif ;
075100130902          else  ;
075200130902          wrecord='N' ;
075300130902         endif  ;
075400130902        endif  ;
075500130828
075600130902       // Se ho trovato il record memorizzo i dati
075700130828        if wrecord<>'N'    ;
075800130828        CAPostis=cosostis  ;
075900130828        CAPostfs=cosostfs  ;
076000130828        cAPomiis=cosomiis  ;
076100130828        CAPomxfs=cosomxfs  ;
076200131010        CAPolrsc=cosolrsc  ;
076300131010        CAPolrnc=cosolrnc  ;
076400131010        CAPolrnk=cosolrnk  ;
076500130828        CAPrifer=wrecord   ;
076600131213        CAPtrzs =costrzs   ;
076700130829        if wrecord='9'  ;
076800131213         CAPprop='OrRif:STD'  ;
076900130829         endif ;
077000130829        if wrecord='F'  ;
077100131213         CAPprop='OrRif:FIL'  ;
077200130829         endif ;
077300130829        if wrecord='C'  ;
077400131213         CAPprop='OrRif:CAP'  ;
077500130829         endif ;
077600130829
077700130828        else  ;
077800130828        clear CAPostis;
077900130828        clear CAPostfs;
078000130828        clear CAPomiis;
078100130828        clear CAPomxfs;
078200131010        clear CAPolrsc;
078300131010        clear CAPolrnc;
078400131010        clear CAPolrnk;
078500130829        clear CAPprop ;
078600131213        clear CAPtrzs ;
078700130828        CAPrifer='N'  ;
078800130828        endif  ;
078900130821
079000130829       exsr Protcampi    ;
079100130829
079200130820       s01nrr=s01nrr + 1 ;
079300130820       write sio2s01     ;
079400130820
079500130820       ENDSR;
079600080206
079700130828       //--------------------------------------------------------------
079800130828       //?Decodifica linea di arrivo
079900130828       //--------------------------------------------------------------
080000130828       BEGSR Decodi_lna  ;
080100151209       clear wdoppiag ;
080200130828
080300130904       select  ;
080400130904       when klna=999  ;
080500131213        v1dlna='Orari Std   '  ;
080600130904       when klna=888  ;
080700131213        v1dlna='OrariContr.'  ;
080800130904        other ;
080900130828       chain klna  azorg01l  ;
081000130828       if %found(azorg01l)  ;
081100131216        if orgde5<>*blanks   ;
081200131216          v1dlna=orgde5  ;
081300131216          else  ;
081400131216          v1dlna=orgdes  ;
081500131216          endif  ;
081600131216
081700130828       else  ;
081800130828         v1dlna=*all'?'  ;
081900130828       endif  ;
082000151204
082100151204       // Verifico se invece che avere soglia POM ha soglia giorno "B/C"
082200151204          clear tibs02ds  ;
082300151204          t02tla=' '      ;
082400151204          t02mod='C'      ;
082500151204          t02cod='FLO'    ;
082600151209          t02ke1=%editc(klna:'X') ;
082700151204          TNTBE_RicercaControllo  (kpjba : tibs02ds);
082800151209          if t02err=' '   and t02atb=' '  ;
082900151204          wdoppiag='S'  ;
083000151204          endif  ;
083100151204
083200130904       endsl  ;
083300130828       ENDSR  ;
083400080206       //--------------------------------------------------------------
083500080627       //?Gestione SFL 01
083600080206       //--------------------------------------------------------------
083700080409       BEGSR GesS01;
083800080207
083900080207         // Inizializzazione videata
084000080409         if  $InzS01 = *on;
084100080409            exsr InzS01;
084200080703         // Posizionamento cursore
084300100507         // C01csr = 1;
084400080703           s01nrr = 1;
084500080703           sfldsp_n=*off;
084600080409            $InzS01  = *off;
084700080207         endif;
084800080207
084900080207
085000080207         // Emissione Testata e Piede con tasti funzionali abilitati
085100080207         if  errGenerico = *off;
085200131003         select   ;
085300131003         when §15ita<>'I'  ;
085400130822           sfldsp_n=*on      ;
085500130822           write  sio2D02;
085600131003
085700140206         when lnaprevuni=0 ;
085800131003           sfldsp_n=*on      ;
085900131003           write  sio2D03;
086000131003         other   ;
086100131003           write  sio2Z01;
086200131003           endsl  ;
086300080207         endif;
086400080703
086500080703         // Posizionamento cursore
086600080703         if  C01csr  > *zero;
086700080703           C01rcd = C01csr;
086800080703         endif  ;
086900080207
087000080207         // Emissione videata
087100130820         exfmt  sio2C01;
087200080410
087300080207         reset errMessage;
087400080207         reset errGenerico;
087500080702         clear Vscmsg;
087600130820         clear Errostis  ;
087700130820         clear Errostfs  ;
087800130820         clear Erromiis  ;
087900130820         clear Erromxfs  ;
088000131010         clear Errolrsc  ;
088100131010         clear Errolrnc  ;
088200131010         clear Errolrnk  ;
088300130820         clear ErrTrzs   ;
088400130821         clear Errlna    ;
088500130828         clear BL_ostis  ;
088600130828         clear BL_ostfs  ;
088700130828         clear BL_omiis  ;
088800130828         clear BL_omxfs  ;
088900131010         clear BL_olrsc  ;
089000131010         clear BL_olrnc  ;
089100131010         clear BL_olrnk  ;
089200131213         clear BL_trzs   ;
089300080207
089400080523 1       SELECT;
089500080207
089600080207         // - F3=Fine
089700080523 1         WHEN  dsp_aid = c_F03;
089800080409            $Fine = *on;
089900130820            Do0f03='1'   ;
090000130828
090100130829         // - F5=Ripristina orari di riferimento (disabilitato)
090200130829 1         // WHEN  dsp_aid = c_F05;
090300130829           // chain    c01csr sio2s01  ;
090400130829           // if %found     ;
090500130829           //  if v1costis<>CAPostis  ;
090600130829           //  v1costis=CAPostis  ;
090700130829           //  BL_ostis=*on  ;
090800130829           // endif  ;
090900130829           // if v1costfs<>CAPostfs  ;
091000130829           //  v1costfs=CAPostfs  ;
091100130829           //  BL_ostfs=*on  ;
091200130829           // endif  ;
091300130829           // if v1comiis<>CAPomiis  ;
091400130829           //  v1comiis=CAPomiis  ;
091500130829           //  BL_omiis=*on  ;
091600130829           // endif  ;
091700130829           // if v1comxfs<>CAPomxfs  ;
091800130829           //  v1comxfs=CAPomxfs  ;
091900130829           //  BL_omxfs=*on  ;
092000130829           // endif  ;
092100130829           // if v1ctrzs <>CAPtrzs   ;
092200130829           //  v1ctrzs =CAPtrzs   ;
092300130829           //  BL_trzs =*on  ;
092400130829           // endif  ;
092500130829           //  else  ;
092600130829            // cursore non ben posizionato : errore
092700130829           //  s01nrr=1  ;
092800130829           // chain    s01nrr sio2s01  ;
092900130829           // errlna  =*on ;
093000130829           // vscmsg = Msg(07);
093100130829           // endif         ;
093200130829
093300130829           // exsr aggiosfl      ;
093400130829
093500130829         // - F7=Visualizza orari di riferimento per tutte le righe
093600131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F07;
093700130829            yy=1           ;
093800130829            s01nrr=1  ;
093900130829            chain s01nrr    sio2s01    ;
094000130829
094100130829 0          dow %found    ;
094200130829
094300130829            if capvisual<>'S'  ;
094400130829            capvisual='S'  ;
094500130829            else  ;
094600130829            capvisual=' '  ;
094700130829            endif  ;
094800130829
094900130829            exsr aggiosfl      ;
095000130829
095100130829            s01nrr=s01nrr+1     ;
095200130829
095300130829            chain s01nrr    sio2s01    ;
095400130829           enddo  ;
095500080207
095600131213         // - F09=Abilita riga soglia
095700131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F09;
095800131213            yy=1           ;
095900131213            s01nrr=1  ;
096000131213            chain s01nrr    sio2s01    ;
096100131213
096200131213 0          dow %found    ;
096300131213
096400151209            if v1csogl<>'   '    ;
096500131213            exsr protcampi  ;
096600131213
096700131213            exsr aggiosfl      ;
096800131216            AbilF9=*off        ;
096900131213            endif  ;
097000131213
097100131213            s01nrr=s01nrr+1     ;
097200131213
097300131213            chain s01nrr    sio2s01    ;
097400131213           enddo  ;
097500080207         // - F12=Ritorno
097600080523 1         WHEN  dsp_aid = c_F12;
097700080702            $Fine = *on;
097800130820            Do0f12='1'   ;
097900080414
098000131003         // - F16=Annullamento  (disabilito)
098100130829 1         WHEN  dsp_aid = c_F16;
098200130829         //  exsr annull_rec    ;
098300130829         //  $Fine = *on;
098400130829
098500080530 x1      // Invio / F6
098600080207           OTHER;
098700081006
098800081006        // Non effettuo controlli in interrogazione
098900131003 1           if orariInt=*off and sfldsp_n=*off ;
099000081006               exsr ContrS01 ;
099100081006 2             if  errGenerico = *on;
099200081006                 leavesr;
099300081006 2             endif;
099400081006 2           endif;
099500080530
099600081023         // F6=conferma Aggiornamento  F10=anche stampa
099700081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
099800131003             if sfldsp_n=*off  ;
099900131003              exsr   ConfAggio ;
100000131003             endif  ;
100100081023
100200080704           $Fine = *on;
100300080530           endif  ;
100400080207
100500080523 1       ENDSL;
100600080207
100700080207       ENDSR;
100800080207
100900080526       //--------------------------------------------------------------
101000080702       //?controlli SFL01
101100080409       //--------------------------------------------------------------
101200080409       BEGSR ContrS01;
101300130821
101400130821       yy=1  ;
1015001308291      dow yy<=12;
101600130821       if lna_es(yy)='R'  ;
101700130821       clear lna_es(YY)  ;
101800130821       endif  ;
101900130821
102000130821       yy=yy+1  ;
1021001308291      enddo    ;
102200130821
102300081126       yy=1           ;
102400080703       s01nrr=1  ;
102500130820       chain s01nrr    sio2s01    ;
102600080703
102700081006 0     dow %found    ;
102800130820
102900130821       clear  v1dlna  ;
103000130821
103100130828       klna=v1clna ;
103200130828       exsr decodi_lna  ;
103300130828 1     if v1dlna=*all'?'        ;
103400130821          errlna  =*on ;
103500130821          vscmsg = Msg(08);
103600130821          EXSR AggioSFL   ;
103700130821          leavesr;
103800130821 1     endif  ;
103900130821
104000130828       // La linea deve essere una di quelle previste
104100130829       //  se non prevista dico che la annullerò
104200130828         Indx=%lookup(v1clna:lna_prev)  ;
1043001308291        if Indx=0  and v1hforzlna=' '  ;
104400130828
104500130821          errlna  =*on ;
104600130821          vscmsg = Msg(04);
104700130829          v1hforzlna='S'  ;
104800130821
104900130821          EXSR AggioSFL   ;
105000130821          leavesr;
105100130821 1     endif  ;
105200130821
105300130822 1     if v1hlna<>v1clna ;
105400130821
105500130821       // Se la riga è RITIRO  controllo il linea doppia e la memorizzo
105600130822  2     if v1htser='R'  ;
105700130821
105800130828         if lna_es(indx)<>' '  ;
105900130821          errlna  =*on ;
106000130821          vscmsg = Msg(05);
106100130821          EXSR AggioSFL   ;
106200130821          leavesr;
106300130821  x3      else  ;
106400130828          lna_es(indx)='R'   ;
106500130821   3      endif  ;
106600130821
106700130821        savlna=v1clna  ;
106800130821
106900130821  x2   else  ;
107000130821   3   if v1clna  <> savlna  ;
107100130821          errlna  =*on ;
107200130821          vscmsg = Msg(13);
107300130821          EXSR AggioSFL   ;
107400130821          leavesr;
107500130821  3    endif  ;
107600130821  2    endif  ;
107700130822  1    endif  ;
107800130821
107900130822       // O R A R I
108000130829       //  li controllo solo per le linee valide
108100130829
108200130829  1a   if indx>0 ;
108300131216
108400131216       // controllo Soglia solo se l'ho abilitata ;
108500131216  1b   if v1csogl= *blanks   or AbilF9=*off  ;
108600131216
108700130829       // Se per la linea di arrivo manca il record di filiale --> non posso inserire
108800130829       if do0cap<>*blanks ;
108900130829       clear kloc  ;
109000130829       clear kcap  ;
109100130829       ktser=v1htser  ;
109200131213           chain(n)  (do0ver:do0nar:kcap:kloc:v1clna:ktser)  azcos03l   ;
109300131213           if not %found(azcos03l)  ;
109400130829          errlna  =*on ;
109500130829          vscmsg = Msg(15);
109600130829          %subst(vscmsg:40:3)=%editc(v1clna:'X')  ;
109700130829          EXSR AggioSFL   ;
109800130829          leavesr;
109900130829           endif  ;
110000130829       endif  ;
110100130829
110200130829
110300130904       // Imposto gli orari in base al servizio
110400130904       if v1clna<>888     ;
110500130904       if v1htser='C'   ;
110600151209       if v1csogl<>'   '  ;
110700131216       W88ostis=C88Postis ;
110800131216       W88ostfs=C88Postfs ;
110900131216       W88omiis=C88Pomiis ;
111000131216       W88omxfs=C88Pomxfs ;
111100131216       W88olrsc=C88Polrsc ;
111200131216       W88olrNc=C88Polrnc ;
111300131216       W88olrNk=C88Polrnk ;
111400131216       W88trzs =C88Ptrzs  ;
111500131216       else  ;
111600130904       W88ostis=C88ostis ;
111700130904       W88ostfs=C88ostfs ;
111800130904       W88omiis=C88omiis ;
111900130904       W88omxfs=C88omxfs ;
112000131010       W88olrsc=C88olrsc ;
112100131010       W88olrNc=C88olrnc ;
112200131010       W88olrNk=C88olrnk ;
112300131216       W88trzs =C88trzs  ;
112400131216       endif  ;
112500131216
112600130904       else  ;
112700130904       W88ostis=r88ostis ;
112800130904       W88ostfs=r88ostfs ;
112900130904       W88omiis=r88omiis ;
113000130904       W88omxfs=r88omxfs ;
113100131010       W88olrsc=r88olrsc ;
113200131010       W88olrnc=r88olrnc ;
113300131010       W88olrnk=r88olrnk ;
113400130904       endif  ;
113500131010       endif  ;
113600131010
113700131011       if v1clna= 888 or w88ostfs=0000  ;
113800130904       W88ostis=0000 ;
113900130904       W88ostfs=2400 ;
114000130904       W88omiis=0000 ;
114100130904       W88omxfs=2400 ;
114200131010       W88olrsc=2400 ;
114300131010       W88olrnc=2400 ;
114400131010       W88olrnk=2400 ;
114500131216       W88trzs =0000 ;
114600130904       endif  ;
114700130904
114800131216
114900131011       // Orario inizio
115000130829 1     if  v1costis=0  ;
115100130820          errostis=*on ;
115200130820          vscmsg = Msg(02);
115300081003          EXSR AggioSFL   ;
115400081003          leavesr;
115500130820 1     endif  ;
115600130821
115700130821       wora=v1costis ;
115800130821       exsr contr_ora ;
115900130829 1     if vscmsg<>*blanks ;
116000130821          errostis=*on ;
116100130821          EXSR AggioSFL   ;
116200130821          leavesr;
116300130821 1     endif  ;
116400130904
116500130904       // no minore del limite previsto
116600130904       if v1costis<w88ostis  ;
116700130904          errostis=*on ;
116800130904          vscmsg = Msg(16);
116900130904          %subst(vscmsg:37:5)=%editw(w88ostis: '0 :  ')    ;
117000130904          EXSR AggioSFL   ;
117100130904          leavesr;
117200130904 1     endif  ;
117300131031       // Orario inizio consegne no maggiore tempo consegna per tipo servizio
117400140210       // non controllo per orari del pomeriggio  e se si tratta di linea oltre
117500140210       if v1htser='C' and do0cap<>*blanks and lna_lol(indx)<>'S'
117600140110          and v1csogl=*blanks              ;
117700140110
117800131031          exsr car_cpt;
117900131031          if v1costis>wcpt0in4;
118000131031             errostis=*on ;
118100131031             vscmsg = Msg(21);
118200131031             vscmsg=%trim(vscmsg)+' ' + %trim(wdtsp);
118300131031             EXSR AggioSFL   ;
118400131031             leavesr;
118500131031          endif;
118600131031       endif;
118700130821
118800130821
118900131011       // Orario fine
119000130820 1     if  v1costfs=0     ;
119100130820          errostfs=*on ;
119200130820          vscmsg = Msg(02);
119300130820          EXSR AggioSFL   ;
119400130820          leavesr;
119500130820 1     endif  ;
119600130821
119700130821       wora=v1costfs ;
119800130821       exsr contr_ora ;
119900130829 1     if vscmsg<>*blanks ;
120000130821          errostfs=*on ;
120100130821          EXSR AggioSFL   ;
120200130821          leavesr;
120300130821 1     endif  ;
120400130904       // no maggiore del limite previsto
120500130904       if v1costfs>w88ostfs  ;
120600130904          errostfs=*on ;
120700130904          vscmsg = Msg(17);
120800130904          %subst(vscmsg:37:5)=%editw(w88ostfs: '0 :  ')    ;
120900130904          EXSR AggioSFL   ;
121000130904          leavesr;
121100130904 1     endif  ;
121200130820
121300130820 1     if  v1costfs<v1costis  ;
121400130820          errostfs=*on ;
121500130820          vscmsg = Msg(03);
121600130820          EXSR AggioSFL   ;
121700130820          leavesr;
121800130820 1     endif  ;
121900131011
122000131016       if v1htser='C'  ;
122100131216       // Tempo oraganizzazione --> solo per CONSEGNE
122200131216 1     if  v1ctrzs<w88trzs   or v1ctrzs>1200 ;
122300131216          Errtrzs =*on ;
122400131216          vscmsg = Msg(22);
122500131216          %subst(vscmsg:37:5)=%editw(w88trzs:'0 :  ')  ;
122600131216          EXSR AggioSFL   ;
122700131216          leavesr;
122800131216 1     endif  ;
122900131216
123000131016       // Orario MINIMO  solo per CONSEGNE
123100131016 1     if  v1comiis=0  ;
123200131016          erromiis=*on ;
123300131016          vscmsg = Msg(02);
123400131016          EXSR AggioSFL   ;
123500131016          leavesr;
123600131016 1     endif  ;
123700131016
123800131016       wora=v1comiis ;
123900131016       exsr contr_ora ;
124000131016 1     if vscmsg<>*blanks ;
124100131016          erromiis=*on ;
124200131016          EXSR AggioSFL   ;
124300131016          leavesr;
124400131016 1     endif  ;
124500131016
124600131016       // no minore del limite previsto
124700131016       if v1comiis<w88omiis  ;
124800131016          erromiis=*on ;
124900131016          vscmsg = Msg(16);
125000131016          %subst(vscmsg:37:5)=%editw(w88omiis: '0 :  ')    ;
125100131016          EXSR AggioSFL   ;
125200131016          leavesr;
125300131016 1     endif  ;
125400131016
125500131016       // no maggiore dell'orario di inizio
125600131016       if v1comiis > v1costis  ;
125700131016          erromiis=*on ;
125800131016          vscmsg = Msg(11);
125900131016          EXSR AggioSFL   ;
126000131016          leavesr;
126100131016 1     endif  ;
126200131016
126300131016       // Orario MASSIMO
126400131016 1     if  v1comxfs=0     ;
126500131016          erromxfs=*on ;
126600131016          vscmsg = Msg(02);
126700131016          EXSR AggioSFL   ;
126800131016          leavesr;
126900131016 1     endif  ;
127000131016
127100131016       wora=v1comxfs ;
127200131016       exsr contr_ora ;
127300131016 1     if vscmsg<>*blanks ;
127400131016          erromxfs=*on ;
127500131016          EXSR AggioSFL   ;
127600131016          leavesr;
127700131016 1     endif  ;
127800131016       // no maggiore del limite previsto
127900131016       if v1comxfs>w88omxfs  ;
128000131016          erromxfs=*on ;
128100131016          vscmsg = Msg(17);
128200131016          %subst(vscmsg:37:5)=%editw(w88omxfs: '0 :  ')    ;
128300131016          EXSR AggioSFL   ;
128400131016          leavesr;
128500131016 1     endif  ;
128600131016
128700131016 1     if  v1comxfs<v1comiis  ;
128800131016          erromxfs=*on ;
128900131016          vscmsg = Msg(03);
129000131016          EXSR AggioSFL   ;
129100131016          leavesr;
129200131016 1     endif  ;
129300131016       // no minore   dell'orario di fine
129400131016       if v1comxfs < v1costfs  ;
129500131016          erromxfs=*on ;
129600131016          vscmsg = Msg(12);
129700131016          EXSR AggioSFL   ;
129800131016          leavesr;
129900131016 1     endif  ;
130000131016 1     endif  ;
130100131016
130200131011       if v1htser='R'  ;
130300131011
130400131011       // Orari limite ritiri: COMMISSIONATI
130500131011 1     if  v1colrsc=0     ;
130600131011          erroLRSC=*on ;
130700131011          vscmsg = Msg(02);
130800131011          EXSR AggioSFL   ;
130900131011          leavesr;
131000131011 1     endif  ;
131100131011
131200131011       wora=v1coLRSC ;
131300131011       exsr contr_ora ;
131400131011 1     if vscmsg<>*blanks ;
131500131011          erroLRSC=*on ;
131600131011          EXSR AggioSFL   ;
131700131011          leavesr;
131800131011 1     endif  ;
131900131011       // no maggiore del limite previsto
132000131011       if v1coLRSC>w88oLRSC  ;
132100131011          erroLRSC=*on ;
132200131011          vscmsg = Msg(17);
132300131011          %subst(vscmsg:37:5)=%editw(w88oLRSC: '0 :  ')    ;
132400131011          EXSR AggioSFL   ;
132500131011          leavesr;
132600131011 1     endif  ;
132700131011       // Orari limite ritiri: NON COMMISSIONATI
132800131011 1     if  v1colrNc=0     ;
132900131011          erroLRNC=*on ;
133000131011          vscmsg = Msg(02);
133100131011          EXSR AggioSFL   ;
133200131011          leavesr;
133300131011 1     endif  ;
133400131011
133500131011       wora=v1coLRNC ;
133600131011       exsr contr_ora ;
133700131011 1     if vscmsg<>*blanks ;
133800131011          erroLRNC=*on ;
133900131011          EXSR AggioSFL   ;
134000131011          leavesr;
134100131011 1     endif  ;
134200131011       // no maggiore del limite previsto
134300131011       if v1coLRNC>w88oLRNC  ;
134400131011          erroLRNC=*on ;
134500131011          vscmsg = Msg(17);
134600131011          %subst(vscmsg:37:5)=%editw(w88oLRNC: '0 :  ')    ;
134700131011          EXSR AggioSFL   ;
134800131011          leavesr;
134900131011 1     endif  ;
135000131011       // Orari limite ritiri: NON COMMISSIONATI CODIFICATI
135100131011 1     if  v1colrNK=0     ;
135200131011          erroLRNK=*on ;
135300131011          vscmsg = Msg(02);
135400131011          EXSR AggioSFL   ;
135500131011          leavesr;
135600131011 1     endif  ;
135700131011
135800131011       wora=v1coLRNK ;
135900131011       exsr contr_ora ;
136000131011 1     if vscmsg<>*blanks ;
136100131011          erroLRNK=*on ;
136200131011          EXSR AggioSFL   ;
136300131011          leavesr;
136400131011 1     endif  ;
136500131011       // no maggiore del limite previsto
136600131011       if v1coLRNK>w88oLRNK  ;
136700131011          erroLRNK=*on ;
136800131011          vscmsg = Msg(17);
136900131011          %subst(vscmsg:37:5)=%editw(w88oLRNK: '0 :  ')    ;
137000131011          EXSR AggioSFL   ;
137100131011          leavesr;
137200131011 1     endif  ;
137300131011
137400131011       // secondo orario >= al primo
137500131011       if v1colrnc<v1colrsc  ;
137600131011          erroLRNc=*on ;
137700131011          vscmsg = Msg(18);
137800131011          EXSR AggioSFL   ;
137900131011          leavesr;
138000131011       endif  ;
138100131011       // terzo orario >= al secondo
138200131011       if v1colrnk<v1colrnc  ;
138300131011          erroLRNk=*on ;
138400131011          vscmsg = Msg(19);
138500131011          EXSR AggioSFL   ;
138600131011          leavesr;
138700131011       endif  ;
138800131011
138900131011       endif  ;
139000130820
139100131216 1b    endif  ;
139200131216 1a    endif  ;
139300130821
139400130821       EXSR AggioSFL   ;
139500080703
139600080703       s01nrr=1+s01nrr  ;
139700130820       chain s01nrr    sio2s01    ;
139800081006 0     enddo         ;
139900081006
140000080409       ENDSR;
140100080409
140200130829       //--------------------------------------------------------------
140300130829       //?Annullamento record
140400130829       //--------------------------------------------------------------
140500130829       //BEGSR Annull_rec  ;
140600130829
140700130829       // Elaboro la schiera delle linee e deleto
140800130829       //yy=1   ;
140900130829       //dow lna_prev(YY)>0     ;
141000130829
141100130829       //klna=lna_prev(yy)     ;
141200130829       //ktser='R'  ;
141300131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
141400130829       //ktser='C'  ;
141500131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
141600130829
141700130829       //yy=yy+1  ;
141800130829       //enddo   ;
141900130829
142000130829       //ENDSR;
142100080703       //--------------------------------------------------------------
142200080703       //?Aggiornamento sfl
142300080703       //--------------------------------------------------------------
142400080703       BEGSR AGGIOSFL  ;
142500080703       if vscmsg<>*blanks   ;
142600080703       errMessage  = *on;
142700080703       errGenerico = *on;
142800080704       c01csr=s01nrr    ;
142900080703       endif    ;
143000080703
143100080703       exsr ProtCampi   ;
143200080703
143300130820       update  sio2s01  ;
143400080703       ENDSR   ;
143500130821       //--------------------------------------------------------------
143600130821       //?Controllo campo ORA
143700130821       //--------------------------------------------------------------
143800130821       BEGSR Contr_ORA ;
143900130821       if whh < 0  or whh > 24  ;
144000130821          vscmsg = Msg(09);
144100130821       endif  ;
144200130821       if wmm < 0  or wmm > 59  ;
144300130821          vscmsg = Msg(09);
144400130821       endif  ;
144500131011
144600131011       // Minuti arrotondati ai 15 minuti
144700131011       if wmm<>00 and wmm<>15 and wmm<>30 and wmm<>45  ;
144800131011          vscmsg = Msg(20);
144900131011       endif  ;
145000130821
145100130821       ENDSR ;
145200080207       //--------------------------------------------------------------
145300080627       //?Inizializzazione SFL01
145400080207       //--------------------------------------------------------------
145500080409       BEGSR InzS01;
145600080207
145700080207       // Pulizia subfile
145800080207         SflDsp_N    = *on;
145900080207         SflDspCtl_N = *on;
146000130820         write  sio2C01;
146100080207         SflDspCtl_N = *off;
146200080207         SflEnd      = *off;
146300080530
146400080530         clear C01rcd;
146500100507         C01csr=1;
146600080627         S01nrr=0 ;
146700080702         clear Vscmsg;
146800080207         errMessage  = *off;
146900080207         errGenerico = *off;
147000130820         immiss    =*off ;
147100130821         clear savlna ;
147200130821         clear yy    ;
147300130821         clear lna_es;
147400130821         clear Wrecord ;
147500130821         clear v1dduv  ;
147600130821         clear v1cute  ;
147700130821
147800130822         // Se la nazione no è italia --> non carico nulla e avviso che non esistono
147900130822         //  orari
148000130822
148100130827 1       if §15ita= 'I'  ;
148200130820
148300130821         // 1)
148400130821         // Leggo e carico eventuali linee non più previste dal cap/località
148500130821
148600130828 1a    if   do0cap<>*blanks   ;
148700130828
148800130829       Protlna=*off ;
148900131213       setll  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
149000131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
149100131213 2      dow  not %eof(azcos03l)  ;
149200130821          klna=coslna ;
149300130827          Indx=%lookup(coslna:lna_prev)  ;
149400130827 3      if indx=0 ;
149500130821          tlna='?'   ;
149600130829           ktser=costser   ;
149700130821          exsr   Leggi_cos ;
1498001308273       endif  ;
149900130821
150000131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
1501001308272      enddo  ;
1502001308281a     endif  ;
150300130821
150400130821         // 2)
150500130821         // Carico i record  già presenti
150600130829         Protlna=*on  ;
150700130902         clear tlna   ;
150800130820
150900130827           YY=1  ;
151000140206 2     //  dow lna_prev(yy)>0  ;
151100140206           for  yy=1 to %elem(lna_prev);
151200140206           if lna_prev(yy)>0;
151300130827
151400130827           klna=lna_prev(YY) ;
151500131213           chain  (do0ver:do0nar:do0cap:do0loc:klna)  azcos03l   ;
151600131213 3         if %found(azcos03l)  ;
151700130827           lna_es(YY)='S'  ;
151800130830           endif  ;
151900131010
152000131213           if klna=999  or klna=888 ;
152100131213            indlna9988=*on  ;
152200131010            else  ;
152300131213            indlna9988=*off ;
152400131010            endif  ;
152500130830
152600131213           ksoglia=*blanks ;
152700131213           ktser='R'  ;
152800130821           exsr Leggi_cos  ;
152900130821           ktser='C'  ;
153000130821           exsr Leggi_cos  ;
153100131213           // Per consegne vedo se presente anche soglia "P"
153200131213
153300131216           if lna_sog(yy)='S'  or abilF9=*on  ;
153400131213           ksoglia='P'  ;
153500131213           exsr Leggi_cos  ;
153600131213           endif  ;
153700130827
153800140206       //  yy=yy+1  ;
153900140206           endif;
154000140206           endfor;
154100140206 2     //  enddo    ;
154200130821
1543001308271        endif  ;
154400130827
154500080702         ENDSR    ;
154600130821       //--------------------------------------------------------------
154700130821       //?Lettura record di AZCOS
154800130821       //--------------------------------------------------------------
154900130821           BEGSR Leggi_cos  ;
155000131213           chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia) azcos03l  ;
155100131213           if %found(azcos03l)  ;
155200130821           Immiss=*off ;
155300130821           wrecord='S'  ;
155400130828           else  ;
155500130821
155600130821           Immiss=*on  ;
155700130828           // Cerca il record di riferimento
155800130828           exsr cerca_rif  ;
155900130828
156000130828           endif  ;
156100130821
156200130821           exsr      Carica_AZCOS ;
156300131213
156400130821           ENDSR  ;
156500130828       //--------------------------------------------------------------
156600130828       //?Cerco record di riferimento per vedere se dati uguali o diversi
156700130828       //--------------------------------------------------------------
156800130828           BEGSR   Cerca_rif  ;
156900130828           wrecord='N'  ;
157000130828
157100130904           if klna<>999  and klna<>888 ;
157200130828
157300130829           if do0cap<>*blanks    ;
157400130829
157500130828 1         if do0loc<>*blanks   ;
157600130828            clear kloc  ;
157700131213           chain(n)  (do0ver:do0nar:do0cap:kloc:klna:ktser:ksoglia) azcos03l ;
157800131213           if %found(azcos03l)  ;
157900130828           wrecord='C'  ;
158000130828           endif  ;
158100130828 1         endif  ;
158200130828
158300130828           // Se non presente a livello di CAP provo a livello di filiale
158400130828 1         if wrecord='N'  ;
158500130828           clear kcap  ;
158600131213           chain(n)  (do0ver:do0nar:kcap:kloc:klna:ktser:ksoglia)  azcos03l   ;
158700131213           if %found(azcos03l)  ;
158800130828           wrecord='F'  ;
158900130828           endif  ;
159000130828 1         endif  ;
159100130829
159200130829 1         endif  ;
159300130828
159400131213           // standard aziendale
159500130828 1         if wrecord='N'  ;
159600130828           clear kcap  ;
159700131213           chain(n)  (do0ver:do0nar:kcap:kloc:lna999:ktser:ksoglia) azcos03l ;
159800131213           if %found(azcos03l)  ;
159900130828           wrecord='9'  ;
160000130828           endif  ;
160100130828 1         endif  ;
160200130828
160300130828 1         endif  ;
160400130828           ENDSR  ;
160500080702       //--------------------------------------------------------------
160600080702       //?Gestione protezione campi del sfl
160700080702       //--------------------------------------------------------------
160800080702          BEGSR   ProtCampi   ;
160900130820           Immiss  =*off ;
161000130829           vis_rifer=*off      ;
161100130829           BL_ostis=*off ;
161200130829           BL_ostfs=*off ;
161300130829           BL_omiis=*off ;
161400130829           BL_omxfs=*off ;
161500131010           BL_olrsc=*off ;
161600131010           BL_olrnc=*off ;
161700131010           BL_olrnK=*off ;
161800131213           BL_trzs =*off ;
161900130829           Protriga=*off ;
162000130829           Protlna=*on  ;
162100131216           Protsoglia=*off   ;
162200131213           IndLna9988=*off ;
162300080703
162400081007           // Se riga di categoria accendo solo protezione e sottolineatura
162500130821          if v1himm='S'    ;
162600130820           Immiss  =*on    ;
162700130820          endif            ;
162800130829
162900130829          // Protezine per linea di arrivo non prevista
163000130829          // Sempre protezione per interrogazione
163100130829          if v1himm='E'    or orariint=*on  ;
163200130829           Protriga=*on  ;
163300130821          endif            ;
163400130829          if v1himm='E' ;
163500130829           Protlna=*off ;
163600130829          endif            ;
163700130829
163800130828          if v1htser='R'  ;
163900130828          HI_dlna=*on  ;
164000130828          else  ;
164100130828          HI_dlna=*off ;
164200130828          endif  ;
164300131003
164400131010          // Se tipo servizio  ritiro non faccio vedere il tempo organizz
164500131003          if v1htser='R'  ;
164600131003          prot_tempo=*on  ;
164700131003          else ;
164800131003          prot_tempo=*off ;
164900131003          endif  ;
165000130821
165100130829          if CAPvisual= 'S'  ;
165200130829          vis_rifer=*on  ;
165300130829          endif  ;
165400131213
165500131213       // Se il record soglia c'è disabilito F9
165600151204       if v1csogl<>'   ' ;
165700131216       if CAPrifer='S' or dsp_aid = c_F09 or AbilF9=*off ;
165800131213        abilF9=*off ;
165900131213        protsoglia=*off   ;
166000131213       else   ;
166100131213        abilF9=*on  ;
166200131213        protsoglia=*on    ;
166300131213       endif   ;
166400131213       endif   ;
166500131010
166600131213          //if v1clna=999  ;
166700131213          // indlna999=*on  ;
166800131213          //endif  ;
166900130829
167000130829          // Evidenzio i campi diversi dal riferimento
167100131003          // solo se sono a livello di CAP/località
167200131003          // non per filiale o standard
167300131003          if do0cap<>*blanks    ;
167400131003
167500130829          if v1costis<>capostis ;
167600130829           BL_ostis=*on ;
167700130829          endif  ;
167800130829          if v1costfs<>capostfs ;
167900130829           BL_ostfs=*on ;
168000130829          endif  ;
168100131016          if v1comiis<>capomiis ;
168200131016           BL_omiis=*on ;
168300131016          endif  ;
168400131016          if v1comxfs<>capomxfs ;
168500131016           BL_omxfs=*on ;
168600131016          endif  ;
168700131010          if v1colrsc<>capolrsc ;
168800131010           BL_olrsc=*on ;
168900131010          endif  ;
169000131010          if v1colrnc<>capolrnc ;
169100131010           BL_olrnc=*on ;
169200131010          endif  ;
169300131010          if v1colrnk<>capolrnk ;
169400131010           BL_olrnk=*on ;
169500131010          endif  ;
169600131213          if v1ctrzs <>captrzs  ;
169700131213           BL_trzs =*on ;
169800131213          endif  ;
169900131010
170000131010
170100131003          endif  ;
170200130829
170300080702          ENDSR    ;
170400080702       //--------------------------------------------------------------
170500080702       //?conferma aggiornamento
170600080702       //--------------------------------------------------------------
170700080702        BEGSR ConfAGGIO              ;
170800081003
170900080704       s01nrr=1  ;
171000130820       chain s01nrr    sio2s01    ;
171100080704
171200080704 1     dow %found    ;
171300130821
171400130821 2     if  v1clna>0  ;
171500130821
171600140116       // Elaboro record soglia solo se
171700140116 2a    if v1csogl=*blanks or (abilF9=*off or caprifer='S')   ;
171800140116       Klna=v1hlna  ;
171900130828       ktser=v1htser  ;
172000151209       if v1csogl<>'   '  ;
172100151209       ksoglia='P'  ;
172200151209       else  ;
172300151209       clear ksoglia  ;
172400151209       endif   ;
172500151209       //   ksoglia=%subst(v1csogl:1:1)  ;
172600130829
172700131216       // Se la linea on è prevista--> la deleto
172800130829 3     if v1himm='E'   ;
172900131216         delete (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
173000130829 x3    else    ;
173100130829
173200131216         chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
173300130829       // se il record  è diventato uguale al record di riferimento
173400130829       //  lo deleto ma non per filiale
173500130829  4       if  CAPostis= v1costis   and
173600130828              CAPostfs= v1costfs   and
173700131016              CAPomiis= v1comiis   and
173800131016              CAPomxfs= v1comxfs   and
173900131010              CAPolrsc= v1colrsc   and
174000131010              CAPolrnc= v1colrnc   and
174100131010              CAPolrnk= v1colrnk   and
174200131216              CAPtrzs = v1ctrzs    and
174300130829              do0cap<>*blanks           ;
174400131213  5           if %found(azcos03l)  ;
174500130821               delete azcos000   ;
174600130829  5           endif  ;
174700130829  x4        else  ;
174800131213  5           if not %found(azcos03l)  ;
174900130821              clear azcos000  ;
175000130821              cosver=do0ver ;
175100130821              cosnar=do0nar ;
175200130821              coscap=do0cap ;
175300130821              cosloc=do0loc ;
175400130829  5           endif  ;
175500130821              cosostis= v1costis      ;
175600130821              cosostfs= v1costfs      ;
175700131016              cosomiis= v1comiis      ;
175800131016              cosomxfs= v1comxfs      ;
175900131010              cosolrsc= v1colrsc   ;
176000131010              cosolrnc= v1colrnc   ;
176100131010              cosolrnk= v1colrnk   ;
176200130821              costrzs = v1ctrzs         ;
176300130821              cosute  =knmus       ;
176400130821              cosduv=%dec(datasys)  ;
176500130821              costser=ktser  ;
176600130821              coslna=v1clna  ;
176700131216              cosserv=ksoglia ;
176800131213  5           if %found(azcos03l)  ;
176900130821               update azcos000   ;
177000130821               else  ;
177100130821               write  azcos000   ;
177200130829  5           endif  ;
177300130829  4        endif  ;
177400130829  3        endif  ;
177500140116 2a      endif  ;
177600140116 2       endif  ;
177700080704
177800080704        s01nrr=s01nrr+1 ;
177900130820        chain s01nrr    sio2s01    ;
178000080704  1     enddo           ;
178100080704
178200080702        ENDSR;
178300131031       //--------------------------------------------------------------
178400131031       //?Caricamento orario più basso del tipo tempo consegna
178500131031       //--------------------------------------------------------------
178600131031        BEGSR car_cpt                ;
178700131031        clear wcpt0in;
178800131031        clear wcpt0in4;
178900131031        clear wdtsp  ;
179000131031        clear wtsp   ;
179100131031        setll (do0ver:wttc) azcpt01l   ;
179200131031        reade (do0ver:wttc) azcpt01l   ;
179300131031        dow not %eof(azcpt01l);
179400131031           if cpt0in<wcpt0in or wcpt0in=0;
179500131031              wcpt0in=cpt0in;
179600131031              wtsp   =cpttsp;
179700131031           endif;
179800131031           reade (do0ver:wttc) azcpt01l   ;
179900131031        enddo;
180000131031        // decodifico il tipo servizio memorizzato
180100131031        clear ds5e;
180200131031        chain (1:'5E':wtsp) tabel00f;
180300131031        if %found(tabel00f);
180400131031           ds5e=tbluni;
180500131031        endif;
180600131031        wdtsp=§5ed08;
180700131031        wcpt0in4=wcpt0in*100;
180800131031        endsr;
180900080702
181000080206       //--------------------------------------------------------------
181100080206       //?Operazioni finali.
181200080206       //--------------------------------------------------------------
181300080206       BEGSR RoutEnd;
181400080627
181500080704         // Chiusura pgm   ;
181600151209          clear tibs02ds  ;
181700151209          t02tla='C'      ;
181800151209          TNTBE_RicercaControllo  (kpjba : tibs02ds);
181900080704
182000080206         *inLR = *on;
182100080704
182200080206         return;
182300080206
182400080206       ENDSR;
182500080206
182600131003
182700080206      /end-free
182800080206
182900080206       //--------------------------------------------------------------
183000080206       //?Schiere a tempo di compilazione.
183100080206       //--------------------------------------------------------------
183200080206
183300080410** - MSG ------------------------------------------------------------------ -*
183400130821Non trovata località: impossibile caricare gli orari servizi!!                            1
183500130821Immettere un orario formalmente corretto                                                  2
183600130821L'orario di fine deve essere maggiore dell'orario iniziale                                3
183700130829Linea non prevista dal cap/località. Verrà eliminata. Enter per continuare                4
183800130829Linea già immessa                                                                         5
183900130828L'inserimento non è ECCEZIONE:orari = a quelli
184000130829Posizionarsi sulla riga di cui si voglio ripristinare gli orari e premere F05             7
184100130821Linea inesistente                                                                         8
184200130821Orario impostato errato                                                                   9
184300130821Non trovato CAP: impossibile caricare gli orari servizi!!                                10
184400130821L'orario minimo non può essere maggiore dell'orario di inizio standard                   11
184500130821L'orario massimo non può essere minore dell'orario di fine standard                      12
184600130822La linea servizio CONSEGNE deve essere uguale alla precedente servizio RITIRO            13
184700130827Non trovata FILIALE: impossibile caricare gli orari servizi!!                            14
184800130904Immettere PRIMA gli orari PER la LINEA xxx, prima di inserire per cap/loc                15
184900130904L'orario non può essere inferiore a xx:xx                                                16
185000130904L'orario non può essere maggiore di xx:xx                                                17
185100131011Orario limite ritiri NON commissionati DEVE essere > = al limite dei commissionati       18
185200131011Orario limite ritiri Cli Codificati DEVE essere > = al limite dei NON commissionati      19
185300131011Si possono inserire soltanto orari arrotondati ai quarti d'ora                           20
185400131031Orario inizio consegna non congruente con tempo di consegna del serv.                    21
185500131216Tempo oraganizzazione non minore di xx:xx  e non maggiore di 12:00                       22
