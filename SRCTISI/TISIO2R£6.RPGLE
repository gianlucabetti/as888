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
001700080604     fTNTBE01l  if   e           k disk
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
069200131213
069300131213       // Soglia
069400131213       if ksoglia='P'  ;
069500131213         v1csogl  ='Pom'  ;
069600131213         else  ;
069700131213         clear v1csogl    ;
069800131213         endif  ;
069900131213
070000130820
070100130827       if Wrecord= 'N'    ;
070200130820        clear  v1costis  ;
070300130820        clear  v1costfs  ;
070400131016        clear  v1comiis  ;
070500131016        clear  v1comxfs  ;
070600131010        clear  v1colrsc  ;
070700131010        clear  v1colrnc  ;
070800131010        clear  v1colrnk  ;
070900130820        clear  v1ctrzs   ;
071000130820       else  ;
071100130820        v1costis=cosostis  ;
071200130820        v1costfs=cosostfs  ;
071300131016        v1comiis=cosomiis  ;
071400131016        v1comxfs=cosomxfs  ;
071500131010        v1colrsc=cosolrsc  ;
071600131010        v1colrnc=cosolrnc  ;
071700131010        v1colrnk=cosolrnk  ;
071800131213        v1ctrzs =costrzs   ;
071900131010
072000130827        select  ;
072100130827        when wrecord='C'  ;
072200131016        v1cprop='cap|'   ;
072300130827        when wrecord='F'  ;
072400131016        v1cprop='fil|'   ;
072500130827        when wrecord='9'  ;
072600131016        v1cprop='std|'   ;
072700130827        endsl  ;
072800130821
072900130820       endif ;
073000130820
073100130829       if Immiss    =*on  ;
073200130829        v1himm='S'  ;
073300130829       else  ;
073400130821       v1cute=cosute  ;
073500130821        if cosduv>0  ;
073600130821        dataiso=%date(cosduv)  ;
073700130821        datadmy=dataiso ;
073800130821        v1dduv=%dec(datadmy) ;
073900130821        endif  ;
074000130828
074100130902        // visto che la linea on è prevista e mi si sposiziona nella lettura del file
074200130902        // non eseguo la ricerca del riferimento
074300130902         if   tlna<>'?'   ;
074400130902          exsr cerca_rif ;
074500130902          else  ;
074600130902          wrecord='N' ;
074700130902         endif  ;
074800130902        endif  ;
074900130828
075000130902       // Se ho trovato il record memorizzo i dati
075100130828        if wrecord<>'N'    ;
075200130828        CAPostis=cosostis  ;
075300130828        CAPostfs=cosostfs  ;
075400130828        cAPomiis=cosomiis  ;
075500130828        CAPomxfs=cosomxfs  ;
075600131010        CAPolrsc=cosolrsc  ;
075700131010        CAPolrnc=cosolrnc  ;
075800131010        CAPolrnk=cosolrnk  ;
075900130828        CAPrifer=wrecord   ;
076000131213        CAPtrzs =costrzs   ;
076100130829        if wrecord='9'  ;
076200131213         CAPprop='OrRif:STD'  ;
076300130829         endif ;
076400130829        if wrecord='F'  ;
076500131213         CAPprop='OrRif:FIL'  ;
076600130829         endif ;
076700130829        if wrecord='C'  ;
076800131213         CAPprop='OrRif:CAP'  ;
076900130829         endif ;
077000130829
077100130828        else  ;
077200130828        clear CAPostis;
077300130828        clear CAPostfs;
077400130828        clear CAPomiis;
077500130828        clear CAPomxfs;
077600131010        clear CAPolrsc;
077700131010        clear CAPolrnc;
077800131010        clear CAPolrnk;
077900130829        clear CAPprop ;
078000131213        clear CAPtrzs ;
078100130828        CAPrifer='N'  ;
078200130828        endif  ;
078300130821
078400130829       exsr Protcampi    ;
078500130829
078600130820       s01nrr=s01nrr + 1 ;
078700130820       write sio2s01     ;
078800130820
078900130820       ENDSR;
079000080206
079100130828       //--------------------------------------------------------------
079200130828       //?Decodifica linea di arrivo
079300130828       //--------------------------------------------------------------
079400130828       BEGSR Decodi_lna  ;
079500130828
079600130904       select  ;
079700130904       when klna=999  ;
079800131213        v1dlna='Orari Std   '  ;
079900130904       when klna=888  ;
080000131213        v1dlna='OrariContr.'  ;
080100130904        other ;
080200130828       chain klna  azorg01l  ;
080300130828       if %found(azorg01l)  ;
080400131216        if orgde5<>*blanks   ;
080500131216          v1dlna=orgde5  ;
080600131216          else  ;
080700131216          v1dlna=orgdes  ;
080800131216          endif  ;
080900131216
081000130828       else  ;
081100130828         v1dlna=*all'?'  ;
081200130828       endif  ;
081300130904       endsl  ;
081400130828       ENDSR  ;
081500080206       //--------------------------------------------------------------
081600080627       //?Gestione SFL 01
081700080206       //--------------------------------------------------------------
081800080409       BEGSR GesS01;
081900080207
082000080207         // Inizializzazione videata
082100080409         if  $InzS01 = *on;
082200080409            exsr InzS01;
082300080703         // Posizionamento cursore
082400100507         // C01csr = 1;
082500080703           s01nrr = 1;
082600080703           sfldsp_n=*off;
082700080409            $InzS01  = *off;
082800080207         endif;
082900080207
083000080207
083100080207         // Emissione Testata e Piede con tasti funzionali abilitati
083200080207         if  errGenerico = *off;
083300131003         select   ;
083400131003         when §15ita<>'I'  ;
083500130822           sfldsp_n=*on      ;
083600130822           write  sio2D02;
083700131003
083800140206         when lnaprevuni=0 ;
083900131003           sfldsp_n=*on      ;
084000131003           write  sio2D03;
084100131003         other   ;
084200131003           write  sio2Z01;
084300131003           endsl  ;
084400080207         endif;
084500080703
084600080703         // Posizionamento cursore
084700080703         if  C01csr  > *zero;
084800080703           C01rcd = C01csr;
084900080703         endif  ;
085000080207
085100080207         // Emissione videata
085200130820         exfmt  sio2C01;
085300080410
085400080207         reset errMessage;
085500080207         reset errGenerico;
085600080702         clear Vscmsg;
085700130820         clear Errostis  ;
085800130820         clear Errostfs  ;
085900130820         clear Erromiis  ;
086000130820         clear Erromxfs  ;
086100131010         clear Errolrsc  ;
086200131010         clear Errolrnc  ;
086300131010         clear Errolrnk  ;
086400130820         clear ErrTrzs   ;
086500130821         clear Errlna    ;
086600130828         clear BL_ostis  ;
086700130828         clear BL_ostfs  ;
086800130828         clear BL_omiis  ;
086900130828         clear BL_omxfs  ;
087000131010         clear BL_olrsc  ;
087100131010         clear BL_olrnc  ;
087200131010         clear BL_olrnk  ;
087300131213         clear BL_trzs   ;
087400080207
087500080523 1       SELECT;
087600080207
087700080207         // - F3=Fine
087800080523 1         WHEN  dsp_aid = c_F03;
087900080409            $Fine = *on;
088000130820            Do0f03='1'   ;
088100130828
088200130829         // - F5=Ripristina orari di riferimento (disabilitato)
088300130829 1         // WHEN  dsp_aid = c_F05;
088400130829           // chain    c01csr sio2s01  ;
088500130829           // if %found     ;
088600130829           //  if v1costis<>CAPostis  ;
088700130829           //  v1costis=CAPostis  ;
088800130829           //  BL_ostis=*on  ;
088900130829           // endif  ;
089000130829           // if v1costfs<>CAPostfs  ;
089100130829           //  v1costfs=CAPostfs  ;
089200130829           //  BL_ostfs=*on  ;
089300130829           // endif  ;
089400130829           // if v1comiis<>CAPomiis  ;
089500130829           //  v1comiis=CAPomiis  ;
089600130829           //  BL_omiis=*on  ;
089700130829           // endif  ;
089800130829           // if v1comxfs<>CAPomxfs  ;
089900130829           //  v1comxfs=CAPomxfs  ;
090000130829           //  BL_omxfs=*on  ;
090100130829           // endif  ;
090200130829           // if v1ctrzs <>CAPtrzs   ;
090300130829           //  v1ctrzs =CAPtrzs   ;
090400130829           //  BL_trzs =*on  ;
090500130829           // endif  ;
090600130829           //  else  ;
090700130829            // cursore non ben posizionato : errore
090800130829           //  s01nrr=1  ;
090900130829           // chain    s01nrr sio2s01  ;
091000130829           // errlna  =*on ;
091100130829           // vscmsg = Msg(07);
091200130829           // endif         ;
091300130829
091400130829           // exsr aggiosfl      ;
091500130829
091600130829         // - F7=Visualizza orari di riferimento per tutte le righe
091700131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F07;
091800130829            yy=1           ;
091900130829            s01nrr=1  ;
092000130829            chain s01nrr    sio2s01    ;
092100130829
092200130829 0          dow %found    ;
092300130829
092400130829            if capvisual<>'S'  ;
092500130829            capvisual='S'  ;
092600130829            else  ;
092700130829            capvisual=' '  ;
092800130829            endif  ;
092900130829
093000130829            exsr aggiosfl      ;
093100130829
093200130829            s01nrr=s01nrr+1     ;
093300130829
093400130829            chain s01nrr    sio2s01    ;
093500130829           enddo  ;
093600080207
093700131213         // - F09=Abilita riga soglia
093800131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F09;
093900131213            yy=1           ;
094000131213            s01nrr=1  ;
094100131213            chain s01nrr    sio2s01    ;
094200131213
094300131213 0          dow %found    ;
094400131213
094500131216            if v1csogl ='Pom'    ;
094600131213            exsr protcampi  ;
094700131213
094800131213            exsr aggiosfl      ;
094900131216            AbilF9=*off        ;
095000131213            endif  ;
095100131213
095200131213            s01nrr=s01nrr+1     ;
095300131213
095400131213            chain s01nrr    sio2s01    ;
095500131213           enddo  ;
095600080207         // - F12=Ritorno
095700080523 1         WHEN  dsp_aid = c_F12;
095800080702            $Fine = *on;
095900130820            Do0f12='1'   ;
096000080414
096100131003         // - F16=Annullamento  (disabilito)
096200130829 1         WHEN  dsp_aid = c_F16;
096300130829         //  exsr annull_rec    ;
096400130829         //  $Fine = *on;
096500130829
096600080530 x1      // Invio / F6
096700080207           OTHER;
096800081006
096900081006        // Non effettuo controlli in interrogazione
097000131003 1           if orariInt=*off and sfldsp_n=*off ;
097100081006               exsr ContrS01 ;
097200081006 2             if  errGenerico = *on;
097300081006                 leavesr;
097400081006 2             endif;
097500081006 2           endif;
097600080530
097700081023         // F6=conferma Aggiornamento  F10=anche stampa
097800081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
097900131003             if sfldsp_n=*off  ;
098000131003              exsr   ConfAggio ;
098100131003             endif  ;
098200081023
098300080704           $Fine = *on;
098400080530           endif  ;
098500080207
098600080523 1       ENDSL;
098700080207
098800080207       ENDSR;
098900080207
099000080526       //--------------------------------------------------------------
099100080702       //?controlli SFL01
099200080409       //--------------------------------------------------------------
099300080409       BEGSR ContrS01;
099400130821
099500130821       yy=1  ;
0996001308291      dow yy<=12;
099700130821       if lna_es(yy)='R'  ;
099800130821       clear lna_es(YY)  ;
099900130821       endif  ;
100000130821
100100130821       yy=yy+1  ;
1002001308291      enddo    ;
100300130821
100400081126       yy=1           ;
100500080703       s01nrr=1  ;
100600130820       chain s01nrr    sio2s01    ;
100700080703
100800081006 0     dow %found    ;
100900130820
101000130821       clear  v1dlna  ;
101100130821
101200130828       klna=v1clna ;
101300130828       exsr decodi_lna  ;
101400130828 1     if v1dlna=*all'?'        ;
101500130821          errlna  =*on ;
101600130821          vscmsg = Msg(08);
101700130821          EXSR AggioSFL   ;
101800130821          leavesr;
101900130821 1     endif  ;
102000130821
102100130828       // La linea deve essere una di quelle previste
102200130829       //  se non prevista dico che la annullerò
102300130828         Indx=%lookup(v1clna:lna_prev)  ;
1024001308291        if Indx=0  and v1hforzlna=' '  ;
102500130828
102600130821          errlna  =*on ;
102700130821          vscmsg = Msg(04);
102800130829          v1hforzlna='S'  ;
102900130821
103000130821          EXSR AggioSFL   ;
103100130821          leavesr;
103200130821 1     endif  ;
103300130821
103400130822 1     if v1hlna<>v1clna ;
103500130821
103600130821       // Se la riga è RITIRO  controllo il linea doppia e la memorizzo
103700130822  2     if v1htser='R'  ;
103800130821
103900130828         if lna_es(indx)<>' '  ;
104000130821          errlna  =*on ;
104100130821          vscmsg = Msg(05);
104200130821          EXSR AggioSFL   ;
104300130821          leavesr;
104400130821  x3      else  ;
104500130828          lna_es(indx)='R'   ;
104600130821   3      endif  ;
104700130821
104800130821        savlna=v1clna  ;
104900130821
105000130821  x2   else  ;
105100130821   3   if v1clna  <> savlna  ;
105200130821          errlna  =*on ;
105300130821          vscmsg = Msg(13);
105400130821          EXSR AggioSFL   ;
105500130821          leavesr;
105600130821  3    endif  ;
105700130821  2    endif  ;
105800130822  1    endif  ;
105900130821
106000130822       // O R A R I
106100130829       //  li controllo solo per le linee valide
106200130829
106300130829  1a   if indx>0 ;
106400131216
106500131216       // controllo Soglia solo se l'ho abilitata ;
106600131216  1b   if v1csogl= *blanks   or AbilF9=*off  ;
106700131216
106800130829       // Se per la linea di arrivo manca il record di filiale --> non posso inserire
106900130829       if do0cap<>*blanks ;
107000130829       clear kloc  ;
107100130829       clear kcap  ;
107200130829       ktser=v1htser  ;
107300131213           chain(n)  (do0ver:do0nar:kcap:kloc:v1clna:ktser)  azcos03l   ;
107400131213           if not %found(azcos03l)  ;
107500130829          errlna  =*on ;
107600130829          vscmsg = Msg(15);
107700130829          %subst(vscmsg:40:3)=%editc(v1clna:'X')  ;
107800130829          EXSR AggioSFL   ;
107900130829          leavesr;
108000130829           endif  ;
108100130829       endif  ;
108200130829
108300130829
108400130904       // Imposto gli orari in base al servizio
108500130904       if v1clna<>888     ;
108600130904       if v1htser='C'   ;
108700131216       if v1csogl='Pom'  ;
108800131216       W88ostis=C88Postis ;
108900131216       W88ostfs=C88Postfs ;
109000131216       W88omiis=C88Pomiis ;
109100131216       W88omxfs=C88Pomxfs ;
109200131216       W88olrsc=C88Polrsc ;
109300131216       W88olrNc=C88Polrnc ;
109400131216       W88olrNk=C88Polrnk ;
109500131216       W88trzs =C88Ptrzs  ;
109600131216       else  ;
109700130904       W88ostis=C88ostis ;
109800130904       W88ostfs=C88ostfs ;
109900130904       W88omiis=C88omiis ;
110000130904       W88omxfs=C88omxfs ;
110100131010       W88olrsc=C88olrsc ;
110200131010       W88olrNc=C88olrnc ;
110300131010       W88olrNk=C88olrnk ;
110400131216       W88trzs =C88trzs  ;
110500131216       endif  ;
110600131216
110700130904       else  ;
110800130904       W88ostis=r88ostis ;
110900130904       W88ostfs=r88ostfs ;
111000130904       W88omiis=r88omiis ;
111100130904       W88omxfs=r88omxfs ;
111200131010       W88olrsc=r88olrsc ;
111300131010       W88olrnc=r88olrnc ;
111400131010       W88olrnk=r88olrnk ;
111500130904       endif  ;
111600131010       endif  ;
111700131010
111800131011       if v1clna= 888 or w88ostfs=0000  ;
111900130904       W88ostis=0000 ;
112000130904       W88ostfs=2400 ;
112100130904       W88omiis=0000 ;
112200130904       W88omxfs=2400 ;
112300131010       W88olrsc=2400 ;
112400131010       W88olrnc=2400 ;
112500131010       W88olrnk=2400 ;
112600131216       W88trzs =0000 ;
112700130904       endif  ;
112800130904
112900131216
113000131011       // Orario inizio
113100130829 1     if  v1costis=0  ;
113200130820          errostis=*on ;
113300130820          vscmsg = Msg(02);
113400081003          EXSR AggioSFL   ;
113500081003          leavesr;
113600130820 1     endif  ;
113700130821
113800130821       wora=v1costis ;
113900130821       exsr contr_ora ;
114000130829 1     if vscmsg<>*blanks ;
114100130821          errostis=*on ;
114200130821          EXSR AggioSFL   ;
114300130821          leavesr;
114400130821 1     endif  ;
114500130904
114600130904       // no minore del limite previsto
114700130904       if v1costis<w88ostis  ;
114800130904          errostis=*on ;
114900130904          vscmsg = Msg(16);
115000130904          %subst(vscmsg:37:5)=%editw(w88ostis: '0 :  ')    ;
115100130904          EXSR AggioSFL   ;
115200130904          leavesr;
115300130904 1     endif  ;
115400131031       // Orario inizio consegne no maggiore tempo consegna per tipo servizio
115500140210       // non controllo per orari del pomeriggio  e se si tratta di linea oltre
115600140210       if v1htser='C' and do0cap<>*blanks and lna_lol(indx)<>'S'
115700140110          and v1csogl=*blanks              ;
115800140110
115900131031          exsr car_cpt;
116000131031          if v1costis>wcpt0in4;
116100131031             errostis=*on ;
116200131031             vscmsg = Msg(21);
116300131031             vscmsg=%trim(vscmsg)+' ' + %trim(wdtsp);
116400131031             EXSR AggioSFL   ;
116500131031             leavesr;
116600131031          endif;
116700131031       endif;
116800130821
116900130821
117000131011       // Orario fine
117100130820 1     if  v1costfs=0     ;
117200130820          errostfs=*on ;
117300130820          vscmsg = Msg(02);
117400130820          EXSR AggioSFL   ;
117500130820          leavesr;
117600130820 1     endif  ;
117700130821
117800130821       wora=v1costfs ;
117900130821       exsr contr_ora ;
118000130829 1     if vscmsg<>*blanks ;
118100130821          errostfs=*on ;
118200130821          EXSR AggioSFL   ;
118300130821          leavesr;
118400130821 1     endif  ;
118500130904       // no maggiore del limite previsto
118600130904       if v1costfs>w88ostfs  ;
118700130904          errostfs=*on ;
118800130904          vscmsg = Msg(17);
118900130904          %subst(vscmsg:37:5)=%editw(w88ostfs: '0 :  ')    ;
119000130904          EXSR AggioSFL   ;
119100130904          leavesr;
119200130904 1     endif  ;
119300130820
119400130820 1     if  v1costfs<v1costis  ;
119500130820          errostfs=*on ;
119600130820          vscmsg = Msg(03);
119700130820          EXSR AggioSFL   ;
119800130820          leavesr;
119900130820 1     endif  ;
120000131011
120100131016       if v1htser='C'  ;
120200131216       // Tempo oraganizzazione --> solo per CONSEGNE
120300131216 1     if  v1ctrzs<w88trzs   or v1ctrzs>1200 ;
120400131216          Errtrzs =*on ;
120500131216          vscmsg = Msg(22);
120600131216          %subst(vscmsg:37:5)=%editw(w88trzs:'0 :  ')  ;
120700131216          EXSR AggioSFL   ;
120800131216          leavesr;
120900131216 1     endif  ;
121000131216
121100131016       // Orario MINIMO  solo per CONSEGNE
121200131016 1     if  v1comiis=0  ;
121300131016          erromiis=*on ;
121400131016          vscmsg = Msg(02);
121500131016          EXSR AggioSFL   ;
121600131016          leavesr;
121700131016 1     endif  ;
121800131016
121900131016       wora=v1comiis ;
122000131016       exsr contr_ora ;
122100131016 1     if vscmsg<>*blanks ;
122200131016          erromiis=*on ;
122300131016          EXSR AggioSFL   ;
122400131016          leavesr;
122500131016 1     endif  ;
122600131016
122700131016       // no minore del limite previsto
122800131016       if v1comiis<w88omiis  ;
122900131016          erromiis=*on ;
123000131016          vscmsg = Msg(16);
123100131016          %subst(vscmsg:37:5)=%editw(w88omiis: '0 :  ')    ;
123200131016          EXSR AggioSFL   ;
123300131016          leavesr;
123400131016 1     endif  ;
123500131016
123600131016       // no maggiore dell'orario di inizio
123700131016       if v1comiis > v1costis  ;
123800131016          erromiis=*on ;
123900131016          vscmsg = Msg(11);
124000131016          EXSR AggioSFL   ;
124100131016          leavesr;
124200131016 1     endif  ;
124300131016
124400131016       // Orario MASSIMO
124500131016 1     if  v1comxfs=0     ;
124600131016          erromxfs=*on ;
124700131016          vscmsg = Msg(02);
124800131016          EXSR AggioSFL   ;
124900131016          leavesr;
125000131016 1     endif  ;
125100131016
125200131016       wora=v1comxfs ;
125300131016       exsr contr_ora ;
125400131016 1     if vscmsg<>*blanks ;
125500131016          erromxfs=*on ;
125600131016          EXSR AggioSFL   ;
125700131016          leavesr;
125800131016 1     endif  ;
125900131016       // no maggiore del limite previsto
126000131016       if v1comxfs>w88omxfs  ;
126100131016          erromxfs=*on ;
126200131016          vscmsg = Msg(17);
126300131016          %subst(vscmsg:37:5)=%editw(w88omxfs: '0 :  ')    ;
126400131016          EXSR AggioSFL   ;
126500131016          leavesr;
126600131016 1     endif  ;
126700131016
126800131016 1     if  v1comxfs<v1comiis  ;
126900131016          erromxfs=*on ;
127000131016          vscmsg = Msg(03);
127100131016          EXSR AggioSFL   ;
127200131016          leavesr;
127300131016 1     endif  ;
127400131016       // no minore   dell'orario di fine
127500131016       if v1comxfs < v1costfs  ;
127600131016          erromxfs=*on ;
127700131016          vscmsg = Msg(12);
127800131016          EXSR AggioSFL   ;
127900131016          leavesr;
128000131016 1     endif  ;
128100131016 1     endif  ;
128200131016
128300131011       if v1htser='R'  ;
128400131011
128500131011       // Orari limite ritiri: COMMISSIONATI
128600131011 1     if  v1colrsc=0     ;
128700131011          erroLRSC=*on ;
128800131011          vscmsg = Msg(02);
128900131011          EXSR AggioSFL   ;
129000131011          leavesr;
129100131011 1     endif  ;
129200131011
129300131011       wora=v1coLRSC ;
129400131011       exsr contr_ora ;
129500131011 1     if vscmsg<>*blanks ;
129600131011          erroLRSC=*on ;
129700131011          EXSR AggioSFL   ;
129800131011          leavesr;
129900131011 1     endif  ;
130000131011       // no maggiore del limite previsto
130100131011       if v1coLRSC>w88oLRSC  ;
130200131011          erroLRSC=*on ;
130300131011          vscmsg = Msg(17);
130400131011          %subst(vscmsg:37:5)=%editw(w88oLRSC: '0 :  ')    ;
130500131011          EXSR AggioSFL   ;
130600131011          leavesr;
130700131011 1     endif  ;
130800131011       // Orari limite ritiri: NON COMMISSIONATI
130900131011 1     if  v1colrNc=0     ;
131000131011          erroLRNC=*on ;
131100131011          vscmsg = Msg(02);
131200131011          EXSR AggioSFL   ;
131300131011          leavesr;
131400131011 1     endif  ;
131500131011
131600131011       wora=v1coLRNC ;
131700131011       exsr contr_ora ;
131800131011 1     if vscmsg<>*blanks ;
131900131011          erroLRNC=*on ;
132000131011          EXSR AggioSFL   ;
132100131011          leavesr;
132200131011 1     endif  ;
132300131011       // no maggiore del limite previsto
132400131011       if v1coLRNC>w88oLRNC  ;
132500131011          erroLRNC=*on ;
132600131011          vscmsg = Msg(17);
132700131011          %subst(vscmsg:37:5)=%editw(w88oLRNC: '0 :  ')    ;
132800131011          EXSR AggioSFL   ;
132900131011          leavesr;
133000131011 1     endif  ;
133100131011       // Orari limite ritiri: NON COMMISSIONATI CODIFICATI
133200131011 1     if  v1colrNK=0     ;
133300131011          erroLRNK=*on ;
133400131011          vscmsg = Msg(02);
133500131011          EXSR AggioSFL   ;
133600131011          leavesr;
133700131011 1     endif  ;
133800131011
133900131011       wora=v1coLRNK ;
134000131011       exsr contr_ora ;
134100131011 1     if vscmsg<>*blanks ;
134200131011          erroLRNK=*on ;
134300131011          EXSR AggioSFL   ;
134400131011          leavesr;
134500131011 1     endif  ;
134600131011       // no maggiore del limite previsto
134700131011       if v1coLRNK>w88oLRNK  ;
134800131011          erroLRNK=*on ;
134900131011          vscmsg = Msg(17);
135000131011          %subst(vscmsg:37:5)=%editw(w88oLRNK: '0 :  ')    ;
135100131011          EXSR AggioSFL   ;
135200131011          leavesr;
135300131011 1     endif  ;
135400131011
135500131011       // secondo orario >= al primo
135600131011       if v1colrnc<v1colrsc  ;
135700131011          erroLRNc=*on ;
135800131011          vscmsg = Msg(18);
135900131011          EXSR AggioSFL   ;
136000131011          leavesr;
136100131011       endif  ;
136200131011       // terzo orario >= al secondo
136300131011       if v1colrnk<v1colrnc  ;
136400131011          erroLRNk=*on ;
136500131011          vscmsg = Msg(19);
136600131011          EXSR AggioSFL   ;
136700131011          leavesr;
136800131011       endif  ;
136900131011
137000131011       endif  ;
137100130820
137200131216 1b    endif  ;
137300131216 1a    endif  ;
137400130821
137500130821       EXSR AggioSFL   ;
137600080703
137700080703       s01nrr=1+s01nrr  ;
137800130820       chain s01nrr    sio2s01    ;
137900081006 0     enddo         ;
138000081006
138100080409       ENDSR;
138200080409
138300130829       //--------------------------------------------------------------
138400130829       //?Annullamento record
138500130829       //--------------------------------------------------------------
138600130829       //BEGSR Annull_rec  ;
138700130829
138800130829       // Elaboro la schiera delle linee e deleto
138900130829       //yy=1   ;
139000130829       //dow lna_prev(YY)>0     ;
139100130829
139200130829       //klna=lna_prev(yy)     ;
139300130829       //ktser='R'  ;
139400131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
139500130829       //ktser='C'  ;
139600131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
139700130829
139800130829       //yy=yy+1  ;
139900130829       //enddo   ;
140000130829
140100130829       //ENDSR;
140200080703       //--------------------------------------------------------------
140300080703       //?Aggiornamento sfl
140400080703       //--------------------------------------------------------------
140500080703       BEGSR AGGIOSFL  ;
140600080703       if vscmsg<>*blanks   ;
140700080703       errMessage  = *on;
140800080703       errGenerico = *on;
140900080704       c01csr=s01nrr    ;
141000080703       endif    ;
141100080703
141200080703       exsr ProtCampi   ;
141300080703
141400130820       update  sio2s01  ;
141500080703       ENDSR   ;
141600130821       //--------------------------------------------------------------
141700130821       //?Controllo campo ORA
141800130821       //--------------------------------------------------------------
141900130821       BEGSR Contr_ORA ;
142000130821       if whh < 0  or whh > 24  ;
142100130821          vscmsg = Msg(09);
142200130821       endif  ;
142300130821       if wmm < 0  or wmm > 59  ;
142400130821          vscmsg = Msg(09);
142500130821       endif  ;
142600131011
142700131011       // Minuti arrotondati ai 15 minuti
142800131011       if wmm<>00 and wmm<>15 and wmm<>30 and wmm<>45  ;
142900131011          vscmsg = Msg(20);
143000131011       endif  ;
143100130821
143200130821       ENDSR ;
143300080207       //--------------------------------------------------------------
143400080627       //?Inizializzazione SFL01
143500080207       //--------------------------------------------------------------
143600080409       BEGSR InzS01;
143700080207
143800080207       // Pulizia subfile
143900080207         SflDsp_N    = *on;
144000080207         SflDspCtl_N = *on;
144100130820         write  sio2C01;
144200080207         SflDspCtl_N = *off;
144300080207         SflEnd      = *off;
144400080530
144500080530         clear C01rcd;
144600100507         C01csr=1;
144700080627         S01nrr=0 ;
144800080702         clear Vscmsg;
144900080207         errMessage  = *off;
145000080207         errGenerico = *off;
145100130820         immiss    =*off ;
145200130821         clear savlna ;
145300130821         clear yy    ;
145400130821         clear lna_es;
145500130821         clear Wrecord ;
145600130821         clear v1dduv  ;
145700130821         clear v1cute  ;
145800130821
145900130822         // Se la nazione no è italia --> non carico nulla e avviso che non esistono
146000130822         //  orari
146100130822
146200130827 1       if §15ita= 'I'  ;
146300130820
146400130821         // 1)
146500130821         // Leggo e carico eventuali linee non più previste dal cap/località
146600130821
146700130828 1a    if   do0cap<>*blanks   ;
146800130828
146900130829       Protlna=*off ;
147000131213       setll  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
147100131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
147200131213 2      dow  not %eof(azcos03l)  ;
147300130821          klna=coslna ;
147400130827          Indx=%lookup(coslna:lna_prev)  ;
147500130827 3      if indx=0 ;
147600130821          tlna='?'   ;
147700130829           ktser=costser   ;
147800130821          exsr   Leggi_cos ;
1479001308273       endif  ;
148000130821
148100131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
1482001308272      enddo  ;
1483001308281a     endif  ;
148400130821
148500130821         // 2)
148600130821         // Carico i record  già presenti
148700130829         Protlna=*on  ;
148800130902         clear tlna   ;
148900130820
149000130827           YY=1  ;
149100140206 2     //  dow lna_prev(yy)>0  ;
149200140206           for  yy=1 to %elem(lna_prev);
149300140206           if lna_prev(yy)>0;
149400130827
149500130827           klna=lna_prev(YY) ;
149600131213           chain  (do0ver:do0nar:do0cap:do0loc:klna)  azcos03l   ;
149700131213 3         if %found(azcos03l)  ;
149800130827           lna_es(YY)='S'  ;
149900130830           endif  ;
150000131010
150100131213           if klna=999  or klna=888 ;
150200131213            indlna9988=*on  ;
150300131010            else  ;
150400131213            indlna9988=*off ;
150500131010            endif  ;
150600130830
150700131213           ksoglia=*blanks ;
150800131213           ktser='R'  ;
150900130821           exsr Leggi_cos  ;
151000130821           ktser='C'  ;
151100130821           exsr Leggi_cos  ;
151200131213           // Per consegne vedo se presente anche soglia "P"
151300131213
151400131216           if lna_sog(yy)='S'  or abilF9=*on  ;
151500131213           ksoglia='P'  ;
151600131213           exsr Leggi_cos  ;
151700131213           endif  ;
151800130827
151900140206       //  yy=yy+1  ;
152000140206           endif;
152100140206           endfor;
152200140206 2     //  enddo    ;
152300130821
1524001308271        endif  ;
152500130827
152600080702         ENDSR    ;
152700130821       //--------------------------------------------------------------
152800130821       //?Lettura record di AZCOS
152900130821       //--------------------------------------------------------------
153000130821           BEGSR Leggi_cos  ;
153100131213           chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia) azcos03l  ;
153200131213           if %found(azcos03l)  ;
153300130821           Immiss=*off ;
153400130821           wrecord='S'  ;
153500130828           else  ;
153600130821
153700130821           Immiss=*on  ;
153800130828           // Cerca il record di riferimento
153900130828           exsr cerca_rif  ;
154000130828
154100130828           endif  ;
154200130821
154300130821           exsr      Carica_AZCOS ;
154400131213
154500130821           ENDSR  ;
154600130828       //--------------------------------------------------------------
154700130828       //?Cerco record di riferimento per vedere se dati uguali o diversi
154800130828       //--------------------------------------------------------------
154900130828           BEGSR   Cerca_rif  ;
155000130828           wrecord='N'  ;
155100130828
155200130904           if klna<>999  and klna<>888 ;
155300130828
155400130829           if do0cap<>*blanks    ;
155500130829
155600130828 1         if do0loc<>*blanks   ;
155700130828            clear kloc  ;
155800131213           chain(n)  (do0ver:do0nar:do0cap:kloc:klna:ktser:ksoglia) azcos03l ;
155900131213           if %found(azcos03l)  ;
156000130828           wrecord='C'  ;
156100130828           endif  ;
156200130828 1         endif  ;
156300130828
156400130828           // Se non presente a livello di CAP provo a livello di filiale
156500130828 1         if wrecord='N'  ;
156600130828           clear kcap  ;
156700131213           chain(n)  (do0ver:do0nar:kcap:kloc:klna:ktser:ksoglia)  azcos03l   ;
156800131213           if %found(azcos03l)  ;
156900130828           wrecord='F'  ;
157000130828           endif  ;
157100130828 1         endif  ;
157200130829
157300130829 1         endif  ;
157400130828
157500131213           // standard aziendale
157600130828 1         if wrecord='N'  ;
157700130828           clear kcap  ;
157800131213           chain(n)  (do0ver:do0nar:kcap:kloc:lna999:ktser:ksoglia) azcos03l ;
157900131213           if %found(azcos03l)  ;
158000130828           wrecord='9'  ;
158100130828           endif  ;
158200130828 1         endif  ;
158300130828
158400130828 1         endif  ;
158500130828           ENDSR  ;
158600080702       //--------------------------------------------------------------
158700080702       //?Gestione protezione campi del sfl
158800080702       //--------------------------------------------------------------
158900080702          BEGSR   ProtCampi   ;
159000130820           Immiss  =*off ;
159100130829           vis_rifer=*off      ;
159200130829           BL_ostis=*off ;
159300130829           BL_ostfs=*off ;
159400130829           BL_omiis=*off ;
159500130829           BL_omxfs=*off ;
159600131010           BL_olrsc=*off ;
159700131010           BL_olrnc=*off ;
159800131010           BL_olrnK=*off ;
159900131213           BL_trzs =*off ;
160000130829           Protriga=*off ;
160100130829           Protlna=*on  ;
160200131216           Protsoglia=*off   ;
160300131213           IndLna9988=*off ;
160400080703
160500081007           // Se riga di categoria accendo solo protezione e sottolineatura
160600130821          if v1himm='S'    ;
160700130820           Immiss  =*on    ;
160800130820          endif            ;
160900130829
161000130829          // Protezine per linea di arrivo non prevista
161100130829          // Sempre protezione per interrogazione
161200130829          if v1himm='E'    or orariint=*on  ;
161300130829           Protriga=*on  ;
161400130821          endif            ;
161500130829          if v1himm='E' ;
161600130829           Protlna=*off ;
161700130829          endif            ;
161800130829
161900130828          if v1htser='R'  ;
162000130828          HI_dlna=*on  ;
162100130828          else  ;
162200130828          HI_dlna=*off ;
162300130828          endif  ;
162400131003
162500131010          // Se tipo servizio  ritiro non faccio vedere il tempo organizz
162600131003          if v1htser='R'  ;
162700131003          prot_tempo=*on  ;
162800131003          else ;
162900131003          prot_tempo=*off ;
163000131003          endif  ;
163100130821
163200130829          if CAPvisual= 'S'  ;
163300130829          vis_rifer=*on  ;
163400130829          endif  ;
163500131213
163600131213       // Se il record soglia c'è disabilito F9
163700131216       if v1csogl='Pom' ;
163800131216       if CAPrifer='S' or dsp_aid = c_F09 or AbilF9=*off ;
163900131213        abilF9=*off ;
164000131213        protsoglia=*off   ;
164100131213       else   ;
164200131213        abilF9=*on  ;
164300131213        protsoglia=*on    ;
164400131213       endif   ;
164500131213       endif   ;
164600131010
164700131213          //if v1clna=999  ;
164800131213          // indlna999=*on  ;
164900131213          //endif  ;
165000130829
165100130829          // Evidenzio i campi diversi dal riferimento
165200131003          // solo se sono a livello di CAP/località
165300131003          // non per filiale o standard
165400131003          if do0cap<>*blanks    ;
165500131003
165600130829          if v1costis<>capostis ;
165700130829           BL_ostis=*on ;
165800130829          endif  ;
165900130829          if v1costfs<>capostfs ;
166000130829           BL_ostfs=*on ;
166100130829          endif  ;
166200131016          if v1comiis<>capomiis ;
166300131016           BL_omiis=*on ;
166400131016          endif  ;
166500131016          if v1comxfs<>capomxfs ;
166600131016           BL_omxfs=*on ;
166700131016          endif  ;
166800131010          if v1colrsc<>capolrsc ;
166900131010           BL_olrsc=*on ;
167000131010          endif  ;
167100131010          if v1colrnc<>capolrnc ;
167200131010           BL_olrnc=*on ;
167300131010          endif  ;
167400131010          if v1colrnk<>capolrnk ;
167500131010           BL_olrnk=*on ;
167600131010          endif  ;
167700131213          if v1ctrzs <>captrzs  ;
167800131213           BL_trzs =*on ;
167900131213          endif  ;
168000131010
168100131010
168200131003          endif  ;
168300130829
168400080702          ENDSR    ;
168500080702       //--------------------------------------------------------------
168600080702       //?conferma aggiornamento
168700080702       //--------------------------------------------------------------
168800080702        BEGSR ConfAGGIO              ;
168900081003
169000080704       s01nrr=1  ;
169100130820       chain s01nrr    sio2s01    ;
169200080704
169300080704 1     dow %found    ;
169400130821
169500130821 2     if  v1clna>0  ;
169600130821
169700140116       // Elaboro record soglia solo se
169800140116 2a    if v1csogl=*blanks or (abilF9=*off or caprifer='S')   ;
169900140116       Klna=v1hlna  ;
170000130828       ktser=v1htser  ;
170100131216       ksoglia=%subst(v1csogl:1:1)  ;
170200130829
170300131216       // Se la linea on è prevista--> la deleto
170400130829 3     if v1himm='E'   ;
170500131216         delete (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
170600130829 x3    else    ;
170700130829
170800131216         chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
170900130829       // se il record  è diventato uguale al record di riferimento
171000130829       //  lo deleto ma non per filiale
171100130829  4       if  CAPostis= v1costis   and
171200130828              CAPostfs= v1costfs   and
171300131016              CAPomiis= v1comiis   and
171400131016              CAPomxfs= v1comxfs   and
171500131010              CAPolrsc= v1colrsc   and
171600131010              CAPolrnc= v1colrnc   and
171700131010              CAPolrnk= v1colrnk   and
171800131216              CAPtrzs = v1ctrzs    and
171900130829              do0cap<>*blanks           ;
172000131213  5           if %found(azcos03l)  ;
172100130821               delete azcos000   ;
172200130829  5           endif  ;
172300130829  x4        else  ;
172400131213  5           if not %found(azcos03l)  ;
172500130821              clear azcos000  ;
172600130821              cosver=do0ver ;
172700130821              cosnar=do0nar ;
172800130821              coscap=do0cap ;
172900130821              cosloc=do0loc ;
173000130829  5           endif  ;
173100130821              cosostis= v1costis      ;
173200130821              cosostfs= v1costfs      ;
173300131016              cosomiis= v1comiis      ;
173400131016              cosomxfs= v1comxfs      ;
173500131010              cosolrsc= v1colrsc   ;
173600131010              cosolrnc= v1colrnc   ;
173700131010              cosolrnk= v1colrnk   ;
173800130821              costrzs = v1ctrzs         ;
173900130821              cosute  =knmus       ;
174000130821              cosduv=%dec(datasys)  ;
174100130821              costser=ktser  ;
174200130821              coslna=v1clna  ;
174300131216              cosserv=ksoglia ;
174400131213  5           if %found(azcos03l)  ;
174500130821               update azcos000   ;
174600130821               else  ;
174700130821               write  azcos000   ;
174800130829  5           endif  ;
174900130829  4        endif  ;
175000130829  3        endif  ;
175100140116 2a      endif  ;
175200140116 2       endif  ;
175300080704
175400080704        s01nrr=s01nrr+1 ;
175500130820        chain s01nrr    sio2s01    ;
175600080704  1     enddo           ;
175700080704
175800080702        ENDSR;
175900131031       //--------------------------------------------------------------
176000131031       //?Caricamento orario più basso del tipo tempo consegna
176100131031       //--------------------------------------------------------------
176200131031        BEGSR car_cpt                ;
176300131031        clear wcpt0in;
176400131031        clear wcpt0in4;
176500131031        clear wdtsp  ;
176600131031        clear wtsp   ;
176700131031        setll (do0ver:wttc) azcpt01l   ;
176800131031        reade (do0ver:wttc) azcpt01l   ;
176900131031        dow not %eof(azcpt01l);
177000131031           if cpt0in<wcpt0in or wcpt0in=0;
177100131031              wcpt0in=cpt0in;
177200131031              wtsp   =cpttsp;
177300131031           endif;
177400131031           reade (do0ver:wttc) azcpt01l   ;
177500131031        enddo;
177600131031        // decodifico il tipo servizio memorizzato
177700131031        clear ds5e;
177800131031        chain (1:'5E':wtsp) tabel00f;
177900131031        if %found(tabel00f);
178000131031           ds5e=tbluni;
178100131031        endif;
178200131031        wdtsp=§5ed08;
178300131031        wcpt0in4=wcpt0in*100;
178400131031        endsr;
178500080702
178600080206       //--------------------------------------------------------------
178700080206       //?Operazioni finali.
178800080206       //--------------------------------------------------------------
178900080206       BEGSR RoutEnd;
179000080627
179100080704         // Chiusura pgm   ;
179200131003         // clear tibs02ds  ;
179300131003         // t02tla='C'      ;
179400131003         // TNTBE_RicercaControllo  (kpjba : tibs02ds);
179500080704
179600080206         *inLR = *on;
179700080704
179800080206         return;
179900080206
180000080206       ENDSR;
180100080206
180200131003
180300080206      /end-free
180400080206
180500080206       //--------------------------------------------------------------
180600080206       //?Schiere a tempo di compilazione.
180700080206       //--------------------------------------------------------------
180800080206
180900080410** - MSG ------------------------------------------------------------------ -*
181000130821Non trovata località: impossibile caricare gli orari servizi!!                            1
181100130821Immettere un orario formalmente corretto                                                  2
181200130821L'orario di fine deve essere maggiore dell'orario iniziale                                3
181300130829Linea non prevista dal cap/località. Verrà eliminata. Enter per continuare                4
181400130829Linea già immessa                                                                         5
181500130828L'inserimento non è ECCEZIONE:orari = a quelli
181600130829Posizionarsi sulla riga di cui si voglio ripristinare gli orari e premere F05             7
181700130821Linea inesistente                                                                         8
181800130821Orario impostato errato                                                                   9
181900130821Non trovato CAP: impossibile caricare gli orari servizi!!                                10
182000130821L'orario minimo non può essere maggiore dell'orario di inizio standard                   11
182100130821L'orario massimo non può essere minore dell'orario di fine standard                      12
182200130822La linea servizio CONSEGNE deve essere uguale alla precedente servizio RITIRO            13
182300130827Non trovata FILIALE: impossibile caricare gli orari servizi!!                            14
182400130904Immettere PRIMA gli orari PER la LINEA xxx, prima di inserire per cap/loc                15
182500130904L'orario non può essere inferiore a xx:xx                                                16
182600130904L'orario non può essere maggiore di xx:xx                                                17
182700131011Orario limite ritiri NON commissionati DEVE essere > = al limite dei commissionati       18
182800131011Orario limite ritiri Cli Codificati DEVE essere > = al limite dei NON commissionati      19
182900131011Si possono inserire soltanto orari arrotondati ai quarti d'ora                           20
183000131031Orario inizio consegna non congruente con tempo di consegna del serv.                    21
183100131216Tempo oraganizzazione non minore di xx:xx  e non maggiore di 12:00                       22
