000100130821      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) dataisT(*yMd.)
000200080206      //--------------------------------------------------------------
000300130820      //?TISIO2R - Cappario: gestione orari servizi
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200130820     fAZcpl01l  if   e           k disk
001300130821     fAZcpc01l  if   e           k disk
001400130827     fAZcpe01l  if   e           k disk
001500131031     fAZcpt01l  if   e           k disk
001600131213     fAZCOS03l  uf a e           k disk
001700130820     fAZORG01L  if   e           k disk
001800080604     fTNTBE01l  if   e           k disk
001900130821     ftabel00f  if   e           k disk
002000130820     fTISIO2D   cf   e             workstn indds(IndDspF)
002100080206     f                                     infds(InfDspF)
002200130821     f                                     sfile(sio2S01 : S01nrr)
002300080206      //---------------------------------------------------------------
002400080207      // - Tasti funzionali a video
002500080207     d c_F01           c                   const(x'31')
002600080207     d c_F02           c                   const(x'32')
002700080207     d c_F03           c                   const(x'33')
002800080207     d c_F05           c                   const(x'35')
002900080207     d c_F06           c                   const(x'36')
003000080207     d c_F07           c                   const(x'37')
003100080207     d c_F08           c                   const(x'38')
003200080207     d c_F09           c                   const(x'39')
003300080207     d c_F10           c                   const(x'3A')
003400080207     d c_F12           c                   const(x'3C')
003500080207     d c_F13           c                   const(x'B1')
003600080207     d c_F14           c                   const(x'B2')
003700080207     d c_F15           c                   const(x'B3')
003800080207     d c_F16           c                   const(x'B4')
003900080207     d c_F17           c                   const(x'B5')
004000080207     d c_F18           c                   const(x'B6')
004100080207     d c_F19           c                   const(x'B7')
004200080207     d c_F20           c                   const(x'B8')
004300080207     d c_F21           c                   const(x'B9')
004400080207     d c_F22           c                   const(x'BA')
004500080207     d c_F23           c                   const(x'BB')
004600080207     d c_F24           c                   const(x'BC')
004700080207     d c_Enter         c                   const(x'F1')
004800080207     d c_RollDown      c                   const(x'F4')
004900080207     d c_RollUp        c                   const(x'F5')
005000080214
005100080214      // - Attributi di visualizzazione
005200080215      //  -  DspAtr() - Normale
005300080214     d C_dspatr_Norm   c                   const(x'20')
005400080215      //  -  DspAtr(RI)
005500080214     d C_dspatr_RI     c                   const(x'21')
005600080215      //  -  DspAtr(ND)
005700080214     d C_dspatr_ND     c                   const(x'27')
005800080215      //  -  DspAtr(BL) / Color(Red)
005900080214     d C_dspatr_BL     c                   const(x'28')
006000080206
006100080206      //---------------------------------------------------------------
006200080206      //?Definizione schiere.
006300080206      //---------------------------------------------------------------
006400080206
006500080206      // - Messaggi di errore
006600130821     d MSG             s             78    dim(25) ctdata perrcd(1)
006700130905     d lna_ES          s              1    dim(14)
006800131213     d lna_SOG         s              1    dim(14)
006900140210     d lna_LOL         s              1    dim(14)
007000140206     d                 ds
007100140206     d lnaprevuni                    42  0
007200140206     d lna_prev                       3  0 dim(14)overlay(lnaprevuni)
007300140206     d*lna_prev        s              3  0 dim(14)
007400081126
007500080206      //---------------------------------------------------------------
007600080206      //?Definizione aree dati.
007700080206      //---------------------------------------------------------------
007800100226
007900080206      // - Dati utente
008000080206     d §AzUte        e ds                  extname(AZUTE00F)
008100080206     d                                     dtaara
008200080206     d §DatiUte      e ds                  extname(dDatiUte)
008300080206     d                                     dtaara
008400080206
008500080206      //---------------------------------------------------------------
008600080206      //?Definizione strutture dati.
008700080206      //---------------------------------------------------------------
008800080619     D CPO01           DS
008900080619     D  cp1NRR               397    400B 0
009000080206
009100080206      // - Status
009200080206     d Psds           sds
009300080206     d   SDSpgm          *proc
009400080206
009500080206      // - InfDS
009600080206     d InfDspF         ds
009700080207     d  dsp_aid              369    369a                                        AID byte
009800080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
009900080207     d  min_nrr              378    379i 0                                      Subfile min rrn
010000080207     d  num_rcds             380    381i 0                                      Subfile num rcds
010100080206
010200080206      // - Indicatori su DspF
010300080208     d IndDspF         ds
010400080206        // - Indicatori di gestione del subfile
010500080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010600080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010700080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010800080206     d  SflEnd                        1n   overlay(IndDspF : 33)
010900080206        // - Indicatori di errore
011000080206     d  errMessage                    1n   overlay(IndDspF : 28)
011100080606     d  errGenerico                   1n   overlay(IndDspF : 99)
011200080627        // - Indicatori vari
011300130829     d  protriga                      1n   overlay(IndDspF : 01)
011400130829     d  orariINT                      1n   overlay(IndDspF : 02)
011500130820     d  Immiss                        1n   overlay(IndDspF : 03)
011600081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
011700120111     d  F03Attivo                     1n   overlay(IndDspF : 05)
011800120111     d  F12Attivo                     1n   overlay(IndDspF : 06)
011900130828     d  Vis_CAP                       1n   overlay(IndDspF : 08)
012000130828     d  Vis_LOC                       1n   overlay(IndDspF : 09)
012100130829     d  Vis_rifer                     1n   overlay(IndDspF : 10)
012200131003     d  prot_tempo                    1n   overlay(IndDspF : 11)
012300131003     d  abilF7                        1n   overlay(IndDspF : 12)
012400131213     d  abilF9                        1n   overlay(IndDspF : 13)
012500130828     d  HI_dlna                       1n   overlay(IndDspF : 14)
012600130828     d  ProtLNA                       1n   overlay(IndDspF : 15)
012700131213     d  indLna9988                    1n   overlay(IndDspF : 16)
012800131213     d  protSoglia                    1n   overlay(IndDspF : 17)
012900130820     d  ErrOSTIS                      1n   overlay(IndDspF : 40)
013000130821     d  ErrOSTFS                      1n   overlay(IndDspF : 41)
013100130820     d  ErrOMIIS                      1n   overlay(IndDspF : 42)
013200130820     d  ErrOMXFS                      1n   overlay(IndDspF : 43)
013300130820     d  ErrTRZS                       1n   overlay(IndDspF : 44)
013400130821     d  ErrLNA                        1n   overlay(IndDspF : 45)
013500131010     d  ErrOlrsc                      1n   overlay(IndDspF : 47)
013600131010     d  ErrOlrnc                      1n   overlay(IndDspF : 48)
013700131010     d  ErrOlrnk                      1n   overlay(IndDspF : 49)
013800130828     d  BL_OSTIS                      1n   overlay(IndDspF : 50)
013900130828     d  BL_OSTFS                      1n   overlay(IndDspF : 51)
014000130828     d  BL_OMIIS                      1n   overlay(IndDspF : 52)
014100130828     d  BL_OMXFS                      1n   overlay(IndDspF : 53)
014200131010     d  BL_OLRSC                      1n   overlay(IndDspF : 55)
014300131010     d  BL_OLRNC                      1n   overlay(IndDspF : 56)
014400131010     d  BL_OLRNK                      1n   overlay(IndDspF : 57)
014500131213     d  BL_TRZS                       1n   overlay(IndDspF : 58)
014600080206
014700080206      // - Parametri ricevuti
014800080206     d KPJBA         e ds
014900080702
015000130820     d tisio0ds      e ds
015100080206
015200080206      // - Reperimento dati utente
015300080206     d TIBS34ds      e ds
015400080206     d dUte01        e ds
015500080702     d DLAT          e ds
015600130821     d ds15          e ds
015700131031     d ds5e          e ds
015800080702
015900080206
016000080206      // - Ricerca/Controllo tabelle
016100080206     d TIBS02ds      e ds                  inz
016200080627
016300080206      //---------------------------------------------------------------
016400080206      //?Definizione variabili globali.
016500080206      //---------------------------------------------------------------
016600080206
016700080206      // - Flags booleani
016800080208     d $Fine           s               n   inz(*off)
016900080208     d $InzS01         s               n   inz(*on)
017000120105     d wAumenta10      s               n   inz(*off)
017100120510     d wAumenta10f     s               n   inz(*off)
017200120110     d wAzzera10       s               n   inz(*off)
017300120105     d wDiminuisci10   s               n   inz(*off)
017400120109     d wForzaAgg       s               n   inz(*off)
017500080206
017600080627     d $Video          s              2    inz('S1')
017700080208     d S01nrr          s              4  0 inz
017800080627     d Indx            s              3  0 inz
017900081126     d yy              s              3  0 inz
018000130821     d Wrighe          s              3  0 inz
018100120109     d wOggi           s              8  0 inz
018200130820     d Tlna            s              1    inz
018300130821     d Wrecord         s              1    inz
018400131213     d Wsoglia         s              1    inz
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
024400131031
024500131031
024600080604     d
024700080605     d                 DS
024800080605     d  AA                     1      4  0
024900080605     d  MM                     5      6  0
025000080605     d  GG                     7      8  0
025100080605     d DATA                    1      8  0
025200130821     d                 DS
025300130821     d  wHH                    1      2  0
025400130821     d  wMM                    3      4  0
025500130821     d WORA                    1      4  0
025600081009     d
025700080206
025800080206      //---------------------------------------------------------------
025900080206      //?Definizione procedure usate.
026000080206      //---------------------------------------------------------------
026100080414      /copy gaitrasrc/srcprotopr,tibs02r
026200080414      /copy gaitrasrc/srcprotopr,tibs34r
026300080206
026400080206      //---------------------------------------------------------------
026500080206      //?Riepilogo indicatori.
026600080206      //---------------------------------------------------------------
026700080207      // - Comuni
026800080207      // 28    : Emissione messaggio di errore a video
026900080207      // - C01/S01
027000080207      // 30    : Condiziona SFLDSP    (*not)
027100080207      // 31    : Condiziona SFLDSPCTL (*not)
027200080207      // 30+31 : Condiziona SFLCLR
027300080207      // 32    : Condiziona SFLNXTCHG in S01
027400080207      // 50    : Errore: Opzione errata
027500080207      // - D01
027600080207      // - Comuni
027700080207      // 99    : Generico di Errore
027800080206      //---------------------------------------------------------------
027900080206
028000080206      //---------------------------------------------------------------
028100080206      //?M A I N - L I N E
028200080206      //---------------------------------------------------------------
028300080206
028400080627     c     *Entry        plist
028500080206     c                   parm                    KPJBA
028600130820     C                   PARM                    tiSIO0ds
028700130820     c                   clear                   do0err
028800130820     c                   clear                   do0msg
028900080702
029000080206      /free
029100080925
029200080206       // Operazioni iniziali
029300080206       exsr RoutInz;
029400080206
029500130820 2      if  Do0Err=' '    ;
029600081215
029700080206       // Gestione video
029800081215 3     DOW $Fine = *off;
029900081215 4       select;
030000080530
030100080530         // Videata di selezioni
030200080627           when $Video = 'S1';
030300080627             exsr GesS01;
030400080530
030500080702           other   ;
030600080206             $Fine = *on;
030700081215 4       endsl;
030800081215 3     ENDDO;
030900081215
031000081215 2     endif    ;
031100080206
031200080206       // Operazioni finali
031300080206       exsr RoutEnd;
031400080206
031500080206       //--------------------------------------------------------------
031600080206       //?Operazioni iniziali.
031700080206       //--------------------------------------------------------------
031800080206       BEGSR RoutInz;
031900131003
032000131003       AbilF7=*off  ;
032100131213       AbilF9=*off  ;
032200131213       IndLna9988=*off  ;
032300080627       $inzs01=*on;
032400080929       clear vsctes2         ;
032500131213       clear Wsoglia         ;
032600080703
032700080206         // Reperimento dati job
032800080206         exsr DatiJob;
032900081003
033000081003         // Se sono EDP accendo indicatore
033100081003         if %subst(knmus:1:3)<>'EDP'    ;
033200081003         UTENonEDP=*on   ;
033300130820         endif  ;
033400080627
033500080704         $Fine=*off   ;
033600080703
033700080702         // Vedo se modo gestione  o interrogazione
033800080703
033900130820         vscpgm='TISIO2R'   ;
034000080703
034100140206 1       if do0tric='G' and §utecap<>'I' ;
034200130830          vsctes2='  VARIAZIONE  '    ;
034300080929 1       endif    ;
034400130830
034500130830 1       if vsctes2= *blanks    ;
034600130830          orariInt =*on  ;
034700130830          vsctes2='INTERROGAZIONE'    ;
034800130830 1       endif;
034900130821
035000130821      /end-free
035100130820
035200130821     c*     Decodifico la nazione
035300130821     C                   MOVEL     1             KTBKUT
035400130821     C                   MOVEL     '15'          KTBCOD
035500130821     C                   MOVEL(P)  do0nar        KTBKEY
035600130821     C     KEYTAB        CHAIN     TABEL00F                           99
035700130821IF  1C     *IN99         IFEQ      *OFF
035800130821     C     TBLFLG        ANDEQ     *BLANKS
035900130821     C                   MOVEL     TBLUNI        DS15
036000130821     C                   MOVEL     §15DES        v1dnar
036100130821     c                   endif
036200130820
036300130821     C     KEYTAB        KLIST
036400130821     C                   KFLD                    KTBKUT                         *CODICE UTENTE
036500130821     C                   KFLD                    KTBCOD                         *TABELLA
036600130821     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
036700130820     C     KEYCPL        KLIST
036800130820     C                   KFLD                    Do0VER                         *VERSIONE
036900130820     C                   KFLD                    Do0NAR                         *NAZIONE
037000130820     C                   KFLD                    Do0CAP                         *CAP
037100130820     C                   KFLD                    KLOC                           *LOCALITA'
037200130820     C     KEYCPC        KLIST
037300130820     C                   KFLD                    Do0VER                         *VERSIONE
037400130820     C                   KFLD                    Do0NAR                         *NAZIONE
037500130820     C                   KFLD                    Do0CAP                         *CAP
037600130820
037700130822     c                   clear                   elalna
037800130822     c                   clear                   elalol
037900130822     c                   clear                   elalos
038000130827     c                   clear                   lna_prev
038100131213     c                   clear                   lna_es
038200131213     c                   clear                   lna_sog
038300140210     c                   clear                   lna_lol
038400131010     c* e la versione non è stata passata uso la data di riferimento per calcolarla
038500130828     c                   if        do0ver=0
038600130828     c
038700130828     c                   if        do0dri=0
038800130828     c                   movel     datasys       do0dri
038900130828     c                   endif
039000130828     C                   Z-ADD     Do0DRI        P96DRI            8 0
039100130828     C                   Z-ADD     *ZEROS        P96VER            5 0
039200130828     C                   CALL      'TISI96R'
039300130828     C                   PARM                    P96DRI
039400130828     C                   PARM                    P96VER
039500130828     c
039600130828     c                   eval      do0ver=p96ver
039700130828     c                   endif
039800130827     c*
039900130821     c* Caricamento dei dati della località passata
040000130827    0c                   if        Do0LOc<>*blanks
040100130828     c                   eval      Vis_LOC=*on
040200130828     c                   eval      Vis_CAP=*on
040300130820     C                   MOVEL     Do0LOC        KLOC
040400130820     C     KEYCPL        CHAIN     AZCPL01L
040500130820IF  1C                   IF        not %found(azcpl01l)
040600130820     C                   MOVEL     MSG(1)        Do0MSG                         *MESSAGGIO
040700130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
040800130827     c                   leavesr
040900130827   x1c                   else
041000130821     c* Se il livello di gestione è CAP--> prendo i dati da questo
041100130827    2c                   if        cplliv='L' and cpllna>0
041200130820     c                   movel     cpllna        elalna
041300140210    3c                   if        cpllos<>cpllna
041400130820     c                   movel     cpllos        elalos
041500130827    3c                   endif
041600140210    3c                   if        cpllol<>cpllna and cpllos<>cpllol
041700140210     c                   movel     cpllol        elalol
041800140210    3c                   endif
041900130821     c
042000130827     c                   exsr      car_lna
042100131031     c                   eval      wttc=cplttc
042200130827     c
042300130827    2c                   endif
042400130827    1C                   ENDIF
042500130820     c
042600130827    0c                   endif
042700130820
042800130827    0c                   if        (do0loc=*blanks and do0cap<>*blanks) or
042900130827     c                              cplliv='C' or (cplliv='L' and cpllna=0)
043000130828     c                   eval      Vis_CAP=*on
043100130820     C     KEYCPC        CHAIN     AZCPC01L
043200130821IF  1C                   IF        not %found(azcpc01l)
043300130821     C                   MOVEL     MSG(10)       Do0MSG                         *MESSAGGIO
043400130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
043500130827     c                   leavesr
043600130827    1c                   endif
043700130827
043800130820     c                   movel     cpclna        elalna
043900140210    1c                   if        cpclos<>cpclna
044000140210     c                   movel     cpclos        elalos
044100130827    1c                   endif
044200140210    1c                   if        cpclol<>cpclna and cpclos<>cpclol
044300140210     c                   movel     cpclol        elalol
044400130827    1C                   ENDIF
044500130827     c*
044600130827     c                   exsr      car_lna
044700131031     c                   eval      wttc=cpcttc
044800130827     c
044900130827     c* Carico eventuali eccezioni di linea se presenti
045000130827     C     KEYCPC        setll     AZCPe01L
045100130827     C     KEYCPC        reade     AZCPe01L
045200130827     c
045300130827    1c                   dow       not %eof(azcpe01l)
045400130827    2c                   if        cpelna>0
045500130827     c                   eval      indx=%lookup(cpelna:lna_prev)
045600130827    3c                   if        Indx=0
045700130905     c                   add       1             yy
045800130827     c                   z-add     cpelna        lna_prev(yy)
045900130827    3c                   endif
046000130827    2c                   endif
046100130827     c
046200130827    2c                   if        cpelos>0
046300130827     c                   eval      indx=%lookup(cpelos:lna_prev)
046400130827    3c                   if        Indx=0
046500130905     c                   add       1             yy
046600130827     c                   z-add     cpelos        lna_prev(yy)
046700130827    3c                   endif
046800130827    2c                   endif
046900140210     c
047000140210    2c                   if        cpelol>0
047100140210    3c                   eval      indx=%lookup(cpelol:lna_prev)
047200140210     c                   if        Indx=0
047300140210     c                   add       1             yy
047400140210     c                   z-add     cpelol        lna_prev(yy)
047500140210     c                   movel     'S'           lna_lol(yy)
047600140210    3c                   endif
047700140210    2c                   endif
047800130827     c
047900130827     C     KEYCPC        reade     AZCPe01L
048000130827    1c                   enddo
048100130827     c
048200130827    0C                   ENDIF
048300130827     c
048400130827     c* Se non passato il CAP ci deve essere la linea di arrivo (inserimento di filiale)
048500130827    0c                   if        do0loc=*blanks and do0cap=*blanks
048600130827     c* Se passato 999 --> orari standard
048700130827    1c                   if        do0lna=0
048800130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
048900130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
049000130827     c                   leavesr
049100130827    1c                   endif
049200131003
049300131003     c                   eval      AbilF7=*on
049400130827     c
049500130904    1c                   if        do0lna=999 or do0lna=888
049600130904     c                   eval      elalna=888
049700130904     c                   eval      elalol=999
049800131010     c
049900130827   x1c                   else
050000130827     c
050100130827     c     do0lna        chain     azorg01l
050200130827    2c                   if        not %found(azorg01l)
050300130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
050400130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
050500130827     c                   leavesr
050600130827    2c                   endif
050700130827     c                   eval      elalna=do0lna
050800131213     c
050900131213     c* Abilito tasto F9 se non presente orario soglia da filiale
051000131213      /free
051100131213         chain(n)  (do0ver:do0nar:kcap:kloc:elalna:'C':'P ') azcos03l ;
051200131213      /end-free
051300131213     c                   if        not %found(azcos03l)
051400140109     c* non abilito in interrogazione
051500140109     c                   if        OrariINT=*off
051600131213     c                   eval      AbilF9=*on
051700140109     c                   endif
051800140109     c
051900131213     c                   else
052000131213     c                   eval      wsoglia='S'
052100131213     c                   endif
052200131213     c
052300130827    1c                   endif
052400131213
052500130827     c                   exsr      car_lna
052600130827    0c                   endif
052700131213
052800130904     c* carico il record 888 per i controlli
052900130904     c                   clear                   kloc
053000130904     c                   clear                   kcap
053100130904      /free
053200130904         ktser='C'       ;
053300131216         clear ksoglia   ;
053400131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
053500131213         if %found(azcos03l)  ;
053600130904
053700130904         C88ostis=cosostis  ;
053800130904         C88ostfs=cosostfs  ;
053900130904         C88omiis=cosomiis  ;
054000130904         C88omxfs=cosomxfs  ;
054100131010         C88olrsc=cosolrsc  ;
054200131010         C88olrnc=cosolrnc  ;
054300131010         C88olrnk=cosolrnk  ;
054400131216         C88trzs =costrzs   ;
054500130904         endif ;
054600131216
054700131216         ksoglia='P ' ;
054800131216         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
054900131216         if %found(azcos03l)  ;
055000131216
055100131216         C88Postis=cosostis  ;
055200131216         C88Postfs=cosostfs  ;
055300131216         C88Pomiis=cosomiis  ;
055400131216         C88Pomxfs=cosomxfs  ;
055500131216         C88Polrsc=cosolrsc  ;
055600131216         C88Polrnc=cosolrnc  ;
055700131216         C88Polrnk=cosolrnk  ;
055800131216         C88Ptrzs =costrzs   ;
055900131216         endif ;
056000130904
056100131011         ktser='R'       ;
056200131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser)  azcos03l   ;
056300131213         if %found(azcos03l)  ;
056400130904
056500130904         R88ostis=cosostis  ;
056600130904         R88ostfs=cosostfs  ;
056700130904         R88omiis=cosomiis  ;
056800130904         R88omxfs=cosomxfs  ;
056900131010         R88olrsc=cosolrsc  ;
057000131010         R88olrnc=cosolrnc  ;
057100131010         R88olrnK=cosolrnK  ;
057200130904         endif ;
057300131003
057400131003       // Tolgo dalle linee previste le linee non ancora partite
057500131003         w003a='999'  ;
057600131003
057700140318 1       if   do0cap<>*blanks ;
057800131003         clear                   kloc       ;
057900131003         clear                   kcap       ;
058000131003
058100131003         yy =  1   ;
058200140318 2       dow lna_prev(yy)>0  ;
058300131003         // escludo linee 888 e 999  ;
058400140318 3       if lna_prev(YY) <> 888 and lna_prev(yy) <> 999  ;
058500131003
058600131216         chain(n)  (do0ver:do0nar:kcap:kloc:lna_prev(YY):'C':'P ') azcos03l ;
0587001403185              if  %found(azcos03l)  ;
058800131216                lna_sog(yy) ='S' ;
0589001403185              endif  ;
059000131216
0591001403183         endif  ;
059200131003
059300131003         yy=yy+1  ;
0594001403182        enddo  ;
0595001403181         endif  ;
059600131003
059700130904      /end-free
059800130820     c                   ENDSR
059900130827     c*--------------------------------------------------------------
060000130827     c* Carico le 3 linee principali
060100130827     c*--------------------------------------------------------------
060200130827     c     Car_lna       BEGSR
060300130827     c                   z-add     1             yy                3 0
060400130827     c                   z-add     elalna        lna_prev(yy)
060500131213     c* imposto soglia presente per 888, 9999 e per linea
060600131213     c                   if        elalna=888 or elalna=999
060700131213     c                             or wsoglia='S'
060800131213     c                   eval      lna_sog(yy)='S'
060900131213     c                   endif
061000131213
061100130827     c                   if        elalol>0
061200130827     c                   add       1             yy
061300130827     c                   z-add     elalol        lna_prev(yy)
061400140210     c                   movel     'S'           lna_lol(yy)
061500131213     c                   if        elalol=888 or elalol=999
061600131213     c                   eval      lna_sog(yy)='S'
061700131213     c                   endif
061800130827     c                   endif
061900130827     c                   if        elalos>0
062000130827     c                   add       1             yy
062100130827     c                   z-add     elalos        lna_prev(yy)
062200131213     c                   if        elalos=888 or elalos=999
062300131213     c                   eval      lna_sog(yy)='S'
062400131213     c                   endif
062500130827     c                   endif
062600131213
062700131213     c                   clear                   wsoglia
062800130827     c                   ENDSR
062900080703
063000130820      /free
063100130820
063200080206       //--------------------------------------------------------------
063300080206       //?Reperimento Dati del job (Utente/Operativi).
063400080206       //--------------------------------------------------------------
063500080206       BEGSR DatiJob;
063600080206
063700080206         in(E) §AzUte;
063800080206         if NOT %error;
063900080206           in(E) §DatiUte;
064000080206         endif;
064100080206         if %error or RSut = *blanks;
064200080206           clear TIBS34ds;
064300080206           tibs34r(tibs34ds);
064400080206           in §AzUte;
064500080206           in §DatiUte;
064600080206         endif;
064700140206         dute01=utefaf  ;
064800080206
064900080206       ENDSR;
065000130820       //--------------------------------------------------------------
065100130820       //?Carica AZCOS00F dalla località o dal cap se non presente
065200130820       //--------------------------------------------------------------
065300130820       BEGSR Carica_AZCOS ;
065400130821
065500130821        clear CAPostis;
065600130821        clear CAPostfs;
065700130821        clear cAPomiis;
065800130821        clear CAPomxfs;
065900131010        clear CAPolrsc;
066000131010        clear CAPolrnc;
066100131010        clear CAPolrnk;
066200130829        clear CAPprop ;
066300130829        clear CAPrifer;
066400130829        clear CAPvisual;
066500131213        clear CAPtrzs  ;
066600130821        clear v1himm  ;
066700130828        clear v1hforz ;
066800130829        clear v1hforzlna;
066900131213        protsoglia=*off  ;
067000131016        v1cprop='   |'   ;
067100130821
067200130827         if   tlna='?'   ;
067300130827         v1himm='E'  ;
067400130827         endif           ;
067500130827
067600130821         if klna= savlna;
067700130821         endif  ;
067800130821         savlna=klna  ;
067900130820
068000130820       v1clna=klna  ;
068100130821       v1hlna=klna  ;
068200130828
068300130828       exsr Decodi_lna  ;
068400130820
068500130821       if ktser  ='R'   ;
068600131216          v1dtser='RITIRI' ;
068700130828          HI_dlna=*on  ;
068800130820       else  ;
068900131216          v1dtser='CONSEG' ;
069000130828          HI_dlna=*off ;
069100130820       endif  ;
069200130822       v1htser=ktser  ;
069300131213
069400131213       // Soglia
069500131213       if ksoglia='P'  ;
069600131213         v1csogl  ='Pom'  ;
069700131213         else  ;
069800131213         clear v1csogl    ;
069900131213         endif  ;
070000131213
070100130820
070200130827       if Wrecord= 'N'    ;
070300130820        clear  v1costis  ;
070400130820        clear  v1costfs  ;
070500131016        clear  v1comiis  ;
070600131016        clear  v1comxfs  ;
070700131010        clear  v1colrsc  ;
070800131010        clear  v1colrnc  ;
070900131010        clear  v1colrnk  ;
071000130820        clear  v1ctrzs   ;
071100130820       else  ;
071200130820        v1costis=cosostis  ;
071300130820        v1costfs=cosostfs  ;
071400131016        v1comiis=cosomiis  ;
071500131016        v1comxfs=cosomxfs  ;
071600131010        v1colrsc=cosolrsc  ;
071700131010        v1colrnc=cosolrnc  ;
071800131010        v1colrnk=cosolrnk  ;
071900131213        v1ctrzs =costrzs   ;
072000131010
072100130827        select  ;
072200130827        when wrecord='C'  ;
072300131016        v1cprop='cap|'   ;
072400130827        when wrecord='F'  ;
072500131016        v1cprop='fil|'   ;
072600130827        when wrecord='9'  ;
072700131016        v1cprop='std|'   ;
072800130827        endsl  ;
072900130821
073000130820       endif ;
073100130820
073200130829       if Immiss    =*on  ;
073300130829        v1himm='S'  ;
073400130829       else  ;
073500130821       v1cute=cosute  ;
073600130821        if cosduv>0  ;
073700130821        dataiso=%date(cosduv)  ;
073800130821        datadmy=dataiso ;
073900130821        v1dduv=%dec(datadmy) ;
074000130821        endif  ;
074100130828
074200130902        // visto che la linea on è prevista e mi si sposiziona nella lettura del file
074300130902        // non eseguo la ricerca del riferimento
074400130902         if   tlna<>'?'   ;
074500130902          exsr cerca_rif ;
074600130902          else  ;
074700130902          wrecord='N' ;
074800130902         endif  ;
074900130902        endif  ;
075000130828
075100130902       // Se ho trovato il record memorizzo i dati
075200130828        if wrecord<>'N'    ;
075300130828        CAPostis=cosostis  ;
075400130828        CAPostfs=cosostfs  ;
075500130828        cAPomiis=cosomiis  ;
075600130828        CAPomxfs=cosomxfs  ;
075700131010        CAPolrsc=cosolrsc  ;
075800131010        CAPolrnc=cosolrnc  ;
075900131010        CAPolrnk=cosolrnk  ;
076000130828        CAPrifer=wrecord   ;
076100131213        CAPtrzs =costrzs   ;
076200130829        if wrecord='9'  ;
076300131213         CAPprop='OrRif:STD'  ;
076400130829         endif ;
076500130829        if wrecord='F'  ;
076600131213         CAPprop='OrRif:FIL'  ;
076700130829         endif ;
076800130829        if wrecord='C'  ;
076900131213         CAPprop='OrRif:CAP'  ;
077000130829         endif ;
077100130829
077200130828        else  ;
077300130828        clear CAPostis;
077400130828        clear CAPostfs;
077500130828        clear CAPomiis;
077600130828        clear CAPomxfs;
077700131010        clear CAPolrsc;
077800131010        clear CAPolrnc;
077900131010        clear CAPolrnk;
078000130829        clear CAPprop ;
078100131213        clear CAPtrzs ;
078200130828        CAPrifer='N'  ;
078300130828        endif  ;
078400130821
078500130829       exsr Protcampi    ;
078600130829
078700130820       s01nrr=s01nrr + 1 ;
078800130820       write sio2s01     ;
078900130820
079000130820       ENDSR;
079100080206
079200130828       //--------------------------------------------------------------
079300130828       //?Decodifica linea di arrivo
079400130828       //--------------------------------------------------------------
079500130828       BEGSR Decodi_lna  ;
079600130828
079700130904       select  ;
079800130904       when klna=999  ;
079900131213        v1dlna='Orari Std   '  ;
080000130904       when klna=888  ;
080100131213        v1dlna='OrariContr.'  ;
080200130904        other ;
080300130828       chain klna  azorg01l  ;
080400130828       if %found(azorg01l)  ;
080500131216        if orgde5<>*blanks   ;
080600131216          v1dlna=orgde5  ;
080700131216          else  ;
080800131216          v1dlna=orgdes  ;
080900131216          endif  ;
081000131216
081100130828       else  ;
081200130828         v1dlna=*all'?'  ;
081300130828       endif  ;
081400130904       endsl  ;
081500130828       ENDSR  ;
081600080206       //--------------------------------------------------------------
081700080627       //?Gestione SFL 01
081800080206       //--------------------------------------------------------------
081900080409       BEGSR GesS01;
082000080207
082100080207         // Inizializzazione videata
082200080409         if  $InzS01 = *on;
082300080409            exsr InzS01;
082400080703         // Posizionamento cursore
082500100507         // C01csr = 1;
082600080703           s01nrr = 1;
082700080703           sfldsp_n=*off;
082800080409            $InzS01  = *off;
082900080207         endif;
083000080207
083100080207
083200080207         // Emissione Testata e Piede con tasti funzionali abilitati
083300080207         if  errGenerico = *off;
083400131003         select   ;
083500131003         when §15ita<>'I'  ;
083600130822           sfldsp_n=*on      ;
083700130822           write  sio2D02;
083800131003
083900140206         when lnaprevuni=0 ;
084000131003           sfldsp_n=*on      ;
084100131003           write  sio2D03;
084200131003         other   ;
084300131003           write  sio2Z01;
084400131003           endsl  ;
084500080207         endif;
084600080703
084700080703         // Posizionamento cursore
084800080703         if  C01csr  > *zero;
084900080703           C01rcd = C01csr;
085000080703         endif  ;
085100080207
085200080207         // Emissione videata
085300130820         exfmt  sio2C01;
085400080410
085500080207         reset errMessage;
085600080207         reset errGenerico;
085700080702         clear Vscmsg;
085800130820         clear Errostis  ;
085900130820         clear Errostfs  ;
086000130820         clear Erromiis  ;
086100130820         clear Erromxfs  ;
086200131010         clear Errolrsc  ;
086300131010         clear Errolrnc  ;
086400131010         clear Errolrnk  ;
086500130820         clear ErrTrzs   ;
086600130821         clear Errlna    ;
086700130828         clear BL_ostis  ;
086800130828         clear BL_ostfs  ;
086900130828         clear BL_omiis  ;
087000130828         clear BL_omxfs  ;
087100131010         clear BL_olrsc  ;
087200131010         clear BL_olrnc  ;
087300131010         clear BL_olrnk  ;
087400131213         clear BL_trzs   ;
087500080207
087600080523 1       SELECT;
087700080207
087800080207         // - F3=Fine
087900080523 1         WHEN  dsp_aid = c_F03;
088000080409            $Fine = *on;
088100130820            Do0f03='1'   ;
088200130828
088300130829         // - F5=Ripristina orari di riferimento (disabilitato)
088400130829 1         // WHEN  dsp_aid = c_F05;
088500130829           // chain    c01csr sio2s01  ;
088600130829           // if %found     ;
088700130829           //  if v1costis<>CAPostis  ;
088800130829           //  v1costis=CAPostis  ;
088900130829           //  BL_ostis=*on  ;
089000130829           // endif  ;
089100130829           // if v1costfs<>CAPostfs  ;
089200130829           //  v1costfs=CAPostfs  ;
089300130829           //  BL_ostfs=*on  ;
089400130829           // endif  ;
089500130829           // if v1comiis<>CAPomiis  ;
089600130829           //  v1comiis=CAPomiis  ;
089700130829           //  BL_omiis=*on  ;
089800130829           // endif  ;
089900130829           // if v1comxfs<>CAPomxfs  ;
090000130829           //  v1comxfs=CAPomxfs  ;
090100130829           //  BL_omxfs=*on  ;
090200130829           // endif  ;
090300130829           // if v1ctrzs <>CAPtrzs   ;
090400130829           //  v1ctrzs =CAPtrzs   ;
090500130829           //  BL_trzs =*on  ;
090600130829           // endif  ;
090700130829           //  else  ;
090800130829            // cursore non ben posizionato : errore
090900130829           //  s01nrr=1  ;
091000130829           // chain    s01nrr sio2s01  ;
091100130829           // errlna  =*on ;
091200130829           // vscmsg = Msg(07);
091300130829           // endif         ;
091400130829
091500130829           // exsr aggiosfl      ;
091600130829
091700130829         // - F7=Visualizza orari di riferimento per tutte le righe
091800131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F07;
091900130829            yy=1           ;
092000130829            s01nrr=1  ;
092100130829            chain s01nrr    sio2s01    ;
092200130829
092300130829 0          dow %found    ;
092400130829
092500130829            if capvisual<>'S'  ;
092600130829            capvisual='S'  ;
092700130829            else  ;
092800130829            capvisual=' '  ;
092900130829            endif  ;
093000130829
093100130829            exsr aggiosfl      ;
093200130829
093300130829            s01nrr=s01nrr+1     ;
093400130829
093500130829            chain s01nrr    sio2s01    ;
093600130829           enddo  ;
093700080207
093800131213         // - F09=Abilita riga soglia
093900131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F09;
094000131213            yy=1           ;
094100131213            s01nrr=1  ;
094200131213            chain s01nrr    sio2s01    ;
094300131213
094400131213 0          dow %found    ;
094500131213
094600131216            if v1csogl ='Pom'    ;
094700131213            exsr protcampi  ;
094800131213
094900131213            exsr aggiosfl      ;
095000131216            AbilF9=*off        ;
095100131213            endif  ;
095200131213
095300131213            s01nrr=s01nrr+1     ;
095400131213
095500131213            chain s01nrr    sio2s01    ;
095600131213           enddo  ;
095700080207         // - F12=Ritorno
095800080523 1         WHEN  dsp_aid = c_F12;
095900080702            $Fine = *on;
096000130820            Do0f12='1'   ;
096100080414
096200131003         // - F16=Annullamento  (disabilito)
096300130829 1         WHEN  dsp_aid = c_F16;
096400130829         //  exsr annull_rec    ;
096500130829         //  $Fine = *on;
096600130829
096700080530 x1      // Invio / F6
096800080207           OTHER;
096900081006
097000081006        // Non effettuo controlli in interrogazione
097100131003 1           if orariInt=*off and sfldsp_n=*off ;
097200081006               exsr ContrS01 ;
097300081006 2             if  errGenerico = *on;
097400081006                 leavesr;
097500081006 2             endif;
097600081006 2           endif;
097700080530
097800081023         // F6=conferma Aggiornamento  F10=anche stampa
097900081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
098000131003             if sfldsp_n=*off  ;
098100131003              exsr   ConfAggio ;
098200131003             endif  ;
098300081023
098400080704           $Fine = *on;
098500080530           endif  ;
098600080207
098700080523 1       ENDSL;
098800080207
098900080207       ENDSR;
099000080207
099100080526       //--------------------------------------------------------------
099200080702       //?controlli SFL01
099300080409       //--------------------------------------------------------------
099400080409       BEGSR ContrS01;
099500130821
099600130821       yy=1  ;
0997001308291      dow yy<=12;
099800130821       if lna_es(yy)='R'  ;
099900130821       clear lna_es(YY)  ;
100000130821       endif  ;
100100130821
100200130821       yy=yy+1  ;
1003001308291      enddo    ;
100400130821
100500081126       yy=1           ;
100600080703       s01nrr=1  ;
100700130820       chain s01nrr    sio2s01    ;
100800080703
100900081006 0     dow %found    ;
101000130820
101100130821       clear  v1dlna  ;
101200130821
101300130828       klna=v1clna ;
101400130828       exsr decodi_lna  ;
101500130828 1     if v1dlna=*all'?'        ;
101600130821          errlna  =*on ;
101700130821          vscmsg = Msg(08);
101800130821          EXSR AggioSFL   ;
101900130821          leavesr;
102000130821 1     endif  ;
102100130821
102200130828       // La linea deve essere una di quelle previste
102300130829       //  se non prevista dico che la annullerò
102400130828         Indx=%lookup(v1clna:lna_prev)  ;
1025001308291        if Indx=0  and v1hforzlna=' '  ;
102600130828
102700130821          errlna  =*on ;
102800130821          vscmsg = Msg(04);
102900130829          v1hforzlna='S'  ;
103000130821
103100130821          EXSR AggioSFL   ;
103200130821          leavesr;
103300130821 1     endif  ;
103400130821
103500130822 1     if v1hlna<>v1clna ;
103600130821
103700130821       // Se la riga è RITIRO  controllo il linea doppia e la memorizzo
103800130822  2     if v1htser='R'  ;
103900130821
104000130828         if lna_es(indx)<>' '  ;
104100130821          errlna  =*on ;
104200130821          vscmsg = Msg(05);
104300130821          EXSR AggioSFL   ;
104400130821          leavesr;
104500130821  x3      else  ;
104600130828          lna_es(indx)='R'   ;
104700130821   3      endif  ;
104800130821
104900130821        savlna=v1clna  ;
105000130821
105100130821  x2   else  ;
105200130821   3   if v1clna  <> savlna  ;
105300130821          errlna  =*on ;
105400130821          vscmsg = Msg(13);
105500130821          EXSR AggioSFL   ;
105600130821          leavesr;
105700130821  3    endif  ;
105800130821  2    endif  ;
105900130822  1    endif  ;
106000130821
106100130822       // O R A R I
106200130829       //  li controllo solo per le linee valide
106300130829
106400130829  1a   if indx>0 ;
106500131216
106600131216       // controllo Soglia solo se l'ho abilitata ;
106700131216  1b   if v1csogl= *blanks   or AbilF9=*off  ;
106800131216
106900130829       // Se per la linea di arrivo manca il record di filiale --> non posso inserire
107000130829       if do0cap<>*blanks ;
107100130829       clear kloc  ;
107200130829       clear kcap  ;
107300130829       ktser=v1htser  ;
107400131213           chain(n)  (do0ver:do0nar:kcap:kloc:v1clna:ktser)  azcos03l   ;
107500131213           if not %found(azcos03l)  ;
107600130829          errlna  =*on ;
107700130829          vscmsg = Msg(15);
107800130829          %subst(vscmsg:40:3)=%editc(v1clna:'X')  ;
107900130829          EXSR AggioSFL   ;
108000130829          leavesr;
108100130829           endif  ;
108200130829       endif  ;
108300130829
108400130829
108500130904       // Imposto gli orari in base al servizio
108600130904       if v1clna<>888     ;
108700130904       if v1htser='C'   ;
108800131216       if v1csogl='Pom'  ;
108900131216       W88ostis=C88Postis ;
109000131216       W88ostfs=C88Postfs ;
109100131216       W88omiis=C88Pomiis ;
109200131216       W88omxfs=C88Pomxfs ;
109300131216       W88olrsc=C88Polrsc ;
109400131216       W88olrNc=C88Polrnc ;
109500131216       W88olrNk=C88Polrnk ;
109600131216       W88trzs =C88Ptrzs  ;
109700131216       else  ;
109800130904       W88ostis=C88ostis ;
109900130904       W88ostfs=C88ostfs ;
110000130904       W88omiis=C88omiis ;
110100130904       W88omxfs=C88omxfs ;
110200131010       W88olrsc=C88olrsc ;
110300131010       W88olrNc=C88olrnc ;
110400131010       W88olrNk=C88olrnk ;
110500131216       W88trzs =C88trzs  ;
110600131216       endif  ;
110700131216
110800130904       else  ;
110900130904       W88ostis=r88ostis ;
111000130904       W88ostfs=r88ostfs ;
111100130904       W88omiis=r88omiis ;
111200130904       W88omxfs=r88omxfs ;
111300131010       W88olrsc=r88olrsc ;
111400131010       W88olrnc=r88olrnc ;
111500131010       W88olrnk=r88olrnk ;
111600130904       endif  ;
111700131010       endif  ;
111800131010
111900131011       if v1clna= 888 or w88ostfs=0000  ;
112000130904       W88ostis=0000 ;
112100130904       W88ostfs=2400 ;
112200130904       W88omiis=0000 ;
112300130904       W88omxfs=2400 ;
112400131010       W88olrsc=2400 ;
112500131010       W88olrnc=2400 ;
112600131010       W88olrnk=2400 ;
112700131216       W88trzs =0000 ;
112800130904       endif  ;
112900130904
113000131216
113100131011       // Orario inizio
113200130829 1     if  v1costis=0  ;
113300130820          errostis=*on ;
113400130820          vscmsg = Msg(02);
113500081003          EXSR AggioSFL   ;
113600081003          leavesr;
113700130820 1     endif  ;
113800130821
113900130821       wora=v1costis ;
114000130821       exsr contr_ora ;
114100130829 1     if vscmsg<>*blanks ;
114200130821          errostis=*on ;
114300130821          EXSR AggioSFL   ;
114400130821          leavesr;
114500130821 1     endif  ;
114600130904
114700130904       // no minore del limite previsto
114800130904       if v1costis<w88ostis  ;
114900130904          errostis=*on ;
115000130904          vscmsg = Msg(16);
115100130904          %subst(vscmsg:37:5)=%editw(w88ostis: '0 :  ')    ;
115200130904          EXSR AggioSFL   ;
115300130904          leavesr;
115400130904 1     endif  ;
115500131031       // Orario inizio consegne no maggiore tempo consegna per tipo servizio
115600140210       // non controllo per orari del pomeriggio  e se si tratta di linea oltre
115700140210       if v1htser='C' and do0cap<>*blanks and lna_lol(indx)<>'S'
115800140110          and v1csogl=*blanks              ;
115900140110
116000131031          exsr car_cpt;
116100131031          if v1costis>wcpt0in4;
116200131031             errostis=*on ;
116300131031             vscmsg = Msg(21);
116400131031             vscmsg=%trim(vscmsg)+' ' + %trim(wdtsp);
116500131031             EXSR AggioSFL   ;
116600131031             leavesr;
116700131031          endif;
116800131031       endif;
116900130821
117000130821
117100131011       // Orario fine
117200130820 1     if  v1costfs=0     ;
117300130820          errostfs=*on ;
117400130820          vscmsg = Msg(02);
117500130820          EXSR AggioSFL   ;
117600130820          leavesr;
117700130820 1     endif  ;
117800130821
117900130821       wora=v1costfs ;
118000130821       exsr contr_ora ;
118100130829 1     if vscmsg<>*blanks ;
118200130821          errostfs=*on ;
118300130821          EXSR AggioSFL   ;
118400130821          leavesr;
118500130821 1     endif  ;
118600130904       // no maggiore del limite previsto
118700130904       if v1costfs>w88ostfs  ;
118800130904          errostfs=*on ;
118900130904          vscmsg = Msg(17);
119000130904          %subst(vscmsg:37:5)=%editw(w88ostfs: '0 :  ')    ;
119100130904          EXSR AggioSFL   ;
119200130904          leavesr;
119300130904 1     endif  ;
119400130820
119500130820 1     if  v1costfs<v1costis  ;
119600130820          errostfs=*on ;
119700130820          vscmsg = Msg(03);
119800130820          EXSR AggioSFL   ;
119900130820          leavesr;
120000130820 1     endif  ;
120100131011
120200131016       if v1htser='C'  ;
120300131216       // Tempo oraganizzazione --> solo per CONSEGNE
120400131216 1     if  v1ctrzs<w88trzs   or v1ctrzs>1200 ;
120500131216          Errtrzs =*on ;
120600131216          vscmsg = Msg(22);
120700131216          %subst(vscmsg:37:5)=%editw(w88trzs:'0 :  ')  ;
120800131216          EXSR AggioSFL   ;
120900131216          leavesr;
121000131216 1     endif  ;
121100131216
121200131016       // Orario MINIMO  solo per CONSEGNE
121300131016 1     if  v1comiis=0  ;
121400131016          erromiis=*on ;
121500131016          vscmsg = Msg(02);
121600131016          EXSR AggioSFL   ;
121700131016          leavesr;
121800131016 1     endif  ;
121900131016
122000131016       wora=v1comiis ;
122100131016       exsr contr_ora ;
122200131016 1     if vscmsg<>*blanks ;
122300131016          erromiis=*on ;
122400131016          EXSR AggioSFL   ;
122500131016          leavesr;
122600131016 1     endif  ;
122700131016
122800131016       // no minore del limite previsto
122900131016       if v1comiis<w88omiis  ;
123000131016          erromiis=*on ;
123100131016          vscmsg = Msg(16);
123200131016          %subst(vscmsg:37:5)=%editw(w88omiis: '0 :  ')    ;
123300131016          EXSR AggioSFL   ;
123400131016          leavesr;
123500131016 1     endif  ;
123600131016
123700131016       // no maggiore dell'orario di inizio
123800131016       if v1comiis > v1costis  ;
123900131016          erromiis=*on ;
124000131016          vscmsg = Msg(11);
124100131016          EXSR AggioSFL   ;
124200131016          leavesr;
124300131016 1     endif  ;
124400131016
124500131016       // Orario MASSIMO
124600131016 1     if  v1comxfs=0     ;
124700131016          erromxfs=*on ;
124800131016          vscmsg = Msg(02);
124900131016          EXSR AggioSFL   ;
125000131016          leavesr;
125100131016 1     endif  ;
125200131016
125300131016       wora=v1comxfs ;
125400131016       exsr contr_ora ;
125500131016 1     if vscmsg<>*blanks ;
125600131016          erromxfs=*on ;
125700131016          EXSR AggioSFL   ;
125800131016          leavesr;
125900131016 1     endif  ;
126000131016       // no maggiore del limite previsto
126100131016       if v1comxfs>w88omxfs  ;
126200131016          erromxfs=*on ;
126300131016          vscmsg = Msg(17);
126400131016          %subst(vscmsg:37:5)=%editw(w88omxfs: '0 :  ')    ;
126500131016          EXSR AggioSFL   ;
126600131016          leavesr;
126700131016 1     endif  ;
126800131016
126900131016 1     if  v1comxfs<v1comiis  ;
127000131016          erromxfs=*on ;
127100131016          vscmsg = Msg(03);
127200131016          EXSR AggioSFL   ;
127300131016          leavesr;
127400131016 1     endif  ;
127500131016       // no minore   dell'orario di fine
127600131016       if v1comxfs < v1costfs  ;
127700131016          erromxfs=*on ;
127800131016          vscmsg = Msg(12);
127900131016          EXSR AggioSFL   ;
128000131016          leavesr;
128100131016 1     endif  ;
128200131016 1     endif  ;
128300131016
128400131011       if v1htser='R'  ;
128500131011
128600131011       // Orari limite ritiri: COMMISSIONATI
128700131011 1     if  v1colrsc=0     ;
128800131011          erroLRSC=*on ;
128900131011          vscmsg = Msg(02);
129000131011          EXSR AggioSFL   ;
129100131011          leavesr;
129200131011 1     endif  ;
129300131011
129400131011       wora=v1coLRSC ;
129500131011       exsr contr_ora ;
129600131011 1     if vscmsg<>*blanks ;
129700131011          erroLRSC=*on ;
129800131011          EXSR AggioSFL   ;
129900131011          leavesr;
130000131011 1     endif  ;
130100131011       // no maggiore del limite previsto
130200131011       if v1coLRSC>w88oLRSC  ;
130300131011          erroLRSC=*on ;
130400131011          vscmsg = Msg(17);
130500131011          %subst(vscmsg:37:5)=%editw(w88oLRSC: '0 :  ')    ;
130600131011          EXSR AggioSFL   ;
130700131011          leavesr;
130800131011 1     endif  ;
130900131011       // Orari limite ritiri: NON COMMISSIONATI
131000131011 1     if  v1colrNc=0     ;
131100131011          erroLRNC=*on ;
131200131011          vscmsg = Msg(02);
131300131011          EXSR AggioSFL   ;
131400131011          leavesr;
131500131011 1     endif  ;
131600131011
131700131011       wora=v1coLRNC ;
131800131011       exsr contr_ora ;
131900131011 1     if vscmsg<>*blanks ;
132000131011          erroLRNC=*on ;
132100131011          EXSR AggioSFL   ;
132200131011          leavesr;
132300131011 1     endif  ;
132400131011       // no maggiore del limite previsto
132500131011       if v1coLRNC>w88oLRNC  ;
132600131011          erroLRNC=*on ;
132700131011          vscmsg = Msg(17);
132800131011          %subst(vscmsg:37:5)=%editw(w88oLRNC: '0 :  ')    ;
132900131011          EXSR AggioSFL   ;
133000131011          leavesr;
133100131011 1     endif  ;
133200131011       // Orari limite ritiri: NON COMMISSIONATI CODIFICATI
133300131011 1     if  v1colrNK=0     ;
133400131011          erroLRNK=*on ;
133500131011          vscmsg = Msg(02);
133600131011          EXSR AggioSFL   ;
133700131011          leavesr;
133800131011 1     endif  ;
133900131011
134000131011       wora=v1coLRNK ;
134100131011       exsr contr_ora ;
134200131011 1     if vscmsg<>*blanks ;
134300131011          erroLRNK=*on ;
134400131011          EXSR AggioSFL   ;
134500131011          leavesr;
134600131011 1     endif  ;
134700131011       // no maggiore del limite previsto
134800131011       if v1coLRNK>w88oLRNK  ;
134900131011          erroLRNK=*on ;
135000131011          vscmsg = Msg(17);
135100131011          %subst(vscmsg:37:5)=%editw(w88oLRNK: '0 :  ')    ;
135200131011          EXSR AggioSFL   ;
135300131011          leavesr;
135400131011 1     endif  ;
135500131011
135600131011       // secondo orario >= al primo
135700131011       if v1colrnc<v1colrsc  ;
135800131011          erroLRNc=*on ;
135900131011          vscmsg = Msg(18);
136000131011          EXSR AggioSFL   ;
136100131011          leavesr;
136200131011       endif  ;
136300131011       // terzo orario >= al secondo
136400131011       if v1colrnk<v1colrnc  ;
136500131011          erroLRNk=*on ;
136600131011          vscmsg = Msg(19);
136700131011          EXSR AggioSFL   ;
136800131011          leavesr;
136900131011       endif  ;
137000131011
137100131011       endif  ;
137200130820
137300131216 1b    endif  ;
137400131216 1a    endif  ;
137500130821
137600130821       EXSR AggioSFL   ;
137700080703
137800080703       s01nrr=1+s01nrr  ;
137900130820       chain s01nrr    sio2s01    ;
138000081006 0     enddo         ;
138100081006
138200080409       ENDSR;
138300080409
138400130829       //--------------------------------------------------------------
138500130829       //?Annullamento record
138600130829       //--------------------------------------------------------------
138700130829       //BEGSR Annull_rec  ;
138800130829
138900130829       // Elaboro la schiera delle linee e deleto
139000130829       //yy=1   ;
139100130829       //dow lna_prev(YY)>0     ;
139200130829
139300130829       //klna=lna_prev(yy)     ;
139400130829       //ktser='R'  ;
139500131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
139600130829       //ktser='C'  ;
139700131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
139800130829
139900130829       //yy=yy+1  ;
140000130829       //enddo   ;
140100130829
140200130829       //ENDSR;
140300080703       //--------------------------------------------------------------
140400080703       //?Aggiornamento sfl
140500080703       //--------------------------------------------------------------
140600080703       BEGSR AGGIOSFL  ;
140700080703       if vscmsg<>*blanks   ;
140800080703       errMessage  = *on;
140900080703       errGenerico = *on;
141000080704       c01csr=s01nrr    ;
141100080703       endif    ;
141200080703
141300080703       exsr ProtCampi   ;
141400080703
141500130820       update  sio2s01  ;
141600080703       ENDSR   ;
141700130821       //--------------------------------------------------------------
141800130821       //?Controllo campo ORA
141900130821       //--------------------------------------------------------------
142000130821       BEGSR Contr_ORA ;
142100130821       if whh < 0  or whh > 24  ;
142200130821          vscmsg = Msg(09);
142300130821       endif  ;
142400130821       if wmm < 0  or wmm > 59  ;
142500130821          vscmsg = Msg(09);
142600130821       endif  ;
142700131011
142800131011       // Minuti arrotondati ai 15 minuti
142900131011       if wmm<>00 and wmm<>15 and wmm<>30 and wmm<>45  ;
143000131011          vscmsg = Msg(20);
143100131011       endif  ;
143200130821
143300130821       ENDSR ;
143400080207       //--------------------------------------------------------------
143500080627       //?Inizializzazione SFL01
143600080207       //--------------------------------------------------------------
143700080409       BEGSR InzS01;
143800080207
143900080207       // Pulizia subfile
144000080207         SflDsp_N    = *on;
144100080207         SflDspCtl_N = *on;
144200130820         write  sio2C01;
144300080207         SflDspCtl_N = *off;
144400080207         SflEnd      = *off;
144500080530
144600080530         clear C01rcd;
144700100507         C01csr=1;
144800080627         S01nrr=0 ;
144900080702         clear Vscmsg;
145000080207         errMessage  = *off;
145100080207         errGenerico = *off;
145200130820         immiss    =*off ;
145300130821         clear savlna ;
145400130821         clear yy    ;
145500130821         clear lna_es;
145600130821         clear Wrecord ;
145700130821         clear v1dduv  ;
145800130821         clear v1cute  ;
145900130821
146000130822         // Se la nazione no è italia --> non carico nulla e avviso che non esistono
146100130822         //  orari
146200130822
146300130827 1       if §15ita= 'I'  ;
146400130820
146500130821         // 1)
146600130821         // Leggo e carico eventuali linee non più previste dal cap/località
146700130821
146800130828 1a    if   do0cap<>*blanks   ;
146900130828
147000130829       Protlna=*off ;
147100131213       setll  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
147200131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
147300131213 2      dow  not %eof(azcos03l)  ;
147400130821          klna=coslna ;
147500130827          Indx=%lookup(coslna:lna_prev)  ;
147600130827 3      if indx=0 ;
147700130821          tlna='?'   ;
147800130829           ktser=costser   ;
147900130821          exsr   Leggi_cos ;
1480001308273       endif  ;
148100130821
148200131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
1483001308272      enddo  ;
1484001308281a     endif  ;
148500130821
148600130821         // 2)
148700130821         // Carico i record  già presenti
148800130829         Protlna=*on  ;
148900130902         clear tlna   ;
149000130820
149100130827           YY=1  ;
149200140206 2     //  dow lna_prev(yy)>0  ;
149300140206           for  yy=1 to %elem(lna_prev);
149400140206           if lna_prev(yy)>0;
149500130827
149600130827           klna=lna_prev(YY) ;
149700131213           chain  (do0ver:do0nar:do0cap:do0loc:klna)  azcos03l   ;
149800131213 3         if %found(azcos03l)  ;
149900130827           lna_es(YY)='S'  ;
150000130830           endif  ;
150100131010
150200131213           if klna=999  or klna=888 ;
150300131213            indlna9988=*on  ;
150400131010            else  ;
150500131213            indlna9988=*off ;
150600131010            endif  ;
150700130830
150800131213           ksoglia=*blanks ;
150900131213           ktser='R'  ;
151000130821           exsr Leggi_cos  ;
151100130821           ktser='C'  ;
151200130821           exsr Leggi_cos  ;
151300131213           // Per consegne vedo se presente anche soglia "P"
151400131213
151500131216           if lna_sog(yy)='S'  or abilF9=*on  ;
151600131213           ksoglia='P'  ;
151700131213           exsr Leggi_cos  ;
151800131213           endif  ;
151900130827
152000140206       //  yy=yy+1  ;
152100140206           endif;
152200140206           endfor;
152300140206 2     //  enddo    ;
152400130821
1525001308271        endif  ;
152600130827
152700080702         ENDSR    ;
152800130821       //--------------------------------------------------------------
152900130821       //?Lettura record di AZCOS
153000130821       //--------------------------------------------------------------
153100130821           BEGSR Leggi_cos  ;
153200131213           chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia) azcos03l  ;
153300131213           if %found(azcos03l)  ;
153400130821           Immiss=*off ;
153500130821           wrecord='S'  ;
153600130828           else  ;
153700130821
153800130821           Immiss=*on  ;
153900130828           // Cerca il record di riferimento
154000130828           exsr cerca_rif  ;
154100130828
154200130828           endif  ;
154300130821
154400130821           exsr      Carica_AZCOS ;
154500131213
154600130821           ENDSR  ;
154700130828       //--------------------------------------------------------------
154800130828       //?Cerco record di riferimento per vedere se dati uguali o diversi
154900130828       //--------------------------------------------------------------
155000130828           BEGSR   Cerca_rif  ;
155100130828           wrecord='N'  ;
155200130828
155300130904           if klna<>999  and klna<>888 ;
155400130828
155500130829           if do0cap<>*blanks    ;
155600130829
155700130828 1         if do0loc<>*blanks   ;
155800130828            clear kloc  ;
155900131213           chain(n)  (do0ver:do0nar:do0cap:kloc:klna:ktser:ksoglia) azcos03l ;
156000131213           if %found(azcos03l)  ;
156100130828           wrecord='C'  ;
156200130828           endif  ;
156300130828 1         endif  ;
156400130828
156500130828           // Se non presente a livello di CAP provo a livello di filiale
156600130828 1         if wrecord='N'  ;
156700130828           clear kcap  ;
156800131213           chain(n)  (do0ver:do0nar:kcap:kloc:klna:ktser:ksoglia)  azcos03l   ;
156900131213           if %found(azcos03l)  ;
157000130828           wrecord='F'  ;
157100130828           endif  ;
157200130828 1         endif  ;
157300130829
157400130829 1         endif  ;
157500130828
157600131213           // standard aziendale
157700130828 1         if wrecord='N'  ;
157800130828           clear kcap  ;
157900131213           chain(n)  (do0ver:do0nar:kcap:kloc:lna999:ktser:ksoglia) azcos03l ;
158000131213           if %found(azcos03l)  ;
158100130828           wrecord='9'  ;
158200130828           endif  ;
158300130828 1         endif  ;
158400130828
158500130828 1         endif  ;
158600130828           ENDSR  ;
158700080702       //--------------------------------------------------------------
158800080702       //?Gestione protezione campi del sfl
158900080702       //--------------------------------------------------------------
159000080702          BEGSR   ProtCampi   ;
159100130820           Immiss  =*off ;
159200130829           vis_rifer=*off      ;
159300130829           BL_ostis=*off ;
159400130829           BL_ostfs=*off ;
159500130829           BL_omiis=*off ;
159600130829           BL_omxfs=*off ;
159700131010           BL_olrsc=*off ;
159800131010           BL_olrnc=*off ;
159900131010           BL_olrnK=*off ;
160000131213           BL_trzs =*off ;
160100130829           Protriga=*off ;
160200130829           Protlna=*on  ;
160300131216           Protsoglia=*off   ;
160400131213           IndLna9988=*off ;
160500080703
160600081007           // Se riga di categoria accendo solo protezione e sottolineatura
160700130821          if v1himm='S'    ;
160800130820           Immiss  =*on    ;
160900130820          endif            ;
161000130829
161100130829          // Protezine per linea di arrivo non prevista
161200130829          // Sempre protezione per interrogazione
161300130829          if v1himm='E'    or orariint=*on  ;
161400130829           Protriga=*on  ;
161500130821          endif            ;
161600130829          if v1himm='E' ;
161700130829           Protlna=*off ;
161800130829          endif            ;
161900130829
162000130828          if v1htser='R'  ;
162100130828          HI_dlna=*on  ;
162200130828          else  ;
162300130828          HI_dlna=*off ;
162400130828          endif  ;
162500131003
162600131010          // Se tipo servizio  ritiro non faccio vedere il tempo organizz
162700131003          if v1htser='R'  ;
162800131003          prot_tempo=*on  ;
162900131003          else ;
163000131003          prot_tempo=*off ;
163100131003          endif  ;
163200130821
163300130829          if CAPvisual= 'S'  ;
163400130829          vis_rifer=*on  ;
163500130829          endif  ;
163600131213
163700131213       // Se il record soglia c'è disabilito F9
163800131216       if v1csogl='Pom' ;
163900131216       if CAPrifer='S' or dsp_aid = c_F09 or AbilF9=*off ;
164000131213        abilF9=*off ;
164100131213        protsoglia=*off   ;
164200131213       else   ;
164300131213        abilF9=*on  ;
164400131213        protsoglia=*on    ;
164500131213       endif   ;
164600131213       endif   ;
164700131010
164800131213          //if v1clna=999  ;
164900131213          // indlna999=*on  ;
165000131213          //endif  ;
165100130829
165200130829          // Evidenzio i campi diversi dal riferimento
165300131003          // solo se sono a livello di CAP/località
165400131003          // non per filiale o standard
165500131003          if do0cap<>*blanks    ;
165600131003
165700130829          if v1costis<>capostis ;
165800130829           BL_ostis=*on ;
165900130829          endif  ;
166000130829          if v1costfs<>capostfs ;
166100130829           BL_ostfs=*on ;
166200130829          endif  ;
166300131016          if v1comiis<>capomiis ;
166400131016           BL_omiis=*on ;
166500131016          endif  ;
166600131016          if v1comxfs<>capomxfs ;
166700131016           BL_omxfs=*on ;
166800131016          endif  ;
166900131010          if v1colrsc<>capolrsc ;
167000131010           BL_olrsc=*on ;
167100131010          endif  ;
167200131010          if v1colrnc<>capolrnc ;
167300131010           BL_olrnc=*on ;
167400131010          endif  ;
167500131010          if v1colrnk<>capolrnk ;
167600131010           BL_olrnk=*on ;
167700131010          endif  ;
167800131213          if v1ctrzs <>captrzs  ;
167900131213           BL_trzs =*on ;
168000131213          endif  ;
168100131010
168200131010
168300131003          endif  ;
168400130829
168500080702          ENDSR    ;
168600080702       //--------------------------------------------------------------
168700080702       //?conferma aggiornamento
168800080702       //--------------------------------------------------------------
168900080702        BEGSR ConfAGGIO              ;
169000081003
169100080704       s01nrr=1  ;
169200130820       chain s01nrr    sio2s01    ;
169300080704
169400080704 1     dow %found    ;
169500130821
169600130821 2     if  v1clna>0  ;
169700130821
169800140116       // Elaboro record soglia solo se
169900140116 2a    if v1csogl=*blanks or (abilF9=*off or caprifer='S')   ;
170000140116       Klna=v1hlna  ;
170100130828       ktser=v1htser  ;
170200131216       ksoglia=%subst(v1csogl:1:1)  ;
170300130829
170400131216       // Se la linea on è prevista--> la deleto
170500130829 3     if v1himm='E'   ;
170600131216         delete (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
170700130829 x3    else    ;
170800130829
170900131216         chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
171000130829       // se il record  è diventato uguale al record di riferimento
171100130829       //  lo deleto ma non per filiale
171200130829  4       if  CAPostis= v1costis   and
171300130828              CAPostfs= v1costfs   and
171400131016              CAPomiis= v1comiis   and
171500131016              CAPomxfs= v1comxfs   and
171600131010              CAPolrsc= v1colrsc   and
171700131010              CAPolrnc= v1colrnc   and
171800131010              CAPolrnk= v1colrnk   and
171900131216              CAPtrzs = v1ctrzs    and
172000130829              do0cap<>*blanks           ;
172100131213  5           if %found(azcos03l)  ;
172200130821               delete azcos000   ;
172300130829  5           endif  ;
172400130829  x4        else  ;
172500131213  5           if not %found(azcos03l)  ;
172600130821              clear azcos000  ;
172700130821              cosver=do0ver ;
172800130821              cosnar=do0nar ;
172900130821              coscap=do0cap ;
173000130821              cosloc=do0loc ;
173100130829  5           endif  ;
173200130821              cosostis= v1costis      ;
173300130821              cosostfs= v1costfs      ;
173400131016              cosomiis= v1comiis      ;
173500131016              cosomxfs= v1comxfs      ;
173600131010              cosolrsc= v1colrsc   ;
173700131010              cosolrnc= v1colrnc   ;
173800131010              cosolrnk= v1colrnk   ;
173900130821              costrzs = v1ctrzs         ;
174000130821              cosute  =knmus       ;
174100130821              cosduv=%dec(datasys)  ;
174200130821              costser=ktser  ;
174300130821              coslna=v1clna  ;
174400131216              cosserv=ksoglia ;
174500131213  5           if %found(azcos03l)  ;
174600130821               update azcos000   ;
174700130821               else  ;
174800130821               write  azcos000   ;
174900130829  5           endif  ;
175000130829  4        endif  ;
175100130829  3        endif  ;
175200140116 2a      endif  ;
175300140116 2       endif  ;
175400080704
175500080704        s01nrr=s01nrr+1 ;
175600130820        chain s01nrr    sio2s01    ;
175700080704  1     enddo           ;
175800080704
175900080702        ENDSR;
176000131031       //--------------------------------------------------------------
176100131031       //?Caricamento orario più basso del tipo tempo consegna
176200131031       //--------------------------------------------------------------
176300131031        BEGSR car_cpt                ;
176400131031        clear wcpt0in;
176500131031        clear wcpt0in4;
176600131031        clear wdtsp  ;
176700131031        clear wtsp   ;
176800131031        setll (do0ver:wttc) azcpt01l   ;
176900131031        reade (do0ver:wttc) azcpt01l   ;
177000131031        dow not %eof(azcpt01l);
177100131031           if cpt0in<wcpt0in or wcpt0in=0;
177200131031              wcpt0in=cpt0in;
177300131031              wtsp   =cpttsp;
177400131031           endif;
177500131031           reade (do0ver:wttc) azcpt01l   ;
177600131031        enddo;
177700131031        // decodifico il tipo servizio memorizzato
177800131031        clear ds5e;
177900131031        chain (1:'5E':wtsp) tabel00f;
178000131031        if %found(tabel00f);
178100131031           ds5e=tbluni;
178200131031        endif;
178300131031        wdtsp=§5ed08;
178400131031        wcpt0in4=wcpt0in*100;
178500131031        endsr;
178600080702
178700080206       //--------------------------------------------------------------
178800080206       //?Operazioni finali.
178900080206       //--------------------------------------------------------------
179000080206       BEGSR RoutEnd;
179100080627
179200080704         // Chiusura pgm   ;
179300131003         // clear tibs02ds  ;
179400131003         // t02tla='C'      ;
179500131003         // TNTBE_RicercaControllo  (kpjba : tibs02ds);
179600080704
179700080206         *inLR = *on;
179800080704
179900080206         return;
180000080206
180100080206       ENDSR;
180200080206
180300131003
180400080206      /end-free
180500080206
180600080206       //--------------------------------------------------------------
180700080206       //?Schiere a tempo di compilazione.
180800080206       //--------------------------------------------------------------
180900080206
181000080410** - MSG ------------------------------------------------------------------ -*
181100130821Non trovata località: impossibile caricare gli orari servizi!!                            1
181200130821Immettere un orario formalmente corretto                                                  2
181300130821L'orario di fine deve essere maggiore dell'orario iniziale                                3
181400130829Linea non prevista dal cap/località. Verrà eliminata. Enter per continuare                4
181500130829Linea già immessa                                                                         5
181600130828L'inserimento non è ECCEZIONE:orari = a quelli
181700130829Posizionarsi sulla riga di cui si voglio ripristinare gli orari e premere F05             7
181800130821Linea inesistente                                                                         8
181900130821Orario impostato errato                                                                   9
182000130821Non trovato CAP: impossibile caricare gli orari servizi!!                                10
182100130821L'orario minimo non può essere maggiore dell'orario di inizio standard                   11
182200130821L'orario massimo non può essere minore dell'orario di fine standard                      12
182300130822La linea servizio CONSEGNE deve essere uguale alla precedente servizio RITIRO            13
182400130827Non trovata FILIALE: impossibile caricare gli orari servizi!!                            14
182500130904Immettere PRIMA gli orari PER la LINEA xxx, prima di inserire per cap/loc                15
182600130904L'orario non può essere inferiore a xx:xx                                                16
182700130904L'orario non può essere maggiore di xx:xx                                                17
182800131011Orario limite ritiri NON commissionati DEVE essere > = al limite dei commissionati       18
182900131011Orario limite ritiri Cli Codificati DEVE essere > = al limite dei NON commissionati      19
183000131011Si possono inserire soltanto orari arrotondati ai quarti d'ora                           20
183100131031Orario inizio consegna non congruente con tempo di consegna del serv.                    21
183200131216Tempo oraganizzazione non minore di xx:xx  e non maggiore di 12:00                       22
