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
006900130905     d lna_prev        s              3  0 dim(14)
007000131003     d
007100131003     D FIL_AB          S              3    DIM(85)
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
017600081006     d XX              s              3  0 inz
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
034000130830 1       if do0tric='G';
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
038200131010     c* e la versione non è stata passata uso la data di riferimento per calcolarla
038300130828     c                   if        do0ver=0
038400130828     c
038500130828     c                   if        do0dri=0
038600130828     c                   movel     datasys       do0dri
038700130828     c                   endif
038800130828     C                   Z-ADD     Do0DRI        P96DRI            8 0
038900130828     C                   Z-ADD     *ZEROS        P96VER            5 0
039000130828     C                   CALL      'TISI96R'
039100130828     C                   PARM                    P96DRI
039200130828     C                   PARM                    P96VER
039300130828     c
039400130828     c                   eval      do0ver=p96ver
039500130828     c                   endif
039600130827     c*
039700130821     c* Caricamento dei dati della località passata
039800130827    0c                   if        Do0LOc<>*blanks
039900130828     c                   eval      Vis_LOC=*on
040000130828     c                   eval      Vis_CAP=*on
040100130820     C                   MOVEL     Do0LOC        KLOC
040200130820     C     KEYCPL        CHAIN     AZCPL01L
040300130820IF  1C                   IF        not %found(azcpl01l)
040400130820     C                   MOVEL     MSG(1)        Do0MSG                         *MESSAGGIO
040500130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
040600130827     c                   leavesr
040700130827   x1c                   else
040800130821     c* Se il livello di gestione è CAP--> prendo i dati da questo
040900130827    2c                   if        cplliv='L' and cpllna>0
041000130820     c                   movel     cpllna        elalna
041100130827    3c                   if        cpllol<>cpllna
041200130820     c                   movel     cpllol        elalol
041300130827    3c                   endif
041400130827    3c                   if        cpllos<>cpllna and cpllos<>cpllol
041500130820     c                   movel     cpllos        elalos
041600130827    3c                   endif
041700130821     c
041800130827     c                   exsr      car_lna
041900131031     c                   eval      wttc=cplttc
042000130827     c
042100130827    2c                   endif
042200130827    1C                   ENDIF
042300130820     c
042400130827    0c                   endif
042500130820
042600130827    0c                   if        (do0loc=*blanks and do0cap<>*blanks) or
042700130827     c                              cplliv='C' or (cplliv='L' and cpllna=0)
042800130828     c                   eval      Vis_CAP=*on
042900130820     C     KEYCPC        CHAIN     AZCPC01L
043000130821IF  1C                   IF        not %found(azcpc01l)
043100130821     C                   MOVEL     MSG(10)       Do0MSG                         *MESSAGGIO
043200130820     C                   MOVEL     '1'           Do0err                         *FINE PGM
043300130827     c                   leavesr
043400130827    1c                   endif
043500130827
043600130820     c                   movel     cpclna        elalna
043700130827    1c                   if        cpclol<>cpclna
043800130820     c                   movel     cpclol        elalol
043900130827    1c                   endif
044000130827    1c                   if        cpclos<>cpclna and cpclos<>cpclol
044100130820     c                   movel     cpclos        elalos
044200130827    1C                   ENDIF
044300130827     c*
044400130827     c                   exsr      car_lna
044500131031     c                   eval      wttc=cpcttc
044600130827     c
044700130827     c* Carico eventuali eccezioni di linea se presenti
044800130827     C     KEYCPC        setll     AZCPe01L
044900130827     C     KEYCPC        reade     AZCPe01L
045000130827     c
045100130827    1c                   dow       not %eof(azcpe01l)
045200130827    2c                   if        cpelna>0
045300130827     c                   eval      indx=%lookup(cpelna:lna_prev)
045400130827    3c                   if        Indx=0
045500130905     c                   add       1             yy
045600130827     c                   z-add     cpelna        lna_prev(yy)
045700130827    3c                   endif
045800130827    2c                   endif
045900130827     c
046000130827    2c                   if        cpelol>0
046100130827    3c                   eval      indx=%lookup(cpelol:lna_prev)
046200130827     c                   if        Indx=0
046300130905     c                   add       1             yy
046400130827     c                   z-add     cpelol        lna_prev(yy)
046500130827    3c                   endif
046600130827    2c                   endif
046700130827
046800130827    2c                   if        cpelos>0
046900130827     c                   eval      indx=%lookup(cpelos:lna_prev)
047000130827    3c                   if        Indx=0
047100130905     c                   add       1             yy
047200130827     c                   z-add     cpelos        lna_prev(yy)
047300130827    3c                   endif
047400130827    2c                   endif
047500130827     c
047600130827     C     KEYCPC        reade     AZCPe01L
047700130827    1c                   enddo
047800130827     c
047900130827    0C                   ENDIF
048000130827     c
048100130827     c* Se non passato il CAP ci deve essere la linea di arrivo (inserimento di filiale)
048200130827    0c                   if        do0loc=*blanks and do0cap=*blanks
048300130827     c* Se passato 999 --> orari standard
048400130827    1c                   if        do0lna=0
048500130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
048600130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
048700130827     c                   leavesr
048800130827    1c                   endif
048900131003
049000131003     c                   eval      AbilF7=*on
049100130827     c
049200130904    1c                   if        do0lna=999 or do0lna=888
049300130904     c                   eval      elalna=888
049400130904     c                   eval      elalol=999
049500131010     c
049600130827   x1c                   else
049700130827     c
049800130827     c     do0lna        chain     azorg01l
049900130827    2c                   if        not %found(azorg01l)
050000130827     C                   MOVEL     MSG(14)       Do0MSG                         *MESSAGGIO
050100130827     C                   MOVEL     '1'           Do0err                         *FINE PGM
050200130827     c                   leavesr
050300130827    2c                   endif
050400130827     c                   eval      elalna=do0lna
050500131213     c
050600131213     c* Abilito tasto F9 se non presente orario soglia da filiale
050700131213      /free
050800131213         chain(n)  (do0ver:do0nar:kcap:kloc:elalna:'C':'P ') azcos03l ;
050900131213      /end-free
051000131213     c                   if        not %found(azcos03l)
051100140109     c* non abilito in interrogazione
051200140109     c                   if        OrariINT=*off
051300131213     c                   eval      AbilF9=*on
051400140109     c                   endif
051500140109     c
051600131213     c                   else
051700131213     c                   eval      wsoglia='S'
051800131213     c                   endif
051900131213     c
052000130827    1c                   endif
052100131213
052200130827     c                   exsr      car_lna
052300130827    0c                   endif
052400131213
052500130904     c* carico il record 888 per i controlli
052600130904     c                   clear                   kloc
052700130904     c                   clear                   kcap
052800130904      /free
052900130904         ktser='C'       ;
053000131216         clear ksoglia   ;
053100131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
053200131213         if %found(azcos03l)  ;
053300130904
053400130904         C88ostis=cosostis  ;
053500130904         C88ostfs=cosostfs  ;
053600130904         C88omiis=cosomiis  ;
053700130904         C88omxfs=cosomxfs  ;
053800131010         C88olrsc=cosolrsc  ;
053900131010         C88olrnc=cosolrnc  ;
054000131010         C88olrnk=cosolrnk  ;
054100131216         C88trzs =costrzs   ;
054200130904         endif ;
054300131216
054400131216         ksoglia='P ' ;
054500131216         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser:ksoglia) azcos03l   ;
054600131216         if %found(azcos03l)  ;
054700131216
054800131216         C88Postis=cosostis  ;
054900131216         C88Postfs=cosostfs  ;
055000131216         C88Pomiis=cosomiis  ;
055100131216         C88Pomxfs=cosomxfs  ;
055200131216         C88Polrsc=cosolrsc  ;
055300131216         C88Polrnc=cosolrnc  ;
055400131216         C88Polrnk=cosolrnk  ;
055500131216         C88Ptrzs =costrzs   ;
055600131216         endif ;
055700130904
055800131011         ktser='R'       ;
055900131213         chain(n)  (do0ver:do0nar:kcap:kloc:888:ktser)  azcos03l   ;
056000131213         if %found(azcos03l)  ;
056100130904
056200130904         R88ostis=cosostis  ;
056300130904         R88ostfs=cosostfs  ;
056400130904         R88omiis=cosomiis  ;
056500130904         R88omxfs=cosomxfs  ;
056600131010         R88olrsc=cosolrsc  ;
056700131010         R88olrnc=cosolrnc  ;
056800131010         R88olrnK=cosolrnK  ;
056900130904         endif ;
057000131003
057100131003       // Tolgo dalle linee previste le linee non ancora partite
057200131003         w003a='999'  ;
057300131003
057400131003         if   do0cap<>*blanks and %lookup(w003a:fil_ab) = 0  ;
057500131003         clear                   kloc       ;
057600131003         clear                   kcap       ;
057700131003
057800131003         yy =  1   ;
057900131003         dow lna_prev(yy)>0  ;
058000131003         // escludo linee 888 e 999  ;
058100131003         if lna_prev(YY) <> 888 and lna_prev(yy) <> 999  ;
058200131003
058300131007         w003a=%editc(lna_prev(YY):'X')  ;
058400131003         Indx=%lookup(w003a:fil_ab)      ;
058500131216
058600131003             if Indx=0  ;
058700131003             // se per la filiale è presente AZCOS --> la carico lo stesso
058800131003             //  altrimenti la ignoro
058900131213              chain(n)  (do0ver:do0nar:kcap:kloc:lna_prev(YY)) azcos03l   ;
059000131213              if not %found(azcos03l)  ;
059100131003                clear lna_prev(YY)  ;
059200131213              else  ;
059300131213              // Verifico se ha la soglia
059400131213         chain(n)  (do0ver:do0nar:kcap:kloc:lna_prev(YY):'C':'P ') azcos03l ;
059500131216               if  %found(azcos03l)  ;
059600131216                lna_sog(yy) ='S' ;
059700131216               endif  ;
059800131216              endif  ;
059900131216            else  ;
060000131216         chain(n)  (do0ver:do0nar:kcap:kloc:lna_prev(YY):'C':'P ') azcos03l ;
060100131216               if  %found(azcos03l)  ;
060200131216                lna_sog(yy) ='S' ;
060300131216               endif  ;
060400131216            endif  ;
060500131216
060600131003          endif  ;
060700131003
060800131003         yy=yy+1  ;
060900131003         enddo  ;
061000131003
061100131003          endif  ;
061200131003
061300130904      /end-free
061400130820     c                   ENDSR
061500130827     c*--------------------------------------------------------------
061600130827     c* Carico le 3 linee principali
061700130827     c*--------------------------------------------------------------
061800130827     c     Car_lna       BEGSR
061900130827     c                   z-add     1             yy                3 0
062000130827     c                   z-add     elalna        lna_prev(yy)
062100131213     c* imposto soglia presente per 888, 9999 e per linea
062200131213     c                   if        elalna=888 or elalna=999
062300131213     c                             or wsoglia='S'
062400131213     c                   eval      lna_sog(yy)='S'
062500131213     c                   endif
062600131213
062700130827     c                   if        elalol>0
062800130827     c                   add       1             yy
062900130827     c                   z-add     elalol        lna_prev(yy)
063000131213     c                   if        elalol=888 or elalol=999
063100131213     c                   eval      lna_sog(yy)='S'
063200131213     c                   endif
063300130827     c                   endif
063400130827     c                   if        elalos>0
063500130827     c                   add       1             yy
063600130827     c                   z-add     elalos        lna_prev(yy)
063700131213     c                   if        elalos=888 or elalos=999
063800131213     c                   eval      lna_sog(yy)='S'
063900131213     c                   endif
064000130827     c                   endif
064100131213
064200131213     c                   clear                   wsoglia
064300130827     c                   ENDSR
064400080703
064500130820      /free
064600130820
064700080206       //--------------------------------------------------------------
064800080206       //?Reperimento Dati del job (Utente/Operativi).
064900080206       //--------------------------------------------------------------
065000080206       BEGSR DatiJob;
065100080206
065200080206         in(E) §AzUte;
065300080206         if NOT %error;
065400080206           in(E) §DatiUte;
065500080206         endif;
065600080206         if %error or RSut = *blanks;
065700080206           clear TIBS34ds;
065800080206           tibs34r(tibs34ds);
065900080206           in §AzUte;
066000080206           in §DatiUte;
066100080206         endif;
066200080206
066300080206       ENDSR;
066400130820       //--------------------------------------------------------------
066500130820       //?Carica AZCOS00F dalla località o dal cap se non presente
066600130820       //--------------------------------------------------------------
066700130820       BEGSR Carica_AZCOS ;
066800130821
066900130821        clear CAPostis;
067000130821        clear CAPostfs;
067100130821        clear cAPomiis;
067200130821        clear CAPomxfs;
067300131010        clear CAPolrsc;
067400131010        clear CAPolrnc;
067500131010        clear CAPolrnk;
067600130829        clear CAPprop ;
067700130829        clear CAPrifer;
067800130829        clear CAPvisual;
067900131213        clear CAPtrzs  ;
068000130821        clear v1himm  ;
068100130828        clear v1hforz ;
068200130829        clear v1hforzlna;
068300131213        protsoglia=*off  ;
068400131016        v1cprop='   |'   ;
068500130821
068600130827         if   tlna='?'   ;
068700130827         v1himm='E'  ;
068800130827         endif           ;
068900130827
069000130821         if klna= savlna;
069100130821         endif  ;
069200130821         savlna=klna  ;
069300130820
069400130820       v1clna=klna  ;
069500130821       v1hlna=klna  ;
069600130828
069700130828       exsr Decodi_lna  ;
069800130820
069900130821       if ktser  ='R'   ;
070000131216          v1dtser='RITIRI' ;
070100130828          HI_dlna=*on  ;
070200130820       else  ;
070300131216          v1dtser='CONSEG' ;
070400130828          HI_dlna=*off ;
070500130820       endif  ;
070600130822       v1htser=ktser  ;
070700131213
070800131213       // Soglia
070900131213       if ksoglia='P'  ;
071000131213         v1csogl  ='Pom'  ;
071100131213         else  ;
071200131213         clear v1csogl    ;
071300131213         endif  ;
071400131213
071500130820
071600130827       if Wrecord= 'N'    ;
071700130820        clear  v1costis  ;
071800130820        clear  v1costfs  ;
071900131016        clear  v1comiis  ;
072000131016        clear  v1comxfs  ;
072100131010        clear  v1colrsc  ;
072200131010        clear  v1colrnc  ;
072300131010        clear  v1colrnk  ;
072400130820        clear  v1ctrzs   ;
072500130820       else  ;
072600130820        v1costis=cosostis  ;
072700130820        v1costfs=cosostfs  ;
072800131016        v1comiis=cosomiis  ;
072900131016        v1comxfs=cosomxfs  ;
073000131010        v1colrsc=cosolrsc  ;
073100131010        v1colrnc=cosolrnc  ;
073200131010        v1colrnk=cosolrnk  ;
073300131213        v1ctrzs =costrzs   ;
073400131010
073500130827        select  ;
073600130827        when wrecord='C'  ;
073700131016        v1cprop='cap|'   ;
073800130827        when wrecord='F'  ;
073900131016        v1cprop='fil|'   ;
074000130827        when wrecord='9'  ;
074100131016        v1cprop='std|'   ;
074200130827        endsl  ;
074300130821
074400130820       endif ;
074500130820
074600130829       if Immiss    =*on  ;
074700130829        v1himm='S'  ;
074800130829       else  ;
074900130821       v1cute=cosute  ;
075000130821        if cosduv>0  ;
075100130821        dataiso=%date(cosduv)  ;
075200130821        datadmy=dataiso ;
075300130821        v1dduv=%dec(datadmy) ;
075400130821        endif  ;
075500130828
075600130902        // visto che la linea on è prevista e mi si sposiziona nella lettura del file
075700130902        // non eseguo la ricerca del riferimento
075800130902         if   tlna<>'?'   ;
075900130902          exsr cerca_rif ;
076000130902          else  ;
076100130902          wrecord='N' ;
076200130902         endif  ;
076300130902        endif  ;
076400130828
076500130902       // Se ho trovato il record memorizzo i dati
076600130828        if wrecord<>'N'    ;
076700130828        CAPostis=cosostis  ;
076800130828        CAPostfs=cosostfs  ;
076900130828        cAPomiis=cosomiis  ;
077000130828        CAPomxfs=cosomxfs  ;
077100131010        CAPolrsc=cosolrsc  ;
077200131010        CAPolrnc=cosolrnc  ;
077300131010        CAPolrnk=cosolrnk  ;
077400130828        CAPrifer=wrecord   ;
077500131213        CAPtrzs =costrzs   ;
077600130829        if wrecord='9'  ;
077700131213         CAPprop='OrRif:STD'  ;
077800130829         endif ;
077900130829        if wrecord='F'  ;
078000131213         CAPprop='OrRif:FIL'  ;
078100130829         endif ;
078200130829        if wrecord='C'  ;
078300131213         CAPprop='OrRif:CAP'  ;
078400130829         endif ;
078500130829
078600130828        else  ;
078700130828        clear CAPostis;
078800130828        clear CAPostfs;
078900130828        clear CAPomiis;
079000130828        clear CAPomxfs;
079100131010        clear CAPolrsc;
079200131010        clear CAPolrnc;
079300131010        clear CAPolrnk;
079400130829        clear CAPprop ;
079500131213        clear CAPtrzs ;
079600130828        CAPrifer='N'  ;
079700130828        endif  ;
079800130821
079900130829       exsr Protcampi    ;
080000130829
080100130820       s01nrr=s01nrr + 1 ;
080200130820       write sio2s01     ;
080300130820
080400130820       ENDSR;
080500080206
080600130828       //--------------------------------------------------------------
080700130828       //?Decodifica linea di arrivo
080800130828       //--------------------------------------------------------------
080900130828       BEGSR Decodi_lna  ;
081000130828
081100130904       select  ;
081200130904       when klna=999  ;
081300131213        v1dlna='Orari Std   '  ;
081400130904       when klna=888  ;
081500131213        v1dlna='OrariContr.'  ;
081600130904        other ;
081700130828       chain klna  azorg01l  ;
081800130828       if %found(azorg01l)  ;
081900131216        if orgde5<>*blanks   ;
082000131216          v1dlna=orgde5  ;
082100131216          else  ;
082200131216          v1dlna=orgdes  ;
082300131216          endif  ;
082400131216
082500130828       else  ;
082600130828         v1dlna=*all'?'  ;
082700130828       endif  ;
082800130904       endsl  ;
082900130828       ENDSR  ;
083000080206       //--------------------------------------------------------------
083100080627       //?Gestione SFL 01
083200080206       //--------------------------------------------------------------
083300080409       BEGSR GesS01;
083400080207
083500080207         // Inizializzazione videata
083600080409         if  $InzS01 = *on;
083700080409            exsr InzS01;
083800080703         // Posizionamento cursore
083900100507         // C01csr = 1;
084000080703           s01nrr = 1;
084100080703           sfldsp_n=*off;
084200080409            $InzS01  = *off;
084300080207         endif;
084400080207
084500080207
084600080207         // Emissione Testata e Piede con tasti funzionali abilitati
084700080207         if  errGenerico = *off;
084800131003         select   ;
084900131003         when §15ita<>'I'  ;
085000130822           sfldsp_n=*on      ;
085100130822           write  sio2D02;
085200131003
085300131003         when lna_prev(1)=0 ;
085400131003           sfldsp_n=*on      ;
085500131003           write  sio2D03;
085600131003         other   ;
085700131003           write  sio2Z01;
085800131003           endsl  ;
085900080207         endif;
086000080703
086100080703         // Posizionamento cursore
086200080703         if  C01csr  > *zero;
086300080703           C01rcd = C01csr;
086400080703         endif  ;
086500080207
086600080207         // Emissione videata
086700130820         exfmt  sio2C01;
086800080410
086900080207         reset errMessage;
087000080207         reset errGenerico;
087100080702         clear Vscmsg;
087200130820         clear Errostis  ;
087300130820         clear Errostfs  ;
087400130820         clear Erromiis  ;
087500130820         clear Erromxfs  ;
087600131010         clear Errolrsc  ;
087700131010         clear Errolrnc  ;
087800131010         clear Errolrnk  ;
087900130820         clear ErrTrzs   ;
088000130821         clear Errlna    ;
088100130828         clear BL_ostis  ;
088200130828         clear BL_ostfs  ;
088300130828         clear BL_omiis  ;
088400130828         clear BL_omxfs  ;
088500131010         clear BL_olrsc  ;
088600131010         clear BL_olrnc  ;
088700131010         clear BL_olrnk  ;
088800131213         clear BL_trzs   ;
088900080207
089000080523 1       SELECT;
089100080207
089200080207         // - F3=Fine
089300080523 1         WHEN  dsp_aid = c_F03;
089400080409            $Fine = *on;
089500130820            Do0f03='1'   ;
089600130828
089700130829         // - F5=Ripristina orari di riferimento (disabilitato)
089800130829 1         // WHEN  dsp_aid = c_F05;
089900130829           // chain    c01csr sio2s01  ;
090000130829           // if %found     ;
090100130829           //  if v1costis<>CAPostis  ;
090200130829           //  v1costis=CAPostis  ;
090300130829           //  BL_ostis=*on  ;
090400130829           // endif  ;
090500130829           // if v1costfs<>CAPostfs  ;
090600130829           //  v1costfs=CAPostfs  ;
090700130829           //  BL_ostfs=*on  ;
090800130829           // endif  ;
090900130829           // if v1comiis<>CAPomiis  ;
091000130829           //  v1comiis=CAPomiis  ;
091100130829           //  BL_omiis=*on  ;
091200130829           // endif  ;
091300130829           // if v1comxfs<>CAPomxfs  ;
091400130829           //  v1comxfs=CAPomxfs  ;
091500130829           //  BL_omxfs=*on  ;
091600130829           // endif  ;
091700130829           // if v1ctrzs <>CAPtrzs   ;
091800130829           //  v1ctrzs =CAPtrzs   ;
091900130829           //  BL_trzs =*on  ;
092000130829           // endif  ;
092100130829           //  else  ;
092200130829            // cursore non ben posizionato : errore
092300130829           //  s01nrr=1  ;
092400130829           // chain    s01nrr sio2s01  ;
092500130829           // errlna  =*on ;
092600130829           // vscmsg = Msg(07);
092700130829           // endif         ;
092800130829
092900130829           // exsr aggiosfl      ;
093000130829
093100130829         // - F7=Visualizza orari di riferimento per tutte le righe
093200131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F07;
093300130829            yy=1           ;
093400130829            s01nrr=1  ;
093500130829            chain s01nrr    sio2s01    ;
093600130829
093700130829 0          dow %found    ;
093800130829
093900130829            if capvisual<>'S'  ;
094000130829            capvisual='S'  ;
094100130829            else  ;
094200130829            capvisual=' '  ;
094300130829            endif  ;
094400130829
094500130829            exsr aggiosfl      ;
094600130829
094700130829            s01nrr=s01nrr+1     ;
094800130829
094900130829            chain s01nrr    sio2s01    ;
095000130829           enddo  ;
095100080207
095200131213         // - F09=Abilita riga soglia
095300131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F09;
095400131213            yy=1           ;
095500131213            s01nrr=1  ;
095600131213            chain s01nrr    sio2s01    ;
095700131213
095800131213 0          dow %found    ;
095900131213
096000131216            if v1csogl ='Pom'    ;
096100131213            exsr protcampi  ;
096200131213
096300131213            exsr aggiosfl      ;
096400131216            AbilF9=*off        ;
096500131213            endif  ;
096600131213
096700131213            s01nrr=s01nrr+1     ;
096800131213
096900131213            chain s01nrr    sio2s01    ;
097000131213           enddo  ;
097100080207         // - F12=Ritorno
097200080523 1         WHEN  dsp_aid = c_F12;
097300080702            $Fine = *on;
097400130820            Do0f12='1'   ;
097500080414
097600131003         // - F16=Annullamento  (disabilito)
097700130829 1         WHEN  dsp_aid = c_F16;
097800130829         //  exsr annull_rec    ;
097900130829         //  $Fine = *on;
098000130829
098100080530 x1      // Invio / F6
098200080207           OTHER;
098300081006
098400081006        // Non effettuo controlli in interrogazione
098500131003 1           if orariInt=*off and sfldsp_n=*off ;
098600081006               exsr ContrS01 ;
098700081006 2             if  errGenerico = *on;
098800081006                 leavesr;
098900081006 2             endif;
099000081006 2           endif;
099100080530
099200081023         // F6=conferma Aggiornamento  F10=anche stampa
099300081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
099400131003             if sfldsp_n=*off  ;
099500131003              exsr   ConfAggio ;
099600131003             endif  ;
099700081023
099800080704           $Fine = *on;
099900080530           endif  ;
100000080207
100100080523 1       ENDSL;
100200080207
100300080207       ENDSR;
100400080207
100500080526       //--------------------------------------------------------------
100600080702       //?controlli SFL01
100700080409       //--------------------------------------------------------------
100800080409       BEGSR ContrS01;
100900130821
101000130821       yy=1  ;
1011001308291      dow yy<=12;
101200130821       if lna_es(yy)='R'  ;
101300130821       clear lna_es(YY)  ;
101400130821       endif  ;
101500130821
101600130821       yy=yy+1  ;
1017001308291      enddo    ;
101800130821
101900081126       yy=1           ;
102000080703       s01nrr=1  ;
102100130820       chain s01nrr    sio2s01    ;
102200080703
102300081006 0     dow %found    ;
102400130820
102500130821       clear  v1dlna  ;
102600130821
102700130828       klna=v1clna ;
102800130828       exsr decodi_lna  ;
102900130828 1     if v1dlna=*all'?'        ;
103000130821          errlna  =*on ;
103100130821          vscmsg = Msg(08);
103200130821          EXSR AggioSFL   ;
103300130821          leavesr;
103400130821 1     endif  ;
103500130821
103600130828       // La linea deve essere una di quelle previste
103700130829       //  se non prevista dico che la annullerò
103800130828         Indx=%lookup(v1clna:lna_prev)  ;
1039001308291        if Indx=0  and v1hforzlna=' '  ;
104000130828
104100130821          errlna  =*on ;
104200130821          vscmsg = Msg(04);
104300130829          v1hforzlna='S'  ;
104400130821
104500130821          EXSR AggioSFL   ;
104600130821          leavesr;
104700130821 1     endif  ;
104800130821
104900130822 1     if v1hlna<>v1clna ;
105000130821
105100130821       // Se la riga è RITIRO  controllo il linea doppia e la memorizzo
105200130822  2     if v1htser='R'  ;
105300130821
105400130828         if lna_es(indx)<>' '  ;
105500130821          errlna  =*on ;
105600130821          vscmsg = Msg(05);
105700130821          EXSR AggioSFL   ;
105800130821          leavesr;
105900130821  x3      else  ;
106000130828          lna_es(indx)='R'   ;
106100130821   3      endif  ;
106200130821
106300130821        savlna=v1clna  ;
106400130821
106500130821  x2   else  ;
106600130821   3   if v1clna  <> savlna  ;
106700130821          errlna  =*on ;
106800130821          vscmsg = Msg(13);
106900130821          EXSR AggioSFL   ;
107000130821          leavesr;
107100130821  3    endif  ;
107200130821  2    endif  ;
107300130822  1    endif  ;
107400130821
107500130822       // O R A R I
107600130829       //  li controllo solo per le linee valide
107700130829
107800130829  1a   if indx>0 ;
107900131216
108000131216       // controllo Soglia solo se l'ho abilitata ;
108100131216  1b   if v1csogl= *blanks   or AbilF9=*off  ;
108200131216
108300130829       // Se per la linea di arrivo manca il record di filiale --> non posso inserire
108400130829       if do0cap<>*blanks ;
108500130829       clear kloc  ;
108600130829       clear kcap  ;
108700130829       ktser=v1htser  ;
108800131213           chain(n)  (do0ver:do0nar:kcap:kloc:v1clna:ktser)  azcos03l   ;
108900131213           if not %found(azcos03l)  ;
109000130829          errlna  =*on ;
109100130829          vscmsg = Msg(15);
109200130829          %subst(vscmsg:40:3)=%editc(v1clna:'X')  ;
109300130829          EXSR AggioSFL   ;
109400130829          leavesr;
109500130829           endif  ;
109600130829       endif  ;
109700130829
109800130829
109900130904       // Imposto gli orari in base al servizio
110000130904       if v1clna<>888     ;
110100130904       if v1htser='C'   ;
110200131216       if v1csogl='Pom'  ;
110300131216       W88ostis=C88Postis ;
110400131216       W88ostfs=C88Postfs ;
110500131216       W88omiis=C88Pomiis ;
110600131216       W88omxfs=C88Pomxfs ;
110700131216       W88olrsc=C88Polrsc ;
110800131216       W88olrNc=C88Polrnc ;
110900131216       W88olrNk=C88Polrnk ;
111000131216       W88trzs =C88Ptrzs  ;
111100131216       else  ;
111200130904       W88ostis=C88ostis ;
111300130904       W88ostfs=C88ostfs ;
111400130904       W88omiis=C88omiis ;
111500130904       W88omxfs=C88omxfs ;
111600131010       W88olrsc=C88olrsc ;
111700131010       W88olrNc=C88olrnc ;
111800131010       W88olrNk=C88olrnk ;
111900131216       W88trzs =C88trzs  ;
112000131216       endif  ;
112100131216
112200130904       else  ;
112300130904       W88ostis=r88ostis ;
112400130904       W88ostfs=r88ostfs ;
112500130904       W88omiis=r88omiis ;
112600130904       W88omxfs=r88omxfs ;
112700131010       W88olrsc=r88olrsc ;
112800131010       W88olrnc=r88olrnc ;
112900131010       W88olrnk=r88olrnk ;
113000130904       endif  ;
113100131010       endif  ;
113200131010
113300131011       if v1clna= 888 or w88ostfs=0000  ;
113400130904       W88ostis=0000 ;
113500130904       W88ostfs=2400 ;
113600130904       W88omiis=0000 ;
113700130904       W88omxfs=2400 ;
113800131010       W88olrsc=2400 ;
113900131010       W88olrnc=2400 ;
114000131010       W88olrnk=2400 ;
114100131216       W88trzs =0000 ;
114200130904       endif  ;
114300130904
114400131216
114500131011       // Orario inizio
114600130829 1     if  v1costis=0  ;
114700130820          errostis=*on ;
114800130820          vscmsg = Msg(02);
114900081003          EXSR AggioSFL   ;
115000081003          leavesr;
115100130820 1     endif  ;
115200130821
115300130821       wora=v1costis ;
115400130821       exsr contr_ora ;
115500130829 1     if vscmsg<>*blanks ;
115600130821          errostis=*on ;
115700130821          EXSR AggioSFL   ;
115800130821          leavesr;
115900130821 1     endif  ;
116000130904
116100130904       // no minore del limite previsto
116200130904       if v1costis<w88ostis  ;
116300130904          errostis=*on ;
116400130904          vscmsg = Msg(16);
116500130904          %subst(vscmsg:37:5)=%editw(w88ostis: '0 :  ')    ;
116600130904          EXSR AggioSFL   ;
116700130904          leavesr;
116800130904 1     endif  ;
116900131031       // Orario inizio consegne no maggiore tempo consegna per tipo servizio
117000140110       // non controllo per orari del pomeriggio
117100140110       if v1htser='C' and do0cap<>*blanks
117200140110          and v1csogl=*blanks              ;
117300140110
117400131031          exsr car_cpt;
117500131031          if v1costis>wcpt0in4;
117600131031             errostis=*on ;
117700131031             vscmsg = Msg(21);
117800131031             vscmsg=%trim(vscmsg)+' ' + %trim(wdtsp);
117900131031             EXSR AggioSFL   ;
118000131031             leavesr;
118100131031          endif;
118200131031       endif;
118300130821
118400130821
118500131011       // Orario fine
118600130820 1     if  v1costfs=0     ;
118700130820          errostfs=*on ;
118800130820          vscmsg = Msg(02);
118900130820          EXSR AggioSFL   ;
119000130820          leavesr;
119100130820 1     endif  ;
119200130821
119300130821       wora=v1costfs ;
119400130821       exsr contr_ora ;
119500130829 1     if vscmsg<>*blanks ;
119600130821          errostfs=*on ;
119700130821          EXSR AggioSFL   ;
119800130821          leavesr;
119900130821 1     endif  ;
120000130904       // no maggiore del limite previsto
120100130904       if v1costfs>w88ostfs  ;
120200130904          errostfs=*on ;
120300130904          vscmsg = Msg(17);
120400130904          %subst(vscmsg:37:5)=%editw(w88ostfs: '0 :  ')    ;
120500130904          EXSR AggioSFL   ;
120600130904          leavesr;
120700130904 1     endif  ;
120800130820
120900130820 1     if  v1costfs<v1costis  ;
121000130820          errostfs=*on ;
121100130820          vscmsg = Msg(03);
121200130820          EXSR AggioSFL   ;
121300130820          leavesr;
121400130820 1     endif  ;
121500131011
121600131016       if v1htser='C'  ;
121700131216       // Tempo oraganizzazione --> solo per CONSEGNE
121800131216 1     if  v1ctrzs<w88trzs   or v1ctrzs>1200 ;
121900131216          Errtrzs =*on ;
122000131216          vscmsg = Msg(22);
122100131216          %subst(vscmsg:37:5)=%editw(w88trzs:'0 :  ')  ;
122200131216          EXSR AggioSFL   ;
122300131216          leavesr;
122400131216 1     endif  ;
122500131216
122600131016       // Orario MINIMO  solo per CONSEGNE
122700131016 1     if  v1comiis=0  ;
122800131016          erromiis=*on ;
122900131016          vscmsg = Msg(02);
123000131016          EXSR AggioSFL   ;
123100131016          leavesr;
123200131016 1     endif  ;
123300131016
123400131016       wora=v1comiis ;
123500131016       exsr contr_ora ;
123600131016 1     if vscmsg<>*blanks ;
123700131016          erromiis=*on ;
123800131016          EXSR AggioSFL   ;
123900131016          leavesr;
124000131016 1     endif  ;
124100131016
124200131016       // no minore del limite previsto
124300131016       if v1comiis<w88omiis  ;
124400131016          erromiis=*on ;
124500131016          vscmsg = Msg(16);
124600131016          %subst(vscmsg:37:5)=%editw(w88omiis: '0 :  ')    ;
124700131016          EXSR AggioSFL   ;
124800131016          leavesr;
124900131016 1     endif  ;
125000131016
125100131016       // no maggiore dell'orario di inizio
125200131016       if v1comiis > v1costis  ;
125300131016          erromiis=*on ;
125400131016          vscmsg = Msg(11);
125500131016          EXSR AggioSFL   ;
125600131016          leavesr;
125700131016 1     endif  ;
125800131016
125900131016       // Orario MASSIMO
126000131016 1     if  v1comxfs=0     ;
126100131016          erromxfs=*on ;
126200131016          vscmsg = Msg(02);
126300131016          EXSR AggioSFL   ;
126400131016          leavesr;
126500131016 1     endif  ;
126600131016
126700131016       wora=v1comxfs ;
126800131016       exsr contr_ora ;
126900131016 1     if vscmsg<>*blanks ;
127000131016          erromxfs=*on ;
127100131016          EXSR AggioSFL   ;
127200131016          leavesr;
127300131016 1     endif  ;
127400131016       // no maggiore del limite previsto
127500131016       if v1comxfs>w88omxfs  ;
127600131016          erromxfs=*on ;
127700131016          vscmsg = Msg(17);
127800131016          %subst(vscmsg:37:5)=%editw(w88omxfs: '0 :  ')    ;
127900131016          EXSR AggioSFL   ;
128000131016          leavesr;
128100131016 1     endif  ;
128200131016
128300131016 1     if  v1comxfs<v1comiis  ;
128400131016          erromxfs=*on ;
128500131016          vscmsg = Msg(03);
128600131016          EXSR AggioSFL   ;
128700131016          leavesr;
128800131016 1     endif  ;
128900131016       // no minore   dell'orario di fine
129000131016       if v1comxfs < v1costfs  ;
129100131016          erromxfs=*on ;
129200131016          vscmsg = Msg(12);
129300131016          EXSR AggioSFL   ;
129400131016          leavesr;
129500131016 1     endif  ;
129600131016 1     endif  ;
129700131016
129800131011       if v1htser='R'  ;
129900131011
130000131011       // Orari limite ritiri: COMMISSIONATI
130100131011 1     if  v1colrsc=0     ;
130200131011          erroLRSC=*on ;
130300131011          vscmsg = Msg(02);
130400131011          EXSR AggioSFL   ;
130500131011          leavesr;
130600131011 1     endif  ;
130700131011
130800131011       wora=v1coLRSC ;
130900131011       exsr contr_ora ;
131000131011 1     if vscmsg<>*blanks ;
131100131011          erroLRSC=*on ;
131200131011          EXSR AggioSFL   ;
131300131011          leavesr;
131400131011 1     endif  ;
131500131011       // no maggiore del limite previsto
131600131011       if v1coLRSC>w88oLRSC  ;
131700131011          erroLRSC=*on ;
131800131011          vscmsg = Msg(17);
131900131011          %subst(vscmsg:37:5)=%editw(w88oLRSC: '0 :  ')    ;
132000131011          EXSR AggioSFL   ;
132100131011          leavesr;
132200131011 1     endif  ;
132300131011       // Orari limite ritiri: NON COMMISSIONATI
132400131011 1     if  v1colrNc=0     ;
132500131011          erroLRNC=*on ;
132600131011          vscmsg = Msg(02);
132700131011          EXSR AggioSFL   ;
132800131011          leavesr;
132900131011 1     endif  ;
133000131011
133100131011       wora=v1coLRNC ;
133200131011       exsr contr_ora ;
133300131011 1     if vscmsg<>*blanks ;
133400131011          erroLRNC=*on ;
133500131011          EXSR AggioSFL   ;
133600131011          leavesr;
133700131011 1     endif  ;
133800131011       // no maggiore del limite previsto
133900131011       if v1coLRNC>w88oLRNC  ;
134000131011          erroLRNC=*on ;
134100131011          vscmsg = Msg(17);
134200131011          %subst(vscmsg:37:5)=%editw(w88oLRNC: '0 :  ')    ;
134300131011          EXSR AggioSFL   ;
134400131011          leavesr;
134500131011 1     endif  ;
134600131011       // Orari limite ritiri: NON COMMISSIONATI CODIFICATI
134700131011 1     if  v1colrNK=0     ;
134800131011          erroLRNK=*on ;
134900131011          vscmsg = Msg(02);
135000131011          EXSR AggioSFL   ;
135100131011          leavesr;
135200131011 1     endif  ;
135300131011
135400131011       wora=v1coLRNK ;
135500131011       exsr contr_ora ;
135600131011 1     if vscmsg<>*blanks ;
135700131011          erroLRNK=*on ;
135800131011          EXSR AggioSFL   ;
135900131011          leavesr;
136000131011 1     endif  ;
136100131011       // no maggiore del limite previsto
136200131011       if v1coLRNK>w88oLRNK  ;
136300131011          erroLRNK=*on ;
136400131011          vscmsg = Msg(17);
136500131011          %subst(vscmsg:37:5)=%editw(w88oLRNK: '0 :  ')    ;
136600131011          EXSR AggioSFL   ;
136700131011          leavesr;
136800131011 1     endif  ;
136900131011
137000131011       // secondo orario >= al primo
137100131011       if v1colrnc<v1colrsc  ;
137200131011          erroLRNc=*on ;
137300131011          vscmsg = Msg(18);
137400131011          EXSR AggioSFL   ;
137500131011          leavesr;
137600131011       endif  ;
137700131011       // terzo orario >= al secondo
137800131011       if v1colrnk<v1colrnc  ;
137900131011          erroLRNk=*on ;
138000131011          vscmsg = Msg(19);
138100131011          EXSR AggioSFL   ;
138200131011          leavesr;
138300131011       endif  ;
138400131011
138500131011       endif  ;
138600130820
138700131216 1b    endif  ;
138800131216 1a    endif  ;
138900130821
139000130821       EXSR AggioSFL   ;
139100080703
139200080703       s01nrr=1+s01nrr  ;
139300130820       chain s01nrr    sio2s01    ;
139400081006 0     enddo         ;
139500081006
139600080409       ENDSR;
139700080409
139800130829       //--------------------------------------------------------------
139900130829       //?Annullamento record
140000130829       //--------------------------------------------------------------
140100130829       //BEGSR Annull_rec  ;
140200130829
140300130829       // Elaboro la schiera delle linee e deleto
140400130829       //yy=1   ;
140500130829       //dow lna_prev(YY)>0     ;
140600130829
140700130829       //klna=lna_prev(yy)     ;
140800130829       //ktser='R'  ;
140900131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
141000130829       //ktser='C'  ;
141100131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
141200130829
141300130829       //yy=yy+1  ;
141400130829       //enddo   ;
141500130829
141600130829       //ENDSR;
141700080703       //--------------------------------------------------------------
141800080703       //?Aggiornamento sfl
141900080703       //--------------------------------------------------------------
142000080703       BEGSR AGGIOSFL  ;
142100080703       if vscmsg<>*blanks   ;
142200080703       errMessage  = *on;
142300080703       errGenerico = *on;
142400080704       c01csr=s01nrr    ;
142500080703       endif    ;
142600080703
142700080703       exsr ProtCampi   ;
142800080703
142900130820       update  sio2s01  ;
143000080703       ENDSR   ;
143100130821       //--------------------------------------------------------------
143200130821       //?Controllo campo ORA
143300130821       //--------------------------------------------------------------
143400130821       BEGSR Contr_ORA ;
143500130821       if whh < 0  or whh > 24  ;
143600130821          vscmsg = Msg(09);
143700130821       endif  ;
143800130821       if wmm < 0  or wmm > 59  ;
143900130821          vscmsg = Msg(09);
144000130821       endif  ;
144100131011
144200131011       // Minuti arrotondati ai 15 minuti
144300131011       if wmm<>00 and wmm<>15 and wmm<>30 and wmm<>45  ;
144400131011          vscmsg = Msg(20);
144500131011       endif  ;
144600130821
144700130821       ENDSR ;
144800080207       //--------------------------------------------------------------
144900080627       //?Inizializzazione SFL01
145000080207       //--------------------------------------------------------------
145100080409       BEGSR InzS01;
145200080207
145300080207       // Pulizia subfile
145400080207         SflDsp_N    = *on;
145500080207         SflDspCtl_N = *on;
145600130820         write  sio2C01;
145700080207         SflDspCtl_N = *off;
145800080207         SflEnd      = *off;
145900080530
146000080530         clear C01rcd;
146100100507         C01csr=1;
146200080627         S01nrr=0 ;
146300080702         clear Vscmsg;
146400080207         errMessage  = *off;
146500080207         errGenerico = *off;
146600130820         immiss    =*off ;
146700130821         clear savlna ;
146800130821         clear yy    ;
146900130821         clear lna_es;
147000130821         clear Wrecord ;
147100130821         clear v1dduv  ;
147200130821         clear v1cute  ;
147300130821
147400130822         // Se la nazione no è italia --> non carico nulla e avviso che non esistono
147500130822         //  orari
147600130822
147700130827 1       if §15ita= 'I'  ;
147800130820
147900130821         // 1)
148000130821         // Leggo e carico eventuali linee non più previste dal cap/località
148100130821
148200130828 1a    if   do0cap<>*blanks   ;
148300130828
148400130829       Protlna=*off ;
148500131213       setll  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
148600131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
148700131213 2      dow  not %eof(azcos03l)  ;
148800130821          klna=coslna ;
148900130827          Indx=%lookup(coslna:lna_prev)  ;
149000130827 3      if indx=0 ;
149100130821          tlna='?'   ;
149200130829           ktser=costser   ;
149300130821          exsr   Leggi_cos ;
1494001308273       endif  ;
149500130821
149600131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
1497001308272      enddo  ;
1498001308281a     endif  ;
149900130821
150000130821         // 2)
150100130821         // Carico i record  già presenti
150200130829         Protlna=*on  ;
150300130902         clear tlna   ;
150400130820
150500130827           YY=1  ;
150600130827 2         dow lna_prev(yy)>0  ;
150700130827
150800130827           klna=lna_prev(YY) ;
150900131213           chain  (do0ver:do0nar:do0cap:do0loc:klna)  azcos03l   ;
151000131213 3         if %found(azcos03l)  ;
151100130827           lna_es(YY)='S'  ;
151200130830           endif  ;
151300131010
151400131213           if klna=999  or klna=888 ;
151500131213            indlna9988=*on  ;
151600131010            else  ;
151700131213            indlna9988=*off ;
151800131010            endif  ;
151900130830
152000131213           ksoglia=*blanks ;
152100131213           ktser='R'  ;
152200130821           exsr Leggi_cos  ;
152300130821           ktser='C'  ;
152400130821           exsr Leggi_cos  ;
152500131213           // Per consegne vedo se presente anche soglia "P"
152600131213
152700131216           if lna_sog(yy)='S'  or abilF9=*on  ;
152800131213           ksoglia='P'  ;
152900131213           exsr Leggi_cos  ;
153000131213           endif  ;
153100130827
153200130827           yy=yy+1  ;
153300130827 2         enddo    ;
153400130821
1535001308271        endif  ;
153600130827
153700080702         ENDSR    ;
153800130821       //--------------------------------------------------------------
153900130821       //?Lettura record di AZCOS
154000130821       //--------------------------------------------------------------
154100130821           BEGSR Leggi_cos  ;
154200131213           chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia) azcos03l  ;
154300131213           if %found(azcos03l)  ;
154400130821           Immiss=*off ;
154500130821           wrecord='S'  ;
154600130828           else  ;
154700130821
154800130821           Immiss=*on  ;
154900130828           // Cerca il record di riferimento
155000130828           exsr cerca_rif  ;
155100130828
155200130828           endif  ;
155300130821
155400130821           exsr      Carica_AZCOS ;
155500131213
155600130821           ENDSR  ;
155700130828       //--------------------------------------------------------------
155800130828       //?Cerco record di riferimento per vedere se dati uguali o diversi
155900130828       //--------------------------------------------------------------
156000130828           BEGSR   Cerca_rif  ;
156100130828           wrecord='N'  ;
156200130828
156300130904           if klna<>999  and klna<>888 ;
156400130828
156500130829           if do0cap<>*blanks    ;
156600130829
156700130828 1         if do0loc<>*blanks   ;
156800130828            clear kloc  ;
156900131213           chain(n)  (do0ver:do0nar:do0cap:kloc:klna:ktser:ksoglia) azcos03l ;
157000131213           if %found(azcos03l)  ;
157100130828           wrecord='C'  ;
157200130828           endif  ;
157300130828 1         endif  ;
157400130828
157500130828           // Se non presente a livello di CAP provo a livello di filiale
157600130828 1         if wrecord='N'  ;
157700130828           clear kcap  ;
157800131213           chain(n)  (do0ver:do0nar:kcap:kloc:klna:ktser:ksoglia)  azcos03l   ;
157900131213           if %found(azcos03l)  ;
158000130828           wrecord='F'  ;
158100130828           endif  ;
158200130828 1         endif  ;
158300130829
158400130829 1         endif  ;
158500130828
158600131213           // standard aziendale
158700130828 1         if wrecord='N'  ;
158800130828           clear kcap  ;
158900131213           chain(n)  (do0ver:do0nar:kcap:kloc:lna999:ktser:ksoglia) azcos03l ;
159000131213           if %found(azcos03l)  ;
159100130828           wrecord='9'  ;
159200130828           endif  ;
159300130828 1         endif  ;
159400130828
159500130828 1         endif  ;
159600130828           ENDSR  ;
159700080702       //--------------------------------------------------------------
159800080702       //?Gestione protezione campi del sfl
159900080702       //--------------------------------------------------------------
160000080702          BEGSR   ProtCampi   ;
160100130820           Immiss  =*off ;
160200130829           vis_rifer=*off      ;
160300130829           BL_ostis=*off ;
160400130829           BL_ostfs=*off ;
160500130829           BL_omiis=*off ;
160600130829           BL_omxfs=*off ;
160700131010           BL_olrsc=*off ;
160800131010           BL_olrnc=*off ;
160900131010           BL_olrnK=*off ;
161000131213           BL_trzs =*off ;
161100130829           Protriga=*off ;
161200130829           Protlna=*on  ;
161300131216           Protsoglia=*off   ;
161400131213           IndLna9988=*off ;
161500080703
161600081007           // Se riga di categoria accendo solo protezione e sottolineatura
161700130821          if v1himm='S'    ;
161800130820           Immiss  =*on    ;
161900130820          endif            ;
162000130829
162100130829          // Protezine per linea di arrivo non prevista
162200130829          // Sempre protezione per interrogazione
162300130829          if v1himm='E'    or orariint=*on  ;
162400130829           Protriga=*on  ;
162500130821          endif            ;
162600130829          if v1himm='E' ;
162700130829           Protlna=*off ;
162800130829          endif            ;
162900130829
163000130828          if v1htser='R'  ;
163100130828          HI_dlna=*on  ;
163200130828          else  ;
163300130828          HI_dlna=*off ;
163400130828          endif  ;
163500131003
163600131010          // Se tipo servizio  ritiro non faccio vedere il tempo organizz
163700131003          if v1htser='R'  ;
163800131003          prot_tempo=*on  ;
163900131003          else ;
164000131003          prot_tempo=*off ;
164100131003          endif  ;
164200130821
164300130829          if CAPvisual= 'S'  ;
164400130829          vis_rifer=*on  ;
164500130829          endif  ;
164600131213
164700131213       // Se il record soglia c'è disabilito F9
164800131216       if v1csogl='Pom' ;
164900131216       if CAPrifer='S' or dsp_aid = c_F09 or AbilF9=*off ;
165000131213        abilF9=*off ;
165100131213        protsoglia=*off   ;
165200131213       else   ;
165300131213        abilF9=*on  ;
165400131213        protsoglia=*on    ;
165500131213       endif   ;
165600131213       endif   ;
165700131010
165800131213          //if v1clna=999  ;
165900131213          // indlna999=*on  ;
166000131213          //endif  ;
166100130829
166200130829          // Evidenzio i campi diversi dal riferimento
166300131003          // solo se sono a livello di CAP/località
166400131003          // non per filiale o standard
166500131003          if do0cap<>*blanks    ;
166600131003
166700130829          if v1costis<>capostis ;
166800130829           BL_ostis=*on ;
166900130829          endif  ;
167000130829          if v1costfs<>capostfs ;
167100130829           BL_ostfs=*on ;
167200130829          endif  ;
167300131016          if v1comiis<>capomiis ;
167400131016           BL_omiis=*on ;
167500131016          endif  ;
167600131016          if v1comxfs<>capomxfs ;
167700131016           BL_omxfs=*on ;
167800131016          endif  ;
167900131010          if v1colrsc<>capolrsc ;
168000131010           BL_olrsc=*on ;
168100131010          endif  ;
168200131010          if v1colrnc<>capolrnc ;
168300131010           BL_olrnc=*on ;
168400131010          endif  ;
168500131010          if v1colrnk<>capolrnk ;
168600131010           BL_olrnk=*on ;
168700131010          endif  ;
168800131213          if v1ctrzs <>captrzs  ;
168900131213           BL_trzs =*on ;
169000131213          endif  ;
169100131010
169200131010
169300131003          endif  ;
169400130829
169500080702          ENDSR    ;
169600080702       //--------------------------------------------------------------
169700080702       //?conferma aggiornamento
169800080702       //--------------------------------------------------------------
169900080702        BEGSR ConfAGGIO              ;
170000081003
170100080704       s01nrr=1  ;
170200130820       chain s01nrr    sio2s01    ;
170300080704
170400080704 1     dow %found    ;
170500130821
170600130821 2     if  v1clna>0  ;
170700130821
170800140116       // Elaboro record soglia solo se
170900140116 2a    if v1csogl=*blanks or (abilF9=*off or caprifer='S')   ;
171000140116       Klna=v1hlna  ;
171100130828       ktser=v1htser  ;
171200131216       ksoglia=%subst(v1csogl:1:1)  ;
171300130829
171400131216       // Se la linea on è prevista--> la deleto
171500130829 3     if v1himm='E'   ;
171600131216         delete (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
171700130829 x3    else    ;
171800130829
171900131216         chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
172000130829       // se il record  è diventato uguale al record di riferimento
172100130829       //  lo deleto ma non per filiale
172200130829  4       if  CAPostis= v1costis   and
172300130828              CAPostfs= v1costfs   and
172400131016              CAPomiis= v1comiis   and
172500131016              CAPomxfs= v1comxfs   and
172600131010              CAPolrsc= v1colrsc   and
172700131010              CAPolrnc= v1colrnc   and
172800131010              CAPolrnk= v1colrnk   and
172900131216              CAPtrzs = v1ctrzs    and
173000130829              do0cap<>*blanks           ;
173100131213  5           if %found(azcos03l)  ;
173200130821               delete azcos000   ;
173300130829  5           endif  ;
173400130829  x4        else  ;
173500131213  5           if not %found(azcos03l)  ;
173600130821              clear azcos000  ;
173700130821              cosver=do0ver ;
173800130821              cosnar=do0nar ;
173900130821              coscap=do0cap ;
174000130821              cosloc=do0loc ;
174100130829  5           endif  ;
174200130821              cosostis= v1costis      ;
174300130821              cosostfs= v1costfs      ;
174400131016              cosomiis= v1comiis      ;
174500131016              cosomxfs= v1comxfs      ;
174600131010              cosolrsc= v1colrsc   ;
174700131010              cosolrnc= v1colrnc   ;
174800131010              cosolrnk= v1colrnk   ;
174900130821              costrzs = v1ctrzs         ;
175000130821              cosute  =knmus       ;
175100130821              cosduv=%dec(datasys)  ;
175200130821              costser=ktser  ;
175300130821              coslna=v1clna  ;
175400131216              cosserv=ksoglia ;
175500131213  5           if %found(azcos03l)  ;
175600130821               update azcos000   ;
175700130821               else  ;
175800130821               write  azcos000   ;
175900130829  5           endif  ;
176000130829  4        endif  ;
176100130829  3        endif  ;
176200140116 2a      endif  ;
176300140116 2       endif  ;
176400080704
176500080704        s01nrr=s01nrr+1 ;
176600130820        chain s01nrr    sio2s01    ;
176700080704  1     enddo           ;
176800080704
176900080702        ENDSR;
177000131031       //--------------------------------------------------------------
177100131031       //?Caricamento orario più basso del tipo tempo consegna
177200131031       //--------------------------------------------------------------
177300131031        BEGSR car_cpt                ;
177400131031        clear wcpt0in;
177500131031        clear wcpt0in4;
177600131031        clear wdtsp  ;
177700131031        clear wtsp   ;
177800131031        setll (do0ver:wttc) azcpt01l   ;
177900131031        reade (do0ver:wttc) azcpt01l   ;
178000131031        dow not %eof(azcpt01l);
178100131031           if cpt0in<wcpt0in or wcpt0in=0;
178200131031              wcpt0in=cpt0in;
178300131031              wtsp   =cpttsp;
178400131031           endif;
178500131031           reade (do0ver:wttc) azcpt01l   ;
178600131031        enddo;
178700131031        // decodifico il tipo servizio memorizzato
178800131031        clear ds5e;
178900131031        chain (1:'5E':wtsp) tabel00f;
179000131031        if %found(tabel00f);
179100131031           ds5e=tbluni;
179200131031        endif;
179300131031        wdtsp=§5ed08;
179400131031        wcpt0in4=wcpt0in*100;
179500131031        endsr;
179600080702
179700080206       //--------------------------------------------------------------
179800080206       //?Operazioni finali.
179900080206       //--------------------------------------------------------------
180000080206       BEGSR RoutEnd;
180100080627
180200080704         // Chiusura pgm   ;
180300131003         // clear tibs02ds  ;
180400131003         // t02tla='C'      ;
180500131003         // TNTBE_RicercaControllo  (kpjba : tibs02ds);
180600080704
180700080206         *inLR = *on;
180800080704
180900080206         return;
181000080206
181100080206       ENDSR;
181200080206
181300131003
181400080206      /end-free
181500131003     C**************************************************************************
181600131003     C     *inzsr        BEGSR
181700131003      /free
181800131003        // Carico filiali di test abilitate
181900131003         clear TIBS02ds;
182000131003         T02Mod = 'C';
182100131003         T02sif = knsif;
182200131003         T02cod = 'VPO';
182300131003         t02tla = 'L'   ;
182400131003         T02ke1 ='DECOFI816' ;
182500131003         TNTBE_RicercaControllo  (kpjba : tibs02ds);
182600131003
182700131003         if  T02err  = *blank;
182800131003         xx= 1 ;
182900131003         yy= 1 ;
183000131003
183100131003         dow xx< 256   ;
183200131003           fil_ab(yy)= %subst(t02uni:xx:3)  ;
183300131003           yy=yy+ 1   ;
183400131003           xx=xx+ 3   ;
183500131003         enddo  ;
183600131003
183700131003         endif;
183800131003
183900131003         ENDSR;
184000131003      /end-free
184100080206
184200080206       //--------------------------------------------------------------
184300080206       //?Schiere a tempo di compilazione.
184400080206       //--------------------------------------------------------------
184500080206
184600080410** - MSG ------------------------------------------------------------------ -*
184700130821Non trovata località: impossibile caricare gli orari servizi!!                            1
184800130821Immettere un orario formalmente corretto                                                  2
184900130821L'orario di fine deve essere maggiore dell'orario iniziale                                3
185000130829Linea non prevista dal cap/località. Verrà eliminata. Enter per continuare                4
185100130829Linea già immessa                                                                         5
185200130828L'inserimento non è ECCEZIONE:orari = a quelli
185300130829Posizionarsi sulla riga di cui si voglio ripristinare gli orari e premere F05             7
185400130821Linea inesistente                                                                         8
185500130821Orario impostato errato                                                                   9
185600130821Non trovato CAP: impossibile caricare gli orari servizi!!                                10
185700130821L'orario minimo non può essere maggiore dell'orario di inizio standard                   11
185800130821L'orario massimo non può essere minore dell'orario di fine standard                      12
185900130822La linea servizio CONSEGNE deve essere uguale alla precedente servizio RITIRO            13
186000130827Non trovata FILIALE: impossibile caricare gli orari servizi!!                            14
186100130904Immettere PRIMA gli orari PER la LINEA xxx, prima di inserire per cap/loc                15
186200130904L'orario non può essere inferiore a xx:xx                                                16
186300130904L'orario non può essere maggiore di xx:xx                                                17
186400131011Orario limite ritiri NON commissionati DEVE essere > = al limite dei commissionati       18
186500131011Orario limite ritiri Cli Codificati DEVE essere > = al limite dei NON commissionati      19
186600131011Si possono inserire soltanto orari arrotondati ai quarti d'ora                           20
186700131031Orario inizio consegna non congruente con tempo di consegna del serv.                    21
186800131216Tempo oraganizzazione non minore di xx:xx  e non maggiore di 12:00                       22
