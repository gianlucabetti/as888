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
006801140206     d                 ds
006802140206     d lnaprevuni                    42  0
006803140206     d lna_prev                       3  0 dim(14)overlay(lnaprevuni)
006900140206     d*lna_prev        s              3  0 dim(14)
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
066200140206         dute01=utefaf  ;
066300080206
066400080206       ENDSR;
066500130820       //--------------------------------------------------------------
066600130820       //?Carica AZCOS00F dalla località o dal cap se non presente
066700130820       //--------------------------------------------------------------
066800130820       BEGSR Carica_AZCOS ;
066900130821
067000130821        clear CAPostis;
067100130821        clear CAPostfs;
067200130821        clear cAPomiis;
067300130821        clear CAPomxfs;
067400131010        clear CAPolrsc;
067500131010        clear CAPolrnc;
067600131010        clear CAPolrnk;
067700130829        clear CAPprop ;
067800130829        clear CAPrifer;
067900130829        clear CAPvisual;
068000131213        clear CAPtrzs  ;
068100130821        clear v1himm  ;
068200130828        clear v1hforz ;
068300130829        clear v1hforzlna;
068400131213        protsoglia=*off  ;
068500131016        v1cprop='   |'   ;
068600130821
068700130827         if   tlna='?'   ;
068800130827         v1himm='E'  ;
068900130827         endif           ;
069000130827
069100130821         if klna= savlna;
069200130821         endif  ;
069300130821         savlna=klna  ;
069400130820
069500130820       v1clna=klna  ;
069600130821       v1hlna=klna  ;
069700130828
069800130828       exsr Decodi_lna  ;
069900130820
070000130821       if ktser  ='R'   ;
070100131216          v1dtser='RITIRI' ;
070200130828          HI_dlna=*on  ;
070300130820       else  ;
070400131216          v1dtser='CONSEG' ;
070500130828          HI_dlna=*off ;
070600130820       endif  ;
070700130822       v1htser=ktser  ;
070800131213
070900131213       // Soglia
071000131213       if ksoglia='P'  ;
071100131213         v1csogl  ='Pom'  ;
071200131213         else  ;
071300131213         clear v1csogl    ;
071400131213         endif  ;
071500131213
071600130820
071700130827       if Wrecord= 'N'    ;
071800130820        clear  v1costis  ;
071900130820        clear  v1costfs  ;
072000131016        clear  v1comiis  ;
072100131016        clear  v1comxfs  ;
072200131010        clear  v1colrsc  ;
072300131010        clear  v1colrnc  ;
072400131010        clear  v1colrnk  ;
072500130820        clear  v1ctrzs   ;
072600130820       else  ;
072700130820        v1costis=cosostis  ;
072800130820        v1costfs=cosostfs  ;
072900131016        v1comiis=cosomiis  ;
073000131016        v1comxfs=cosomxfs  ;
073100131010        v1colrsc=cosolrsc  ;
073200131010        v1colrnc=cosolrnc  ;
073300131010        v1colrnk=cosolrnk  ;
073400131213        v1ctrzs =costrzs   ;
073500131010
073600130827        select  ;
073700130827        when wrecord='C'  ;
073800131016        v1cprop='cap|'   ;
073900130827        when wrecord='F'  ;
074000131016        v1cprop='fil|'   ;
074100130827        when wrecord='9'  ;
074200131016        v1cprop='std|'   ;
074300130827        endsl  ;
074400130821
074500130820       endif ;
074600130820
074700130829       if Immiss    =*on  ;
074800130829        v1himm='S'  ;
074900130829       else  ;
075000130821       v1cute=cosute  ;
075100130821        if cosduv>0  ;
075200130821        dataiso=%date(cosduv)  ;
075300130821        datadmy=dataiso ;
075400130821        v1dduv=%dec(datadmy) ;
075500130821        endif  ;
075600130828
075700130902        // visto che la linea on è prevista e mi si sposiziona nella lettura del file
075800130902        // non eseguo la ricerca del riferimento
075900130902         if   tlna<>'?'   ;
076000130902          exsr cerca_rif ;
076100130902          else  ;
076200130902          wrecord='N' ;
076300130902         endif  ;
076400130902        endif  ;
076500130828
076600130902       // Se ho trovato il record memorizzo i dati
076700130828        if wrecord<>'N'    ;
076800130828        CAPostis=cosostis  ;
076900130828        CAPostfs=cosostfs  ;
077000130828        cAPomiis=cosomiis  ;
077100130828        CAPomxfs=cosomxfs  ;
077200131010        CAPolrsc=cosolrsc  ;
077300131010        CAPolrnc=cosolrnc  ;
077400131010        CAPolrnk=cosolrnk  ;
077500130828        CAPrifer=wrecord   ;
077600131213        CAPtrzs =costrzs   ;
077700130829        if wrecord='9'  ;
077800131213         CAPprop='OrRif:STD'  ;
077900130829         endif ;
078000130829        if wrecord='F'  ;
078100131213         CAPprop='OrRif:FIL'  ;
078200130829         endif ;
078300130829        if wrecord='C'  ;
078400131213         CAPprop='OrRif:CAP'  ;
078500130829         endif ;
078600130829
078700130828        else  ;
078800130828        clear CAPostis;
078900130828        clear CAPostfs;
079000130828        clear CAPomiis;
079100130828        clear CAPomxfs;
079200131010        clear CAPolrsc;
079300131010        clear CAPolrnc;
079400131010        clear CAPolrnk;
079500130829        clear CAPprop ;
079600131213        clear CAPtrzs ;
079700130828        CAPrifer='N'  ;
079800130828        endif  ;
079900130821
080000130829       exsr Protcampi    ;
080100130829
080200130820       s01nrr=s01nrr + 1 ;
080300130820       write sio2s01     ;
080400130820
080500130820       ENDSR;
080600080206
080700130828       //--------------------------------------------------------------
080800130828       //?Decodifica linea di arrivo
080900130828       //--------------------------------------------------------------
081000130828       BEGSR Decodi_lna  ;
081100130828
081200130904       select  ;
081300130904       when klna=999  ;
081400131213        v1dlna='Orari Std   '  ;
081500130904       when klna=888  ;
081600131213        v1dlna='OrariContr.'  ;
081700130904        other ;
081800130828       chain klna  azorg01l  ;
081900130828       if %found(azorg01l)  ;
082000131216        if orgde5<>*blanks   ;
082100131216          v1dlna=orgde5  ;
082200131216          else  ;
082300131216          v1dlna=orgdes  ;
082400131216          endif  ;
082500131216
082600130828       else  ;
082700130828         v1dlna=*all'?'  ;
082800130828       endif  ;
082900130904       endsl  ;
083000130828       ENDSR  ;
083100080206       //--------------------------------------------------------------
083200080627       //?Gestione SFL 01
083300080206       //--------------------------------------------------------------
083400080409       BEGSR GesS01;
083500080207
083600080207         // Inizializzazione videata
083700080409         if  $InzS01 = *on;
083800080409            exsr InzS01;
083900080703         // Posizionamento cursore
084000100507         // C01csr = 1;
084100080703           s01nrr = 1;
084200080703           sfldsp_n=*off;
084300080409            $InzS01  = *off;
084400080207         endif;
084500080207
084600080207
084700080207         // Emissione Testata e Piede con tasti funzionali abilitati
084800080207         if  errGenerico = *off;
084900131003         select   ;
085000131003         when §15ita<>'I'  ;
085100130822           sfldsp_n=*on      ;
085200130822           write  sio2D02;
085300131003
085400140206         when lnaprevuni=0 ;
085500131003           sfldsp_n=*on      ;
085600131003           write  sio2D03;
085700131003         other   ;
085800131003           write  sio2Z01;
085900131003           endsl  ;
086000080207         endif;
086100080703
086200080703         // Posizionamento cursore
086300080703         if  C01csr  > *zero;
086400080703           C01rcd = C01csr;
086500080703         endif  ;
086600080207
086700080207         // Emissione videata
086800130820         exfmt  sio2C01;
086900080410
087000080207         reset errMessage;
087100080207         reset errGenerico;
087200080702         clear Vscmsg;
087300130820         clear Errostis  ;
087400130820         clear Errostfs  ;
087500130820         clear Erromiis  ;
087600130820         clear Erromxfs  ;
087700131010         clear Errolrsc  ;
087800131010         clear Errolrnc  ;
087900131010         clear Errolrnk  ;
088000130820         clear ErrTrzs   ;
088100130821         clear Errlna    ;
088200130828         clear BL_ostis  ;
088300130828         clear BL_ostfs  ;
088400130828         clear BL_omiis  ;
088500130828         clear BL_omxfs  ;
088600131010         clear BL_olrsc  ;
088700131010         clear BL_olrnc  ;
088800131010         clear BL_olrnk  ;
088900131213         clear BL_trzs   ;
089000080207
089100080523 1       SELECT;
089200080207
089300080207         // - F3=Fine
089400080523 1         WHEN  dsp_aid = c_F03;
089500080409            $Fine = *on;
089600130820            Do0f03='1'   ;
089700130828
089800130829         // - F5=Ripristina orari di riferimento (disabilitato)
089900130829 1         // WHEN  dsp_aid = c_F05;
090000130829           // chain    c01csr sio2s01  ;
090100130829           // if %found     ;
090200130829           //  if v1costis<>CAPostis  ;
090300130829           //  v1costis=CAPostis  ;
090400130829           //  BL_ostis=*on  ;
090500130829           // endif  ;
090600130829           // if v1costfs<>CAPostfs  ;
090700130829           //  v1costfs=CAPostfs  ;
090800130829           //  BL_ostfs=*on  ;
090900130829           // endif  ;
091000130829           // if v1comiis<>CAPomiis  ;
091100130829           //  v1comiis=CAPomiis  ;
091200130829           //  BL_omiis=*on  ;
091300130829           // endif  ;
091400130829           // if v1comxfs<>CAPomxfs  ;
091500130829           //  v1comxfs=CAPomxfs  ;
091600130829           //  BL_omxfs=*on  ;
091700130829           // endif  ;
091800130829           // if v1ctrzs <>CAPtrzs   ;
091900130829           //  v1ctrzs =CAPtrzs   ;
092000130829           //  BL_trzs =*on  ;
092100130829           // endif  ;
092200130829           //  else  ;
092300130829            // cursore non ben posizionato : errore
092400130829           //  s01nrr=1  ;
092500130829           // chain    s01nrr sio2s01  ;
092600130829           // errlna  =*on ;
092700130829           // vscmsg = Msg(07);
092800130829           // endif         ;
092900130829
093000130829           // exsr aggiosfl      ;
093100130829
093200130829         // - F7=Visualizza orari di riferimento per tutte le righe
093300131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F07;
093400130829            yy=1           ;
093500130829            s01nrr=1  ;
093600130829            chain s01nrr    sio2s01    ;
093700130829
093800130829 0          dow %found    ;
093900130829
094000130829            if capvisual<>'S'  ;
094100130829            capvisual='S'  ;
094200130829            else  ;
094300130829            capvisual=' '  ;
094400130829            endif  ;
094500130829
094600130829            exsr aggiosfl      ;
094700130829
094800130829            s01nrr=s01nrr+1     ;
094900130829
095000130829            chain s01nrr    sio2s01    ;
095100130829           enddo  ;
095200080207
095300131213         // - F09=Abilita riga soglia
095400131213 1         WHEN  sfldsp_n=*off and dsp_aid = c_F09;
095500131213            yy=1           ;
095600131213            s01nrr=1  ;
095700131213            chain s01nrr    sio2s01    ;
095800131213
095900131213 0          dow %found    ;
096000131213
096100131216            if v1csogl ='Pom'    ;
096200131213            exsr protcampi  ;
096300131213
096400131213            exsr aggiosfl      ;
096500131216            AbilF9=*off        ;
096600131213            endif  ;
096700131213
096800131213            s01nrr=s01nrr+1     ;
096900131213
097000131213            chain s01nrr    sio2s01    ;
097100131213           enddo  ;
097200080207         // - F12=Ritorno
097300080523 1         WHEN  dsp_aid = c_F12;
097400080702            $Fine = *on;
097500130820            Do0f12='1'   ;
097600080414
097700131003         // - F16=Annullamento  (disabilito)
097800130829 1         WHEN  dsp_aid = c_F16;
097900130829         //  exsr annull_rec    ;
098000130829         //  $Fine = *on;
098100130829
098200080530 x1      // Invio / F6
098300080207           OTHER;
098400081006
098500081006        // Non effettuo controlli in interrogazione
098600131003 1           if orariInt=*off and sfldsp_n=*off ;
098700081006               exsr ContrS01 ;
098800081006 2             if  errGenerico = *on;
098900081006                 leavesr;
099000081006 2             endif;
099100081006 2           endif;
099200080530
099300081023         // F6=conferma Aggiornamento  F10=anche stampa
099400081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
099500131003             if sfldsp_n=*off  ;
099600131003              exsr   ConfAggio ;
099700131003             endif  ;
099800081023
099900080704           $Fine = *on;
100000080530           endif  ;
100100080207
100200080523 1       ENDSL;
100300080207
100400080207       ENDSR;
100500080207
100600080526       //--------------------------------------------------------------
100700080702       //?controlli SFL01
100800080409       //--------------------------------------------------------------
100900080409       BEGSR ContrS01;
101000130821
101100130821       yy=1  ;
1012001308291      dow yy<=12;
101300130821       if lna_es(yy)='R'  ;
101400130821       clear lna_es(YY)  ;
101500130821       endif  ;
101600130821
101700130821       yy=yy+1  ;
1018001308291      enddo    ;
101900130821
102000081126       yy=1           ;
102100080703       s01nrr=1  ;
102200130820       chain s01nrr    sio2s01    ;
102300080703
102400081006 0     dow %found    ;
102500130820
102600130821       clear  v1dlna  ;
102700130821
102800130828       klna=v1clna ;
102900130828       exsr decodi_lna  ;
103000130828 1     if v1dlna=*all'?'        ;
103100130821          errlna  =*on ;
103200130821          vscmsg = Msg(08);
103300130821          EXSR AggioSFL   ;
103400130821          leavesr;
103500130821 1     endif  ;
103600130821
103700130828       // La linea deve essere una di quelle previste
103800130829       //  se non prevista dico che la annullerò
103900130828         Indx=%lookup(v1clna:lna_prev)  ;
1040001308291        if Indx=0  and v1hforzlna=' '  ;
104100130828
104200130821          errlna  =*on ;
104300130821          vscmsg = Msg(04);
104400130829          v1hforzlna='S'  ;
104500130821
104600130821          EXSR AggioSFL   ;
104700130821          leavesr;
104800130821 1     endif  ;
104900130821
105000130822 1     if v1hlna<>v1clna ;
105100130821
105200130821       // Se la riga è RITIRO  controllo il linea doppia e la memorizzo
105300130822  2     if v1htser='R'  ;
105400130821
105500130828         if lna_es(indx)<>' '  ;
105600130821          errlna  =*on ;
105700130821          vscmsg = Msg(05);
105800130821          EXSR AggioSFL   ;
105900130821          leavesr;
106000130821  x3      else  ;
106100130828          lna_es(indx)='R'   ;
106200130821   3      endif  ;
106300130821
106400130821        savlna=v1clna  ;
106500130821
106600130821  x2   else  ;
106700130821   3   if v1clna  <> savlna  ;
106800130821          errlna  =*on ;
106900130821          vscmsg = Msg(13);
107000130821          EXSR AggioSFL   ;
107100130821          leavesr;
107200130821  3    endif  ;
107300130821  2    endif  ;
107400130822  1    endif  ;
107500130821
107600130822       // O R A R I
107700130829       //  li controllo solo per le linee valide
107800130829
107900130829  1a   if indx>0 ;
108000131216
108100131216       // controllo Soglia solo se l'ho abilitata ;
108200131216  1b   if v1csogl= *blanks   or AbilF9=*off  ;
108300131216
108400130829       // Se per la linea di arrivo manca il record di filiale --> non posso inserire
108500130829       if do0cap<>*blanks ;
108600130829       clear kloc  ;
108700130829       clear kcap  ;
108800130829       ktser=v1htser  ;
108900131213           chain(n)  (do0ver:do0nar:kcap:kloc:v1clna:ktser)  azcos03l   ;
109000131213           if not %found(azcos03l)  ;
109100130829          errlna  =*on ;
109200130829          vscmsg = Msg(15);
109300130829          %subst(vscmsg:40:3)=%editc(v1clna:'X')  ;
109400130829          EXSR AggioSFL   ;
109500130829          leavesr;
109600130829           endif  ;
109700130829       endif  ;
109800130829
109900130829
110000130904       // Imposto gli orari in base al servizio
110100130904       if v1clna<>888     ;
110200130904       if v1htser='C'   ;
110300131216       if v1csogl='Pom'  ;
110400131216       W88ostis=C88Postis ;
110500131216       W88ostfs=C88Postfs ;
110600131216       W88omiis=C88Pomiis ;
110700131216       W88omxfs=C88Pomxfs ;
110800131216       W88olrsc=C88Polrsc ;
110900131216       W88olrNc=C88Polrnc ;
111000131216       W88olrNk=C88Polrnk ;
111100131216       W88trzs =C88Ptrzs  ;
111200131216       else  ;
111300130904       W88ostis=C88ostis ;
111400130904       W88ostfs=C88ostfs ;
111500130904       W88omiis=C88omiis ;
111600130904       W88omxfs=C88omxfs ;
111700131010       W88olrsc=C88olrsc ;
111800131010       W88olrNc=C88olrnc ;
111900131010       W88olrNk=C88olrnk ;
112000131216       W88trzs =C88trzs  ;
112100131216       endif  ;
112200131216
112300130904       else  ;
112400130904       W88ostis=r88ostis ;
112500130904       W88ostfs=r88ostfs ;
112600130904       W88omiis=r88omiis ;
112700130904       W88omxfs=r88omxfs ;
112800131010       W88olrsc=r88olrsc ;
112900131010       W88olrnc=r88olrnc ;
113000131010       W88olrnk=r88olrnk ;
113100130904       endif  ;
113200131010       endif  ;
113300131010
113400131011       if v1clna= 888 or w88ostfs=0000  ;
113500130904       W88ostis=0000 ;
113600130904       W88ostfs=2400 ;
113700130904       W88omiis=0000 ;
113800130904       W88omxfs=2400 ;
113900131010       W88olrsc=2400 ;
114000131010       W88olrnc=2400 ;
114100131010       W88olrnk=2400 ;
114200131216       W88trzs =0000 ;
114300130904       endif  ;
114400130904
114500131216
114600131011       // Orario inizio
114700130829 1     if  v1costis=0  ;
114800130820          errostis=*on ;
114900130820          vscmsg = Msg(02);
115000081003          EXSR AggioSFL   ;
115100081003          leavesr;
115200130820 1     endif  ;
115300130821
115400130821       wora=v1costis ;
115500130821       exsr contr_ora ;
115600130829 1     if vscmsg<>*blanks ;
115700130821          errostis=*on ;
115800130821          EXSR AggioSFL   ;
115900130821          leavesr;
116000130821 1     endif  ;
116100130904
116200130904       // no minore del limite previsto
116300130904       if v1costis<w88ostis  ;
116400130904          errostis=*on ;
116500130904          vscmsg = Msg(16);
116600130904          %subst(vscmsg:37:5)=%editw(w88ostis: '0 :  ')    ;
116700130904          EXSR AggioSFL   ;
116800130904          leavesr;
116900130904 1     endif  ;
117000131031       // Orario inizio consegne no maggiore tempo consegna per tipo servizio
117100140110       // non controllo per orari del pomeriggio
117200140110       if v1htser='C' and do0cap<>*blanks
117300140110          and v1csogl=*blanks              ;
117400140110
117500131031          exsr car_cpt;
117600131031          if v1costis>wcpt0in4;
117700131031             errostis=*on ;
117800131031             vscmsg = Msg(21);
117900131031             vscmsg=%trim(vscmsg)+' ' + %trim(wdtsp);
118000131031             EXSR AggioSFL   ;
118100131031             leavesr;
118200131031          endif;
118300131031       endif;
118400130821
118500130821
118600131011       // Orario fine
118700130820 1     if  v1costfs=0     ;
118800130820          errostfs=*on ;
118900130820          vscmsg = Msg(02);
119000130820          EXSR AggioSFL   ;
119100130820          leavesr;
119200130820 1     endif  ;
119300130821
119400130821       wora=v1costfs ;
119500130821       exsr contr_ora ;
119600130829 1     if vscmsg<>*blanks ;
119700130821          errostfs=*on ;
119800130821          EXSR AggioSFL   ;
119900130821          leavesr;
120000130821 1     endif  ;
120100130904       // no maggiore del limite previsto
120200130904       if v1costfs>w88ostfs  ;
120300130904          errostfs=*on ;
120400130904          vscmsg = Msg(17);
120500130904          %subst(vscmsg:37:5)=%editw(w88ostfs: '0 :  ')    ;
120600130904          EXSR AggioSFL   ;
120700130904          leavesr;
120800130904 1     endif  ;
120900130820
121000130820 1     if  v1costfs<v1costis  ;
121100130820          errostfs=*on ;
121200130820          vscmsg = Msg(03);
121300130820          EXSR AggioSFL   ;
121400130820          leavesr;
121500130820 1     endif  ;
121600131011
121700131016       if v1htser='C'  ;
121800131216       // Tempo oraganizzazione --> solo per CONSEGNE
121900131216 1     if  v1ctrzs<w88trzs   or v1ctrzs>1200 ;
122000131216          Errtrzs =*on ;
122100131216          vscmsg = Msg(22);
122200131216          %subst(vscmsg:37:5)=%editw(w88trzs:'0 :  ')  ;
122300131216          EXSR AggioSFL   ;
122400131216          leavesr;
122500131216 1     endif  ;
122600131216
122700131016       // Orario MINIMO  solo per CONSEGNE
122800131016 1     if  v1comiis=0  ;
122900131016          erromiis=*on ;
123000131016          vscmsg = Msg(02);
123100131016          EXSR AggioSFL   ;
123200131016          leavesr;
123300131016 1     endif  ;
123400131016
123500131016       wora=v1comiis ;
123600131016       exsr contr_ora ;
123700131016 1     if vscmsg<>*blanks ;
123800131016          erromiis=*on ;
123900131016          EXSR AggioSFL   ;
124000131016          leavesr;
124100131016 1     endif  ;
124200131016
124300131016       // no minore del limite previsto
124400131016       if v1comiis<w88omiis  ;
124500131016          erromiis=*on ;
124600131016          vscmsg = Msg(16);
124700131016          %subst(vscmsg:37:5)=%editw(w88omiis: '0 :  ')    ;
124800131016          EXSR AggioSFL   ;
124900131016          leavesr;
125000131016 1     endif  ;
125100131016
125200131016       // no maggiore dell'orario di inizio
125300131016       if v1comiis > v1costis  ;
125400131016          erromiis=*on ;
125500131016          vscmsg = Msg(11);
125600131016          EXSR AggioSFL   ;
125700131016          leavesr;
125800131016 1     endif  ;
125900131016
126000131016       // Orario MASSIMO
126100131016 1     if  v1comxfs=0     ;
126200131016          erromxfs=*on ;
126300131016          vscmsg = Msg(02);
126400131016          EXSR AggioSFL   ;
126500131016          leavesr;
126600131016 1     endif  ;
126700131016
126800131016       wora=v1comxfs ;
126900131016       exsr contr_ora ;
127000131016 1     if vscmsg<>*blanks ;
127100131016          erromxfs=*on ;
127200131016          EXSR AggioSFL   ;
127300131016          leavesr;
127400131016 1     endif  ;
127500131016       // no maggiore del limite previsto
127600131016       if v1comxfs>w88omxfs  ;
127700131016          erromxfs=*on ;
127800131016          vscmsg = Msg(17);
127900131016          %subst(vscmsg:37:5)=%editw(w88omxfs: '0 :  ')    ;
128000131016          EXSR AggioSFL   ;
128100131016          leavesr;
128200131016 1     endif  ;
128300131016
128400131016 1     if  v1comxfs<v1comiis  ;
128500131016          erromxfs=*on ;
128600131016          vscmsg = Msg(03);
128700131016          EXSR AggioSFL   ;
128800131016          leavesr;
128900131016 1     endif  ;
129000131016       // no minore   dell'orario di fine
129100131016       if v1comxfs < v1costfs  ;
129200131016          erromxfs=*on ;
129300131016          vscmsg = Msg(12);
129400131016          EXSR AggioSFL   ;
129500131016          leavesr;
129600131016 1     endif  ;
129700131016 1     endif  ;
129800131016
129900131011       if v1htser='R'  ;
130000131011
130100131011       // Orari limite ritiri: COMMISSIONATI
130200131011 1     if  v1colrsc=0     ;
130300131011          erroLRSC=*on ;
130400131011          vscmsg = Msg(02);
130500131011          EXSR AggioSFL   ;
130600131011          leavesr;
130700131011 1     endif  ;
130800131011
130900131011       wora=v1coLRSC ;
131000131011       exsr contr_ora ;
131100131011 1     if vscmsg<>*blanks ;
131200131011          erroLRSC=*on ;
131300131011          EXSR AggioSFL   ;
131400131011          leavesr;
131500131011 1     endif  ;
131600131011       // no maggiore del limite previsto
131700131011       if v1coLRSC>w88oLRSC  ;
131800131011          erroLRSC=*on ;
131900131011          vscmsg = Msg(17);
132000131011          %subst(vscmsg:37:5)=%editw(w88oLRSC: '0 :  ')    ;
132100131011          EXSR AggioSFL   ;
132200131011          leavesr;
132300131011 1     endif  ;
132400131011       // Orari limite ritiri: NON COMMISSIONATI
132500131011 1     if  v1colrNc=0     ;
132600131011          erroLRNC=*on ;
132700131011          vscmsg = Msg(02);
132800131011          EXSR AggioSFL   ;
132900131011          leavesr;
133000131011 1     endif  ;
133100131011
133200131011       wora=v1coLRNC ;
133300131011       exsr contr_ora ;
133400131011 1     if vscmsg<>*blanks ;
133500131011          erroLRNC=*on ;
133600131011          EXSR AggioSFL   ;
133700131011          leavesr;
133800131011 1     endif  ;
133900131011       // no maggiore del limite previsto
134000131011       if v1coLRNC>w88oLRNC  ;
134100131011          erroLRNC=*on ;
134200131011          vscmsg = Msg(17);
134300131011          %subst(vscmsg:37:5)=%editw(w88oLRNC: '0 :  ')    ;
134400131011          EXSR AggioSFL   ;
134500131011          leavesr;
134600131011 1     endif  ;
134700131011       // Orari limite ritiri: NON COMMISSIONATI CODIFICATI
134800131011 1     if  v1colrNK=0     ;
134900131011          erroLRNK=*on ;
135000131011          vscmsg = Msg(02);
135100131011          EXSR AggioSFL   ;
135200131011          leavesr;
135300131011 1     endif  ;
135400131011
135500131011       wora=v1coLRNK ;
135600131011       exsr contr_ora ;
135700131011 1     if vscmsg<>*blanks ;
135800131011          erroLRNK=*on ;
135900131011          EXSR AggioSFL   ;
136000131011          leavesr;
136100131011 1     endif  ;
136200131011       // no maggiore del limite previsto
136300131011       if v1coLRNK>w88oLRNK  ;
136400131011          erroLRNK=*on ;
136500131011          vscmsg = Msg(17);
136600131011          %subst(vscmsg:37:5)=%editw(w88oLRNK: '0 :  ')    ;
136700131011          EXSR AggioSFL   ;
136800131011          leavesr;
136900131011 1     endif  ;
137000131011
137100131011       // secondo orario >= al primo
137200131011       if v1colrnc<v1colrsc  ;
137300131011          erroLRNc=*on ;
137400131011          vscmsg = Msg(18);
137500131011          EXSR AggioSFL   ;
137600131011          leavesr;
137700131011       endif  ;
137800131011       // terzo orario >= al secondo
137900131011       if v1colrnk<v1colrnc  ;
138000131011          erroLRNk=*on ;
138100131011          vscmsg = Msg(19);
138200131011          EXSR AggioSFL   ;
138300131011          leavesr;
138400131011       endif  ;
138500131011
138600131011       endif  ;
138700130820
138800131216 1b    endif  ;
138900131216 1a    endif  ;
139000130821
139100130821       EXSR AggioSFL   ;
139200080703
139300080703       s01nrr=1+s01nrr  ;
139400130820       chain s01nrr    sio2s01    ;
139500081006 0     enddo         ;
139600081006
139700080409       ENDSR;
139800080409
139900130829       //--------------------------------------------------------------
140000130829       //?Annullamento record
140100130829       //--------------------------------------------------------------
140200130829       //BEGSR Annull_rec  ;
140300130829
140400130829       // Elaboro la schiera delle linee e deleto
140500130829       //yy=1   ;
140600130829       //dow lna_prev(YY)>0     ;
140700130829
140800130829       //klna=lna_prev(yy)     ;
140900130829       //ktser='R'  ;
141000131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
141100130829       //ktser='C'  ;
141200131213       //  delete (do0ver:do0nar:do0cap:do0loc:klna:ktser)  azcos03l   ;
141300130829
141400130829       //yy=yy+1  ;
141500130829       //enddo   ;
141600130829
141700130829       //ENDSR;
141800080703       //--------------------------------------------------------------
141900080703       //?Aggiornamento sfl
142000080703       //--------------------------------------------------------------
142100080703       BEGSR AGGIOSFL  ;
142200080703       if vscmsg<>*blanks   ;
142300080703       errMessage  = *on;
142400080703       errGenerico = *on;
142500080704       c01csr=s01nrr    ;
142600080703       endif    ;
142700080703
142800080703       exsr ProtCampi   ;
142900080703
143000130820       update  sio2s01  ;
143100080703       ENDSR   ;
143200130821       //--------------------------------------------------------------
143300130821       //?Controllo campo ORA
143400130821       //--------------------------------------------------------------
143500130821       BEGSR Contr_ORA ;
143600130821       if whh < 0  or whh > 24  ;
143700130821          vscmsg = Msg(09);
143800130821       endif  ;
143900130821       if wmm < 0  or wmm > 59  ;
144000130821          vscmsg = Msg(09);
144100130821       endif  ;
144200131011
144300131011       // Minuti arrotondati ai 15 minuti
144400131011       if wmm<>00 and wmm<>15 and wmm<>30 and wmm<>45  ;
144500131011          vscmsg = Msg(20);
144600131011       endif  ;
144700130821
144800130821       ENDSR ;
144900080207       //--------------------------------------------------------------
145000080627       //?Inizializzazione SFL01
145100080207       //--------------------------------------------------------------
145200080409       BEGSR InzS01;
145300080207
145400080207       // Pulizia subfile
145500080207         SflDsp_N    = *on;
145600080207         SflDspCtl_N = *on;
145700130820         write  sio2C01;
145800080207         SflDspCtl_N = *off;
145900080207         SflEnd      = *off;
146000080530
146100080530         clear C01rcd;
146200100507         C01csr=1;
146300080627         S01nrr=0 ;
146400080702         clear Vscmsg;
146500080207         errMessage  = *off;
146600080207         errGenerico = *off;
146700130820         immiss    =*off ;
146800130821         clear savlna ;
146900130821         clear yy    ;
147000130821         clear lna_es;
147100130821         clear Wrecord ;
147200130821         clear v1dduv  ;
147300130821         clear v1cute  ;
147400130821
147500130822         // Se la nazione no è italia --> non carico nulla e avviso che non esistono
147600130822         //  orari
147700130822
147800130827 1       if §15ita= 'I'  ;
147900130820
148000130821         // 1)
148100130821         // Leggo e carico eventuali linee non più previste dal cap/località
148200130821
148300130828 1a    if   do0cap<>*blanks   ;
148400130828
148500130829       Protlna=*off ;
148600131213       setll  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
148700131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
148800131213 2      dow  not %eof(azcos03l)  ;
148900130821          klna=coslna ;
149000130827          Indx=%lookup(coslna:lna_prev)  ;
149100130827 3      if indx=0 ;
149200130821          tlna='?'   ;
149300130829           ktser=costser   ;
149400130821          exsr   Leggi_cos ;
1495001308273       endif  ;
149600130821
149700131213       reade  (do0ver:do0nar:do0cap:do0loc)  azcos03l   ;
1498001308272      enddo  ;
1499001308281a     endif  ;
150000130821
150100130821         // 2)
150200130821         // Carico i record  già presenti
150300130829         Protlna=*on  ;
150400130902         clear tlna   ;
150500130820
150600130827           YY=1  ;
150700140206 2     //  dow lna_prev(yy)>0  ;
150701140206           for  yy=1 to %elem(lna_prev);
150702140206           if lna_prev(yy)>0;
150800130827
150900130827           klna=lna_prev(YY) ;
151000131213           chain  (do0ver:do0nar:do0cap:do0loc:klna)  azcos03l   ;
151100131213 3         if %found(azcos03l)  ;
151200130827           lna_es(YY)='S'  ;
151300130830           endif  ;
151400131010
151500131213           if klna=999  or klna=888 ;
151600131213            indlna9988=*on  ;
151700131010            else  ;
151800131213            indlna9988=*off ;
151900131010            endif  ;
152000130830
152100131213           ksoglia=*blanks ;
152200131213           ktser='R'  ;
152300130821           exsr Leggi_cos  ;
152400130821           ktser='C'  ;
152500130821           exsr Leggi_cos  ;
152600131213           // Per consegne vedo se presente anche soglia "P"
152700131213
152800131216           if lna_sog(yy)='S'  or abilF9=*on  ;
152900131213           ksoglia='P'  ;
153000131213           exsr Leggi_cos  ;
153100131213           endif  ;
153200130827
153300140206       //  yy=yy+1  ;
153301140206           endif;
153302140206           endfor;
153400140206 2     //  enddo    ;
153500130821
1536001308271        endif  ;
153700130827
153800080702         ENDSR    ;
153900130821       //--------------------------------------------------------------
154000130821       //?Lettura record di AZCOS
154100130821       //--------------------------------------------------------------
154200130821           BEGSR Leggi_cos  ;
154300131213           chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia) azcos03l  ;
154400131213           if %found(azcos03l)  ;
154500130821           Immiss=*off ;
154600130821           wrecord='S'  ;
154700130828           else  ;
154800130821
154900130821           Immiss=*on  ;
155000130828           // Cerca il record di riferimento
155100130828           exsr cerca_rif  ;
155200130828
155300130828           endif  ;
155400130821
155500130821           exsr      Carica_AZCOS ;
155600131213
155700130821           ENDSR  ;
155800130828       //--------------------------------------------------------------
155900130828       //?Cerco record di riferimento per vedere se dati uguali o diversi
156000130828       //--------------------------------------------------------------
156100130828           BEGSR   Cerca_rif  ;
156200130828           wrecord='N'  ;
156300130828
156400130904           if klna<>999  and klna<>888 ;
156500130828
156600130829           if do0cap<>*blanks    ;
156700130829
156800130828 1         if do0loc<>*blanks   ;
156900130828            clear kloc  ;
157000131213           chain(n)  (do0ver:do0nar:do0cap:kloc:klna:ktser:ksoglia) azcos03l ;
157100131213           if %found(azcos03l)  ;
157200130828           wrecord='C'  ;
157300130828           endif  ;
157400130828 1         endif  ;
157500130828
157600130828           // Se non presente a livello di CAP provo a livello di filiale
157700130828 1         if wrecord='N'  ;
157800130828           clear kcap  ;
157900131213           chain(n)  (do0ver:do0nar:kcap:kloc:klna:ktser:ksoglia)  azcos03l   ;
158000131213           if %found(azcos03l)  ;
158100130828           wrecord='F'  ;
158200130828           endif  ;
158300130828 1         endif  ;
158400130829
158500130829 1         endif  ;
158600130828
158700131213           // standard aziendale
158800130828 1         if wrecord='N'  ;
158900130828           clear kcap  ;
159000131213           chain(n)  (do0ver:do0nar:kcap:kloc:lna999:ktser:ksoglia) azcos03l ;
159100131213           if %found(azcos03l)  ;
159200130828           wrecord='9'  ;
159300130828           endif  ;
159400130828 1         endif  ;
159500130828
159600130828 1         endif  ;
159700130828           ENDSR  ;
159800080702       //--------------------------------------------------------------
159900080702       //?Gestione protezione campi del sfl
160000080702       //--------------------------------------------------------------
160100080702          BEGSR   ProtCampi   ;
160200130820           Immiss  =*off ;
160300130829           vis_rifer=*off      ;
160400130829           BL_ostis=*off ;
160500130829           BL_ostfs=*off ;
160600130829           BL_omiis=*off ;
160700130829           BL_omxfs=*off ;
160800131010           BL_olrsc=*off ;
160900131010           BL_olrnc=*off ;
161000131010           BL_olrnK=*off ;
161100131213           BL_trzs =*off ;
161200130829           Protriga=*off ;
161300130829           Protlna=*on  ;
161400131216           Protsoglia=*off   ;
161500131213           IndLna9988=*off ;
161600080703
161700081007           // Se riga di categoria accendo solo protezione e sottolineatura
161800130821          if v1himm='S'    ;
161900130820           Immiss  =*on    ;
162000130820          endif            ;
162100130829
162200130829          // Protezine per linea di arrivo non prevista
162300130829          // Sempre protezione per interrogazione
162400130829          if v1himm='E'    or orariint=*on  ;
162500130829           Protriga=*on  ;
162600130821          endif            ;
162700130829          if v1himm='E' ;
162800130829           Protlna=*off ;
162900130829          endif            ;
163000130829
163100130828          if v1htser='R'  ;
163200130828          HI_dlna=*on  ;
163300130828          else  ;
163400130828          HI_dlna=*off ;
163500130828          endif  ;
163600131003
163700131010          // Se tipo servizio  ritiro non faccio vedere il tempo organizz
163800131003          if v1htser='R'  ;
163900131003          prot_tempo=*on  ;
164000131003          else ;
164100131003          prot_tempo=*off ;
164200131003          endif  ;
164300130821
164400130829          if CAPvisual= 'S'  ;
164500130829          vis_rifer=*on  ;
164600130829          endif  ;
164700131213
164800131213       // Se il record soglia c'è disabilito F9
164900131216       if v1csogl='Pom' ;
165000131216       if CAPrifer='S' or dsp_aid = c_F09 or AbilF9=*off ;
165100131213        abilF9=*off ;
165200131213        protsoglia=*off   ;
165300131213       else   ;
165400131213        abilF9=*on  ;
165500131213        protsoglia=*on    ;
165600131213       endif   ;
165700131213       endif   ;
165800131010
165900131213          //if v1clna=999  ;
166000131213          // indlna999=*on  ;
166100131213          //endif  ;
166200130829
166300130829          // Evidenzio i campi diversi dal riferimento
166400131003          // solo se sono a livello di CAP/località
166500131003          // non per filiale o standard
166600131003          if do0cap<>*blanks    ;
166700131003
166800130829          if v1costis<>capostis ;
166900130829           BL_ostis=*on ;
167000130829          endif  ;
167100130829          if v1costfs<>capostfs ;
167200130829           BL_ostfs=*on ;
167300130829          endif  ;
167400131016          if v1comiis<>capomiis ;
167500131016           BL_omiis=*on ;
167600131016          endif  ;
167700131016          if v1comxfs<>capomxfs ;
167800131016           BL_omxfs=*on ;
167900131016          endif  ;
168000131010          if v1colrsc<>capolrsc ;
168100131010           BL_olrsc=*on ;
168200131010          endif  ;
168300131010          if v1colrnc<>capolrnc ;
168400131010           BL_olrnc=*on ;
168500131010          endif  ;
168600131010          if v1colrnk<>capolrnk ;
168700131010           BL_olrnk=*on ;
168800131010          endif  ;
168900131213          if v1ctrzs <>captrzs  ;
169000131213           BL_trzs =*on ;
169100131213          endif  ;
169200131010
169300131010
169400131003          endif  ;
169500130829
169600080702          ENDSR    ;
169700080702       //--------------------------------------------------------------
169800080702       //?conferma aggiornamento
169900080702       //--------------------------------------------------------------
170000080702        BEGSR ConfAGGIO              ;
170100081003
170200080704       s01nrr=1  ;
170300130820       chain s01nrr    sio2s01    ;
170400080704
170500080704 1     dow %found    ;
170600130821
170700130821 2     if  v1clna>0  ;
170800130821
170900140116       // Elaboro record soglia solo se
171000140116 2a    if v1csogl=*blanks or (abilF9=*off or caprifer='S')   ;
171100140116       Klna=v1hlna  ;
171200130828       ktser=v1htser  ;
171300131216       ksoglia=%subst(v1csogl:1:1)  ;
171400130829
171500131216       // Se la linea on è prevista--> la deleto
171600130829 3     if v1himm='E'   ;
171700131216         delete (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
171800130829 x3    else    ;
171900130829
172000131216         chain  (do0ver:do0nar:do0cap:do0loc:klna:ktser:ksoglia)  azcos03l   ;
172100130829       // se il record  è diventato uguale al record di riferimento
172200130829       //  lo deleto ma non per filiale
172300130829  4       if  CAPostis= v1costis   and
172400130828              CAPostfs= v1costfs   and
172500131016              CAPomiis= v1comiis   and
172600131016              CAPomxfs= v1comxfs   and
172700131010              CAPolrsc= v1colrsc   and
172800131010              CAPolrnc= v1colrnc   and
172900131010              CAPolrnk= v1colrnk   and
173000131216              CAPtrzs = v1ctrzs    and
173100130829              do0cap<>*blanks           ;
173200131213  5           if %found(azcos03l)  ;
173300130821               delete azcos000   ;
173400130829  5           endif  ;
173500130829  x4        else  ;
173600131213  5           if not %found(azcos03l)  ;
173700130821              clear azcos000  ;
173800130821              cosver=do0ver ;
173900130821              cosnar=do0nar ;
174000130821              coscap=do0cap ;
174100130821              cosloc=do0loc ;
174200130829  5           endif  ;
174300130821              cosostis= v1costis      ;
174400130821              cosostfs= v1costfs      ;
174500131016              cosomiis= v1comiis      ;
174600131016              cosomxfs= v1comxfs      ;
174700131010              cosolrsc= v1colrsc   ;
174800131010              cosolrnc= v1colrnc   ;
174900131010              cosolrnk= v1colrnk   ;
175000130821              costrzs = v1ctrzs         ;
175100130821              cosute  =knmus       ;
175200130821              cosduv=%dec(datasys)  ;
175300130821              costser=ktser  ;
175400130821              coslna=v1clna  ;
175500131216              cosserv=ksoglia ;
175600131213  5           if %found(azcos03l)  ;
175700130821               update azcos000   ;
175800130821               else  ;
175900130821               write  azcos000   ;
176000130829  5           endif  ;
176100130829  4        endif  ;
176200130829  3        endif  ;
176300140116 2a      endif  ;
176400140116 2       endif  ;
176500080704
176600080704        s01nrr=s01nrr+1 ;
176700130820        chain s01nrr    sio2s01    ;
176800080704  1     enddo           ;
176900080704
177000080702        ENDSR;
177100131031       //--------------------------------------------------------------
177200131031       //?Caricamento orario più basso del tipo tempo consegna
177300131031       //--------------------------------------------------------------
177400131031        BEGSR car_cpt                ;
177500131031        clear wcpt0in;
177600131031        clear wcpt0in4;
177700131031        clear wdtsp  ;
177800131031        clear wtsp   ;
177900131031        setll (do0ver:wttc) azcpt01l   ;
178000131031        reade (do0ver:wttc) azcpt01l   ;
178100131031        dow not %eof(azcpt01l);
178200131031           if cpt0in<wcpt0in or wcpt0in=0;
178300131031              wcpt0in=cpt0in;
178400131031              wtsp   =cpttsp;
178500131031           endif;
178600131031           reade (do0ver:wttc) azcpt01l   ;
178700131031        enddo;
178800131031        // decodifico il tipo servizio memorizzato
178900131031        clear ds5e;
179000131031        chain (1:'5E':wtsp) tabel00f;
179100131031        if %found(tabel00f);
179200131031           ds5e=tbluni;
179300131031        endif;
179400131031        wdtsp=§5ed08;
179500131031        wcpt0in4=wcpt0in*100;
179600131031        endsr;
179700080702
179800080206       //--------------------------------------------------------------
179900080206       //?Operazioni finali.
180000080206       //--------------------------------------------------------------
180100080206       BEGSR RoutEnd;
180200080627
180300080704         // Chiusura pgm   ;
180400131003         // clear tibs02ds  ;
180500131003         // t02tla='C'      ;
180600131003         // TNTBE_RicercaControllo  (kpjba : tibs02ds);
180700080704
180800080206         *inLR = *on;
180900080704
181000080206         return;
181100080206
181200080206       ENDSR;
181300080206
181400131003
181500080206      /end-free
181600131003     C**************************************************************************
181700131003     C     *inzsr        BEGSR
181800131003      /free
181900131003        // Carico filiali di test abilitate
182000131003         clear TIBS02ds;
182100131003         T02Mod = 'C';
182200131003         T02sif = knsif;
182300131003         T02cod = 'VPO';
182400131003         t02tla = 'L'   ;
182500131003         T02ke1 ='DECOFI816' ;
182600131003         TNTBE_RicercaControllo  (kpjba : tibs02ds);
182700131003
182800131003         if  T02err  = *blank;
182900131003         xx= 1 ;
183000131003         yy= 1 ;
183100131003
183200131003         dow xx< 256   ;
183300131003           fil_ab(yy)= %subst(t02uni:xx:3)  ;
183400131003           yy=yy+ 1   ;
183500131003           xx=xx+ 3   ;
183600131003         enddo  ;
183700131003
183800131003         endif;
183900131003
184000131003         ENDSR;
184100131003      /end-free
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
