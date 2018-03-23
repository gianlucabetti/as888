000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300100903      //?TNSDUPR - sblocca data area pgm immissione matriclle per
000400100903      //                       inVII GIORNALIERI UNITÀ EDP
000500080206      //--------------------------------------------------------------
000600080206
000700080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000800080206
000900080206      //---------------------------------------------------------------
001000080206      //?Dichiarazione file.
001100080206      //---------------------------------------------------------------
001200050704
001300100903     fTNSDUPD   cf   e             workstn indds(IndDspF)
001400080206     f                                     infds(InfDspF)
001500080206      //---------------------------------------------------------------
001600080207      // - Tasti funzionali a video
001700080207     d c_F01           c                   const(x'31')
001800080207     d c_F02           c                   const(x'32')
001900080207     d c_F03           c                   const(x'33')
002000080207     d c_F05           c                   const(x'35')
002100080207     d c_F06           c                   const(x'36')
002200080207     d c_F07           c                   const(x'37')
002300080207     d c_F08           c                   const(x'38')
002400080207     d c_F09           c                   const(x'39')
002500080207     d c_F10           c                   const(x'3A')
002600080207     d c_F12           c                   const(x'3C')
002700080207     d c_F13           c                   const(x'B1')
002800080207     d c_F14           c                   const(x'B2')
002900080207     d c_F15           c                   const(x'B3')
003000080207     d c_F16           c                   const(x'B4')
003100080207     d c_F17           c                   const(x'B5')
003200080207     d c_F18           c                   const(x'B6')
003300080207     d c_F19           c                   const(x'B7')
003400080207     d c_F20           c                   const(x'B8')
003500080207     d c_F21           c                   const(x'B9')
003600080207     d c_F22           c                   const(x'BA')
003700080207     d c_F23           c                   const(x'BB')
003800080207     d c_F24           c                   const(x'BC')
003900080207     d c_Enter         c                   const(x'F1')
004000080207     d c_RollDown      c                   const(x'F4')
004100080207     d c_RollUp        c                   const(x'F5')
004200080214
004300080214      // - Attributi di visualizzazione
004400080215      //  -  DspAtr() - Normale
004500080214     d C_dspatr_Norm   c                   const(x'20')
004600080215      //  -  DspAtr(RI)
004700080214     d C_dspatr_RI     c                   const(x'21')
004800080215      //  -  DspAtr(ND)
004900080214     d C_dspatr_ND     c                   const(x'27')
005000080215      //  -  DspAtr(BL) / Color(Red)
005100080214     d C_dspatr_BL     c                   const(x'28')
005200080206
005300080206      //---------------------------------------------------------------
005400080206      //?Definizione schiere.
005500080206      //---------------------------------------------------------------
005600100907     d MSG             s             78    dim(01) ctdata perrcd(1)
005700080206      //---------------------------------------------------------------
005800080206      //?Definizione aree dati.
005900080206      //---------------------------------------------------------------
006000080206      // - Dati utente
006100080206     d §AzUte        e ds                  extname(AZUTE00F)
006200080206     d                                     dtaara
006300080206     d §DatiUte      e ds                  extname(dDatiUte)
006400080206     d                                     dtaara
006500100903     d TNSDUO        e ds                  extname(TNSDUODS)
006600100903     d                                     dtaara
006700080206
006800080206      //---------------------------------------------------------------
006900080206      //?Definizione strutture dati.
007000080206      //---------------------------------------------------------------
007100080206      // - Status
007200080206     d Psds           sds
007300080206     d   SDSpgm          *proc
007400080206
007500080206      // - InfDS
007600080206     d InfDspF         ds
007700080207     d  dsp_aid              369    369a                                        AID byte
007800080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
007900080207     d  min_nrr              378    379i 0                                      Subfile min rrn
008000080207     d  num_rcds             380    381i 0                                      Subfile num rcds
008100080206
008200080206      // - Indicatori su DspF
008300080208     d IndDspF         ds
008400080206        // - Indicatori di errore
008500080206     d  errMessage                    1n   overlay(IndDspF : 28)
008600080606     d  errGenerico                   1n   overlay(IndDspF : 99)
008700080627        // - Indicatori vari
008800100903     d  EmisSBLO                      1n   overlay(IndDspF : 01)
008900100315     d
009000080206
009100080206      // - Reperimento dati utente
009200080206     d TIBS34ds      e ds
009300080206     d dUte01        e ds
009400080702     d DLAT          e ds
009500080702
009600100901     d tnsdulds      e ds
009700100902     d KPJBA         e ds
009800080206
009900080206      // - Ricerca/Controllo tabelle
010000080206     d TIBS02ds      e ds                  inz
010100100316
010200100316
010300080206      //---------------------------------------------------------------
010400080206      //?Definizione variabili globali.
010500080206      //---------------------------------------------------------------
010600080206
010700080206      // - Flags booleani
010800080208     d $Fine           s               n   inz(*off)
010900080206
011000100901     d $Inzd01         s               n   inz(*on)
011100100312     d $Video          s              2    inz('D1')
011200100901     d Indx            s              3  0 inz
011300080414
011400100315     d Dataeur         s               d   datfmt(*eur)
011500100315     d Dataiso         s               d   datfmt(*iso)
011600080605     d Dataymd         s               d   datfmt(*ymd)
011700080605     d Datadmy         s               d   datfmt(*dmy)
011800100901     d Datasys         s               d   inz(*sys)
011900080604     d
012000081009     d
012100080206      //---------------------------------------------------------------
012200080206      //?Definizione procedure usate.
012300080206      //---------------------------------------------------------------
012400080414      /copy gaitrasrc/srcprotopr,tibs34r
012500080206
012600080206      //---------------------------------------------------------------
012700080206      //?Riepilogo indicatori.
012800080206      //---------------------------------------------------------------
012900080207      // - Comuni
013000080207      // 28    : Emissione messaggio di errore a video
013100080207      // - C01/S01
013200080207      // 30    : Condiziona SFLDSP    (*not)
013300080207      // 31    : Condiziona SFLDSPCTL (*not)
013400080207      // 30+31 : Condiziona SFLCLR
013500100824      // 32    : Condiziona SFLNXTCHG
013600080207      // 50    : Errore: Opzione errata
013700080207      // - D01
013800080207      // - Comuni
013900080207      // 99    : Generico di Errore
014000080206      //---------------------------------------------------------------
014100080206
014200080206      //---------------------------------------------------------------
014300080206      //?M A I N - L I N E
014400080206      //---------------------------------------------------------------
014500080206
014600080627     c     *Entry        plist
014700080206     c                   parm                    KPJBA
014800080702
014900080206      /free
015000080206
015100080206       // Operazioni iniziali
015200080206       exsr RoutInz;
015300081215
015400080206       // Gestione video
015500081215 3     DOW $Fine = *off;
015600081215 4       select;
015700080530
015800080530         // Videata di selezioni
015900100312           when $Video = 'D1';
016000100312             exsr GesD01;
016100080530
016200080702           other   ;
016300080206             $Fine = *on;
016400081215 4       endsl;
016500081215 3     ENDDO;
016600080206
016700080206       // Operazioni finali
016800080206       exsr RoutEnd;
016900080206
017000080206       //--------------------------------------------------------------
017100080206       //?Operazioni iniziali.
017200080206       //--------------------------------------------------------------
017300080206       BEGSR RoutInz;
017400100312       $inzd01=*on;
017500080703
017600080206         // Reperimento dati job
017700080206         exsr DatiJob;
017800100903         vscpgm='TNSDUPR'         ;
017900080703
018000080627         ENDSR;
018100080206
018200080206       //--------------------------------------------------------------
018300080206       //?Reperimento Dati del job (Utente/Operativi).
018400080206       //--------------------------------------------------------------
018500080206       BEGSR DatiJob;
018600080206
018700080206         in(E) §AzUte;
018800080206         if NOT %error;
018900080206           in(E) §DatiUte;
019000080206         endif;
019100080206         if %error or RSut = *blanks;
019200080206           clear TIBS34ds;
019300080206           tibs34r(tibs34ds);
019400080206           in §AzUte;
019500080206           in §DatiUte;
019600080206         endif;
019700080206
019800080206       ENDSR;
019900100901       //--------------------------------------------------------------
020000100901       //?Inizializzazione videata 01
020100100901       //--------------------------------------------------------------
020200100901       BEGSR Inzd01  ;
020300100901
020400100901         clear V1cmsg;
020500100901         errMessage  = *off;
020600100901         errGenerico = *off;
020700100903         in *lock   tnsduo ;
020800100903         if §sduodat=0 ;
020900100903         clear v1cdat  ;
021000100903         else                   ;
021100100903         dataiso=%date(§sduodat)  ;
021200100903         dataeur=dataiso  ;
021300100903         v1cdat=%dec(dataeur)  ;
021400100903         endif    ;
021500100903
021600100903         v1cute=§sduoute        ;
021700100907         v1cpgm=§sduopgm         ;
021800100903         v1csblo='NO'  ;
021900100901
022000100901         ENDSR    ;
022100100312       //--------------------------------------------------------------
022200100312       //?Gestione viodeata 01
022300100312       //--------------------------------------------------------------
022400100312       BEGSR Gesd01;
022500100312
022600100312         // Inizializzazione videata
022700100312         if  $Inzd01 = *on;
022800100312            exsr Inzd01;
022900100312            $Inzd01  = *off;
023000100312         endif;
023100100312
023200100312
023300100312         // Emissione Testata e Piede con tasti funzionali abilitati
023400100824         if  errMessage  = *off;
023500100903           write  SDUpT01;
023600100312         endif;
023700100312
023800100312         // Emissione videata
023900100903         exfmt  SDUpD01;
024000100312
024100100312         reset errMessage;
024200100312         reset errGenerico;
024300100903         clear V1cmsg    ;
024400100312
024500100312 1       SELECT;
024600100312
024700100312         // - F3=Fine
024800100312 1         WHEN  dsp_aid = c_F03;
024900100312            $Fine = *on;
025000100312
025100100824 x1      // Invio
025200100312           OTHER;
025300100316
025400100316           exsr CTRD01  ;
025500100901
025600100316
025700100901 2         if ErrGenerico=*ON    ;
025800100316                 leavesr;
025900100316 2             endif;
026000100901
026100100901 2         if    dsp_aid = c_F06;
026200100903 3         if v1csblo='SI'  and emisSBLO=*on  ;
026300100903         clear tnsduo  ;
026400100903         out tnsduo  ;
026500100903         $Fine = *on;
026600100903 3       endif ;
026700100903 2       endif ;
026800100312 1       ENDSL;
026900100312
027000100312       ENDSR;
027100100312
027200100316       //--------------------------------------------------------------
027300100316       //?controlli video 1
027400100316       //--------------------------------------------------------------
027500100316       BEGSR CTRD01  ;
027600100903
027700100907       // data area non bloccata
027800100907       if v1cpgm = *blanks  ;
027900100907       v1cmsg=msg(1)  ;
028000100907       errGenerico=*on  ;
028100100907       errMessage =*on  ;
028200100907       leavesr  ;
028300100907       endif  ;
028400100907
028500100903       if EmisSBLO=*off  ;
028600100903       EmisSblo=*on   ;
028700100903       errGenerico=*on  ;
028800100903       leavesr  ;
028900100903       endif  ;
029000100316
029100100903       if v1csblo ='NO' and  EmisSBLO=*on  ;
029200100903       EmisSBLO=*off  ;
029300100903       errGenerico=*on  ;
029400100903       leavesr  ;
029500100903       endif  ;
029600100708
029700100316       ENDSR  ;
029800080206       //--------------------------------------------------------------
029900080206       //?Operazioni finali.
030000080206       //--------------------------------------------------------------
030100080206       BEGSR RoutEnd;
030200080627
030300100903         unlock tnsduo  ;
030400080704
030500080206         *inLR = *on;
030600080704
030700080206         return;
030800080206
030900080206       ENDSR;
031000080206
031100080206      /end-free
031200100907**
031300100907Nessun blocco al momento attivo   !!
