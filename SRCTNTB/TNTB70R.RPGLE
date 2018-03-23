000100080206      //--------------------------------------------------------------
000200081007      //?tntb70R - riceca e scelta sottotipo info commerciale
000300080206      //--------------------------------------------------------------
000400080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500080206      //---------------------------------------------------------------
000600080206      //?Dichiarazione file.
000700080206      //---------------------------------------------------------------
000800080604     fTNTBE01l  if   e           k disk
000900081007     ftntb70D   cf   e             workstn indds(IndDspF)
001000080206     f                                     infds(InfDspF)
001100081008     f                                     sfile(tb70S01 : S01nrr)
001200080206      //---------------------------------------------------------------
001300080207      // - Tasti funzionali a video
001400080207     d c_F01           c                   const(x'31')
001500080207     d c_F02           c                   const(x'32')
001600080207     d c_F03           c                   const(x'33')
001700080207     d c_F05           c                   const(x'35')
001800080207     d c_F06           c                   const(x'36')
001900080207     d c_F07           c                   const(x'37')
002000080207     d c_F08           c                   const(x'38')
002100080207     d c_F09           c                   const(x'39')
002200080207     d c_F10           c                   const(x'3A')
002300080207     d c_F12           c                   const(x'3C')
002400080207     d c_F13           c                   const(x'B1')
002500080207     d c_F14           c                   const(x'B2')
002600080207     d c_F15           c                   const(x'B3')
002700080207     d c_F16           c                   const(x'B4')
002800080207     d c_F17           c                   const(x'B5')
002900080207     d c_F18           c                   const(x'B6')
003000080207     d c_F19           c                   const(x'B7')
003100080207     d c_F20           c                   const(x'B8')
003200080207     d c_F21           c                   const(x'B9')
003300080207     d c_F22           c                   const(x'BA')
003400080207     d c_F23           c                   const(x'BB')
003500080207     d c_F24           c                   const(x'BC')
003600080207     d c_Enter         c                   const(x'F1')
003700080207     d c_RollDown      c                   const(x'F4')
003800080207     d c_RollUp        c                   const(x'F5')
003900080214
004000080214      // - Attributi di visualizzazione
004100080215      //  -  DspAtr() - Normale
004200080214     d C_dspatr_Norm   c                   const(x'20')
004300080215      //  -  DspAtr(RI)
004400080214     d C_dspatr_RI     c                   const(x'21')
004500080215      //  -  DspAtr(ND)
004600080214     d C_dspatr_ND     c                   const(x'27')
004700080215      //  -  DspAtr(BL) / Color(Red)
004800080214     d C_dspatr_BL     c                   const(x'28')
004900080206
005000080206      //---------------------------------------------------------------
005100080206      //?Definizione schiere.
005200080206      //---------------------------------------------------------------
005300080206      // - Messaggi di errore
005400081008     d MSG             s             78    dim(01) ctdata perrcd(1)
005500080206      //---------------------------------------------------------------
005600080206      //?Definizione aree dati.
005700080206      //---------------------------------------------------------------
005800080206      // - Dati utente
005900080206     d 你zUte        e ds                  extname(AZUTE00F)
006000080206     d                                     dtaara
006100080206     d 伶atiUte      e ds                  extname(dDatiUte)
006200080206     d                                     dtaara
006300080206      //---------------------------------------------------------------
006400080206      //?Definizione strutture dati.
006500080206      //---------------------------------------------------------------
006600080206      // - Status
006700080206     d Psds           sds
006800080206     d   SDSpgm          *proc
006900080206
007000080206      // - InfDS
007100080206     d InfDspF         ds
007200080207     d  dsp_aid              369    369a                                        AID byte
007300080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
007400080207     d  min_nrr              378    379i 0                                      Subfile min rrn
007500080207     d  num_rcds             380    381i 0                                      Subfile num rcds
007600080206
007700080206      // - Indicatori su DspF
007800080208     d IndDspF         ds
007900080206        // - Indicatori di gestione del subfile
008000080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008100080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
008200080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
008300080206     d  SflEnd                        1n   overlay(IndDspF : 33)
008400080206        // - Indicatori di errore
008500081023     d  PassatoPot                    1n   overlay(IndDspF : 01)
008600081127     d  ProtRiga                      1n   overlay(IndDspF : 02)
008700081023     d  errMessage                    1n   overlay(IndDspF : 28)
008800080606     d  errGenerico                   1n   overlay(IndDspF : 99)
008900080627        // - Indicatori vari
009000080206
009100080206      // - Parametri ricevuti
009200080206     d KPJBA         e ds
009300081007     d tntb70ds      e ds
009400080206
009500080206      // - Reperimento dati utente
009600080206     d TIBS34ds      e ds
009700080206     d dUte01        e ds
009800080702
009900081006     d Difs          e ds
010000090511     d Difo          e ds
010100080206
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione variabili globali.
010400080206      //---------------------------------------------------------------
010500080206
010600080206      // - Flags booleani
010700080208     d $Fine           s               n   inz(*off)
010800080208     d $InzS01         s               n   inz(*on)
010900080206
011000080627     d $Video          s              2    inz('S1')
011100080208     d S01nrr          s              4  0 inz
011200081008     d Primavolta      S              1
011300081127     d SAVtipo         S              1
011400081127     d tipo            S              1
011500090511     d kcod            S                   like(tbeke1)
011600090511     d
011700090511     D Cdesinfo        C                   CONST('Informazione  Commerciale :')
011800090511     D Cdescat         C                   CONST('Categoria Info Commerciale:')
011900080206      //---------------------------------------------------------------
012000080206      //?Definizione procedure usate.
012100080206      //---------------------------------------------------------------
012200080414      /copy gaitrasrc/srcprotopr,tibs34r
012300080206      //---------------------------------------------------------------
012400080206      //?Riepilogo indicatori.
012500080206      //---------------------------------------------------------------
012600080207      // - Comuni
012700080207      // 28    : Emissione messaggio di errore a video
012800080207      // - C01/S01
012900080207      // 30    : Condiziona SFLDSP    (*not)
013000080207      // 31    : Condiziona SFLDSPCTL (*not)
013100080207      // 30+31 : Condiziona SFLCLR
013200080207      // 32    : Condiziona SFLNXTCHG in S01
013300080207      // - D01
013400080207      // - Comuni
013500080207      // 99    : Generico di Errore
013600080206      //---------------------------------------------------------------
013700080206
013800080206      //---------------------------------------------------------------
013900080206      //?M A I N - L I N E
014000080206      //---------------------------------------------------------------
014100080206
014200080627     c     *Entry        plist
014300080206     c                   parm                    KPJBA
014400081007     C                   PARM                    tntb70ds
014500080627
014600081008     C                   CLEAR                   Otb70spf
014700081008     C                   CLEAR                   otb70dspf
014800080702
014900080206      /free
015000080206
015100081007        if  itb70tla<>'C'   ;
015200080925
015300080206       // Operazioni iniziali
015400080206       exsr RoutInz;
015500080206
015600080206       // Gestione video
015700080206       DOW $Fine = *off;
015800080206         select;
015900080530
016000080530         // Videata di selezioni
016100080627           when $Video = 'S1';
016200080627             exsr GesS01;
016300080530
016400080702           other   ;
016500080206             $Fine = *on;
016600080206         endsl;
016700080206       ENDDO;
016800080925
016900080925       endif    ;
017000080206
017100080206       // Operazioni finali
017200080206       exsr RoutEnd;
017300080206
017400080206       //--------------------------------------------------------------
017500080206       //?Operazioni iniziali.
017600080206       //--------------------------------------------------------------
017700080206       BEGSR RoutInz;
017800080627       $inzs01=*on;
017900081127       $Fine=*off   ;
018000080206
018100080703       // Solo la prima volta
018200080703 1     if primavolta=' '   ;
018300080703
018400080206         // Reperimento dati job
018500080206         exsr DatiJob;
018600080627
018700080703         // IMPOSTO IL potenziale in visualizzazione
018800081023         if itb70cpo>0   ;
018900081023         PassatoPot=*on    ;
019000081007         vsccpo=%editc(itb70cpo:'X')   ;
019100081007         vscrag=itb70rag   ;
019200081023         endif     ;
019300081007
019400090511         // SELEZIONE per info commerciale
019500090511 2a      if itb70tpf<>*blanks      ;
019600090511           vsctpf=itb70tpf   ;
019700090511           vsctipo=cdesinfo  ;
019800090511           KCOD='IFS'        ;
019900090511 2         if itb70dtpf<>*blanks  ;
020000090511           vsdtpf=itb70dtpf  ;
020100090511 x2        else              ;
020200090511           clear tbeke1 ;
020300090511           tbeke1=itb70tpf   ;
020400090511           chain    ('IFO':tbeke1) tntbe01l    ;
020500090511 3           if %found(tntbe01l)   ;
020600090511             vsdtpf=tbeuni         ;
020700090511             else                  ;
020800090511             vsdtpf=*all'?'        ;
020900090511 3           endif                 ;
021000090511 2       endif                 ;
021100090511 x2a     else   ;
021200090511
021300090511         // SELEZIONE per categoria commerciale
021400090511           vsctipo=cdescat   ;
021500090511           vsctpf=itb70ifg   ;
021600090511           KCOD='IFO'        ;
021700090511 2         if itb70dtpf<>*blanks  ;
021800090511           vsdtpf=itb70difg  ;
021900090511 x2        else              ;
022000090511           clear tbeke1 ;
022100090511           tbeke1=itb70ifg   ;
022200090511           chain    ('IFG':tbeke1) tntbe01l    ;
022300090511 3           if %found(tntbe01l)   ;
022400090511             vsdtpf=tbeuni         ;
022500090511             else                  ;
022600090511             vsdtpf=*all'?'        ;
022700090511 3           endif                 ;
022800090511 2       endif                 ;
022900090511 2a      endif                 ;
023000090511
023100081008 1       endif                 ;
023200080703
023300080627         ENDSR;
023400080206       //--------------------------------------------------------------
023500080206       //?Reperimento Dati del job (Utente/Operativi).
023600080206       //--------------------------------------------------------------
023700080206       BEGSR DatiJob;
023800080206
023900080206         in(E) 你zUte;
024000080206         if NOT %error;
024100080206           in(E) 伶atiUte;
024200080206         endif;
024300080206         if %error or RSut = *blanks;
024400080206           clear TIBS34ds;
024500080206           tibs34r(tibs34ds);
024600080206           in 你zUte;
024700080206           in 伶atiUte;
024800080206         endif;
024900080206
025000080206       ENDSR;
025100080206
025200080206       //--------------------------------------------------------------
025300080627       //?Gestione SFL 01
025400080206       //--------------------------------------------------------------
025500080409       BEGSR GesS01;
025600080207
025700080207         // Inizializzazione videata
025800080409         if  $InzS01 = *on;
025900080409            exsr InzS01;
026000081008
026100081008          if s01nrr>0  ;
026200080703           sfldsp_n=*off;
026300081008
026400081008         // Posizionamento cursore
026500081127          if itb70tpf='MOI'   ;
026600081127            C01csr = 2;
026700081127           s01nrr = 2;
026800081127           else   ;
026900081127            C01csr = 1;
027000081127           s01nrr = 1;
027100081008           endif     ;
027200081127           endif     ;
027300081008
027400080409            $InzS01  = *off;
027500080207         endif;
027600080207
027700080207
027800080207         // Emissione Testata e Piede con tasti funzionali abilitati
027900080207         if  errGenerico = *off;
028000081008           write  tb70Z01;
028100081008         if s01nrr=0    ;
028200081008           write  tb70D02;
028300081008         endif ;
028400080207         endif;
028500080703
028600080703         // Posizionamento cursore
028700080703         if  C01csr  > *zero;
028800080703           C01rcd = C01csr;
028900080703         endif  ;
029000080207
029100080207         // Emissione videata
029200081008         exfmt  TB70C01;
029300080410
029400080207         reset errMessage;
029500080207         reset errGenerico;
029600080702         clear Vscmsg;
029700080207
029800080523 1       SELECT;
029900080207
030000080207         // - F3=Fine
030100080523 1         WHEN  dsp_aid = c_F03;
030200080409            $Fine = *on;
030300080207
030400080207         // - F12=Ritorno
030500080523 1         WHEN  dsp_aid = c_F12;
030600080702            $Fine = *on;
030700080414
030800081008 x1      // Invio
030900080207           OTHER;
031000081008
031100081008 2         if s01nrr>0      ;
031200081008           readc tb70s01   ;
031300081008 3         dow not %eof    ;
031400081008
031500081008 4         if vscsce='1'   ;
031600081008           otb70spf=vscspf  ;
031700081008           otb70dspf=vscdes  ;
031800081008           $Fine = *on;
031900081008           leave            ;
032000081008 x4        else            ;
032100081008           readc tb70s01   ;
032200081008 4         endif           ;
032300081008 3         enddo           ;
032400081008
032500081008           // Errore se non effettuata nemmeno una scelta
032600081008 3         if otb70spf=*blanks ;
032700081008            errGenerico = *on;
032800081008            errMessage  = *on;
032900081008            s01nrr=1         ;
033000081008            vscmsg=msg(1)    ;
033100081008 3         endif               ;
033200081008 2         endif               ;
033300081008
033400080523 1       ENDSL;
033500080207
033600080207       ENDSR;
033700080207       //--------------------------------------------------------------
033800080627       //?Inizializzazione SFL01
033900080207       //--------------------------------------------------------------
034000080409       BEGSR InzS01;
034100080207
034200080207       // Pulizia subfile
034300080207         SflDsp_N    = *on;
034400080207         SflDspCtl_N = *on;
034500081008         write  TB70C01;
034600080207         SflDspCtl_N = *off;
034700080207         SflEnd      = *off;
034800080530
034900080530         clear C01rcd;
035000080627         clear C01csr;
035100080627         S01nrr=0 ;
035200080702         clear Vscmsg;
035300080207         errMessage  = *off;
035400080207         errGenerico = *off;
035500081127         Protriga    = *off;
035600081008         $Fine=*off   ;
035700080409
035800081008       // Carico I SOTTOTIPO INFO
035900090511 1       if kcod='IFS'   ;
036000090511
036100090511         clear  tbeke1   ;
036200081008         tbeke1=itb70tpf   ;
036300081008
036400081008         clear s01nrr      ;
036500081127         clear savtipo     ;
036600081008
036700090511         setll (kcod :tbeke1) tntbe01l   ;
036800090511         READE (kcod :tbeke1) tntbe01l   ;
036900081008
037000090511 2       dow not %eof(tntbe01l)         ;
037100090511 3       if tbeatb=' '  ;
037200081127
037300081127          // SE NECESSARIA AGGIUNGO RIGA DI SEPARAZIONE  per MOI
037400081127          if itb70tpf='MOI'   ;
037500081127            tipo= %subst(tbeke2:1:1)         ;
037600081127            if tipo<>savtipo   ;
037700081127              exsr CambiaMOI                   ;
037800081127              savtipo=tipo    ;
037900081127            endif    ;
038000081127          else    ;
038100081127            clear  savtipo  ;
038200081127          endif    ;
038300081127
038400090511         clear vscsce  ;
038500081008         dIFS=tbeuni    ;
038600081008         vscspf=tbeke2 ;
038700081008         vscdes=告fsdes;
038800081008         vscdel=告fsdel;
038900081008
039000081008         s01nrr=s01nrr+1 ;
039100081008         write tb70s01   ;
039200090511 3       endif        ;
039300081008
039400090511         READE (kcod :tbeke1) tntbe01l   ;
039500090511
039600090511 2       enddo   ;
039700090511
039800090511 x1      else  ;
039900090511
040000090511       // Carico LE INFO COMMERCIALI
040100090511         clear  tbeke1   ;
040200090511
040300090511         clear s01nrr      ;
040400090511         clear savtipo     ;
040500090511
040600090511         CLEAR SAVTIPO            ;
040700090511         setll (kcod ) tntbe01l   ;
040800090511         READE (kcod ) tntbe01l   ;
040900090511
041000090511 2       dow not %eof(tntbe01l)         ;
041100090511 3       if tbeatb=' '  ;
041200090511
041300090511         clear vscsce  ;
041400090511         dIFo=tbeuni    ;
041500090511
041600090511 4       if 告foifg= itb70ifg  ;
041700090511
041800090511         vscspf=tbeke1 ;
041900090511         vscdes=告fodes;
042000090511
042100090511         s01nrr=s01nrr+1 ;
042200090511         write tb70s01   ;
042300090511 4       endif        ;
042400090511 3       endif        ;
042500090511
042600090511         READE (kcod ) tntbe01l   ;
042700090511 2       enddo   ;
042800090511 1       endif        ;
042900080702         ENDSR    ;
043000080206       //--------------------------------------------------------------
043100080206       //?Operazioni finali.
043200080206       //--------------------------------------------------------------
043300080206       BEGSR RoutEnd;
043400080627
043500081008         if itb70tla<>' '    ;
043600080704         // Chiusura pgm   ;
043700080206         *inLR = *on;
043800080704         endif             ;
043900080704
044000080206         return;
044100080206
044200080206       ENDSR;
044300081127       //--------------------------------------------------------------
044400081127       //?Solo per MOI  lascio riga con scritta ita o est o bianca
044500081127       //--------------------------------------------------------------
044600081127       BEGSR CambiaMOI   ;
044700081127
044800081127        Protriga = *on      ;
044900081127        select   ;
045000081127        when tipo='0'         ;
045100081127        vscdes='----  Motivi ITALIA  ----'   ;
045200081127        when tipo='1'         ;
045300081127        vscdes='----  Motivi ESTERO  ----'   ;
045400081127        OTHER  ;
045500081127        vscdes='----  ALTRI  Motivi  ----'   ;
045600081127        ENDSL   ;
045700081127
045800081127        CLEAR vscdel    ;
045900081127        CLEAR vscSCE    ;
046000081127        CLEAR vscspf    ;
046100081127
046200081127        s01nrr=s01nrr+1 ;
046300081127        write tb70s01   ;
046400081127
046500081127        Protriga = *off     ;
046600081127
046700081127       ENDSR;
046800080206
046900080206      /end-free
047000080206
047100080206       //--------------------------------------------------------------
047200080206       //?Schiere a tempo di compilazione.
047300080206       //--------------------------------------------------------------
047400080206
047500080410** - MSG ------------------------------------------------------------------ -*
047600081008Effettuare una scelta                                                           01
