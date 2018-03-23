000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300081007      //?tntb70R - riceca e scelta sottotipo info commerciale
000400080206      //--------------------------------------------------------------
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206      //---------------------------------------------------------------
000700080206      //?Dichiarazione file.
000800080206      //---------------------------------------------------------------
000900080604     fTNTBE01l  if   e           k disk
001000081007     ftntb70D   cf   e             workstn indds(IndDspF)
001100080206     f                                     infds(InfDspF)
001200081008     f                                     sfile(tb70S01 : S01nrr)
001300080206      //---------------------------------------------------------------
001400080207      // - Tasti funzionali a video
001500080207     d c_F01           c                   const(x'31')
001600080207     d c_F02           c                   const(x'32')
001700080207     d c_F03           c                   const(x'33')
001800080207     d c_F05           c                   const(x'35')
001900080207     d c_F06           c                   const(x'36')
002000080207     d c_F07           c                   const(x'37')
002100080207     d c_F08           c                   const(x'38')
002200080207     d c_F09           c                   const(x'39')
002300080207     d c_F10           c                   const(x'3A')
002400080207     d c_F12           c                   const(x'3C')
002500080207     d c_F13           c                   const(x'B1')
002600080207     d c_F14           c                   const(x'B2')
002700080207     d c_F15           c                   const(x'B3')
002800080207     d c_F16           c                   const(x'B4')
002900080207     d c_F17           c                   const(x'B5')
003000080207     d c_F18           c                   const(x'B6')
003100080207     d c_F19           c                   const(x'B7')
003200080207     d c_F20           c                   const(x'B8')
003300080207     d c_F21           c                   const(x'B9')
003400080207     d c_F22           c                   const(x'BA')
003500080207     d c_F23           c                   const(x'BB')
003600080207     d c_F24           c                   const(x'BC')
003700080207     d c_Enter         c                   const(x'F1')
003800080207     d c_RollDown      c                   const(x'F4')
003900080207     d c_RollUp        c                   const(x'F5')
004000080214
004100080214      // - Attributi di visualizzazione
004200080215      //  -  DspAtr() - Normale
004300080214     d C_dspatr_Norm   c                   const(x'20')
004400080215      //  -  DspAtr(RI)
004500080214     d C_dspatr_RI     c                   const(x'21')
004600080215      //  -  DspAtr(ND)
004700080214     d C_dspatr_ND     c                   const(x'27')
004800080215      //  -  DspAtr(BL) / Color(Red)
004900080214     d C_dspatr_BL     c                   const(x'28')
005000080206
005100080206      //---------------------------------------------------------------
005200080206      //?Definizione schiere.
005300080206      //---------------------------------------------------------------
005400080206      // - Messaggi di errore
005500081008     d MSG             s             78    dim(01) ctdata perrcd(1)
005600080206      //---------------------------------------------------------------
005700080206      //?Definizione aree dati.
005800080206      //---------------------------------------------------------------
005900080206      // - Dati utente
006000080206     d 你zUte        e ds                  extname(AZUTE00F)
006100080206     d                                     dtaara
006200080206     d 伶atiUte      e ds                  extname(dDatiUte)
006300080206     d                                     dtaara
006400080206      //---------------------------------------------------------------
006500080206      //?Definizione strutture dati.
006600080206      //---------------------------------------------------------------
006700080206      // - Status
006800080206     d Psds           sds
006900080206     d   SDSpgm          *proc
007000080206
007100080206      // - InfDS
007200080206     d InfDspF         ds
007300080207     d  dsp_aid              369    369a                                        AID byte
007400080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
007500080207     d  min_nrr              378    379i 0                                      Subfile min rrn
007600080207     d  num_rcds             380    381i 0                                      Subfile num rcds
007700080206
007800080206      // - Indicatori su DspF
007900080208     d IndDspF         ds
008000080206        // - Indicatori di gestione del subfile
008100080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008200080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
008300080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
008400080206     d  SflEnd                        1n   overlay(IndDspF : 33)
008500080206        // - Indicatori di errore
008600081023     d  PassatoPot                    1n   overlay(IndDspF : 01)
008700081127     d  ProtRiga                      1n   overlay(IndDspF : 02)
008800081023     d  errMessage                    1n   overlay(IndDspF : 28)
008900080606     d  errGenerico                   1n   overlay(IndDspF : 99)
009000080627        // - Indicatori vari
009100080206
009200080206      // - Parametri ricevuti
009300080206     d KPJBA         e ds
009400081007     d tntb70ds      e ds
009500080206
009600080206      // - Reperimento dati utente
009700080206     d TIBS34ds      e ds
009800080206     d dUte01        e ds
009900080702
010000081006     d Difs          e ds
010100090511     d Difo          e ds
010200080206
010300080206      //---------------------------------------------------------------
010400080206      //?Definizione variabili globali.
010500080206      //---------------------------------------------------------------
010600080206
010700080206      // - Flags booleani
010800080208     d $Fine           s               n   inz(*off)
010900080208     d $InzS01         s               n   inz(*on)
011000080206
011100080627     d $Video          s              2    inz('S1')
011200080208     d S01nrr          s              4  0 inz
011300081008     d Primavolta      S              1
011400081127     d SAVtipo         S              1
011500081127     d tipo            S              1
011600090511     d kcod            S                   like(tbeke1)
011700090511     d
011800090511     D Cdesinfo        C                   CONST('Informazione  Commerciale :')
011900090511     D Cdescat         C                   CONST('Categoria Info Commerciale:')
012000080206      //---------------------------------------------------------------
012100080206      //?Definizione procedure usate.
012200080206      //---------------------------------------------------------------
012300080414      /copy gaitrasrc/srcprotopr,tibs34r
012400080206      //---------------------------------------------------------------
012500080206      //?Riepilogo indicatori.
012600080206      //---------------------------------------------------------------
012700080207      // - Comuni
012800080207      // 28    : Emissione messaggio di errore a video
012900080207      // - C01/S01
013000080207      // 30    : Condiziona SFLDSP    (*not)
013100080207      // 31    : Condiziona SFLDSPCTL (*not)
013200080207      // 30+31 : Condiziona SFLCLR
013300080207      // 32    : Condiziona SFLNXTCHG in S01
013400080207      // - D01
013500080207      // - Comuni
013600080207      // 99    : Generico di Errore
013700080206      //---------------------------------------------------------------
013800080206
013900080206      //---------------------------------------------------------------
014000080206      //?M A I N - L I N E
014100080206      //---------------------------------------------------------------
014200080206
014300080627     c     *Entry        plist
014400080206     c                   parm                    KPJBA
014500081007     C                   PARM                    tntb70ds
014600080627
014700081008     C                   CLEAR                   Otb70spf
014800081008     C                   CLEAR                   otb70dspf
014900080702
015000080206      /free
015100080206
015200081007        if  itb70tla<>'C'   ;
015300080925
015400080206       // Operazioni iniziali
015500080206       exsr RoutInz;
015600080206
015700080206       // Gestione video
015800080206       DOW $Fine = *off;
015900080206         select;
016000080530
016100080530         // Videata di selezioni
016200080627           when $Video = 'S1';
016300080627             exsr GesS01;
016400080530
016500080702           other   ;
016600080206             $Fine = *on;
016700080206         endsl;
016800080206       ENDDO;
016900080925
017000080925       endif    ;
017100080206
017200080206       // Operazioni finali
017300080206       exsr RoutEnd;
017400080206
017500080206       //--------------------------------------------------------------
017600080206       //?Operazioni iniziali.
017700080206       //--------------------------------------------------------------
017800080206       BEGSR RoutInz;
017900080627       $inzs01=*on;
018000081127       $Fine=*off   ;
018100080206
018200080703       // Solo la prima volta
018300080703 1     if primavolta=' '   ;
018400080703
018500080206         // Reperimento dati job
018600080206         exsr DatiJob;
018700080627
018800080703         // IMPOSTO IL potenziale in visualizzazione
018900081023         if itb70cpo>0   ;
019000081023         PassatoPot=*on    ;
019100081007         vsccpo=%editc(itb70cpo:'X')   ;
019200081007         vscrag=itb70rag   ;
019300081023         endif     ;
019400081007
019500090511         // SELEZIONE per info commerciale
019600090511 2a      if itb70tpf<>*blanks      ;
019700090511           vsctpf=itb70tpf   ;
019800090511           vsctipo=cdesinfo  ;
019900090511           KCOD='IFS'        ;
020000090511 2         if itb70dtpf<>*blanks  ;
020100090511           vsdtpf=itb70dtpf  ;
020200090511 x2        else              ;
020300090511           clear tbeke1 ;
020400090511           tbeke1=itb70tpf   ;
020500090511           chain    ('IFO':tbeke1) tntbe01l    ;
020600090511 3           if %found(tntbe01l)   ;
020700090511             vsdtpf=tbeuni         ;
020800090511             else                  ;
020900090511             vsdtpf=*all'?'        ;
021000090511 3           endif                 ;
021100090511 2       endif                 ;
021200090511 x2a     else   ;
021300090511
021400090511         // SELEZIONE per categoria commerciale
021500090511           vsctipo=cdescat   ;
021600090511           vsctpf=itb70ifg   ;
021700090511           KCOD='IFO'        ;
021800090511 2         if itb70dtpf<>*blanks  ;
021900090511           vsdtpf=itb70difg  ;
022000090511 x2        else              ;
022100090511           clear tbeke1 ;
022200090511           tbeke1=itb70ifg   ;
022300090511           chain    ('IFG':tbeke1) tntbe01l    ;
022400090511 3           if %found(tntbe01l)   ;
022500090511             vsdtpf=tbeuni         ;
022600090511             else                  ;
022700090511             vsdtpf=*all'?'        ;
022800090511 3           endif                 ;
022900090511 2       endif                 ;
023000090511 2a      endif                 ;
023100090511
023200081008 1       endif                 ;
023300080703
023400080627         ENDSR;
023500080206       //--------------------------------------------------------------
023600080206       //?Reperimento Dati del job (Utente/Operativi).
023700080206       //--------------------------------------------------------------
023800080206       BEGSR DatiJob;
023900080206
024000080206         in(E) 你zUte;
024100080206         if NOT %error;
024200080206           in(E) 伶atiUte;
024300080206         endif;
024400080206         if %error or RSut = *blanks;
024500080206           clear TIBS34ds;
024600080206           tibs34r(tibs34ds);
024700080206           in 你zUte;
024800080206           in 伶atiUte;
024900080206         endif;
025000080206
025100080206       ENDSR;
025200080206
025300080206       //--------------------------------------------------------------
025400080627       //?Gestione SFL 01
025500080206       //--------------------------------------------------------------
025600080409       BEGSR GesS01;
025700080207
025800080207         // Inizializzazione videata
025900080409         if  $InzS01 = *on;
026000080409            exsr InzS01;
026100081008
026200081008          if s01nrr>0  ;
026300080703           sfldsp_n=*off;
026400081008
026500081008         // Posizionamento cursore
026600081127          if itb70tpf='MOI'   ;
026700081127            C01csr = 2;
026800081127           s01nrr = 2;
026900081127           else   ;
027000081127            C01csr = 1;
027100081127           s01nrr = 1;
027200081008           endif     ;
027300081127           endif     ;
027400081008
027500080409            $InzS01  = *off;
027600080207         endif;
027700080207
027800080207
027900080207         // Emissione Testata e Piede con tasti funzionali abilitati
028000080207         if  errGenerico = *off;
028100081008           write  tb70Z01;
028200081008         if s01nrr=0    ;
028300081008           write  tb70D02;
028400081008         endif ;
028500080207         endif;
028600080703
028700080703         // Posizionamento cursore
028800080703         if  C01csr  > *zero;
028900080703           C01rcd = C01csr;
029000080703         endif  ;
029100080207
029200080207         // Emissione videata
029300081008         exfmt  TB70C01;
029400080410
029500080207         reset errMessage;
029600080207         reset errGenerico;
029700080702         clear Vscmsg;
029800080207
029900080523 1       SELECT;
030000080207
030100080207         // - F3=Fine
030200080523 1         WHEN  dsp_aid = c_F03;
030300080409            $Fine = *on;
030400080207
030500080207         // - F12=Ritorno
030600080523 1         WHEN  dsp_aid = c_F12;
030700080702            $Fine = *on;
030800080414
030900081008 x1      // Invio
031000080207           OTHER;
031100081008
031200081008 2         if s01nrr>0      ;
031300081008           readc tb70s01   ;
031400081008 3         dow not %eof    ;
031500081008
031600081008 4         if vscsce='1'   ;
031700081008           otb70spf=vscspf  ;
031800081008           otb70dspf=vscdes  ;
031900081008           $Fine = *on;
032000081008           leave            ;
032100081008 x4        else            ;
032200081008           readc tb70s01   ;
032300081008 4         endif           ;
032400081008 3         enddo           ;
032500081008
032600081008           // Errore se non effettuata nemmeno una scelta
032700081008 3         if otb70spf=*blanks ;
032800081008            errGenerico = *on;
032900081008            errMessage  = *on;
033000081008            s01nrr=1         ;
033100081008            vscmsg=msg(1)    ;
033200081008 3         endif               ;
033300081008 2         endif               ;
033400081008
033500080523 1       ENDSL;
033600080207
033700080207       ENDSR;
033800080207       //--------------------------------------------------------------
033900080627       //?Inizializzazione SFL01
034000080207       //--------------------------------------------------------------
034100080409       BEGSR InzS01;
034200080207
034300080207       // Pulizia subfile
034400080207         SflDsp_N    = *on;
034500080207         SflDspCtl_N = *on;
034600081008         write  TB70C01;
034700080207         SflDspCtl_N = *off;
034800080207         SflEnd      = *off;
034900080530
035000080530         clear C01rcd;
035100080627         clear C01csr;
035200080627         S01nrr=0 ;
035300080702         clear Vscmsg;
035400080207         errMessage  = *off;
035500080207         errGenerico = *off;
035600081127         Protriga    = *off;
035700081008         $Fine=*off   ;
035800080409
035900081008       // Carico I SOTTOTIPO INFO
036000090511 1       if kcod='IFS'   ;
036100090511
036200090511         clear  tbeke1   ;
036300081008         tbeke1=itb70tpf   ;
036400081008
036500081008         clear s01nrr      ;
036600081127         clear savtipo     ;
036700081008
036800090511         setll (kcod :tbeke1) tntbe01l   ;
036900090511         READE (kcod :tbeke1) tntbe01l   ;
037000081008
037100090511 2       dow not %eof(tntbe01l)         ;
037200090511 3       if tbeatb=' '  ;
037300081127
037400081127          // SE NECESSARIA AGGIUNGO RIGA DI SEPARAZIONE  per MOI
037500081127          if itb70tpf='MOI'   ;
037600081127            tipo= %subst(tbeke2:1:1)         ;
037700081127            if tipo<>savtipo   ;
037800081127              exsr CambiaMOI                   ;
037900081127              savtipo=tipo    ;
038000081127            endif    ;
038100081127          else    ;
038200081127            clear  savtipo  ;
038300081127          endif    ;
038400081127
038500090511         clear vscsce  ;
038600081008         dIFS=tbeuni    ;
038700081008         vscspf=tbeke2 ;
038800081008         vscdes=告fsdes;
038900081008         vscdel=告fsdel;
039000081008
039100081008         s01nrr=s01nrr+1 ;
039200081008         write tb70s01   ;
039300090511 3       endif        ;
039400081008
039500090511         READE (kcod :tbeke1) tntbe01l   ;
039600090511
039700090511 2       enddo   ;
039800090511
039900090511 x1      else  ;
040000090511
040100090511       // Carico LE INFO COMMERCIALI
040200090511         clear  tbeke1   ;
040300090511
040400090511         clear s01nrr      ;
040500090511         clear savtipo     ;
040600090511
040700090511         CLEAR SAVTIPO            ;
040800090511         setll (kcod ) tntbe01l   ;
040900090511         READE (kcod ) tntbe01l   ;
041000090511
041100090511 2       dow not %eof(tntbe01l)         ;
041200090511 3       if tbeatb=' '  ;
041300090511
041400090511         clear vscsce  ;
041500090511         dIFo=tbeuni    ;
041600090511
041700090511 4       if 告foifg= itb70ifg  ;
041800090511
041900090511         vscspf=tbeke1 ;
042000090511         vscdes=告fodes;
042100090511
042200090511         s01nrr=s01nrr+1 ;
042300090511         write tb70s01   ;
042400090511 4       endif        ;
042500090511 3       endif        ;
042600090511
042700090511         READE (kcod ) tntbe01l   ;
042800090511 2       enddo   ;
042900090511 1       endif        ;
043000080702         ENDSR    ;
043100080206       //--------------------------------------------------------------
043200080206       //?Operazioni finali.
043300080206       //--------------------------------------------------------------
043400080206       BEGSR RoutEnd;
043500080627
043600081008         if itb70tla<>' '    ;
043700080704         // Chiusura pgm   ;
043800080206         *inLR = *on;
043900080704         endif             ;
044000080704
044100080206         return;
044200080206
044300080206       ENDSR;
044400081127       //--------------------------------------------------------------
044500081127       //?Solo per MOI  lascio riga con scritta ita o est o bianca
044600081127       //--------------------------------------------------------------
044700081127       BEGSR CambiaMOI   ;
044800081127
044900081127        Protriga = *on      ;
045000081127        select   ;
045100081127        when tipo='0'         ;
045200081127        vscdes='----  Motivi ITALIA  ----'   ;
045300081127        when tipo='1'         ;
045400081127        vscdes='----  Motivi ESTERO  ----'   ;
045500081127        OTHER  ;
045600081127        vscdes='----  ALTRI  Motivi  ----'   ;
045700081127        ENDSL   ;
045800081127
045900081127        CLEAR vscdel    ;
046000081127        CLEAR vscSCE    ;
046100081127        CLEAR vscspf    ;
046200081127
046300081127        s01nrr=s01nrr+1 ;
046400081127        write tb70s01   ;
046500081127
046600081127        Protriga = *off     ;
046700081127
046800081127       ENDSR;
046900080206
047000080206      /end-free
047100080206
047200080206       //--------------------------------------------------------------
047300080206       //?Schiere a tempo di compilazione.
047400080206       //--------------------------------------------------------------
047500080206
047600080410** - MSG ------------------------------------------------------------------ -*
047700081008Effettuare una scelta                                                           01
