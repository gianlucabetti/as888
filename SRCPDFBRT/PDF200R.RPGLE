000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300130527      // PDF200R - Elenco file presenti su folder IFS
000400080206      //--------------------------------------------------------------
000500080206
000600130527     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700130527     h DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('TRUL')
000800080206
000900080206      //---------------------------------------------------------------
001000130527      // Dichiarazione file.
001100080206      //---------------------------------------------------------------
001200050704
001300140829     fPDFLIST   o  a e           k disk
001400140829
001500130527     fPDF200D   cf   e             workstn indds(IndDspF)
001600080206     f                                     infds(InfDspF)
001700130527     f                                     sfile(PDF2S01 : S01nrr)
001800080206      //---------------------------------------------------------------
001900080207      // - Tasti funzionali a video
002000080207     d c_F01           c                   const(x'31')
002100080207     d c_F02           c                   const(x'32')
002200080207     d c_F03           c                   const(x'33')
002300080207     d c_F05           c                   const(x'35')
002400080207     d c_F06           c                   const(x'36')
002500080207     d c_F07           c                   const(x'37')
002600080207     d c_F08           c                   const(x'38')
002700080207     d c_F09           c                   const(x'39')
002800080207     d c_F10           c                   const(x'3A')
002900080207     d c_F12           c                   const(x'3C')
003000080207     d c_F13           c                   const(x'B1')
003100080207     d c_F14           c                   const(x'B2')
003200080207     d c_F15           c                   const(x'B3')
003300080207     d c_F16           c                   const(x'B4')
003400080207     d c_F17           c                   const(x'B5')
003500080207     d c_F18           c                   const(x'B6')
003600080207     d c_F19           c                   const(x'B7')
003700080207     d c_F20           c                   const(x'B8')
003800080207     d c_F21           c                   const(x'B9')
003900080207     d c_F22           c                   const(x'BA')
004000080207     d c_F23           c                   const(x'BB')
004100080207     d c_F24           c                   const(x'BC')
004200080207     d c_Enter         c                   const(x'F1')
004300080207     d c_RollDown      c                   const(x'F4')
004400080207     d c_RollUp        c                   const(x'F5')
004500080214
004600080214      // - Attributi di visualizzazione
004700080215      //  -  DspAtr() - Normale
004800080214     d C_dspatr_Norm   c                   const(x'20')
004900080215      //  -  DspAtr(RI)
005000080214     d C_dspatr_RI     c                   const(x'21')
005100080215      //  -  DspAtr(ND)
005200080214     d C_dspatr_ND     c                   const(x'27')
005300080215      //  -  DspAtr(BL) / Color(Red)
005400080214     d C_dspatr_BL     c                   const(x'28')
005500080206
005600080206      //---------------------------------------------------------------
005700080206      //?Definizione schiere.
005800080206      //---------------------------------------------------------------
005900080206
006000080206      // - Messaggi di errore
006100120105     d MSG             s             78    dim(35) ctdata perrcd(1)
006200080206      //---------------------------------------------------------------
006300080206      //?Definizione aree dati.
006400080206      //---------------------------------------------------------------
006500100226
006600080206      // - Dati utente
006700080206     d §AzUte        e ds                  extname(AZUTE00F)
006800080206     d                                     dtaara
006900080206     d §DatiUte      e ds                  extname(dDatiUte)
007000080206     d                                     dtaara
007100080206
007200080206      //---------------------------------------------------------------
007300080206      //?Definizione strutture dati.
007400080206      //---------------------------------------------------------------
007500080619     D CPO01           DS
007600080619     D  cp1NRR               397    400B 0
007700080206
007800080206      // - Status
007900080206     d Psds           sds
008000080206     d   SDSpgm          *proc
008100140829     d   SDSuser             254    263
008200080206
008300080206      // - InfDS
008400080206     d InfDspF         ds
008500080207     d  dsp_aid              369    369a                                        AID byte
008600080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
008700080207     d  min_nrr              378    379i 0                                      Subfile min rrn
008800080207     d  num_rcds             380    381i 0                                      Subfile num rcds
008900080206
009000080206      // - Indicatori su DspF
009100080208     d IndDspF         ds
009200080206        // - Indicatori di gestione del subfile
009300080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009400080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009500080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009600080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009700080206        // - Indicatori di errore
009800080206     d  errMessage                    1n   overlay(IndDspF : 28)
009900080606     d  errGenerico                   1n   overlay(IndDspF : 99)
010000080627        // - Indicatori vari
010100130529     d  S01File_Err                   1n   overlay(IndDspF : 50)
010200130529     d  F06Attivo                     1n   overlay(IndDspF : 06)
010300130529     d  C01Path_PR                    1n   overlay(IndDspF : 51)
010400080206
010500080206      // - Parametri ricevuti
010600080206     d KPJBA         e ds
010700080206
010800080206      // - Reperimento dati utente
010900080206     d TIBS34ds      e ds
011000080206     d dUte01        e ds
011100080206
011200080206      // - Ricerca/Controllo tabelle
011300080206     d TIBS02ds      e ds                  inz
011400080627
011500080627     d
011600080206      //---------------------------------------------------------------
011700080206      //?Definizione variabili globali.
011800080206      //---------------------------------------------------------------
011900080206
012000080206      // - Flags booleani
012100080208     d $Fine           s               n   inz(*off)
012200080208     d $InzS01         s               n   inz(*on)
012300080206
012400080627     d $Video          s              2    inz('S1')
012500080208     d S01nrr          s              4  0 inz
012600130528     d SavC01Path      s                   like(C01Path)
012700081006     d XX              s              3  0 inz
012800080627     d Indx            s              3  0 inz
012900120109     d wOggi           s              8  0 inz
013000080414
013100080605     d Dataiso         s               d   datfmt(*iso)
013200080605     d Dataymd         s               d   datfmt(*ymd)
013300080605     d Datadmy         s               d   datfmt(*dmy)
013400080604     d
013500080605     d                 DS
013600080605     d  AA                     1      4  0
013700080605     d  MM                     5      6  0
013800080605     d  GG                     7      8  0
013900080605     d DATA                    1      8  0
014000130527
014100130527     D this_idxDirObj...
014200130527     D                 DS                  LIKEDS(DIRINF_idxDirObj) INZ
014300130527     D esito           S             10I 0
014400130528     d  wPath          s             60
014500130528     d  wPDF           s             61
014600130528     d  wResol         s              1
014700130528     d  wOrient        s             10
014800130528     d  wDelete        s              4
014900130528     d  wEsiOut        s              1
015000130529     d  wElab          s              5s 0
015100130529     d  wElabOK        s              5s 0
015200130529     d  wElabKO        s              5s 0
015300080206
015400080206      //---------------------------------------------------------------
015500080206      //?Definizione procedure usate.
015600080206      //---------------------------------------------------------------
015700130527      /COPY GAITRASRC/SRCCONST,TRULDIRINF
015800130527      /COPY GAITRASRC/SRCPROTOPR,TRULDIRINF
015900130527      /copy gaitrasrc/srcprotopr,tibs34r
016000130527      /copy gaitrasrc/srcprotopr,tibs02r
016100130527     d currSysNeta     s              8a   inz
016200130527      /copy gaitrasrc/srcProtoPr,UBRTVNETA
016300140829      /copy gaitrasrc/srcProtoPI,PDF00STAR
016400140829      /copy gaitrasrc/srcProtoPR,PDF00STAR
016500130528
016600080206
016700080206      //---------------------------------------------------------------
016800080206      //?Riepilogo indicatori.
016900080206      //---------------------------------------------------------------
017000080207      // - Comuni
017100080207      // 28    : Emissione messaggio di errore a video
017200080207      // - C01/S01
017300080207      // 30    : Condiziona SFLDSP    (*not)
017400080207      // 31    : Condiziona SFLDSPCTL (*not)
017500080207      // 30+31 : Condiziona SFLCLR
017600080207      // 32    : Condiziona SFLNXTCHG in S01
017700080207      // - D01
017800080207      // - Comuni
017900080207      // 99    : Generico di Errore
018000080206      //---------------------------------------------------------------
018100080206
018200080206      //---------------------------------------------------------------
018300080206      //?M A I N - L I N E
018400080206      //---------------------------------------------------------------
018500080206
018600080627     c     *Entry        plist
018700080206     c                   parm                    KPJBA
018800080627
018900080206      /free
019000080925
019100080206       // Operazioni iniziali
019200080206       exsr RoutInz;
019300080206
019400080206       // Gestione video
019500081215 3     DOW $Fine = *off;
019600081215 4       select;
019700080530
019800080530         // Videata di selezioni
019900080627           when $Video = 'S1';
020000080627             exsr GesS01;
020100080530
020200080702           other   ;
020300080206             $Fine = *on;
020400081215 4       endsl;
020500081215 3     ENDDO;
020600080206
020700080206       // Operazioni finali
020800080206       exsr RoutEnd;
020900080206
021000080206       //--------------------------------------------------------------
021100130527       // Operazioni iniziali.
021200080206       //--------------------------------------------------------------
021300080206       BEGSR RoutInz;
021400130528
021500130528         $inzs01=*on;
021600130528
021700130528         // default C01
021800140829         VSCPgm = SDSpgm;
021900130528         C01Path = '/BrtPDF/Test';
022000130528         SavC01Path = C01Path;
022100080703
022200080206         // Reperimento dati job
022300080206         exsr DatiJob;
022400130527
022500160401         // Controllo che siamo su AS888 oppure su SETRASMI
022600160516         // oppure su AS888MI
022700130529         C01Path_PR = *off;
022800130527         IF UBRTVNETA_Rtv( currSysNeta )<>*ZEROS or
022900160401         (CURRSYSNETA <> 'AS888' AND
023000160516          CURRSYSNETA <> 'SETRASMI' AND
023100160516          CURRSYSNETA <> 'AS888MI');
023200130527           // NON carico il SFL ed emetto errore
023300130528           $inzs01=*off;
023400130528           errMessage=*on;
023500130528           errGenerico=*on;
023600130528           Vscmsg=msg(01);
023700130528           SflDsp_N    = *on;
023800130528           SflDspCtl_N = *on;
023900130529           write  PDF2C01;
024000130529           SflDspCtl_N = *off;
024100130529           SflEnd      = *off;
024200130529           C01Path_PR = *on;
024300130529           exfmt  PDF2C01;
024400130529           $Fine = *on;
024500130527         ENDIF;
024600080703
024700130527       ENDSR;
024800080206
024900080206       //--------------------------------------------------------------
025000130527       // Reperimento Dati del job (Utente/Operativi).
025100080206       //--------------------------------------------------------------
025200080206       BEGSR DatiJob;
025300080206
025400080206         in(E) §AzUte;
025500080206         if NOT %error;
025600080206           in(E) §DatiUte;
025700080206         endif;
025800080206         if %error or RSut = *blanks;
025900080206           clear TIBS34ds;
026000080206           tibs34r(tibs34ds);
026100080206           in §AzUte;
026200080206           in §DatiUte;
026300080206         endif;
026400080206
026500080206       ENDSR;
026600130527
026700130527       //--------------------------------------------------------------
026800130527       // Gestione SFL 01
026900130527       //--------------------------------------------------------------
027000130527       BEGSR GesS01;
027100130527
027200080207         // Inizializzazione videata
027300080409         if  $InzS01 = *on;
027400080409            exsr InzS01;
027500080409            $InzS01  = *off;
027600080207         endif;
027700080207
027800080207         // Emissione Testata e Piede con tasti funzionali abilitati
027900130528         write  PDF2Z01;
028000080703
028100080703         // Posizionamento cursore
028200080703         if  C01csr  > *zero;
028300080703           C01rcd = C01csr;
028400080703         endif  ;
028500080207
028600080207         // Emissione videata
028700130527         exfmt  PDF2C01;
028800080410
028900080207         reset errMessage;
029000080207         reset errGenerico;
029100080702         clear Vscmsg;
029200080207
029300080523 1       SELECT;
029400080207
029500080207         // - F3=Fine
029600080523 1         WHEN  dsp_aid = c_F03;
029700080409            $Fine = *on;
029800080207
029900080207         // - F12=Ritorno
030000080523 1         WHEN  dsp_aid = c_F12;
030100080702            $Fine = *on;
030200080414
030300080530 x1      // Invio / F6
030400080207           OTHER;
030500080530
030600130528           // se è cambiato il Path, ricarico il sfl e non accetto F6
030700130528           if C01Path <> SavC01Path;
030800130528             $InzS01  = *on;
030900130528             SavC01Path = C01Path;
031000130528           else;
031100130528             // F6=conferma Aggiornamento
031200130528 1           if   dsp_aid = c_F06;
031300130528               exsr   ConfAggio ;
031400130528             endif  ;
031500130528           endif  ;
031600080207
031700080523 1       ENDSL;
031800080207
031900080207       ENDSR;
032000080207
032100080207       //--------------------------------------------------------------
032200080627       //?Inizializzazione SFL01
032300080207       //--------------------------------------------------------------
032400080409       BEGSR InzS01;
032500130528
032600130528         // Pulizia subfile
032700130528         SflDsp_N    = *on;
032800080207         SflDspCtl_N = *on;
032900130527         write  PDF2C01;
033000130527         SflDspCtl_N = *off;
033100130527         SflEnd      = *off;
033200080530
033300130527         C01Rcd=0;
033400130527         C01csr=0;
033500080627         S01nrr=0 ;
033600080702         clear Vscmsg;
033700080207         errMessage  = *off;
033800080207         errGenerico = *off;
033900130528         // Disattivazione preventiva dell'F06
034000130528         F06Attivo  = *off;
034100130528
034200130528         // default C01
034300130528         C01Path = SavC01Path;
034400080409
034500130527         // Recupero l'elenco dei file presenti su folder IFS
034600130527
034700130527         // controllo esistenza folder
034800130528         IF DIRINF_NewRichiamo(%trim(C01Path)) < DIRINF_ESITO_OK;
034900130528           // se non esiste emetto msg
035000130528           errMessage=*on;
035100130528           errGenerico=*on;
035200130528           Vscmsg=msg(02);
035300130528           leavesr;
035400130527         ENDIF;
035500130527
035600130527         // imposto quali selezioni ci sono sui file da reperire
035700130527         DIRINF_SetWhereObjectName(' LIKE ''%.pdf''');
035800130527
035900130527         // preparo il richiamo
036000130527         esito = DIRINF_RunRichiamo();
036100130527         IF esito < DIRINF_ESITO_OK;
036200130528           // se non riesco a ricevere l'elenco emetto msg
036300130528           errMessage=*on;
036400130528           errGenerico=*on;
036500130528           Vscmsg=msg(03);
036600130528           DIRINF_EndRichiamo();
036700130528           leavesr;
036800130527         ENDIF;
036900130527
037000130527         // Carico l'elenco dei file presenti su folder IFS sul SFL
037100130527
037200130527         DOU esito < DIRINF_ESITO_OK OR esito = DIRINF_ESITO_ROW_NOT_FOUND;
037300130527           esito = DIRINF_GetNextObjectName(this_idxDirObj);
037400130527           IF esito = DIRINF_ESITO_ROW_NOT_FOUND OR esito < DIRINF_ESITO_OK;
037500130527             DIRINF_EndRichiamo();
037600130527           ELSE ;
037700130527             // attivo il SFL adesso che so che scriverò (basterebbe una volta sola)
037800130527             S01File = this_idxDirObj.objNam;
037900130529             S01File_Err = *off;
038000130528             // se il nome PDF più lungo di 60 char, metto il dato in RI
038100130528             if S01File <> this_idxDirObj.objNam;
038200130529               //*in50 = *on;
038300130529               S01File_Err = *on;
038400130528             endif;
038500130527             s01nrr=s01nrr+1   ;
038600130527             write PDF2s01     ;
038700130529             //*in50 = *off;
038800130529             S01File_Err = *off;
038900130527           ENDIF;
039000130527         ENDDO;
039100130527
039200130528         // Attivazione dell'F06 (se ci sono dati)
039300130528         F06Attivo  = (S01nrr > 0)     ;
039400130528
039500130528         // Visualizzazione del SFL (se ci sono dati)
039600130528         SflDsp_N  = (S01nrr <= *zero);
039700130527
039800130527         // Attivazione del SFLEND
039900130527         SflEnd = (S01nrr  > 36);
040000130527
040100130527         // Posizionamento cursore al 1° rcd della pagina (se ci sono dati)
040200130527         if S01nrr > *zero;
040300130527           C01Rcd = 1;
040400130527           C01Csr = C01Rcd;
040500130527         else;
040600130527           clear C01Rcd;
040700130527         endif;
040800130527
040900080702         ENDSR    ;
041000080702       //--------------------------------------------------------------
041100080702       //?conferma aggiornamento
041200080702       //--------------------------------------------------------------
041300080702        BEGSR ConfAGGIO              ;
041400081003
041500130528          s01nrr=1  ;
041600130528          chain s01nrr    PDF2s01    ;
041700130529          wElab   = 0;
041800130529          wElabKO = 0;
041900130529          wElabOK = 0;
042000080704
042100130528 1        dow %found    ;
042200130528
042300130528            // se il campo col nome file è pieno
042400130528            if S01File <> *blank;
042500130529              wElab   = wElab   + 1;
042600130528              wPDF = S01File;
042700130528              wResol = 'S';
042800130528              wOrient = '*AUTO';
042900130528              wDelete = '*YES';
043000130528              wEsiOut = '3';
043100130528              // lancio il trattamento prestampa
043200130528      /end-free
043300130528     C                   CALL      'PDF02TRTC'
043400130528     C                   PARM                    C01Path
043500130528     C                   PARM                    wPDF
043600130528     C                   PARM                    wResol
043700130528     C                   PARM                    wOrient
043800130528     C                   PARM                    wDelete
043900130528     C                   PARM                    wEsiOut
044000130528      /free
044100130528
044200130529              // se l'esito va male, metto in RI il dato
044300130529              if wEsiOut <> '0';
044400130529                wElabKO = wElabKO + 1;
044500130529                //*in50 = *on;
044600130529                S01File_Err = *on;
044700130529                update PDF2S01;
044800130529                //*in50 = *off;
044900130529                S01File_Err = *off;
045000140829              // se l'esito va bene,
045100140829              // scrivo un rcd nel file PDFLIST
045200140829              // lancio il test di stampa
045300130529              else;
045400130529                wElabOK = wElabOK + 1;
045500140829                clear  RPDF;
045600140829                PDFklist  = 'T' + %subst(%char(%dec(%timestamp())) : 5 : 12);
045700140829                PDFksped  = '2014001000000001';
045800140829                PDFkey    = %trim(S01File);
045900140829                PDFinfo   = ' COPIA PER IL DESTINATARIO - Test elaborabilità +
046000140829                              PDF cliente ai fini packing list';
046100140829                PDFresol  = 'S';
046200140829                PDFcopieM = '1';
046300140829                PDFcopieD = '0';
046400140829                PDFcopieS = '0';
046500140829                PDFinsDt  = %date();
046600140829                PDFinsTm  = %time();
046700140829                PDFusrins = SDSuser;
046800140829                write  RPDF;
046900140829
047000140829                // - Impostazione parametri per prova di stampa
047100140829                PDF_KList    = PDFklist;
047200140829                PDF_Printer  = 'PRT01     ';
047300140829                PDF_FormType = '*LDVUNI   ';
047400140829                PDF_UsrDta   = 'LDV_UNI   ';
047500140829
047600140829                // - Richiamo funzione di stampa di tutte le immagini PDF
047700140829                //   (segnalando - in stampa - l'eventuale errore)
047800140829                monitor;
047900140829                callp(E)  Stampa_PDF ( PDF_KList :
048000140829                                       PDF_Printer :
048100140829                                       PDF_FormType :
048200140829                                       PDF_UsrDta );
048300140829
048400140829                on-error;
048500140829                 errMessage=*on;
048600140829                 Vscmsg='Errore in prova stampa su: ' +
048700140829                        PDFKLIST + ' / ' + PDFKey;
048800140829                 leavesr;
048900140829                endmon;
049000130529              endif;
049100130529
049200130529            endif;
049300130528
049400130528            s01nrr=s01nrr+1 ;
049500130528            chain s01nrr    PDF2s01    ;
049600130528  1       enddo   ;
049700130529
049800130529          // emetto messaggio riepilogativo
049900130529          errMessage=*on;
050000130529          Vscmsg='Elaborati : ' + %char(wElab) +
050100130529                 '  Corretti: ' + %char(wElabOK) +
050200130529                 '  In errore: ' + %char(wElabKO);
050300130528
050400080702        ENDSR;
050500080702
050600080206       //--------------------------------------------------------------
050700080206       //?Operazioni finali.
050800080206       //--------------------------------------------------------------
050900080206       BEGSR RoutEnd;
051000080627
051100080704         // Chiusura pgm   ;
051200080704         clear tibs02ds  ;
051300080704         t02tla='C'      ;
051400080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
051500080704
051600080206         *inLR = *on;
051700080206
051800080206       ENDSR;
051900080206
052000080206      /end-free
052100080206
052200080206       //--------------------------------------------------------------
052300080206       //?Schiere a tempo di compilazione.
052400080206       //--------------------------------------------------------------
052500080206
052600080410** - MSG ------------------------------------------------------------------ -*
052700160516Il sistema DEVE essere AS888 oppure SETRASMI oppure AS888MI                    01
052800130528Folder non trovata                                                             02
052900130528Il comando RunRichiamo è andato in errore                                      03
