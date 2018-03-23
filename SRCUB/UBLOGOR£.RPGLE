000100130122      *END
000200130122       //==============================================================
000300130122       //?(Eventuale) Creazione & Stampa di un Segmento di Pagina.     ?
000400130122       //==============================================================
000500130122
000600130122     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700130122     h dftactgrp(*no) actgrp('QILE') bnddir('UBBNDDIR')
000800130122
000900130122       //--------------------------------------------------------------
001000130122       //?Dichiarazione file.                                          ?
001100130122       //--------------------------------------------------------------
001200130122
001300130122       // -?Video?
001400130122     fubLogoD   cf   e             workstn
001500130122     f                                     indds(IndDspF)
001600130122     f                                     infds(InfDspF)
001700130122
001800130122       // -?Stampa?
001900130122     fubLogoP   o    e             printer usropn infds(InfPrtF)
002000130122
002100130122       //--------------------------------------------------------------
002200130122       //?Definizione costanti.                                        ?
002300130122       //--------------------------------------------------------------
002400130122
002500130122       // -?Tasti funzionali a video?
002600130122     d c_F01           c                   const(x'31')
002700130122     d c_F02           c                   const(x'32')
002800130122     d c_F03           c                   const(x'33')
002900130122     d c_F04           c                   const(x'34')
003000130122     d c_F05           c                   const(x'35')
003100130122     d c_F06           c                   const(x'36')
003200130122     d c_F07           c                   const(x'37')
003300130122     d c_F08           c                   const(x'38')
003400130122     d c_F09           c                   const(x'39')
003500130122     d c_F10           c                   const(x'3A')
003600130122     d c_F11           c                   const(x'3B')
003700130122     d c_F12           c                   const(x'3C')
003800130122     d c_F13           c                   const(x'B1')
003900130122     d c_F14           c                   const(x'B2')
004000130122     d c_F15           c                   const(x'B3')
004100130122     d c_F16           c                   const(x'B4')
004200130122     d c_F17           c                   const(x'B5')
004300130122     d c_F18           c                   const(x'B6')
004400130122     d c_F19           c                   const(x'B7')
004500130122     d c_F20           c                   const(x'B8')
004600130122     d c_F21           c                   const(x'B9')
004700130122     d c_F22           c                   const(x'BA')
004800130122     d c_F23           c                   const(x'BB')
004900130122     d c_F24           c                   const(x'BC')
005000130122     d c_Enter         c                   const(x'F1')
005100130122     d c_RollDown      c                   const(x'F4')
005200130122     d c_RollUp        c                   const(x'F5')
005300130122
005400130122       //--------------------------------------------------------------
005500130122       //?Definizione schiere.                                         ?
005600130122       //--------------------------------------------------------------
005700130122
005800130122       // -?Messaggi di errore?
005900130122     d sk_Msg          s             78    dim(10)  ctdata  perrcd( 1)
006000130122
006100130122       //--------------------------------------------------------------
006200130122       //?Definizione aree dati.                                       ?
006300130122       //--------------------------------------------------------------
006400130122
006500130122       // -?Dati utente?
006600130122     d §AzUte        e ds                  extname(AZUTE00F)
006700130122     d                                     dtaara
006800130122     d §DatiUte      e ds                  extname(dDatiUte)
006900130122     d                                     dtaara
007000130122
007100130122       //--------------------------------------------------------------
007200130122       //?Definizione strutture dati.                                  ?
007300130122       //--------------------------------------------------------------
007400130122
007500130122       // -?Status ds?
007600130122     d Status         sds
007700130122     d   SDSpgm          *proc
007800130122     d   SDSprm          *parms
007900130122     d   SDSusr              254    263
008000130122
008100130122       // -?InfDS?
008200130122     d InfDspF         ds
008300130122     d   dsp_aid             369    369a
008400130122     d InfPrtF         ds
008500130122     d  CurLine              367    368i 0
008600130122
008700130122       // -?Indicatori su DspF?
008800130122     d IndDspF         ds
008900130122         // -?Abilitazione tasti funzionali?
009000130122     d   AbilitF06                     n   overlay(IndDspF : 06)
009100130122     d   AbilitF12                     n   overlay(IndDspF : 12)
009200130122         // -?Indicatori di errore?
009300130122     d   ErrMessage                    n   overlay(IndDspF : 28)
009400130122     d   PosCurCrPagS                  n   overlay(IndDspF : 50)
009500130122     d   PosCurLibF                    n   overlay(IndDspF : 51)
009600130122     d   PosCurFile                    n   overlay(IndDspF : 52)
009700130122     d   PosCurLibPS                   n   overlay(IndDspF : 53)
009800130122     d   PosCurPagSeg                  n   overlay(IndDspF : 54)
009900130122     d   ErrGenerico                   n   overlay(IndDspF : 99)
010000130122
010100130122       //--------------------------------------------------------------
010200130122       //?Definizione variabili globali.                               ?
010300130122       //--------------------------------------------------------------
010400130122
010500130122       // -?Flags booleani?
010600130122     d $Fine           s               n   inz(*off)
010700130122     d $InzD01         s               n   inz(*on)
010800130122     d $InzD02         s               n   inz(*on)
010900130122
011000130122       // -?Campi per la gestione del video?
011100130122     d $Video          s              2a   inz('D1')
011200130122
011300130122       //--------------------------------------------------------------
011400130122       //?Definizione prototipi procedure usate.                       ?
011500130122       //--------------------------------------------------------------
011600130122
011700130122       // -?Reperimento dati utente?
011800130122     d TIBS34ds      e ds
011900130122      /copy gaitrasrc/srcProtoPR,TIBS34R
012000130122
012100130122       // -?Parametri API QUSROBJD (Retrieve Object Description)?
012200130122      /copy qSysInc/qRpgleSrc,QUSROBJD
012300130122      /copy GaiTraSrc/srcProtoPr,QUSROBJD
012400130122
012500130122       // -?Parametri API QCAPCMD (Process Commands)?
012600130122     d Qcmd            s           1024    inz  varying
012700130122      /copy qSysInc/qRpgleSrc,QCAPCMD
012800130122      /copy gaitrasrc/srcProtoPR,QCAPCMD
012900130122
013000130122       // -?Parametri gestione errori API.?
013100130122      /copy qSysInc/qRpgleSrc,QUSEC
013200130122
013300130122       //--------------------------------------------------------------
013400130122       //?Definizione key-list.                                        ?
013500130122       //--------------------------------------------------------------
013600130122
013700130122
013800130122       //--------------------------------------------------------------
013900130122       //?Riepilogo indicatori.                                        ?
014000130122       //--------------------------------------------------------------
014100130122
014200130122
014300130122       //--------------------------------------------------------------
014400130122       //?M A I N - L I N E                                            ?
014500130122       //--------------------------------------------------------------
014600130122
014700130122      /free
014800130122
014900130122       // -?Start?
015000130122       *inLR = *on;
015100130122
015200130122       // -?Elab?
015300130122       DoW  $Fine = *off;
015400130122         select;
015500130122           // -?Richiesta nome logo?
015600130122           when  $Video = 'D1';
015700130122             exsr  sr_GesD01;
015800130122           // -?Visualizzazione esito?
015900130122           when  $Video = 'D2';
016000130122             exsr  sr_GesD02;
016100130122           // -?? ? ??
016200130122           other;
016300130122             $Fine = *on;
016400130122         endsl;
016500130122       EndDo;
016600130122
016700130122       // -?End?
016800130122       return;
016900130122
017000130122       //--------------------------------------------------------------
017100130122       //?Operazioni iniziali.                                         ?
017200130122       //--------------------------------------------------------------
017300130122       BEGSR  *InzSR;
017400130122
017500130122         // -?Reperimento dati utente?
017600130122         exsr  sr_DatiJob;
017700130122
017800130122         // -?Impostazione nome programma in testata a video?
017900130122         //VTDpgm = SDSpgm;
018000130122
018100130122       ENDSR;
018200130122
018300130122       //--------------------------------------------------------------
018400130122       //?Reperimento Dati del job (Utente/Operativi).                 ?
018500130122       //--------------------------------------------------------------
018600130122       BEGSR  sr_DatiJob;
018700130122
018800130122         in(E) §AzUte;
018900130122         if  NOT %error;
019000130122           in(E)  §DatiUte;
019100130122         endif;
019200130122         if  %error  or  RSut = *blank;
019300130122           clear  TIBS34ds;
019400130122           tibs34r(tibs34ds);
019500130122           in §AzUte;
019600130122           in §DatiUte;
019700130122         endif;
019800130122
019900130122       ENDSR;
020000130122
020100130122       //--------------------------------------------------------------
020200130122       //?Gestione videata D01.                                        ?
020300130122       //--------------------------------------------------------------
020400130122       BEGSR  sr_GesD01;
020500130122
020600130122         // -?Inizializzazione videata?
020700130122         if  $InzD01 = *on;
020800130122           clear  ubLOGOd1;
020900130122           $InzD01 = *off;
021000130122         endif;
021100130122         AbilitF06 = *on;
021200130122         AbilitF12 = *off;
021300130122
021400130122         // -?Emissione videata?
021500130122         if  ErrMessage = *off;
021600130122           write  ubLOGOt1;
021700130122           write  ubLOGOz1;
021800130122           PosCurLibPS = *on;
021900130122         endif;
022000130122
022100130122         exfmt ubLOGOd1;
022200130122
022300130122         reset  ErrMessage;
022400130122         reset  ErrGenerico;
022500130122         clear  V1msg;
022600130122
022700130122         SELECT;
022800130122
022900130122           // -?F3=Fine?
023000130122           WHEN  dsp_aid = c_F03;
023100130122             $Fine = *on;
023200130122
023300130122           // -?Invio o F6=Conferma?
023400130122           OTHER;
023500130122             exsr  sr_CtrD01;
023600130122             select;
023700130122               // -?rilevato errore?
023800130122               when  ErrGenerico = *on;
023900130122               // -?non premuto F6?
024000130122               when  dsp_aid <> c_F06;
024100130122               // -?premuto F6 ?
024200130122               other;
024300130122                 $Video = 'D2';
024400130122                 reset  $InzD02;
024500130122             endsl;
024600130122
024700130122         ENDSL;
024800130122
024900130122       ENDSR;
025000130122
025100130122       //--------------------------------------------------------------
025200130122       //?Controllo dati in videata D01.                               ?
025300130122       //--------------------------------------------------------------
025400130122       BEGSR  sr_CtrD01;
025500130122
025600130122         %subst(IndDspF : 28) = *off;
025700130122
025800130122         // -?Flag "creazione segmento di pagina da file" errato?
025900130122         if  V1crPagS <> *blank  and  V1crPagS <> 'S';
026000130122           ErrGenerico = *on;
026100130122           ErrMessage  = *on;
026200130122           PosCurCrPagS  = *on;
026300130122           V1msg = sk_Msg(01);
026400130122           leavesr;
026500130122         endif;
026600130122
026700130122         // -?Controllo inserimento campi x creazione segmento di pag.?
026800130122         IF  V1crPagS = 'S';
026900130122
027000130122           // -?Nome libreria obbligatorio?
027100130122           if  V1libF = *blank;
027200130122             ErrGenerico = *on;
027300130122             ErrMessage  = *on;
027400130122             PosCurLibF  = *on;
027500130122             V1msg = sk_Msg(02);
027600130122             leavesr;
027700130122           endif;
027800130122
027900130122           // -?Nome libreria errato?
028000130122           clear QUSD0100;
028100130122           clear QUSEC;
028200130122           Qusbprv = %size(Qusec);
028300130122           RetrieveObjectDescription( QUSD0100 :
028400130122                                      %len(QUSD0100) :
028500130122                                      'OBJD0100' :
028600130122                                      V1libF + 'QSYS      ' :
028700130122                                      '*LIB   ' :
028800130122                                      qUsec );
028900130122           if  qUsei <> *blank;
029000130122             ErrGenerico = *on;
029100130122             ErrMessage  = *on;
029200130122             PosCurLibF  = *on;
029300130122             V1msg = sk_Msg(03);
029400130122             leavesr;
029500130122           endif;
029600130122
029700130122           // -?Nome file obbligatorio?
029800130122           if  V1file = *blank;
029900130122             ErrGenerico = *on;
030000130122             ErrMessage  = *on;
030100130122             PosCurFile  = *on;
030200130122             V1msg = sk_Msg(04);
030300130122             leavesr;
030400130122           endif;
030500130122
030600130122           // -?Nome file errato?
030700130122           clear QUSD0100;
030800130122           clear QUSEC;
030900130122           Qusbprv = %size(Qusec);
031000130122           RetrieveObjectDescription( QUSD0100 :
031100130122                                      %len(QUSD0100) :
031200130122                                      'OBJD0100' :
031300130122                                      V1file + V1libF :
031400130122                                      '*FILE  ' :
031500130122                                      qUsec );
031600130122           if  qUsei <> *blank;
031700130122             ErrGenerico = *on;
031800130122             ErrMessage  = *on;
031900130122             PosCurFile  = *on;
032000130122             V1msg = sk_Msg(05);
032100130122             leavesr;
032200130122           endif;
032300130122
032400130122         ENDIF;
032500130122
032600130122         // -?Nome libreria del segmento di pagina obbligatorio?
032700130122         //if  V1libPS = *blank;
032800130122         //  ErrGenerico = *on;
032900130122         //  ErrMessage  = *on;
033000130122         //  PosCurLibPS = *on;
033100130122         //  V1msg = sk_Msg(06);
033200130122         //  leavesr;
033300130122         //endif;
033400130122         if  V1libPS = *blank;
033500130122           V1libPS = '*LIBL     ';
033600130122         endif;
033700130122
033800130122         // -?Nome libreria del segmento di pagina errato?
033900130122         If  V1libPS <> '*LIBL     ';
034000130122           clear QUSD0100;
034100130122           clear QUSEC;
034200130122           Qusbprv = %size(Qusec);
034300130122           RetrieveObjectDescription( QUSD0100 :
034400130122                                      %len(QUSD0100) :
034500130122                                      'OBJD0100' :
034600130122                                      V1libPS + 'QSYS      ' :
034700130122                                      '*LIB   ' :
034800130122                                      qUsec );
034900130122           if  qUsei <> *blank;
035000130122             ErrGenerico = *on;
035100130122             ErrMessage  = *on;
035200130122             PosCurLibPS = *on;
035300130122             V1msg = sk_Msg(07);
035400130122             leavesr;
035500130122           endif;
035600130122         EndIf;
035700130122
035800130122         // -?Nome segmento di pagina obbligatorio?
035900130122         if  V1pagSeg = *blank;
036000130122           ErrGenerico  = *on;
036100130122           ErrMessage   = *on;
036200130122           PosCurPagSeg = *on;
036300130122           V1msg = sk_Msg(08);
036400130122           leavesr;
036500130122         endif;
036600130122
036700130122         // -?Nome segmento di pagina errato?
036800130122         If  V1crPagS = *blank;
036900130122           clear QUSD0100;
037000130122           clear QUSEC;
037100130122           Qusbprv = %size(Qusec);
037200130122           RetrieveObjectDescription( QUSD0100 :
037300130122                                      %len(QUSD0100) :
037400130122                                      'OBJD0100' :
037500130122                                      V1pagSeg + '  ' + V1libPS :
037600130122                                      '*PAGSEG' :
037700130122                                      qUsec );
037800130122           if  qUsei <> *blank;
037900130122             ErrGenerico  = *on;
038000130122             ErrMessage   = *on;
038100130122             PosCurPagSeg = *on;
038200130122             V1msg = sk_Msg(09);
038300130122             leavesr;
038400130122           endif;
038500130122         EndIf;
038600130122
038700130122       ENDSR;
038800130122
038900130122       //--------------------------------------------------------------
039000130122       //?Gestione videata D02.                                        ?
039100130122       //--------------------------------------------------------------
039200130122       BEGSR  sr_GesD02;
039300130122
039400130122         // -?Inizializzazione videata?
039500130122         if  $InzD02 = *on;
039600130122           exsr  sr_InzD02;
039700130122           $InzD02  = *off;
039800130122           AbilitF06  = *off;
039900130122           AbilitF12  = *on;
040000130122         endif;
040100130122
040200130122         // -?Emissione videata?
040300130122         if  ErrMessage = *off;
040400130122           write  ubLOGOt1;
040500130122           write  ubLOGOz1;
040600130122           write  ubLOGOd1;
040700130122           write  Protect;
040800130122         endif;
040900130122
041000130122         exfmt ubLOGOd2;
041100130122
041200130122         reset ErrMessage;
041300130122         reset ErrGenerico;
041400130122         clear V1msg;
041500130122
041600130122         SELECT;
041700130122
041800130122           // -?F3=Fine?
041900130122           WHEN  dsp_aid = c_F03;
042000130122             $Fine = *on;
042100130122
042200130122           // -?F12=Ritorno?
042300130122           WHEN  dsp_aid = c_F12;
042400130122             $Video = 'D1';
042500130122             //reset $InzD01;
042600130122             reset $InzD02;
042700130122
042800130122           // -?Invio?
042900130122           OTHER;
043000130122
043100130122         ENDSL;
043200130122
043300130122       ENDSR;
043400130122
043500130122       //--------------------------------------------------------------
043600130122       //?Inizializzazione videata D02.                                ?
043700130122       //--------------------------------------------------------------
043800130122       BEGSR  sr_InzD02;
043900130122
044000130122         // -?Creazione del segmento di pagina (SE richiesto)?
044100130122         If  V1crPagS = 'S';
044200130122           Qcmd = 'CRTPAGSEG pagseg(' + %trim(V1libPS) + '/' +
044300130122                                        %trim(V1pagSeg) + ') +
044400130122                             file(' + %trim(V1libF) + '/' +
044500130122                                      %trim(V1file) + ') +
044600130122                             mbr(*pagseg) +
044700130122                             text(''' + %trim(V1txtPS) + ''')';
044800130122           exsr  sr_ExecCmd;
044900130122           if  Qusei <> *blank;
045000130122             //$Video = 'D1';
045100130122             //ErrGenerico  = *on;
045200130122             //ErrMessage   = *on;
045300130122             //V1msg = sk_Msg(10);
045400130122             //leavesr;
045500130122             V1txt = 'Rilevato errore in fase di creazione segmento +
045600130122                      di pagina';
045700130122             leavesr;
045800130122           endif;
045900130122         EndIf;
046000130122
046100130122         // -?Stampa del segmento di pagina?
046200130122         // ·?Apertura file di stampa?
046300130122         open  ubLogoP;
046400130122         // ·?Stampa testata?
046500130122         txtDES1 = SDSusr + ' - ' + SDSpgm + ' - ' + %editc( *date:'Y' )
046600130122                          + ' - ' + %char( %time() );
046700130122         txtDES2 = 'Logo ' + %trim(V1libPS) + '/' + %trim(V1pagSeg)
046800130122                           + ' - ' + %trim(V1txtPS);
046900130122         write  PrtTXT;
047000130122         // ·?Stampa logo?
047100130122         clear PrtLOG;
047200130122         §Libr  = V1libPS;
047300130122         §Logo  = V1pagSeg;
047400130122         §PB1  += 1.000;
047500130122         //if  §PB1   > 10.500;
047600130122         //  write  PrtTXT;
047700130122         //  §PB1   = 1.000;
047800130122         //endif;
047900130122         write  PrtLOG;
048000130122         // ·?Chiusura file di stampa?
048100130122         close  ubLogoP;
048200130122
048300130122         if  V1crPagS = 'S';
048400130122           V1txt = 'Logo creato e stampato.';
048500130122         else;
048600130122           V1txt = 'Logo stampato.';
048700130122         endif;
048800130122
048900130122       ENDSR;
049000130122
049100130122       //--------------------------------------------------------------
049200130122       //?Esecuzione del comando (già impostato).                      ?
049300130122       //--------------------------------------------------------------
049400130122       BEGSR  sr_ExecCmd;
049500130122
049600130122         clear Qcap0100;
049700130122         Qcabcsdh = *off;
049800130122         Qcapa    = *off;
049900130122         Qcacmdss = *off;
050000130122         Qcaerved = *allX'00';
050100130122
050200130122         clear Qusec;
050300130122         Qusbprv  = %size(Qusec);
050400130122
050500130122         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
050600130122                           %size(Qcap0100) : 'CPOP0100' : *omit :
050700130122                           0 : 0 : Qusec);
050800130122
050900130122         //if  Qusei <> *blank;
051000130122         //  ...;
051100130122         //endif;
051200130122
051300130122       ENDSR;
051400130122
051500130122      /end-free
051600130122
051700130122       //--------------------------------------------------------------
051800130122       //?Definizione schiere a tempo di compilazione                  ?
051900130122       //--------------------------------------------------------------
052000130122
052100130122** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
052200130122Richiesta di creazione segmento di pagina da file errata                        1
052300130122Nome libreria del file obbligatorio (se richiesta la creazione del *PagSeg)     2
052400130122Nome libreria del file errato o libreria inesistente                            3
052500130122Nome file obbligatorio (se richiesta la creazione del *PagSeg)                  4
052600130122Nome file errato o file inesistente                                             5
052700130122Nome libreria del segmento di pagina obbligatorio                               6
052800130122Nome libreria del segmento di pagina errato: libreria inesistente               7
052900130122Nome segmento di pagina obbligatorio                                            8
053000130122Nome segmento di pagina errato                                                  9
053100130122Segmento di pagina NON creato                                                  10
