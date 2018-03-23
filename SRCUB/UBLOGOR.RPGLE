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
010900130204
011000130204       // -?Indici / Contatori?
011100130204     d xx              s              3  0 inz
011200130122
011300130122       // -?Campi per la gestione del video?
011400130122     d $Video          s              2a   inz('D1')
011500130122
011600130122       //--------------------------------------------------------------
011700130122       //?Definizione prototipi procedure usate.                       ?
011800130122       //--------------------------------------------------------------
011900130122
012000130122       // -?Reperimento dati utente?
012100130122     d TIBS34ds      e ds
012200130122      /copy gaitrasrc/srcProtoPR,TIBS34R
012300130122
012400130122       // -?Parametri API QUSROBJD (Retrieve Object Description)?
012500130122      /copy qSysInc/qRpgleSrc,QUSROBJD
012600130122      /copy GaiTraSrc/srcProtoPr,QUSROBJD
012700130122
012800130122       // -?Parametri API QCAPCMD (Process Commands)?
012900130122     d Qcmd            s           1024    inz  varying
013000130122      /copy qSysInc/qRpgleSrc,QCAPCMD
013100130122      /copy gaitrasrc/srcProtoPR,QCAPCMD
013200130122
013300130122       // -?Parametri gestione errori API.?
013400130122      /copy qSysInc/qRpgleSrc,QUSEC
013500130122
013600130122       //--------------------------------------------------------------
013700130122       //?Definizione key-list.                                        ?
013800130122       //--------------------------------------------------------------
013900130122
014000130122
014100130122       //--------------------------------------------------------------
014200130122       //?Riepilogo indicatori.                                        ?
014300130122       //--------------------------------------------------------------
014400130122
014500130122
014600130122       //--------------------------------------------------------------
014700130122       //?M A I N - L I N E                                            ?
014800130122       //--------------------------------------------------------------
014900130122
015000130122      /free
015100130122
015200130122       // -?Start?
015300130122       *inLR = *on;
015400130122
015500130122       // -?Elab?
015600130122       DoW  $Fine = *off;
015700130122         select;
015800130122           // -?Richiesta nome logo?
015900130122           when  $Video = 'D1';
016000130122             exsr  sr_GesD01;
016100130122           // -?Visualizzazione esito?
016200130122           when  $Video = 'D2';
016300130122             exsr  sr_GesD02;
016400130122           // -?? ? ??
016500130122           other;
016600130122             $Fine = *on;
016700130122         endsl;
016800130122       EndDo;
016900130122
017000130122       // -?End?
017100130122       return;
017200130122
017300130122       //--------------------------------------------------------------
017400130122       //?Operazioni iniziali.                                         ?
017500130122       //--------------------------------------------------------------
017600130122       BEGSR  *InzSR;
017700130122
017800130122         // -?Reperimento dati utente?
017900130122         exsr  sr_DatiJob;
018000130122
018100130122         // -?Impostazione nome programma in testata a video?
018200130122         //VTDpgm = SDSpgm;
018300130122
018400130122       ENDSR;
018500130122
018600130122       //--------------------------------------------------------------
018700130122       //?Reperimento Dati del job (Utente/Operativi).                 ?
018800130122       //--------------------------------------------------------------
018900130122       BEGSR  sr_DatiJob;
019000130122
019100130122         in(E) §AzUte;
019200130122         if  NOT %error;
019300130122           in(E)  §DatiUte;
019400130122         endif;
019500130122         if  %error  or  RSut = *blank;
019600130122           clear  TIBS34ds;
019700130122           tibs34r(tibs34ds);
019800130122           in §AzUte;
019900130122           in §DatiUte;
020000130122         endif;
020100130122
020200130122       ENDSR;
020300130122
020400130122       //--------------------------------------------------------------
020500130122       //?Gestione videata D01.                                        ?
020600130122       //--------------------------------------------------------------
020700130122       BEGSR  sr_GesD01;
020800130122
020900130122         // -?Inizializzazione videata?
021000130122         if  $InzD01 = *on;
021100130122           clear  ubLOGOd1;
021200130122           $InzD01 = *off;
021300130122         endif;
021400130122         AbilitF06 = *on;
021500130122         AbilitF12 = *off;
021600130122
021700130122         // -?Emissione videata?
021800130122         if  ErrMessage = *off;
021900130122           write  ubLOGOt1;
022000130122           write  ubLOGOz1;
022100130122           PosCurLibPS = *on;
022200130122         endif;
022300130122
022400130122         exfmt ubLOGOd1;
022500130122
022600130122         reset  ErrMessage;
022700130122         reset  ErrGenerico;
022800130122         clear  V1msg;
022900130122
023000130122         SELECT;
023100130122
023200130122           // -?F3=Fine?
023300130122           WHEN  dsp_aid = c_F03;
023400130122             $Fine = *on;
023500130122
023600130122           // -?Invio o F6=Conferma?
023700130122           OTHER;
023800130122             exsr  sr_CtrD01;
023900130122             select;
024000130122               // -?rilevato errore?
024100130122               when  ErrGenerico = *on;
024200130122               // -?non premuto F6?
024300130122               when  dsp_aid <> c_F06;
024400130122               // -?premuto F6 ?
024500130122               other;
024600130122                 $Video = 'D2';
024700130122                 reset  $InzD02;
024800130122             endsl;
024900130122
025000130122         ENDSL;
025100130122
025200130122       ENDSR;
025300130122
025400130122       //--------------------------------------------------------------
025500130122       //?Controllo dati in videata D01.                               ?
025600130122       //--------------------------------------------------------------
025700130122       BEGSR  sr_CtrD01;
025800130122
025900130122         %subst(IndDspF : 28) = *off;
026000130122
026100130122         // -?Flag "creazione segmento di pagina da file" errato?
026200130122         if  V1crPagS <> *blank  and  V1crPagS <> 'S';
026300130122           ErrGenerico = *on;
026400130122           ErrMessage  = *on;
026500130122           PosCurCrPagS  = *on;
026600130122           V1msg = sk_Msg(01);
026700130122           leavesr;
026800130122         endif;
026900130122
027000130122         // -?Controllo inserimento campi x creazione segmento di pag.?
027100130122         IF  V1crPagS = 'S';
027200130122
027300130122           // -?Nome libreria obbligatorio?
027400130122           if  V1libF = *blank;
027500130122             ErrGenerico = *on;
027600130122             ErrMessage  = *on;
027700130122             PosCurLibF  = *on;
027800130122             V1msg = sk_Msg(02);
027900130122             leavesr;
028000130122           endif;
028100130122
028200130122           // -?Nome libreria errato?
028300130122           clear QUSD0100;
028400130122           clear QUSEC;
028500130122           Qusbprv = %size(Qusec);
028600130122           RetrieveObjectDescription( QUSD0100 :
028700130122                                      %len(QUSD0100) :
028800130122                                      'OBJD0100' :
028900130122                                      V1libF + 'QSYS      ' :
029000130122                                      '*LIB   ' :
029100130122                                      qUsec );
029200130122           if  qUsei <> *blank;
029300130122             ErrGenerico = *on;
029400130122             ErrMessage  = *on;
029500130122             PosCurLibF  = *on;
029600130122             V1msg = sk_Msg(03);
029700130122             leavesr;
029800130122           endif;
029900130122
030000130122           // -?Nome file obbligatorio?
030100130122           if  V1file = *blank;
030200130122             ErrGenerico = *on;
030300130122             ErrMessage  = *on;
030400130122             PosCurFile  = *on;
030500130122             V1msg = sk_Msg(04);
030600130122             leavesr;
030700130122           endif;
030800130122
030900130122           // -?Nome file errato?
031000130122           clear QUSD0100;
031100130122           clear QUSEC;
031200130122           Qusbprv = %size(Qusec);
031300130122           RetrieveObjectDescription( QUSD0100 :
031400130122                                      %len(QUSD0100) :
031500130122                                      'OBJD0100' :
031600130122                                      V1file + V1libF :
031700130122                                      '*FILE  ' :
031800130122                                      qUsec );
031900130122           if  qUsei <> *blank;
032000130122             ErrGenerico = *on;
032100130122             ErrMessage  = *on;
032200130122             PosCurFile  = *on;
032300130122             V1msg = sk_Msg(05);
032400130122             leavesr;
032500130122           endif;
032600130122
032700130122         ENDIF;
032800130122
032900130122         // -?Nome libreria del segmento di pagina obbligatorio?
033000130122         //if  V1libPS = *blank;
033100130122         //  ErrGenerico = *on;
033200130122         //  ErrMessage  = *on;
033300130122         //  PosCurLibPS = *on;
033400130122         //  V1msg = sk_Msg(06);
033500130122         //  leavesr;
033600130122         //endif;
033700130122         if  V1libPS = *blank;
033800130122           V1libPS = '*LIBL     ';
033900130122         endif;
034000130122
034100130122         // -?Nome libreria del segmento di pagina errato?
034200130122         If  V1libPS <> '*LIBL     ';
034300130122           clear QUSD0100;
034400130122           clear QUSEC;
034500130122           Qusbprv = %size(Qusec);
034600130122           RetrieveObjectDescription( QUSD0100 :
034700130122                                      %len(QUSD0100) :
034800130122                                      'OBJD0100' :
034900130122                                      V1libPS + 'QSYS      ' :
035000130122                                      '*LIB   ' :
035100130122                                      qUsec );
035200130122           if  qUsei <> *blank;
035300130122             ErrGenerico = *on;
035400130122             ErrMessage  = *on;
035500130122             PosCurLibPS = *on;
035600130122             V1msg = sk_Msg(07);
035700130122             leavesr;
035800130122           endif;
035900130122         EndIf;
036000130122
036100130122         // -?Nome segmento di pagina obbligatorio?
036200130122         if  V1pagSeg = *blank;
036300130122           ErrGenerico  = *on;
036400130122           ErrMessage   = *on;
036500130122           PosCurPagSeg = *on;
036600130122           V1msg = sk_Msg(08);
036700130122           leavesr;
036800130122         endif;
036900130122
037000130122         // -?Nome segmento di pagina errato?
037100130122         If  V1crPagS = *blank;
037200130122           clear QUSD0100;
037300130122           clear QUSEC;
037400130122           Qusbprv = %size(Qusec);
037500130122           RetrieveObjectDescription( QUSD0100 :
037600130122                                      %len(QUSD0100) :
037700130122                                      'OBJD0100' :
037800130122                                      V1pagSeg + '  ' + V1libPS :
037900130122                                      '*PAGSEG' :
038000130122                                      qUsec );
038100130122           if  qUsei <> *blank;
038200130122             ErrGenerico  = *on;
038300130122             ErrMessage   = *on;
038400130122             PosCurPagSeg = *on;
038500130122             V1msg = sk_Msg(09);
038600130122             leavesr;
038700130122           endif;
038800130122         EndIf;
038900130122
039000130122       ENDSR;
039100130122
039200130122       //--------------------------------------------------------------
039300130122       //?Gestione videata D02.                                        ?
039400130122       //--------------------------------------------------------------
039500130122       BEGSR  sr_GesD02;
039600130122
039700130122         // -?Inizializzazione videata?
039800130122         if  $InzD02 = *on;
039900130122           exsr  sr_InzD02;
040000130122           $InzD02  = *off;
040100130122           AbilitF06  = *off;
040200130122           AbilitF12  = *on;
040300130122         endif;
040400130122
040500130122         // -?Emissione videata?
040600130122         if  ErrMessage = *off;
040700130122           write  ubLOGOt1;
040800130122           write  ubLOGOz1;
040900130122           write  ubLOGOd1;
041000130122           write  Protect;
041100130122         endif;
041200130122
041300130122         exfmt ubLOGOd2;
041400130122
041500130122         reset ErrMessage;
041600130122         reset ErrGenerico;
041700130122         clear V1msg;
041800130122
041900130122         SELECT;
042000130122
042100130122           // -?F3=Fine?
042200130122           WHEN  dsp_aid = c_F03;
042300130122             $Fine = *on;
042400130122
042500130122           // -?F12=Ritorno?
042600130122           WHEN  dsp_aid = c_F12;
042700130122             $Video = 'D1';
042800130122             //reset $InzD01;
042900130122             reset $InzD02;
043000130122
043100130122           // -?Invio?
043200130122           OTHER;
043300130122
043400130122         ENDSL;
043500130122
043600130122       ENDSR;
043700130122
043800130122       //--------------------------------------------------------------
043900130122       //?Inizializzazione videata D02.                                ?
044000130122       //--------------------------------------------------------------
044100130122       BEGSR  sr_InzD02;
044200130122
044300130122         // -?Creazione del segmento di pagina (SE richiesto)?
044400130122         If  V1crPagS = 'S';
044500130122           Qcmd = 'CRTPAGSEG pagseg(' + %trim(V1libPS) + '/' +
044600130122                                        %trim(V1pagSeg) + ') +
044700130122                             file(' + %trim(V1libF) + '/' +
044800130122                                      %trim(V1file) + ') +
044900130122                             mbr(*pagseg) +
045000130122                             text(''' + %trim(V1txtPS) + ''')';
045100130122           exsr  sr_ExecCmd;
045200130122           if  Qusei <> *blank;
045300130122             //$Video = 'D1';
045400130122             //ErrGenerico  = *on;
045500130122             //ErrMessage   = *on;
045600130122             //V1msg = sk_Msg(10);
045700130122             //leavesr;
045800130122             V1txt = 'Rilevato errore in fase di creazione segmento +
045900130122                      di pagina';
046000130122             leavesr;
046100130122           endif;
046200130122         EndIf;
046300130122
046400130122         // -?Stampa del segmento di pagina?
046500130122         // ·?Apertura file di stampa?
046600130122         open  ubLogoP;
046700130122         // ·?Stampa testata?
046800130122         txtDES1 = SDSusr + ' - ' + SDSpgm + ' - ' + %editc( *date:'Y' )
046900130122                          + ' - ' + %char( %time() );
047000130122         txtDES2 = 'Logo ' + %trim(V1libPS) + '/' + %trim(V1pagSeg)
047100130122                           + ' - ' + %trim(V1txtPS);
047200130122         write  PrtTXT;
047300130122         // ·?Stampa logo?
047400130122         clear PrtLOG;
047500130122         §Libr  = V1libPS;
047600130122         §Logo  = V1pagSeg;
047700130122         §PB1  += 1.000;
047800130122         //if  §PB1   > 10.500;
047900130122         //  write  PrtTXT;
048000130122         //  §PB1   = 1.000;
048100130122         //endif;
048200130122         write  PrtLOG;
048300130204         // ·?Stampa righe numerate?
048400130204         For  NumRow = 1  To  15;
048500130204           write  PrtNrRow;
048600130204           §PB1 += 1/8;
048700130204         EndFor;
048800130122         // ·?Chiusura file di stampa?
048900130122         close  ubLogoP;
049000130122
049100130122         if  V1crPagS = 'S';
049200130122           V1txt = 'Logo creato e stampato.';
049300130122         else;
049400130122           V1txt = 'Logo stampato.';
049500130122         endif;
049600130122
049700130122       ENDSR;
049800130122
049900130122       //--------------------------------------------------------------
050000130122       //?Esecuzione del comando (già impostato).                      ?
050100130122       //--------------------------------------------------------------
050200130122       BEGSR  sr_ExecCmd;
050300130122
050400130122         clear Qcap0100;
050500130122         Qcabcsdh = *off;
050600130122         Qcapa    = *off;
050700130122         Qcacmdss = *off;
050800130122         Qcaerved = *allX'00';
050900130122
051000130122         clear Qusec;
051100130122         Qusbprv  = %size(Qusec);
051200130122
051300130122         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
051400130122                           %size(Qcap0100) : 'CPOP0100' : *omit :
051500130122                           0 : 0 : Qusec);
051600130122
051700130122         //if  Qusei <> *blank;
051800130122         //  ...;
051900130122         //endif;
052000130122
052100130122       ENDSR;
052200130122
052300130122      /end-free
052400130122
052500130122       //--------------------------------------------------------------
052600130122       //?Definizione schiere a tempo di compilazione                  ?
052700130122       //--------------------------------------------------------------
052800130122
052900130122** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
053000130122Richiesta di creazione segmento di pagina da file errata                        1
053100130122Nome libreria del file obbligatorio (se richiesta la creazione del *PagSeg)     2
053200130122Nome libreria del file errato o libreria inesistente                            3
053300130122Nome file obbligatorio (se richiesta la creazione del *PagSeg)                  4
053400130122Nome file errato o file inesistente                                             5
053500130122Nome libreria del segmento di pagina obbligatorio                               6
053600130122Nome libreria del segmento di pagina errato: libreria inesistente               7
053700130122Nome segmento di pagina obbligatorio                                            8
053800130122Nome segmento di pagina errato                                                  9
053900130122Segmento di pagina NON creato                                                  10
