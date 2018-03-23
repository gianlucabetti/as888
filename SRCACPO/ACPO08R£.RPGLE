000100140902       //==============================================================
000200140902       //?Acquisizione Potenziali: aggiunta delle classificazioni      ?
000300140902       //?   attività economiche ATECO 2007 mancanti in WATECO0F       ?
000400140902       //?   (dai dati di De Luca, in EDPSMTEMP/WFATECOF).             ?
000500140902       //==============================================================
000600140902
000700140902       //--------------------------------------------------------------
000800140902       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900140902       //--------------------------------------------------------------
001000140902
001100140902     /*PRM  dbgview(*source)
001200140902     /*CMD  ovrdbf file(WFATECOF) tofile(EDPSMtemp/WFATECOF) +
001300140902     /*CMD         ovrscope(*calllvl)
001400140902     /*END  dltovr file(WFATECOF) lvl(*)
001500140902     /*END
001600140902
001700140902       //--------------------------------------------------------------
001800140902       //?Specifiche di controllo.                                     ?
001900140902       //--------------------------------------------------------------
002000140902
002100140902     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200140902     h dftactgrp(*no)
002300140902
002400140902       //--------------------------------------------------------------
002500140902       //?Dichiarazione file.                                          ?
002600140902       //--------------------------------------------------------------
002700140902
002800140902       // -?Dati ricevuti da De Luca?
002900140902     fWFATECOF  if   e           k disk
003000140902     f                                     extfile(wLibFile)
003100140902     f                                     rename(WFATECOF : WFATECO0)
003200140902     f                                     prefix('X')
003300140902     f                                     usropn
003400140902
003500140902       // -?WrkF Classificazione attività economiche ATECO 2007?
003600140902     fWATECO0F  o    e             disk
003700140902
003800140902       // -?Stampa segnalazioni di errore?
003900140902     fQSYSPRT   o    f  132        printer
004000140902     f                                     usropn
004100140902     f                                     oflind(*inOF)
004200140902
004300140902       //--------------------------------------------------------------
004400140902       //?Definizione costanti.                                        ?
004500140902       //--------------------------------------------------------------
004600140902
004700140902       // -?Costante per controllo "caratteri solo numerici"?
004800140902     d c_Digits        c                   const('0123456789')
004900140902
005000140902       // -?Categoria da Mantenere (De Luca)?
005100140902     d c_DaMantenere   c                   const('MANTENERE CATEGORIA +
005200140902     d                                            ATTUALE')
005300140902
005400140902       //--------------------------------------------------------------
005500140902       //?Definizione schiere.                                         ?
005600140902       //--------------------------------------------------------------
005700140902
005800140902
005900140902       //--------------------------------------------------------------
006000140902       //?Definizione aree dati.                                       ?
006100140902       //--------------------------------------------------------------
006200140902
006300140902       // -?Dati utente?
006400140902     d §AzUte        e ds                  extname(AZUTE00F)
006500140902     d                                     dtaara
006600140902     d §DatiUte      e ds                  extname(dDatiUte)
006700140902     d                                     dtaara
006800140902
006900140902       //--------------------------------------------------------------
007000140902       //?Definizione strutture dati.                                  ?
007100140902       //--------------------------------------------------------------
007200140902
007300140902       // -?Status ds?
007400140902     d Status         sds
007500140902     d   SDSpgmName      *proc
007600140902     d*//SDSparms        *parms
007700140902     d   SDSjobName          244    253                                         Job name
007800140902     d   SDSjobUser          254    263                                         User name
007900140902     d*//SDSjobNumber        264    269s 0                                      Job number
008000140902
008100140902       // -?Parametri ricevuti (forse)?
008200140902     d KPJBA         e ds                  inz
008300140902
008400140902       //--------------------------------------------------------------
008500140902       //?Definizione variabili globali.                               ?
008600140902       //--------------------------------------------------------------
008700140902
008800140902       // -?Parametri ricevuti (forse)?
008900140902     d wKPJBA          s                   like(KPJBA)
009000140902
009100140902       // -?Nome Libreria in cui si trova il file di De Luca (WFATECOF)?
009200140902     d wLibWF          s             10    inz('EDPSMTEMP ')
009300140902
009400140902       // -?Nome esteso Libreria/File del file da elaborare?
009500140902     d wLibFile        s             21a   inz
009600140902
009700140902       // -?Campi di comodo?
009800140902     d wTime           s              6  0 inz
009900140902     d wErr            s              3  0 inz
010000140902     d wPosChar1       s              3  0 inz
010100140902     d wPosChar2       s              3  0 inz
010200140902
010300140902       //--------------------------------------------------------------
010400140902       //?Definizione prototipi procedure.                             ?
010500140902       //--------------------------------------------------------------
010600140902
010700140902       // -?Reperimento dati utente?
010800140902     d TIBS34ds      e ds                  inz
010900140902      /copy gaitrasrc/srcProtoPR,TIBS34R
011000140902
011100140902       // -?Parametri API QCAPCMD (Process Commands)?
011200140902     d Qcmd            s           2048    inz  varying
011300140902      /copy qSysInc/qRpgleSrc,QCAPCMD
011400140902       // -?API QCAPCMD (Process Commands)?
011500140902      /copy gaitrasrc/srcProtoPR,QCAPCMD
011600140902
011700140902       // -?Parametri gestione errori API.?
011800140902      /copy qsysinc/qrpglesrc,QUSEC
011900140902
012000140902       //--------------------------------------------------------------
012100140902       //?Definizione key-list.                                        ?
012200140902       //--------------------------------------------------------------
012300140902
012400140902
012500140902       //--------------------------------------------------------------
012600140902       //?Riepilogo indicatori utilizzati.                             ?
012700140902       //--------------------------------------------------------------
012800140902
012900140902
013000140902       //--------------------------------------------------------------
013100140902       //?M A I N - L I N E                                            ?
013200140902       //--------------------------------------------------------------
013300140902
013400140902     c     *Entry        plist
013500140902     c                   parm                    wKPJBA
013600140902
013700140902      /free
013800140902
013900140902       // -?Operazioni iniziali?
014000140902       exsr sr_RoutInz;
014100140902
014200140902       // -?Ciclo di lettura file WFATECOF?
014300140902       read  WFATECO0;
014400140902
014500140902       DoW  Not %eof(WFATECOF);
014600140902
014700140902         exsr  sr_ElabCateg;
014800140902
014900140902         read  WFATECO0;
015000140902
015100140902       EndDo;
015200140902
015300140902       // -?Operazioni finali?
015400140902       exsr sr_RoutEnd;
015500140902
015600140902       //--------------------------------------------------------------
015700140902       //?Operazioni iniziali.                                         ?
015800140902       //--------------------------------------------------------------
015900140902       BEGSR  sr_RoutInz;
016000140902
016100140902         // -?Impostazione chiusura?
016200140902         *inLR = *on;
016300140902
016400140902         // -?Ricezione parametri?
016500140902         if  %parms() > *zero;
016600140902           KPJBA = wKPJBA;
016700140902         else;
016800140902           knsif = *all'? ';
016900140902           knmus = SDSjobUser;
017000140902         endif;
017100140902
017200140902         // -?Reperimento orario?
017300140902         wTime = %dec( %time() );
017400140902
017500140902         // -?Reperimento dati job?
017600140902         exsr  sr_DatiJob;
017700140902
017800140902         // -?Impostazione libreria del file di De Luca (WFATECOF)?
017900140902         if  %parms() > 1  and  kpjbu <> *blank;
018000140902           wLibWF = %subst( kpjbu : 1 : 10);
018100140902         endif;
018200140902
018300140902         // -?Apertura del file di De Luca (WFATECOF)?
018400140902         wLibFile = %trim( wLibWF ) + '/WFATECOF';
018500140902         open(e)  WFATECOF;
018600140902         if %error();
018700140902           wErr = 1;
018800140902           exsr  sr_PrintErr;
018900140902           exsr  sr_RoutEnd;
019000140902         endif;
019100140902
019200140902       ENDSR;
019300140902
019400140902       //--------------------------------------------------------------
019500140902       //?Reperimento Dati del job (Utente/Operativi).                 ?
019600140902       //--------------------------------------------------------------
019700140902       BEGSR  sr_DatiJob;
019800140902
019900140902         in(e) §AzUte;
020000140902         if NOT %error;
020100140902           in(e) §DatiUte;
020200140902         endif;
020300140902         if %error or RSut = *blank;
020400140902           tibs34r ( tibs34ds );
020500140902           in §AzUte;
020600140902           in §DatiUte;
020700140902         endif;
020800140902
020900140902       ENDSR;
021000140902
021100140902       //--------------------------------------------------------------
021200140902       //?Elaborazione singolo Codice ATECO / Categoria Potenziale.    ?
021300140902       //--------------------------------------------------------------
021400140902       BEGSR  sr_ElabCateg;
021500140902
021600140902         // -?Verifica numericità del codice Ateco?
021700140902         if  %check( c_Digits : %trimR( xCat_Ateco ) ) > *zero;
021800140902           wErr = 2;
021900140902           exsr  sr_PrintErr;
022000140902           leavesr;
022100140902         endif;
022200140902
022300140902         // -?Verifica numericità del codice Categoria Seat?
022400140902         //  ?(se NON Codice Ateco da scartare)?
022500140902         If  xCod_C00001 <> c_DaMantenere;
022600140902           if  %check( c_Digits : %trimR( xCod_C00001 ) ) > *zero;
022700140902             wErr = 3;
022800140902             exsr  sr_PrintErr;
022900140902             leavesr;
023000140902           endif;
023100140902         EndIf;
023200140902
023300140902
023400140902         clear  wPosChar2;
023500140902         clear  WATECO00;
023600140902
023700140902         // -?Codice ATECO senza separatore (.)?
023800140902         WATaCod1 = xCat_Ateco;
023900140902         // -?Codice ATECO con separatore (.)?
024000140902         For  wPosChar1 = 1  To  %size( xCat_Ateco );
024100140902           wPosChar2 += 1;
024200140902           if  wPosChar1 = 3  or  wPosChar1 = 5;
024300140902             %subst( WATaCod2 : wPosChar2 : 1 ) = '.';
024400140902             wPosChar2 += 1;
024500140902           endif;
024600140902           if  %subst( xCat_Ateco : wPosChar1 : 1 ) = *blank;
024700140902             %subst( WATaCod2 : wPosChar2 : 1 ) = *zero;
024800140902           else;
024900140902             %subst( WATaCod2 : wPosChar2 : 1 ) =
025000140902                     %subst( xCat_Ateco : wPosChar1 : 1 );
025100140902           endif;
025200140902         EndFor;
025300140902         // -?Descrizione ATECO?
025400140902         WATaDes  = xDescr00001;
025500140902         // -?Descrizione Categoria Merceologica?
025600140902         WATbDCat = xDescr00002;
025700140902         // -?Codice Categoria Merceologica (SE NON da mantenere l'attuale)?
025800140902         if  xCod_C00001 <> c_DaMantenere;
025900140902           WATbCatM = %int( %trimR( xCod_C00001 ) );
026000140902         endif;
026100140902
026200140902         // -?Scrittura record?
026300140902         //______________
026400140902         write  WATECO00;
026500140902         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
026600140902
026700140902       ENDSR;
026800140902
026900140902       //--------------------------------------------------------------
027000140902       //?Stampa segnalazione dell'errore rilevato.                    ?
027100140902       //--------------------------------------------------------------
027200140902       BEGSR  sr_PrintErr;
027300140902
027400140902         // -?Apertura del printer-file?
027500140902         if  Not %open(QSYSPRT);
027600140902           Qcmd = 'OVRPRTF file(QSYSPRT) +
027700140902                           usrdta(''ACPO08*ERR'') +
027800140902                           ovrscope(*actgrpdfn)';
027900140902           exsr  sr_ExecCmd;
028000140902           open  QSYSPRT;
028100140902           except  PRTtxt;
028200140902         endif;
028300140902
028400140902         if  *inOF;
028500140902           except  PRTtxt;
028600140902           *inOF = *off;
028700140902         endif;
028800140902
028900140902         Select;
029000140902           When  wErr = 1;
029100140902             except  PrtErr1;
029200140902           When  wErr = 2;
029300140902             except  PrtErr2;
029400140902           When  wErr = 3;
029500140902             except  PrtErr3;
029600140902         EndSl;
029700140902
029800140902       ENDSR;
029900140902
030000140902       //--------------------------------------------------------------
030100140902       //?Operazioni finali.                                           ?
030200140902       //--------------------------------------------------------------
030300140902       BEGSR  sr_RoutEnd;
030400140902
030500140902         // -?Chiusura spool di stampa errori?
030600140902         if  %open(QSYSPRT);
030700140902           except  PrtEND;
030800140902           close  QSYSPRT;
030900140902         endif;
031000140902
031100140902         // -?Chiusura Work-File di output?
031200140902         if  %open(WFATECOF);
031300140902           close WFATECOF;
031400140902         endif;
031500140902
031600140902         // -?Rimozione overridea al file di CRIBIS (ATECO)?
031700140902         Qcmd = 'DLTOVR file(WFATECOF) +
031800140902                        lvl(*actgrpdfn)';
031900140902         exsr  sr_ExecCmd;
032000140902
032100140902         // -?Chiusura *pgm?
032200140902         return;
032300140902
032400140902       ENDSR;
032500140902
032600140902       //--------------------------------------------------------------
032700140902       //?Esecuzione del comando (già impostato).                      ?
032800140902       //--------------------------------------------------------------
032900140902       BEGSR  sr_ExecCmd;
033000140902
033100140902         clear Qcap0100;
033200140902         Qcabcsdh = *off;
033300140902         Qcapa    = *off;
033400140902         Qcacmdss = *off;
033500140902         Qcaerved = *allX'00';
033600140902
033700140902         clear Qusec;
033800140902         Qusbprv  = %size(Qusec);
033900140902
034000140902         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
034100140902                           %size(Qcap0100) : 'CPOP0100' : *omit :
034200140902                           0 : 0 : Qusec);
034300140902
034400140902         //if  Qusei <> *blank;
034500140902         //  ...;
034600140902         //endif;
034700140902
034800140902       ENDSR;
034900140902
035000140902      /end-free
035100140902
035200140902       //--------------------------------------------------------------
035300140902       //?S P O O L - F I L E S                                        ?
035400140902       //--------------------------------------------------------------
035500140902
035600140902     oQSYSPRT   e            PRTtxt            1
035700140902     o                       RSUT
035800140902     o                                        +   4 'LISTA ERRORI RILE-
035900140902     o                                              VATI IN FASE DI EL-
036000140902     o                                              ABORAZIONE DATI NE-
036100140902     o                                              L FILE'
036200140902     o                       wLibFile         +   1
036300140902     o                       SDSpgmName       +   4
036400140902     o                       *Date         y  +   2
036500140902     o          e            PRTtxt      0
036600140902     o                                          +24 'LISTA ERRORI RILE-
036700140902     o                                              VATI IN FASE DI EL-
036800140902     o                                              ABORAZIONE DATI NE-
036900140902     o                                              L FILE'
037000140902     o                       wLibFile         +   1
037100140902     o          e            PRTtxt      1
037200140902     o                       KNSIF
037300140902     o                       KNMUS            +   1
037400140902     o                                        +   3 '------------------
037500140902     o                                              -------------------
037600140902     o                                              -------------------
037700140902     o                                              -------------------
037800140902     o                                              ----------'
037900140902     o                                        +   4 'Pag.'
038000140902     o                       Page          z  +   0
038100140902     o                       wTime            +   4 '  :  :  '
038200140902      *
038300140902     o          e            PRTtxt      2
038400140902     o                                              'CRIF Number    '
038500140902     o                                        +   2 'Messaggio'
038600140902     o          e            PRTtxt      1
038700140902     o                                              '---------------'
038800140902     o                                        +   2 '---------'
038900140902      *
039000140902     o          e            PRTerr1     2
039100140902     o                                              'NON TROVATO IL +
039200140902     o                                               FILE'
039300140902     o                       wLibFile         +   1
039400140902      *
039500140902     o          e            PRTerr2     2
039600140902     o                       xCat_Ateco
039700140902     o                                        +   2 'Cod. ATECO con dati +
039800140902     o                                               NON numerici.'
039900140902      *
040000140902     o          e            PRTerr3     2
040100140902     o                       xCat_Ateco
040200140902     o                                        +   2 'Categoria SEAT NON +
040300140902     o                                               numerica:'
040400140902     o                       xCod_C00001      +   1
040500140902      *
040600140902     o          e            PRTend      2
040700140902     o                                        +  24 '***  Fine Lista  ***'
040800140902     o          e            PRTend      0
040900140902     o                                        +  24 '***  Fine Lista  ***'
