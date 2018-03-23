000100150202       //==============================================================
000200150202       //?TRKC91R * Passaggio dati da WWDELAUM (dati.XLS) a WADELAUM.  ?
000300150202       //==============================================================
000400150202
000500150202       //--------------------------------------------------------------
000600150202       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150202       //--------------------------------------------------------------
000800150202
000900150202     /*PRM  dbgview(*source)
001000150202     /*END
001100150202
001200150202       //--------------------------------------------------------------
001300150202       //?Specifiche di controllo.                                     ?
001400150202       //--------------------------------------------------------------
001500150202
001600150202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150202     h dftactgrp(*no)
001800150203     h bnddir('TRUL')
001900150202     h alwnull(*inputonly)
002000150202
002100150202       //--------------------------------------------------------------
002200150202       //?Dichiarazione file.                                          ?
002300150202       //--------------------------------------------------------------
002400150202
002500150202       // -?Dati ricevuti nel foglio Excel?
002600150202     fWWDELAUM  if   e             disk
002700150202
002800150202       // -?Clienti in Campagne Commerciali?
002900150202     fTICMC02L  if   e           k disk
003000160202       // -?Campagne Commerciali?
003100160202     fTICMP01L  if   e           k disk
003200150202       // -?Anagrafica Commerciali?
003300150202     fAZCMM01L  if   e           k disk
003400150202       // -?Organigramma Filiale/Aziendale?
003500150202     fAZORG01L  if   e           k disk
003600150202
003700150202       // -?WF Aumento Tariffe/Campagna?
003800150202     fWADELAUM  o    e             disk
003900150202
004000150202       // -?Lista Errori rilevati?
004100150203     fQSYSPRT   o    f  132        printer usropn
004200150202     f                                     oflind(*inOF)
004300150202
004400150202       //--------------------------------------------------------------
004500150202       //?Definizione costanti.                                        ?
004600150202       //--------------------------------------------------------------
004700150203
004800150203       // -?Tipo Aumento?
004900150203     d c_TipoAumento_Campagna...
005000150203     d                 c                   const('C')
005100150203     d c_TipoAumento_Lettera...
005200150203     d                 c                   const('L')
005300150202
005400150203      *// // -?Numero Campagna che si sta generando?
005500150203     d*// c_Campagna      c                   const(1)
005600150203     d*// c_Lettera       c                   const(2)
005700150202
005800150203      *// // -?Fase da impostare?
005900150203     d*// c_Fase_ObiettivoIniziale...
006000150203     d*//                 c                   const(' 10')
006100150202
006200150202       //--------------------------------------------------------------
006300150202       //?Definizione schiere.                                         ?
006400150202       //--------------------------------------------------------------
006500150202
006600160202      // -?Campagne attive ad oggi?
006700160202     d sk_CampagneAttive...
006800160202     d                 s                   like(CMPncm)  dim(9)  inz
006900150202
007000150202       //--------------------------------------------------------------
007100150202       //?Definizione aree dati.                                       ?
007200150202       //--------------------------------------------------------------
007300150202
007400150202       // -?Dati utente?
007500150202     d §AzUte        e ds                  extname(AZUTE00F)
007600150202     d                                     dtaara
007700150202     d §DatiUte      e ds                  extname(dDatiUte)
007800150202     d                                     dtaara
007900150202
008000150202       //--------------------------------------------------------------
008100150202       //?Definizione strutture dati.                                  ?
008200150202       //--------------------------------------------------------------
008300150202
008400150202       // -?Status ds?
008500150202     d Status         sds
008600150202     d   SDSpgm          *proc
008700150202     d*//SDSprm          *parms
008800150202     d*//SDSdta              191    198
008900150202     d*//SDSjob              244    253
009000150202     d   SDSusr              254    263
009100150202
009200150202       // -?Data/Ora attuali?
009300150202     d wTime_ds        ds                  inz
009400150202     d   wDate                        8s 0 inz
009500150202     d   wTime                        6s 0 inz
009600150202
009700150202       // -?Tabella "05" = Codici Area?
009800150202     d ds05          e ds                  inz
009900150202       // -?Tabella "17" = Distretti aziendali?
010000150202     d ds17          e ds                  inz
010100150203
010200150203       // -?Di comodo?
010300150203     d WADELAUMds    e ds                  inz  extname(WADELAUM)
010400150202
010500150202       //--------------------------------------------------------------
010600150202       //?Definizione variabili globali.                               ?
010700150202       //--------------------------------------------------------------
010800150202
010900150202       // -?Codice e Descrizione errore rilevato?
011000150202     d wERR            s              3  0 inz
011100150202     d prt_Msg         s            123    inz
011200150202     d wPercErr        s              9  3 inz
011300150202
011400150202       // -?Conteggio del record già inseriti in WADELAUM?
011500150202       //  ?per lo stesso cliente?
011600150202     d wCount          s              3  0 inz
011700160202
011800160202       // -?Indici di schiera?
011900160202     d xx              s              3  0 inz
012000150202
012100150202       // -?Data *ISO?
012200150202     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
012300150202       // -?Data *EUR?
012400150202     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
012500150202
012600150202       //--------------------------------------------------------------
012700150202       //?Definizione prototipi procedure usate.                       ?
012800150202       //--------------------------------------------------------------
012900150202
013000150202       // -?Reperimento dati utente?
013100150202     d TIBS34ds      e ds                  inz
013200150202      /copy gaitrasrc/srcProtoPR,TIBS34R
013300150202
013400150202       // -?Reperimento cliente Unificante?
013500150202      /copy gaitrasrc/srcProtoPI,TIBS10R
013600150202      /copy gaitrasrc/srcProtoPR,TIBS10R
013700150202
013800150202       // -?Controllo/Decodifica cliente?
013900150202      /copy gaitrasrc/srcProtoPI,TIBS69R
014000150202      /copy gaitrasrc/srcProtoPR,TIBS69R
014100150202
014200150202       // -?Reperimento dati tabelle?
014300150202      /copy gaitrasrc/srcProtoPI,TRULTAB
014400150202      /copy gaitrasrc/srcProtoPR,TRULTAB
014500150202
014600150202       //--------------------------------------------------------------
014700150202       //?Definizione key-list.                                        ?
014800150202       //--------------------------------------------------------------
014900150202
015000150202       // -?File TICMC02L?
015100150202     d keyTICMC02    e ds                  extname( TICMC02L : *key )
015200150202     d                                     prefix( k_ )  inz
015300150202
015400150202       // -?File AZCMM01L?
015500150202     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
015600150202     d                                     prefix( k_ )  inz
015700150202
015800150202       // -?File AZORG01L?
015900150202     d keyAZORG01    e ds                  extname( AZORG01L : *key )
016000150202     d                                     prefix( k_ )  inz
016100150202
016200150202       //--------------------------------------------------------------
016300150202       //?Riepilogo indicatori utilizzati.                             ?
016400150202       //--------------------------------------------------------------
016500150202       //--------------------------------------------------------------
016600150202
016700150202       //--------------------------------------------------------------
016800150202       //?M A I N - L I N E                                            ?
016900150202       //--------------------------------------------------------------
017000150202
017100150202      /free
017200150202
017300150202       // -?Operazioni iniziali?
017400150202       exsr  sr_RoutInz;
017500150202
017600150202       // -?Ciclo di lettura WWDELAUM per scrittura WADELAUM?
017700150202       read  WWDELAUM0;
017800150202
017900150202       DoW  Not %eof(WWDELAUM);
018000150202
018100150202         exsr  sr_CtrlWWDELAUM;
018200150203
018300150203         if  wErr = *zero;
018400150203           exsr  sr_DatiMancanti;
018500150203         endif;
018600150202
018700150202         if  wErr = *zero;
018800150202           exsr  sr_WriteWADELAUM;
018900150202         else;
019000150202           exsr  sr_StampaERR;
019100150202         endif;
019200150202
019300150202         read  WWDELAUM0;
019400150202
019500150202       EndDo;
019600150202
019700150202       // -?Operazioni finali?
019800150202       exsr  sr_RoutEnd;
019900150202
020000150202       //--------------------------------------------------------------
020100150202       //?Operazioni iniziali.                                         ?
020200150202       //--------------------------------------------------------------
020300150202       BEGSR  sr_RoutInz;
020400150202
020500150202         exec sql  set option  dynusrprf = *owner,
020600150202                               closqlcsr = *endmod;
020700150202
020800150202         // -?Impostazione chiusura?
020900150202         *inLR = *on;
021000150202
021100150202         // -?Reperimento dati job?
021200150202         exsr  sr_DatiJob;
021300150202
021400150202         // -?Reperimento data e ora attuali?
021500150202         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
021600150202         wDate_Iso = %date( wDate : *Iso );
021700150202         wDate_Eur = wDate_Iso;
021800150202         wDate     = %dec( wDate_Eur );
021900160202
022000160202         // -?Memorizzazione Campagne Attive oggi?
022100160202         setll  ( *loval )  TICMP000;
022200160202         read   TICMP000;
022300160202         DoW  NOT %eof( TICMP01L );
022400160202           if  CMPdfc >= %dec( wDate_Iso );
022500160202             xx += 1;
022600160202             sk_CampagneAttive(xx) = CMPncm;
022700160202           endif;
022800160202           read  TICMP000;
022900160202         EndDo;
023000150202
023100150202       ENDSR;
023200150202
023300150202       //--------------------------------------------------------------
023400150202       //?Reperimento Dati del job (Utente/Operativi).                 ?
023500150202       //--------------------------------------------------------------
023600150202       BEGSR  sr_DatiJob;
023700150202
023800150202         in(e) §AzUte;
023900150202         if NOT %error;
024000150202           in(e) §DatiUte;
024100150202         endif;
024200150202         if %error or RSut = *blank;
024300150202           tibs34r ( tibs34ds );
024400150202           in §AzUte;
024500150202           in §DatiUte;
024600150202         endif;
024700150202
024800150202       ENDSR;
024900150202
025000150202       //--------------------------------------------------------------
025100150202       //?Controllo dati ricevuti                                      ?
025200150202       //?(già passati dal foglio Excel al file WWDELAUM).             ?
025300150202       //--------------------------------------------------------------
025400150202       BEGSR  sr_CtrlWWDELAUM;
025500150202
025600150202         clear  wERR;
025700150202
025800150202         // -?Selezione dei rec. con Tipo AUmento "C"?
025900150202         //if  wwpTAU <> c_TipoAumento_Campagna;
026000150202         //  wERR = 1;
026100150202         //  leavesr;
026200150202         //endif;
026300150202
026400150202         // -?Selezione dei clienti Italia?
026500150202         //if  wwpIOE <> 'I';
026600150202         //  wERR = 2;
026700150202         //  leavesr;
026800150202         //endif;
026900150202
027000150202         // -?Esclusione dei clienti già in Campagna?
027100150202         clear  keyTICMC02;
027200150202         k_CMCksu = wwpCLI;
027300150203         setll  %kds( keyTICMC02 : 1 )  TICMC000;
027400150202         if  %equal(TICMC02L);
027500160202           reade  %kds( keyTICMC02 : 1 )  TICMC000;
027600160202           DoW  NOT  %eof( TICMC02L );
027700160202             //**chain  ( CMCncm )  TICMP000;
027800160202             //**if  %found( TICMP01L )  and  CMPdfc > %dec( wDate_Iso );
027900160202             if  %lookup( CMCncm : sk_CampagneAttive ) > *zero;
028000160202               // -?Scriviamo lo stesso il record per evidenziarlo?
028100160202               wwpTAU = '3';
028200160202               leave;
028300160202             endif;
028400160202             reade  %kds( keyTICMC02 : 1 )  TICMC000;
028500160202           EndDo;
028600150202         endif;
028700150202
028800150202         // -?Esclusione dei clienti già in WADELAUM?
028900150202         clear  wCount;
029000150202         exec sql   set :wCount = ( select count(*)
029100150202                                      from WADELAUM
029200150202                                     where wapCLI = :wwpCLI );
029300150202         if  wCount > *zero;
029400150203           // -?Scriviamo lo stesso il record per evidenziarlo?
029500150203           wwpTAU = '4';
029600150202         endif;
029700150202
029800150202       ENDSR;
029900150203
030000150203       //--------------------------------------------------------------
030100150203       //?Reperimento dati mancanti per il cliente.                    ?
030200150203       //--------------------------------------------------------------
030300150203       BEGSR  sr_DatiMancanti;
030400150203
030500150203         // -?Ragione Sociale / Commerciale / Codice Importanza /?
030600150203         //  ?Blocco Cliente?
030700150203         clear  TIBS69ds;
030800150203         clear  ds_CNACO;
030900150203         clear  ds_CNIND;
031000150203         clear  ds_CNCLP;
031100150203         clear  ds_FNCLS;
031200150203         i69kac = wwpCLI;
031300150203         i69kcp = wwpCLI;
031400150203         tibs69r ( TIBS69ds :
031500150203                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
031600150203         if  O69err <> *blank;
031700150203           wERR = 5;
031800150203           leavesr;
031900150203         endif;
032000150203         if  wwpCLD = *blank;
032100150203           wwpCLD = acoRAG;
032200150203         endif;
032300150203         if  wwpAGE = *zero;
032400150203           wwpAGE = clpAGE;
032500150203         endif;
032600150203         if  wwpCLV = *blank;
032700150203           wwpCLV = clpCLV;
032800150203         endif;
032900150203         wwpBLO = acoABL;
033000150203
033100150203         // -?Descrizione Commerciale / Comm.le Unificante?
033200150203         If  wwpAGD = *blank  or  wwpAUC = *zero;
033300150203           clear  keyAZCMM01;
033400150203           k_CMMcod = wwpAGE;
033500150203           chain  %kds( keyAZCMM01 )  AZCMM000;
033600150203           if  Not %found(AZCMM01L);
033700150203             wERR = 6;
033800150203             leavesr;
033900150203           endif;
034000150203           if  wwpAGD = *blank;
034100150203             wwpAGD = cmmDES;
034200150203           endif;
034300150203           if  wwpAUC = *zero;
034400150203             wwpAUC = cmmUNI;
034500150203           endif;
034600150203         EndIf;
034700150203
034800150203         // -?Descrizione Commerciale Unificante?
034900150203         If  wwpAUD = *blank;
035000150203           clear  keyAZCMM01;
035100150203           k_CMMcod = wwpAUC;
035200150203           chain  %kds( keyAZCMM01 )  AZCMM000;
035300150203           if  Not %found(AZCMM01L);
035400150203             wERR = 7;
035500150203             leavesr;
035600150203           endif;
035700150203           wwpAUD = cmmDES;
035800150203         EndIf;
035900150203
036000150203         // -?Filiale, Distretto e Area del Commerciale Unificante?
036100150203         If  wwpFID = *blank  or  wwpDIV = *blank  or  wwpARAU = *zero;
036200150203           clear  keyAZORG01;
036300150203             k_ORGfil = wwpAUC / 10000;
036400150203           chain  %kds( keyAZORG01 )  AZORG;
036500150203           if  Not %found(AZORG01L);
036600150203             wERR = 8;
036700150203             leavesr;
036800150203           endif;
036900150203           if  wwpFID = *blank;
037000150203             wwpFID = orgDES;
037100150203           endif;
037200150203           if  wwpDIV = *blank;
037300150203             wwpDIV = orgFL3;
037400150203           endif;
037500150203           if  wwpARAU = *zero;
037600150203             wwpARAU = orgCAR;
037700150203           endif;
037800150203         EndIf;
037900150203         If  wwpARE = *zero;
038000150203           wwpARE = wwpARAU;
038100150203         EndIf;
038200150203
038300150203         // -?Descrizione Area Unificante?
038400150203         If  wwpARAUD = *blank;
038500150203           clear  ds05;
038600150203           ds_TNTBE.TBEke1 = %editc( wwpARAU : 'X' );
038700150203           if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
038800150203                            *omit : *omit : *omit :
038900150203                            *omit : *omit :
039000150203                            *omit : *omit : *omit : *omit :
039100150203                            *omit : *omit : *omit : *omit :
039200150203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
039300150203                          = *zero      AND
039400150203               ds_TNTBE.TBEatb = *blank;
039500150203             ds05   = ds_TNTBE.TBEuni;
039600150203             wwpARAUD = §05des;
039700150203           else;
039800150203             wERR = 9;
039900150203             leavesr;
040000150203           endif;
040100150203         EndIf;
040200150203         If  wwpARD = *blank;
040300150203           wwpARD = wwpARAUD;
040400150203         EndIf;
040500150203
040600150203         // -?Descrizione Distretto Unificante?
040700150203         If  wwpDID = *blank;
040800150203           clear  ds17;
040900150203           ds_TNTBE.TBEke1 = wwpDIV;
041000150203           if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
041100150203                            *omit : *omit : *omit :
041200150203                            *omit : *omit :
041300150203                            *omit : *omit : *omit : *omit :
041400150203                            *omit : *omit : *omit : *omit :
041500150203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
041600150203                          = *zero      AND
041700150203               ds_TNTBE.TBEatb = *blank;
041800150203             ds17   = ds_TNTBE.TBEuni;
041900150203             wwpDID = §17des;
042000150203           else;
042100150203             wERR = 10;
042200150203             leavesr;
042300150203           endif;
042400150203         EndIf;
042500150203
042600150203       ENDSR;
042700150202
042800150202       //--------------------------------------------------------------
042900150202       //?Scrittura record in WADELAUM.                                ?
043000150202       //--------------------------------------------------------------
043100150202       BEGSR  sr_WriteWADELAUM;
043200150202
043300150203         //clear  WADELAUM0;
043400150203         clear  WADELAUMds;
043500150202
043600150202         wapAAD = wwpAAD;
043700150202         wapMMD = wwpMMD;
043800150202         wapAAA = wwpAAA;
043900150202         wapMMA = wwpMMA;
044000150202
044100150203         // -?Dati Italia/Estero?
044200150203         wapIOE = wwpIOE;
044300150202
044400150203         // -?Distretto/Area/Filiale del Commerciale Unificante?
044500150202         wapDIV = wwpDIV;
044600150202         wapDID = wwpDID;
044700150202         wapARE = wwpARE;
044800150202         wapARD = wwpARD;
044900150203         wapFIL = wwpAUC / 10000;
045000150202         wapFID = wwpFID;
045100150202
045200150203         // -?Codice Importanza cliente?
045300150203         wapCLV = wwpCLV;
045400150202
045500150203         // -?Cliente?
045600150202         wapCLI = wwpCLI;
045700150203         wapCLD = wwpCLD;
045800150202
045900150203         // -?Categoria cliente?
046000150202         wapITC = wwpITC;
046100150202
046200150203         // -?Flag cliente Nuovo/Acquisito?
046300150203         select;
046400150203           when  wwpCLN = 'Acquisito'  or  %trim(wwpCLN) = 'A';
046500150204             wapCLN = 'A';
046600150203           when  wwpCLN = 'Nuovo    '  or  %trim(wwpCLN) = 'N';
046700150204             wapCLN = 'N';
046800150203         endsl;
046900150203         // -?Mese di Inizio x cliente Acquisito?
047000150203         if  %abs(wwpNEW_MES) < 100;
047100150203           wapNEW_MES = wwpNEW_MES;
047200150203         endif;
047300150203         // -?Flag cliente Nuovo/Acquisito: se non impostato metto A=Acquisito?
047400150204         select;
047500150204           when  wapCLN = *blank  and  wapNEW_MES = *zero;
047600150204             wapCLN = 'A';
047700150204           when  wapCLN = *blank  and  wapNEW_MES > *zero;
047800150204             wapCLN = 'N';
047900150204         endsl;
048000150202
048100150203         // -?Commerciale?
048200150202         wapAGE = wwpAGE;
048300150202         wapAGD = wwpAGD;
048400150202
048500150203         // -?Numero Spedizioni Anno Precedente?
048600150202         //wapSAP = ......;
048700150203         // -?Importo Fatturato Anno Precedente?
048800150202         //wapFAP = ......;
048900150203         // -?% Delta Anno Precedente?
049000150202         //wapDAP = ......;
049100150202
049200150203         // -?Numero Spedizioni Anno Corrente?
049300150202         wapSAC = wwpSAC;
049400150203         // -?Importo Fatturato Anno Corrente?
049500150202         wapFAC = wwpFAC;
049600150203         // -?% Delta Anno Corrente?
049700150202         eval(H) wapDAC = wwpDAC;
049800150202
049900150203         // -?Flag Cliente Bloccato?
050000150202         wapBLO = wwpBLO;
050100150202
050200150203         // -?Commerciale Unificante?
050300150202         wapAUC = wwpAUC;
050400150202         wapAUD = wwpAUD;
050500150202
050600150203         // -?Area del Commerciale Unificante?
050700150202         wapARAU  = wwpARAU;
050800150202         wapARAUD = wwpARAUD;
050900150202
051000150203         // -?Peso kg Anno Precedente ma valorizzato con il Peso Medio Anno Precedente?
051100150203         eval(H) wapKGP =  wwpKGP;
051200150203         // -?Peso kg Anno Corrente ma valorizzato con il Peso Medio Anno Corrente?
051300150203         eval(H) wapKGC =  wwpKGC;
051400150202
051500150203         // -?ISTAT: Sì/No?
051600150202         select;
051700150202           when  wwpIST = 'ISTAT SI'  or  %trim(wwpIST) = 'SI';
051800150202             wapIST = 'S';
051900150202           when  wwpIST = 'ISTAT NO'  or  %trim(wwpIST) = 'NO';
052000150202             wapIST = 'N';
052100150202         endsl;
052200150202
052300150203         // -?% Obiettivo Inizio (ex campo % in foglio Excel)?
052400150203         if  wwpOBI_I <> *zero;
052500150203           eval(H) wapOBI_I = wwpOBI_I * 100;
052600150203         endif;
052700150202
052800150203         // -?% Addizionale Tariffa (ex campo % in foglio Excel)?
052900150203         if  wwpADT <> *zero;
053000150203           eval(H) wapADT = wwpADT * 100;
053100150203         endif;
053200150202
053300150203         // -?Tipo Aumento: Campagna o Lettera?
053400150203         select;
053500150203           when  wwpTAU = 'NO LETTERA';
053600150203             wapTAU = c_TipoAumento_Campagna;
053700150203           when  wwpTAU = 'LETTERA';
053800150203             wapTAU = c_TipoAumento_Lettera;
053900150203           other;
054000150203             wapTAU = wwpTAU;
054100150203         endsl;
054200150202
054300150203         // -?Fascia Delta?
054400150202         wapFDL = wwpFDL;
054500150203
054600150203         //_______________
054700150203         write  WADELAUM0;
054800150203         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
054900150202
055000150202       ENDSR;
055100150202
055200150202       //--------------------------------------------------------------
055300150202       //?Operazioni finali.                                           ?
055400150202       //--------------------------------------------------------------
055500150202       BEGSR  sr_RoutEnd;
055600150202
055700150202         // -?SE stampato almeno un Errore: stampa "fine"?
055800150202         if  %open(QSYSPRT);
055900150202           except  PRTend;
056000150202           close   QSYSPRT;
056100150202         endif;
056200150202
056300150202         // -?Chiusura applicazioni precedentemente aperte?
056400150202         reset  TIBS69ds;
056500150203         //I69tla = 'C';         ?(già così)?
056600150202         tibs69r ( TIBS69ds :
056700150202                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
056800150202
056900150202         clear  TIBS10ds;
057000150202         GetKSCfamily ( TIBS10ds : 'C' );
057100150203
057200150203         cloTabella ( c_Tntbe );
057300150202
057400150202         // -?Uscita dal *pgm?
057500150202         return;
057600150202
057700150202       ENDSR;
057800150202
057900150202       //--------------------------------------------------------------
058000150202       //?Stampa singolo Errore.                                       ?
058100150202       //--------------------------------------------------------------
058200150202       BEGSR  sr_StampaERR;
058300150202
058400150202         if  Not  %open(QSYSPRT);
058500150202           open  QSYSPRT;
058600150202           *inOF = *on;
058700150202         endif;
058800150202
058900150202         clear  prt_Msg;
059000150202         Select;
059100150202           When   wERR = 1;
059200150202             prt_Msg = 'Tipo Aumento ' + wwpTAU + ' errato';
059300150202           When   wERR = 2;
059400150202             prt_Msg = 'Flag cliente Italia/Estero ' + wwpIOE + ' errato';
059500150202           When   wERR = 3;
059600150203             prt_Msg = 'Cliente già in Campagna';
059700150202           When   wERR = 4;
059800150202             prt_Msg = 'Cliente già inserito nel file WADELAUM';
059900150202           When   wERR = 5;
060000150202             prt_Msg = 'Cliente NON trovato in Anagrafica Clienti';
060100150202           When   wERR = 6;
060200150202             prt_Msg = 'Commerciale ' + %editc( wwpAGE : 'X' ) +
060300150202                       ' NON trovato in Anagrafica Commerciali';
060400150202           When   wERR = 7;
060500150202             prt_Msg = 'Comm.le Unificante ' + %editc( wwpAUC : 'X' ) +
060600150202                       ' NON trovato in Anagrafica Commerciali';
060700150202           When   wERR = 8;
060800150202             prt_Msg = 'Filiale del Comm.le Unificante ' +
060900150202                       %editc( wwpAUC : 'X' ) +
061000150202                       ' NON trovata in Organigramma';
061100150202           When   wERR = 9;
061200150202             prt_Msg = 'Area ' + %editc( wwpARE : '2' ) +
061300150202                       ' NON trovata in Tab. "05"';
061400150202           When   wERR = 10;
061500150202             prt_Msg = 'Distretto ' + wwpDIV + ' NON trovato in Tab. "17"';
061600150202           When   wERR = 11;
061700150202             prt_Msg = 'Dicitura "ISTAT SI/NO" NON prevista: ' + wwpIST;
061800150202           When   wERR = 12;
061900150203             wPercErr = wwpOBI_I * 100;
062000150202             prt_Msg = '% Obiettivo Inizio eccessiva: ' +
062100150202                       %editc( wPercErr : '2' );
062200150203           When   wERR = 13;
062300150203             wPercErr = wwpADT * 100;
062400150203             prt_Msg = '% Addizionale tariffa eccessiva: ' +
062500150203                       %editc( wPercErr : '2' );
062600150202           Other;
062700150202             prt_Msg = *all'? ';
062800150202         EndSl;
062900150202
063000150202         if  *inOF;
063100150203           except  PRTtxt;
063200150202           *inOF = *off;
063300150202         endif;
063400150202
063500150202         except  PRTdet;
063600150202
063700150202       ENDSR;
063800150202
063900150202       //--------------------------------------------------------------
064000150202       //?Output di stampa.                                            ?
064100150202       //--------------------------------------------------------------
064200150202
064300150202     oQSYSPRT   e            PRTtxt            1
064400150202     o                                              'LISTA ERRORI RILE-
064500150202     o                                              VATI IN FASE DI SC-
064600150202     o                                              RITTURA CAMPAGNA P-
064700150202     o                                              ER CLIENTE'
064800150202     o                       SDSpgm           +   5
064900150202     o                       SDSusr           +   5
065000150202     o                       wDate         y  +   5
065100150202     o                       wTime            +   3 '  :  :  '
065200150202     o                                        +   5 'Pag.'
065300150202     o                       Page          z  +   0
065400150202     o          e            PRTtxt      0
065500150202     o                                              'LISTA ERRORI RILE-
065600150202     o                                              VATI IN FASE DI SC-
065700150202     o                                              RITTURA CAMPAGNA P-
065800150202     o                                              ER CLIENTE'
065900150202     o          e            PRTtxt      1
066000150202     o                                              '------------------
066100150202     o                                              -------------------
066200150202     o                                              -------------------
066300150202     o                                              ----------'
066400150202     o          e            PRTtxt      0
066500150202     o                                              '------------------
066600150202     o                                              -------------------
066700150202     o                                              -------------------
066800150202     o                                              ----------'
066900150202     o          e            PRTtxt      2
067000150202     o                                              'Cliente'
067100150202     o                                        +   2 'Errore'
067200150203     o          e            PRTtxt      1  1
067300150202     o                                              '-------'
067400150202     o                                        +   2 '------'
067500150202      *
067600150202     o          e            PRTdet      1
067700150202     o                       wwpCLI        x
067800150202     o                       prt_Msg          +   2
067900150202      *
068000150202     o          e            PRTend      2
068100150202     o                                              '***  Fine Lista  ***'
068200150202     o          e            PRTend      0
068300150202     o                                              '***  Fine Lista  ***'
