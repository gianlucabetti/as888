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
005300170116
005400170116       // -?Costante per controllo "caratteri solo numerici"?
005500170116     d c_Digits        c                   const('0123456789')
005600150202
005700150203      *// // -?Numero Campagna che si sta generando?
005800150203     d*// c_Campagna      c                   const(1)
005900150203     d*// c_Lettera       c                   const(2)
006000150202
006100150203      *// // -?Fase da impostare?
006200150203     d*// c_Fase_ObiettivoIniziale...
006300150203     d*//                 c                   const(' 10')
006400150202
006500150202       //--------------------------------------------------------------
006600150202       //?Definizione schiere.                                         ?
006700150202       //--------------------------------------------------------------
006800150202
006900160202      // -?Campagne attive ad oggi?
007000160202     d sk_CampagneAttive...
007100160202     d                 s                   like(CMPncm)  dim(9)  inz
007200150202
007300150202       //--------------------------------------------------------------
007400150202       //?Definizione aree dati.                                       ?
007500150202       //--------------------------------------------------------------
007600150202
007700150202       // -?Dati utente?
007800150202     d §AzUte        e ds                  extname(AZUTE00F)
007900150202     d                                     dtaara
008000150202     d §DatiUte      e ds                  extname(dDatiUte)
008100150202     d                                     dtaara
008200150202
008300150202       //--------------------------------------------------------------
008400150202       //?Definizione strutture dati.                                  ?
008500150202       //--------------------------------------------------------------
008600150202
008700150202       // -?Status ds?
008800150202     d Status         sds
008900150202     d   SDSpgm          *proc
009000150202     d*//SDSprm          *parms
009100150202     d*//SDSdta              191    198
009200150202     d*//SDSjob              244    253
009300150202     d   SDSusr              254    263
009400150202
009500150202       // -?Data/Ora attuali?
009600150202     d wTime_ds        ds                  inz
009700150202     d   wDate                        8s 0 inz
009800150202     d   wTime                        6s 0 inz
009900150202
010000150202       // -?Tabella "05" = Codici Area?
010100150202     d ds05          e ds                  inz
010200150202       // -?Tabella "17" = Distretti aziendali?
010300150202     d ds17          e ds                  inz
010400150203
010500150203       // -?Di comodo?
010600150203     d WADELAUMds    e ds                  inz  extname(WADELAUM)
010700150202
010800150202       //--------------------------------------------------------------
010900150202       //?Definizione variabili globali.                               ?
011000150202       //--------------------------------------------------------------
011100170116
011200170116       // -?Eventuale n° Campagna ricevuto come parametro?
011300170116     d prm_Campagna    s              7
011400170116     d wp_Campagna     s              7  0 inz
011500150202
011600150202       // -?Codice e Descrizione errore rilevato?
011700150202     d wERR            s              3  0 inz
011800150202     d prt_Msg         s            123    inz
011900150202     d wPercErr        s              9  3 inz
012000150202
012100150202       // -?Conteggio del record già inseriti in WADELAUM?
012200150202       //  ?per lo stesso cliente?
012300150202     d wCount          s              3  0 inz
012400160202
012500160202       // -?Indici di schiera?
012600160202     d xx              s              3  0 inz
012700150202
012800150202       // -?Data *ISO?
012900150202     d wDate_Iso       s               d   datfmt(*ISO)    inz(*loval)
013000150202       // -?Data *EUR?
013100150202     d wDate_Eur       s               d   datfmt(*EUR)    inz(*loval)
013200150202
013300150202       //--------------------------------------------------------------
013400150202       //?Definizione prototipi procedure usate.                       ?
013500150202       //--------------------------------------------------------------
013600150202
013700150202       // -?Reperimento dati utente?
013800150202     d TIBS34ds      e ds                  inz
013900150202      /copy gaitrasrc/srcProtoPR,TIBS34R
014000150202
014100150202       // -?Reperimento cliente Unificante?
014200150202      /copy gaitrasrc/srcProtoPI,TIBS10R
014300150202      /copy gaitrasrc/srcProtoPR,TIBS10R
014400150202
014500150202       // -?Controllo/Decodifica cliente?
014600150202      /copy gaitrasrc/srcProtoPI,TIBS69R
014700150202      /copy gaitrasrc/srcProtoPR,TIBS69R
014800150202
014900150202       // -?Reperimento dati tabelle?
015000150202      /copy gaitrasrc/srcProtoPI,TRULTAB
015100150202      /copy gaitrasrc/srcProtoPR,TRULTAB
015200150202
015300150202       //--------------------------------------------------------------
015400150202       //?Definizione key-list.                                        ?
015500150202       //--------------------------------------------------------------
015600150202
015700150202       // -?File TICMC02L?
015800150202     d keyTICMC02    e ds                  extname( TICMC02L : *key )
015900150202     d                                     prefix( k_ )  inz
016000150202
016100150202       // -?File AZCMM01L?
016200150202     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
016300150202     d                                     prefix( k_ )  inz
016400150202
016500150202       // -?File AZORG01L?
016600150202     d keyAZORG01    e ds                  extname( AZORG01L : *key )
016700150202     d                                     prefix( k_ )  inz
016800150202
016900150202       //--------------------------------------------------------------
017000150202       //?Riepilogo indicatori utilizzati.                             ?
017100150202       //--------------------------------------------------------------
017200150202       //--------------------------------------------------------------
017300150202
017400150202       //--------------------------------------------------------------
017500150202       //?M A I N - L I N E                                            ?
017600150202       //--------------------------------------------------------------
017700170116
017800170116     c     *Entry        plist
017900170116     c                   parm                    prm_Campagna
018000150202
018100150202      /free
018200150202
018300150202       // -?Operazioni iniziali?
018400150202       exsr  sr_RoutInz;
018500150202
018600150202       // -?Ciclo di lettura WWDELAUM per scrittura WADELAUM?
018700150202       read  WWDELAUM0;
018800150202
018900150202       DoW  Not %eof(WWDELAUM);
019000150202
019100150202         exsr  sr_CtrlWWDELAUM;
019200150203
019300150203         if  wErr = *zero;
019400150203           exsr  sr_DatiMancanti;
019500150203         endif;
019600150202
019700150202         if  wErr = *zero;
019800150202           exsr  sr_WriteWADELAUM;
019900150202         else;
020000150202           exsr  sr_StampaERR;
020100150202         endif;
020200150202
020300150202         read  WWDELAUM0;
020400150202
020500150202       EndDo;
020600150202
020700150202       // -?Operazioni finali?
020800150202       exsr  sr_RoutEnd;
020900150202
021000150202       //--------------------------------------------------------------
021100150202       //?Operazioni iniziali.                                         ?
021200150202       //--------------------------------------------------------------
021300150202       BEGSR  sr_RoutInz;
021400150202
021500150202         exec sql  set option  dynusrprf = *owner,
021600150202                               closqlcsr = *endmod;
021700150202
021800150202         // -?Impostazione chiusura?
021900150202         *inLR = *on;
022000150202
022100150202         // -?Reperimento dati job?
022200150202         exsr  sr_DatiJob;
022300150202
022400150202         // -?Reperimento data e ora attuali?
022500150202         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
022600150202         wDate_Iso = %date( wDate : *Iso );
022700150202         wDate_Eur = wDate_Iso;
022800150202         wDate     = %dec( wDate_Eur );
022900160202
023000160202         // -?Memorizzazione Campagne Attive oggi?
023100160202         setll  ( *loval )  TICMP000;
023200160202         read   TICMP000;
023300160202         DoW  NOT %eof( TICMP01L );
023400160202           if  CMPdfc >= %dec( wDate_Iso );
023500160202             xx += 1;
023600160202             sk_CampagneAttive(xx) = CMPncm;
023700160202           endif;
023800160202           read  TICMP000;
023900160202         EndDo;
024000170116
024100170116         // -?Verifica SE ricevute parzializzazioni?
024200170116         clear  wp_Campagna;
024300170116         if  %parms()               > *zero   and
024400170116             %trim( prm_Campagna ) <> *blank  and
024500170116             %check( c_Digits : %trim( prm_Campagna ) ) = *zero;
024600170116           wp_Campagna = %int( %trim( prm_Campagna ) );
024700170116         endif;
024800150202
024900150202       ENDSR;
025000150202
025100150202       //--------------------------------------------------------------
025200150202       //?Reperimento Dati del job (Utente/Operativi).                 ?
025300150202       //--------------------------------------------------------------
025400150202       BEGSR  sr_DatiJob;
025500150202
025600150202         in(e) §AzUte;
025700150202         if NOT %error;
025800150202           in(e) §DatiUte;
025900150202         endif;
026000150202         if %error or RSut = *blank;
026100150202           tibs34r ( tibs34ds );
026200150202           in §AzUte;
026300150202           in §DatiUte;
026400150202         endif;
026500150202
026600150202       ENDSR;
026700150202
026800150202       //--------------------------------------------------------------
026900150202       //?Controllo dati ricevuti                                      ?
027000150202       //?(già passati dal foglio Excel al file WWDELAUM).             ?
027100150202       //--------------------------------------------------------------
027200150202       BEGSR  sr_CtrlWWDELAUM;
027300150202
027400150202         clear  wERR;
027500150202
027600150202         // -?Selezione dei rec. con Tipo AUmento "C"?
027700150202         //if  wwpTAU <> c_TipoAumento_Campagna;
027800150202         //  wERR = 1;
027900150202         //  leavesr;
028000150202         //endif;
028100150202
028200150202         // -?Selezione dei clienti Italia?
028300150202         //if  wwpIOE <> 'I';
028400150202         //  wERR = 2;
028500150202         //  leavesr;
028600150202         //endif;
028700150202
028800150202         // -?Esclusione dei clienti già in Campagna?
028900150202         clear  keyTICMC02;
029000150202         k_CMCksu = wwpCLI;
029100150203         setll  %kds( keyTICMC02 : 1 )  TICMC000;
029200150202         if  %equal(TICMC02L);
029300160202           reade  %kds( keyTICMC02 : 1 )  TICMC000;
029400160202           DoW  NOT  %eof( TICMC02L );
029500160202             //**chain  ( CMCncm )  TICMP000;
029600160202             //**if  %found( TICMP01L )  and  CMPdfc > %dec( wDate_Iso );
029700160202             if  %lookup( CMCncm : sk_CampagneAttive ) > *zero;
029800160202               // -?Scriviamo lo stesso il record per evidenziarlo?
029900160202               wwpTAU = '3';
030000160202               leave;
030100160202             endif;
030200160202             reade  %kds( keyTICMC02 : 1 )  TICMC000;
030300160202           EndDo;
030400150202         endif;
030500150202
030600150202         // -?Esclusione dei clienti già in WADELAUM?
030700150202         clear  wCount;
030800150202         exec sql   set :wCount = ( select count(*)
030900150202                                      from WADELAUM
031000150202                                     where wapCLI = :wwpCLI );
031100150202         if  wCount > *zero;
031200150203           // -?Scriviamo lo stesso il record per evidenziarlo?
031300150203           wwpTAU = '4';
031400150202         endif;
031500150202
031600150202       ENDSR;
031700150203
031800150203       //--------------------------------------------------------------
031900150203       //?Reperimento dati mancanti per il cliente.                    ?
032000150203       //--------------------------------------------------------------
032100150203       BEGSR  sr_DatiMancanti;
032200150203
032300150203         // -?Ragione Sociale / Commerciale / Codice Importanza /?
032400150203         //  ?Blocco Cliente?
032500150203         clear  TIBS69ds;
032600150203         clear  ds_CNACO;
032700150203         clear  ds_CNIND;
032800150203         clear  ds_CNCLP;
032900150203         clear  ds_FNCLS;
033000150203         i69kac = wwpCLI;
033100150203         i69kcp = wwpCLI;
033200150203         tibs69r ( TIBS69ds :
033300150203                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
033400150203         if  O69err <> *blank;
033500150203           wERR = 5;
033600150203           leavesr;
033700150203         endif;
033800150203         if  wwpCLD = *blank;
033900150203           wwpCLD = acoRAG;
034000150203         endif;
034100150203         if  wwpAGE = *zero;
034200150203           wwpAGE = clpAGE;
034300150203         endif;
034400150203         if  wwpCLV = *blank;
034500150203           wwpCLV = clpCLV;
034600150203         endif;
034700150203         wwpBLO = acoABL;
034800150203
034900150203         // -?Descrizione Commerciale / Comm.le Unificante?
035000150203         If  wwpAGD = *blank  or  wwpAUC = *zero;
035100150203           clear  keyAZCMM01;
035200150203           k_CMMcod = wwpAGE;
035300150203           chain  %kds( keyAZCMM01 )  AZCMM000;
035400150203           if  Not %found(AZCMM01L);
035500150203             wERR = 6;
035600150203             leavesr;
035700150203           endif;
035800150203           if  wwpAGD = *blank;
035900150203             wwpAGD = cmmDES;
036000150203           endif;
036100150203           if  wwpAUC = *zero;
036200150203             wwpAUC = cmmUNI;
036300150203           endif;
036400150203         EndIf;
036500150203
036600150203         // -?Descrizione Commerciale Unificante?
036700150203         If  wwpAUD = *blank;
036800150203           clear  keyAZCMM01;
036900150203           k_CMMcod = wwpAUC;
037000150203           chain  %kds( keyAZCMM01 )  AZCMM000;
037100150203           if  Not %found(AZCMM01L);
037200150203             wERR = 7;
037300150203             leavesr;
037400150203           endif;
037500150203           wwpAUD = cmmDES;
037600150203         EndIf;
037700150203
037800150203         // -?Filiale, Distretto e Area del Commerciale Unificante?
037900150203         If  wwpFID = *blank  or  wwpDIV = *blank  or  wwpARAU = *zero;
038000150203           clear  keyAZORG01;
038100150203             k_ORGfil = wwpAUC / 10000;
038200150203           chain  %kds( keyAZORG01 )  AZORG;
038300150203           if  Not %found(AZORG01L);
038400150203             wERR = 8;
038500150203             leavesr;
038600150203           endif;
038700150203           if  wwpFID = *blank;
038800150203             wwpFID = orgDES;
038900150203           endif;
039000150203           if  wwpDIV = *blank;
039100150203             wwpDIV = orgFL3;
039200150203           endif;
039300150203           if  wwpARAU = *zero;
039400150203             wwpARAU = orgCAR;
039500150203           endif;
039600150203         EndIf;
039700150203         If  wwpARE = *zero;
039800150203           wwpARE = wwpARAU;
039900150203         EndIf;
040000150203
040100150203         // -?Descrizione Area Unificante?
040200150203         If  wwpARAUD = *blank;
040300150203           clear  ds05;
040400150203           ds_TNTBE.TBEke1 = %editc( wwpARAU : 'X' );
040500150203           if  getTabella ( c_Tabel : '05'  : ds_TNTBE.TBEke1 :
040600150203                            *omit : *omit : *omit :
040700150203                            *omit : *omit :
040800150203                            *omit : *omit : *omit : *omit :
040900150203                            *omit : *omit : *omit : *omit :
041000150203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
041100150203                          = *zero      AND
041200150203               ds_TNTBE.TBEatb = *blank;
041300150203             ds05   = ds_TNTBE.TBEuni;
041400150203             wwpARAUD = §05des;
041500150203           else;
041600150203             wERR = 9;
041700150203             leavesr;
041800150203           endif;
041900150203         EndIf;
042000150203         If  wwpARD = *blank;
042100150203           wwpARD = wwpARAUD;
042200150203         EndIf;
042300150203
042400150203         // -?Descrizione Distretto Unificante?
042500150203         If  wwpDID = *blank;
042600150203           clear  ds17;
042700150203           ds_TNTBE.TBEke1 = wwpDIV;
042800150203           if  getTabella ( c_Tabel : '17'  : ds_TNTBE.TBEke1 :
042900150203                            *omit : *omit : *omit :
043000150203                            *omit : *omit :
043100150203                            *omit : *omit : *omit : *omit :
043200150203                            *omit : *omit : *omit : *omit :
043300150203                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
043400150203                          = *zero      AND
043500150203               ds_TNTBE.TBEatb = *blank;
043600150203             ds17   = ds_TNTBE.TBEuni;
043700150203             wwpDID = §17des;
043800150203           else;
043900150203             wERR = 10;
044000150203             leavesr;
044100150203           endif;
044200150203         EndIf;
044300150203
044400150203       ENDSR;
044500150202
044600150202       //--------------------------------------------------------------
044700150202       //?Scrittura record in WADELAUM.                                ?
044800150202       //--------------------------------------------------------------
044900150202       BEGSR  sr_WriteWADELAUM;
045000150202
045100150203         //clear  WADELAUM0;
045200150203         clear  WADELAUMds;
045300170116
045400170116         select;
045500170116           // -?Impostazione "Campagna" reperita da WWDELAUM.WWPAAD?
045600170116           when  WWPaad > *zero;
045700170116             wapAAD = wwpAAD;
045800170116           // -?Forzatura campagna ricevuta come parametro?
045900170116           when  wp_Campagna > *zero;
046000170116             wapAAD = wp_Campagna;
046100170116           other;
046200170116             clear  wapAAD;
046300170116         endsl;
046400170116
046500150202         wapMMD = wwpMMD;
046600150202         wapAAA = wwpAAA;
046700150202         wapMMA = wwpMMA;
046800150202
046900150203         // -?Dati Italia/Estero?
047000150203         wapIOE = wwpIOE;
047100150202
047200150203         // -?Distretto/Area/Filiale del Commerciale Unificante?
047300150202         wapDIV = wwpDIV;
047400150202         wapDID = wwpDID;
047500150202         wapARE = wwpARE;
047600150202         wapARD = wwpARD;
047700150203         wapFIL = wwpAUC / 10000;
047800150202         wapFID = wwpFID;
047900150202
048000150203         // -?Codice Importanza cliente?
048100150203         wapCLV = wwpCLV;
048200150202
048300150203         // -?Cliente?
048400150202         wapCLI = wwpCLI;
048500150203         wapCLD = wwpCLD;
048600150202
048700150203         // -?Categoria cliente?
048800150202         wapITC = wwpITC;
048900150202
049000150203         // -?Flag cliente Nuovo/Acquisito?
049100150203         select;
049200150203           when  wwpCLN = 'Acquisito'  or  %trim(wwpCLN) = 'A';
049300150204             wapCLN = 'A';
049400150203           when  wwpCLN = 'Nuovo    '  or  %trim(wwpCLN) = 'N';
049500150204             wapCLN = 'N';
049600150203         endsl;
049700150203         // -?Mese di Inizio x cliente Acquisito?
049800150203         if  %abs(wwpNEW_MES) < 100;
049900150203           wapNEW_MES = wwpNEW_MES;
050000150203         endif;
050100150203         // -?Flag cliente Nuovo/Acquisito: se non impostato metto A=Acquisito?
050200150204         select;
050300150204           when  wapCLN = *blank  and  wapNEW_MES = *zero;
050400150204             wapCLN = 'A';
050500150204           when  wapCLN = *blank  and  wapNEW_MES > *zero;
050600150204             wapCLN = 'N';
050700150204         endsl;
050800150202
050900150203         // -?Commerciale?
051000150202         wapAGE = wwpAGE;
051100150202         wapAGD = wwpAGD;
051200150202
051300150203         // -?Numero Spedizioni Anno Precedente?
051400170116         wapSAP = wwpSAP;
051500150203         // -?Importo Fatturato Anno Precedente?
051600170116         wapFAP = wwpFAP;
051700150203         // -?% Delta Anno Precedente?
051800170113         eval(H) wapDAP = wwpDAP;
051900150202
052000150203         // -?Numero Spedizioni Anno Corrente?
052100150202         wapSAC = wwpSAC;
052200150203         // -?Importo Fatturato Anno Corrente?
052300150202         wapFAC = wwpFAC;
052400150203         // -?% Delta Anno Corrente?
052500150202         eval(H) wapDAC = wwpDAC;
052600150202
052700150203         // -?Flag Cliente Bloccato?
052800150202         wapBLO = wwpBLO;
052900150202
053000150203         // -?Commerciale Unificante?
053100150202         wapAUC = wwpAUC;
053200150202         wapAUD = wwpAUD;
053300150202
053400150203         // -?Area del Commerciale Unificante?
053500150202         wapARAU  = wwpARAU;
053600150202         wapARAUD = wwpARAUD;
053700150202
053800150203         // -?Peso kg Anno Precedente ma valorizzato con il Peso Medio Anno Precedente?
053900150203         eval(H) wapKGP =  wwpKGP;
054000150203         // -?Peso kg Anno Corrente ma valorizzato con il Peso Medio Anno Corrente?
054100150203         eval(H) wapKGC =  wwpKGC;
054200150202
054300150203         // -?ISTAT: Sì/No?
054400150202         select;
054500150202           when  wwpIST = 'ISTAT SI'  or  %trim(wwpIST) = 'SI';
054600150202             wapIST = 'S';
054700150202           when  wwpIST = 'ISTAT NO'  or  %trim(wwpIST) = 'NO';
054800150202             wapIST = 'N';
054900150202         endsl;
055000150202
055100150203         // -?% Obiettivo Inizio (ex campo % in foglio Excel)?
055200150203         if  wwpOBI_I <> *zero;
055300150203           eval(H) wapOBI_I = wwpOBI_I * 100;
055400150203         endif;
055500150202
055600150203         // -?% Addizionale Tariffa (ex campo % in foglio Excel)?
055700150203         if  wwpADT <> *zero;
055800150203           eval(H) wapADT = wwpADT * 100;
055900150203         endif;
056000150202
056100150203         // -?Tipo Aumento: Campagna o Lettera?
056200150203         select;
056300150203           when  wwpTAU = 'NO LETTERA';
056400150203             wapTAU = c_TipoAumento_Campagna;
056500150203           when  wwpTAU = 'LETTERA';
056600150203             wapTAU = c_TipoAumento_Lettera;
056601171018           when  wwpTAU = 'lettera';
056602171018             wapTAU = c_TipoAumento_Lettera;
056700150203           other;
056800150203             wapTAU = wwpTAU;
056900150203         endsl;
057000150202
057100150203         // -?Fascia Delta?
057200150202         wapFDL = wwpFDL;
057300150203
057400150203         //_______________
057500150203         write  WADELAUM0;
057600150203         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
057700150202
057800150202       ENDSR;
057900150202
058000150202       //--------------------------------------------------------------
058100150202       //?Operazioni finali.                                           ?
058200150202       //--------------------------------------------------------------
058300150202       BEGSR  sr_RoutEnd;
058400150202
058500150202         // -?SE stampato almeno un Errore: stampa "fine"?
058600150202         if  %open(QSYSPRT);
058700150202           except  PRTend;
058800150202           close   QSYSPRT;
058900150202         endif;
059000150202
059100150202         // -?Chiusura applicazioni precedentemente aperte?
059200150202         reset  TIBS69ds;
059300150203         //I69tla = 'C';         ?(già così)?
059400150202         tibs69r ( TIBS69ds :
059500150202                   ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
059600150202
059700150202         clear  TIBS10ds;
059800150202         GetKSCfamily ( TIBS10ds : 'C' );
059900150203
060000150203         cloTabella ( c_Tntbe );
060100150202
060200150202         // -?Uscita dal *pgm?
060300150202         return;
060400150202
060500150202       ENDSR;
060600150202
060700150202       //--------------------------------------------------------------
060800150202       //?Stampa singolo Errore.                                       ?
060900150202       //--------------------------------------------------------------
061000150202       BEGSR  sr_StampaERR;
061100150202
061200150202         if  Not  %open(QSYSPRT);
061300150202           open  QSYSPRT;
061400150202           *inOF = *on;
061500150202         endif;
061600150202
061700150202         clear  prt_Msg;
061800150202         Select;
061900150202           When   wERR = 1;
062000150202             prt_Msg = 'Tipo Aumento ' + wwpTAU + ' errato';
062100150202           When   wERR = 2;
062200150202             prt_Msg = 'Flag cliente Italia/Estero ' + wwpIOE + ' errato';
062300150202           When   wERR = 3;
062400150203             prt_Msg = 'Cliente già in Campagna';
062500150202           When   wERR = 4;
062600150202             prt_Msg = 'Cliente già inserito nel file WADELAUM';
062700150202           When   wERR = 5;
062800150202             prt_Msg = 'Cliente NON trovato in Anagrafica Clienti';
062900150202           When   wERR = 6;
063000150202             prt_Msg = 'Commerciale ' + %editc( wwpAGE : 'X' ) +
063100150202                       ' NON trovato in Anagrafica Commerciali';
063200150202           When   wERR = 7;
063300150202             prt_Msg = 'Comm.le Unificante ' + %editc( wwpAUC : 'X' ) +
063400150202                       ' NON trovato in Anagrafica Commerciali';
063500150202           When   wERR = 8;
063600150202             prt_Msg = 'Filiale del Comm.le Unificante ' +
063700150202                       %editc( wwpAUC : 'X' ) +
063800150202                       ' NON trovata in Organigramma';
063900150202           When   wERR = 9;
064000150202             prt_Msg = 'Area ' + %editc( wwpARE : '2' ) +
064100150202                       ' NON trovata in Tab. "05"';
064200150202           When   wERR = 10;
064300150202             prt_Msg = 'Distretto ' + wwpDIV + ' NON trovato in Tab. "17"';
064400150202           When   wERR = 11;
064500150202             prt_Msg = 'Dicitura "ISTAT SI/NO" NON prevista: ' + wwpIST;
064600150202           When   wERR = 12;
064700150203             wPercErr = wwpOBI_I * 100;
064800150202             prt_Msg = '% Obiettivo Inizio eccessiva: ' +
064900150202                       %editc( wPercErr : '2' );
065000150203           When   wERR = 13;
065100150203             wPercErr = wwpADT * 100;
065200150203             prt_Msg = '% Addizionale tariffa eccessiva: ' +
065300150203                       %editc( wPercErr : '2' );
065400150202           Other;
065500150202             prt_Msg = *all'? ';
065600150202         EndSl;
065700150202
065800150202         if  *inOF;
065900150203           except  PRTtxt;
066000150202           *inOF = *off;
066100150202         endif;
066200150202
066300150202         except  PRTdet;
066400150202
066500150202       ENDSR;
066600150202
066700150202       //--------------------------------------------------------------
066800150202       //?Output di stampa.                                            ?
066900150202       //--------------------------------------------------------------
067000150202
067100150202     oQSYSPRT   e            PRTtxt            1
067200150202     o                                              'LISTA ERRORI RILE-
067300150202     o                                              VATI IN FASE DI SC-
067400150202     o                                              RITTURA CAMPAGNA P-
067500150202     o                                              ER CLIENTE'
067600150202     o                       SDSpgm           +   5
067700150202     o                       SDSusr           +   5
067800150202     o                       wDate         y  +   5
067900150202     o                       wTime            +   3 '  :  :  '
068000150202     o                                        +   5 'Pag.'
068100150202     o                       Page          z  +   0
068200150202     o          e            PRTtxt      0
068300150202     o                                              'LISTA ERRORI RILE-
068400150202     o                                              VATI IN FASE DI SC-
068500150202     o                                              RITTURA CAMPAGNA P-
068600150202     o                                              ER CLIENTE'
068700150202     o          e            PRTtxt      1
068800150202     o                                              '------------------
068900150202     o                                              -------------------
069000150202     o                                              -------------------
069100150202     o                                              ----------'
069200150202     o          e            PRTtxt      0
069300150202     o                                              '------------------
069400150202     o                                              -------------------
069500150202     o                                              -------------------
069600150202     o                                              ----------'
069700150202     o          e            PRTtxt      2
069800150202     o                                              'Cliente'
069900150202     o                                        +   2 'Errore'
070000150203     o          e            PRTtxt      1  1
070100150202     o                                              '-------'
070200150202     o                                        +   2 '------'
070300150202      *
070400150202     o          e            PRTdet      1
070500150202     o                       wwpCLI        x
070600150202     o                       prt_Msg          +   2
070700150202      *
070800150202     o          e            PRTend      2
070900150202     o                                              '***  Fine Lista  ***'
071000150202     o          e            PRTend      0
071100150202     o                                              '***  Fine Lista  ***'
