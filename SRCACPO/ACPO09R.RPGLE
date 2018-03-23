000100140903       //==============================================================
000200140903       //?Acquisizione Potenziali: aggiornamento WFACPV0F              ?
000300140903       //?(Acquisiz. Potenziali: dati ricevuti da CRIBIS / da variare) ?
000400140903       //?dai dati di De Luca, in EDPFG/INATT                          ?
000500140903       //==============================================================
000600140903
000700140903       //--------------------------------------------------------------
000800140903       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000900140903       //--------------------------------------------------------------
001000140903
001100140903     /*PRM  dbgview(*source)
001200140903     /*CMD  ovrdbf file(INATT) tofile(EDPFG/INATT) +
001300140903     /*CMD         ovrscope(*calllvl)
001400140903     /*END  dltovr file(INATT) lvl(*)
001500140903     /*END
001600140903
001700140903       //--------------------------------------------------------------
001800140903       //?Specifiche di controllo.                                     ?
001900140903       //--------------------------------------------------------------
002000140903
002100140903     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200140903     h dftactgrp(*no)
002300140903
002400140903       //--------------------------------------------------------------
002500140903       //?Dichiarazione file.                                          ?
002600140903       //--------------------------------------------------------------
002700140903
002800140903       // -?Dati ricevuti da De Luca?
002900140903     fINATT     if   e           k disk
003000140903     f                                     extfile(wLibFile)
003100140903     f                                     rename(INATT : INATT000)
003200140903     f                                     prefix('X')
003300140903     f                                     usropn
003400140903
003500140903       // -?WrkF Variazione Potenziali con dati ricevuti da CRIBIS?
003600140903     fWFACPV1L  Uf   e           k disk
003700140903
003800140903       // -?Stampa segnalazioni di errore?
003900140903     fQSYSPRT   o    f  132        printer
004000140903     f                                     usropn
004100140903     f                                     oflind(*inOF)
004200140903
004300140903       //--------------------------------------------------------------
004400140903       //?Definizione costanti.                                        ?
004500140903       //--------------------------------------------------------------
004600140903
004700140903       // -?Carattere separatore tra i campi nel file ricevuto?
004800140903       //  ?(standard: ";", da noi richiesto: "]")?
004900140903     d c_SepChar       c                   const(';')
005000140903
005100140903       // -?Costante per controllo "caratteri solo numerici"?
005200140903     d c_Digits        c                   const('0123456789')
005300140903
005400140903       // -?Costanti per conversione caratteri maiuscoli in minuscoli e viceversa?
005500140903     d c_Maiu          c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
005600140903     d c_Minu          c                   const('abcdefghijklmnopqrstuvwxyz')
005700140903
005800140903       //--------------------------------------------------------------
005900140903       //?Definizione schiere.                                         ?
006000140903       //--------------------------------------------------------------
006100140903
006200140903       // -?Descrizione campi per stampa errori?
006300140903     d $Campi          s             25    dim(  7)  ctdata  perrcd( 1)
006400140903
006500140903       //--------------------------------------------------------------
006600140903       //?Definizione aree dati.                                       ?
006700140903       //--------------------------------------------------------------
006800140903
006900140903       // -?Dati utente?
007000140903     d §AzUte        e ds                  extname(AZUTE00F)
007100140903     d                                     dtaara
007200140903     d §DatiUte      e ds                  extname(dDatiUte)
007300140903     d                                     dtaara
007400140903
007500140903       //--------------------------------------------------------------
007600140903       //?Definizione strutture dati.                                  ?
007700140903       //--------------------------------------------------------------
007800140903
007900140903       // -?Status ds?
008000140903     d Status         sds
008100140903     d   SDSpgmName      *proc
008200140903     d*//SDSparms        *parms
008300140903     d   SDSjobName          244    253                                         Job name
008400140903     d   SDSjobUser          254    263                                         User name
008500140903     d*//SDSjobNumber        264    269s 0                                      Job number
008600140903
008700140903       // -?Parametri ricevuti (forse)?
008800140903     d KPJBA         e ds                  inz
008900140903
009000140903       //--------------------------------------------------------------
009100140903       //?Definizione variabili globali.                               ?
009200140903       //--------------------------------------------------------------
009300140903
009400140903       // -?Parametri ricevuti (forse)?
009500140903     d wKPJBA          s                   like(KPJBA)
009600140903
009700140903       // -?Nome Libreria in cui si trova il file di De Luca (INATT)?
009800140903     d wLibWF          s             10    inz('EDPFG     ')
009900140903
010000140903       // -?Nome esteso Libreria/File del file da elaborare?
010100140903     d wLibFile        s             21a   inz
010200140903
010300140903       // -?Flags Booleani?
010400140903     d $Negativo       s               n   inz
010500140903     d $Perc           s               n   inz
010600140903
010700140903       // -?Campi di comodo?
010800140903     d wTime           s              6  0 inz
010900140903     d wErr            s              3  0 inz
011000140903     d wValoreA15      s             15    inz
011100140903     d wCampo          s              3  0 inz
011200140903     d wPosI           s              4  0 inz
011300140903     d wPosF           s              4  0 inz
011400140903     d wTipo           s              1    inz
011500140903     d wValoreA        s            255    inz
011600140903     d wValoreN        s             17  2 inz
011700140903     d wLungh          s              3  0 inz
011800140903     d wPosVirg        s              3  0 inz
011900140903     d wNumDec         s              1  0 inz
012000140903     d wPosSep         s              3  0 inz
012100140903
012200140903       //--------------------------------------------------------------
012300140903       //?Definizione prototipi procedure.                             ?
012400140903       //--------------------------------------------------------------
012500140903
012600140903       // -?Reperimento dati utente?
012700140903     d TIBS34ds      e ds                  inz
012800140903      /copy gaitrasrc/srcProtoPR,TIBS34R
012900140903
013000140903       // -?Parametri API QCAPCMD (Process Commands)?
013100140903     d Qcmd            s           2048    inz  varying
013200140903      /copy qSysInc/qRpgleSrc,QCAPCMD
013300140903       // -?API QCAPCMD (Process Commands)?
013400140903      /copy gaitrasrc/srcProtoPR,QCAPCMD
013500140903
013600140903       // -?Parametri gestione errori API.?
013700140903      /copy qsysinc/qrpglesrc,QUSEC
013800140903
013900140903       //--------------------------------------------------------------
014000140903       //?Definizione key-list.                                        ?
014100140903       //--------------------------------------------------------------
014200140903
014300140903       // -?File WFACPV1L?
014400140903     d k04wfacpv1    e ds                  extname(WFACPV1L : *key)
014500140903     d                                     prefix(k_)   inz
014600140903
014700140903       //--------------------------------------------------------------
014800140903       //?Riepilogo indicatori utilizzati.                             ?
014900140903       //--------------------------------------------------------------
015000140903
015100140903
015200140903       //--------------------------------------------------------------
015300140903       //?M A I N - L I N E                                            ?
015400140903       //--------------------------------------------------------------
015500140903
015600140903     c     *Entry        plist
015700140903     c                   parm                    wKPJBA
015800140903
015900140903      /free
016000140903
016100140903       // -?Operazioni iniziali?
016200140903       exsr sr_RoutInz;
016300140903
016400140903       // -?Ciclo di lettura file BRT_SEDI?
016500140903       read  Inatt000;
016600140903       if  %subst( xINATT : 1 : 22 ) = 'ID BRT' + c_SepChar +
016700140903                                       'CODICE FISCALE' + c_SepChar;
016800140903         read  Inatt000;
016900140903       endif;
017000140903
017100140903       DoW  Not %eof(INATT);
017200140903
017300140903         exsr  sr_ElabPot;
017400140903
017500140903         read  Inatt000;
017600140903
017700140903       EndDo;
017800140903
017900140903       // -?Operazioni finali?
018000140903       exsr sr_RoutEnd;
018100140903
018200140903       //--------------------------------------------------------------
018300140903       //?Operazioni iniziali.                                         ?
018400140903       //--------------------------------------------------------------
018500140903       BEGSR  sr_RoutInz;
018600140903
018700140903         // -?Impostazione chiusura?
018800140903         *inLR = *on;
018900140903
019000140903         // -?Ricezione parametri?
019100140903         if  %parms() > *zero;
019200140903           KPJBA = wKPJBA;
019300140903         else;
019400140903           knsif = *all'? ';
019500140903           knmus = SDSjobUser;
019600140903         endif;
019700140903
019800140903         // -?Reperimento orario?
019900140903         wTime = %dec( %time() );
020000140903
020100140903         // -?Reperimento dati job?
020200140903         exsr  sr_DatiJob;
020300140903
020400140903         // -?Impostazione libreria del file di De Luca (INATT)?
020500140903         if  %parms() > 1  and  kpjbu <> *blank;
020600140903           wLibWF = %subst( kpjbu : 1 : 10);
020700140903         endif;
020800140903
020900140903         // -?Apertura del file di De Luca (INATT)?
021000140903         wLibFile = %trim( wLibWF ) + '/INATT';
021100140903         open(e)  INATT;
021200140903         if %error();
021300140903           wErr = 1;
021400140903           exsr  sr_PrintErr;
021500140903           exsr  sr_RoutEnd;
021600140903         endif;
021700140903
021800140903       ENDSR;
021900140903
022000140903       //--------------------------------------------------------------
022100140903       //?Reperimento Dati del job (Utente/Operativi).                 ?
022200140903       //--------------------------------------------------------------
022300140903       BEGSR  sr_DatiJob;
022400140903
022500140903         in(e) §AzUte;
022600140903         if NOT %error;
022700140903           in(e) §DatiUte;
022800140903         endif;
022900140903         if %error or RSut = *blank;
023000140903           tibs34r ( tibs34ds );
023100140903           in §AzUte;
023200140903           in §DatiUte;
023300140903         endif;
023400140903
023500140903       ENDSR;
023600140903
023700140903       //--------------------------------------------------------------
023800140903       //?Elaborazione singolo cliente Potenziale.                     ?
023900140903       //--------------------------------------------------------------
024000140903       BEGSR  sr_ElabPot;
024100140903
024200140903         clear  wPosI;
024300140903         clear  wPosF;
024400140903         clear  wCampo;
024500140903         clear  wValoreA15;
024600140903
024700140903         clear  k04wfacpv1;
024800140903
024900140903         // -? 1° campo: ID BRT                  ?
025000140903         //  ?(1° campo chiave)?
025100140903         wCampo += 1;
025200140903         wTipo   = 'N';        //?- numerico?
025300140903         wLungh  = %len( WAPcpo );
025400140903         exsr  sr_GetFldVal;
025500140903         if  wErr > *zero;
025600140903           wValoreA15 = wValoreA;
025700140903           leavesr;
025800140903         endif;
025900140903         k_WAPcpo   = wValoreN;
026000140903         // -? 2° campo: Codice Fiscale          ?
026100140903         //  ?(4° campo chiave)?
026200140903         wCampo += 1;
026300140903         wTipo   = 'A';        //?- alfanumerico?
026400140903         wLungh  = %len( WAPcfi2 );
026500140903         exsr  sr_GetFldVal;
026600140903         if  wErr > *zero;
026700140903           wValoreA15 = wValoreA;
026800140903           leavesr;
026900140903         endif;
027000140903         if  wValoreA <> 'NULL ';
027100140903           k_WAPcfi2  = wValoreA;
027200140903         endif;
027300140903         // -? 3° campo: Partita Iva             ?
027400140903         //  ?(3° campo chiave)?
027500140903         wCampo += 1;
027600140903         wTipo   = 'A';        //?- alfanumerico?
027700140903         wLungh  = %len( WAPcpi2 );
027800140903         exsr  sr_GetFldVal;
027900140903         if  wErr > *zero;
028000140903           wValoreA15 = wValoreA;
028100140903           leavesr;
028200140903         endif;
028300140903         if  wValoreA <> 'NULL ';
028400140903           k_WAPcpi2  = wValoreA;
028500140903         endif;
028600140903         // -? 4° campo: Crif Number             ?
028700140903         //  ?(2° campo chiave)?
028800140903         wCampo += 1;
028900140903         wTipo   = 'A';        //?- alfanumerico?
029000140903         wLungh  = %len( WAPcrif );
029100140903         exsr  sr_GetFldVal;
029200140903         if  wErr > *zero;
029300140903           wValoreA15 = wValoreA;
029400140903           leavesr;
029500140903         endif;
029600140903         k_WAPCrif  = wValoreA;
029700140903
029800140903         // -? 5° campo: Natura Giuridica        ?
029900140903         wCampo += 1;
030000140903         clear  wTipo;         //?- nessun campo?
030100140903         clear  wLungh;        //?- nessun campo?
030200140903         exsr  sr_GetFldVal;
030300140903         // -? 6° campo: Stato impresa CRIBIS    ?
030400140903         wCampo += 1;
030500140903         clear  wTipo;         //?- nessun campo?
030600140903         clear  wLungh;        //?- nessun campo?
030700140903         exsr  sr_GetFldVal;
030800140903
030900140903         // -? 7° campo: Data Inizio Attività    ?
031000140903         wCampo += 1;
031100140903         wTipo   = 'D';        //?- data?
031200140903         wLungh  = %len( WAPdtIni );
031300140903         exsr  sr_GetFldVal;
031400140903         if  wErr > *zero;
031500140903           wValoreA15 = wValoreA;
031600140903           leavesr;
031700140903         endif;
031800140903         //WAPdtIni = wValoreA;
031900140903
032000140903         // -?Reperimento dati da aggiornare?
032100140903         chain  %kds( k04wfacpv1 )  WFACPV00;
032200140903
032300140903         // -?SE cliente NON trovato in WFACPV0F => uscita?
032400140903         if  Not %found(WFACPV1L);
032500140903           wErr = 5;
032600140903           //exsr  sr_PrintErr;
032700140903           leavesr;
032800140903         endif;
032900140903
033000140903         // -?AGGIORNAMENTO Data Inizio Attività (se diversa)?
033100140903         If  WAPdtIni <> wValoreN;
033200140903
033300140903           WAPdtIni = wValoreN;
033400140903
033500140903           //_______________
033600140903           update  WFACPV00;
033700140903           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
033800140903
033900140903         Else;
034000140903
034100140903           unlock  WFACPV1L;
034200140903
034300140903         EndIf;
034400140903
034500140903       ENDSR;
034600140903
034700140903       //--------------------------------------------------------------
034800140903       //?Reperimento valore del campo in elaborazione.                ?
034900140903       //--------------------------------------------------------------
035000140903       BEGSR  sr_GetFldVal;
035100140903
035200140903         // -?Calcolo Posizioni Iniziale e Finale?
035300140903         wPosI = wPosF + 1;
035400140903         wPosF = %scan( c_SepChar : xINATT : wPosI );
035500140903         if  wPosF = *zero;
035600140903           wPosF = %len( %trimr( xINATT ) ) + 1;
035700140903         endif;
035800140903
035900140903         // -?Estrazione dati da "convertire"?
036000140903         clear  wErr;
036100140903         clear  wValoreA;
036200140903         clear  wValoreN;
036300140903         clear  $Negativo;
036400140903         clear  $Perc;
036500140903         // -?...SE reperiti dati?
036600140903         if  wPosF > wPosI;
036700140903           wValoreA = %trimL( %subst( xINATT : wPosI : wPosF - wPosI ) );
036800140903         endif;
036900140903
037000140903         Select;
037100140903
037200140903           // -?Campo vuoto?
037300140903           When  wValoreA = *blank;
037400140903             leavesr;
037500140903
037600140903           // -?Caratteri alfanumerici da convertire in Maiuscolo?
037700140903           When  wTipo = 'a';
037800140903             wValoreA = %xlate( c_Minu : c_Maiu : wValoreA );
037900140903
038000140903           // -?Caratteri numerici con segno e/o con imbolo "%"?
038100140903           When  wTipo = 'n';
038200140903             if  %scan( '-' : wValoreA ) > *zero;
038300140903               $Negativo = *on;
038400140903               %subst( wValoreA : %scan( '-' : wValoreA ) : 1 ) = ' ';
038500140903               wValoreA  = %trim( wValoreA );
038600140903             endif;
038700140903             if  %scan( '%' : wValoreA ) > *zero;
038800140903               $Perc    = *on;
038900140903               %subst( wValoreA : %scan( '%' : wValoreA ) : 1 ) = ' ';
039000140903               wValoreA = %trim( wValoreA );
039100140903             endif;
039200140903
039300140903           // -?Caratteri numerici per data (con separatore "/")?
039400140903           When  wTipo = 'D';
039500140903             wPosSep = %scan( '/' : wValoreA );
039600140903             doW  wPosSep > *zero;
039700140903               wValoreA = %subst( wValoreA : 1 : wPosSep - 1 ) +
039800140903                          %subst( wValoreA : wPosSep + 1 );
039900140903               wPosSep = %scan( '/' : wValoreA );
040000140903             enddo;
040100140903
040200140903         EndSl;
040300140903
040400140903         Select;
040500140903
040600140903           // -?Caratteri alfanumerici di cui controllare la lunghezza?
040700140903           When  wTipo = 'A';
040800140903             exsr  sr_CtrlAlfa;
040900140903
041000140903           // -?Caratteri alfanumerici da convertire in Numerici e?
041100140903           //  ?di cui controllare i decimali?
041200140903           When  wTipo = 'N'  or  wTipo = 'n'  or  wTipo = 'D';
041300140903             exsr  sr_CtrlNum;
041400140903
041500140903         EndSl;
041600140903
041700140903       ENDSR;
041800140903
041900140903       //--------------------------------------------------------------
042000140903       //?Controlli valori per campo alfanumerico.                     ?
042100140903       //--------------------------------------------------------------
042200140903       BEGSR  sr_CtrlAlfa;
042300140903
042400140903         // -?Trovati troppi caratteri?
042500140903         //  ?(SOLO per campi lunghi MENO di 50 caratteri,?
042600140903         //  ?dai 50 in su: si tagliano!)?
042700140903         if  %len( %trimR( wValoreA ) ) > wLungh  and
042800140903             wLungh < 50;
042900140903           wLungh = %len( %trimR( wValoreA ) );
043000140903           wErr = 2;
043100140903           exsr  sr_PrintErr;
043200140903           leavesr;
043300140903         endif;
043400140903
043500140903       ENDSR;
043600140903
043700140903       //--------------------------------------------------------------
043800140903       //?Controlli valori per campo numerico.                         ?
043900140903       //--------------------------------------------------------------
044000140903       BEGSR  sr_CtrlNum;
044100140903
044200140903         // -?Ricerca virgola (",") e Conteggio decimali (max 2)?
044300140903         wPosVirg = %scan( ',' : wValoreA );
044400140903
044500140903         // -?Campo numerico intero?
044600140903         If  wPosVirg = *zero;
044700140903
044800140903           // -?Trovati caratteri NON numerici?
044900140903           if  %check( c_Digits : %trimR( wValoreA ) ) > *zero;
045000140903             wErr = 3;
045100140903             exsr  sr_PrintErr;
045200140903             leavesr;
045300140903           endif;
045400140903           // -?Valorizzazione dato numerico?
045500140903           wValoreN = %int( %trimR( wValoreA ) );
045600140903
045700140903         // -?Campo numerico con decimali?
045800140903         Else;
045900140903
046000140903           For  wNumDec = 1  To  3;
046100140903             if  %subst( wValoreA : wPosVirg + wNumDec ) = *blank;
046200140903               leave;
046300140903             endif;
046400140903           EndFor;
046500140903           wNumDec -= 1;
046600140903
046700140903           Select;
046800140903
046900140903             When  wNumDec = *zero;
047000140903               // -?Trovati caratteri NON numerici?
047100140903               if  %check( c_Digits : %subst( wValoreA : 1 :
047200140903                                              wPosVirg - 1 ) ) > *zero;
047300140903                 wErr = 3;
047400140903                 exsr  sr_PrintErr;
047500140903                 leavesr;
047600140903               endif;
047700140903               // -?Valorizzazione dato numerico?
047800140903               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) );
047900140903
048000140903             When  wNumDec = 1;
048100140903               // -?Trovati caratteri NON numerici?
048200140903               if  %check( c_Digits : %subst( wValoreA : 1 :
048300140903                                              wPosVirg - 1 ) ) > *zero
048400140903                   or
048500140903                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
048600140903                                              1 ) ) > *zero;
048700140903                 wErr = 3;
048800140903                 exsr  sr_PrintErr;
048900140903                 leavesr;
049000140903               endif;
049100140903               // -?Valorizzazione dato numerico?
049200140903               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
049300140903                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
049400140903                                          1 ) ) / 10 );
049500140903
049600140903             Other;
049700140903               // -?Trovati caratteri NON numerici?
049800140903               if  %check( c_Digits : %subst( wValoreA : 1 :
049900140903                                              wPosVirg - 1 ) ) > *zero
050000140903                   or
050100140903                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
050200140903                                              2 ) ) > *zero;
050300140903                 wErr = 3;
050400140903                 exsr  sr_PrintErr;
050500140903                 leavesr;
050600140903               endif;
050700140903               // -?Valorizzazione dato numerico?
050800140903               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
050900140903                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
051000140903                                          2 ) ) / 100 );
051100140903
051200140903           EndSl;
051300140903
051400140903         EndIf;
051500140903
051600140903         // -?Campo numerico troppo lungo?
051700140903         if  ( wPosVirg > *zero  and
051800140903               %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > wLungh )  OR
051900140903             ( wPosVirg = *zero  and
052000140903               %len( %trim( wValoreA ) ) > wLungh );
052100140903           wErr = 4;
052200140903           exsr  sr_PrintErr;
052300140903           leavesr;
052400140903         endif;
052500140903
052600140903         // -?Campo negativo?
052700140903         if  $Negativo;
052800140903           wValoreN *= (-1);
052900140903         endif;
053000140903
053100140903         //// -?Percentuale?
053200140903         //if  $Perc;
053300140903         //  ...;
053400140903         //endif;
053500140903
053600140903       ENDSR;
053700140903
053800140903       //--------------------------------------------------------------
053900140903       //?Stampa segnalazione dell'errore rilevato.                    ?
054000140903       //--------------------------------------------------------------
054100140903       BEGSR  sr_PrintErr;
054200140903
054300140903         // -?Apertura del printer-file?
054400140903         if  Not %open(QSYSPRT);
054500140903           Qcmd = 'OVRPRTF file(QSYSPRT) +
054600140903                           usrdta(''ACPO01*ERR'') +
054700140903                           ovrscope(*actgrpdfn)';
054800140903           exsr  sr_ExecCmd;
054900140903           open  QSYSPRT;
055000140903           except  PRTtxt;
055100140903         endif;
055200140903
055300140903         if  *inOF;
055400140903           except  PRTtxt;
055500140903           *inOF = *off;
055600140903         endif;
055700140903
055800140903         Select;
055900140903           When  wErr = 1;
056000140903             except  PrtErr1;
056100140903           When  wErr = 2;
056200140903             except  PrtErr2;
056300140903           When  wErr = 3;
056400140903             except  PrtErr3;
056500140903           When  wErr = 4;
056600140903             except  PrtErr4;
056700140903           When  wErr = 5;
056800140903             except  PrtErr5;
056900140903         EndSl;
057000140903
057100140903       ENDSR;
057200140903
057300140903       //--------------------------------------------------------------
057400140903       //?Operazioni finali.                                           ?
057500140903       //--------------------------------------------------------------
057600140903       BEGSR  sr_RoutEnd;
057700140903
057800140903         // -?Chiusura spool di stampa errori?
057900140903         if  %open(QSYSPRT);
058000140903           except  PrtEND;
058100140903           close  QSYSPRT;
058200140903         endif;
058300140903
058400140903         // -?Chiusura Work-File di output?
058500140903         if  %open(INATT);
058600140903           close  INATT;
058700140903         endif;
058800140903
058900140903         // -?Rimozione overridea al file di CRIBIS (BRT_SEDI)?
059000140903         Qcmd = 'DLTOVR file(INATT) +
059100140903                        lvl(*actgrpdfn)';
059200140903         exsr  sr_ExecCmd;
059300140903
059400140903         // -?Chiusura *pgm?
059500140903         return;
059600140903
059700140903       ENDSR;
059800140903
059900140903       //--------------------------------------------------------------
060000140903       //?Esecuzione del comando (già impostato).                      ?
060100140903       //--------------------------------------------------------------
060200140903       BEGSR  sr_ExecCmd;
060300140903
060400140903         clear Qcap0100;
060500140903         Qcabcsdh = *off;
060600140903         Qcapa    = *off;
060700140903         Qcacmdss = *off;
060800140903         Qcaerved = *allX'00';
060900140903
061000140903         clear Qusec;
061100140903         Qusbprv  = %size(Qusec);
061200140903
061300140903         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
061400140903                           %size(Qcap0100) : 'CPOP0100' : *omit :
061500140903                           0 : 0 : Qusec);
061600140903
061700140903         //if  Qusei <> *blank;
061800140903         //  ...;
061900140903         //endif;
062000140903
062100140903       ENDSR;
062200140903
062300140903      /end-free
062400140903
062500140903       //--------------------------------------------------------------
062600140903       //?S P O O L - F I L E S                                        ?
062700140903       //--------------------------------------------------------------
062800140903
062900140903     oQSYSPRT   e            PRTtxt            1
063000140903     o                       RSUT
063100140903     o                                        +   4 'LISTA ERRORI RILE-
063200140903     o                                              VATI IN FASE DI EL-
063300140903     o                                              ABORAZIONE DATI NE-
063400140903     o                                              L FILE'
063500140903     o                       wLibFile         +   1
063600140903     o                       SDSpgmName       +   4
063700140903     o                       *Date         y  +   2
063800140903     o          e            PRTtxt      0
063900140903     o                                          +24 'LISTA ERRORI RILE-
064000140903     o                                              VATI IN FASE DI EL-
064100140903     o                                              ABORAZIONE DATI NE-
064200140903     o                                              L FILE'
064300140903     o                       wLibFile         +   1
064400140903     o          e            PRTtxt      1
064500140903     o                       KNSIF
064600140903     o                       KNMUS            +   1
064700140903     o                                        +   3 '------------------
064800140903     o                                              -------------------
064900140903     o                                              -------------------
065000140903     o                                              -------------------
065100140903     o                                              ----------'
065200140903     o                                        +   4 'Pag.'
065300140903     o                       Page          z  +   0
065400140903     o                       wTime            +   4 '  :  :  '
065500140903      *
065600140903     o          e            PRTtxt      2
065700140903     o                                              'Potenziale '
065800140903     o                                        +   2 'Messaggio'
065900140903     o          e            PRTtxt      1
066000140903     o                                              '-----------'
066100140903     o                                        +   2 '---------'
066200140903      *
066300140903     o          e            PRTerr1     2
066400140903     o                                              'NON TROVATO IL +
066500140903     o                                               FILE'
066600140903     o                       wLibFile         +   1
066700140903      *
066800140903     o          e            PRTerr2     2
066900140903     o                       k_WAPcpo
067000140903     o                                        +   2 'Troppi caratteri ('
067100140903     o                       wLungh        z  +   0
067200140903     o                                        +   0 ') nel'
067300140903     o                       wCampo        z  +   1
067400140903     o                                        +   0 '° campo:'
067500140903     o                       $Campi(wCampo)   +   1
067600140903     o                                        +   1 '-'
067700140903     o                       wValoreA15       +   1
067800140903      *
067900140903     o          e            PRTerr3     2
068000140903     o                       k_WAPcpo
068100140903     o                                        +   2 'Dati NON numerici +
068200140903     o                                               nel'
068300140903     o                       wCampo        z  +   1
068400140903     o                                        +   0 '° campo:'
068500140903     o                       $Campi(wCampo)   +   1
068600140903     o                                        +   1 '-'
068700140903     o                       wValoreA15       +   1
068800140903      *
068900140903     o          e            PRTerr4     2
069000140903     o                       k_WAPcpo
069100140903     o                                        +   2 'Troppe cifre nel'
069200140903     o                       wCampo        z  +   1
069300140903     o                                        +   0 '° campo:'
069400140903     o                       $Campi(wCampo)   +   1
069500140903     o                                        +   1 '-'
069600140903     o                       wValoreA15       +   1
069700140903      *
069800140903     o          e            PRTerr5     2
069900140903     o                       k_WAPcpo
070000140903     o                                        +   2 'Potenziale NON tr-
070100140903     o                                              ovato in WFACPV0F.'
070200140903      *
070300140903     o          e            PRTend      2
070400140903     o                                        +  24 '***  Fine Lista  ***'
070500140903     o          e            PRTend      0
070600140903     o                                        +  24 '***  Fine Lista  ***'
070700140903
070800140903       //--------------------------------------------------------------
070900140903       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
071000140903       //--------------------------------------------------------------
071100140903
071200140903** --?$Campi -----------*?Nomi di campi da indicare in stampa?
071300140903ID BRT                      1
071400140903Codice Fiscale              2
071500140903Partita Iva                 3
071600140903Crif Number                 4
071700140903Natura Giuridica            5
071800140903Stato Impresa CRIBIS        6
071900140903Data inizio attività        7
