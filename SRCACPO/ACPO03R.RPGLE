000100140801       //==============================================================
000200140801       //?Acquisizione Potenziali: riempimento file WATECO0F con dati  ?
000300140801       //?                         ricevuti da CRIF (file ATECO)       ?
000400140801       //==============================================================
000500140801
000600140801       //--------------------------------------------------------------
000700140801       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800140801       //--------------------------------------------------------------
000900140801
001000140801     /*PRM  dbgview(*source)
001100140801     /*CMD  ovrdbf file(ATECO) tofile(EDPFG/ATECO) +
001200140801     /*CMD         ovrscope(*calllvl)
001300140801     /*END  dltovr file(ATECO) lvl(*)
001400140801     /*END
001500140801
001600140801       //--------------------------------------------------------------
001700140801       //?Specifiche di controllo.                                     ?
001800140801       //--------------------------------------------------------------
001900140801
002000140801     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100140801     h dftactgrp(*no)
002200140801
002300140801       //--------------------------------------------------------------
002400140801       //?Dichiarazione file.                                          ?
002500140801       //--------------------------------------------------------------
002600140801
002700140801       // -?Main Dati Potenziali ricevuti da CRIF?
002800140801     fATECO     if   e           k disk
002900140801     f                                     extfile(wLibFile)
003000140801     f                                     rename(ATECO : ATECO_)
003100140801     f                                     prefix('X')
003200140801     f                                     usropn
003300140801
003400140801       // -?WrkF Classificazione attività economiche ATECO?
003500140801     fWATECO0F  o    e             disk
003600140801     f                                     usropn
003700140801
003800140801       // -?Stampa segnalazioni di errore?
003900140801     fQSYSPRT   o    f  132        printer
004000140801     f                                     usropn
004100140801     f                                     oflind(*inOF)
004200140801
004300140801       //--------------------------------------------------------------
004400140801       //?Definizione costanti.                                        ?
004500140801       //--------------------------------------------------------------
004600140801
004700140801       // -?Carattere separatore tra i campi nel file ricevuto?
004800140801       //  ?(standard: ";", da noi richiesto; "]")?
004900140801     d c_SepChar       c                   const(';')
005000140801     d*// c_SepChar       c                   const(']')
005100140801
005200140801       // -?Costante per controllo "caratteri solo numerici"?
005300140801     d c_Digits        c                   const('0123456789')
005400140801
005500140801       // -?Costanti per conversione caratteri maiuscoli in minuscoli e viceversa?
005600140801     d c_Maiu          c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
005700140801     d c_Minu          c                   const('abcdefghijklmnopqrstuvwxyz')
005800140801
005900140801       //--------------------------------------------------------------
006000140801       //?Definizione schiere.                                         ?
006100140801       //--------------------------------------------------------------
006200140801
006300140801       // -?Descrizione campi per stampa errori?
006400140801     d $Campi          s             35    dim(  4)  ctdata  perrcd( 1)
006500140801
006600140801       //--------------------------------------------------------------
006700140801       //?Definizione aree dati.                                       ?
006800140801       //--------------------------------------------------------------
006900140801
007000140801       // -?Dati utente?
007100140801     d §AzUte        e ds                  extname(AZUTE00F)
007200140801     d                                     dtaara
007300140801     d §DatiUte      e ds                  extname(dDatiUte)
007400140801     d                                     dtaara
007500140801
007600140801       //--------------------------------------------------------------
007700140801       //?Definizione strutture dati.                                  ?
007800140801       //--------------------------------------------------------------
007900140801
008000140801       // -?Status ds?
008100140801     d Status         sds
008200140801     d   SDSpgmName      *proc
008300140801     d*//SDSparms        *parms
008400140801     d   SDSjobName          244    253                                         Job name
008500140801     d   SDSjobUser          254    263                                         User name
008600140801     d*//SDSjobNumber        264    269s 0                                      Job number
008700140801
008800140801       // -?Parametri ricevuti (forse)?
008900140801     d KPJBA         e ds                  inz
009000140801
009100140801       //--------------------------------------------------------------
009200140801       //?Definizione variabili globali.                               ?
009300140801       //--------------------------------------------------------------
009400140801
009500140801       // -?Parametri ricevuti (forse)?
009600140801     d wKPJBA          s                   like(KPJBA)
009700140801
009800140801       // -?Nome Libreria in cui si trova il file di CRIBIS (ATECO)?
009900140801     d wLibWF          s             10    inz('EDPFG     ')
010000140801
010100140801       // -?Nome esteso Libreria/File del file da elaborare?
010200140801     d wLibFile        s             21a   inz
010300140801
010400140801       // -?Flags Booleani?
010500140801     d $Negativo       s               n   inz
010600140801     d*// $Perc           s               n   inz
010700140801     d $Apici          s               n   inz
010800140801     d $DoppiApici     s               n   inz
010900140801
011000140801       // -?Campi di comodo?
011100140801     d wDate_Iso       s               d   inz  datfmt(*iso)
011200140801     d wDate_Eur       s               d   inz  datfmt(*eur)
011300140801     d wDate           s              8  0 inz
011400140801     d wTime           s              6  0 inz
011500140801     d wErr            s              3  0 inz
011600140801     d wAtecoCod       s                   inz  like(WATaCod2)
011700140801     d wValoreA1       s            127    inz
011800140801     d wValoreA2       s            127    inz
011900140801     d wCampo          s              3  0 inz
012000140801     d wPosI           s              4  0 inz
012100140801     d wPosF           s              4  0 inz
012200140801     d wPosW           s              4  0 inz
012300140801     d wTipo           s              1    inz
012400140801     d wValoreA        s            255    inz
012500140801     d wValoreN        s             17  2 inz
012600140801     d wLungh          s              3  0 inz
012700140801     d wPosVirg        s              3  0 inz
012800140801     d wNumDec         s              1  0 inz
012900140801     d wPosSep         s              3  0 inz
013000140801
013100140801       //--------------------------------------------------------------
013200140801       //?Definizione prototipi procedure.                             ?
013300140801       //--------------------------------------------------------------
013400140801
013500140801       // -?Reperimento dati utente?
013600140801     d TIBS34ds      e ds                  inz
013700140801      /copy gaitrasrc/srcProtoPR,TIBS34R
013800140801
013900140801       // -?Parametri API QCAPCMD (Process Commands)?
014000140801     d Qcmd            s           2048    inz  varying
014100140801      /copy qSysInc/qRpgleSrc,QCAPCMD
014200140801       // -?API QCAPCMD (Process Commands)?
014300140801      /copy gaitrasrc/srcProtoPR,QCAPCMD
014400140801
014500140801       // -?Parametri gestione errori API.?
014600140801      /copy qsysinc/qrpglesrc,QUSEC
014700140801
014800140801       //--------------------------------------------------------------
014900140801       //?Definizione key-list.                                        ?
015000140801       //--------------------------------------------------------------
015100140801
015200140801
015300140801       //--------------------------------------------------------------
015400140801       //?Riepilogo indicatori utilizzati.                             ?
015500140801       //--------------------------------------------------------------
015600140801
015700140801
015800140801       //--------------------------------------------------------------
015900140801       //?M A I N - L I N E                                            ?
016000140801       //--------------------------------------------------------------
016100140801
016200140801     c     *Entry        plist
016300140801     c                   parm                    wKPJBA
016400140801
016500140801      /free
016600140801
016700140801       // -?Operazioni iniziali?
016800140801       exsr sr_RoutInz;
016900140801
017000140801       // -?Ciclo di lettura file ATECO?
017100140801       read  ATECO_;
017200140801       if  %subst( xATECO : 1 : 26 ) = 'CODICE ATECO 2007;DESCRIZIONE;';
017300140801         read  ATECO_;
017400140801       endif;
017500140801
017600140801       DoW  Not %eof(ATECO);
017700140801
017800140801         exsr  sr_ElabCAE;
017900140801
018000140801         read  ATECO_;
018100140801
018200140801       EndDo;
018300140801
018400140801       // -?Operazioni finali?
018500140801       exsr sr_RoutEnd;
018600140801
018700140801       //--------------------------------------------------------------
018800140801       //?Operazioni iniziali.                                         ?
018900140801       //--------------------------------------------------------------
019000140801       BEGSR  sr_RoutInz;
019100140801
019200140801         // -?Impostazione chiusura?
019300140801         *inLR = *on;
019400140801
019500140801         // -?Ricezione parametri?
019600140801         if  %parms() > *zero;
019700140801           KPJBA = wKPJBA;
019800140801         else;
019900140801           knsif = *all'? ';
020000140801           knmus = SDSjobUser;
020100140801         endif;
020200140801
020300140801         // -?Reperimento data odierna in formato aaaammgg?
020400140801         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
020500140801         wDate_Iso = %date();
020600140801         wDate_Eur = wDate_Iso;
020700140801         wDate = %dec( wDate_Eur );
020800140801
020900140801         // -?Reperimento orario?
021000140801         wTime = %dec( %time() );
021100140801
021200140801         // -?Reperimento dati job?
021300140801         exsr  sr_DatiJob;
021400140801
021500140801         // -?Pulizia del file di Output?
021600140801         Qcmd = 'CLRPFM file(*libl/WATECO0F)';
021700140801         exsr  sr_ExecCmd;
021800140801         if Qusei <> *blank;
021900140801           wErr = 1;
022000140801           exsr  sr_PrintErr;
022100140801           exsr  sr_RoutEnd;
022200140801         endif;
022300140801
022400140801         open  WATECO0F;
022500140801
022600140801         // -?Impostazione libreria del file di CRIBIS (ATECO)?
022700140801         if  %parms() > 1  and  kpjbu <> *blank;
022800140801           wLibWF = %subst( kpjbu : 1 : 10);
022900140801         endif;
023000140801
023100140801         // -?Apertura del file di CRIBIS (ATECO)?
023200140801         wLibFile = %trim( wLibWF ) + '/ATECO';
023300140801         open(e)  ATECO;
023400140801         if %error();
023500140801           wErr = 2;
023600140801           exsr  sr_PrintErr;
023700140801           exsr  sr_RoutEnd;
023800140801         endif;
023900140801
024000140801       ENDSR;
024100140801
024200140801       //--------------------------------------------------------------
024300140801       //?Reperimento Dati del job (Utente/Operativi).                 ?
024400140801       //--------------------------------------------------------------
024500140801       BEGSR  sr_DatiJob;
024600140801
024700140801         in(e) §AzUte;
024800140801         if NOT %error;
024900140801           in(e) §DatiUte;
025000140801         endif;
025100140801         if %error or RSut = *blank;
025200140801           tibs34r ( tibs34ds );
025300140801           in §AzUte;
025400140801           in §DatiUte;
025500140801         endif;
025600140801
025700140801       ENDSR;
025800140801
025900140801       //--------------------------------------------------------------
026000140801       //?Elaborazione singola Classificazione Attività Economica ATECO?
026100140801       //--------------------------------------------------------------
026200140801       BEGSR  sr_ElabCAE;
026300140801
026400140801         clear  WATECO00;
026500140801
026600140801         clear  wPosI;
026700140801         clear  wPosF;
026800140801         clear  wCampo;
026900140801         clear  wAtecoCod;
027000140801
027100140801         // -?  1° campo:?
027200140801         //  ?Codice Ateco (senza ".")?
027300140801         wCampo += 1;
027400140801         wTipo   = 'X';        //?- codice Ateco 2007?
027500140801         wLungh  = %len( WATaCod1 );
027600140801         exsr  sr_GetFldVal;
027700140801         if  wErr > *zero;
027800140801           leavesr;
027900140801         endif;
028000140801         WATaCod1   = wValoreA;
028100140801         //wAtecoCod  = wValoreA;
028200140801         // -?  2° campo:?
028300140801         //  ?Codice Ateco (originale)?
028400140801         WATaCod2   = wAtecoCod;
028500140801         // -?  3° campo:?
028600140801         //  ?Descrizione Ateco 2007  ?
028700140801         wCampo += 1;
028800140801         wTipo   = 'A';        //?- alfanumerico?
028900140801         wLungh  = %len( WATaDes );
029000140801         exsr  sr_GetFldVal;
029100140801         if  wErr > *zero;
029200140801           leavesr;
029300140801         endif;
029400140801         WATaDes    = wValoreA;
029500140801         // -?  4° campo:?
029600140801         //  ?Descriz. Categoria Merc.?
029700140801         wCampo += 1;
029800140801         wTipo   = 'A';        //?- alfanumerico?
029900140801         wLungh  = %len( WATbDCat );
030000140801         exsr  sr_GetFldVal;
030100140801         if  wErr > *zero;
030200140801           leavesr;
030300140801         endif;
030400140801         WATbDCat   = wValoreA;
030500140801         // -?  5° campo:?
030600140801         //  ?Codice Categoria Merc.  ?
030700140801         wCampo += 1;
030800140801         wTipo   = 'N';        //?- numerico?
030900140801         wLungh  = %len( WATbCatM );
031000140801         exsr  sr_GetFldVal;
031100140801         if  wErr > *zero;
031200140801           leavesr;
031300140801         endif;
031400140801         WATbCatM   = wValoreN;
031500140801
031600140801
031700140801         // -?Scrittura record in WATECO0F?
031800140801         //______________
031900140801         WRITE  WATECO00;
032000140801         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
032100140801
032200140801       ENDSR;
032300140801
032400140801       //--------------------------------------------------------------
032500140801       //?Reperimento valore del campo in elaborazione.                ?
032600140801       //--------------------------------------------------------------
032700140801       BEGSR  sr_GetFldVal;
032800140801
032900140801         // -?Calcolo Posizioni Iniziale e Finale?
033000140801         wPosI = wPosF + 1;
033100140801         wPosF = %scan( c_SepChar : xATECO : wPosI );
033200140801
033300140801         if  wTipo = 'X';
033400140801           wAtecoCod = %subst( xATECO : wPosI : wPosF - wPosI );
033500140801         endif;
033600140801
033700140801         // -?Verifica esistenza punteggiatura NON separatrice (';')?
033800140801         //  ?SCARTANDO il controllo per l'ULTIMO campo del file?
033900140801         If  wPosF > *zero  and  wCampo < 4;
034000140801           DoW  %subst( xATECO : wPosF + 1 : 1 ) = *blank;
034100140801             wPosF = %scan( c_SepChar : xATECO : wPosF + 1 );
034200140801           EndDo;
034300140801         EndIf;
034400140801
034500140801         // -?Impostazione finale SE ultimo campo (senza ';' a dx)?
034600140801         if  wPosF = *zero;
034700140801           wPosF = %len( %trimr( xATECO ) ) + 1;
034800140801         endif;
034900140801
035000140801         // -?Estrazione dati da "convertire"?
035100140801         clear  wErr;
035200140801         clear  wValoreA;
035300140801         clear  wValoreN;
035400140801         clear  $Negativo;
035500140801         //clear  $Perc;
035600140801         // -?...SE reperiti dati?
035700140801         if  wPosF > wPosI;
035800140801           wValoreA = %trimL( %subst( xATECO : wPosI : wPosF - wPosI ) );
035900140801         endif;
036000140801
036100140801         Select;
036200140801
036300140801           // -?Campo vuoto?
036400140801           When  wValoreA = *blank;
036500140801             leavesr;
036600140801
036700140801           // -?Caratteri alfanumerici da convertire in Maiuscolo?
036800140801           When  wTipo = 'a';
036900140801             wValoreA = %xlate( c_Minu : c_Maiu : wValoreA );
037000140801
037100140801           // -?Caratteri numerici con segno e/o con imbolo "%"?
037200140801           When  wTipo = 'n';
037300140801             if  %scan( '-' : wValoreA ) > *zero;
037400140801               $Negativo = *on;
037500140801               %subst( wValoreA : %scan( '-' : wValoreA ) : 1 ) = ' ';
037600140801               wValoreA  = %trim( wValoreA );
037700140801             endif;
037800140801             //if  %scan( '%' : wValoreA ) > *zero;
037900140801             //  $Perc    = *on;
038000140801             //  %subst( wValoreA : %scan( '%' : wValoreA ) : 1 ) = ' ';
038100140801             //  wValoreA = %trim( wValoreA );
038200140801             //endif;
038300140801
038400140801           // -?Codice Ateco 2007 - con "separatori" particolari?
038500140801           //  ?(vedi, per esempio, "01.49.30" => 01493)?
038600140801           When  wTipo = 'X';
038700140801             wAtecoCod = wValoreA;
038800140801             wPosSep = %scan( '.' : wValoreA );
038900140801             doW  wPosSep > *zero;
039000140801               wValoreA = %subst( wValoreA : 1 : wPosSep - 1 ) +
039100140801                          %subst( wValoreA : wPosSep + 1 );
039200140801               wPosSep  = %scan( '.' : wValoreA );
039300140801             enddo;
039400140801             For  wPosSep = %len( %trimR( wValoreA ) )  By 1  DownTo 1;
039500140801               if  %subst( wValoreA : wPosSep : 1 ) = '0';
039600140801                 %subst( wValoreA : wPosSep : 1 ) = *blank;
039700140801               else;
039800140801                 leave;
039900140801               endif;
040000140801             EndFor;
040100140801
040200140801           // -?Caratteri numerici per data (con separatore "/")?
040300140801           When  wTipo = 'D';
040400140801             wPosSep = %scan( '/' : wValoreA );
040500140801             doW  wPosSep > *zero;
040600140801               wValoreA = %subst( wValoreA : 1 : wPosSep - 1 ) +
040700140801                          %subst( wValoreA : wPosSep + 1 );
040800140801               wPosSep = %scan( '/' : wValoreA );
040900140801             enddo;
041000140801
041100140801         EndSl;
041200140801
041300140801         Select;
041400140801
041500140801           // -?Caratteri alfanumerici di cui controllare la lunghezza?
041600140801           When  wTipo = 'A';
041700140801             exsr  sr_CtrlAlfa;
041800140801
041900140801           // -?Caratteri alfanumerici da convertire in Numerici e?
042000140801           //  ?di cui controllare i decimali?
042100140801           When  wTipo = 'N'  or  wTipo = 'n'  or  wTipo = 'D';
042200140801             exsr  sr_CtrlNum;
042300140801
042400140801         EndSl;
042500140801
042600140801       ENDSR;
042700140801
042800140801       //--------------------------------------------------------------
042900140801       //?Controlli valori per campo alfanumerico.                     ?
043000140801       //--------------------------------------------------------------
043100140801       BEGSR  sr_CtrlAlfa;
043200140801
043300140801         // -?Trovati troppi caratteri?
043400140801         if  %len( %trimR( wValoreA ) ) > wLungh;
043500140801           wLungh = %len( %trimR( wValoreA ) );
043600140801           wErr = 3;
043700140801           exsr  sr_PrintErr;
043800140801           leavesr;
043900140801         endif;
044000140801
044100140801       ENDSR;
044200140801
044300140801       //--------------------------------------------------------------
044400140801       //?Controlli valori per campo numerico.                         ?
044500140801       //--------------------------------------------------------------
044600140801       BEGSR  sr_CtrlNum;
044700140801
044800140801         // -?Ricerca virgola (",") e Conteggio decimali (max 2)?
044900140801         wPosVirg = %scan( ',' : wValoreA );
045000140801
045100140801         // -?Campo numerico intero?
045200140801         If  wPosVirg = *zero;
045300140801
045400140801           // -?Trovati caratteri NON numerici?
045500140801           if  %check( c_Digits : %trimR( wValoreA ) ) > *zero;
045600140801             wErr = 4;
045700140801             exsr  sr_PrintErr;
045800140801             leavesr;
045900140801           endif;
046000140801           // -?Valorizzazione dato numerico?
046100140801           wValoreN = %int( %trimR( wValoreA ) );
046200140801
046300140801         // -?Campo numerico con decimali?
046400140801         Else;
046500140801
046600140801           For  wNumDec = 1  To  3;
046700140801             if  %subst( wValoreA : wPosVirg + wNumDec ) = *blank;
046800140801               leave;
046900140801             endif;
047000140801           EndFor;
047100140801           wNumDec -= 1;
047200140801
047300140801           Select;
047400140801
047500140801             When  wNumDec = *zero;
047600140801               // -?Trovati caratteri NON numerici?
047700140801               if  %check( c_Digits : %subst( wValoreA : 1 :
047800140801                                              wPosVirg - 1 ) ) > *zero;
047900140801                 wErr = 4;
048000140801                 exsr  sr_PrintErr;
048100140801                 leavesr;
048200140801               endif;
048300140801               // -?Valorizzazione dato numerico?
048400140801               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) );
048500140801
048600140801             When  wNumDec = 1;
048700140801               // -?Trovati caratteri NON numerici?
048800140801               if  %check( c_Digits : %subst( wValoreA : 1 :
048900140801                                              wPosVirg - 1 ) ) > *zero
049000140801                   or
049100140801                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
049200140801                                              1 ) ) > *zero;
049300140801                 wErr = 4;
049400140801                 exsr  sr_PrintErr;
049500140801                 leavesr;
049600140801               endif;
049700140801               // -?Valorizzazione dato numerico?
049800140801               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
049900140801                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
050000140801                                          1 ) ) / 10 );
050100140801
050200140801             //When  wNumDec = 2;
050300140801             Other;
050400140801               // -?Trovati caratteri NON numerici?
050500140801               if  %check( c_Digits : %subst( wValoreA : 1 :
050600140801                                              wPosVirg - 1 ) ) > *zero
050700140801                   or
050800140801                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
050900140801                                              2 ) ) > *zero;
051000140801                 wErr = 4;
051100140801                 exsr  sr_PrintErr;
051200140801                 leavesr;
051300140801               endif;
051400140801               // -?Valorizzazione dato numerico?
051500140801               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
051600140801                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
051700140801                                          2 ) ) / 100 );
051800140801             //Other;
051900140801             //  wErr = 5;
052000140801             //  exsr  sr_PrintErr;
052100140801             //  leavesr;
052200140801
052300140801           EndSl;
052400140801
052500140801         EndIf;
052600140801
052700140801         // -?Campo numerico troppo lungo?
052800140801         if  ( wPosVirg > *zero  and
052900140801               %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > wLungh )  OR
053000140801             ( wPosVirg = *zero  and
053100140801               %len( %trim( wValoreA ) ) > wLungh );
053200140801             //( wLungh   = 3      and (
053300140801             //  wPosVirg > *zero  and
053400140801             //  %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > 3         or
053500140801             //  wPosVirg = *zero  and
053600140801             //  %len( %trim( wValoreA ) ) > 3 ) );
053700140801           wErr = 6;
053800140801           exsr  sr_PrintErr;
053900140801           leavesr;
054000140801         endif;
054100140801
054200140801         // -?Campo negativo?
054300140801         if  $Negativo;
054400140801           wValoreN *= (-1);
054500140801         endif;
054600140801
054700140801         //// -?Percentuale?
054800140801         //if  $Perc;
054900140801         //  ...;
055000140801         //endif;
055100140801
055200140801       ENDSR;
055300140801
055400140801       //--------------------------------------------------------------
055500140801       //?Stampa segnalazione dell'errore rilevato.                    ?
055600140801       //--------------------------------------------------------------
055700140801       BEGSR  sr_PrintErr;
055800140801
055900140801         // -?Apertura del printer-file?
056000140801         if  Not %open(QSYSPRT);
056100140801           Qcmd = 'OVRPRTF file(QSYSPRT) +
056200140801                           usrdta(''ACPO03*ERR'') +
056300140801                           ovrscope(*actgrpdfn)';
056400140801           exsr  sr_ExecCmd;
056500140801           open  QSYSPRT;
056600140801           except  PRTtxt;
056700140801         endif;
056800140801
056900140801         if  *inOF;
057000140801           except  PRTtxt;
057100140801           *inOF = *off;
057200140801         endif;
057300140801
057400140801         Select;
057500140801           When  wErr = 1;
057600140801             except  PrtErr1;
057700140801           When  wErr = 2;
057800140801             except  PrtErr2;
057900140801           When  wErr = 3;
058000140801             except  PrtErr3;
058100140801           When  wErr = 4;
058200140801             except  PrtErr4;
058300140801           When  wErr = 5;
058400140801             except  PrtErr5;
058500140801           When  wErr = 6;
058600140801             except  PrtErr6;
058700140801         EndSl;
058800140801
058900140801         if  wErr >= 3;
059000140801           wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
059100140801           wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
059200140801           except  PrtErrVAL;
059300140801         endif;
059400140801
059500140801       ENDSR;
059600140801
059700140801       //--------------------------------------------------------------
059800140801       //?Operazioni finali.                                           ?
059900140801       //--------------------------------------------------------------
060000140801       BEGSR  sr_RoutEnd;
060100140801
060200140801         // -?Chiusura spool di stampa errori?
060300140801         if  %open(QSYSPRT);
060400140801           except  PrtEND;
060500140801           close  QSYSPRT;
060600140801         endif;
060700140801
060800140801         // -?Chiusura Work-File di output?
060900140801         if  %open(ATECO);
061000140801           close ATECO;
061100140801         endif;
061200140801
061300140801         // -?Rimozione overridea al file di CRIBIS (ATECO)?
061400140801         Qcmd = 'DLTOVR file(ATECO) +
061500140801                        lvl(*actgrpdfn)';
061600140801         exsr  sr_ExecCmd;
061700140801
061800140801         // -?Chiusura *pgm?
061900140801         return;
062000140801
062100140801       ENDSR;
062200140801
062300140801       //--------------------------------------------------------------
062400140801       //?Esecuzione del comando (già impostato).                      ?
062500140801       //--------------------------------------------------------------
062600140801       BEGSR  sr_ExecCmd;
062700140801
062800140801         clear Qcap0100;
062900140801         Qcabcsdh = *off;
063000140801         Qcapa    = *off;
063100140801         Qcacmdss = *off;
063200140801         Qcaerved = *allX'00';
063300140801
063400140801         clear Qusec;
063500140801         Qusbprv  = %size(Qusec);
063600140801
063700140801         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
063800140801                           %size(Qcap0100) : 'CPOP0100' : *omit :
063900140801                           0 : 0 : Qusec);
064000140801
064100140801         //if  Qusei <> *blank;
064200140801         //  ...;
064300140801         //endif;
064400140801
064500140801       ENDSR;
064600140801
064700140801      /end-free
064800140801
064900140801       //--------------------------------------------------------------
065000140801       //?S P O O L - F I L E S                                        ?
065100140801       //--------------------------------------------------------------
065200140801
065300140801     oQSYSPRT   e            PRTtxt            1
065400140801     o                       RSUT
065500140801     o                                        +   4 'LISTA ERRORI RILE-
065600140801     o                                              VATI IN FASE DI EL-
065700140801     o                                              ABORAZIONE DATI NE-
065800140801     o                                              L FILE'
065900140801     o                       wLibFile         +   1
066000140801     o                       SDSpgmName       +   4
066100140801     o                       *Date         y  +   2
066200140801     o          e            PRTtxt      0
066300140801     o                                          +24 'LISTA ERRORI RILE-
066400140801     o                                              VATI IN FASE DI EL-
066500140801     o                                              ABORAZIONE DATI NE-
066600140801     o                                              L FILE'
066700140801     o                       wLibFile         +   1
066800140801     o          e            PRTtxt      1
066900140801     o                       KNSIF
067000140801     o                       KNMUS            +   1
067100140801     o                                        +   3 '------------------
067200140801     o                                              -------------------
067300140801     o                                              -------------------
067400140801     o                                              -------------------
067500140801     o                                              ----------'
067600140801     o                                        +   4 'Pag.'
067700140801     o                       Page          z  +   0
067800140801     o                       wTime            +   4 '  :  :  '
067900140801      *
068000140801     o          e            PRTtxt      2
068100140801     o                                              'CRIF Number    '
068200140801     o                                        +   2 'Messaggio'
068300140801     o          e            PRTtxt      1
068400140801     o                                              '---------------'
068500140801     o                                        +   2 '---------'
068600140801      *
068700140801     o          e            PRTerr1     2
068800140801     o                                              'NON RIUSCITA LA +
068900140801     o                                               PULIZIA DEL WORK-+
069000140801     o                                               FILE WATECO0F'
069100140801      *
069200140801     o          e            PRTerr2     2
069300140801     o                                              'NON TROVATO IL +
069400140801     o                                               FILE'
069500140903     o                       wLibFile         +   1
069600140801      *
069700140801     o          e            PRTerr3     2
069800140801     o                       wAtecoCod
069900140801     o                                        +   2 'Troppi caratteri ('
070000140801     o                       wLungh        z  +   0
070100140801     o                                        +   0 ') nel'
070200140801     o                       wCampo        z  +   1
070300140801     o                                        +   0 '° campo:'
070400140801     o                       $Campi(wCampo)   +   1
070500140801      *
070600140801     o          e            PRTerr4     2
070700140801     o                       wAtecoCod
070800140801     o                                        +   2 'Dati NON numerici +
070900140801     o                                               nel'
071000140801     o                       wCampo        z  +   1
071100140801     o                                        +   0 '° campo:'
071200140801     o                       $Campi(wCampo)   +   1
071300140801      *
071400140801     o          e            PRTerr5     2
071500140801     o                       wAtecoCod
071600140801     o                                        +   2 'Numero decimali ('
071700140801     o                       wNumDec       z  +   0
071800140801     o                                        +   0 ') errato nel'
071900140801     o                       wCampo        z  +   1
072000140801     o                                        +   0 '° campo:'
072100140801     o                       $Campi(wCampo)   +   1
072200140801      *
072300140801     o          e            PRTerr6     2
072400140801     o                       wAtecoCod
072500140801     o                                        +   2 'Troppe cifre ('
072600140801     o                       wLungh        z  +   0
072700140801     o                                        +   0 ') nel'
072800140801     o                       wCampo        z  +   1
072900140801     o                                        +   0 '° campo:'
073000140801     o                       $Campi(wCampo)   +   1
073100140801      *
073200140801     o          e            PRTerrVAL   1
073300140801     o                       wValoreA1        +   5
073400140801     o          e            PRTerrVAL   1
073500140801     o                       wValoreA2        +   5
073600140801      *
073700140801     o          e            PRTend      2
073800140801     o                                        +  24 '***  Fine Lista  ***'
073900140801     o          e            PRTend      0
074000140801     o                                        +  24 '***  Fine Lista  ***'
074100140801
074200140801       //--------------------------------------------------------------
074300140801       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
074400140801       //--------------------------------------------------------------
074500140801
074600140801** --?$Campi ---------------------*?Nomi di campi da indicare in stampa?
074700140801Codice Ateco 2007                    1
074800140801Descrizione Ateco 2007               2
074900140801Descriz. Categoria Merceologica BRT  3
075000140801Codice Categoria Merceologica BRT    4
