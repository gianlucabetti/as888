000100140728       //==============================================================
000200140728       //?Acquisizione Potenziali: riempimento file WFACP00F con dati  ?
000300140801       //?                         ricevuti da CRIBIS (file BRT_SEDI)  ?
000400140728       //==============================================================
000500140728
000600140728       //--------------------------------------------------------------
000700140728       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800140728       //--------------------------------------------------------------
000900140728
001000140728     /*PRM  dbgview(*source)
001100140801     /*CMD  ovrdbf file(BRT_SEDI) tofile(EDPFG/BRT_SEDI) +
001200140728     /*CMD         ovrscope(*calllvl)
001300140801     /*END  dltovr file(BRT_SEDI) lvl(*)
001400140728     /*END
001500140728
001600140728       //--------------------------------------------------------------
001700140728       //?Specifiche di controllo.                                     ?
001800140728       //--------------------------------------------------------------
001900140728
002000140728     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100140728     h dftactgrp(*no)
002200140728
002300140728       //--------------------------------------------------------------
002400140728       //?Dichiarazione file.                                          ?
002500140728       //--------------------------------------------------------------
002600140728
002700140728       // -?Dati ricevuti da CRIBIS?
002800140801     fBRT_SEDI  if   e           k disk
002900140728     f                                     extfile(wLibFile)
003000140801     f                                     rename(BRT_SEDI : BRTSEDI)
003100140728     f                                     prefix('X')
003200140728     f                                     usropn
003300140728
003400140728       // -?WrkF Acquisizione Potenziali con dati ricevuti da CRIBIS?
003500140728     fWFACP00F  o    e             disk
003600140728     f                                     usropn
003700140728
003800140728       // -?Stampa segnalazioni di errore?
003900140728     fQSYSPRT   o    f  132        printer
004000140728     f                                     usropn
004100140728     f                                     oflind(*inOF)
004200140728
004300140728       //--------------------------------------------------------------
004400140728       //?Definizione costanti.                                        ?
004500140728       //--------------------------------------------------------------
004600140731
004700140731       // -?Carattere separatore tra i campi nel file ricevuto?
004800140731       //  ?(standard: ";", da noi richiesto: "]")?
004900140731     d*// c_SepChar       c                   const(';')
005000140731     d c_SepChar       c                   const(']')
005100140731
005200140731      *// // -?Numero MAX di campi nel file?
005300140731      *// //  ?(per il controllo dell'eventuale esistenza del carattere?
005400140731      *// //  ?'; '/'] ' - seguito da uno spazio)?
005500140731     d*// c_MaxFldWF      c                   const(110)
005600140728
005700140728       // -?Costante per controllo "caratteri solo numerici"?
005800140728     d c_Digits        c                   const('0123456789')
005900140728
006000140728       // -?Costanti per conversione caratteri maiuscoli in minuscoli e viceversa?
006100140729     d c_Maiu          c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
006200140729     d c_Minu          c                   const('abcdefghijklmnopqrstuvwxyz')
006300140728
006400140728       //--------------------------------------------------------------
006500140728       //?Definizione schiere.                                         ?
006600140728       //--------------------------------------------------------------
006700140728
006800140728       // -?Descrizione campi per stampa errori?
006900140728     d $Campi          s             25    dim(110)  ctdata  perrcd( 1)
007000140728
007100140728       //--------------------------------------------------------------
007200140728       //?Definizione aree dati.                                       ?
007300140728       //--------------------------------------------------------------
007400140728
007500140728       // -?Dati utente?
007600140728     d §AzUte        e ds                  extname(AZUTE00F)
007700140728     d                                     dtaara
007800140728     d §DatiUte      e ds                  extname(dDatiUte)
007900140728     d                                     dtaara
008000140728
008100140728       //--------------------------------------------------------------
008200140728       //?Definizione strutture dati.                                  ?
008300140728       //--------------------------------------------------------------
008400140728
008500140728       // -?Status ds?
008600140728     d Status         sds
008700140728     d   SDSpgmName      *proc
008800140728     d*//SDSparms        *parms
008900140728     d   SDSjobName          244    253                                         Job name
009000140728     d   SDSjobUser          254    263                                         User name
009100140728     d*//SDSjobNumber        264    269s 0                                      Job number
009200140728
009300140729       // -?Parametri ricevuti (forse)?
009400140729     d KPJBA         e ds                  inz
009500140728
009600140728       //--------------------------------------------------------------
009700140728       //?Definizione variabili globali.                               ?
009800140728       //--------------------------------------------------------------
009900140729
010000140729       // -?Parametri ricevuti (forse)?
010100140729     d wKPJBA          s                   like(KPJBA)
010200140728
010300140801       // -?Nome Libreria in cui si trova il file di CRIBIS (BRT_SEDI)?
010400140728     d wLibWF          s             10    inz('EDPFG     ')
010500140728
010600140728       // -?Nome esteso Libreria/File del file da elaborare?
010700140728     d wLibFile        s             21a   inz
010800140729
010900140729       // -?Flags Booleani?
011000140729     d $Negativo       s               n   inz
011100140729     d $Perc           s               n   inz
011200140728
011300140728       // -?Campi di comodo?
011400140728     d wDate_Iso       s               d   inz  datfmt(*iso)
011500140728     d wDate_Eur       s               d   inz  datfmt(*eur)
011600140728     d wDate           s              8  0 inz
011700140728     d wTime           s              6  0 inz
011800140728     d wErr            s              3  0 inz
011900140729     d wPotenz         s             11    inz
012000140801     d wValoreA1       s            127    inz
012100140801     d wValoreA2       s            127    inz
012200140728     d wCampo          s              3  0 inz
012300140729     d wPosI           s              4  0 inz
012400140729     d wPosF           s              4  0 inz
012500140728     d wTipo           s              1    inz
012600140801     d wValoreA        s            255    inz
012700140728     d wValoreN        s             17  2 inz
012800140729     d wLungh          s              3  0 inz
012900140728     d wPosVirg        s              3  0 inz
013000140728     d wNumDec         s              1  0 inz
013100140801     d wPosSep         s              3  0 inz
013200140728
013300140728       //--------------------------------------------------------------
013400140728       //?Definizione prototipi procedure.                             ?
013500140728       //--------------------------------------------------------------
013600140728
013700140728       // -?Reperimento dati utente?
013800140728     d TIBS34ds      e ds                  inz
013900140728      /copy gaitrasrc/srcProtoPR,TIBS34R
014000140728
014100140728       // -?Parametri API QCAPCMD (Process Commands)?
014200140728     d Qcmd            s           2048    inz  varying
014300140728      /copy qSysInc/qRpgleSrc,QCAPCMD
014400140728       // -?API QCAPCMD (Process Commands)?
014500140728      /copy gaitrasrc/srcProtoPR,QCAPCMD
014600140728
014700140728       // -?Parametri gestione errori API.?
014800140728      /copy qsysinc/qrpglesrc,QUSEC
014900140728
015000140728       //--------------------------------------------------------------
015100140728       //?Definizione key-list.                                        ?
015200140728       //--------------------------------------------------------------
015300140728
015400140728
015500140728       //--------------------------------------------------------------
015600140728       //?Riepilogo indicatori utilizzati.                             ?
015700140728       //--------------------------------------------------------------
015800140728
015900140728
016000140728       //--------------------------------------------------------------
016100140728       //?M A I N - L I N E                                            ?
016200140728       //--------------------------------------------------------------
016300140728
016400140728     c     *Entry        plist
016500140729     c                   parm                    wKPJBA
016600140728
016700140728      /free
016800140728
016900140728       // -?Operazioni iniziali?
017000140728       exsr sr_RoutInz;
017100140728
017200140801       // -?Ciclo di lettura file BRT_SEDI?
017300140801       read  BrtSedi;
017400140801       if  %subst( xBRT_SEDI : 1 : 10 ) = 'ID' + c_SepChar +
017500140801                                          'ID BRT' + c_SepChar;
017600140801         read  BrtSedi;
017700140729       endif;
017800140728
017900140801       DoW  Not %eof(BRT_SEDI);
018000140728
018100140728         exsr  sr_ElabPot;
018200140728
018300140801         read  BrtSedi;
018400140728
018500140728       EndDo;
018600140728
018700140728       // -?Operazioni finali?
018800140728       exsr sr_RoutEnd;
018900140728
019000140728       //--------------------------------------------------------------
019100140728       //?Operazioni iniziali.                                         ?
019200140728       //--------------------------------------------------------------
019300140728       BEGSR  sr_RoutInz;
019400140728
019500140728         // -?Impostazione chiusura?
019600140728         *inLR = *on;
019700140729
019800140729         // -?Ricezione parametri?
019900140729         if  %parms() > *zero;
020000140729           KPJBA = wKPJBA;
020100140729         else;
020200140729           knsif = *all'? ';
020300140729           knmus = SDSjobUser;
020400140729         endif;
020500140728
020600140728         // -?Reperimento data odierna in formato aaaammgg?
020700140728         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
020800140728         wDate_Iso = %date();
020900140728         wDate_Eur = wDate_Iso;
021000140728         wDate = %dec( wDate_Eur );
021100140728
021200140728         // -?Reperimento orario?
021300140728         wTime = %dec( %time() );
021400140728
021500140728         // -?Reperimento dati job?
021600140728         exsr  sr_DatiJob;
021700140728
021800140728         // -?Pulizia del file di Output?
021900140728         Qcmd = 'CLRPFM file(*libl/WFACP00F)';
022000140728         exsr  sr_ExecCmd;
022100140728         if Qusei <> *blank;
022200140728           wErr = 1;
022300140728           exsr  sr_PrintErr;
022400140728           exsr  sr_RoutEnd;
022500140728         endif;
022600140728
022700140728         open  WFACP00F;
022800140728
022900140801         // -?Impostazione libreria del file di CRIBIS (BRT_SEDI)?
023000140728         if  %parms() > 1  and  kpjbu <> *blank;
023100140728           wLibWF = %subst( kpjbu : 1 : 10);
023200140728         endif;
023300140728
023400140801         // -?Apertura del file di CRIBIS (BRT_SEDI)?
023500140801         wLibFile = %trim( wLibWF ) + '/BRT_SEDI';
023600140801         open(e)  BRT_SEDI;
023700140729         if %error();
023800140728           wErr = 2;
023900140728           exsr  sr_PrintErr;
024000140728           exsr  sr_RoutEnd;
024100140728         endif;
024200140728
024300140728       ENDSR;
024400140728
024500140728       //--------------------------------------------------------------
024600140728       //?Reperimento Dati del job (Utente/Operativi).                 ?
024700140728       //--------------------------------------------------------------
024800140728       BEGSR  sr_DatiJob;
024900140728
025000140728         in(e) §AzUte;
025100140728         if NOT %error;
025200140728           in(e) §DatiUte;
025300140728         endif;
025400140728         if %error or RSut = *blank;
025500140728           tibs34r ( tibs34ds );
025600140728           in §AzUte;
025700140728           in §DatiUte;
025800140728         endif;
025900140728
026000140728       ENDSR;
026100140728
026200140728       //--------------------------------------------------------------
026300140728       //?Elaborazione singolo cliente Potenziale.                     ?
026400140728       //--------------------------------------------------------------
026500140728       BEGSR  sr_ElabPot;
026600140728
026700140729         clear  WFACP000;
026800140729
026900140728         clear  wPosI;
027000140729         clear  wPosF;
027100140728         clear  wCampo;
027200140729         clear  wPotenz;
027300140729
027400140801         // -?  1° campo:?
027500140801         //  ?ID CRIBIS               ?
027600140728         wCampo += 1;
027700140728         wTipo   = 'N';        //?- numerico?
027800140729         wLungh  = %len( WAPidCri );
027900140728         exsr  sr_GetFldVal;
028000140729         if  wErr > *zero;
028100140729           leavesr;
028200140729         endif;
028300140728         WAPidCri   = wValoreN;
028400140729         wPotenz = wValoreA;
028500140801         // -?  2° campo:?
028600140801         //  ?ID BRT                  ?
028700140728         wCampo += 1;
028800140728         wTipo   = 'N';        //?- numerico?
028900140729         wLungh  = %len( WAPcpo );
029000140728         exsr  sr_GetFldVal;
029100140729         if  wErr > *zero;
029200140729           leavesr;
029300140729         endif;
029400140728         WAPcpo     = wValoreN;
029500140729         //if  wValoreA <> *blank  and  %trim( wValoreA ) <> *zero;
029600140729         //  wPotenz = wValoreA;
029700140729         //endif;
029800140801         // -?  3° campo:?
029900140801         //  ?Ragione sociale BRT     ?
030000140728         wCampo += 1;
030100140728         wTipo   = 'A';        //?- alfanumerico?
030200140729         wLungh  = %len( WAPrag );
030300140728         exsr  sr_GetFldVal;
030400140729         if  wErr > *zero;
030500140729           leavesr;
030600140729         endif;
030700140728         WAPrag     = wValoreA;
030800140801         // -?  4° campo:?
030900140801         //  ?Partita Iva BRT         ?
031000140728         wCampo += 1;
031100140728         wTipo   = 'A';        //?- alfanumerico?
031200140729         wLungh  = %len( WAPcpi );
031300140728         exsr  sr_GetFldVal;
031400140729         if  wErr > *zero;
031500140729           leavesr;
031600140729         endif;
031700140728         WAPcpi     = wValoreA;
031800140801         // -?  5° campo:?
031900140801         //  ?Codice Fiscale BRT      ?
032000140728         wCampo += 1;
032100140728         wTipo   = 'A';        //?- alfanumerico?
032200140729         wLungh  = %len( WAPcfi );
032300140728         exsr  sr_GetFldVal;
032400140729         if  wErr > *zero;
032500140729           leavesr;
032600140729         endif;
032700140728         WAPcfi     = wValoreA;
032800140801         // -?  6° campo:?
032900140801         //  ?Indirizzo BRT           ?
033000140728         wCampo += 1;
033100140728         wTipo   = 'A';        //?- alfanumerico?
033200140729         wLungh  = %len( WAPind );
033300140728         exsr  sr_GetFldVal;
033400140729         if  wErr > *zero;
033500140729           leavesr;
033600140729         endif;
033700140728         WAPind     = wValoreA;
033800140801         // -?  7° campo:?
033900140801         //  ?Comune BRT              ?
034000140728         wCampo += 1;
034100140728         wTipo   = 'A';        //?- alfanumerico?
034200140729         wLungh  = %len( WAPloc );
034300140728         exsr  sr_GetFldVal;
034400140729         if  wErr > *zero;
034500140729           leavesr;
034600140729         endif;
034700140728         WAPloc     = wValoreA;
034800140801         // -?  8° campo:?
034900140801         //  ?Cap BRT                 ?
035000140728         wCampo += 1;
035100140728         wTipo   = 'A';        //?- alfanumerico?
035200140729         wLungh  = %len( WAPcap );
035300140728         exsr  sr_GetFldVal;
035400140729         if  wErr > *zero;
035500140729           leavesr;
035600140729         endif;
035700140728         WAPcap     = wValoreA;
035800140801         // -?  9° campo:?
035900140801         //  ?Provincia BRT           ?
036000140728         wCampo += 1;
036100140728         wTipo   = 'A';        //?- alfanumerico?
036200140729         wLungh  = %len( WAPprv );
036300140728         exsr  sr_GetFldVal;
036400140729         if  wErr > *zero;
036500140729           leavesr;
036600140729         endif;
036700140728         WAPprv     = wValoreA;
036800140801         // -? 10° campo:?
036900140801         //  ?Provincia estesa BRT    ?
037000140728         wCampo += 1;
037100140728         wTipo   = 'a';        //?- alfanumerico anche minuscolo?
037200140729         wLungh  = %len( WAPprvDes );
037300140728         exsr  sr_GetFldVal;
037400140729         if  wErr > *zero;
037500140729           leavesr;
037600140729         endif;
037700140728         WAPprvDes  = wValoreA;
037800140801         // -? 11° campo:?
037900140801         //  ?Nazione BRT             ?
038000140728         wCampo += 1;
038100140728         wTipo   = 'A';        //?- alfanumerico?
038200140729         wLungh  = %len( WAPnaz );
038300140728         exsr  sr_GetFldVal;
038400140729         if  wErr > *zero;
038500140729           leavesr;
038600140729         endif;
038700140728         WAPnaz     = wValoreA;
038800140801         // -? 12° campo:?
038900140801         //  ?Telefono BRT            ?
039000140728         wCampo += 1;
039100140728         wTipo   = 'A';        //?- alfanumerico?
039200140729         wLungh  = %len( WAPtel );
039300140728         exsr  sr_GetFldVal;
039400140729         if  wErr > *zero;
039500140729           leavesr;
039600140729         endif;
039700140728         WAPtel     = wValoreA;
039800140801         // -? 13° campo:?
039900140801         //  ?Tipo indirizzo BRT      ?
040000140728         wCampo += 1;
040100140728         wTipo   = 'A';        //?- alfanumerico?
040200140729         wLungh  = %len( WAPtipInd );
040300140728         exsr  sr_GetFldVal;
040400140729         if  wErr > *zero;
040500140729           leavesr;
040600140729         endif;
040700140728         WAPtipInd  = wValoreA;
040800140801         // -? 14° campo:?
040900140801         //  ?DUNS BRT                ?
041000140728         wCampo += 1;
041100140728         wTipo   = 'A';        //?- alfanumerico?
041200140729         wLungh  = %len( WAPduns );
041300140728         exsr  sr_GetFldVal;
041400140729         if  wErr > *zero;
041500140729           leavesr;
041600140729         endif;
041700140728         WAPduns    = wValoreA;
041800140801         // -? 15° campo:?
041900140801         //  ?Crif Number             ?
042000140728         wCampo += 1;
042100140729         wTipo   = 'A';        //?- numerico?
042200140729         wLungh  = %len( WAPcrif );
042300140728         exsr  sr_GetFldVal;
042400140729         if  wErr > *zero;
042500140729           leavesr;
042600140729         endif;
042700140729         WAPcrif    = wValoreA;
042800140801         // -? 16° campo:?
042900140801         //  ?OOB Indicator           ?
043000140728         wCampo += 1;
043100140728         wTipo   = 'A';        //?- alfanumerico?
043200140729         wLungh  = %len( WAPoob );
043300140728         exsr  sr_GetFldVal;
043400140729         if  wErr > *zero;
043500140729           leavesr;
043600140729         endif;
043700140728         WAPoob     = wValoreA;
043800140801         // -? 17° campo:?
043900140801         //  ?Compravendita           ?
044000140728         wCampo += 1;
044100140728         wTipo   = 'A';        //?- alfanumerico?
044200140729         wLungh  = %len( WAPcmpVnd );
044300140728         exsr  sr_GetFldVal;
044400140729         if  wErr > *zero;
044500140729           leavesr;
044600140729         endif;
044700140728         WAPcmpVnd  = wValoreA;
044800140801         // -? 18° campo:?
044900140801         //  ?Confidence code         ?
045000140728         wCampo += 1;
045100140728         wTipo   = 'A';        //?- alfanumerico?
045200140729         wLungh  = %len( WAPconf );
045300140728         exsr  sr_GetFldVal;
045400140729         if  wErr > *zero;
045500140729           leavesr;
045600140729         endif;
045700140728         WAPconf    = wValoreA;
045800140801         // -? 19° campo:?
045900140801         //  ?Name similarity %       ?
046000140728         wCampo += 1;
046100140729         wTipo   = 'n';        //?- numerico con "%"?
046200140729         wLungh  = %len( WAPsimN );
046300140728         exsr  sr_GetFldVal;
046400140729         if  wErr > *zero;
046500140729           leavesr;
046600140729         endif;
046700140728         WAPsimN    = wValoreN;
046800140801         // -? 20° campo:?
046900140801         //  ?City similarity %       ?
047000140728         wCampo += 1;
047100140729         wTipo   = 'n';        //?- numerico con "%"?
047200140729         wLungh  = %len( WAPsimC );
047300140728         exsr  sr_GetFldVal;
047400140729         if  wErr > *zero;
047500140729           leavesr;
047600140729         endif;
047700140728         WAPsimC    = wValoreN;
047800140801         // -? 21° campo:?
047900140801         //  ?Address similarity %    ?
048000140728         wCampo += 1;
048100140729         wTipo   = 'n';        //?- numerico con "%"?
048200140729         wLungh  = %len( WAPsimA );
048300140728         exsr  sr_GetFldVal;
048400140729         if  wErr > *zero;
048500140729           leavesr;
048600140729         endif;
048700140728         WAPsimA    = wValoreN;
048800140801         // -? 22° campo:?
048900140801         //  ?Province similarity %   ?
049000140728         wCampo += 1;
049100140729         wTipo   = 'n';        //?- numerico con "%"?
049200140729         wLungh  = %len( WAPsimP );
049300140728         exsr  sr_GetFldVal;
049400140729         if  wErr > *zero;
049500140729           leavesr;
049600140729         endif;
049700140728         WAPsimP    = wValoreN;
049800140801         // -? 23° campo:?
049900140801         //  ?Name Match Grade %      ?
050000140728         wCampo += 1;
050100140729         wTipo   = 'n';        //?- numerico con "%"?
050200140729         wLungh  = %len( WAPmgrN );
050300140728         exsr  sr_GetFldVal;
050400140729         if  wErr > *zero;
050500140729           leavesr;
050600140729         endif;
050700140728         WAPmgrN    = wValoreN;
050800140801         // -? 24° campo:?
050900140801         //  ?City Match Grade %      ?
051000140728         wCampo += 1;
051100140729         wTipo   = 'n';        //?- numerico con "%"?
051200140729         wLungh  = %len( WAPmgrC );
051300140728         exsr  sr_GetFldVal;
051400140729         if  wErr > *zero;
051500140729           leavesr;
051600140729         endif;
051700140728         WAPmgrC    = wValoreN;
051800140801         // -? 25° campo:?
051900140801         //  ?Address Match Grade %   ?
052000140728         wCampo += 1;
052100140729         wTipo   = 'n';        //?- numerico con "%"?
052200140729         wLungh  = %len( WAPmgrA );
052300140728         exsr  sr_GetFldVal;
052400140729         if  wErr > *zero;
052500140729           leavesr;
052600140729         endif;
052700140728         WAPmgrA    = wValoreN;
052800140801         // -? 26° campo:?
052900140801         //  ?Province Match Grade %  ?
053000140728         wCampo += 1;
053100140729         wTipo   = 'n';        //?- numerico con "%"?
053200140729         wLungh  = %len( WAPmgrP );
053300140728         exsr  sr_GetFldVal;
053400140729         if  wErr > *zero;
053500140729           leavesr;
053600140729         endif;
053700140728         WAPmgrP    = wValoreN;
053800140801         // -? 27° campo:?
053900140801         //  ?Denominazione           ?
054000140728         wCampo += 1;
054100140728         wTipo   = 'A';        //?- alfanumerico?
054200140729         wLungh  = %len( WAPdeno );
054300140728         exsr  sr_GetFldVal;
054400140729         if  wErr > *zero;
054500140729           leavesr;
054600140729         endif;
054700140728         WAPdeno    = wValoreA;
054800140801         // -? 28° campo:?
054900140801         //  ?Indirizzo               ?
055000140728         wCampo += 1;
055100140728         wTipo   = 'A';        //?- alfanumerico?
055200140729         wLungh  = %len( WAPind2 );
055300140728         exsr  sr_GetFldVal;
055400140729         if  wErr > *zero;
055500140729           leavesr;
055600140729         endif;
055700140728         WAPind2    = wValoreA;
055800140801         // -? 29° campo:?
055900140801         //  ?Comune                  ?
056000140728         wCampo += 1;
056100140728         wTipo   = 'A';        //?- alfanumerico?
056200140729         wLungh  = %len( WAPloc2 );
056300140728         exsr  sr_GetFldVal;
056400140729         if  wErr > *zero;
056500140729           leavesr;
056600140729         endif;
056700140728         WAPloc2    = wValoreA;
056800140801         // -? 30° campo:?
056900140801         //  ?Cap                     ?
057000140728         wCampo += 1;
057100140728         wTipo   = 'N';        //?- numerico?
057200140729         wLungh  = %len( WAPcap2 );
057300140728         exsr  sr_GetFldVal;
057400140729         if  wErr > *zero;
057500140729           leavesr;
057600140729         endif;
057700140728         WAPcap2    = wValoreA;
057800140801         // -? 31° campo:?
057900140801         //  ?Regione                 ?
058000140728         wCampo += 1;
058100140728         wTipo   = 'A';        //?- alfanumerico?
058200140729         wLungh  = %len( WAPreg2 );
058300140728         exsr  sr_GetFldVal;
058400140729         if  wErr > *zero;
058500140729           leavesr;
058600140729         endif;
058700140728         WAPreg2    = wValoreA;
058800140801         // -? 32° campo:?
058900140801         //  ?Frazione                ?
059000140728         wCampo += 1;
059100140728         wTipo   = 'A';        //?- alfanumerico?
059200140729         wLungh  = %len( WAPfra2 );
059300140728         exsr  sr_GetFldVal;
059400140729         if  wErr > *zero;
059500140729           leavesr;
059600140729         endif;
059700140728         WAPfra2    = wValoreA;
059800140801         // -? 33° campo:?
059900140801         //  ?Provincia               ?
060000140728         wCampo += 1;
060100140728         wTipo   = 'A';        //?- alfanumerico?
060200140729         wLungh  = %len( WAPprv2 );
060300140728         exsr  sr_GetFldVal;
060400140729         if  wErr > *zero;
060500140729           leavesr;
060600140729         endif;
060700140728         WAPprv2    = wValoreA;
060800140801         // -? 34° campo:?
060900140801         //  ?Provincia estesa        ?
061000140728         wCampo += 1;
061100140729         wTipo   = 'a';        //?- alfanumerico anche minuscolo?
061200140729         wLungh  = %len( WAPprvDe2 );
061300140728         exsr  sr_GetFldVal;
061400140729         if  wErr > *zero;
061500140729           leavesr;
061600140729         endif;
061700140728         WAPprvDe2  = wValoreA;
061800140801         // -? 35° campo:?
061900140801         //  ?Codice Fiscale          ?
062000140728         wCampo += 1;
062100140728         wTipo   = 'A';        //?- alfanumerico?
062200140729         wLungh  = %len( WAPcfi2 );
062300140728         exsr  sr_GetFldVal;
062400140729         if  wErr > *zero;
062500140729           leavesr;
062600140729         endif;
062700140728         WAPcfi2    = wValoreA;
062800140801         // -? 36° campo:?
062900140801         //  ?Partita Iva             ?
063000140728         wCampo += 1;
063100140728         wTipo   = 'A';        //?- alfanumerico?
063200140729         wLungh  = %len( WAPcpi2 );
063300140728         exsr  sr_GetFldVal;
063400140729         if  wErr > *zero;
063500140729           leavesr;
063600140729         endif;
063700140728         WAPcpi2    = wValoreA;
063800140801         // -? 37° campo:?
063900140801         //  ?Telefono 1              ?
064000140728         wCampo += 1;
064100140728         wTipo   = 'A';        //?- alfanumerico?
064200140729         wLungh  = %len( WAPtel2 );
064300140729         exsr  sr_GetFldVal;
064400140729         if  wErr > *zero;
064500140729           leavesr;
064600140729         endif;
064700140728         WAPtel2    = wValoreA;
064800140801         // -? 38° campo:?
064900140801         //  ?Fax 1                   ?
065000140728         wCampo += 1;
065100140728         wTipo   = 'A';        //?- alfanumerico?
065200140729         wLungh  = %len( WAPfax2 );
065300140728         exsr  sr_GetFldVal;
065400140729         if  wErr > *zero;
065500140729           leavesr;
065600140729         endif;
065700140728         WAPfax2    = wValoreA;
065800140801         // -? 39° campo:?
065900140801         //  ?Sito Internet 1         ?
066000140728         wCampo += 1;
066100140728         wTipo   = 'A';        //?- alfanumerico?
066200140729         wLungh  = %len( WAPsito2 );
066300140728         exsr  sr_GetFldVal;
066400140729         if  wErr > *zero;
066500140729           leavesr;
066600140729         endif;
066700140728         WAPsito2   = wValoreA;
066800140801         // -? 40° campo:?
066900140801         //  ?Flag E-commerce         ?
067000140728         wCampo += 1;
067100140728         wTipo   = 'A';        //?- alfanumerico?
067200140729         wLungh  = %len( WAPecom );
067300140728         exsr  sr_GetFldVal;
067400140729         if  wErr > *zero;
067500140729           leavesr;
067600140729         endif;
067700140728         WAPecom    = wValoreA;
067800140801         // -? 41° campo:?
067900140801         //  ?Capitale sociale Euro   ?
068000140728         wCampo += 1;
068100140728         wTipo   = 'N';        //?- numerico?
068200140729         wLungh  = %len( WAPcapSoc );
068300140728         exsr  sr_GetFldVal;
068400140729         if  wErr > *zero;
068500140729           leavesr;
068600140729         endif;
068700140728         WAPcapSoc  = wValoreN;
068800140801         // -? 42° campo:?
068900140801         //  ?Anno Fondazione         ?
069000140728         wCampo += 1;
069100140728         wTipo   = 'N';        //?- numerico?
069200140729         wLungh  = %len( WAPaaFon );
069300140728         exsr  sr_GetFldVal;
069400140729         if  wErr > *zero;
069500140729           leavesr;
069600140729         endif;
069700140728         WAPaaFon   = wValoreN;
069800140801         // -? 43° campo:?
069900140801         //  ?Anno Costituzione       ?
070000140801         wCampo += 1;
070100140801         wTipo   = 'N';        //?- numerico?
070200140801         wLungh  = %len( WAPaaCos );
070300140801         exsr  sr_GetFldVal;
070400140801         if  wErr > *zero;
070500140801           leavesr;
070600140801         endif;
070700140801         WAPaaCos   = wValoreN;
070800140801         // -? 44° campo:?
070900140801         //  ?Data inizio attività    ?
071000140728         wCampo += 1;
071100140801         wTipo   = 'D';        //?- data (gg/mm/aaaa)?
071200140729         wLungh  = %len( WAPdtIni );
071300140728         exsr  sr_GetFldVal;
071400140729         if  wErr > *zero;
071500140729           leavesr;
071600140729         endif;
071700140728         WAPdtIni   = wValoreN;
071800140801         // -? 45° campo:?
071900140801         //  ?Ateco (2007)  1         ?
072000140728         wCampo += 1;
072100140728         wTipo   = 'A';        //?- alfanumerico?
072200140729         wLungh  = %len( WAPateco );
072300140728         exsr  sr_GetFldVal;
072400140729         if  wErr > *zero;
072500140729           leavesr;
072600140729         endif;
072700140728         WAPateco   = wValoreA;
072800140801         // -? 46° campo:?
072900140801         //  ?Descrizione codice ateco?
073000140728         wCampo += 1;
073100140728         wTipo   = 'A';        //?- alfanumerico?
073200140729         wLungh  = %len( WAPatecoDe );
073300140728         exsr  sr_GetFldVal;
073400140729         if  wErr > *zero;
073500140729           leavesr;
073600140729         endif;
073700140728         WAPatecoDe = wValoreA;
073800140801         // -? 47° campo:?
073900140801         //  ?Sic primario            ?
074000140728         wCampo += 1;
074100140728         wTipo   = 'A';        //?- alfanumerico?
074200140729         wLungh  = %len( WAPsicP );
074300140728         exsr  sr_GetFldVal;
074400140729         if  wErr > *zero;
074500140729           leavesr;
074600140729         endif;
074700140728         WAPsicP    = wValoreA;
074800140801         // -? 48° campo:?
074900140801         //  ?Descrizione Sic         ?
075000140728         wCampo += 1;
075100140728         wTipo   = 'A';        //?- alfanumerico?
075200140729         wLungh  = %len( WAPsicPdes );
075300140728         exsr  sr_GetFldVal;
075400140729         if  wErr > *zero;
075500140729           leavesr;
075600140729         endif;
075700140728         WAPsicPdes = wValoreA;
075800140801         // -? 49° campo:?
075900140801         //  ?Dipendenti              ?
076000140728         wCampo += 1;
076100140728         wTipo   = 'N';        //?- numerico?
076200140729         wLungh  = %len( WAPdip );
076300140728         exsr  sr_GetFldVal;
076400140729         if  wErr > *zero;
076500140729           leavesr;
076600140729         endif;
076700140728         WAPdip     = wValoreN;
076800140801         // -? 50° campo:?
076900140801         //  ?Natura giuridica        ?
077000140728         wCampo += 1;
077100140728         wTipo   = 'A';        //?- alfanumerico?
077200140729         wLungh  = %len( WAPnatG );
077300140728         exsr  sr_GetFldVal;
077400140729         if  wErr > *zero;
077500140729           leavesr;
077600140729         endif;
077700140728         WAPnatG    = wValoreA;
077800140801         // -? 51° campo:?
077900140801         //  ?CCIAA                   ?
078000140728         wCampo += 1;
078100140728         wTipo   = 'A';        //?- alfanumerico?
078200140729         wLungh  = %len( WAPcciaa );
078300140728         exsr  sr_GetFldVal;
078400140729         if  wErr > *zero;
078500140729           leavesr;
078600140729         endif;
078700140728         WAPcciaa   = wValoreA;
078800140801         // -? 52° campo:?
078900140801         //  ?REA                     ?
079000140728         wCampo += 1;
079100140728         wTipo   = 'A';        //?- alfanumerico?
079200140729         wLungh  = %len( WAPrea );
079300140728         exsr  sr_GetFldVal;
079400140729         if  wErr > *zero;
079500140729           leavesr;
079600140729         endif;
079700140728         WAPrea     = wValoreA;
079800140801         // -? 53° campo:?
079900140801         //  ?Oggetto sociale         ?
080000140728         wCampo += 1;
080100140728         wTipo   = 'A';        //?- alfanumerico?
080200140729         wLungh  = %len( WAPoggSoc );
080300140728         exsr  sr_GetFldVal;
080400140729         if  wErr > *zero;
080500140729           leavesr;
080600140729         endif;
080700140728         WAPoggSoc  = wValoreA;
080800140801         // -? 54° campo:?
080900140801         //  ?Stato Impresa Cribis    ?
081000140728         wCampo += 1;
081100140728         wTipo   = 'A';        //?- alfanumerico?
081200140729         wLungh  = %len( WAPstImp );
081300140728         exsr  sr_GetFldVal;
081400140729         if  wErr > *zero;
081500140729           leavesr;
081600140729         endif;
081700140728         WAPstImp   = wValoreA;
081800140801         // -? 55° campo:?
081900140801         //  ?Denominazione Esp 1     ?
082000140728         wCampo += 1;
082100140728         wTipo   = 'A';        //?- alfanumerico?
082200140729         wLungh  = %len( WAPdeno1 );
082300140728         exsr  sr_GetFldVal;
082400140729         if  wErr > *zero;
082500140729           leavesr;
082600140729         endif;
082700140728         WAPdeno1   = wValoreA;
082800140801         // -? 56° campo:?
082900140801         //  ?Cognome Esp 1           ?
083000140728         wCampo += 1;
083100140728         wTipo   = 'A';        //?- alfanumerico?
083200140729         wLungh  = %len( WAPcogn1 );
083300140728         exsr  sr_GetFldVal;
083400140729         if  wErr > *zero;
083500140729           leavesr;
083600140729         endif;
083700140728         WAPcogn1   = wValoreA;
083800140801         // -? 57° campo:?
083900140801         //  ?Nome Esp 1              ?
084000140728         wCampo += 1;
084100140728         wTipo   = 'A';        //?- alfanumerico?
084200140729         wLungh  = %len( WAPnome1 );
084300140728         exsr  sr_GetFldVal;
084400140729         if  wErr > *zero;
084500140729           leavesr;
084600140729         endif;
084700140728         WAPnome1   = wValoreA;
084800140801         // -? 58° campo:?
084900140801         //  ?Sesso Esp 1             ?
085000140728         wCampo += 1;
085100140728         wTipo   = 'A';        //?- alfanumerico?
085200140729         wLungh  = %len( WAPsess1 );
085300140728         exsr  sr_GetFldVal;
085400140729         if  wErr > *zero;
085500140729           leavesr;
085600140729         endif;
085700140728         WAPsess1   = wValoreA;
085800140801         // -? 59° campo:?
085900140801         //  ?Carica Esp 1            ?
086000140728         wCampo += 1;
086100140728         wTipo   = 'A';        //?- alfanumerico?
086200140729         wLungh  = %len( WAPcari1 );
086300140728         exsr  sr_GetFldVal;
086400140729         if  wErr > *zero;
086500140729           leavesr;
086600140729         endif;
086700140728         WAPcari1   = wValoreA;
086800140801         // -? 60° campo:?
086900140801         //  ?Denominazione Esp 2     ?
087000140728         wCampo += 1;
087100140728         wTipo   = 'A';        //?- alfanumerico?
087200140729         wLungh  = %len( WAPdeno2 );
087300140728         exsr  sr_GetFldVal;
087400140729         if  wErr > *zero;
087500140729           leavesr;
087600140729         endif;
087700140728         WAPdeno2   = wValoreA;
087800140801         // -? 61° campo:?
087900140801         //  ?Cognome Esp 2           ?
088000140728         wCampo += 1;
088100140728         wTipo   = 'A';        //?- alfanumerico?
088200140729         wLungh  = %len( WAPcogn2 );
088300140728         exsr  sr_GetFldVal;
088400140729         if  wErr > *zero;
088500140729           leavesr;
088600140729         endif;
088700140728         WAPcogn2   = wValoreA;
088800140801         // -? 62° campo:?
088900140801         //  ?Nome Esp 2              ?
089000140728         wCampo += 1;
089100140728         wTipo   = 'A';        //?- alfanumerico?
089200140729         wLungh  = %len( WAPnome2 );
089300140728         exsr  sr_GetFldVal;
089400140729         if  wErr > *zero;
089500140729           leavesr;
089600140729         endif;
089700140728         WAPnome2   = wValoreA;
089800140801         // -? 63° campo:?
089900140801         //  ?Sesso Esp 2             ?
090000140728         wCampo += 1;
090100140728         wTipo   = 'A';        //?- alfanumerico?
090200140729         wLungh  = %len( WAPsess2 );
090300140728         exsr  sr_GetFldVal;
090400140729         if  wErr > *zero;
090500140729           leavesr;
090600140729         endif;
090700140728         WAPsess2   = wValoreA;
090800140801         // -? 64° campo:?
090900140801         //  ?Carica Esp 2            ?
091000140728         wCampo += 1;
091100140728         wTipo   = 'A';        //?- alfanumerico?
091200140729         wLungh  = %len( WAPcari2 );
091300140728         exsr  sr_GetFldVal;
091400140729         if  wErr > *zero;
091500140729           leavesr;
091600140729         endif;
091700140728         WAPcari2   = wValoreA;
091800140801         // -? 65° campo:?
091900140801         //  ?Denominazione Esp 3     ?
092000140728         wCampo += 1;
092100140728         wTipo   = 'A';        //?- alfanumerico?
092200140729         wLungh  = %len( WAPdeno3 );
092300140728         exsr  sr_GetFldVal;
092400140729         if  wErr > *zero;
092500140729           leavesr;
092600140729         endif;
092700140728         WAPdeno3   = wValoreA;
092800140801         // -? 66° campo:?
092900140801         //  ?Cognome Esp 3           ?
093000140728         wCampo += 1;
093100140728         wTipo   = 'A';        //?- alfanumerico?
093200140729         wLungh  = %len( WAPcogn3 );
093300140728         exsr  sr_GetFldVal;
093400140729         if  wErr > *zero;
093500140729           leavesr;
093600140729         endif;
093700140728         WAPcogn3   = wValoreA;
093800140801         // -? 67° campo:?
093900140801         //  ?Nome Esp 3              ?
094000140728         wCampo += 1;
094100140728         wTipo   = 'A';        //?- alfanumerico?
094200140729         wLungh  = %len( WAPnome3 );
094300140728         exsr  sr_GetFldVal;
094400140729         if  wErr > *zero;
094500140729           leavesr;
094600140729         endif;
094700140728         WAPnome3   = wValoreA;
094800140801         // -? 68° campo:?
094900140801         //  ?Sesso Esp 3             ?
095000140728         wCampo += 1;
095100140728         wTipo   = 'A';        //?- alfanumerico?
095200140729         wLungh  = %len( WAPsess3 );
095300140728         exsr  sr_GetFldVal;
095400140729         if  wErr > *zero;
095500140729           leavesr;
095600140729         endif;
095700140728         WAPsess3   = wValoreA;
095800140801         // -? 69° campo:?
095900140801         //  ?Carica Esp 3            ?
096000140728         wCampo += 1;
096100140728         wTipo   = 'A';        //?- alfanumerico?
096200140729         wLungh  = %len( WAPcari3 );
096300140728         exsr  sr_GetFldVal;
096400140729         if  wErr > *zero;
096500140729           leavesr;
096600140729         endif;
096700140728         WAPcari3   = wValoreA;
096800140801         // -? 70° campo:?
096900140801         //  ?Denominazione Esp 4     ?
097000140728         wCampo += 1;
097100140728         wTipo   = 'A';        //?- alfanumerico?
097200140729         wLungh  = %len( WAPdeno4 );
097300140728         exsr  sr_GetFldVal;
097400140729         if  wErr > *zero;
097500140729           leavesr;
097600140729         endif;
097700140728         WAPdeno4   = wValoreA;
097800140801         // -? 71° campo:?
097900140801         //  ?Cognome Esp 4           ?
098000140728         wCampo += 1;
098100140728         wTipo   = 'A';        //?- alfanumerico?
098200140729         wLungh  = %len( WAPcogn4 );
098300140728         exsr  sr_GetFldVal;
098400140729         if  wErr > *zero;
098500140729           leavesr;
098600140729         endif;
098700140728         WAPcogn4   = wValoreA;
098800140801         // -? 72° campo:?
098900140801         // -?Nome Esp 4              ?
099000140728         wCampo += 1;
099100140728         wTipo   = 'A';        //?- alfanumerico?
099200140729         wLungh  = %len( WAPnome4 );
099300140728         exsr  sr_GetFldVal;
099400140729         if  wErr > *zero;
099500140729           leavesr;
099600140729         endif;
099700140728         WAPnome4   = wValoreA;
099800140801         // -? 73° campo:?
099900140801         //  ?Sesso Esp 4             ?
100000140728         wCampo += 1;
100100140728         wTipo   = 'A';        //?- alfanumerico?
100200140729         wLungh  = %len( WAPsess4 );
100300140728         exsr  sr_GetFldVal;
100400140729         if  wErr > *zero;
100500140729           leavesr;
100600140729         endif;
100700140728         WAPsess4   = wValoreA;
100800140801         // -? 74° campo:?
100900140801         //  ?Carica Esp 4            ?
101000140728         wCampo += 1;
101100140728         wTipo   = 'A';        //?- alfanumerico?
101200140729         wLungh  = %len( WAPcari4 );
101300140728         exsr  sr_GetFldVal;
101400140729         if  wErr > *zero;
101500140729           leavesr;
101600140729         endif;
101700140728         WAPcari4   = wValoreA;
101800140801         // -? 75° campo:?
101900140801         //  ?Denominazione Esp 5     ?
102000140728         wCampo += 1;
102100140728         wTipo   = 'A';        //?- alfanumerico?
102200140729         wLungh  = %len( WAPdeno5 );
102300140728         exsr  sr_GetFldVal;
102400140729         if  wErr > *zero;
102500140729           leavesr;
102600140729         endif;
102700140728         WAPdeno5   = wValoreA;
102800140801         // -? 76° campo:?
102900140801         //  ?Cognome Esp 5           ?
103000140728         wCampo += 1;
103100140728         wTipo   = 'A';        //?- alfanumerico?
103200140729         wLungh  = %len( WAPcogn5 );
103300140728         exsr  sr_GetFldVal;
103400140729         if  wErr > *zero;
103500140729           leavesr;
103600140729         endif;
103700140728         WAPcogn5   = wValoreA;
103800140801         // -? 77° campo:?
103900140801         //  ?Nome Esp 5              ?
104000140728         wCampo += 1;
104100140728         wTipo   = 'A';        //?- alfanumerico?
104200140729         wLungh  = %len( WAPnome5 );
104300140728         exsr  sr_GetFldVal;
104400140729         if  wErr > *zero;
104500140729           leavesr;
104600140729         endif;
104700140728         WAPnome5   = wValoreA;
104800140801         // -? 78° campo:?
104900140801         //  ?Sesso Esp 5             ?
105000140728         wCampo += 1;
105100140728         wTipo   = 'A';        //?- alfanumerico?
105200140729         wLungh  = %len( WAPsess5 );
105300140728         exsr  sr_GetFldVal;
105400140729         if  wErr > *zero;
105500140729           leavesr;
105600140729         endif;
105700140728         WAPsess5   = wValoreA;
105800140801         // -? 79° campo:?
105900140801         //  ?Carica Esp 5            ?
106000140728         wCampo += 1;
106100140728         wTipo   = 'A';        //?- alfanumerico?
106200140729         wLungh  = %len( WAPcari5 );
106300140728         exsr  sr_GetFldVal;
106400140729         if  wErr > *zero;
106500140729           leavesr;
106600140729         endif;
106700140728         WAPcari5   = wValoreA;
106800140801         // -? 80° campo:?
106900140801         //  ?Denominazione Esp 6     ?
107000140728         wCampo += 1;
107100140728         wTipo   = 'A';        //?- alfanumerico?
107200140729         wLungh  = %len( WAPdeno6 );
107300140728         exsr  sr_GetFldVal;
107400140729         if  wErr > *zero;
107500140729           leavesr;
107600140729         endif;
107700140728         WAPdeno6   = wValoreA;
107800140801         // -? 81° campo:?
107900140801         //  ?Cognome Esp 6           ?
108000140728         wCampo += 1;
108100140728         wTipo   = 'A';        //?- alfanumerico?
108200140729         wLungh  = %len( WAPcogn6 );
108300140728         exsr  sr_GetFldVal;
108400140729         if  wErr > *zero;
108500140729           leavesr;
108600140729         endif;
108700140728         WAPcogn6   = wValoreA;
108800140801         // -? 82° campo:?
108900140801         //  ?Nome Esp 6              ?
109000140728         wCampo += 1;
109100140728         wTipo   = 'A';        //?- alfanumerico?
109200140729         wLungh  = %len( WAPnome6 );
109300140728         exsr  sr_GetFldVal;
109400140729         if  wErr > *zero;
109500140729           leavesr;
109600140729         endif;
109700140728         WAPnome6   = wValoreA;
109800140801         // -? 83° campo:?
109900140801         //  ?Sesso Esp 6             ?
110000140728         wCampo += 1;
110100140728         wTipo   = 'A';        //?- alfanumerico?
110200140729         wLungh  = %len( WAPsess6 );
110300140728         exsr  sr_GetFldVal;
110400140729         if  wErr > *zero;
110500140729           leavesr;
110600140729         endif;
110700140728         WAPsess6   = wValoreA;
110800140801         // -? 84° campo:?
110900140801         //  ?Carica Esp 6            ?
111000140728         wCampo += 1;
111100140728         wTipo   = 'A';        //?- alfanumerico?
111200140729         wLungh  = %len( WAPcari6 );
111300140728         exsr  sr_GetFldVal;
111400140729         if  wErr > *zero;
111500140729           leavesr;
111600140729         endif;
111700140728         WAPcari6   = wValoreA;
111800140801         // -? 85° campo:?
111900140801         //  ?Denominazione Esp 7     ?
112000140728         wCampo += 1;
112100140728         wTipo   = 'A';        //?- alfanumerico?
112200140729         wLungh  = %len( WAPdeno7 );
112300140728         exsr  sr_GetFldVal;
112400140729         if  wErr > *zero;
112500140729           leavesr;
112600140729         endif;
112700140728         WAPdeno7   = wValoreA;
112800140801         // -? 86° campo:?
112900140801         //  ?Cognome Esp 7           ?
113000140728         wCampo += 1;
113100140728         wTipo   = 'A';        //?- alfanumerico?
113200140729         wLungh  = %len( WAPcogn7 );
113300140728         exsr  sr_GetFldVal;
113400140729         if  wErr > *zero;
113500140729           leavesr;
113600140729         endif;
113700140728         WAPcogn7   = wValoreA;
113800140801         // -? 87° campo:?
113900140801         //  ?Nome Esp 7              ?
114000140728         wCampo += 1;
114100140728         wTipo   = 'A';        //?- alfanumerico?
114200140729         wLungh  = %len( WAPnome7 );
114300140728         exsr  sr_GetFldVal;
114400140729         if  wErr > *zero;
114500140729           leavesr;
114600140729         endif;
114700140728         WAPnome7   = wValoreA;
114800140801         // -? 88° campo:?
114900140801         //  ?Sesso Esp 7             ?
115000140728         wCampo += 1;
115100140728         wTipo   = 'A';        //?- alfanumerico?
115200140729         wLungh  = %len( WAPsess7 );
115300140728         exsr  sr_GetFldVal;
115400140729         if  wErr > *zero;
115500140729           leavesr;
115600140729         endif;
115700140728         WAPsess7   = wValoreA;
115800140801         // -? 89° campo:?
115900140801         //  ?Carica Esp 7            ?
116000140728         wCampo += 1;
116100140728         wTipo   = 'A';        //?- alfanumerico?
116200140729         wLungh  = %len( WAPcari7 );
116300140728         exsr  sr_GetFldVal;
116400140729         if  wErr > *zero;
116500140729           leavesr;
116600140729         endif;
116700140728         WAPcari7   = wValoreA;
116800140801         // -? 90° campo:?
116900140801         //  ?Denominazione Esp 8     ?
117000140728         wCampo += 1;
117100140728         wTipo   = 'A';        //?- alfanumerico?
117200140729         wLungh  = %len( WAPdeno8 );
117300140728         exsr  sr_GetFldVal;
117400140729         if  wErr > *zero;
117500140729           leavesr;
117600140729         endif;
117700140728         WAPdeno8   = wValoreA;
117800140801         // -? 91° campo:?
117900140801         //  ?Cognome Esp 8           ?
118000140728         wCampo += 1;
118100140728         wTipo   = 'A';        //?- alfanumerico?
118200140729         wLungh  = %len( WAPcogn8 );
118300140728         exsr  sr_GetFldVal;
118400140729         if  wErr > *zero;
118500140729           leavesr;
118600140729         endif;
118700140728         WAPcogn8   = wValoreA;
118800140801         // -? 92° campo:?
118900140801         //  ?Nome Esp 8              ?
119000140728         wCampo += 1;
119100140728         wTipo   = 'A';        //?- alfanumerico?
119200140729         wLungh  = %len( WAPnome8 );
119300140728         exsr  sr_GetFldVal;
119400140729         if  wErr > *zero;
119500140729           leavesr;
119600140729         endif;
119700140728         WAPnome8   = wValoreA;
119800140801         // -? 93° campo:?
119900140801         //  ?Sesso Esp 8             ?
120000140728         wCampo += 1;
120100140728         wTipo   = 'A';        //?- alfanumerico?
120200140729         wLungh  = %len( WAPsess8 );
120300140728         exsr  sr_GetFldVal;
120400140729         if  wErr > *zero;
120500140729           leavesr;
120600140729         endif;
120700140728         WAPsess8   = wValoreA;
120800140801         // -? 94° campo:?
120900140801         //  ?Carica Esp 8            ?
121000140728         wCampo += 1;
121100140728         wTipo   = 'A';        //?- alfanumerico?
121200140729         wLungh  = %len( WAPcari8 );
121300140728         exsr  sr_GetFldVal;
121400140729         if  wErr > *zero;
121500140729           leavesr;
121600140729         endif;
121700140728         WAPcari8   = wValoreA;
121800140801         // -? 95° campo:?
121900140801         //  ?Fatturato Euro          ?
122000140728         wCampo += 1;
122100140801         wTipo   = 'n';        //?- numerico con segno?
122200140729         wLungh  = %len( WAPfatt );
122300140728         exsr  sr_GetFldVal;
122400140729         if  wErr > *zero;
122500140729           leavesr;
122600140729         endif;
122700140728         WAPfatt    = wValoreN;
122800140801         // -? 96° campo:?
122900140801         //  ?Anno Fatturato          ?
123000140728         wCampo += 1;
123100140728         wTipo   = 'N';        //?- numerico?
123200140729         wLungh  = %len( WAPaaFt );
123300140728         exsr  sr_GetFldVal;
123400140729         if  wErr > *zero;
123500140729           leavesr;
123600140729         endif;
123700140728         WAPaaFt    = wValoreN;
123800140801         // -? 97° campo:?
123900140801         //  ?Provenienza Fatturato   ?
124000140728         wCampo += 1;
124100140728         wTipo   = 'A';        //?- alfanumerico?
124200140729         wLungh  = %len( WAPpvnFt );
124300140728         exsr  sr_GetFldVal;
124400140729         if  wErr > *zero;
124500140729           leavesr;
124600140729         endif;
124700140728         WAPpvnFt   = wValoreA;
124800140801         // -? 98° campo:?
124900140801         //  ?Utile/Perdita           ?
125000140728         wCampo += 1;
125100140729         wTipo   = 'n';        //?- numerico con segno?
125200140729         wLungh  = %len( WAPutile );
125300140728         exsr  sr_GetFldVal;
125400140729         if  wErr > *zero;
125500140729           leavesr;
125600140729         endif;
125700140728         WAPutile   = wValoreN;
125800140801         // -? 99° campo:?
125900140801         //  ?% import                ?
126000140728         wCampo += 1;
126100140729         wTipo   = 'n';        //?- numerico con "%"?
126200140729         wLungh  = %len( WAPpImp );
126300140728         exsr  sr_GetFldVal;
126400140729         if  wErr > *zero;
126500140729           leavesr;
126600140729         endif;
126700140729         if  wValoreN > 999.99;
126800140729           wErr = 6;
126900140729           exsr  sr_PrintErr;
127000140729           leavesr;
127100140729         endif;
127200140728         WAPpImp    = wValoreN;
127300140801         // -?100° campo:?
127400140801         //  ?% export                ?
127500140728         wCampo += 1;
127600140729         wTipo   = 'n';        //?- numerico con "%"?
127700140729         wLungh  = %len( WAPpExp );
127800140728         exsr  sr_GetFldVal;
127900140729         if  wErr > *zero;
128000140729           leavesr;
128100140729         endif;
128200140729         if  wValoreN > 999.99;
128300140729           wErr = 6;
128400140729           exsr  sr_PrintErr;
128500140729           leavesr;
128600140729         endif;
128700140728         WAPpExp    = wValoreN;
128800140801         // -?101° campo:?
128900140801         //  ?Crif Number             ?
129000140801         wCampo += 1;
129100140801         wTipo   = 'A';        //?- alfanumerico?
129200140801         wLungh  = %len( WAPcmCrif );
129300140801         exsr  sr_GetFldVal;
129400140801         if  wErr > *zero;
129500140801           leavesr;
129600140801         endif;
129700140801         WAPcmCrif  = wValoreA;
129800140801         // -?102° campo:?
129900140801         //  ?Nome casa madre         ?
130000140728         wCampo += 1;
130100140728         wTipo   = 'A';        //?- alfanumerico?
130200140729         wLungh  = %len( WAPcmNom );
130300140728         exsr  sr_GetFldVal;
130400140729         if  wErr > *zero;
130500140729           leavesr;
130600140729         endif;
130700140728         WAPcmNom   = wValoreA;
130800140801         // -?103° campo:?
130900140801         //  ?Indirizzo casa madre    ?
131000140728         wCampo += 1;
131100140728         wTipo   = 'A';        //?- alfanumerico?
131200140729         wLungh  = %len( WAPcmInd );
131300140728         exsr  sr_GetFldVal;
131400140729         if  wErr > *zero;
131500140729           leavesr;
131600140729         endif;
131700140728         WAPcmInd   = wValoreA;
131800140801         // -?104° campo:?
131900140801         //  ?Cap casa madre          ?
132000140728         wCampo += 1;
132100140728         wTipo   = 'A';        //?- alfanumerico?
132200140729         wLungh  = %len( WAPcmCap );
132300140728         exsr  sr_GetFldVal;
132400140729         if  wErr > *zero;
132500140729           leavesr;
132600140729         endif;
132700140728         WAPcmCap   = wValoreA;
132800140801         // -?105° campo:?
132900140801         //  ?Città casa madre        ?
133000140728         wCampo += 1;
133100140728         wTipo   = 'A';        //?- alfanumerico?
133200140729         wLungh  = %len( WAPcmLoc );
133300140728         exsr  sr_GetFldVal;
133400140729         if  wErr > *zero;
133500140729           leavesr;
133600140729         endif;
133700140728         WAPcmLoc   = wValoreA;
133800140801         // -?106° campo:?
133900140801         //  ?Provincia casa madre    ?
134000140728         wCampo += 1;
134100140728         wTipo   = 'A';        //?- alfanumerico?
134200140729         wLungh  = %len( WAPcmPrv );
134300140728         exsr  sr_GetFldVal;
134400140729         if  wErr > *zero;
134500140729           leavesr;
134600140729         endif;
134700140728         WAPcmPrv   = wValoreA;
134800140801         // -?107° campo:?
134900140801         //  ?Nazione casa madre      ?
135000140728         wCampo += 1;
135100140728         wTipo   = 'A';        //?- alfanumerico?
135200140729         wLungh  = %len( WAPcmNaz );
135300140728         exsr  sr_GetFldVal;
135400140729         if  wErr > *zero;
135500140729           leavesr;
135600140729         endif;
135700140728         WAPcmNaz   = wValoreA;
135800140801         // -?108° campo:?
135900140801         //  ?FlagRating4             ?
136000140728         wCampo += 1;
136100140728         wTipo   = 'A';        //?- alfanumerico?
136200140729         wLungh  = %len( WAPfr4 );
136300140728         exsr  sr_GetFldVal;
136400140729         if  wErr > *zero;
136500140729           leavesr;
136600140729         endif;
136700140728         WAPfr4     = wValoreA;
136800140801         // -?109° campo:?
136900140801         //  ?% impex                 ?
137000140728         wCampo += 1;
137100140729         wTipo   = 'n';        //?- numerico con "%"?
137200140729         wLungh  = %len( WAPpImpEx );
137300140728         exsr  sr_GetFldVal;
137400140729         if  wErr > *zero;
137500140729           leavesr;
137600140729         endif;
137700140729         if  wValoreN > 999.99;
137800140729           wErr = 6;
137900140729           exsr  sr_PrintErr;
138000140729           leavesr;
138100140729         endif;
138200140729         WAPpImpEx  = wValoreN;
138300140801         // -?110° campo:?
138400140801         //  ?anno %impex             ?
138500140728         wCampo += 1;
138600140728         wTipo   = 'N';        //?- numerico?
138700140729         wLungh  = %len( WAPaImpEx );
138800140728         exsr  sr_GetFldVal;
138900140729         if  wErr > *zero;
139000140729           leavesr;
139100140729         endif;
139200140729         WAPaImpEx  = wValoreN;
139300140728
139400140728
139500140728         // -?Scrittura record in WFACP00F?
139600140728         //______________
139700140728         WRITE  WFACP000;
139800140728         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
139900140728
140000140728       ENDSR;
140100140728
140200140728       //--------------------------------------------------------------
140300140728       //?Reperimento valore del campo in elaborazione.                ?
140400140728       //--------------------------------------------------------------
140500140728       BEGSR  sr_GetFldVal;
140600140728
140700140728         // -?Calcolo Posizioni Iniziale e Finale?
140800140728         wPosI = wPosF + 1;
140900140801         wPosF = %scan( c_SepChar : xBRT_SEDI : wPosI );
141000140728         if  wPosF = *zero;
141100140801           wPosF = %len( %trimr( xBRT_SEDI ) ) + 1;
141200140728         endif;
141300140731
141400140731         //// -?Verifica esistenza punteggiatura NON separatrice ('; '/'] '')?
141500140731         ////  ?SCARTANDO il controllo dal PENULTIMO campo del file?
141600140731         //If  wPosF > *zero  and  wCampo < ( c_MaxFldWF - 1 );
141700140731         //  DoW  %subst( xProsp_Dati : wPosF + 1 : 1 ) = *blank;
141800140731         //    wPosF = %scan( c_SepChar : xProsp_Dati : wPosF + 1 );
141900140731         //  EndDo;
142000140731         //EndIf;
142100140728
142200140728         // -?Estrazione dati da "convertire"?
142300140729         clear  wErr;
142400140729         clear  wValoreA;
142500140729         clear  wValoreN;
142600140729         clear  $Negativo;
142700140729         clear  $Perc;
142800140728         // -?...SE reperiti dati?
142900140728         if  wPosF > wPosI;
143000140801           wValoreA = %trimL( %subst( xBRT_SEDI : wPosI : wPosF - wPosI ) );
143100140728         endif;
143200140728
143300140728         Select;
143400140728
143500140728           // -?Campo vuoto?
143600140728           When  wValoreA = *blank;
143700140729             leavesr;
143800140728
143900140728           // -?Caratteri alfanumerici da convertire in Maiuscolo?
144000140728           When  wTipo = 'a';
144100140729             wValoreA = %xlate( c_Minu : c_Maiu : wValoreA );
144200140729
144300140729           // -?Caratteri numerici con segno e/o con imbolo "%"?
144400140729           When  wTipo = 'n';
144500140729             if  %scan( '-' : wValoreA ) > *zero;
144600140729               $Negativo = *on;
144700140729               %subst( wValoreA : %scan( '-' : wValoreA ) : 1 ) = ' ';
144800140729               wValoreA  = %trim( wValoreA );
144900140729             endif;
145000140729             if  %scan( '%' : wValoreA ) > *zero;
145100140729               $Perc    = *on;
145200140729               %subst( wValoreA : %scan( '%' : wValoreA ) : 1 ) = ' ';
145300140729               wValoreA = %trim( wValoreA );
145400140729             endif;
145500140801
145600140801           // -?Caratteri numerici per data (con separatore "/")?
145700140801           When  wTipo = 'D';
145800140801             wPosSep = %scan( '/' : wValoreA );
145900140801             doW  wPosSep > *zero;
146000140801               wValoreA = %subst( wValoreA : 1 : wPosSep - 1 ) +
146100140801                          %subst( wValoreA : wPosSep + 1 );
146200140801               wPosSep = %scan( '/' : wValoreA );
146300140801             enddo;
146400140729
146500140729         EndSl;
146600140729
146700140729         Select;
146800140729
146900140729           // -?Caratteri alfanumerici di cui controllare la lunghezza?
147000140729           When  wTipo = 'A';
147100140729             exsr  sr_CtrlAlfa;
147200140728
147300140729           // -?Caratteri alfanumerici da convertire in Numerici e?
147400140729           //  ?di cui controllare i decimali?
147500140801           When  wTipo = 'N'  or  wTipo = 'n'  or  wTipo = 'D';
147600140729             exsr  sr_CtrlNum;
147700140728
147800140728         EndSl;
147900140728
148000140728       ENDSR;
148100140729
148200140729       //--------------------------------------------------------------
148300140729       //?Controlli valori per campo alfanumerico.                     ?
148400140729       //--------------------------------------------------------------
148500140729       BEGSR  sr_CtrlAlfa;
148600140729
148700140729         // -?Trovati troppi caratteri?
148800140801         //  ?(SOLO per campi lunghi MENO di 50 caratteri,?
148900140801         //  ?dai 50 in su: si tagliano!)?
149000140801         if  %len( %trimR( wValoreA ) ) > wLungh  and
149100140801             wLungh < 50;
149200140729           wLungh = %len( %trimR( wValoreA ) );
149300140729           wErr = 3;
149400140729           exsr  sr_PrintErr;
149500140729           leavesr;
149600140729         endif;
149700140729
149800140729       ENDSR;
149900140729
150000140729       //--------------------------------------------------------------
150100140729       //?Controlli valori per campo numerico.                         ?
150200140729       //--------------------------------------------------------------
150300140729       BEGSR  sr_CtrlNum;
150400140729
150500140729         // -?Ricerca virgola (",") e Conteggio decimali (max 2)?
150600140729         wPosVirg = %scan( ',' : wValoreA );
150700140729
150800140729         // -?Campo numerico intero?
150900140729         If  wPosVirg = *zero;
151000140729
151100140729           // -?Trovati caratteri NON numerici?
151200140729           if  %check( c_Digits : %trimR( wValoreA ) ) > *zero;
151300140729             wErr = 4;
151400140729             exsr  sr_PrintErr;
151500140729             leavesr;
151600140729           endif;
151700140729           // -?Valorizzazione dato numerico?
151800140729           wValoreN = %int( %trimR( wValoreA ) );
151900140729
152000140729         // -?Campo numerico con decimali?
152100140729         Else;
152200140729
152300140729           For  wNumDec = 1  To  3;
152400140729             if  %subst( wValoreA : wPosVirg + wNumDec ) = *blank;
152500140729               leave;
152600140729             endif;
152700140729           EndFor;
152800140729           wNumDec -= 1;
152900140729
153000140729           Select;
153100140729
153200140729             When  wNumDec = *zero;
153300140729               // -?Trovati caratteri NON numerici?
153400140729               if  %check( c_Digits : %subst( wValoreA : 1 :
153500140729                                              wPosVirg - 1 ) ) > *zero;
153600140729                 wErr = 4;
153700140729                 exsr  sr_PrintErr;
153800140729                 leavesr;
153900140729               endif;
154000140729               // -?Valorizzazione dato numerico?
154100140729               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) );
154200140729
154300140729             When  wNumDec = 1;
154400140729               // -?Trovati caratteri NON numerici?
154500140729               if  %check( c_Digits : %subst( wValoreA : 1 :
154600140729                                              wPosVirg - 1 ) ) > *zero
154700140729                   or
154800140729                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
154900140729                                              1 ) ) > *zero;
155000140729                 wErr = 4;
155100140729                 exsr  sr_PrintErr;
155200140729                 leavesr;
155300140729               endif;
155400140729               // -?Valorizzazione dato numerico?
155500140729               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
155600140729                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
155700140729                                          1 ) ) / 10 );
155800140729
155900140801             //When  wNumDec = 2;
156000140801             Other;
156100140729               // -?Trovati caratteri NON numerici?
156200140729               if  %check( c_Digits : %subst( wValoreA : 1 :
156300140729                                              wPosVirg - 1 ) ) > *zero
156400140729                   or
156500140729                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
156600140729                                              2 ) ) > *zero;
156700140729                 wErr = 4;
156800140729                 exsr  sr_PrintErr;
156900140729                 leavesr;
157000140729               endif;
157100140729               // -?Valorizzazione dato numerico?
157200140729               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
157300140729                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
157400140729                                          2 ) ) / 100 );
157500140801             //Other;
157600140801             //  wErr = 5;
157700140801             //  exsr  sr_PrintErr;
157800140801             //  leavesr;
157900140729
158000140729           EndSl;
158100140729
158200140729         EndIf;
158300140729
158400140729         // -?Campo numerico troppo lungo?
158500140729         if  ( wPosVirg > *zero  and
158600140729               %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > wLungh )  OR
158700140729             ( wPosVirg = *zero  and
158800140729               %len( %trim( wValoreA ) ) > wLungh );
158900140729             //( wLungh   = 3      and (
159000140729             //  wPosVirg > *zero  and
159100140729             //  %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > 3         or
159200140729             //  wPosVirg = *zero  and
159300140729             //  %len( %trim( wValoreA ) ) > 3 ) );
159400140729           wErr = 6;
159500140729           exsr  sr_PrintErr;
159600140729           leavesr;
159700140729         endif;
159800140729
159900140729         // -?Campo negativo?
160000140729         if  $Negativo;
160100140729           wValoreN *= (-1);
160200140729         endif;
160300140729
160400140729         //// -?Percentuale?
160500140729         //if  $Perc;
160600140729         //  ...;
160700140729         //endif;
160800140729
160900140729       ENDSR;
161000140728
161100140728       //--------------------------------------------------------------
161200140728       //?Stampa segnalazione dell'errore rilevato.                    ?
161300140728       //--------------------------------------------------------------
161400140728       BEGSR  sr_PrintErr;
161500140728
161600140728         // -?Apertura del printer-file?
161700140728         if  Not %open(QSYSPRT);
161800140728           Qcmd = 'OVRPRTF file(QSYSPRT) +
161900140728                           usrdta(''ACPO01*ERR'') +
162000140728                           ovrscope(*actgrpdfn)';
162100140728           exsr  sr_ExecCmd;
162200140728           open  QSYSPRT;
162300140728           except  PRTtxt;
162400140728         endif;
162500140728
162600140728         if  *inOF;
162700140728           except  PRTtxt;
162800140728           *inOF = *off;
162900140728         endif;
163000140728
163100140728         Select;
163200140728           When  wErr = 1;
163300140728             except  PrtErr1;
163400140728           When  wErr = 2;
163500140728             except  PrtErr2;
163600140728           When  wErr = 3;
163700140728             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
163800140728             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
163900140728             except  PrtErr3;
164000140728           When  wErr = 4;
164100140728             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
164200140728             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
164300140728             except  PrtErr4;
164400140729           When  wErr = 5;
164500140729             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
164600140729             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
164700140729             except  PrtErr5;
164800140729           When  wErr = 6;
164900140729             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
165000140729             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
165100140729             except  PrtErr6;
165200140728         EndSl;
165300140728
165400140728       ENDSR;
165500140728
165600140728       //--------------------------------------------------------------
165700140728       //?Operazioni finali.                                           ?
165800140728       //--------------------------------------------------------------
165900140728       BEGSR  sr_RoutEnd;
166000140729
166100140729         // -?Chiusura spool di stampa errori?
166200140729         if  %open(QSYSPRT);
166300140729           except  PrtEND;
166400140729           close  QSYSPRT;
166500140729         endif;
166600140729
166700140729         // -?Chiusura Work-File di output?
166800140801         if  %open(BRT_SEDI);
166900140801           close BRT_SEDI;
167000140729         endif;
167100140728
167200140801         // -?Rimozione overridea al file di CRIBIS (BRT_SEDI)?
167300140801         Qcmd = 'DLTOVR file(BRT_SEDI) +
167400140728                        lvl(*actgrpdfn)';
167500140728         exsr  sr_ExecCmd;
167600140728
167700140728         // -?Chiusura *pgm?
167800140728         return;
167900140728
168000140728       ENDSR;
168100140728
168200140728       //--------------------------------------------------------------
168300140728       //?Esecuzione del comando (già impostato).                      ?
168400140728       //--------------------------------------------------------------
168500140728       BEGSR  sr_ExecCmd;
168600140728
168700140728         clear Qcap0100;
168800140728         Qcabcsdh = *off;
168900140728         Qcapa    = *off;
169000140728         Qcacmdss = *off;
169100140728         Qcaerved = *allX'00';
169200140728
169300140728         clear Qusec;
169400140728         Qusbprv  = %size(Qusec);
169500140728
169600140728         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
169700140728                           %size(Qcap0100) : 'CPOP0100' : *omit :
169800140728                           0 : 0 : Qusec);
169900140728
170000140728         //if  Qusei <> *blank;
170100140728         //  ...;
170200140728         //endif;
170300140728
170400140728       ENDSR;
170500140728
170600140728      /end-free
170700140728
170800140728       //--------------------------------------------------------------
170900140728       //?S P O O L - F I L E S                                        ?
171000140728       //--------------------------------------------------------------
171100140728
171200140728     oQSYSPRT   e            PRTtxt            1
171300140728     o                       RSUT
171400140728     o                                        +   4 'LISTA ERRORI RILE-
171500140728     o                                              VATI IN FASE DI EL-
171600140728     o                                              ABORAZIONE DATI NE-
171700140728     o                                              L FILE'
171800140729     o                       wLibFile         +   1
171900140728     o                       SDSpgmName       +   4
172000140728     o                       *Date         y  +   2
172100140728     o          e            PRTtxt      0
172200140728     o                                          +24 'LISTA ERRORI RILE-
172300140728     o                                              VATI IN FASE DI EL-
172400140728     o                                              ABORAZIONE DATI NE-
172500140728     o                                              L FILE'
172600140729     o                       wLibFile         +   1
172700140728     o          e            PRTtxt      1
172800140728     o                       KNSIF
172900140728     o                       KNMUS            +   1
173000140729     o                                        +   3 '------------------
173100140728     o                                              -------------------
173200140728     o                                              -------------------
173300140728     o                                              -------------------
173400140729     o                                              ----------'
173500140729     o                                        +   4 'Pag.'
173600140728     o                       Page          z  +   0
173700140728     o                       wTime            +   4 '  :  :  '
173800140729      *
173900140729     o          e            PRTtxt      2
174000140729     o                                              'Potenziale '
174100140729     o                                        +   2 'Messaggio'
174200140729     o          e            PRTtxt      1
174300140729     o                                              '-----------'
174400140729     o                                        +   2 '---------'
174500140728      *
174600140728     o          e            PRTerr1     2
174700140728     o                                              'NON RIUSCITA LA +
174800140728     o                                               PULIZIA DEL WORK-+
174900140728     o                                               FILE WFACP00F'
175000140728      *
175100140728     o          e            PRTerr2     2
175200140729     o                                              'NON TROVATO IL +
175300140729     o                                               FILE'
175400140903     o                       wLibFile         +   1
175500140729      *
175600140729     o          e            PRTerr3     2
175700140729     o                       wPotenz
175800140729     o                                        +   2 'Troppi caratteri ('
175900140729     o                       wLungh        z  +   0
176000140729     o                                        +   0 ') nel'
176100140729     o                       wCampo        z  +   1
176200140729     o                                        +   0 '° campo:'
176300140729     o                       $Campi(wCampo)   +   1
176400140729     o          e            PRTerr3     1
176500140729     o                       wValoreA1        +   5
176600140729     o          e            PRTerr3     1
176700140729     o                       wValoreA2        +   5
176800140728      *
176900140729     o          e            PRTerr4     2
177000140729     o                       wPotenz
177100140729     o                                        +   2 'Dati NON numerici +
177200140728     o                                               nel'
177300140728     o                       wCampo        z  +   1
177400140729     o                                        +   0 '° campo:'
177500140728     o                       $Campi(wCampo)   +   1
177600140729     o          e            PRTerr4     1
177700140728     o                       wValoreA1        +   5
177800140729     o          e            PRTerr4     1
177900140728     o                       wValoreA2        +   5
178000140728      *
178100140729     o          e            PRTerr5     2
178200140729     o                       wPotenz
178300140729     o                                        +   2 'Numero decimali ('
178400140728     o                       wNumDec       z  +   0
178500140729     o                                        +   0 ') errato nel'
178600140729     o                       wCampo        z  +   1
178700140729     o                                        +   0 '° campo:'
178800140728     o                       $Campi(wCampo)   +   1
178900140729     o          e            PRTerr5     1
179000140728     o                       wValoreA1        +   5
179100140729     o          e            PRTerr5     1
179200140729     o                       wValoreA2        +   5
179300140729      *
179400140729     o          e            PRTerr6     2
179500140729     o                       wPotenz
179600140729     o                                        +   2 'Troppe cifre ('
179700140729     o                       wLungh        z  +   0
179800140729     o                                        +   0 ') nel'
179900140729     o                       wCampo        z  +   1
180000140729     o                                        +   0 '° campo:'
180100140729     o                       $Campi(wCampo)   +   1
180200140729     o          e            PRTerr6     1
180300140729     o                       wValoreA1        +   5
180400140729     o          e            PRTerr6     1
180500140729     o                       wValoreA2        +   5
180600140729      *
180700140729     o          e            PRTend      2
180800140729     o                                        +  24 '***  Fine Lista  ***'
180900140729     o          e            PRTend      0
181000140729     o                                        +  24 '***  Fine Lista  ***'
181100140728
181200140728       //--------------------------------------------------------------
181300140728       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
181400140728       //--------------------------------------------------------------
181500140728
181600140728** --?$Campi -----------*?Nomi di campi da indicare in stampa?
181700140728ID CRIBIS                   1
181800140728ID BRT                      2
181900140728Ragione sociale BRT         3
182000140728Partita Iva BRT             4
182100140728Codice Fiscale BRT          5
182200140728Indirizzo BRT               6
182300140728Località BRT                7
182400140728Cap BRT                     8
182500140728Provincia BRT               9
182600140728Provincia estesa BRT       10
182700140728Nazione BRT                11
182800140728Telefono BRT               12
182900140801Tipo indirizzo BRT         13
183000140801DUNS BRT                   14
183100140801Crif Number                15
183200140801OOB Indicator              16
183300140801Compravendita              17
183400140801Confidence code            18
183500140801Name similarity %          19
183600140801City similarity %          20
183700140801Address similarity %       21
183800140801Province similarity %      22
183900140801Name Match Grade %         23
184000140801City Match Grade %         24
184100140801Address Match Grade %      25
184200140801Province Match Grade %     26
184300140801Denominazione              27
184400140801Indirizzo                  28
184500140801Comune                     29
184600140801Cap                        30
184700140801Regione                    31
184800140801Frazione                   32
184900140801Provincia                  33
185000140801Provincia estesa           34
185100140801Codice Fiscale             35
185200140801Partita Iva                36
185300140801Telefono 1                 37
185400140801Fax 1                      38
185500140801Sito Internet 1            39
185600140801Flag E-commerce            40
185700140801Capitale sociale Euro      41
185800140801Anno Fondazione            42
185900140801Anno Costituzione          43
186000140801Data inizio attività       44
186100140801Ateco (2007)  1            45
186200140801Descrizione codice ateco   46
186300140801Sic primario               47
186400140801Descrizione Sic            48
186500140801Dipendenti                 49
186600140801Natura giuridica           50
186700140801CCIAA                      51
186800140801REA                        52
186900140801Oggetto sociale            53
187000140801Stato Impresa Cribis       54
187100140801Denominazione Esp 1        55
187200140801Cognome Esp 1              56
187300140801Nome Esp 1                 57
187400140801Sesso Esp 1                58
187500140801Carica Esp 1               59
187600140801Denominazione Esp 2        60
187700140801Cognome Esp 2              61
187800140801Nome Esp 2                 62
187900140801Sesso Esp 2                63
188000140801Carica Esp 2               64
188100140801Denominazione Esp 3        65
188200140801Cognome Esp 3              66
188300140801Nome Esp 3                 67
188400140801Sesso Esp 3                68
188500140801Carica Esp 3               69
188600140801Denominazione Esp 4        70
188700140801Cognome Esp 4              71
188800140801Nome Esp 4                 72
188900140801Sesso Esp 4                73
189000140801Carica Esp 4               74
189100140801Denominazione Esp 5        75
189200140801Cognome Esp 5              76
189300140801Nome Esp 5                 77
189400140801Sesso Esp 5                78
189500140801Carica Esp 5               79
189600140801Denominazione Esp 6        80
189700140801Cognome Esp 6              81
189800140801Nome Esp 6                 82
189900140801Sesso Esp 6                83
190000140801Carica Esp 6               84
190100140801Denominazione Esp 7        85
190200140801Cognome Esp 7              86
190300140801Nome Esp 7                 87
190400140801Sesso Esp 7                88
190500140801Carica Esp 7               89
190600140801Denominazione Esp 8        90
190700140801Cognome Esp 8              91
190800140801Nome Esp 8                 92
190900140801Sesso Esp 8                93
191000140801Carica Esp 8               94
191100140801Fatturato Euro             95
191200140801Anno Fatturato             96
191300140801Provenienza Fatturato      97
191400140801Utile/Perdita              98
191500140801% import                   99
191600140801% export                  100
191700140801Crif Number casa madre    101
191800140728Nome casa madre           102
191900140728Indirizzo casa madre      103
192000140728Cap casa madre            104
192100140728Città casa madre          105
192200140728Provincia casa madre      106
192300140728Nazione casa madre        107
192400140728FlagRating4               108
192500140728% impex                   109
192600140728anno %impex               110
