000100140730       //==============================================================
000200140730       //?Acquisizione Potenziali: riempimento file WFMDP00F con dati  ?
000300140730       //?                         ricevuti da CRIF (file PROSP_DATI)  ?
000400140730       //==============================================================
000500140730
000600140730       //--------------------------------------------------------------
000700140730       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800140730       //--------------------------------------------------------------
000900140730
001000140730     /*PRM  dbgview(*source)
001100140730     /*CMD  ovrdbf file(PROSP_DATI) tofile(EDPFG/PROSP_DATI) +
001200140730     /*CMD         ovrscope(*calllvl)
001300140730     /*END  dltovr file(PROSP_DATI) lvl(*)
001400140730     /*END
001500140730
001600140730       //--------------------------------------------------------------
001700140730       //?Specifiche di controllo.                                     ?
001800140730       //--------------------------------------------------------------
001900140730
002000140730     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100140730     h dftactgrp(*no)
002200140730
002300140730       //--------------------------------------------------------------
002400140730       //?Dichiarazione file.                                          ?
002500140730       //--------------------------------------------------------------
002600140730
002700140730       // -?Main Dati Potenziali ricevuti da CRIF?
002800140730     fPROSP_DATIif   e           k disk
002900140730     f                                     extfile(wLibFile)
003000140730     f                                     rename(PROSP_DATI : PROSPDATI)
003100140730     f                                     prefix('X')
003200140730     f                                     usropn
003300140730
003400140730       // -?WrkF Main Dati Potenziali con dati ricevuti da CRIF?
003500140730     fWFMDP00F  o    e             disk
003600140730     f                                     usropn
003700140730
003800140730       // -?Stampa segnalazioni di errore?
003900140730     fQSYSPRT   o    f  132        printer
004000140730     f                                     usropn
004100140730     f                                     oflind(*inOF)
004200140730
004300140730       //--------------------------------------------------------------
004400140730       //?Definizione costanti.                                        ?
004500140730       //--------------------------------------------------------------
004600140731
004700140731       // -?Carattere separatore tra i campi nel file ricevuto?
004800140731       //  ?(standard: ";", da noi richiesto; "]")?
004900140801     d*// c_SepChar       c                   const(';')
005000140801     d c_SepChar       c                   const(']')
005100140731
005200140731      *// // -?Numero MAX di campi nel file?
005300140731      *// //  ?(per il controllo dell'eventuale esistenza del carattere?
005400140731      *// //  ?'; '/'] ' - seguito da uno spazio)?
005500140731     d*// c_MaxFldWF      c                   const(110)
005600140730
005700140730       // -?Costante per controllo "caratteri solo numerici"?
005800140730     d c_Digits        c                   const('0123456789')
005900140730
006000140730       // -?Costanti per conversione caratteri maiuscoli in minuscoli e viceversa?
006100140730     d c_Maiu          c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
006200140730     d c_Minu          c                   const('abcdefghijklmnopqrstuvwxyz')
006300140730
006400140730       //--------------------------------------------------------------
006500140730       //?Definizione schiere.                                         ?
006600140730       //--------------------------------------------------------------
006700140730
006800140730       // -?Descrizione campi per stampa errori?
006900140730     d $Campi          s             25    dim( 86)  ctdata  perrcd( 1)
007000140730
007100140730       //--------------------------------------------------------------
007200140730       //?Definizione aree dati.                                       ?
007300140730       //--------------------------------------------------------------
007400140730
007500140730       // -?Dati utente?
007600140730     d §AzUte        e ds                  extname(AZUTE00F)
007700140730     d                                     dtaara
007800140730     d §DatiUte      e ds                  extname(dDatiUte)
007900140730     d                                     dtaara
008000140730
008100140730       //--------------------------------------------------------------
008200140730       //?Definizione strutture dati.                                  ?
008300140730       //--------------------------------------------------------------
008400140730
008500140730       // -?Status ds?
008600140730     d Status         sds
008700140730     d   SDSpgmName      *proc
008800140730     d*//SDSparms        *parms
008900140730     d   SDSjobName          244    253                                         Job name
009000140730     d   SDSjobUser          254    263                                         User name
009100140730     d*//SDSjobNumber        264    269s 0                                      Job number
009200140730
009300140730       // -?Parametri ricevuti (forse)?
009400140730     d KPJBA         e ds                  inz
009500140730
009600140730       //--------------------------------------------------------------
009700140730       //?Definizione variabili globali.                               ?
009800140730       //--------------------------------------------------------------
009900140730
010000140730       // -?Parametri ricevuti (forse)?
010100140730     d wKPJBA          s                   like(KPJBA)
010200140730
010300140730       // -?Nome Libreria in cui si trova il file di CRIBIS (PROSP_DATI)?
010400140730     d wLibWF          s             10    inz('EDPFG     ')
010500140730
010600140730       // -?Nome esteso Libreria/File del file da elaborare?
010700140730     d wLibFile        s             21a   inz
010800140730
010900140730       // -?Flags Booleani?
011000140730     d $Negativo       s               n   inz
011100140730     d*// $Perc           s               n   inz
011200140730
011300140730       // -?Campi di comodo?
011400140730     d wDate_Iso       s               d   inz  datfmt(*iso)
011500140730     d wDate_Eur       s               d   inz  datfmt(*eur)
011600140730     d wDate           s              8  0 inz
011700140730     d wTime           s              6  0 inz
011800140730     d wErr            s              3  0 inz
011900140730     d wCrifNr         s                   inz  like(WMPcrif)
012000140801     d wValoreA1       s            127    inz
012100140801     d wValoreA2       s            127    inz
012200140730     d wCampo          s              3  0 inz
012300140730     d wPosI           s              4  0 inz
012400140730     d wPosF           s              4  0 inz
012500140730     d wTipo           s              1    inz
012600140801     d wValoreA        s            255    inz
012700140730     d wValoreN        s             17  2 inz
012800140730     d wLungh          s              3  0 inz
012900140730     d wPosVirg        s              3  0 inz
013000140730     d wNumDec         s              1  0 inz
013100140730     d wPosSep         s              3  0 inz
013200140730
013300140730       //--------------------------------------------------------------
013400140730       //?Definizione prototipi procedure.                             ?
013500140730       //--------------------------------------------------------------
013600140730
013700140730       // -?Reperimento dati utente?
013800140730     d TIBS34ds      e ds                  inz
013900140730      /copy gaitrasrc/srcProtoPR,TIBS34R
014000140730
014100140730       // -?Parametri API QCAPCMD (Process Commands)?
014200140730     d Qcmd            s           2048    inz  varying
014300140730      /copy qSysInc/qRpgleSrc,QCAPCMD
014400140730       // -?API QCAPCMD (Process Commands)?
014500140730      /copy gaitrasrc/srcProtoPR,QCAPCMD
014600140730
014700140730       // -?Parametri gestione errori API.?
014800140730      /copy qsysinc/qrpglesrc,QUSEC
014900140730
015000140730       //--------------------------------------------------------------
015100140730       //?Definizione key-list.                                        ?
015200140730       //--------------------------------------------------------------
015300140730
015400140730
015500140730       //--------------------------------------------------------------
015600140730       //?Riepilogo indicatori utilizzati.                             ?
015700140730       //--------------------------------------------------------------
015800140730
015900140730
016000140730       //--------------------------------------------------------------
016100140730       //?M A I N - L I N E                                            ?
016200140730       //--------------------------------------------------------------
016300140730
016400140730     c     *Entry        plist
016500140730     c                   parm                    wKPJBA
016600140730
016700140730      /free
016800140730
016900140730       // -?Operazioni iniziali?
017000140730       exsr sr_RoutInz;
017100140730
017200140730       // -?Ciclo di lettura file PROSP_DATI?
017300140730       read  PROSPDATI;
017400140801       if  %subst( xProsp_Dati : 1 : 26 ) = 'Crif Number' + c_SepChar +
017500140801                                            'OOB Indicator' + c_SepChar;
017600140730         read  PROSPDATI;
017700140730       endif;
017800140730
017900140730       DoW  Not %eof(PROSP_DATI);
018000140730
018100140730         exsr  sr_ElabPot;
018200140730
018300140730         read  PROSPDATI;
018400140730
018500140730       EndDo;
018600140730
018700140730       // -?Operazioni finali?
018800140730       exsr sr_RoutEnd;
018900140730
019000140730       //--------------------------------------------------------------
019100140730       //?Operazioni iniziali.                                         ?
019200140730       //--------------------------------------------------------------
019300140730       BEGSR  sr_RoutInz;
019400140730
019500140730         // -?Impostazione chiusura?
019600140730         *inLR = *on;
019700140730
019800140730         // -?Ricezione parametri?
019900140730         if  %parms() > *zero;
020000140730           KPJBA = wKPJBA;
020100140730         else;
020200140730           knsif = *all'? ';
020300140730           knmus = SDSjobUser;
020400140730         endif;
020500140730
020600140730         // -?Reperimento data odierna in formato aaaammgg?
020700140730         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
020800140730         wDate_Iso = %date();
020900140730         wDate_Eur = wDate_Iso;
021000140730         wDate = %dec( wDate_Eur );
021100140730
021200140730         // -?Reperimento orario?
021300140730         wTime = %dec( %time() );
021400140730
021500140730         // -?Reperimento dati job?
021600140730         exsr  sr_DatiJob;
021700140730
021800140730         // -?Pulizia del file di Output?
021900140730         Qcmd = 'CLRPFM file(*libl/WFMDP00F)';
022000140730         exsr  sr_ExecCmd;
022100140730         if Qusei <> *blank;
022200140730           wErr = 1;
022300140730           exsr  sr_PrintErr;
022400140730           exsr  sr_RoutEnd;
022500140730         endif;
022600140730
022700140730         open  WFMDP00F;
022800140730
022900140730         // -?Impostazione libreria del file di CRIBIS (PROSP_DATI)?
023000140730         if  %parms() > 1  and  kpjbu <> *blank;
023100140730           wLibWF = %subst( kpjbu : 1 : 10);
023200140730         endif;
023300140730
023400140730         // -?Apertura del file di CRIBIS (PROSP_DATI)?
023500140730         wLibFile = %trim( wLibWF ) + '/PROSP_DATI';
023600140730         open(e)  PROSP_DATI;
023700140730         if %error();
023800140730           wErr = 2;
023900140730           exsr  sr_PrintErr;
024000140730           exsr  sr_RoutEnd;
024100140730         endif;
024200140730
024300140730       ENDSR;
024400140730
024500140730       //--------------------------------------------------------------
024600140730       //?Reperimento Dati del job (Utente/Operativi).                 ?
024700140730       //--------------------------------------------------------------
024800140730       BEGSR  sr_DatiJob;
024900140730
025000140730         in(e) §AzUte;
025100140730         if NOT %error;
025200140730           in(e) §DatiUte;
025300140730         endif;
025400140730         if %error or RSut = *blank;
025500140730           tibs34r ( tibs34ds );
025600140730           in §AzUte;
025700140730           in §DatiUte;
025800140730         endif;
025900140730
026000140730       ENDSR;
026100140730
026200140730       //--------------------------------------------------------------
026300140730       //?Elaborazione singolo cliente Potenziale.                     ?
026400140730       //--------------------------------------------------------------
026500140730       BEGSR  sr_ElabPot;
026600140730
026700140730         clear  WFMDP000;
026800140730
026900140730         clear  wPosI;
027000140730         clear  wPosF;
027100140730         clear  wCampo;
027200140730         clear  wCrifNr;
027300140730
027400140730         // -?  1° campo: Crif Number             ?
027500140730         wCampo += 1;
027600140801         wTipo   = 'A';        //?- alfanumerico?
027700140730         wLungh  = %len( WMPcrif );
027800140730         exsr  sr_GetFldVal;
027900140730         if  wErr > *zero;
028000140730           leavesr;
028100140730         endif;
028200140730         WMPcrif    = wValoreA;
028300140730         wCrifNr    = wValoreA;
028400140730         // -?  2° campo: OOB Indicator           ?
028500140730         wCampo += 1;
028600140730         wTipo   = 'A';        //?- alfanumerico?
028700140730         wLungh  = %len( WMPoob );
028800140730         exsr  sr_GetFldVal;
028900140730         if  wErr > *zero;
029000140730           leavesr;
029100140730         endif;
029200140730         WMPoob     = wValoreA;
029300140730         // -?  3° campo: Denominazione           ?
029400140730         wCampo += 1;
029500140730         wTipo   = 'A';        //?- alfanumerico?
029600140730         wLungh  = %len( WMPdeno );
029700140730         exsr  sr_GetFldVal;
029800140730         if  wErr > *zero;
029900140730           leavesr;
030000140730         endif;
030100140730         WMPdeno    = wValoreA;
030200140730         // -?  4° campo: Indirizzo               ?
030300140730         wCampo += 1;
030400140730         wTipo   = 'A';        //?- alfanumerico?
030500140730         wLungh  = %len( WMPind );
030600140730         exsr  sr_GetFldVal;
030700140730         if  wErr > *zero;
030800140730           leavesr;
030900140730         endif;
031000140730         WMPind     = wValoreA;
031100140730         // -?  5° campo: Comune                  ?
031200140730         wCampo += 1;
031300140730         wTipo   = 'A';        //?- alfanumerico?
031400140730         wLungh  = %len( WMPloc );
031500140730         exsr  sr_GetFldVal;
031600140730         if  wErr > *zero;
031700140730           leavesr;
031800140730         endif;
031900140730         WMPloc     = wValoreA;
032000140730         // -?  6° campo: Cap                     ?
032100140730         wCampo += 1;
032200140730         wTipo   = 'N';        //?- numerico?
032300140730         wLungh  = %len( WMPcap );
032400140730         exsr  sr_GetFldVal;
032500140730         if  wErr > *zero;
032600140730           leavesr;
032700140730         endif;
032800140730         WMPcap     = wValoreA;
032900140730         // -?  7° campo: Regione                 ?
033000140730         wCampo += 1;
033100140730         wTipo   = 'A';        //?- alfanumerico?
033200140730         wLungh  = %len( WMPreg );
033300140730         exsr  sr_GetFldVal;
033400140730         if  wErr > *zero;
033500140730           leavesr;
033600140730         endif;
033700140730         WMPreg     = wValoreA;
033800140730         // -?  8° campo: Frazione                ?
033900140730         wCampo += 1;
034000140730         wTipo   = 'A';        //?- alfanumerico?
034100140730         wLungh  = %len( WMPfra );
034200140730         exsr  sr_GetFldVal;
034300140730         if  wErr > *zero;
034400140730           leavesr;
034500140730         endif;
034600140730         WMPfra     = wValoreA;
034700140730         // -?  9° campo: Provincia               ?
034800140730         wCampo += 1;
034900140730         wTipo   = 'A';        //?- alfanumerico?
035000140730         wLungh  = %len( WMPprv );
035100140730         exsr  sr_GetFldVal;
035200140730         if  wErr > *zero;
035300140730           leavesr;
035400140730         endif;
035500140730         WMPprv     = wValoreA;
035600140730         // -? 10° campo: Provincia estesa        ?
035700140730         wCampo += 1;
035800140730         wTipo   = 'a';        //?- alfanumerico anche minuscolo?
035900140730         wLungh  = %len( WMPprvDes );
036000140730         exsr  sr_GetFldVal;
036100140730         if  wErr > *zero;
036200140730           leavesr;
036300140730         endif;
036400140730         WMPprvDes  = wValoreA;
036500140730         // -? 11° campo: Codice Fiscale          ?
036600140730         wCampo += 1;
036700140730         wTipo   = 'A';        //?- alfanumerico?
036800140730         wLungh  = %len( WMPcfi );
036900140730         exsr  sr_GetFldVal;
037000140730         if  wErr > *zero;
037100140730           leavesr;
037200140730         endif;
037300140730         WMPcfi     = wValoreA;
037400140730         // -? 12° campo: Partita Iva             ?
037500140730         wCampo += 1;
037600140730         wTipo   = 'A';        //?- alfanumerico?
037700140730         wLungh  = %len( WMPcpi );
037800140730         exsr  sr_GetFldVal;
037900140730         if  wErr > *zero;
038000140730           leavesr;
038100140730         endif;
038200140730         WMPcpi     = wValoreA;
038300140730         // -? 13° campo: Telefono 1              ?
038400140730         wCampo += 1;
038500140730         wTipo   = 'A';        //?- alfanumerico?
038600140730         wLungh  = %len( WMPtel );
038700140730         exsr  sr_GetFldVal;
038800140730         if  wErr > *zero;
038900140730           leavesr;
039000140730         endif;
039100140730         WMPtel     = wValoreA;
039200140730         // -? 14° campo: Fax 1                   ?
039300140730         wCampo += 1;
039400140730         wTipo   = 'A';        //?- alfanumerico?
039500140730         wLungh  = %len( WMPfax );
039600140730         exsr  sr_GetFldVal;
039700140730         if  wErr > *zero;
039800140730           leavesr;
039900140730         endif;
040000140730         WMPfax     = wValoreA;
040100140730         // -? 15° campo: Sito Internet 1         ?
040200140730         wCampo += 1;
040300140730         wTipo   = 'A';        //?- alfanumerico?
040400140730         wLungh  = %len( WMPsito );
040500140730         exsr  sr_GetFldVal;
040600140730         if  wErr > *zero;
040700140730           leavesr;
040800140730         endif;
040900140730         WMPsito    = wValoreA;
041000140730         // -? 16° campo: Flag E-commerce         ?
041100140730         wCampo += 1;
041200140730         wTipo   = 'A';        //?- alfanumerico?
041300140730         wLungh  = %len( WMPecom );
041400140730         exsr  sr_GetFldVal;
041500140730         if  wErr > *zero;
041600140730           leavesr;
041700140730         endif;
041800140730         WMPecom    = wValoreA;
041900140730         // -? 17° campo: Capitale sociale Euro   ?
042000140730         wCampo += 1;
042100140730         wTipo   = 'N';        //?- numerico?
042200140730         wLungh  = %len( WMPcapSoc );
042300140730         exsr  sr_GetFldVal;
042400140730         if  wErr > *zero;
042500140730           leavesr;
042600140730         endif;
042700140730         WMPcapSoc  = wValoreN;
042800140730         // -? 18° campo: Anno Costituzione       ?
042900140730         wCampo += 1;
043000140730         wTipo   = 'N';        //?- numerico?
043100140730         wLungh  = %len( WMPaaCos );
043200140730         exsr  sr_GetFldVal;
043300140730         if  wErr > *zero;
043400140730           leavesr;
043500140730         endif;
043600140730         WMPaaCos   = wValoreN;
043700140730         // -? 19° campo: Anno Fondazione         ?
043800140730         wCampo += 1;
043900140730         wTipo   = 'N';        //?- numerico?
044000140730         wLungh  = %len( WMPaaFon );
044100140730         exsr  sr_GetFldVal;
044200140730         if  wErr > *zero;
044300140730           leavesr;
044400140730         endif;
044500140730         WMPaaFon   = wValoreN;
044600140730         // -? 20° campo: Data inizio attività    ?
044700140730         wCampo += 1;
044800140730         wTipo   = 'D';        //?- data (gg/mm/aaaa)?
044900140730         wLungh  = %len( WMPdtIni );
045000140730         exsr  sr_GetFldVal;
045100140730         if  wErr > *zero;
045200140730           leavesr;
045300140730         endif;
045400140730         WMPdtIni   = wValoreN;
045500140730         // -? 21° campo: Ateco (2007)  1         ?
045600140730         wCampo += 1;
045700140730         wTipo   = 'A';        //?- alfanumerico?
045800140730         wLungh  = %len( WMPateco );
045900140730         exsr  sr_GetFldVal;
046000140730         if  wErr > *zero;
046100140730           leavesr;
046200140730         endif;
046300140730         WMPateco   = wValoreA;
046400140730         // -? 22° campo: Descrizione codice ateco?
046500140730         wCampo += 1;
046600140730         wTipo   = 'A';        //?- alfanumerico?
046700140730         wLungh  = %len( WMPatecoDe );
046800140730         exsr  sr_GetFldVal;
046900140730         if  wErr > *zero;
047000140730           leavesr;
047100140730         endif;
047200140730         WMPatecoDe = wValoreA;
047300140730         // -? 23° campo: Sic primario            ?
047400140730         wCampo += 1;
047500140730         wTipo   = 'A';        //?- alfanumerico?
047600140730         wLungh  = %len( WMPsicP );
047700140730         exsr  sr_GetFldVal;
047800140730         if  wErr > *zero;
047900140730           leavesr;
048000140730         endif;
048100140730         WMPsicP    = wValoreA;
048200140730         // -? 24° campo: Descrizione Sic         ?
048300140730         wCampo += 1;
048400140730         wTipo   = 'A';        //?- alfanumerico?
048500140730         wLungh  = %len( WMPsicPdes );
048600140730         exsr  sr_GetFldVal;
048700140730         if  wErr > *zero;
048800140730           leavesr;
048900140730         endif;
049000140730         WMPsicPdes = wValoreA;
049100140730         // -? 25° campo: Dipendenti              ?
049200140730         wCampo += 1;
049300140730         wTipo   = 'N';        //?- numerico?
049400140730         wLungh  = %len( WMPdip );
049500140730         exsr  sr_GetFldVal;
049600140730         if  wErr > *zero;
049700140730           leavesr;
049800140730         endif;
049900140730         WMPdip     = wValoreN;
050000140730         // -? 26° campo: Natura giuridica        ?
050100140730         wCampo += 1;
050200140730         wTipo   = 'A';        //?- alfanumerico?
050300140730         wLungh  = %len( WMPnatG );
050400140730         exsr  sr_GetFldVal;
050500140730         if  wErr > *zero;
050600140730           leavesr;
050700140730         endif;
050800140730         WMPnatG    = wValoreA;
050900140730         // -? 27° campo: CCIAA                   ?
051000140730         wCampo += 1;
051100140730         wTipo   = 'A';        //?- alfanumerico?
051200140730         wLungh  = %len( WMPcciaa );
051300140730         exsr  sr_GetFldVal;
051400140730         if  wErr > *zero;
051500140730           leavesr;
051600140730         endif;
051700140730         WMPcciaa   = wValoreA;
051800140730         // -? 28° campo: REA                     ?
051900140730         wCampo += 1;
052000140730         wTipo   = 'A';        //?- alfanumerico?
052100140730         wLungh  = %len( WMPrea );
052200140730         exsr  sr_GetFldVal;
052300140730         if  wErr > *zero;
052400140730           leavesr;
052500140730         endif;
052600140730         WMPrea     = wValoreA;
052700140730         // -? 29° campo: Oggetto sociale         ?
052800140730         wCampo += 1;
052900140730         wTipo   = 'A';        //?- alfanumerico?
053000140730         wLungh  = %len( WMPoggSoc );
053100140730         exsr  sr_GetFldVal;
053200140730         if  wErr > *zero;
053300140730           leavesr;
053400140730         endif;
053500140730         WMPoggSoc  = wValoreA;
053600140730         // -? 30° campo: Stato Impresa Cribis    ?
053700140730         wCampo += 1;
053800140730         wTipo   = 'A';        //?- alfanumerico?
053900140730         wLungh  = %len( WMPstImp );
054000140730         exsr  sr_GetFldVal;
054100140730         if  wErr > *zero;
054200140730           leavesr;
054300140730         endif;
054400140730         WMPstImp   = wValoreA;
054500140730         // -? 31° campo: Denominazione Esp 1     ?
054600140730         wCampo += 1;
054700140730         wTipo   = 'A';        //?- alfanumerico?
054800140730         wLungh  = %len( WMPdeno1 );
054900140730         exsr  sr_GetFldVal;
055000140730         if  wErr > *zero;
055100140730           leavesr;
055200140730         endif;
055300140730         WMPdeno1   = wValoreA;
055400140730         // -? 32° campo: Cognome Esp 1           ?
055500140730         wCampo += 1;
055600140730         wTipo   = 'A';        //?- alfanumerico?
055700140730         wLungh  = %len( WMPcogn1 );
055800140730         exsr  sr_GetFldVal;
055900140730         if  wErr > *zero;
056000140730           leavesr;
056100140730         endif;
056200140730         WMPcogn1   = wValoreA;
056300140730         // -? 33° campo: Nome Esp 1              ?
056400140730         wCampo += 1;
056500140730         wTipo   = 'A';        //?- alfanumerico?
056600140730         wLungh  = %len( WMPnome1 );
056700140730         exsr  sr_GetFldVal;
056800140730         if  wErr > *zero;
056900140730           leavesr;
057000140730         endif;
057100140730         WMPnome1   = wValoreA;
057200140730         // -? 34° campo: Sesso Esp 1             ?
057300140730         wCampo += 1;
057400140730         wTipo   = 'A';        //?- alfanumerico?
057500140730         wLungh  = %len( WMPsess1 );
057600140730         exsr  sr_GetFldVal;
057700140730         if  wErr > *zero;
057800140730           leavesr;
057900140730         endif;
058000140730         WMPsess1   = wValoreA;
058100140730         // -? 35° campo: Carica Esp 1            ?
058200140730         wCampo += 1;
058300140730         wTipo   = 'A';        //?- alfanumerico?
058400140730         wLungh  = %len( WMPcari1 );
058500140730         exsr  sr_GetFldVal;
058600140730         if  wErr > *zero;
058700140730           leavesr;
058800140730         endif;
058900140730         WMPcari1   = wValoreA;
059000140730         // -? 36° campo: Denominazione Esp 2     ?
059100140730         wCampo += 1;
059200140730         wTipo   = 'A';        //?- alfanumerico?
059300140730         wLungh  = %len( WMPdeno2 );
059400140730         exsr  sr_GetFldVal;
059500140730         if  wErr > *zero;
059600140730           leavesr;
059700140730         endif;
059800140730         WMPdeno2   = wValoreA;
059900140730         // -? 37° campo: Cognome Esp 2           ?
060000140730         wCampo += 1;
060100140730         wTipo   = 'A';        //?- alfanumerico?
060200140730         wLungh  = %len( WMPcogn2 );
060300140730         exsr  sr_GetFldVal;
060400140730         if  wErr > *zero;
060500140730           leavesr;
060600140730         endif;
060700140730         WMPcogn2   = wValoreA;
060800140730         // -? 38° campo: Nome Esp 2              ?
060900140730         wCampo += 1;
061000140730         wTipo   = 'A';        //?- alfanumerico?
061100140730         wLungh  = %len( WMPnome2 );
061200140730         exsr  sr_GetFldVal;
061300140730         if  wErr > *zero;
061400140730           leavesr;
061500140730         endif;
061600140730         WMPnome2   = wValoreA;
061700140730         // -? 39° campo: Sesso Esp 2             ?
061800140730         wCampo += 1;
061900140730         wTipo   = 'A';        //?- alfanumerico?
062000140730         wLungh  = %len( WMPsess2 );
062100140730         exsr  sr_GetFldVal;
062200140730         if  wErr > *zero;
062300140730           leavesr;
062400140730         endif;
062500140730         WMPsess2   = wValoreA;
062600140730         // -? 40° campo: Carica Esp 2            ?
062700140730         wCampo += 1;
062800140730         wTipo   = 'A';        //?- alfanumerico?
062900140730         wLungh  = %len( WMPcari2 );
063000140730         exsr  sr_GetFldVal;
063100140730         if  wErr > *zero;
063200140730           leavesr;
063300140730         endif;
063400140730         WMPcari2   = wValoreA;
063500140730         // -? 41° campo: Denominazione Esp 3     ?
063600140730         wCampo += 1;
063700140730         wTipo   = 'A';        //?- alfanumerico?
063800140730         wLungh  = %len( WMPdeno3 );
063900140730         exsr  sr_GetFldVal;
064000140730         if  wErr > *zero;
064100140730           leavesr;
064200140730         endif;
064300140730         WMPdeno3   = wValoreA;
064400140730         // -? 42° campo: Cognome Esp 3           ?
064500140730         wCampo += 1;
064600140730         wTipo   = 'A';        //?- alfanumerico?
064700140730         wLungh  = %len( WMPcogn3 );
064800140730         exsr  sr_GetFldVal;
064900140730         if  wErr > *zero;
065000140730           leavesr;
065100140730         endif;
065200140730         WMPcogn3   = wValoreA;
065300140730         // -? 43° campo: Nome Esp 3              ?
065400140730         wCampo += 1;
065500140730         wTipo   = 'A';        //?- alfanumerico?
065600140730         wLungh  = %len( WMPnome3 );
065700140730         exsr  sr_GetFldVal;
065800140730         if  wErr > *zero;
065900140730           leavesr;
066000140730         endif;
066100140730         WMPnome3   = wValoreA;
066200140730         // -? 44° campo: Sesso Esp 3             ?
066300140730         wCampo += 1;
066400140730         wTipo   = 'A';        //?- alfanumerico?
066500140730         wLungh  = %len( WMPsess3 );
066600140730         exsr  sr_GetFldVal;
066700140730         if  wErr > *zero;
066800140730           leavesr;
066900140730         endif;
067000140730         WMPsess3   = wValoreA;
067100140730         // -? 40° campo: Carica Esp 3            ?
067200140730         wCampo += 1;
067300140730         wTipo   = 'A';        //?- alfanumerico?
067400140730         wLungh  = %len( WMPcari3 );
067500140730         exsr  sr_GetFldVal;
067600140730         if  wErr > *zero;
067700140730           leavesr;
067800140730         endif;
067900140730         WMPcari3   = wValoreA;
068000140730         // -? 46° campo: Denominazione Esp 4     ?
068100140730         wCampo += 1;
068200140730         wTipo   = 'A';        //?- alfanumerico?
068300140730         wLungh  = %len( WMPdeno4 );
068400140730         exsr  sr_GetFldVal;
068500140730         if  wErr > *zero;
068600140730           leavesr;
068700140730         endif;
068800140730         WMPdeno4   = wValoreA;
068900140730         // -? 47° campo: Cognome Esp 4           ?
069000140730         wCampo += 1;
069100140730         wTipo   = 'A';        //?- alfanumerico?
069200140730         wLungh  = %len( WMPcogn4 );
069300140730         exsr  sr_GetFldVal;
069400140730         if  wErr > *zero;
069500140730           leavesr;
069600140730         endif;
069700140730         WMPcogn4   = wValoreA;
069800140730         // -? 48° campo: Nome Esp 4              ?
069900140730         wCampo += 1;
070000140730         wTipo   = 'A';        //?- alfanumerico?
070100140730         wLungh  = %len( WMPnome4 );
070200140730         exsr  sr_GetFldVal;
070300140730         if  wErr > *zero;
070400140730           leavesr;
070500140730         endif;
070600140730         WMPnome4   = wValoreA;
070700140730         // -? 49° campo: Sesso Esp 4             ?
070800140730         wCampo += 1;
070900140730         wTipo   = 'A';        //?- alfanumerico?
071000140730         wLungh  = %len( WMPsess4 );
071100140730         exsr  sr_GetFldVal;
071200140730         if  wErr > *zero;
071300140730           leavesr;
071400140730         endif;
071500140730         WMPsess4   = wValoreA;
071600140730         // -? 50° campo: Carica Esp 4            ?
071700140730         wCampo += 1;
071800140730         wTipo   = 'A';        //?- alfanumerico?
071900140730         wLungh  = %len( WMPcari4 );
072000140730         exsr  sr_GetFldVal;
072100140730         if  wErr > *zero;
072200140730           leavesr;
072300140730         endif;
072400140730         WMPcari4   = wValoreA;
072500140730         // -? 51° campo: Denominazione Esp 5     ?
072600140730         wCampo += 1;
072700140730         wTipo   = 'A';        //?- alfanumerico?
072800140730         wLungh  = %len( WMPdeno5 );
072900140730         exsr  sr_GetFldVal;
073000140730         if  wErr > *zero;
073100140730           leavesr;
073200140730         endif;
073300140730         WMPdeno5   = wValoreA;
073400140730         // -? 52° campo: Cognome Esp 5           ?
073500140730         wCampo += 1;
073600140730         wTipo   = 'A';        //?- alfanumerico?
073700140730         wLungh  = %len( WMPcogn5 );
073800140730         exsr  sr_GetFldVal;
073900140730         if  wErr > *zero;
074000140730           leavesr;
074100140730         endif;
074200140730         WMPcogn5   = wValoreA;
074300140730         // -? 53° campo: Nome Esp 5              ?
074400140730         wCampo += 1;
074500140730         wTipo   = 'A';        //?- alfanumerico?
074600140730         wLungh  = %len( WMPnome5 );
074700140730         exsr  sr_GetFldVal;
074800140730         if  wErr > *zero;
074900140730           leavesr;
075000140730         endif;
075100140730         WMPnome5   = wValoreA;
075200140730         // -? 54° campo: Sesso Esp 5             ?
075300140730         wCampo += 1;
075400140730         wTipo   = 'A';        //?- alfanumerico?
075500140730         wLungh  = %len( WMPsess5 );
075600140730         exsr  sr_GetFldVal;
075700140730         if  wErr > *zero;
075800140730           leavesr;
075900140730         endif;
076000140730         WMPsess5   = wValoreA;
076100140730         // -? 55° campo: Carica Esp 5            ?
076200140730         wCampo += 1;
076300140730         wTipo   = 'A';        //?- alfanumerico?
076400140730         wLungh  = %len( WMPcari5 );
076500140730         exsr  sr_GetFldVal;
076600140730         if  wErr > *zero;
076700140730           leavesr;
076800140730         endif;
076900140730         WMPcari5   = wValoreA;
077000140730         // -? 56° campo: Denominazione Esp 6     ?
077100140730         wCampo += 1;
077200140730         wTipo   = 'A';        //?- alfanumerico?
077300140730         wLungh  = %len( WMPdeno6 );
077400140730         exsr  sr_GetFldVal;
077500140730         if  wErr > *zero;
077600140730           leavesr;
077700140730         endif;
077800140730         WMPdeno6   = wValoreA;
077900140730         // -? 57° campo: Cognome Esp 6           ?
078000140730         wCampo += 1;
078100140730         wTipo   = 'A';        //?- alfanumerico?
078200140730         wLungh  = %len( WMPcogn6 );
078300140730         exsr  sr_GetFldVal;
078400140730         if  wErr > *zero;
078500140730           leavesr;
078600140730         endif;
078700140730         WMPcogn6   = wValoreA;
078800140730         // -? 58° campo: Nome Esp 6              ?
078900140730         wCampo += 1;
079000140730         wTipo   = 'A';        //?- alfanumerico?
079100140730         wLungh  = %len( WMPnome6 );
079200140730         exsr  sr_GetFldVal;
079300140730         if  wErr > *zero;
079400140730           leavesr;
079500140730         endif;
079600140730         WMPnome6   = wValoreA;
079700140730         // -? 59° campo: Sesso Esp 6             ?
079800140730         wCampo += 1;
079900140730         wTipo   = 'A';        //?- alfanumerico?
080000140730         wLungh  = %len( WMPsess6 );
080100140730         exsr  sr_GetFldVal;
080200140730         if  wErr > *zero;
080300140730           leavesr;
080400140730         endif;
080500140730         WMPsess6   = wValoreA;
080600140730         // -? 60° campo: Carica Esp 6            ?
080700140730         wCampo += 1;
080800140730         wTipo   = 'A';        //?- alfanumerico?
080900140730         wLungh  = %len( WMPcari6 );
081000140730         exsr  sr_GetFldVal;
081100140730         if  wErr > *zero;
081200140730           leavesr;
081300140730         endif;
081400140730         WMPcari6   = wValoreA;
081500140730         // -? 61° campo: Denominazione Esp 7     ?
081600140730         wCampo += 1;
081700140730         wTipo   = 'A';        //?- alfanumerico?
081800140730         wLungh  = %len( WMPdeno7 );
081900140730         exsr  sr_GetFldVal;
082000140730         if  wErr > *zero;
082100140730           leavesr;
082200140730         endif;
082300140730         WMPdeno7   = wValoreA;
082400140730         // -? 62° campo: Cognome Esp 7           ?
082500140730         wCampo += 1;
082600140730         wTipo   = 'A';        //?- alfanumerico?
082700140730         wLungh  = %len( WMPcogn7 );
082800140730         exsr  sr_GetFldVal;
082900140730         if  wErr > *zero;
083000140730           leavesr;
083100140730         endif;
083200140730         WMPcogn7   = wValoreA;
083300140730         // -? 63° campo: Nome Esp 7              ?
083400140730         wCampo += 1;
083500140730         wTipo   = 'A';        //?- alfanumerico?
083600140730         wLungh  = %len( WMPnome7 );
083700140730         exsr  sr_GetFldVal;
083800140730         if  wErr > *zero;
083900140730           leavesr;
084000140730         endif;
084100140730         WMPnome7   = wValoreA;
084200140730         // -? 64° campo: Sesso Esp 7             ?
084300140730         wCampo += 1;
084400140730         wTipo   = 'A';        //?- alfanumerico?
084500140730         wLungh  = %len( WMPsess7 );
084600140730         exsr  sr_GetFldVal;
084700140730         if  wErr > *zero;
084800140730           leavesr;
084900140730         endif;
085000140730         WMPsess7   = wValoreA;
085100140730         // -? 65° campo: Carica Esp 7            ?
085200140730         wCampo += 1;
085300140730         wTipo   = 'A';        //?- alfanumerico?
085400140730         wLungh  = %len( WMPcari7 );
085500140730         exsr  sr_GetFldVal;
085600140730         if  wErr > *zero;
085700140730           leavesr;
085800140730         endif;
085900140730         WMPcari7   = wValoreA;
086000140730         // -? 66° campo: Denominazione Esp 8     ?
086100140730         wCampo += 1;
086200140730         wTipo   = 'A';        //?- alfanumerico?
086300140730         wLungh  = %len( WMPdeno8 );
086400140730         exsr  sr_GetFldVal;
086500140730         if  wErr > *zero;
086600140730           leavesr;
086700140730         endif;
086800140730         WMPdeno8   = wValoreA;
086900140730         // -? 67° campo: Cognome Esp 8           ?
087000140730         wCampo += 1;
087100140730         wTipo   = 'A';        //?- alfanumerico?
087200140730         wLungh  = %len( WMPcogn8 );
087300140730         exsr  sr_GetFldVal;
087400140730         if  wErr > *zero;
087500140730           leavesr;
087600140730         endif;
087700140730         WMPcogn8   = wValoreA;
087800140730         // -? 68° campo: Nome Esp 8              ?
087900140730         wCampo += 1;
088000140730         wTipo   = 'A';        //?- alfanumerico?
088100140730         wLungh  = %len( WMPnome8 );
088200140730         exsr  sr_GetFldVal;
088300140730         if  wErr > *zero;
088400140730           leavesr;
088500140730         endif;
088600140730         WMPnome8   = wValoreA;
088700140730         // -? 69° campo: Sesso Esp 8             ?
088800140730         wCampo += 1;
088900140730         wTipo   = 'A';        //?- alfanumerico?
089000140730         wLungh  = %len( WMPsess8 );
089100140730         exsr  sr_GetFldVal;
089200140730         if  wErr > *zero;
089300140730           leavesr;
089400140730         endif;
089500140730         WMPsess8   = wValoreA;
089600140730         // -? 70° campo: Carica Esp 8            ?
089700140730         wCampo += 1;
089800140730         wTipo   = 'A';        //?- alfanumerico?
089900140730         wLungh  = %len( WMPcari8 );
090000140730         exsr  sr_GetFldVal;
090100140730         if  wErr > *zero;
090200140730           leavesr;
090300140730         endif;
090400140730         WMPcari8   = wValoreA;
090500140730         // -? 71° campo: Fatturato Euro          ?
090600140730         wCampo += 1;
090700140730         wTipo   = 'n';        //?- numerico con segno?
090800140730         wLungh  = %len( WMPfatt );
090900140730         exsr  sr_GetFldVal;
091000140730         if  wErr > *zero;
091100140730           leavesr;
091200140730         endif;
091300140730         WMPfatt    = wValoreN;
091400140730         // -? 72° campo: Anno Fatturato          ?
091500140730         wCampo += 1;
091600140730         wTipo   = 'N';        //?- numerico?
091700140730         wLungh  = %len( WMPaaFt );
091800140730         exsr  sr_GetFldVal;
091900140730         if  wErr > *zero;
092000140730           leavesr;
092100140730         endif;
092200140730         WMPaaFt    = wValoreN;
092300140730         // -? 73° campo: Provenienza Fatturato   ?
092400140730         wCampo += 1;
092500140730         wTipo   = 'A';        //?- alfanumerico?
092600140730         wLungh  = %len( WMPpvnFt );
092700140730         exsr  sr_GetFldVal;
092800140730         if  wErr > *zero;
092900140730           leavesr;
093000140730         endif;
093100140730         WMPpvnFt   = wValoreA;
093200140730         // -? 74° campo: Utile/Perdita           ?
093300140730         wCampo += 1;
093400140730         wTipo   = 'n';        //?- numerico con segno?
093500140730         wLungh  = %len( WMPutile );
093600140730         exsr  sr_GetFldVal;
093700140730         if  wErr > *zero;
093800140730           leavesr;
093900140730         endif;
094000140730         WMPutile   = wValoreN;
094100140730         // -? 75° campo: % import                ?
094200140730         wCampo += 1;
094300140730         wTipo   = 'n';        //?- numerico con "%"?
094400140730         wLungh  = %len( WMPpImp );
094500140730         exsr  sr_GetFldVal;
094600140730         if  wErr > *zero;
094700140730           leavesr;
094800140730         endif;
094900140730         if  wValoreN > 999.99;
095000140730           wErr = 6;
095100140730           exsr  sr_PrintErr;
095200140730           leavesr;
095300140730         endif;
095400140730         WMPpImp    = wValoreN;
095500140730         // -? 76° campo: % export                ?
095600140730         wCampo += 1;
095700140730         wTipo   = 'n';        //?- numerico con "%"?
095800140730         wLungh  = %len( WMPpExp );
095900140730         exsr  sr_GetFldVal;
096000140730         if  wErr > *zero;
096100140730           leavesr;
096200140730         endif;
096300140730         if  wValoreN > 999.99;
096400140730           wErr = 6;
096500140730           exsr  sr_PrintErr;
096600140730           leavesr;
096700140730         endif;
096800140730         WMPpExp    = wValoreN;
096900140730         // -? 77° campo: Crif Number Casa Madre  ?
097000140730         wCampo += 1;
097100140730         wTipo   = 'A';        //?- numerico?
097200140730         wLungh  = %len( WMPcmCrif );
097300140730         exsr  sr_GetFldVal;
097400140730         if  wErr > *zero;
097500140730           leavesr;
097600140730         endif;
097700140730         WMPcmCrif  = wValoreA;
097800140730         // -? 78° campo: Nome casa madre         ?
097900140730         wCampo += 1;
098000140730         wTipo   = 'A';        //?- alfanumerico?
098100140730         wLungh  = %len( WMPcmNom );
098200140730         exsr  sr_GetFldVal;
098300140730         if  wErr > *zero;
098400140730           leavesr;
098500140730         endif;
098600140730         WMPcmNom   = wValoreA;
098700140730         // -? 79° campo: Indirizzo casa madre    ?
098800140730         wCampo += 1;
098900140730         wTipo   = 'A';        //?- alfanumerico?
099000140730         wLungh  = %len( WMPcmInd );
099100140730         exsr  sr_GetFldVal;
099200140730         if  wErr > *zero;
099300140730           leavesr;
099400140730         endif;
099500140730         WMPcmInd   = wValoreA;
099600140730         // -? 80° campo: Cap casa madre          ?
099700140730         wCampo += 1;
099800140730         wTipo   = 'A';        //?- alfanumerico?
099900140730         wLungh  = %len( WMPcmCap );
100000140730         exsr  sr_GetFldVal;
100100140730         if  wErr > *zero;
100200140730           leavesr;
100300140730         endif;
100400140730         WMPcmCap   = wValoreA;
100500140730         // -? 81° campo: Città casa madre        ?
100600140730         wCampo += 1;
100700140730         wTipo   = 'A';        //?- alfanumerico?
100800140730         wLungh  = %len( WMPcmLoc );
100900140730         exsr  sr_GetFldVal;
101000140730         if  wErr > *zero;
101100140730           leavesr;
101200140730         endif;
101300140730         WMPcmLoc   = wValoreA;
101400140730         // -? 82° campo: Provincia casa madre    ?
101500140730         wCampo += 1;
101600140730         wTipo   = 'A';        //?- alfanumerico?
101700140730         wLungh  = %len( WMPcmPrv );
101800140730         exsr  sr_GetFldVal;
101900140730         if  wErr > *zero;
102000140730           leavesr;
102100140730         endif;
102200140730         WMPcmPrv   = wValoreA;
102300140730         // -? 83° campo: Nazione casa madre      ?
102400140730         wCampo += 1;
102500140730         wTipo   = 'A';        //?- alfanumerico?
102600140730         wLungh  = %len( WMPcmNaz );
102700140730         exsr  sr_GetFldVal;
102800140730         if  wErr > *zero;
102900140730           leavesr;
103000140730         endif;
103100140730         WMPcmNaz   = wValoreA;
103200140730         // -? 84° campo: FlagRating4             ?
103300140730         wCampo += 1;
103400140730         wTipo   = 'A';        //?- alfanumerico?
103500140730         wLungh  = %len( WMPfr4 );
103600140730         exsr  sr_GetFldVal;
103700140730         if  wErr > *zero;
103800140730           leavesr;
103900140730         endif;
104000140730         WMPfr4     = wValoreA;
104100140730         // -? 85° campo: % impex                 ?
104200140730         wCampo += 1;
104300140730         wTipo   = 'n';        //?- numerico con "%"?
104400140730         wLungh  = %len( WMPpImpEx );
104500140730         exsr  sr_GetFldVal;
104600140730         if  wErr > *zero;
104700140730           leavesr;
104800140730         endif;
104900140730         if  wValoreN > 999.99;
105000140730           wErr = 6;
105100140730           exsr  sr_PrintErr;
105200140730           leavesr;
105300140730         endif;
105400140730         WMPpImpEx  = wValoreN;
105500140730         // -? 86° campo: anno %impex             ?
105600140730         wCampo += 1;
105700140730         wTipo   = 'N';        //?- numerico?
105800140730         wLungh  = %len( WMPaImpEx );
105900140730         exsr  sr_GetFldVal;
106000140730         if  wErr > *zero;
106100140730           leavesr;
106200140730         endif;
106300140730         WMPaImpEx  = wValoreN;
106400140730
106500140730
106600140730         // -?Scrittura record in WFMDP00F?
106700140730         //______________
106800140730         WRITE  WFMDP000;
106900140730         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
107000140730
107100140730       ENDSR;
107200140730
107300140730       //--------------------------------------------------------------
107400140730       //?Reperimento valore del campo in elaborazione.                ?
107500140730       //--------------------------------------------------------------
107600140730       BEGSR  sr_GetFldVal;
107700140730
107800140730         // -?Calcolo Posizioni Iniziale e Finale?
107900140730         wPosI = wPosF + 1;
108000140731         wPosF = %scan( c_SepChar : xProsp_Dati : wPosI );
108100140730
108200140731         //// -?Verifica esistenza punteggiatura NON separatrice ('; '/'] '')?
108300140731         ////  ?SCARTANDO il controllo dal PENULTIMO campo del file?
108400140731         //If  wPosF > *zero  and  wCampo < ( c_MaxFldWF - 1 );
108500140731         //  DoW  %subst( xProsp_Dati : wPosF + 1 : 1 ) = *blank;
108600140731         //    wPosF = %scan( c_SepChar : xProsp_Dati : wPosF + 1 );
108700140731         //  EndDo;
108800140731         //EndIf;
108900140730
109000140730         // -?Impostazione finale se ultimo campo?
109100140730         if  wPosF = *zero;
109200140730           wPosF = %len( %trimr( xProsp_Dati ) ) + 1;
109300140730         endif;
109400140730
109500140730         // -?Estrazione dati da "convertire"?
109600140730         clear  wErr;
109700140730         clear  wValoreA;
109800140730         clear  wValoreN;
109900140730         clear  $Negativo;
110000140730         //clear  $Perc;
110100140730         // -?...SE reperiti dati?
110200140730         if  wPosF > wPosI;
110300140730           wValoreA = %trimL( %subst( xProsp_Dati : wPosI : wPosF - wPosI ) );
110400140730         endif;
110500140730
110600140730         Select;
110700140730
110800140730           // -?Campo vuoto?
110900140730           When  wValoreA = *blank;
111000140730             leavesr;
111100140730
111200140730           // -?Caratteri alfanumerici da convertire in Maiuscolo?
111300140730           When  wTipo = 'a';
111400140730             wValoreA = %xlate( c_Minu : c_Maiu : wValoreA );
111500140730
111600140730           // -?Caratteri numerici con segno e/o con imbolo "%"?
111700140730           When  wTipo = 'n';
111800140730             if  %scan( '-' : wValoreA ) > *zero;
111900140730               $Negativo = *on;
112000140730               %subst( wValoreA : %scan( '-' : wValoreA ) : 1 ) = ' ';
112100140730               wValoreA  = %trim( wValoreA );
112200140730             endif;
112300140730             //if  %scan( '%' : wValoreA ) > *zero;
112400140730             //  $Perc    = *on;
112500140730             //  %subst( wValoreA : %scan( '%' : wValoreA ) : 1 ) = ' ';
112600140730             //  wValoreA = %trim( wValoreA );
112700140730             //endif;
112800140730
112900140730           // -?Caratteri numerici per data (con separatore "/")?
113000140730           When  wTipo = 'D';
113100140730             wPosSep = %scan( '/' : wValoreA );
113200140730             doW  wPosSep > *zero;
113300140730               wValoreA = %subst( wValoreA : 1 : wPosSep - 1 ) +
113400140730                          %subst( wValoreA : wPosSep + 1 );
113500140730               wPosSep = %scan( '/' : wValoreA );
113600140730             enddo;
113700140730
113800140730         EndSl;
113900140730
114000140730         Select;
114100140730
114200140730           // -?Caratteri alfanumerici di cui controllare la lunghezza?
114300140730           When  wTipo = 'A';
114400140730             exsr  sr_CtrlAlfa;
114500140730
114600140730           // -?Caratteri alfanumerici da convertire in Numerici e?
114700140730           //  ?di cui controllare i decimali?
114800140730           When  wTipo = 'N'  or  wTipo = 'n'  or  wTipo = 'D';
114900140730             exsr  sr_CtrlNum;
115000140730
115100140730         EndSl;
115200140730
115300140730       ENDSR;
115400140730
115500140730       //--------------------------------------------------------------
115600140730       //?Controlli valori per campo alfanumerico.                     ?
115700140730       //--------------------------------------------------------------
115800140730       BEGSR  sr_CtrlAlfa;
115900140730
116000140730         // -?Trovati troppi caratteri?
116100140730         if  %len( %trimR( wValoreA ) ) > wLungh;
116200140730           wLungh = %len( %trimR( wValoreA ) );
116300140730           wErr = 3;
116400140730           exsr  sr_PrintErr;
116500140730           leavesr;
116600140730         endif;
116700140730
116800140730       ENDSR;
116900140730
117000140730       //--------------------------------------------------------------
117100140730       //?Controlli valori per campo numerico.                         ?
117200140730       //--------------------------------------------------------------
117300140730       BEGSR  sr_CtrlNum;
117400140730
117500140730         // -?Ricerca virgola (",") e Conteggio decimali (max 2)?
117600140730         wPosVirg = %scan( ',' : wValoreA );
117700140730
117800140730         // -?Campo numerico intero?
117900140730         If  wPosVirg = *zero;
118000140730
118100140730           // -?Trovati caratteri NON numerici?
118200140730           if  %check( c_Digits : %trimR( wValoreA ) ) > *zero;
118300140730             wErr = 4;
118400140730             exsr  sr_PrintErr;
118500140730             leavesr;
118600140730           endif;
118700140730           // -?Valorizzazione dato numerico?
118800140730           wValoreN = %int( %trimR( wValoreA ) );
118900140730
119000140730         // -?Campo numerico con decimali?
119100140730         Else;
119200140730
119300140730           For  wNumDec = 1  To  3;
119400140730             if  %subst( wValoreA : wPosVirg + wNumDec ) = *blank;
119500140730               leave;
119600140730             endif;
119700140730           EndFor;
119800140730           wNumDec -= 1;
119900140730
120000140730           Select;
120100140730
120200140730             When  wNumDec = *zero;
120300140730               // -?Trovati caratteri NON numerici?
120400140730               if  %check( c_Digits : %subst( wValoreA : 1 :
120500140730                                              wPosVirg - 1 ) ) > *zero;
120600140730                 wErr = 4;
120700140730                 exsr  sr_PrintErr;
120800140730                 leavesr;
120900140730               endif;
121000140730               // -?Valorizzazione dato numerico?
121100140730               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) );
121200140730
121300140730             When  wNumDec = 1;
121400140730               // -?Trovati caratteri NON numerici?
121500140730               if  %check( c_Digits : %subst( wValoreA : 1 :
121600140730                                              wPosVirg - 1 ) ) > *zero
121700140730                   or
121800140730                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
121900140730                                              1 ) ) > *zero;
122000140730                 wErr = 4;
122100140730                 exsr  sr_PrintErr;
122200140730                 leavesr;
122300140730               endif;
122400140730               // -?Valorizzazione dato numerico?
122500140730               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
122600140730                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
122700140730                                          1 ) ) / 10 );
122800140730
122900140730             //When  wNumDec = 2;
123000140730             Other;
123100140730               // -?Trovati caratteri NON numerici?
123200140730               if  %check( c_Digits : %subst( wValoreA : 1 :
123300140730                                              wPosVirg - 1 ) ) > *zero
123400140730                   or
123500140730                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
123600140730                                              2 ) ) > *zero;
123700140730                 wErr = 4;
123800140730                 exsr  sr_PrintErr;
123900140730                 leavesr;
124000140730               endif;
124100140730               // -?Valorizzazione dato numerico?
124200140730               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
124300140730                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
124400140730                                          2 ) ) / 100 );
124500140730             //Other;
124600140730             //  wErr = 5;
124700140730             //  exsr  sr_PrintErr;
124800140730             //  leavesr;
124900140730
125000140730           EndSl;
125100140730
125200140730         EndIf;
125300140730
125400140730         // -?Campo numerico troppo lungo?
125500140730         if  ( wPosVirg > *zero  and
125600140730               %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > wLungh )  OR
125700140730             ( wPosVirg = *zero  and
125800140730               %len( %trim( wValoreA ) ) > wLungh );
125900140730             //( wLungh   = 3      and (
126000140730             //  wPosVirg > *zero  and
126100140730             //  %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > 3         or
126200140730             //  wPosVirg = *zero  and
126300140730             //  %len( %trim( wValoreA ) ) > 3 ) );
126400140730           wErr = 6;
126500140730           exsr  sr_PrintErr;
126600140730           leavesr;
126700140730         endif;
126800140730
126900140730         // -?Campo negativo?
127000140730         if  $Negativo;
127100140730           wValoreN *= (-1);
127200140730         endif;
127300140730
127400140730         //// -?Percentuale?
127500140730         //if  $Perc;
127600140730         //  ...;
127700140730         //endif;
127800140730
127900140730       ENDSR;
128000140730
128100140730       //--------------------------------------------------------------
128200140730       //?Stampa segnalazione dell'errore rilevato.                    ?
128300140730       //--------------------------------------------------------------
128400140730       BEGSR  sr_PrintErr;
128500140730
128600140730         // -?Apertura del printer-file?
128700140730         if  Not %open(QSYSPRT);
128800140730           Qcmd = 'OVRPRTF file(QSYSPRT) +
128900140730                           usrdta(''ACPO02*ERR'') +
129000140730                           ovrscope(*actgrpdfn)';
129100140730           exsr  sr_ExecCmd;
129200140730           open  QSYSPRT;
129300140730           except  PRTtxt;
129400140730         endif;
129500140730
129600140730         if  *inOF;
129700140730           except  PRTtxt;
129800140730           *inOF = *off;
129900140730         endif;
130000140730
130100140730         Select;
130200140730           When  wErr = 1;
130300140730             except  PrtErr1;
130400140730           When  wErr = 2;
130500140730             except  PrtErr2;
130600140730           When  wErr = 3;
130700140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
130800140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
130900140730             except  PrtErr3;
131000140730           When  wErr = 4;
131100140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
131200140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
131300140730             except  PrtErr4;
131400140730           When  wErr = 5;
131500140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
131600140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
131700140730             except  PrtErr5;
131800140730           When  wErr = 6;
131900140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
132000140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
132100140730             except  PrtErr6;
132200140730         EndSl;
132300140730
132400140730       ENDSR;
132500140730
132600140730       //--------------------------------------------------------------
132700140730       //?Operazioni finali.                                           ?
132800140730       //--------------------------------------------------------------
132900140730       BEGSR  sr_RoutEnd;
133000140730
133100140730         // -?Chiusura spool di stampa errori?
133200140730         if  %open(QSYSPRT);
133300140730           except  PrtEND;
133400140730           close  QSYSPRT;
133500140730         endif;
133600140730
133700140730         // -?Chiusura Work-File di output?
133800140730         if  %open(PROSP_DATI);
133900140730           close PROSP_DATI;
134000140730         endif;
134100140730
134200140730         // -?Rimozione overridea al file di CRIBIS (PROSP_DATI)?
134300140730         Qcmd = 'DLTOVR file(PROSP_DATI) +
134400140730                        lvl(*actgrpdfn)';
134500140730         exsr  sr_ExecCmd;
134600140730
134700140730         // -?Chiusura *pgm?
134800140730         return;
134900140730
135000140730       ENDSR;
135100140730
135200140730       //--------------------------------------------------------------
135300140730       //?Esecuzione del comando (già impostato).                      ?
135400140730       //--------------------------------------------------------------
135500140730       BEGSR  sr_ExecCmd;
135600140730
135700140730         clear Qcap0100;
135800140730         Qcabcsdh = *off;
135900140730         Qcapa    = *off;
136000140730         Qcacmdss = *off;
136100140730         Qcaerved = *allX'00';
136200140730
136300140730         clear Qusec;
136400140730         Qusbprv  = %size(Qusec);
136500140730
136600140730         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
136700140730                           %size(Qcap0100) : 'CPOP0100' : *omit :
136800140730                           0 : 0 : Qusec);
136900140730
137000140730         //if  Qusei <> *blank;
137100140730         //  ...;
137200140730         //endif;
137300140730
137400140730       ENDSR;
137500140730
137600140730      /end-free
137700140730
137800140730       //--------------------------------------------------------------
137900140730       //?S P O O L - F I L E S                                        ?
138000140730       //--------------------------------------------------------------
138100140730
138200140730     oQSYSPRT   e            PRTtxt            1
138300140730     o                       RSUT
138400140730     o                                        +   4 'LISTA ERRORI RILE-
138500140730     o                                              VATI IN FASE DI EL-
138600140730     o                                              ABORAZIONE DATI NE-
138700140730     o                                              L FILE'
138800140730     o                       wLibFile         +   1
138900140730     o                       SDSpgmName       +   4
139000140730     o                       *Date         y  +   2
139100140730     o          e            PRTtxt      0
139200140730     o                                          +24 'LISTA ERRORI RILE-
139300140730     o                                              VATI IN FASE DI EL-
139400140730     o                                              ABORAZIONE DATI NE-
139500140730     o                                              L FILE'
139600140730     o                       wLibFile         +   1
139700140730     o          e            PRTtxt      1
139800140730     o                       KNSIF
139900140730     o                       KNMUS            +   1
140000140730     o                                        +   3 '------------------
140100140730     o                                              -------------------
140200140730     o                                              -------------------
140300140730     o                                              -------------------
140400140730     o                                              ----------'
140500140730     o                                        +   4 'Pag.'
140600140730     o                       Page          z  +   0
140700140730     o                       wTime            +   4 '  :  :  '
140800140730      *
140900140730     o          e            PRTtxt      2
141000140730     o                                              'CRIF Number    '
141100140730     o                                        +   2 'Messaggio'
141200140730     o          e            PRTtxt      1
141300140730     o                                              '---------------'
141400140730     o                                        +   2 '---------'
141500140730      *
141600140730     o          e            PRTerr1     2
141700140730     o                                              'NON RIUSCITA LA +
141800140730     o                                               PULIZIA DEL WORK-+
141900140730     o                                               FILE WFMDP00F'
142000140730      *
142100140730     o          e            PRTerr2     2
142200140730     o                                              'NON TROVATO IL +
142300140730     o                                               FILE'
142400140730     o                       wLibWF           +   1
142500140730      *
142600140730     o          e            PRTerr3     2
142700140730     o                       wCrifNr
142800140730     o                                        +   2 'Troppi caratteri ('
142900140730     o                       wLungh        z  +   0
143000140730     o                                        +   0 ') nel'
143100140730     o                       wCampo        z  +   1
143200140730     o                                        +   0 '° campo:'
143300140730     o                       $Campi(wCampo)   +   1
143400140730     o          e            PRTerr3     1
143500140730     o                       wValoreA1        +   5
143600140730     o          e            PRTerr3     1
143700140730     o                       wValoreA2        +   5
143800140730      *
143900140730     o          e            PRTerr4     2
144000140730     o                       wCrifNr
144100140730     o                                        +   2 'Dati NON numerici +
144200140730     o                                               nel'
144300140730     o                       wCampo        z  +   1
144400140730     o                                        +   0 '° campo:'
144500140730     o                       $Campi(wCampo)   +   1
144600140730     o          e            PRTerr4     1
144700140730     o                       wValoreA1        +   5
144800140730     o          e            PRTerr4     1
144900140730     o                       wValoreA2        +   5
145000140730      *
145100140730     o          e            PRTerr5     2
145200140730     o                       wCrifNr
145300140730     o                                        +   2 'Numero decimali ('
145400140730     o                       wNumDec       z  +   0
145500140730     o                                        +   0 ') errato nel'
145600140730     o                       wCampo        z  +   1
145700140730     o                                        +   0 '° campo:'
145800140730     o                       $Campi(wCampo)   +   1
145900140730     o          e            PRTerr5     1
146000140730     o                       wValoreA1        +   5
146100140730     o          e            PRTerr5     1
146200140730     o                       wValoreA2        +   5
146300140730      *
146400140730     o          e            PRTerr6     2
146500140730     o                       wCrifNr
146600140730     o                                        +   2 'Troppe cifre ('
146700140730     o                       wLungh        z  +   0
146800140730     o                                        +   0 ') nel'
146900140730     o                       wCampo        z  +   1
147000140730     o                                        +   0 '° campo:'
147100140730     o                       $Campi(wCampo)   +   1
147200140730     o          e            PRTerr6     1
147300140730     o                       wValoreA1        +   5
147400140730     o          e            PRTerr6     1
147500140730     o                       wValoreA2        +   5
147600140730      *
147700140730     o          e            PRTend      2
147800140730     o                                        +  24 '***  Fine Lista  ***'
147900140730     o          e            PRTend      0
148000140730     o                                        +  24 '***  Fine Lista  ***'
148100140730
148200140730       //--------------------------------------------------------------
148300140730       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
148400140730       //--------------------------------------------------------------
148500140730
148600140730** --?$Campi -----------*?Nomi di campi da indicare in stampa?
148700140730Crif Number                 1
148800140730OOB Indicator               2
148900140730Denominazione               3
149000140730Indirizzo                   4
149100140730Comune                      5
149200140730Cap                         6
149300140730Regione                     7
149400140730Frazione                    8
149500140730Provincia                   9
149600140730Provincia estesa           10
149700140730Codice Fiscale             11
149800140730Partita Iva                12
149900140730Telefono 1                 13
150000140730Fax 1                      14
150100140730Sito Internet 1            15
150200140730Flag E-commerce            16
150300140730Capitale sociale Euro      17
150400140730Anno Costituzione          18
150500140730Anno Fondazione            19
150600140730Data inizio attività       20
150700140730Ateco (2007)  1            21
150800140730Descrizione codice ateco   22
150900140730Sic primario               23
151000140730Descrizione Sic            24
151100140730Dipendenti                 25
151200140730Natura giuridica           26
151300140730CCIAA                      27
151400140730REA                        28
151500140730Oggetto sociale            29
151600140730Stato Impresa Cribis       30
151700140730Denominazione Esp 1        31
151800140730Cognome Esp 1              32
151900140730Nome Esp 1                 33
152000140730Sesso Esp 1                34
152100140730Carica Esp 1               35
152200140730Denominazione Esp 2        36
152300140730Cognome Esp 2              37
152400140730Nome Esp 2                 38
152500140730Sesso Esp 2                39
152600140730Carica Esp 2               40
152700140730Denominazione Esp 3        41
152800140730Cognome Esp 3              42
152900140730Nome Esp 3                 43
153000140730Sesso Esp 3                44
153100140730Carica Esp 3               45
153200140730Denominazione Esp 4        46
153300140730Cognome Esp 4              47
153400140730Nome Esp 4                 48
153500140730Sesso Esp 4                49
153600140730Carica Esp 4               50
153700140730Denominazione Esp 5        51
153800140730Cognome Esp 5              52
153900140730Nome Esp 5                 53
154000140730Sesso Esp 5                54
154100140730Carica Esp 5               55
154200140730Denominazione Esp 6        56
154300140730Cognome Esp 6              57
154400140730Nome Esp 6                 58
154500140730Sesso Esp 6                59
154600140730Carica Esp 6               60
154700140730Denominazione Esp 7        61
154800140730Cognome Esp 7              62
154900140730Nome Esp 7                 63
155000140730Sesso Esp 7                64
155100140730Carica Esp 7               65
155200140730Denominazione Esp 8        66
155300140730Cognome Esp 8              67
155400140730Nome Esp 8                 68
155500140730Sesso Esp 8                69
155600140730Carica Esp 8               70
155700140730Fatturato Euro             71
155800140730Anno Fatturato             72
155900140730Provenienza Fatturato      73
156000140730Utile/Perdita              74
156100140730% import                   75
156200140730% export                   76
156300140730Crif Number casa madre     77
156400140730Nome casa madre            78
156500140730Indirizzo casa madre       79
156600140730Cap casa madre             80
156700140730Città casa madre           81
156800140730Provincia casa madre       82
156900140730Nazione casa madre         83
157000140730FlagRating4                84
157100140730% impex                    85
157200140730anno %impex                86
