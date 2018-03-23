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
004900140731     d*// c_SepChar       c                   const(';')
005000140731     d c_SepChar       c                   const(']')
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
017400140731       if  %subst( xProsp_Dati : 1 : 26 ) = 'Crif Number;OOB Indicator;';
017500140730         read  PROSPDATI;
017600140730       endif;
017700140730
017800140730       DoW  Not %eof(PROSP_DATI);
017900140730
018000140730         exsr  sr_ElabPot;
018100140730
018200140730         read  PROSPDATI;
018300140730
018400140730       EndDo;
018500140730
018600140730       // -?Operazioni finali?
018700140730       exsr sr_RoutEnd;
018800140730
018900140730       //--------------------------------------------------------------
019000140730       //?Operazioni iniziali.                                         ?
019100140730       //--------------------------------------------------------------
019200140730       BEGSR  sr_RoutInz;
019300140730
019400140730         // -?Impostazione chiusura?
019500140730         *inLR = *on;
019600140730
019700140730         // -?Ricezione parametri?
019800140730         if  %parms() > *zero;
019900140730           KPJBA = wKPJBA;
020000140730         else;
020100140730           knsif = *all'? ';
020200140730           knmus = SDSjobUser;
020300140730         endif;
020400140730
020500140730         // -?Reperimento data odierna in formato aaaammgg?
020600140730         //wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
020700140730         wDate_Iso = %date();
020800140730         wDate_Eur = wDate_Iso;
020900140730         wDate = %dec( wDate_Eur );
021000140730
021100140730         // -?Reperimento orario?
021200140730         wTime = %dec( %time() );
021300140730
021400140730         // -?Reperimento dati job?
021500140730         exsr  sr_DatiJob;
021600140730
021700140730         // -?Pulizia del file di Output?
021800140730         Qcmd = 'CLRPFM file(*libl/WFMDP00F)';
021900140730         exsr  sr_ExecCmd;
022000140730         if Qusei <> *blank;
022100140730           wErr = 1;
022200140730           exsr  sr_PrintErr;
022300140730           exsr  sr_RoutEnd;
022400140730         endif;
022500140730
022600140730         open  WFMDP00F;
022700140730
022800140730         // -?Impostazione libreria del file di CRIBIS (PROSP_DATI)?
022900140730         if  %parms() > 1  and  kpjbu <> *blank;
023000140730           wLibWF = %subst( kpjbu : 1 : 10);
023100140730         endif;
023200140730
023300140730         // -?Apertura del file di CRIBIS (PROSP_DATI)?
023400140730         wLibFile = %trim( wLibWF ) + '/PROSP_DATI';
023500140730         open(e)  PROSP_DATI;
023600140730         if %error();
023700140730           wErr = 2;
023800140730           exsr  sr_PrintErr;
023900140730           exsr  sr_RoutEnd;
024000140730         endif;
024100140730
024200140730       ENDSR;
024300140730
024400140730       //--------------------------------------------------------------
024500140730       //?Reperimento Dati del job (Utente/Operativi).                 ?
024600140730       //--------------------------------------------------------------
024700140730       BEGSR  sr_DatiJob;
024800140730
024900140730         in(e) §AzUte;
025000140730         if NOT %error;
025100140730           in(e) §DatiUte;
025200140730         endif;
025300140730         if %error or RSut = *blank;
025400140730           tibs34r ( tibs34ds );
025500140730           in §AzUte;
025600140730           in §DatiUte;
025700140730         endif;
025800140730
025900140730       ENDSR;
026000140730
026100140730       //--------------------------------------------------------------
026200140730       //?Elaborazione singolo cliente Potenziale.                     ?
026300140730       //--------------------------------------------------------------
026400140730       BEGSR  sr_ElabPot;
026500140730
026600140730         clear  WFMDP000;
026700140730
026800140730         clear  wPosI;
026900140730         clear  wPosF;
027000140730         clear  wCampo;
027100140730         clear  wCrifNr;
027200140730
027300140730         // -?  1° campo: Crif Number             ?
027400140730         wCampo += 1;
027500140730         wTipo   = 'A';        //?- numerico?
027600140730         wLungh  = %len( WMPcrif );
027700140730         exsr  sr_GetFldVal;
027800140730         if  wErr > *zero;
027900140730           leavesr;
028000140730         endif;
028100140730         WMPcrif    = wValoreA;
028200140730         wCrifNr    = wValoreA;
028300140730         // -?  2° campo: OOB Indicator           ?
028400140730         wCampo += 1;
028500140730         wTipo   = 'A';        //?- alfanumerico?
028600140730         wLungh  = %len( WMPoob );
028700140730         exsr  sr_GetFldVal;
028800140730         if  wErr > *zero;
028900140730           leavesr;
029000140730         endif;
029100140730         WMPoob     = wValoreA;
029200140730         // -?  3° campo: Denominazione           ?
029300140730         wCampo += 1;
029400140730         wTipo   = 'A';        //?- alfanumerico?
029500140730         wLungh  = %len( WMPdeno );
029600140730         exsr  sr_GetFldVal;
029700140730         if  wErr > *zero;
029800140730           leavesr;
029900140730         endif;
030000140730         WMPdeno    = wValoreA;
030100140730         // -?  4° campo: Indirizzo               ?
030200140730         wCampo += 1;
030300140730         wTipo   = 'A';        //?- alfanumerico?
030400140730         wLungh  = %len( WMPind );
030500140730         exsr  sr_GetFldVal;
030600140730         if  wErr > *zero;
030700140730           leavesr;
030800140730         endif;
030900140730         WMPind     = wValoreA;
031000140730         // -?  5° campo: Comune                  ?
031100140730         wCampo += 1;
031200140730         wTipo   = 'A';        //?- alfanumerico?
031300140730         wLungh  = %len( WMPloc );
031400140730         exsr  sr_GetFldVal;
031500140730         if  wErr > *zero;
031600140730           leavesr;
031700140730         endif;
031800140730         WMPloc     = wValoreA;
031900140730         // -?  6° campo: Cap                     ?
032000140730         wCampo += 1;
032100140730         wTipo   = 'N';        //?- numerico?
032200140730         wLungh  = %len( WMPcap );
032300140730         exsr  sr_GetFldVal;
032400140730         if  wErr > *zero;
032500140730           leavesr;
032600140730         endif;
032700140730         WMPcap     = wValoreA;
032800140730         // -?  7° campo: Regione                 ?
032900140730         wCampo += 1;
033000140730         wTipo   = 'A';        //?- alfanumerico?
033100140730         wLungh  = %len( WMPreg );
033200140730         exsr  sr_GetFldVal;
033300140730         if  wErr > *zero;
033400140730           leavesr;
033500140730         endif;
033600140730         WMPreg     = wValoreA;
033700140730         // -?  8° campo: Frazione                ?
033800140730         wCampo += 1;
033900140730         wTipo   = 'A';        //?- alfanumerico?
034000140730         wLungh  = %len( WMPfra );
034100140730         exsr  sr_GetFldVal;
034200140730         if  wErr > *zero;
034300140730           leavesr;
034400140730         endif;
034500140730         WMPfra     = wValoreA;
034600140730         // -?  9° campo: Provincia               ?
034700140730         wCampo += 1;
034800140730         wTipo   = 'A';        //?- alfanumerico?
034900140730         wLungh  = %len( WMPprv );
035000140730         exsr  sr_GetFldVal;
035100140730         if  wErr > *zero;
035200140730           leavesr;
035300140730         endif;
035400140730         WMPprv     = wValoreA;
035500140730         // -? 10° campo: Provincia estesa        ?
035600140730         wCampo += 1;
035700140730         wTipo   = 'a';        //?- alfanumerico anche minuscolo?
035800140730         wLungh  = %len( WMPprvDes );
035900140730         exsr  sr_GetFldVal;
036000140730         if  wErr > *zero;
036100140730           leavesr;
036200140730         endif;
036300140730         WMPprvDes  = wValoreA;
036400140730         // -? 11° campo: Codice Fiscale          ?
036500140730         wCampo += 1;
036600140730         wTipo   = 'A';        //?- alfanumerico?
036700140730         wLungh  = %len( WMPcfi );
036800140730         exsr  sr_GetFldVal;
036900140730         if  wErr > *zero;
037000140730           leavesr;
037100140730         endif;
037200140730         WMPcfi     = wValoreA;
037300140730         // -? 12° campo: Partita Iva             ?
037400140730         wCampo += 1;
037500140730         wTipo   = 'A';        //?- alfanumerico?
037600140730         wLungh  = %len( WMPcpi );
037700140730         exsr  sr_GetFldVal;
037800140730         if  wErr > *zero;
037900140730           leavesr;
038000140730         endif;
038100140730         WMPcpi     = wValoreA;
038200140730         // -? 13° campo: Telefono 1              ?
038300140730         wCampo += 1;
038400140730         wTipo   = 'A';        //?- alfanumerico?
038500140730         wLungh  = %len( WMPtel );
038600140730         exsr  sr_GetFldVal;
038700140730         if  wErr > *zero;
038800140730           leavesr;
038900140730         endif;
039000140730         WMPtel     = wValoreA;
039100140730         // -? 14° campo: Fax 1                   ?
039200140730         wCampo += 1;
039300140730         wTipo   = 'A';        //?- alfanumerico?
039400140730         wLungh  = %len( WMPfax );
039500140730         exsr  sr_GetFldVal;
039600140730         if  wErr > *zero;
039700140730           leavesr;
039800140730         endif;
039900140730         WMPfax     = wValoreA;
040000140730         // -? 15° campo: Sito Internet 1         ?
040100140730         wCampo += 1;
040200140730         wTipo   = 'A';        //?- alfanumerico?
040300140730         wLungh  = %len( WMPsito );
040400140730         exsr  sr_GetFldVal;
040500140730         if  wErr > *zero;
040600140730           leavesr;
040700140730         endif;
040800140730         WMPsito    = wValoreA;
040900140730         // -? 16° campo: Flag E-commerce         ?
041000140730         wCampo += 1;
041100140730         wTipo   = 'A';        //?- alfanumerico?
041200140730         wLungh  = %len( WMPecom );
041300140730         exsr  sr_GetFldVal;
041400140730         if  wErr > *zero;
041500140730           leavesr;
041600140730         endif;
041700140730         WMPecom    = wValoreA;
041800140730         // -? 17° campo: Capitale sociale Euro   ?
041900140730         wCampo += 1;
042000140730         wTipo   = 'N';        //?- numerico?
042100140730         wLungh  = %len( WMPcapSoc );
042200140730         exsr  sr_GetFldVal;
042300140730         if  wErr > *zero;
042400140730           leavesr;
042500140730         endif;
042600140730         WMPcapSoc  = wValoreN;
042700140730         // -? 18° campo: Anno Costituzione       ?
042800140730         wCampo += 1;
042900140730         wTipo   = 'N';        //?- numerico?
043000140730         wLungh  = %len( WMPaaCos );
043100140730         exsr  sr_GetFldVal;
043200140730         if  wErr > *zero;
043300140730           leavesr;
043400140730         endif;
043500140730         WMPaaCos   = wValoreN;
043600140730         // -? 19° campo: Anno Fondazione         ?
043700140730         wCampo += 1;
043800140730         wTipo   = 'N';        //?- numerico?
043900140730         wLungh  = %len( WMPaaFon );
044000140730         exsr  sr_GetFldVal;
044100140730         if  wErr > *zero;
044200140730           leavesr;
044300140730         endif;
044400140730         WMPaaFon   = wValoreN;
044500140730         // -? 20° campo: Data inizio attività    ?
044600140730         wCampo += 1;
044700140730         wTipo   = 'D';        //?- data (gg/mm/aaaa)?
044800140730         wLungh  = %len( WMPdtIni );
044900140730         exsr  sr_GetFldVal;
045000140730         if  wErr > *zero;
045100140730           leavesr;
045200140730         endif;
045300140730         WMPdtIni   = wValoreN;
045400140730         // -? 21° campo: Ateco (2007)  1         ?
045500140730         wCampo += 1;
045600140730         wTipo   = 'A';        //?- alfanumerico?
045700140730         wLungh  = %len( WMPateco );
045800140730         exsr  sr_GetFldVal;
045900140730         if  wErr > *zero;
046000140730           leavesr;
046100140730         endif;
046200140730         WMPateco   = wValoreA;
046300140730         // -? 22° campo: Descrizione codice ateco?
046400140730         wCampo += 1;
046500140730         wTipo   = 'A';        //?- alfanumerico?
046600140730         wLungh  = %len( WMPatecoDe );
046700140730         exsr  sr_GetFldVal;
046800140730         if  wErr > *zero;
046900140730           leavesr;
047000140730         endif;
047100140730         WMPatecoDe = wValoreA;
047200140730         // -? 23° campo: Sic primario            ?
047300140730         wCampo += 1;
047400140730         wTipo   = 'A';        //?- alfanumerico?
047500140730         wLungh  = %len( WMPsicP );
047600140730         exsr  sr_GetFldVal;
047700140730         if  wErr > *zero;
047800140730           leavesr;
047900140730         endif;
048000140730         WMPsicP    = wValoreA;
048100140730         // -? 24° campo: Descrizione Sic         ?
048200140730         wCampo += 1;
048300140730         wTipo   = 'A';        //?- alfanumerico?
048400140730         wLungh  = %len( WMPsicPdes );
048500140730         exsr  sr_GetFldVal;
048600140730         if  wErr > *zero;
048700140730           leavesr;
048800140730         endif;
048900140730         WMPsicPdes = wValoreA;
049000140730         // -? 25° campo: Dipendenti              ?
049100140730         wCampo += 1;
049200140730         wTipo   = 'N';        //?- numerico?
049300140730         wLungh  = %len( WMPdip );
049400140730         exsr  sr_GetFldVal;
049500140730         if  wErr > *zero;
049600140730           leavesr;
049700140730         endif;
049800140730         WMPdip     = wValoreN;
049900140730         // -? 26° campo: Natura giuridica        ?
050000140730         wCampo += 1;
050100140730         wTipo   = 'A';        //?- alfanumerico?
050200140730         wLungh  = %len( WMPnatG );
050300140730         exsr  sr_GetFldVal;
050400140730         if  wErr > *zero;
050500140730           leavesr;
050600140730         endif;
050700140730         WMPnatG    = wValoreA;
050800140730         // -? 27° campo: CCIAA                   ?
050900140730         wCampo += 1;
051000140730         wTipo   = 'A';        //?- alfanumerico?
051100140730         wLungh  = %len( WMPcciaa );
051200140730         exsr  sr_GetFldVal;
051300140730         if  wErr > *zero;
051400140730           leavesr;
051500140730         endif;
051600140730         WMPcciaa   = wValoreA;
051700140730         // -? 28° campo: REA                     ?
051800140730         wCampo += 1;
051900140730         wTipo   = 'A';        //?- alfanumerico?
052000140730         wLungh  = %len( WMPrea );
052100140730         exsr  sr_GetFldVal;
052200140730         if  wErr > *zero;
052300140730           leavesr;
052400140730         endif;
052500140730         WMPrea     = wValoreA;
052600140730         // -? 29° campo: Oggetto sociale         ?
052700140730         wCampo += 1;
052800140730         wTipo   = 'A';        //?- alfanumerico?
052900140730         wLungh  = %len( WMPoggSoc );
053000140730         exsr  sr_GetFldVal;
053100140730         if  wErr > *zero;
053200140730           leavesr;
053300140730         endif;
053400140730         WMPoggSoc  = wValoreA;
053500140730         // -? 30° campo: Stato Impresa Cribis    ?
053600140730         wCampo += 1;
053700140730         wTipo   = 'A';        //?- alfanumerico?
053800140730         wLungh  = %len( WMPstImp );
053900140730         exsr  sr_GetFldVal;
054000140730         if  wErr > *zero;
054100140730           leavesr;
054200140730         endif;
054300140730         WMPstImp   = wValoreA;
054400140730         // -? 31° campo: Denominazione Esp 1     ?
054500140730         wCampo += 1;
054600140730         wTipo   = 'A';        //?- alfanumerico?
054700140730         wLungh  = %len( WMPdeno1 );
054800140730         exsr  sr_GetFldVal;
054900140730         if  wErr > *zero;
055000140730           leavesr;
055100140730         endif;
055200140730         WMPdeno1   = wValoreA;
055300140730         // -? 32° campo: Cognome Esp 1           ?
055400140730         wCampo += 1;
055500140730         wTipo   = 'A';        //?- alfanumerico?
055600140730         wLungh  = %len( WMPcogn1 );
055700140730         exsr  sr_GetFldVal;
055800140730         if  wErr > *zero;
055900140730           leavesr;
056000140730         endif;
056100140730         WMPcogn1   = wValoreA;
056200140730         // -? 33° campo: Nome Esp 1              ?
056300140730         wCampo += 1;
056400140730         wTipo   = 'A';        //?- alfanumerico?
056500140730         wLungh  = %len( WMPnome1 );
056600140730         exsr  sr_GetFldVal;
056700140730         if  wErr > *zero;
056800140730           leavesr;
056900140730         endif;
057000140730         WMPnome1   = wValoreA;
057100140730         // -? 34° campo: Sesso Esp 1             ?
057200140730         wCampo += 1;
057300140730         wTipo   = 'A';        //?- alfanumerico?
057400140730         wLungh  = %len( WMPsess1 );
057500140730         exsr  sr_GetFldVal;
057600140730         if  wErr > *zero;
057700140730           leavesr;
057800140730         endif;
057900140730         WMPsess1   = wValoreA;
058000140730         // -? 35° campo: Carica Esp 1            ?
058100140730         wCampo += 1;
058200140730         wTipo   = 'A';        //?- alfanumerico?
058300140730         wLungh  = %len( WMPcari1 );
058400140730         exsr  sr_GetFldVal;
058500140730         if  wErr > *zero;
058600140730           leavesr;
058700140730         endif;
058800140730         WMPcari1   = wValoreA;
058900140730         // -? 36° campo: Denominazione Esp 2     ?
059000140730         wCampo += 1;
059100140730         wTipo   = 'A';        //?- alfanumerico?
059200140730         wLungh  = %len( WMPdeno2 );
059300140730         exsr  sr_GetFldVal;
059400140730         if  wErr > *zero;
059500140730           leavesr;
059600140730         endif;
059700140730         WMPdeno2   = wValoreA;
059800140730         // -? 37° campo: Cognome Esp 2           ?
059900140730         wCampo += 1;
060000140730         wTipo   = 'A';        //?- alfanumerico?
060100140730         wLungh  = %len( WMPcogn2 );
060200140730         exsr  sr_GetFldVal;
060300140730         if  wErr > *zero;
060400140730           leavesr;
060500140730         endif;
060600140730         WMPcogn2   = wValoreA;
060700140730         // -? 38° campo: Nome Esp 2              ?
060800140730         wCampo += 1;
060900140730         wTipo   = 'A';        //?- alfanumerico?
061000140730         wLungh  = %len( WMPnome2 );
061100140730         exsr  sr_GetFldVal;
061200140730         if  wErr > *zero;
061300140730           leavesr;
061400140730         endif;
061500140730         WMPnome2   = wValoreA;
061600140730         // -? 39° campo: Sesso Esp 2             ?
061700140730         wCampo += 1;
061800140730         wTipo   = 'A';        //?- alfanumerico?
061900140730         wLungh  = %len( WMPsess2 );
062000140730         exsr  sr_GetFldVal;
062100140730         if  wErr > *zero;
062200140730           leavesr;
062300140730         endif;
062400140730         WMPsess2   = wValoreA;
062500140730         // -? 40° campo: Carica Esp 2            ?
062600140730         wCampo += 1;
062700140730         wTipo   = 'A';        //?- alfanumerico?
062800140730         wLungh  = %len( WMPcari2 );
062900140730         exsr  sr_GetFldVal;
063000140730         if  wErr > *zero;
063100140730           leavesr;
063200140730         endif;
063300140730         WMPcari2   = wValoreA;
063400140730         // -? 41° campo: Denominazione Esp 3     ?
063500140730         wCampo += 1;
063600140730         wTipo   = 'A';        //?- alfanumerico?
063700140730         wLungh  = %len( WMPdeno3 );
063800140730         exsr  sr_GetFldVal;
063900140730         if  wErr > *zero;
064000140730           leavesr;
064100140730         endif;
064200140730         WMPdeno3   = wValoreA;
064300140730         // -? 42° campo: Cognome Esp 3           ?
064400140730         wCampo += 1;
064500140730         wTipo   = 'A';        //?- alfanumerico?
064600140730         wLungh  = %len( WMPcogn3 );
064700140730         exsr  sr_GetFldVal;
064800140730         if  wErr > *zero;
064900140730           leavesr;
065000140730         endif;
065100140730         WMPcogn3   = wValoreA;
065200140730         // -? 43° campo: Nome Esp 3              ?
065300140730         wCampo += 1;
065400140730         wTipo   = 'A';        //?- alfanumerico?
065500140730         wLungh  = %len( WMPnome3 );
065600140730         exsr  sr_GetFldVal;
065700140730         if  wErr > *zero;
065800140730           leavesr;
065900140730         endif;
066000140730         WMPnome3   = wValoreA;
066100140730         // -? 44° campo: Sesso Esp 3             ?
066200140730         wCampo += 1;
066300140730         wTipo   = 'A';        //?- alfanumerico?
066400140730         wLungh  = %len( WMPsess3 );
066500140730         exsr  sr_GetFldVal;
066600140730         if  wErr > *zero;
066700140730           leavesr;
066800140730         endif;
066900140730         WMPsess3   = wValoreA;
067000140730         // -? 40° campo: Carica Esp 3            ?
067100140730         wCampo += 1;
067200140730         wTipo   = 'A';        //?- alfanumerico?
067300140730         wLungh  = %len( WMPcari3 );
067400140730         exsr  sr_GetFldVal;
067500140730         if  wErr > *zero;
067600140730           leavesr;
067700140730         endif;
067800140730         WMPcari3   = wValoreA;
067900140730         // -? 46° campo: Denominazione Esp 4     ?
068000140730         wCampo += 1;
068100140730         wTipo   = 'A';        //?- alfanumerico?
068200140730         wLungh  = %len( WMPdeno4 );
068300140730         exsr  sr_GetFldVal;
068400140730         if  wErr > *zero;
068500140730           leavesr;
068600140730         endif;
068700140730         WMPdeno4   = wValoreA;
068800140730         // -? 47° campo: Cognome Esp 4           ?
068900140730         wCampo += 1;
069000140730         wTipo   = 'A';        //?- alfanumerico?
069100140730         wLungh  = %len( WMPcogn4 );
069200140730         exsr  sr_GetFldVal;
069300140730         if  wErr > *zero;
069400140730           leavesr;
069500140730         endif;
069600140730         WMPcogn4   = wValoreA;
069700140730         // -? 48° campo: Nome Esp 4              ?
069800140730         wCampo += 1;
069900140730         wTipo   = 'A';        //?- alfanumerico?
070000140730         wLungh  = %len( WMPnome4 );
070100140730         exsr  sr_GetFldVal;
070200140730         if  wErr > *zero;
070300140730           leavesr;
070400140730         endif;
070500140730         WMPnome4   = wValoreA;
070600140730         // -? 49° campo: Sesso Esp 4             ?
070700140730         wCampo += 1;
070800140730         wTipo   = 'A';        //?- alfanumerico?
070900140730         wLungh  = %len( WMPsess4 );
071000140730         exsr  sr_GetFldVal;
071100140730         if  wErr > *zero;
071200140730           leavesr;
071300140730         endif;
071400140730         WMPsess4   = wValoreA;
071500140730         // -? 50° campo: Carica Esp 4            ?
071600140730         wCampo += 1;
071700140730         wTipo   = 'A';        //?- alfanumerico?
071800140730         wLungh  = %len( WMPcari4 );
071900140730         exsr  sr_GetFldVal;
072000140730         if  wErr > *zero;
072100140730           leavesr;
072200140730         endif;
072300140730         WMPcari4   = wValoreA;
072400140730         // -? 51° campo: Denominazione Esp 5     ?
072500140730         wCampo += 1;
072600140730         wTipo   = 'A';        //?- alfanumerico?
072700140730         wLungh  = %len( WMPdeno5 );
072800140730         exsr  sr_GetFldVal;
072900140730         if  wErr > *zero;
073000140730           leavesr;
073100140730         endif;
073200140730         WMPdeno5   = wValoreA;
073300140730         // -? 52° campo: Cognome Esp 5           ?
073400140730         wCampo += 1;
073500140730         wTipo   = 'A';        //?- alfanumerico?
073600140730         wLungh  = %len( WMPcogn5 );
073700140730         exsr  sr_GetFldVal;
073800140730         if  wErr > *zero;
073900140730           leavesr;
074000140730         endif;
074100140730         WMPcogn5   = wValoreA;
074200140730         // -? 53° campo: Nome Esp 5              ?
074300140730         wCampo += 1;
074400140730         wTipo   = 'A';        //?- alfanumerico?
074500140730         wLungh  = %len( WMPnome5 );
074600140730         exsr  sr_GetFldVal;
074700140730         if  wErr > *zero;
074800140730           leavesr;
074900140730         endif;
075000140730         WMPnome5   = wValoreA;
075100140730         // -? 54° campo: Sesso Esp 5             ?
075200140730         wCampo += 1;
075300140730         wTipo   = 'A';        //?- alfanumerico?
075400140730         wLungh  = %len( WMPsess5 );
075500140730         exsr  sr_GetFldVal;
075600140730         if  wErr > *zero;
075700140730           leavesr;
075800140730         endif;
075900140730         WMPsess5   = wValoreA;
076000140730         // -? 55° campo: Carica Esp 5            ?
076100140730         wCampo += 1;
076200140730         wTipo   = 'A';        //?- alfanumerico?
076300140730         wLungh  = %len( WMPcari5 );
076400140730         exsr  sr_GetFldVal;
076500140730         if  wErr > *zero;
076600140730           leavesr;
076700140730         endif;
076800140730         WMPcari5   = wValoreA;
076900140730         // -? 56° campo: Denominazione Esp 6     ?
077000140730         wCampo += 1;
077100140730         wTipo   = 'A';        //?- alfanumerico?
077200140730         wLungh  = %len( WMPdeno6 );
077300140730         exsr  sr_GetFldVal;
077400140730         if  wErr > *zero;
077500140730           leavesr;
077600140730         endif;
077700140730         WMPdeno6   = wValoreA;
077800140730         // -? 57° campo: Cognome Esp 6           ?
077900140730         wCampo += 1;
078000140730         wTipo   = 'A';        //?- alfanumerico?
078100140730         wLungh  = %len( WMPcogn6 );
078200140730         exsr  sr_GetFldVal;
078300140730         if  wErr > *zero;
078400140730           leavesr;
078500140730         endif;
078600140730         WMPcogn6   = wValoreA;
078700140730         // -? 58° campo: Nome Esp 6              ?
078800140730         wCampo += 1;
078900140730         wTipo   = 'A';        //?- alfanumerico?
079000140730         wLungh  = %len( WMPnome6 );
079100140730         exsr  sr_GetFldVal;
079200140730         if  wErr > *zero;
079300140730           leavesr;
079400140730         endif;
079500140730         WMPnome6   = wValoreA;
079600140730         // -? 59° campo: Sesso Esp 6             ?
079700140730         wCampo += 1;
079800140730         wTipo   = 'A';        //?- alfanumerico?
079900140730         wLungh  = %len( WMPsess6 );
080000140730         exsr  sr_GetFldVal;
080100140730         if  wErr > *zero;
080200140730           leavesr;
080300140730         endif;
080400140730         WMPsess6   = wValoreA;
080500140730         // -? 60° campo: Carica Esp 6            ?
080600140730         wCampo += 1;
080700140730         wTipo   = 'A';        //?- alfanumerico?
080800140730         wLungh  = %len( WMPcari6 );
080900140730         exsr  sr_GetFldVal;
081000140730         if  wErr > *zero;
081100140730           leavesr;
081200140730         endif;
081300140730         WMPcari6   = wValoreA;
081400140730         // -? 61° campo: Denominazione Esp 7     ?
081500140730         wCampo += 1;
081600140730         wTipo   = 'A';        //?- alfanumerico?
081700140730         wLungh  = %len( WMPdeno7 );
081800140730         exsr  sr_GetFldVal;
081900140730         if  wErr > *zero;
082000140730           leavesr;
082100140730         endif;
082200140730         WMPdeno7   = wValoreA;
082300140730         // -? 62° campo: Cognome Esp 7           ?
082400140730         wCampo += 1;
082500140730         wTipo   = 'A';        //?- alfanumerico?
082600140730         wLungh  = %len( WMPcogn7 );
082700140730         exsr  sr_GetFldVal;
082800140730         if  wErr > *zero;
082900140730           leavesr;
083000140730         endif;
083100140730         WMPcogn7   = wValoreA;
083200140730         // -? 63° campo: Nome Esp 7              ?
083300140730         wCampo += 1;
083400140730         wTipo   = 'A';        //?- alfanumerico?
083500140730         wLungh  = %len( WMPnome7 );
083600140730         exsr  sr_GetFldVal;
083700140730         if  wErr > *zero;
083800140730           leavesr;
083900140730         endif;
084000140730         WMPnome7   = wValoreA;
084100140730         // -? 64° campo: Sesso Esp 7             ?
084200140730         wCampo += 1;
084300140730         wTipo   = 'A';        //?- alfanumerico?
084400140730         wLungh  = %len( WMPsess7 );
084500140730         exsr  sr_GetFldVal;
084600140730         if  wErr > *zero;
084700140730           leavesr;
084800140730         endif;
084900140730         WMPsess7   = wValoreA;
085000140730         // -? 65° campo: Carica Esp 7            ?
085100140730         wCampo += 1;
085200140730         wTipo   = 'A';        //?- alfanumerico?
085300140730         wLungh  = %len( WMPcari7 );
085400140730         exsr  sr_GetFldVal;
085500140730         if  wErr > *zero;
085600140730           leavesr;
085700140730         endif;
085800140730         WMPcari7   = wValoreA;
085900140730         // -? 66° campo: Denominazione Esp 8     ?
086000140730         wCampo += 1;
086100140730         wTipo   = 'A';        //?- alfanumerico?
086200140730         wLungh  = %len( WMPdeno8 );
086300140730         exsr  sr_GetFldVal;
086400140730         if  wErr > *zero;
086500140730           leavesr;
086600140730         endif;
086700140730         WMPdeno8   = wValoreA;
086800140730         // -? 67° campo: Cognome Esp 8           ?
086900140730         wCampo += 1;
087000140730         wTipo   = 'A';        //?- alfanumerico?
087100140730         wLungh  = %len( WMPcogn8 );
087200140730         exsr  sr_GetFldVal;
087300140730         if  wErr > *zero;
087400140730           leavesr;
087500140730         endif;
087600140730         WMPcogn8   = wValoreA;
087700140730         // -? 68° campo: Nome Esp 8              ?
087800140730         wCampo += 1;
087900140730         wTipo   = 'A';        //?- alfanumerico?
088000140730         wLungh  = %len( WMPnome8 );
088100140730         exsr  sr_GetFldVal;
088200140730         if  wErr > *zero;
088300140730           leavesr;
088400140730         endif;
088500140730         WMPnome8   = wValoreA;
088600140730         // -? 69° campo: Sesso Esp 8             ?
088700140730         wCampo += 1;
088800140730         wTipo   = 'A';        //?- alfanumerico?
088900140730         wLungh  = %len( WMPsess8 );
089000140730         exsr  sr_GetFldVal;
089100140730         if  wErr > *zero;
089200140730           leavesr;
089300140730         endif;
089400140730         WMPsess8   = wValoreA;
089500140730         // -? 70° campo: Carica Esp 8            ?
089600140730         wCampo += 1;
089700140730         wTipo   = 'A';        //?- alfanumerico?
089800140730         wLungh  = %len( WMPcari8 );
089900140730         exsr  sr_GetFldVal;
090000140730         if  wErr > *zero;
090100140730           leavesr;
090200140730         endif;
090300140730         WMPcari8   = wValoreA;
090400140730         // -? 71° campo: Fatturato Euro          ?
090500140730         wCampo += 1;
090600140730         wTipo   = 'n';        //?- numerico con segno?
090700140730         wLungh  = %len( WMPfatt );
090800140730         exsr  sr_GetFldVal;
090900140730         if  wErr > *zero;
091000140730           leavesr;
091100140730         endif;
091200140730         WMPfatt    = wValoreN;
091300140730         // -? 72° campo: Anno Fatturato          ?
091400140730         wCampo += 1;
091500140730         wTipo   = 'N';        //?- numerico?
091600140730         wLungh  = %len( WMPaaFt );
091700140730         exsr  sr_GetFldVal;
091800140730         if  wErr > *zero;
091900140730           leavesr;
092000140730         endif;
092100140730         WMPaaFt    = wValoreN;
092200140730         // -? 73° campo: Provenienza Fatturato   ?
092300140730         wCampo += 1;
092400140730         wTipo   = 'A';        //?- alfanumerico?
092500140730         wLungh  = %len( WMPpvnFt );
092600140730         exsr  sr_GetFldVal;
092700140730         if  wErr > *zero;
092800140730           leavesr;
092900140730         endif;
093000140730         WMPpvnFt   = wValoreA;
093100140730         // -? 74° campo: Utile/Perdita           ?
093200140730         wCampo += 1;
093300140730         wTipo   = 'n';        //?- numerico con segno?
093400140730         wLungh  = %len( WMPutile );
093500140730         exsr  sr_GetFldVal;
093600140730         if  wErr > *zero;
093700140730           leavesr;
093800140730         endif;
093900140730         WMPutile   = wValoreN;
094000140730         // -? 75° campo: % import                ?
094100140730         wCampo += 1;
094200140730         wTipo   = 'n';        //?- numerico con "%"?
094300140730         wLungh  = %len( WMPpImp );
094400140730         exsr  sr_GetFldVal;
094500140730         if  wErr > *zero;
094600140730           leavesr;
094700140730         endif;
094800140730         if  wValoreN > 999.99;
094900140730           wErr = 6;
095000140730           exsr  sr_PrintErr;
095100140730           leavesr;
095200140730         endif;
095300140730         WMPpImp    = wValoreN;
095400140730         // -? 76° campo: % export                ?
095500140730         wCampo += 1;
095600140730         wTipo   = 'n';        //?- numerico con "%"?
095700140730         wLungh  = %len( WMPpExp );
095800140730         exsr  sr_GetFldVal;
095900140730         if  wErr > *zero;
096000140730           leavesr;
096100140730         endif;
096200140730         if  wValoreN > 999.99;
096300140730           wErr = 6;
096400140730           exsr  sr_PrintErr;
096500140730           leavesr;
096600140730         endif;
096700140730         WMPpExp    = wValoreN;
096800140730         // -? 77° campo: Crif Number Casa Madre  ?
096900140730         wCampo += 1;
097000140730         wTipo   = 'A';        //?- numerico?
097100140730         wLungh  = %len( WMPcmCrif );
097200140730         exsr  sr_GetFldVal;
097300140730         if  wErr > *zero;
097400140730           leavesr;
097500140730         endif;
097600140730         WMPcmCrif  = wValoreA;
097700140730         // -? 78° campo: Nome casa madre         ?
097800140730         wCampo += 1;
097900140730         wTipo   = 'A';        //?- alfanumerico?
098000140730         wLungh  = %len( WMPcmNom );
098100140730         exsr  sr_GetFldVal;
098200140730         if  wErr > *zero;
098300140730           leavesr;
098400140730         endif;
098500140730         WMPcmNom   = wValoreA;
098600140730         // -? 79° campo: Indirizzo casa madre    ?
098700140730         wCampo += 1;
098800140730         wTipo   = 'A';        //?- alfanumerico?
098900140730         wLungh  = %len( WMPcmInd );
099000140730         exsr  sr_GetFldVal;
099100140730         if  wErr > *zero;
099200140730           leavesr;
099300140730         endif;
099400140730         WMPcmInd   = wValoreA;
099500140730         // -? 80° campo: Cap casa madre          ?
099600140730         wCampo += 1;
099700140730         wTipo   = 'A';        //?- alfanumerico?
099800140730         wLungh  = %len( WMPcmCap );
099900140730         exsr  sr_GetFldVal;
100000140730         if  wErr > *zero;
100100140730           leavesr;
100200140730         endif;
100300140730         WMPcmCap   = wValoreA;
100400140730         // -? 81° campo: Città casa madre        ?
100500140730         wCampo += 1;
100600140730         wTipo   = 'A';        //?- alfanumerico?
100700140730         wLungh  = %len( WMPcmLoc );
100800140730         exsr  sr_GetFldVal;
100900140730         if  wErr > *zero;
101000140730           leavesr;
101100140730         endif;
101200140730         WMPcmLoc   = wValoreA;
101300140730         // -? 82° campo: Provincia casa madre    ?
101400140730         wCampo += 1;
101500140730         wTipo   = 'A';        //?- alfanumerico?
101600140730         wLungh  = %len( WMPcmPrv );
101700140730         exsr  sr_GetFldVal;
101800140730         if  wErr > *zero;
101900140730           leavesr;
102000140730         endif;
102100140730         WMPcmPrv   = wValoreA;
102200140730         // -? 83° campo: Nazione casa madre      ?
102300140730         wCampo += 1;
102400140730         wTipo   = 'A';        //?- alfanumerico?
102500140730         wLungh  = %len( WMPcmNaz );
102600140730         exsr  sr_GetFldVal;
102700140730         if  wErr > *zero;
102800140730           leavesr;
102900140730         endif;
103000140730         WMPcmNaz   = wValoreA;
103100140730         // -? 84° campo: FlagRating4             ?
103200140730         wCampo += 1;
103300140730         wTipo   = 'A';        //?- alfanumerico?
103400140730         wLungh  = %len( WMPfr4 );
103500140730         exsr  sr_GetFldVal;
103600140730         if  wErr > *zero;
103700140730           leavesr;
103800140730         endif;
103900140730         WMPfr4     = wValoreA;
104000140730         // -? 85° campo: % impex                 ?
104100140730         wCampo += 1;
104200140730         wTipo   = 'n';        //?- numerico con "%"?
104300140730         wLungh  = %len( WMPpImpEx );
104400140730         exsr  sr_GetFldVal;
104500140730         if  wErr > *zero;
104600140730           leavesr;
104700140730         endif;
104800140730         if  wValoreN > 999.99;
104900140730           wErr = 6;
105000140730           exsr  sr_PrintErr;
105100140730           leavesr;
105200140730         endif;
105300140730         WMPpImpEx  = wValoreN;
105400140730         // -? 86° campo: anno %impex             ?
105500140730         wCampo += 1;
105600140730         wTipo   = 'N';        //?- numerico?
105700140730         wLungh  = %len( WMPaImpEx );
105800140730         exsr  sr_GetFldVal;
105900140730         if  wErr > *zero;
106000140730           leavesr;
106100140730         endif;
106200140730         WMPaImpEx  = wValoreN;
106300140730
106400140730
106500140730         // -?Scrittura record in WFMDP00F?
106600140730         //______________
106700140730         WRITE  WFMDP000;
106800140730         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
106900140730
107000140730       ENDSR;
107100140730
107200140730       //--------------------------------------------------------------
107300140730       //?Reperimento valore del campo in elaborazione.                ?
107400140730       //--------------------------------------------------------------
107500140730       BEGSR  sr_GetFldVal;
107600140730
107700140730         // -?Calcolo Posizioni Iniziale e Finale?
107800140730         wPosI = wPosF + 1;
107900140731         wPosF = %scan( c_SepChar : xProsp_Dati : wPosI );
108000140730
108100140731         //// -?Verifica esistenza punteggiatura NON separatrice ('; '/'] '')?
108200140731         ////  ?SCARTANDO il controllo dal PENULTIMO campo del file?
108300140731         //If  wPosF > *zero  and  wCampo < ( c_MaxFldWF - 1 );
108400140731         //  DoW  %subst( xProsp_Dati : wPosF + 1 : 1 ) = *blank;
108500140731         //    wPosF = %scan( c_SepChar : xProsp_Dati : wPosF + 1 );
108600140731         //  EndDo;
108700140731         //EndIf;
108800140730
108900140730         // -?Impostazione finale se ultimo campo?
109000140730         if  wPosF = *zero;
109100140730           wPosF = %len( %trimr( xProsp_Dati ) ) + 1;
109200140730         endif;
109300140730
109400140730         // -?Estrazione dati da "convertire"?
109500140730         clear  wErr;
109600140730         clear  wValoreA;
109700140730         clear  wValoreN;
109800140730         clear  $Negativo;
109900140730         //clear  $Perc;
110000140730         // -?...SE reperiti dati?
110100140730         if  wPosF > wPosI;
110200140730           wValoreA = %trimL( %subst( xProsp_Dati : wPosI : wPosF - wPosI ) );
110300140730         endif;
110400140730
110500140730         Select;
110600140730
110700140730           // -?Campo vuoto?
110800140730           When  wValoreA = *blank;
110900140730             leavesr;
111000140730
111100140730           // -?Caratteri alfanumerici da convertire in Maiuscolo?
111200140730           When  wTipo = 'a';
111300140730             wValoreA = %xlate( c_Minu : c_Maiu : wValoreA );
111400140730
111500140730           // -?Caratteri numerici con segno e/o con imbolo "%"?
111600140730           When  wTipo = 'n';
111700140730             if  %scan( '-' : wValoreA ) > *zero;
111800140730               $Negativo = *on;
111900140730               %subst( wValoreA : %scan( '-' : wValoreA ) : 1 ) = ' ';
112000140730               wValoreA  = %trim( wValoreA );
112100140730             endif;
112200140730             //if  %scan( '%' : wValoreA ) > *zero;
112300140730             //  $Perc    = *on;
112400140730             //  %subst( wValoreA : %scan( '%' : wValoreA ) : 1 ) = ' ';
112500140730             //  wValoreA = %trim( wValoreA );
112600140730             //endif;
112700140730
112800140730           // -?Caratteri numerici per data (con separatore "/")?
112900140730           When  wTipo = 'D';
113000140730             wPosSep = %scan( '/' : wValoreA );
113100140730             doW  wPosSep > *zero;
113200140730               wValoreA = %subst( wValoreA : 1 : wPosSep - 1 ) +
113300140730                          %subst( wValoreA : wPosSep + 1 );
113400140730               wPosSep = %scan( '/' : wValoreA );
113500140730             enddo;
113600140730
113700140730         EndSl;
113800140730
113900140730         Select;
114000140730
114100140730           // -?Caratteri alfanumerici di cui controllare la lunghezza?
114200140730           When  wTipo = 'A';
114300140730             exsr  sr_CtrlAlfa;
114400140730
114500140730           // -?Caratteri alfanumerici da convertire in Numerici e?
114600140730           //  ?di cui controllare i decimali?
114700140730           When  wTipo = 'N'  or  wTipo = 'n'  or  wTipo = 'D';
114800140730             exsr  sr_CtrlNum;
114900140730
115000140730         EndSl;
115100140730
115200140730       ENDSR;
115300140730
115400140730       //--------------------------------------------------------------
115500140730       //?Controlli valori per campo alfanumerico.                     ?
115600140730       //--------------------------------------------------------------
115700140730       BEGSR  sr_CtrlAlfa;
115800140730
115900140730         // -?Trovati troppi caratteri?
116000140730         if  %len( %trimR( wValoreA ) ) > wLungh;
116100140730           wLungh = %len( %trimR( wValoreA ) );
116200140730           wErr = 3;
116300140730           exsr  sr_PrintErr;
116400140730           leavesr;
116500140730         endif;
116600140730
116700140730       ENDSR;
116800140730
116900140730       //--------------------------------------------------------------
117000140730       //?Controlli valori per campo numerico.                         ?
117100140730       //--------------------------------------------------------------
117200140730       BEGSR  sr_CtrlNum;
117300140730
117400140730         // -?Ricerca virgola (",") e Conteggio decimali (max 2)?
117500140730         wPosVirg = %scan( ',' : wValoreA );
117600140730
117700140730         // -?Campo numerico intero?
117800140730         If  wPosVirg = *zero;
117900140730
118000140730           // -?Trovati caratteri NON numerici?
118100140730           if  %check( c_Digits : %trimR( wValoreA ) ) > *zero;
118200140730             wErr = 4;
118300140730             exsr  sr_PrintErr;
118400140730             leavesr;
118500140730           endif;
118600140730           // -?Valorizzazione dato numerico?
118700140730           wValoreN = %int( %trimR( wValoreA ) );
118800140730
118900140730         // -?Campo numerico con decimali?
119000140730         Else;
119100140730
119200140730           For  wNumDec = 1  To  3;
119300140730             if  %subst( wValoreA : wPosVirg + wNumDec ) = *blank;
119400140730               leave;
119500140730             endif;
119600140730           EndFor;
119700140730           wNumDec -= 1;
119800140730
119900140730           Select;
120000140730
120100140730             When  wNumDec = *zero;
120200140730               // -?Trovati caratteri NON numerici?
120300140730               if  %check( c_Digits : %subst( wValoreA : 1 :
120400140730                                              wPosVirg - 1 ) ) > *zero;
120500140730                 wErr = 4;
120600140730                 exsr  sr_PrintErr;
120700140730                 leavesr;
120800140730               endif;
120900140730               // -?Valorizzazione dato numerico?
121000140730               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) );
121100140730
121200140730             When  wNumDec = 1;
121300140730               // -?Trovati caratteri NON numerici?
121400140730               if  %check( c_Digits : %subst( wValoreA : 1 :
121500140730                                              wPosVirg - 1 ) ) > *zero
121600140730                   or
121700140730                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
121800140730                                              1 ) ) > *zero;
121900140730                 wErr = 4;
122000140730                 exsr  sr_PrintErr;
122100140730                 leavesr;
122200140730               endif;
122300140730               // -?Valorizzazione dato numerico?
122400140730               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
122500140730                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
122600140730                                          1 ) ) / 10 );
122700140730
122800140730             //When  wNumDec = 2;
122900140730             Other;
123000140730               // -?Trovati caratteri NON numerici?
123100140730               if  %check( c_Digits : %subst( wValoreA : 1 :
123200140730                                              wPosVirg - 1 ) ) > *zero
123300140730                   or
123400140730                   %check( c_Digits : %subst( wValoreA : wPosVirg + 1 :
123500140730                                              2 ) ) > *zero;
123600140730                 wErr = 4;
123700140730                 exsr  sr_PrintErr;
123800140730                 leavesr;
123900140730               endif;
124000140730               // -?Valorizzazione dato numerico?
124100140730               wValoreN = %int( %subst( wValoreA : 1 : wPosVirg - 1 ) )
124200140730                        + ( %int( %subst( wValoreA : wPosVirg + 1 : +
124300140730                                          2 ) ) / 100 );
124400140730             //Other;
124500140730             //  wErr = 5;
124600140730             //  exsr  sr_PrintErr;
124700140730             //  leavesr;
124800140730
124900140730           EndSl;
125000140730
125100140730         EndIf;
125200140730
125300140730         // -?Campo numerico troppo lungo?
125400140730         if  ( wPosVirg > *zero  and
125500140730               %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > wLungh )  OR
125600140730             ( wPosVirg = *zero  and
125700140730               %len( %trim( wValoreA ) ) > wLungh );
125800140730             //( wLungh   = 3      and (
125900140730             //  wPosVirg > *zero  and
126000140730             //  %len( %subst( wValoreA : 1 : wPosVirg - 1 ) ) > 3         or
126100140730             //  wPosVirg = *zero  and
126200140730             //  %len( %trim( wValoreA ) ) > 3 ) );
126300140730           wErr = 6;
126400140730           exsr  sr_PrintErr;
126500140730           leavesr;
126600140730         endif;
126700140730
126800140730         // -?Campo negativo?
126900140730         if  $Negativo;
127000140730           wValoreN *= (-1);
127100140730         endif;
127200140730
127300140730         //// -?Percentuale?
127400140730         //if  $Perc;
127500140730         //  ...;
127600140730         //endif;
127700140730
127800140730       ENDSR;
127900140730
128000140730       //--------------------------------------------------------------
128100140730       //?Stampa segnalazione dell'errore rilevato.                    ?
128200140730       //--------------------------------------------------------------
128300140730       BEGSR  sr_PrintErr;
128400140730
128500140730         // -?Apertura del printer-file?
128600140730         if  Not %open(QSYSPRT);
128700140730           Qcmd = 'OVRPRTF file(QSYSPRT) +
128800140730                           usrdta(''ACPO02*ERR'') +
128900140730                           ovrscope(*actgrpdfn)';
129000140730           exsr  sr_ExecCmd;
129100140730           open  QSYSPRT;
129200140730           except  PRTtxt;
129300140730         endif;
129400140730
129500140730         if  *inOF;
129600140730           except  PRTtxt;
129700140730           *inOF = *off;
129800140730         endif;
129900140730
130000140730         Select;
130100140730           When  wErr = 1;
130200140730             except  PrtErr1;
130300140730           When  wErr = 2;
130400140730             except  PrtErr2;
130500140730           When  wErr = 3;
130600140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
130700140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
130800140730             except  PrtErr3;
130900140730           When  wErr = 4;
131000140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
131100140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
131200140730             except  PrtErr4;
131300140730           When  wErr = 5;
131400140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
131500140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
131600140730             except  PrtErr5;
131700140730           When  wErr = 6;
131800140730             wValoreA1 = %subst( wValoreA : 1 : %len(wValoreA1) );
131900140730             wValoreA2 = %subst( wValoreA : %len(wValoreA1) + 1 );
132000140730             except  PrtErr6;
132100140730         EndSl;
132200140730
132300140730       ENDSR;
132400140730
132500140730       //--------------------------------------------------------------
132600140730       //?Operazioni finali.                                           ?
132700140730       //--------------------------------------------------------------
132800140730       BEGSR  sr_RoutEnd;
132900140730
133000140730         // -?Chiusura spool di stampa errori?
133100140730         if  %open(QSYSPRT);
133200140730           except  PrtEND;
133300140730           close  QSYSPRT;
133400140730         endif;
133500140730
133600140730         // -?Chiusura Work-File di output?
133700140730         if  %open(PROSP_DATI);
133800140730           close PROSP_DATI;
133900140730         endif;
134000140730
134100140730         // -?Rimozione overridea al file di CRIBIS (PROSP_DATI)?
134200140730         Qcmd = 'DLTOVR file(PROSP_DATI) +
134300140730                        lvl(*actgrpdfn)';
134400140730         exsr  sr_ExecCmd;
134500140730
134600140730         // -?Chiusura *pgm?
134700140730         return;
134800140730
134900140730       ENDSR;
135000140730
135100140730       //--------------------------------------------------------------
135200140730       //?Esecuzione del comando (già impostato).                      ?
135300140730       //--------------------------------------------------------------
135400140730       BEGSR  sr_ExecCmd;
135500140730
135600140730         clear Qcap0100;
135700140730         Qcabcsdh = *off;
135800140730         Qcapa    = *off;
135900140730         Qcacmdss = *off;
136000140730         Qcaerved = *allX'00';
136100140730
136200140730         clear Qusec;
136300140730         Qusbprv  = %size(Qusec);
136400140730
136500140730         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
136600140730                           %size(Qcap0100) : 'CPOP0100' : *omit :
136700140730                           0 : 0 : Qusec);
136800140730
136900140730         //if  Qusei <> *blank;
137000140730         //  ...;
137100140730         //endif;
137200140730
137300140730       ENDSR;
137400140730
137500140730      /end-free
137600140730
137700140730       //--------------------------------------------------------------
137800140730       //?S P O O L - F I L E S                                        ?
137900140730       //--------------------------------------------------------------
138000140730
138100140730     oQSYSPRT   e            PRTtxt            1
138200140730     o                       RSUT
138300140730     o                                        +   4 'LISTA ERRORI RILE-
138400140730     o                                              VATI IN FASE DI EL-
138500140730     o                                              ABORAZIONE DATI NE-
138600140730     o                                              L FILE'
138700140730     o                       wLibFile         +   1
138800140730     o                       SDSpgmName       +   4
138900140730     o                       *Date         y  +   2
139000140730     o          e            PRTtxt      0
139100140730     o                                          +24 'LISTA ERRORI RILE-
139200140730     o                                              VATI IN FASE DI EL-
139300140730     o                                              ABORAZIONE DATI NE-
139400140730     o                                              L FILE'
139500140730     o                       wLibFile         +   1
139600140730     o          e            PRTtxt      1
139700140730     o                       KNSIF
139800140730     o                       KNMUS            +   1
139900140730     o                                        +   3 '------------------
140000140730     o                                              -------------------
140100140730     o                                              -------------------
140200140730     o                                              -------------------
140300140730     o                                              ----------'
140400140730     o                                        +   4 'Pag.'
140500140730     o                       Page          z  +   0
140600140730     o                       wTime            +   4 '  :  :  '
140700140730      *
140800140730     o          e            PRTtxt      2
140900140730     o                                              'CRIF Number    '
141000140730     o                                        +   2 'Messaggio'
141100140730     o          e            PRTtxt      1
141200140730     o                                              '---------------'
141300140730     o                                        +   2 '---------'
141400140730      *
141500140730     o          e            PRTerr1     2
141600140730     o                                              'NON RIUSCITA LA +
141700140730     o                                               PULIZIA DEL WORK-+
141800140730     o                                               FILE WFMDP00F'
141900140730      *
142000140730     o          e            PRTerr2     2
142100140730     o                                              'NON TROVATO IL +
142200140730     o                                               FILE'
142300140730     o                       wLibWF           +   1
142400140730      *
142500140730     o          e            PRTerr3     2
142600140730     o                       wCrifNr
142700140730     o                                        +   2 'Troppi caratteri ('
142800140730     o                       wLungh        z  +   0
142900140730     o                                        +   0 ') nel'
143000140730     o                       wCampo        z  +   1
143100140730     o                                        +   0 '° campo:'
143200140730     o                       $Campi(wCampo)   +   1
143300140730     o          e            PRTerr3     1
143400140730     o                       wValoreA1        +   5
143500140730     o          e            PRTerr3     1
143600140730     o                       wValoreA2        +   5
143700140730      *
143800140730     o          e            PRTerr4     2
143900140730     o                       wCrifNr
144000140730     o                                        +   2 'Dati NON numerici +
144100140730     o                                               nel'
144200140730     o                       wCampo        z  +   1
144300140730     o                                        +   0 '° campo:'
144400140730     o                       $Campi(wCampo)   +   1
144500140730     o          e            PRTerr4     1
144600140730     o                       wValoreA1        +   5
144700140730     o          e            PRTerr4     1
144800140730     o                       wValoreA2        +   5
144900140730      *
145000140730     o          e            PRTerr5     2
145100140730     o                       wCrifNr
145200140730     o                                        +   2 'Numero decimali ('
145300140730     o                       wNumDec       z  +   0
145400140730     o                                        +   0 ') errato nel'
145500140730     o                       wCampo        z  +   1
145600140730     o                                        +   0 '° campo:'
145700140730     o                       $Campi(wCampo)   +   1
145800140730     o          e            PRTerr5     1
145900140730     o                       wValoreA1        +   5
146000140730     o          e            PRTerr5     1
146100140730     o                       wValoreA2        +   5
146200140730      *
146300140730     o          e            PRTerr6     2
146400140730     o                       wCrifNr
146500140730     o                                        +   2 'Troppe cifre ('
146600140730     o                       wLungh        z  +   0
146700140730     o                                        +   0 ') nel'
146800140730     o                       wCampo        z  +   1
146900140730     o                                        +   0 '° campo:'
147000140730     o                       $Campi(wCampo)   +   1
147100140730     o          e            PRTerr6     1
147200140730     o                       wValoreA1        +   5
147300140730     o          e            PRTerr6     1
147400140730     o                       wValoreA2        +   5
147500140730      *
147600140730     o          e            PRTend      2
147700140730     o                                        +  24 '***  Fine Lista  ***'
147800140730     o          e            PRTend      0
147900140730     o                                        +  24 '***  Fine Lista  ***'
148000140730
148100140730       //--------------------------------------------------------------
148200140730       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
148300140730       //--------------------------------------------------------------
148400140730
148500140730** --?$Campi -----------*?Nomi di campi da indicare in stampa?
148600140730Crif Number                 1
148700140730OOB Indicator               2
148800140730Denominazione               3
148900140730Indirizzo                   4
149000140730Comune                      5
149100140730Cap                         6
149200140730Regione                     7
149300140730Frazione                    8
149400140730Provincia                   9
149500140730Provincia estesa           10
149600140730Codice Fiscale             11
149700140730Partita Iva                12
149800140730Telefono 1                 13
149900140730Fax 1                      14
150000140730Sito Internet 1            15
150100140730Flag E-commerce            16
150200140730Capitale sociale Euro      17
150300140730Anno Costituzione          18
150400140730Anno Fondazione            19
150500140730Data inizio attività       20
150600140730Ateco (2007)  1            21
150700140730Descrizione codice ateco   22
150800140730Sic primario               23
150900140730Descrizione Sic            24
151000140730Dipendenti                 25
151100140730Natura giuridica           26
151200140730CCIAA                      27
151300140730REA                        28
151400140730Oggetto sociale            29
151500140730Stato Impresa Cribis       30
151600140730Denominazione Esp 1        31
151700140730Cognome Esp 1              32
151800140730Nome Esp 1                 33
151900140730Sesso Esp 1                34
152000140730Carica Esp 1               35
152100140730Denominazione Esp 2        36
152200140730Cognome Esp 2              37
152300140730Nome Esp 2                 38
152400140730Sesso Esp 2                39
152500140730Carica Esp 2               40
152600140730Denominazione Esp 3        41
152700140730Cognome Esp 3              42
152800140730Nome Esp 3                 43
152900140730Sesso Esp 3                44
153000140730Carica Esp 3               45
153100140730Denominazione Esp 4        46
153200140730Cognome Esp 4              47
153300140730Nome Esp 4                 48
153400140730Sesso Esp 4                49
153500140730Carica Esp 4               50
153600140730Denominazione Esp 5        51
153700140730Cognome Esp 5              52
153800140730Nome Esp 5                 53
153900140730Sesso Esp 5                54
154000140730Carica Esp 5               55
154100140730Denominazione Esp 6        56
154200140730Cognome Esp 6              57
154300140730Nome Esp 6                 58
154400140730Sesso Esp 6                59
154500140730Carica Esp 6               60
154600140730Denominazione Esp 7        61
154700140730Cognome Esp 7              62
154800140730Nome Esp 7                 63
154900140730Sesso Esp 7                64
155000140730Carica Esp 7               65
155100140730Denominazione Esp 8        66
155200140730Cognome Esp 8              67
155300140730Nome Esp 8                 68
155400140730Sesso Esp 8                69
155500140730Carica Esp 8               70
155600140730Fatturato Euro             71
155700140730Anno Fatturato             72
155800140730Provenienza Fatturato      73
155900140730Utile/Perdita              74
156000140730% import                   75
156100140730% export                   76
156200140730Crif Number casa madre     77
156300140730Nome casa madre            78
156400140730Indirizzo casa madre       79
156500140730Cap casa madre             80
156600140730Città casa madre           81
156700140730Provincia casa madre       82
156800140730Nazione casa madre         83
156900140730FlagRating4                84
157000140730% impex                    85
157100140730anno %impex                86
