000100130702       //==============================================================
000200130708       //?TRMK41R * Immissione/Manutenzione Commerciale                ?
000300130702       //==============================================================
000400130702
000500130702       //--------------------------------------------------------------
000600130702       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130702       //--------------------------------------------------------------
000800130702
000900130702     /*PRM dbgview(*source)
001000130702     /*END
001100130702
001200130702       //--------------------------------------------------------------
001300130702       //?Specifiche di controllo.                                     ?
001400130702       //--------------------------------------------------------------
001500130702
001600130702     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130702     h dftactgrp(*no)
001800130705     h bnddir('TIBS':'TRUL')
001900130702
002000130702       //--------------------------------------------------------------
002100130702       //?Dichiarazione file.                                          ?
002200130702       //--------------------------------------------------------------
002300130702
002400130702       // -?Organigramma Filiale/Aziendale?
002500130702     fAZORG01L  if   e           k disk
002600130702       // -?Anagrafica Dipendenti?
002700130702     fAZDAN01L  if   e           k disk
002800130702
002900130708       // -?Anagrafica Commerciali?
003000130708     fAZCMM01L  Uf A e           k disk
003100130708       // -?Note/Rubrica Commerciali?
003200130709     fAZNTC01L  Uf A e           k disk
003300130702
003400130702       // -?Video Gestione?
003500130702     fTRMK41D   cf   e             workstn
003600130702     f                                     indds(IndDspF)
003700130702     f                                     infds(InfDspF)
003800130702
003900130702       //--------------------------------------------------------------
004000130702       //?Definizione costanti.                                        ?
004100130702       //--------------------------------------------------------------
004200130702
004300130702       // -?@brt.it?
004400130702     d c_atBrt         c                   const('@brt.it')
004500130702
004600130702       // -?Data 31/12/2039 (formato "EUR")?
004700130702     d c_MaxDate_Eur   c                   const(31122039)
004800130702
004900130702       // -?Costante per controllo "caratteri solo numerici"?
005000130702     d c_Digits        c                   const('0123456789')
005100130702
005200130702       // -?Tasti funzionali a video?
005300130702     d c_F01           c                   const(x'31')
005400130702     d c_F02           c                   const(x'32')
005500130702     d c_F03           c                   const(x'33')
005600130702     d c_F04           c                   const(x'34')
005700130702     d c_F05           c                   const(x'35')
005800130702     d c_F06           c                   const(x'36')
005900130702     d c_F07           c                   const(x'37')
006000130702     d c_F08           c                   const(x'38')
006100130702     d c_F09           c                   const(x'39')
006200130702     d c_F10           c                   const(x'3A')
006300130702     d c_F11           c                   const(x'3B')
006400130702     d c_F12           c                   const(x'3C')
006500130702     d c_F13           c                   const(x'B1')
006600130702     d c_F14           c                   const(x'B2')
006700130702     d c_F15           c                   const(x'B3')
006800130702     d c_F16           c                   const(x'B4')
006900130702     d c_F17           c                   const(x'B5')
007000130702     d c_F18           c                   const(x'B6')
007100130702     d c_F19           c                   const(x'B7')
007200130702     d c_F20           c                   const(x'B8')
007300130702     d c_F21           c                   const(x'B9')
007400130702     d c_F22           c                   const(x'BA')
007500130702     d c_F23           c                   const(x'BB')
007600130702     d c_F24           c                   const(x'BC')
007700130702     d c_Enter         c                   const(x'F1')
007800130702     d c_RollDown      c                   const(x'F4')
007900130702     d c_RollUp        c                   const(x'F5')
008000130702
008100130702       //--------------------------------------------------------------
008200130702       //?Definizione schiere.                                         ?
008300130702       //--------------------------------------------------------------
008400130702
008500130702       // -?Opzioni e relative decodifiche?
008600130702     d $Opz            s                   like(iMK41opz)
008700130702     d                                     dim(6)
008800130702     d                                     ctdata   perrcd(1)
008900130702     d $OpzD           s                   like(V1Topz)
009000130702     d                                     dim(6)
009100130702     d                                     alt($Opz)
009200130702
009300130702       // -?Messaggi di errore?
009400130712     d $Msg            s             78    dim(33) ctdata perrcd(1)
009500130702
009600141212       // -?Dati dei Commerciali?
009700150108     d c_Max_Cmm_W     c                   const(9000)
009800141212     d c_Max_Cmm       c                   const(9999)
009900130708     d $Cmm            s                   like(CMMcod) dim(c_Max_Cmm)
010000130702     d                                     inz
010100130708     d $CmmDes         s                   like(CMMdes) dim(c_Max_Cmm)
010200130702     d                                     inz
010300130708     d $CmmUni         s                   like(CMMuni) dim(c_Max_Cmm)
010400130702     d                                     inz
010500130702
010600130702       // -?Filiali con commerciale unificante?
010700141212     d c_Max_Cmm_F     c                   const(999)
010800141212     d $Cmu_Fil        s                   like(CMMfil) dim(c_Max_Cmm_F)
010900130702     d                                     inz
011000130704       // -?1° comm.le di filiale x comm.le unificante (COD+DES)?
011100141212     d $Cmu_Cmm        s        +     8    like(CMMdes) dim(c_Max_Cmm_F)
011200130702     d                                     inz
011300130702
011400130702       //--------------------------------------------------------------
011500130702       //?Definizione aree dati.                                       ?
011600130702       //--------------------------------------------------------------
011700130702
011800130702       // -?Dati utente?
011900130702     d §AzUte        e ds                  extname(AZUTE00F)
012000130702     d                                     dtaara
012100130702     d §DatiUte      e ds                  extname(dDatiUte)
012200130702     d                                     dtaara
012300130702
012400130702       //--------------------------------------------------------------
012500130702       //?Definizione strutture dati.                                  ?
012600130702       //--------------------------------------------------------------
012700130702
012800130702       // -?Status ds?
012900130702     d Status         sds
013000130702     d   SDSpgm          *proc
013100141212     d   SDSusr              254    263
013200130702
013300130702       // -?InfDS?
013400130702     d InfDspF         ds
013500130702     d   dsp_aid             369    369a
013600130702
013700130702       // -?Indicatori su DspF?
013800130702     d IndDspF         ds                  inz
013900130702         // -?Abilitazione tasti funzionali?
014000130702     d   F3Attivo                      n   overlay(IndDspF : 03)
014100130702     d*//F5Attivo                      n   overlay(IndDspF : 05)
014200130702     d   F6Attivo                      n   overlay(IndDspF : 06)
014300130702     d   F12Attivo                     n   overlay(IndDspF : 12)
014400130702     d*//F16Attivo                     n   overlay(IndDspF : 16)
014500130702         // -?Emissione messaggio di errore?
014600130702     d   ErrMessage                    n   overlay(IndDspF : 28)
014700130710         // -?Indicatori per Attributi di visualizzazione?
014800130702     d   ProtectKey                    n   overlay(IndDspF : 40)
014900130702     d   ProtectFld                    n   overlay(IndDspF : 41)
015000130705     d   ProtectDtIni                  n   overlay(IndDspF : 42)
015100130704     d   VisualFil                     n   overlay(IndDspF : 43)
015200130702         // -?Posizionamento cursore & Segnalazione errore?
015300130708     d   PosCurCOD                     n   overlay(IndDspF : 50)
015400130708     d   PosCurDES                     n   overlay(IndDspF : 51)
015500130702     d   PosCurUNI                     n   overlay(IndDspF : 52)
015600130702     d   PosCurPAR                     n   overlay(IndDspF : 53)
015700130702     d   PosCurFUN                     n   overlay(IndDspF : 54)
015800130702     d   PosCurPLI                     n   overlay(IndDspF : 55)
015900130702     d   PosCurPLE                     n   overlay(IndDspF : 56)
016000130702     d   PosCurDTINI                   n   overlay(IndDspF : 57)
016100130702     d   PosCurDTFIN                   n   overlay(IndDspF : 58)
016200130702     d   PosCurDIP                     n   overlay(IndDspF : 59)
016300130702     d   PosCurSOC                     n   overlay(IndDspF : 60)
016400130702     d   PosCurFIL                     n   overlay(IndDspF : 61)
016500130705     d   PosCurRNT01                   n   overlay(IndDspF : 62)
016600130705     d   PosCurRNT02                   n   overlay(IndDspF : 63)
016700130705     d   PosCurFILw1                   n   overlay(IndDspF : 71)
016800130702         // -?Riemissione videata?
016900130702     d   ErrGenerico                   n   overlay(IndDspF : 99)
017000130704
017100130704       // -?Date decorrenza e fine attività?
017200130704     d ds_DtAtt        ds                  inz
017300130704     d   V1DtIni                           inz
017400130704     d   V1DtFin                           inz
017500130702
017600130702       // -?Codice dipendente del commerciale e relativa società?
017700130702     d ds_DIP          ds                  inz
017800130702     d   V1Cdip                            inz
017900130702     d   V1Csoc                            inz
018000130702
018100130702       // -?Descrizione commerciale e relativo indirizzo e-mail?
018200130709     d ds_NTC          ds                  inz
018300130705     d   V1Crnt01                          inz
018400130705     d   V1Crnt02                          inz
018500141217
018600141217       // -?Parametri ricevuti?
018700141217     d KPJBA         e ds
018800141217     d prm_AZCMM_ds  e ds                  extname(AZCMM00F)
018900141217     d TRMK41ds      e ds                  inz
019000141217
019100141217       // -?Tabelle usate:?
019200141217       // ·?"dFUN" = Funzione aziendale?
019300141217     d dFUN          e ds                  inz
019400141217
019500141217       // -?Descrizioni dell'organigramma:?
019600141217     d Og143         e ds                  inz  qualified
019700130702
019800130702       //--------------------------------------------------------------
019900130702       //?Definizione variabili globali.                               ?
020000130702       //--------------------------------------------------------------
020100130702
020200130702       // -?Flags booleani?
020300130702     d $Fine           s               n   inz
020400130704     d $Forza1         s               n   inz
020500130704     d $Forza2         s               n   inz
020600130702     d $InzD01         s               n   inz(*on)
020700130702     d $InzW01         s               n   inz(*on)
020800130703     d $InzW02         s               n   inz(*on)
020900130702
021000130702       // -?Indici di schiera?
021100130702     d xx              s              5  0 inz
021200130702
021300130702       // -?Variabili per la gestione del video?
021400130702     d $Video          s              2    inz('D1')
021500130702     d Save_dsp_aid    s                   like(dsp_aid)  inz
021600130702
021700130702       // -?Controllo cambiamenti tra i dati gestiti a video:?
021800130708       // ·?Codice commerciale?
021900130702     d SaveUNI         s                   like(V1Cuni)   inz(*hival)
022000130704       // ·?Particolarità?
022100130704     d SavePAR         s                   like(V1Cpar)   inz(*hival)
022200130704       // ·?Funzione aziendale?
022300130704     d SaveFUN         s                   like(V1Cfun)   inz
022400130704       // ·?Date decorrenza e scadenza attività?
022500130704     d SaveDtAtt       s                   like(ds_DtAtt) inz(*zero)
022600130702       // ·?Codice dipendente?
022700130702     d SaveDIP         s                   like(ds_Dip)   inz
022800130704       // ·?Filiale di appartenenza
022900130704     d SaveFIL         s                   like(V1Cfil)   inz
023000130702       // ·?Descrizione commerciale e relativo indirizzo e-mail?
023100130709     d SaveNTC         s                   like(ds_NTC)   inz
023200130703
023300130703       // -?Nuovo codice commerciale in cui copiare i dati?
023400130708     d wNEWcmm         s                   like(CMMcod)  inz
023500130703
023600130703       // -?Progressivo per nuovo codice commerciale (xxx.yNNN)?
023700130703     d wProgr          s              3  0 inz
023800141212
023900141212       // -?Numero totale di Commerciali inseriti in AZCMM?
024000141212     d wTotCmm         s              5  0 inz
024100130703
024200130703       // -?Codice abilitazioni utente?
024300130703     d wAbi            s                   like(UTEaut)  inz
024400130702
024500130702       // -?Campi di comodo?
024600130709     d wNTCrnt01       s                   like(NTCrnt)  inz
024700130709     d wNTCrnt02       s                   like(NTCrnt)  inz
024800130708     d wFil            s                   like(CMMfil)  inz
024900170330     d wFilD           s                   like(ORGdes)  inz
025000130702     d wDate           s              8  0 inz
025100130702     d wDataIni        s              8  0 inz
025200130702     d wDataFin        s              8  0 inz
025300130705     d wDate_ISO       s               d   inz  datfmt(*iso)
025400130702
025500130702       //--------------------------------------------------------------
025600130702       //?Definizione prototipi procedure.                             ?
025700130702       //--------------------------------------------------------------
025800130702
025900130702       // -?Reperimento dati utente?
026000130702     d TIBS34ds      e ds                  inz
026100130702      /copy gaitrasrc/srcProtoPR,TIBS34R
026200130703       // -?Flags Abilitazioni utenti aziendali?
026300130703     d dUte01        e ds                  inz
026400130705
026500130705       // -?Verifica se commerciale disattivabile?
026600130705     d TRMK42ds      e ds                  inz
026700130705      /copy gaitrasrc/srcProtoPr,TRMK42R
026800130703
026900130703       // -?Caricamento filiali gestite?
027000130703     d TRUL31ds      e ds                  inz
027100130703     d   $POg                 10    759    inz   dim(250)
027200130703      /copy gaitrasrc/srcProtoPr,TRUL31R
027300141212
027400141212       // -?Controllo n° record (commerciali) inseriti?
027500141212     d TRUL0Sds      e ds                  inz
027600141212      /copy gaitrasrc/srcProtoPr,TRUL0SR
027700130702
027800130702       // -?Controllo formale / Inversione data?
027900130702     d WLBdat          ds                  inz
028000130702     d   G02dat                1      8  0 inz
028100130702     d   G02inv                9     16  0 inz
028200130702     d   G02err               17     17    inz('3')
028300130702     d   G02tgi               18     22  0 inz
028400130702      /copy gaitrasrc/srcProtoPR,XSRDA8
028500130702
028600130702       // -?Ricerca/Controllo tabelle (TNTBE)?
028700130702     d TIBS02ds      e ds                  inz
028800130702     d   T02mod      e                     inz('R')
028900130702     d   T02cod      e                     inz('FUN')
029000130702      /copy gaitrasrc/srcProtoPR,TIBS02R
029100130702
029200130702       // -?Reperimento dati tabelle?
029300130704      *///copy gaitrasrc/srcProtoPI,TRULTAB
029400130704      *///copy gaitrasrc/srcProtoPR,TRULTAB
029500130704
029600130704       // -?Reperimento dati Anagrafica Società?
029700130704      /copy gaitrasrc/srcConst,TIBSSOCR
029800130705     d*//prmRqsOpCode    s             10a
029900130705     d*//prmRpyOpCode    s             10a
030000130705     d*//prmRpyIdMsg     s             10i 0
030100130705     d*//prmRqsFormato   s             10a
030200130705     d*//prmRqsDataSize  s             10i 0
030300130705     d*//prmRpyFormato   s             10a
030400130705     d*//prmRpyDataSize  s             10i 0
030500130704     d tibsSocI0     e ds                  qualified  inz
030600130704     d tibsSocO0     e ds                  qualified  inz
030700130705      /Define   DFTACTGRP_NO
030800130704      /copy gaitrasrc/SrcProtoPR,TIBSSOCR
030900130705      /UnDefine DFTACTGRP_NO
031000130704
031100130704       // -?Reperimento dati Anagrafica Dipendenti?
031200130704     d TRMZ47ds      e ds                  inz
031300130704     d trmz47r         pr                  extpgm('TRMZ47R')
031400130704     d   kpjba                             likeds(KPJBA)
031500130702
031600130702       // -?Controllo formale indirizzi e-mail?
031700130702     d dsEMAIL       e ds                  inz
031800130702      /copy gaitrasrc/srcProtoPR,XEMAIL
031900130702
032000130702       // -?Parametri API QCAPCMD (Process Commands)?
032100130702     d Qcmd            s           2048    inz  varying
032200130702      /copy qSysInc/qRpgleSrc,QCAPCMD
032300130702       // -?API QCAPCMD (Process Commands)?
032400130702      /copy gaitrasrc/srcProtoPR,QCAPCMD
032500130702
032600130702       // -?Parametri gestione errori API.?
032700130702      /copy qsysinc/qrpglesrc,QUSEC
032800130702
032900130702       //--------------------------------------------------------------
033000130702       //?Definizione key-list.                                        ?
033100130702       //--------------------------------------------------------------
033200130702
033300130702       // -?File AZSOC01L: Anagrafica Società?
033400130702     d k01azsoc01    e ds                  extname(AZSOC01L : *key)
033500130702     d                                     prefix(k_)   inz
033600130702
033700130702       // -?File AZDAN01L: Anagrafica Dipendenti?
033800130702     d k02azdan01    e ds                  extname(AZDAN01L : *key)
033900130702     d                                     prefix(k_)   inz
034000130702
034100130708       // -?File AZCMM01L: Anagrafica Commerciali?
034200130708     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
034300130702     d                                     prefix(k_)   inz
034400130702
034500130709       // -?File AZNTC01L: Note/Rubrica Commerciali?
034600130709     d k02azntc01    e ds                  extname(AZNTC01L : *key)
034700130702     d                                     prefix(k_)   inz
034800130702
034900130702       //--------------------------------------------------------------
035000130702       //?M A I N - L I N E                                            ?
035100130702       //--------------------------------------------------------------
035200130702
035300130702     c     *Entry        plist
035400130702     c                   parm                    KPJBA
035500130708     c                   parm                    prm_AZCMM_ds
035600130709     c*//                parm                    prm_AZNTC01_ds
035700130709     c*//                parm                    prm_AZNTC02_ds
035800130702
035900130702      /free
036000130702
036100130702       // -?Operazioni iniziali?
036200130702       exsr sr_RoutInz;
036300130702
036400130702       // -?Gestione videate?
036500130702       DoW  $Fine = *off;
036600130702
036700130702         select;
036800130702
036900130702           // -?Gestione videata D1 (gestione dati del commerciale)?
037000130702           when $Video = 'D1';
037100130702             exsr sr_GesD01;
037200130705
037300130705           // -?Gestione window W1 (richiesta filiale per cui copiare)?
037400130705           when $Video = 'W1';
037500130705             exsr sr_GesW01;
037600130702
037700130702           // -?? ? ? ? ??
037800130702           other;
037900130702             $Fine = *on;
038000130702
038100130702         endsl;
038200130702
038300130702       EndDo;
038400130702
038500130702       // -?Operazioni finali?
038600130702       exsr sr_RoutEnd;
038700130702
038800130702       //--------------------------------------------------------------
038900130702       //?Operazioni iniziali.                                         ?
039000130702       //--------------------------------------------------------------
039100130702       BEGSR  sr_RoutInz;
039200130702
039300130702         *inLR = *on;
039400130702
039500130702         // -?Reperimento dati job?
039600130702         exsr  sr_DatiJob;
039700130711         // -?Se ho errori nei dati utente esco dal pgm?
039800130711         if  DUTerr = 'E';
039900130711           $Fine = *on;
040000130711           leavesr;
040100130711         endif;
040200130702
040300130702         // -?Reperimento data odierna?
040400130702         wDate = %int( %subst( %char( %dec( %timestamp() ) )
040500130702                               : 1 : 8 ) );
040600130705         wDate_ISO = %date();
040700130702
040800130702         // -?Impostazione nome programma a video?
040900130702         V1Tpgm = SDSpgm;
041000130702
041100130702         // -?Impostazione parametri ricevuti?
041200130702         TRMK41ds = KPJBU;
041300130702
041400130702         // -?Pulizia eventuali parametri di output (se ricevuti)?
041500130704         oMK41err = *off;
041600130704         clear  oMK41msg;
041700130702         if  %parms() > 1;
041800130708           clear  prm_AZCMM_ds;
041900130709           //clear  prm_AZNTC01_ds;
042000130709           //clear  prm_AZNTC02_ds;
042100130702         endif;
042200130702
042300130702         // -?Decodifica opzione richiesta?
042400130702         xx = %lookup( iMK41opz : $Opz );
042500130702         if  xx > *zero;
042600130702           V1Topz = $OpzD(xx);
042700130702         else;
042800130702           oMK41err = *on;
042900130702           oMK41msg = $Msg(01);
043000130704           $Fine  = *on;
043100130702           leavesr;
043200130702         endif;
043300130703
043400130703         // -?Controlli relativi alla Disattivazione?
043500130703         If  iMK41opz = 4;
043600130704           exsr  sr_CallTRMK42;
043700130703           if  ErrGenerico;
043800130703             $Fine = *on;
043900130703             leavesr;
044000130703           endif;
044100130703         EndIf;
044200130702
044300130702         // -?Caricamento commerciali?
044400130702         clear  xx;
044500130708         clear  $Cmm;
044600130708         clear  $CmmDes;
044700130708         clear  $CmmUni;
044800130708         setll  (*loval)  AZCMM000;
044900130708         read(n)  AZCMM000;
045000130708         DoW  Not %eof(AZCMM01L);
045100130708           If  CMMdtFin > wDate;
045200130702             xx  += 1;
045300130708             $Cmm(xx)    = CMMcod;
045400130708             $CmmDes(xx) = CMMdes;
045500130708             $CmmUni(xx) = CMMuni;
045600130702           EndIf;
045700130708           read(n)  AZCMM000;
045800130702         EndDo;
045900141212         wTotCmm = xx;
046000130705
046100130705         // -?Impostazione videata?
046200130705         if  iMK41opz = 3;
046300130705           $Video  = 'W1';
046400130705           $InzW01 = *on;
046500130705         else;
046600130705           $Video  = 'D1';
046700130705           $InzD01 = *on;
046800130705         endif;
046900130702
047000130702       ENDSR;
047100130702
047200130702       //--------------------------------------------------------------
047300130702       //?Reperimento Dati del job (Utente/Operativi).                 ?
047400130702       //--------------------------------------------------------------
047500130702       BEGSR  sr_DatiJob;
047600130702
047700130702         in(e) §AzUte;
047800130702         if NOT %error;
047900130702           in(e) §DatiUte;
048000130702         endif;
048100130702         if %error or RSut = *blank;
048200130702           tibs34r ( tibs34ds );
048300130702           in §AzUte;
048400130702           in §DatiUte;
048500130702         endif;
048600130702
048700130702       ENDSR;
048800130703
048900130703       //--------------------------------------------------------------
049000130703       //?Controllo se il commerciale è disattivabile.                 ?
049100130703       //--------------------------------------------------------------
049200130705       BEGSR  sr_CallTRMK42;
049300130704
049400130704         reset  TRMK42ds;
049500130708         iMK42cmm = iMK41cmm;
049600130704         kpjbu = TRMK42ds;
049700130704
049800130704         trmk42r ( kpjba );
049900130704
050000130704         TRMK42ds = kpjbu;
050100130704         if  oMK42err <> *off;
050200130704           oMK41err = *on;
050300130704           oMK41msg = oMK42msg;
050400130704           ErrGenerico = *on;
050500130704         endif;
050600130703
050700130703       ENDSR;
050800130703
050900130703       //--------------------------------------------------------------
051000130703       //?Richiesta del codice filiale in cui copiare il commerciale.  ?
051100130703       //--------------------------------------------------------------
051200130703       BEGSR  sr_GesW01;
051300130703
051400130703         // -?Inizializzazione videata (window)?
051500130703         if  $InzW01 = *on;
051600130703           exsr  sr_InzW01;
051700130703           if  $Fine = *on;
051800130703             leavesr;
051900130703           endif;
052000130703           $InzW01 = *off;
052100130703         endif;
052200130703
052300130705         // -?Emissione testata (window)?
052400130705         write  MK41WT1;
052500130703
052600130705         // -?Emissione videata?
052700130703         exfmt  MK41W01;
052800130703
052900130703         clear  VIDmsg;
053000130703         reset  ErrMessage;
053100130703         reset  ErrGenerico;
053200130703
053300130703         SELECT;
053400130703
053500130703           // -?F3=Fine?
053600130703           WHEN  dsp_aid = c_F03;
053700130703             oMK41fxx = '03';
053800130703             $Fine = *on;
053900130703             leavesr;
054000130703
054100130703           // -?F12=Ritorno?
054200130703           WHEN  dsp_aid = c_F12;
054300130703             oMK41fxx = '12';
054400130703             $Fine = *on;
054500130703             leavesr;
054600130703
054700130703           // -?Invio / F6=Conferma?
054800130703           OTHER;
054900130703             exsr  sr_CtrW01;
055000130703             // -?Impostazione dati e passaggio alla videata D01?
055100130703             If  NOT  ErrGenerico   and   dsp_aid = c_F06;
055200130703               $InzD01 = *on;
055300130703               $Video  = 'D1';
055400130703             EndIf;
055500130703
055600130703         ENDSL;
055700130703
055800130703       ENDSR;
055900130703
056000130703       //--------------------------------------------------------------
056100130703       //?Inizializzazione window W01.                                 ?
056200130703       //--------------------------------------------------------------
056300130703       BEGSR  sr_InzW01;
056400130703
056500130703         // -?Caricamento filiali gestibili dall'utente?
056600130703         clear  wAbi;
056700130703         select;
056800130703           // -?Se ho errori nei dati utente esco dal pgm?
056900130711           //  ?(controllo già eseguito nella subr. sr_RoutInz)?
057000130711           //when  DUTerr = 'E';
057100130711           //  $Fine = *on;
057200130703           // -?Se c'è l'abilitazione: prendo quella?
057300130703           when  UTEaut <> *blank;
057400130703             dUTE01 = UTEfaf;
057500130703             if  §UTEgtc <> *blank;
057600130703               wAbi = §UTEpot;
057700130703             else;
057800130703               wAbi = UTEaut;
057900130703             endif;
058000130703           // -?se sede: abilitazione aziendale?
058100130703           when  DUTlpo =  'S';
058200130703             wAbi = 'AZ';
058300130703           // -?se 1° livello: abilitazioni al terminal?
058400130703           when  DUTlpo =  '1';
058500130703             wAbi = 'TP';
058600130703           // -?se 2° livello: abilitazioni alla filiale?
058700130703           when  DUTlpo =  '2';
058800130703             wAbi = 'PO';
058900130703         endsl;
059000130703         clear  TRUL31ds;
059100170331         If  DUTlpo <> 'S';
059200170331           I31abi = wAbi;
059300170331           I31cpo = DUTpou;
059400170331           trul31r ( KPJBA : TRUL31ds );
059500170331           if  O31pog <= *zero;
059600170331             oMK41err = *on;
059700170331             oMK41msg = $Msg(27);
059800170331             $Fine = *on;
059900170331           endif;
060000170331         EndIf;
060100130705
060200130705         Select;
060300130705
060400130705           // -?Codice commerciale errato (xxx.x999)?
060500130708           When  %subst( %editc( iMK41cmm : 'X' ) : 5 : 3 ) = *all'9';
060600130705             oMK41err = *on;
060700130705             oMK41msg = $Msg(28);
060800130705             $Fine = *on;
060900130705
061000130705           // -?Codice commerciale di filiale NON gestibile dall'utente?
061100130705           //  ?(se di filiale)?
061200170331           When  DUTlpo <> 'S'   and
061300170331                 %lookup ( %subst( %editc( iMK41cmm : 'X' )
061400130705                           : 1 : 3 ) : $POg ) = *zero;
061500130705             oMK41err = *on;
061600130705             oMK41msg = $Msg(29);
061700130705             exsr  sr_RoutEnd;
061800130705
061900130705         EndSl;
062000130703
062100130703         // -?Impostazione campi a video?
062200130712         clear  MK41W01;
062300130712
062400130708         chain(N)  (iMK41cmm)  AZCMM000;
062500130705
062600130708         W1Ccmm = iMK41cmm;
062700130705
062800130705         Select;
062900130705           // -?Codice commerciale NON reperito?
063000130708           When  Not %found(AZCMM01L);
063100130705             //W1Dcmm = *all'? ';
063200130705             oMK41err = *on;
063300130705             oMK41msg = $Msg(02);
063400130705             exsr  sr_RoutEnd;
063500130705           // -?Codice commerciale NON unificante?
063600130708           when  iMK41cmm <> CMMuni;
063700130705             oMK41err = *on;
063800130705             oMK41msg = $Msg(30);
063900130705             exsr  sr_RoutEnd;
064000130705           Other;
064100130708             W1Dcmm = CMMdes;
064200130705         EndSl;
064300130703
064400130703         // -?Intabellamento dati dei commerciali con unificante W1CCMM?
064500130704         exsr  sr_Intab_Fil_Cmm;
064600130703
064700130703       ENDSR;
064800130703
064900130703       //--------------------------------------------------------------
065000130703       //?Controllo dati window W01.                                   ?
065100130703       //--------------------------------------------------------------
065200130703       BEGSR  sr_CtrW01;
065300130703
065400170330         clear  W1Dfil;
065500170330
065600130703         // -?Codice filiale obbligatorio?
065700130703         if  W1Cfil = *zero;
065800130712           VIDmsg = $Msg(31);
065900130703           PosCurFILw1 = *on;
066000130703           ErrMessage  = *on;
066100130703           ErrGenerico = *on;
066200130703           leavesr;
066300130703         endif;
066400130703
066500130703         W1Cfil = %abs(W1Cfil);
066600130703
066700130703         // -?Filiale errata?
066800170330         wFIL   = W1Cfil;
066900170330         exsr  sr_CtrlFIL;
067000170330         W1Dfil = wFILd;
067100170330         if  ErrMessage  = *on;
067200170330           VIDmsg = $Msg(32);
067300170330           PosCurFILw1 = *on;
067400170330           PosCurFIL   = *off;
067500170330           leavesr;
067600170330         endif;
067700130703
067800130703         // -?Unificante già presente per la filiale in cui copiare?
067900130703         xx = %lookup( W1Cfil : $Cmu_Fil );
068000130703         if  xx > *zero;
068100130712           VIDmsg = %trimr($Msg(07)) + ' '
068200130712                  + %subst( %editc( W1Cfil : 'X' ) : 1 : 3 )
068300130712                  + ': ' + $Cmu_Cmm(xx);
068400130703           PosCurFILw1 = *on;
068500130703           ErrMessage  = *on;
068600130703           ErrGenerico = *on;
068700130703           leavesr;
068800130703         endif;
068900130703
069000130703         // -?Ricerca nuovo codice commerciale?
069100130703         wProgr = 1;
069200130708         k_CMMcod = ( W1Cfil * 10000 ) +
069300130705                    ( %int( %subst( %editc( W1Ccmm : 'X' )
069400130705                                    : 4 : 1 ) ) * 1000 ) +
069500130705                    wProgr;
069600130708         setll  %kds(k01azcmm01)  AZCMM000;
069700130708         read(N)  AZCMM000;
069800130708         Dow  Not %eof(AZCMM01L)   and
069900130708              %subst( %trim( %editc( CMMcod : 'X' ) ) : 1 : 4 ) =
070000130708              %subst( %trim( %editc( k_CMMcod : 'X' ) ) : 1 : 4 );
070100130708           if  wProgr <> %int( %subst( %trim( %editc( CMMcod : 'X' ) )
070200130703                                       : 5 : 3 ) );
070300130708             //wNEWcmm = %int( %subst( %trim( %editc( CMMcod : 'X' ) )
070400130703             //                        : 1 : 4 ) ) * 1000 + wProgr;
070500130703             leave;
070600130703           endif;
070700130703           if  wProgr = *hival;
070800130712             VIDmsg = $Msg(33);
070900130703             PosCurFIL   = *on;
071000130703             ErrMessage  = *on;
071100130703             ErrGenerico = *on;
071200130703             leavesr;
071300130703           endif;
071400130703           wProgr += 1;
071500130708           read  AZCMM000;
071600130703         EndDo;
071700130703
071800130708         wNEWcmm = %int( %subst( %trim( %editc( k_CMMcod : 'X' ) )
071900130703                                 : 1 : 4 ) ) * 1000 + wProgr;
072000130703         $Video  = 'D1';
072100130703         $InzD01 = *on;
072200130703
072300130703       ENDSR;
072400130702
072500130702       //--------------------------------------------------------------
072600130702       //?Gestione videata D01.                                        ?
072700130702       //--------------------------------------------------------------
072800130702       BEGSR  sr_GesD01;
072900130702
073000130702         // -?Inizializzazione videata?
073100130702         if  $InzD01 = *on;
073200130702           exsr  sr_InzD01;
073300130702           $InzD01 = *off;
073400130702         endif;
073500130702
073600130702         // -?(Ri)Abilitazione tasti funzionali?
073700130702         exsr  sr_AbilFxxD01;
073800130702
073900130702         // -?Emissione Testata e Piede?
074000130702         write  MK41T01;
074100130702         write  MK41P01;
074200130702
074300130702         // -?Emissione videata?
074400130702         if  iMK41opz <> 5;
074500130702           exfmt  MK41D01;
074600130702         else;
074700130702           write  MK41D01;
074800130702           exfmt  Protect;
074900130702         endif;
075000130702
075100130702         clear  VIDmsg;
075200130702         reset  ErrMessage;
075300130702         reset  ErrGenerico;
075400130702
075500130702         SELECT;
075600130702
075700130702           // -?F3=Fine?
075800130702           WHEN  dsp_aid = c_F03;
075900130708             unlock  AZCMM01L;
076000130709             unlock  AZNTC01L;
076100130702             oMK41fxx = '03';
076200130702             $Fine  = *on;
076300130702
076400130702           // -?F12=Ritorno?
076500130702           WHEN  dsp_aid = c_F12;
076600130708             unlock  AZCMM01L;
076700130709             unlock  AZNTC01L;
076800130702             oMK41fxx = '02';
076900130702             $Fine = *on;
077000130702
077100130702           // -?Invio in Visualizzazione?
077200130702           //WHEN  iMK41opz  = 5   and
077300130702           //      dsp_aid = c_Enter;
077400130702           //  $Fine = *on;
077500130702
077600130703           // -?Invio / F6=Aggiornamento?
077700130702           OTHER;
077800130702             // -?Controlli NON eseguiti SE richiesta Disabilitazione?
077900130702             if  iMK41opz = 4;
078000130719               //exsr  sr_CallTRMK42;    ?- già eseguito in sr_RoutInz?
078100130702             else;
078200130702               exsr  sr_CtrD01;
078300130702             endif;
078400130702             if  ErrGenerico = *on;
078500130702               leavesr;
078600130702             endif;
078700130702             // -?Verifica se modificati dei dati allineabili in tutta?
078800130702             //  ?la famiglia del commerciale (SE UNIFICANTE)?
078900130702             If  dsp_aid = c_F06  and
079000130702                 (iMK41opz = *zero  or  iMK41opz = 2   or
079100130705                                        iMK41opz = 3)  and
079200130708                 V1Ccod  = V1Cuni;
079300130705               $InzW02 = *on;
079400130703               exsr  sr_GesW02;
079500130702               if  ErrGenerico = *on  or  dsp_aid = c_F12;
079600130702                 leavesr;
079700130702               endif;
079800130702             EndIf;
079900130702             // -?Aggiornamento?
080000130702             If  dsp_aid = c_F06;
080100130702               exsr  sr_UpdateAll;
080200130702               if  ErrGenerico = *on;
080300130702                 leavesr;
080400130702               endif;
080500130702               // ·?Uscita dal programma?
080600130702               $Fine = *on;
080700130702             EndIf;
080800130702
080900130702         ENDSL;
081000130702
081100130702       ENDSR;
081200130702
081300130702       //--------------------------------------------------------------
081400130702       //?Inizializzazione videata D01.                                ?
081500130702       //--------------------------------------------------------------
081600130702       BEGSR  sr_InzD01;
081700130702
081800130702         clear  MK41D01;
081900130705         clear  $Forza1;
082000130705         clear  $Forza2;
082100130705         clear  Save_dsp_aid;
082200130702
082300130702         // -?Impostazioni di protezione:?
082400130702         ProtectDtIni = *on;
082500130702         Select;
082600130702           // ·?Inserimento: protezione nulla?
082700130702           When  iMK41opz = *zero;
082800130702             ProtectKey   = *off;
082900130702             ProtectFld   = *off;
083000130702             ProtectDtIni = *off;
083100130702           // ·?Modifica e Riabilitazione: protezione campi chiave?
083200130702           //  ?(e data inizio attività, MA si veda anche dopo...)?
083300141217           When  iMK41opz = 2  or  iMK41opz = 7;
083400130702             ProtectKey   = *on;
083500130702             //ProtectDtIni = *on;      ?(già così)?
083600130702           // ·?Copia: protezione altri campi?
083700130702           //  ?(tranne la data inizio attività)?
083800130702           When  iMK41opz = 3;
083900130702             ProtectFld   = *on;
084000130702             ProtectDtIni = *off;
084100130702           // ·?Disabilitazione e Visualizzazione: protezione totale?
084200130702           //When  iMK41opz = 4  or  iMK41opz = 5;
084300130702           Other;
084400130702             ProtectKey = *on;
084500130702             ProtectFld = *on;
084600130705             //ProtectDtIni = *on;      ?(già così)?
084700130702         EndSl;
084800130702
084900130702         //*//  ?Utente EDP: data sempre sprotetta?
085000130702         //*if  %subst(Knmus:1:3) = 'EDP';
085100130702         //*  ProtectDtIni = *off;
085200130702         //*endif;
085300130702
085400130704         //  ?Utente EDP: Filiale Appartenenza SEMPRE visualizzata?
085500130702         if  %subst(Knmus:1:3) = 'EDP';
085600130704           VisualFIL = *on;
085700130702         endif;
085800130702
085900130704
086000130708         // -?Reperimento dati commerciale?
086100130708         k_CMMcod = iMK41cmm;
086200130702         Select;
086300130702           // ·?Inserimento: non reperisce nessun dato?
086400130702           When  iMK41opz = *zero;
086500130702             V1DtIni = *Date;
086600130702             V1DtFin = c_MaxDate_Eur;
086700130702             leavesr;
086800130702           // ·?Copia o Visualizzazione?
086900130702           When  iMK41opz = 3  or  iMK41opz = 5;
087000130708             chain(N)  %kds(k01azcmm01)  AZCMM000;
087100130702           // ·?Modifica, Annullamento o Ripristino?
087200130702           Other;
087300130708             chain(E)  %kds(k01azcmm01)  AZCMM000;
087400130702         EndSl;
087500130704
087600130702
087700130702         select;
087800130702           // ·?Record NON trovato?
087900130708           when  Not %found(AZCMM01L);
088000130702             $Fine = *on;
088100130705             oMK41err = *on;
088200130705             oMK41msg = $Msg(02);
088300130702             exsr  sr_RoutEnd;
088400130702           // ·?Record allocato?
088500130702           when  iMK41opz <> 3  and  iMK41opz <> 5  and  %error();
088600130702             $Fine = *on;
088700130705             oMK41err = *on;
088800130705             oMK41msg = $Msg(03);
088900130702             exsr  sr_RoutEnd;
089000130702         endsl;
089100130704
089200130702
089300130702         // -?Sprotezione Data Inizio Attività?
089400130702         //  ?SE  modifica  &  Data Inizio Attività > *date?
089500130708         if  iMK41opz = 2  and  CMMDtIni > wDate;
089600130705           ProtectDtIni = *off;
089700130702         endif;
089800130704
089900130702
090000130709         // -?Reperimento dati in file AZNTC?
090100130709         clear  wNTCrnt01;
090200130709         clear  wNTCrnt02;
090300130709         k_NTCcmm = iMK41cmm;
090400130704
090500130709         k_NTCtnt = '01';
090600130702         Select;
090700130704           // ·?Immissione?
090800130702           When  iMK41opz = *zero;
090900130704           // ·?Copia o Visualizzazione?
091000130702           When  iMK41opz = 3  or  iMK41opz = 5;
091100130709             chain(N)  %kds(k02azntc01)  AZNTC000;
091200130704           // ·?Modifica, Disattivazione o Riattivazione?
091300130702           Other;
091400130709             chain  %kds(k02azntC01)  AZNTC000;
091500130702         EndSl;
091600130709         if  iMK41opz > *zero  and  %found(AZNTC01L);
091700130709           wNTCrnt01 = NTCrnt;
091800130702         endif;
091900130702
092000130709         k_NTCtnt = '02';
092100130702         Select;
092200130704           // ·?Immissione?
092300130702           When  iMK41opz = *zero;
092400130704           // ·?Copia o Visualizzazione?
092500130702           When  iMK41opz = 3  or  iMK41opz = 5;
092600130709             chain(N)  %kds(k02azntc01)  AZNTC000;
092700130704           // ·?Modifica, Disattivazione o Riattivazione?
092800130702           Other;
092900130709             chain  %kds(k02azntc01)  AZNTC000;
093000130702         EndSl;
093100130709         if  iMK41opz > *zero  and  %found(AZNTC01L);
093200130709           wNTCrnt02 = NTCrnt;
093300130702         endif;
093400130702
093500130704
093600130702         // -?Impostazioni campi a video?
093700130703         if  iMK41opz = 3;
093800130708           V1Ccod = wNEWcmm;      // -?Cod. Commerciale?
093900130708           //V1Cfil = W1Cfil;       // -?Fil. Appartenenza?
094000130702         else;
094100130708           V1Ccod = CMMcod;       // -?Cod. Commerciale?
094200130708           V1Cpli = CMMpli;       // -?% di lavoro Italia?
094300130708           V1Cple = CMMple;       // -?% di lavoro Estero?
094400130708           //V1Cfil = CMMfil;       // -?Fil. Appartenenza?
094500130702         endif;
094600130702         // -?Descrizione?
094700130708         V1Cdes = CMMdes;
094800130702         // -?Commerciale unificante?
094900130708         V1Cuni = CMMuni;
095000130702         // -?Particolarità di Attività?
095100130708         V1Cpar = CMMpar;
095200130704         // -?Funzione aziendale?
095300130708         V1Cfun = CMMfun;
095400130702         // -?Data decorrenza attività?
095500130702         Select;
095600130705           // ·?Copia: si imposta la data odierna (*Date)?
095700130702           When  iMK41opz = 3;
095800130704             V1DtIni = *date;
095900130702           // ·?Altrimenti si visualizza la data presente?
096000130708           When  CMMdtIni > *zero;
096100130702             reset  WLBdat;
096200130708             G02inv  = CMMdtIni;
096300130702             xsrda8 ( WLBdat );
096400130702             V1DtIni = G02dat;
096500130702         EndSl;
096600130702         // -?Data fine attività?
096700130708         Select;
096800130708         //  // ·?Copia: si imposta *Max?
096900130708         //  When  iMK41opz = 3;
097000130708         //    V1DtFin = c_MaxDate_Eur;
097100130708         //  // ·?Disabilitazione: si imposta la data odierna (*Date)?
097200130708         //  When  iMK41opz = 4;
097300130708         //    V1DtFin = *Date;
097400130708         //  // ·?Riabilitazione: si imposta *Max?
097500130708         //  When  iMK41opz = 7;
097600130708         //    V1DtFin = c_MaxDate_Eur;
097700130702           // ·?Altrimenti si visualizza la data presente?
097800130708           When  CMMdtFin > *zero;
097900130702             reset  WLBdat;
098000130708             G02inv  = CMMdtFin;
098100130702             xsrda8 ( WLBdat );
098200130702             V1DtFin = G02dat;
098300130702         EndSl;
098400130704         // -?Dipendente: Codice e Società?
098500130708         if  CMMdip > *zero;
098600130708           V1Cdip = CMMdip;
098700130708           V1Csoc = CMMsoc;
098800130704         endif;
098900130708
099000130704         // -?Data inserimento?
099100130704         reset  WLBdat;
099200130708         G02inv  = CMMdtIns;
099300130704         xsrda8 ( WLBdat );
099400130704         V1DtIns = G02dat;
099500130704         // -?Data ultima modifica?
099600130704         reset  WLBdat;
099700130708         G02inv  = CMMdtMod;
099800130704         xsrda8 ( WLBdat );
099900130704         V1DtMod = G02dat;
100000130704         // -?Utente Inserimento/Ultima modifica?
100100130708         V1Dute = CMMute;
100200130702
100300130709         // -?Dati da note "01" e "02" di AZNTC00F?
100400130709         V1Crnt01 = wNTCrnt01;
100500130709         V1Crnt02 = wNTCrnt02;
100600130702
100700130702         // -?Decodifica commerciale unificante?
100800130705         exsr  sr_CtrlUNI;
100900130702         reset  ErrMessage;
101000130702         reset  ErrGenerico;
101100130702         // -?Decodifica funzione aziendale?
101200130705         exsr  sr_CheckFUN;
101300130702         reset  ErrMessage;
101400130702         reset  ErrGenerico;
101500130708         //// -?Decodifica codice del commerciale (dipendente)?
101600130708         //exsr  sr_CtrlDIP;
101700130708         //reset  ErrMessage;
101800130708         //reset  ErrGenerico;
101900130705         // -?Decodifica società del commerciale (dipendente)?
102000130705         exsr  sr_CtrlSOC;
102100130705         reset  ErrMessage;
102200130705         reset  ErrGenerico;
102300130702         // -?Decodifica filiale appartenenza?
102400130702         clear V1Dfil;
102500130704         If  V1Cfil > *zero;
102600130704           chain  (V1Cfil)  AZORG;
102700130704           if  %found(AZORG01L);
102800130702             V1Dfil = ORGdes;
102900130704           endif;
103000130704         EndIf;
103100130702
103200130702         clear  VIDmsg;
103300130702         %subst(IndDspF : 50) = *off;
103400130702
103500130702         // -?Salvataggio dati per verificare eventuali modifiche?
103600130704         reset  SaveUNI;
103700130704         reset  SavePAR;
103800130702         SaveFUN   = V1Cfun;
103900130702         SaveDIP   = ds_DIP;
104000130702         SaveDtAtt = ds_DtAtt;
104100130709         SaveNTC   = ds_NTC;
104200130702         SaveFIL   = V1Cfil;
104300130702
104400130702       ENDSR;
104500130702
104600130702       //--------------------------------------------------------------
104700130702       //?Abilitazione tasti funzionali in P01 (per D01).              ?
104800130702       //--------------------------------------------------------------
104900130702       BEGSR  sr_AbilFxxD01;
105000130702
105100130702         // ->?F3 = Fine?
105200130702         F3Attivo = *on;
105300130702
105400130702         // ->?F6 = Conferma?
105500130703         F6Attivo = (iMK41opz <> 5);
105600130702
105700130702         // ->?F12 = Ritorno?
105800130702         F12Attivo = *on;
105900130702
106000130702       ENDSR;
106100130702
106200130702       //--------------------------------------------------------------
106300130702       //?Controllo dati videata D01.                                  ?
106400130702       //--------------------------------------------------------------
106500130702       BEGSR  sr_CtrD01;
106600130702
106700130704         // -?Spegnimento indicatori di errore?
106800130702         %subst(IndDspF : 50) = *off;
106900130702
107000130702         // -?Controllo codice commerciale?
107100130702         //  ?(SOLO SE inserimento o copia)?
107200130703         If  iMK41opz = *zero  or  iMK41opz = 3;
107300130708           exsr  sr_CtrlCMM;
107400130702           if  ErrGenerico;
107500130702             leavesr;
107600130702           endif;
107700130702         EndIf;
107800130702
107900130705         // -?Ragione sociale commerciale obbligatoria?
108000130708         If  V1Cdes = *blank;
108100130704           VIDmsg = $Msg(08);
108200130708           PosCurDES   = *on;
108300130702           ErrMessage  = *on;
108400130702           ErrGenerico = *on;
108500130702           leavesr;
108600130702         EndIf;
108700130702
108800130702         // -?Controllo codice commerciale unificante?
108900130705         exsr  sr_CtrlUNI;
109000130702         if  ErrGenerico;
109100130702           leavesr;
109200130702         endif;
109300130702
109400130702         // -?Controllo congruenza particolarità con codice?
109500130705         exsr  sr_CtrlPAR;
109600130702         if  ErrGenerico;
109700130702           leavesr;
109800130702         endif;
109900130702
110000130702         // -?Controllo funzione aziendale?
110100130705         exsr  sr_CtrlFUN;
110200130702         if  ErrGenerico;
110300130702           leavesr;
110400130702         endif;
110500130702
110600130702         // -?% di lavoro Italia obbligatoria?
110700130702         //  ?La percentuale DEVE essere positiva.?
110800130702         //  ?Non si sa perchè, ma può superare il 100% quindi?
110900130702         //  ?si controlla solo il valore positivo?
111000130704         If  V1Cpli < *zero;
111100130704           VIDmsg = $Msg(16);
111200130705           PosCurPLI   = *on;
111300130702           ErrMessage  = *on;
111400130702           ErrGenerico = *on;
111500130702           leavesr;
111600130702         EndIf;
111700130702
111800130702         // -?% di lavoro Estero obbligatoria?
111900130702         //  ?(questa può anche essere *zero, ma NON negativa)?
112000130704         If  V1Cple < *zero;
112100130704           VIDmsg = $Msg(16);
112200130705           PosCurPLE   = *on;
112300130702           ErrMessage  = *on;
112400130702           ErrGenerico = *on;
112500130702           leavesr;
112600130702         EndIf;
112700130704
112800130704         // -?Controllo data decorrenza attività?
112900130705         clear  wDataIni;
113000130704         If  V1DtIni <> *zero;
113100130704           clear  WLBdat;
113200130704           G02dat = V1DtIni;
113300130704           xsrda8 ( WLBdat );
113400130704           if  G02err = *on;
113500130704             VIDmsg = $Msg(22);
113600130704             PosCurDtINI = *on;
113700130704             ErrMessage  = *on;
113800130704             ErrGenerico = *on;
113900130704             leavesr;
114000130704           endif;
114100130704           V1dtIni  = G02dat;
114200130704           wDataIni = G02inv;
114300130704         EndIf;
114400130704
114500130704         if  iMK41opz = *zero  or  iMK41opz = 2
114600130704                               or  iMK41opz = 3;
114700130704           select;
114800130704             // -?Obbligatoria?
114900130705             when  V1DtIni = *zero;
115000130704               VIDmsg = $Msg(23);
115100130704               PosCurDtINI = *on;
115200130704               ErrMessage  = *on;
115300130704               ErrGenerico = *on;
115400130704               leavesr;
115500130704             // -?In Immissione/Copia: NON può essere PRECEDENTE?
115600130705             when  wDataIni < wDate  and
115700130704                  (iMK41opz = *zero  or  iMK41opz = 3);
115800130704               VIDmsg = $Msg(24);
115900130704               PosCurDtINI = *on;
116000130704               ErrMessage  = *on;
116100130704               ErrGenerico = *on;
116200130704               leavesr;
116300130704             // -?In Manutenzione (SE futura): NON può essese PRECEDENTE?
116400130705             when  wDataIni <  wDate  and
116500130704                   iMK41opz =  2      and
116600130708                   CMMDtIni >= wDate  and
116700130708                   wDataIni <> CMMDtIni;
116800130704               VIDmsg = $Msg(24);
116900130704               PosCurDtINI = *on;
117000130704               ErrMessage  = *on;
117100130704               ErrGenerico = *on;
117200130704               leavesr;
117300130704             // -?In Manutenzione (modificata): può essere solo FUTURA?
117400130705             //  ?(in realtà il campo V1DtIni dovrebbe essere protetto)?
117500130705             when  wDataIni <  wDate  and
117600130704                   iMK41opz =  2      and
117700130708                   wDataIni <> CMMDtIni;
117800130704               VIDmsg = $Msg(25);
117900130704               PosCurDtINI = *on;
118000130704               ErrMessage  = *on;
118100130704               ErrGenerico = *on;
118200130704               leavesr;
118300130704           endsl;
118400130704         endif;
118500130704
118600130704         // -?Controllo data scadenza attività?
118700130704         clear  wDataFin;
118800130704         If  V1DtFin <> *zero;
118900130704           clear  WLBdat;
119000130704           G02dat = V1DtFin;
119100130704           xsrda8 ( WLBdat );
119200130704           if  G02err = *on;
119300130704             VIDmsg = $Msg(22);
119400130704             PosCurDtFIN = *on;
119500130704             ErrMessage  = *on;
119600130704             ErrGenerico = *on;
119700130704             leavesr;
119800130704           endif;
119900130704           V1DtFin  = G02dat;
120000130704           wDataFin = G02inv;
120100130704         EndIf;
120200130704
120300130704         // -?Data decorrenza > Data scadenza?
120400130704         If  wDataIni > wDataFin   and   wDataFin > *zero;
120500130704           VIDmsg = $Msg(26);
120600130704           PosCurDtINI = *on;
120700130704           ErrMessage  = *on;
120800130704           ErrGenerico = *on;
120900130704           leavesr;
121000130704         endif;
121100130704
121200130704         // -?Codice e Società dipendente?
121300130708         //clear  V1Ddip;
121400130704         clear  V1Dsoc;
121500130702         Select;
121600130702           When  V1Cdip <> *zero  and  V1Csoc =  *blank;
121700130704             VIDmsg = $Msg(17);
121800130702             PosCurSOC   = *on;
121900130702             ErrMessage  = *on;
122000130702             ErrGenerico = *on;
122100130702             leavesr;
122200130702           When  V1Cdip  = *zero  and  V1Csoc <> *blank;
122300130704             VIDmsg = $Msg(18);
122400130702             PosCurDIP   = *on;
122500130702             ErrMessage  = *on;
122600130702             ErrGenerico = *on;
122700130702             leavesr;
122800130702         EndSl;
122900130704
123000130708         //// -?Controllo/Decodifica Dipendente?
123100130708         //exsr  sr_CtrlDIP;
123200130708         //if  ErrGenerico;
123300130708         //  leavesr;
123400130708         //endif;
123500130704
123600130704         // -?Controllo/Decodifica Società?
123700130705         exsr  sr_CtrlSOC;
123800130705         if  ErrGenerico;
123900130705           leavesr;
124000130705         endif;
124100130704
124200130704         // -?Filiale appartenenza?
124300170330         wFIL   = V1Cfil;
124400130705         exsr  sr_CtrlFIL;
124500170330         V1Dfil = wFILd;
124600130705         if  ErrGenerico;
124700130705           leavesr;
124800130705         endif;
124900130704
125000130702         // -?Dati per testi HTML: descrizione ed indirizzo e-mail?
125100130702         //  ?(non obbligatori se particolarità impostata?
125200130702         Select;
125300130705           When  V1Crnt01 = *blank  and  V1Cpar = *blank;
125400130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
125500130704                 //                 and
125600130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
125700130704             VIDmsg = $Msg(08);
125800130705             PosCurRNT01 = *on;
125900130702             ErrMessage  = *on;
126000130702             ErrGenerico = *on;
126100130702             leavesr;
126200130705           When  V1Crnt02 = *blank  and  V1Cpar   = *blank;
126300130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
126400130704                 //                 and
126500130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
126600130704             VIDmsg = $Msg(21);
126700130705             PosCurRNT02 = *on;
126800130702             ErrMessage  = *on;
126900130702             ErrGenerico = *on;
127000130702             leavesr;
127100130702         EndSl;
127200130702
127300130702         // -?Controllo validità indirizzo e-mail?
127400130705         If  V1Crnt02 <> *blank;
127500130702           clear  dsEMAIL;
127600130705           §EMLindI = %trimr(V1Crnt02) + c_atBrt;
127700130702           xemail ( dsEMAIL );
127800130702           if  §EMLerrO <> *off;
127900130704             //VIDmsg = $Msg(21);
128000130702             VIDmsg = §EMLmsgO;
128100130705             PosCurRNT02 = *on;
128200130702             ErrMessage  = *on;
128300130702             ErrGenerico = *on;
128400130702             leavesr;
128500130702           endif;
128600130702         EndIf;
128700130702
128800130702       ENDSR;
128900130702
129000130702       //--------------------------------------------------------------
129100130703       //?Gestione window W02.                                         ?
129200130702       //?(Richiesta di allineamento variazioni su tutta la "famiglia")?
129300130702       //--------------------------------------------------------------
129400130703       BEGSR  sr_GesW02;
129500130702
129600130702         // -?Verifica se emettere la window per richiedere?
129700130702         //  ?l'allineamento delle modifiche a tutti i commerciali?
129800130702         //  ?della famiglia (con lo stesso unificante)?
129900130702         select;
130000130711           // -?Inserimento di un nuovo unificante:?
130100130711           //  ?> Nessun allineamento richiesto <?
130200130711           when  (iMK41opz = *zero  or  iMK41opz = 3)  and
130300130711                 V1Ccod = V1Cuni;
130400130711             clear  MK41W02;
130500130711             //$Video = 'D1';         ?(ancora/già così)?
130600130702           // -?Variata la Funzione aziendale?
130700130702           when  V1Cfun   <> SaveFUN;
130800130703             $Video = 'W2';
130900130704           // -?Variate le Date decorrenza e/o fine attività?
131000130704           when  ds_DtAtt <> SaveDtAtt;
131100130704             $Video = 'W2';
131200130702           // -?Variato il Dipendente?
131300130702           when  ds_DIP   <> SaveDIP;
131400130703             $Video = 'W2';
131500130704           // -?Variata la Filiale di Appartenenza
131600130704           when  V1Cfil   <> SaveFIL;
131700130704             $Video = 'W2';
131800130704           // -?Variati i Dati per testi HTML?
131900130709           when  ds_NTC   <> SaveNTC;
132000130704             $Video = 'W2';
132100130702           // -?> Nessun allineamento <?
132200130702           other;
132300130711             clear  MK41W02;
132400130702             //$Video = 'D1';         ?(ancora/già così)?
132500130702         endsl;
132600130702
132700130703         if  $Video <> 'W2';
132800130702           leavesr;
132900130702         endif;
133000130702
133100130703         if  $InzW02;
133200130702           Save_dsp_aid = dsp_aid;
133300130705           clear  MK41W02;
133400130703           W02agg  = 'SI';
133500130703           $InzW02 = *off;
133600130702         endif;
133700130702
133800130703         exfmt  MK41W02;
133900130702
134000130702         Select;
134100130702
134200130702           // -?Invio => Prosegue con l'aggiornamento (F6)?
134300130702           When  dsp_aid = c_Enter;
134400130702             dsp_aid = Save_dsp_aid;
134500130702
134600130702           //  ?F12   => Ritorna alla videata "D1"?
134700130702           When  dsp_aid = c_F12;
134800130702             ErrGenerico = *on;
134900130702             $Video = 'D1';
135000130702             leavesr;
135100130702
135200130702         EndSl;
135300130702
135400130702       ENDSR;
135500130703
135600130703       //--------------------------------------------------------------
135700130708       //?Controllo del codice Commerciale (SOLO IN INSERIMENTO)       ?
135800130703       //--------------------------------------------------------------
135900130708       BEGSR  sr_CtrlCMM;
136000130704
136100130703         // -?Codice obbligatorio?
136200130708         if  V1Ccod <= *zero;
136300130703           VIDmsg = $Msg(04);
136400130708           PosCurCOD   = *on;
136500130703           ErrMessage  = *on;
136600130703           ErrGenerico = *on;
136700130703           leavesr;
136800130703         endif;
136900130703
137000130708         // -?Controllo (in)esistenza in AZCMM?
137100130708         setll  (V1Ccod)  AZCMM000;
137200130708         if  %equal(AZCMM01L);
137300130703           VIDmsg = $Msg(05);
137400130708           PosCurCOD   = *on;
137500130703           ErrMessage  = *on;
137600130703           ErrGenerico = *on;
137700130703           leavesr;
137800130703         endif;
137900130703
138000130703         // -?Controllo primi 3 bytes del codice:?
138100170330         //  ?devono essere una filiale (anche Logistica)?
138200170330         clear  Og143;
138300130708         wFIL = V1Ccod / 10000;
138400170330         if  wFIL = *zero;
138500170330           VIDmsg = $Msg(06);
138600170330           PosCurCOD   = *on;
138700170330           ErrMessage  = *on;
138800170330           ErrGenerico = *on;
138900170330           leavesr;
139000170330         else;
139100170330           exsr  sr_CtrlFIL;
139200170330           if  ErrGenerico = *on;
139300170330             PosCurCOD = *on;
139400170330             PosCurFIL = *off;
139500170330             leavesr;
139600170330           endif;
139700170330         endif;
139800130703
139900130704         // -?Intabellamento dati dei commerciali con unificante V1CUNI?
140000130704         If  V1Cuni <> SaveUNI;
140100130704           SaveUNI = V1Cuni;
140200130712           W1Ccmm  = V1Cuni;
140300130704           exsr  sr_Intab_Fil_Cmm;
140400130704         EndIf;
140500130703
140600130703         // -?Unificante già presente per la filiale in cui?
140700130703         //  ?inserire/copiare?
140800130708         wFIL = V1Ccod / 10000;
140900130704         xx = %lookup( wFIL : $Cmu_Fil );
141000130703         if  xx > *zero;
141100130704           VIDmsg = %trimr($Msg(07)) + ' '
141200130708                  + %subst( %editc( V1Ccod : 'X' ) : 1 : 3 )
141300130703                  + ': ' + $Cmu_Cmm(xx);
141400130708           PosCurCOD   = *on;
141500130703           ErrMessage  = *on;
141600130703           ErrGenerico = *on;
141700130703           leavesr;
141800130703         endif;
141900130703
142000130703       ENDSR;
142100130703
142200130703       //--------------------------------------------------------------
142300130703       //?Controllo del Commerciale Unificante                         ?
142400130703       //--------------------------------------------------------------
142500130705       BEGSR  sr_CtrlUNI;
142600130703
142700130704         clear  V1Duni;
142800130703
142900130703         Select;
143000130703
143100130703           // -?Codice unificante obbligatorio?
143200130704           When  V1Cuni = *zero;
143300130704             VIDmsg = $Msg(09);
143400130704             PosCurUNI   = *on;
143500130703             ErrMessage  = *on;
143600130703             ErrGenerico = *on;
143700130703             leavesr;
143800130703
143900130703           // -?Codice unificante errato?
144000130708           When  V1Cuni <> V1Ccod;
144100130708             xx = %lookup( V1Cuni : $Cmm );
144200130703             if  xx = *zero;
144300130704               VIDmsg = $Msg(10);
144400130704               PosCurUNI   = *on;
144500130703               ErrMessage  = *on;
144600130703               ErrGenerico = *on;
144700130703               leavesr;
144800130703             endif;
144900130708             V1Duni = $CmmDes(xx);
145000130705
145100130705           // -?Codice unificante se stesso?
145200130705           Other;
145300130705             select;
145400130705               when  iMK41opz = *zero  or  iMK41opz = 3;
145500130708                 V1Duni = V1Cdes;
145600130705               other;
145700130708                 V1Duni = CMMdes;
145800130705             endsl;
145900130703
146000130703         EndSl;
146100130703
146200130703       ENDSR;
146300130703
146400130703       //--------------------------------------------------------------
146500130703       //?Controllo congruenza Particolarità codice comm.le                    ?
146600130703       //--------------------------------------------------------------
146700130705       BEGSR  sr_CtrlPAR;
146800130704
146900130704         select;
147000130704
147100130704           When  V1Cpar = SavePAR;
147200130704
147300130704           When  V1Cpar <> *blank  and
147400130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
147500130704                                   and
147600130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
147700130704             SavePAR = V1Cpar;
147800130704             if  not $Forza1;
147900130704               VIDmsg = $Msg(11);
148000130704               PosCurPAR   = *on;
148100130704               ErrMessage  = *on;
148200130704               ErrGenerico = *on;
148300130705               $Forza1 = *on;
148400130704               leavesr;
148500130704             endif;
148600130703
148700130704           When  V1Cpar = *blank   and
148800130708                 (%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) = '0000'
148900130704                                   or
149000130708                  %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) = '0999');
149100130704             SavePAR = V1Cpar;
149200130704             if  not $Forza2;
149300130704               VIDmsg = $Msg(12);
149400130704               PosCurPAR   = *on;
149500130704               ErrMessage  = *on;
149600130704               ErrGenerico = *on;
149700130705               $Forza2 = *on;
149800130704               leavesr;
149900130704             endif;
150000130704
150100130704         EndSl;
150200130703
150300130703       ENDSR;
150400130703
150500130703       //--------------------------------------------------------------
150600130703       //?Controllo della Funzione Aziendale                           ?
150700130703       //--------------------------------------------------------------
150800130705       BEGSR  sr_CtrlFUN;
150900130703
151000130703         clear  V1Dfun;
151100130703         clear  dFUN;
151200130703
151300130703         Select;
151400130703
151500130703           // -?Obbligatoria?
151600130703           //  ?(NON lo è se "fittizia" - 0000-0999)?
151700130703           When  V1Cfun = *blank  and
151800130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
151900130703                                  and
152000130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
152100130704             VIDmsg = $Msg(13);
152200130703             PosCurFUN   = *on;
152300130703             ErrMessage  = *on;
152400130703             ErrGenerico = *on;
152500130703             leavesr;
152600130703
152700130703           // -?Ricerca?
152800130703           When  %scan( '?' : V1Cfun ) > *zero;
152900130703             reset  TIBS02ds;
153000130703             T02mod = 'R';
153100130703             T02sif = knsif;
153200130703             TNTBE_RicercaControllo (kpjba : TIBS02ds);
153300130703             if  T02err = *blank;
153400130703               dFUN   = T02uni;
153500130703               V1Cfun = T02ke1;
153600130703               V1Dfun = §FUNdes;
153700130703             endif;
153800130703             ErrGenerico = *on;
153900130703             leavesr;
154000130703
154100130703           // -?Controllo?
154200130703           When  V1Cfun <> *blank;
154300130705             exsr  sr_CheckFUN;
154400130703             if  ErrGenerico;
154500130703               leavesr;
154600130703             endif;
154700130703
154800130703         EndSl;
154900130703
155000130703       ENDSR;
155100130704
155200130704       //--------------------------------------------------------------
155300130705       //?Decodifica della Funzione Aziendale                           ?
155400130704       //--------------------------------------------------------------
155500130705       BEGSR  sr_CheckFUN;
155600130704
155700130704         clear  dFUN;
155800130704
155900130704         reset  TIBS02ds;
156000130704         T02mod = 'C';
156100130704         T02sif = knsif;
156200130704         T02ke1 = V1Cfun;
156300130704
156400130704         TNTBE_RicercaControllo (kpjba : TIBS02ds);
156500130704
156600130704         if  T02err <> *blank;
156700130704           VIDmsg = $Msg(14);
156800130704           PosCurFUN   = *on;
156900130704           ErrMessage  = *on;
157000130704           ErrGenerico = *on;
157100130704           leavesr;
157200130704         endif;
157300130704
157400130704         dFUN = T02uni;
157500130704
157600130704         V1Dfun = §FUNdes;
157700130704
157800130704       ENDSR;
157900130705
158000130705       //--------------------------------------------------------------
158100130705       //?Controllo Codice del dipendente                              ?
158200130705       //--------------------------------------------------------------
158300130708       //BEGSR  sr_CtrlDIP;
158400130708       //
158500130708       //  clear  V1Ddip;
158600130708       //
158700130708       //  // -?Società NON immessa?
158800130708       //  if  V1Cdip = *zero;
158900130708       //    leavesr;
159000130708       //  endif;
159100130708       //
159200130708       //  clear  TRMZ47ds;
159300130708       //
159400130708       //  D47soc = V1Csoc;
159500130708       //  D47mat = V1Cdip;
159600130708       //  kpjbu = TRMZ47ds;
159700130708       //
159800130708       //  trmz47r ( kpjba );
159900130708       //
160000130708       //  TRMZ47ds = kpjbu;
160100130708       //  If  D47errO <> *blank;
160200130708       //
160300130708       //    V1Ddip = *all'? ';
160400130708       //    VIDmsg = $Msg(20);
160500130708       //    PosCurDIP   = *on;
160600130708       //    ErrMessage  = *on;
160700130708       //    ErrGenerico = *on;
160800130708       //    leavesr;
160900130708       //
161000130708       //  Else;
161100130708       //
161200130708       //    V1Ddip = %trim(D47cogO) + ' ' + %trim(D47nomO);
161300130708       //
161400130708       //  EndIf;
161500130708       //
161600130708       //ENDSR;
161700130705
161800130705       //--------------------------------------------------------------
161900130705       //?Controllo Società del dipendente                             ?
162000130705       //--------------------------------------------------------------
162100130705       BEGSR  sr_CtrlSOC;
162200130705
162300130705         clear  V1Dsoc;
162400130705
162500130705         // -?Società NON immessa?
162600130705         if  V1Csoc = *blank;
162700130705           leavesr;
162800130705         endif;
162900130705
163000130705         // -?Apertura del programma.?
163100130705         Societa_Init();
163200130705
163300130705         // -?Decodifica.?
163400130705         clear  tibsSOCo0;
163500130705         if  Societa_NewSocieta ( V1Csoc ) >= *zero;
163600130705           tibsSOCo0 = Societa_GetAnagrafica ( 'TIBSSOCO0' );
163700130705         endif;
163800130705
163900130705         // -?Chiusura del programma.?
164000130705         Societa_Finalize();
164100130705
164200130705         // -?Valorizzazione descrizione a video.?
164300130705         If  tibsSOCo0.IdSocieta <> *blank;
164400130708           //V1Dsoc = tibsSOCo0.RagSocBrev;
164500130705         Else;
164600130705           V1Dsoc = *all'? ';
164700130705           VIDmsg = $Msg(19);
164800130705           PosCurSOC   = *on;
164900130705           ErrMessage  = *on;
165000130705           ErrGenerico = *on;
165100130705           leavesr;
165200130705         EndIf;
165300130705
165400130705       ENDSR;
165500130703
165600130703       //--------------------------------------------------------------
165700130705       //?Controllo Filiale di apprtenenza                             ?
165800130703       //--------------------------------------------------------------
165900130705       BEGSR  sr_CtrlFIL;
166000130703
166100130704         clear Og143;
166200170330         clear wFILd;
166300130705
166400130705         // -?Filiale NON immessa?
166500170330         if  wFIL = *zero;
166600130705           leavesr;
166700130705         endif;
166800130704
166900130703         // -?Controllo?
167000170330         ORGfil = wFIL;
167100130703         chain  (ORGfil)  AZORG;
167200130704
167300130704         If  not %found(AZORG01L);
167400130704           VIDmsg = $Msg(15);
167500130704           PosCurFIL   = *on;
167600130703           ErrMessage  = *on;
167700130703           ErrGenerico = *on;
167800130703           leavesr;
167900130704         EndIf;
168000130704
168100170330         wFILd  = ORGdes;
168200170330         Og143  = ORGde3;
168300170330
168400170330         If  ORGfva <> *blank  or  (
168500170330             ORGfag <> 'A'    and  ORGfag <> 'F'  and
168600170330             Og143.§OGntw <> 'LOG' );
168700130704           VIDmsg = $Msg(15);
168800130704           PosCurFIL   = *on;
168900130703           ErrMessage  = *on;
169000130703           ErrGenerico = *on;
169100130703           leavesr;
169200130704         EndIf;
169300130703
169400130703       ENDSR;
169500130704
169600130704       //--------------------------------------------------------------
169700130704       //?Intabellamento Filiali e 1° Comm.le per Comm.le Unificante   ?
169800130704       //--------------------------------------------------------------
169900130704       BEGSR  sr_Intab_FIL_CMM;
170000130704
170100130704         clear  xx;
170200130704         clear  $Cmu_Fil;
170300130704         clear  $Cmu_Cmm;
170400130704
170500130708         setll  (*loval)  AZCMM000;
170600130708         read(N)  AZCMM000;
170700130704
170800130708         DoW  Not  %eof(AZCMM01L);
170900130704
171000130708           wFIL = CMMcod / 10000;
171100130704
171200130712           if  CMMuni = W1Ccmm   and
171300130704               %lookup( wFIL : $CmU_Fil ) = *zero;
171400130704
171500130704             xx += 1;
171600130704             $Cmu_Fil(xx) = wFIL;
171700130708             $Cmu_Cmm(xx) = %editc( CMMcod : 'X' );
171800130708                        //+ '-' + CMMdes;
171900130704           endif;
172000130704
172100130708           read(N)  AZCMM000;
172200130704
172300130704         EndDo;
172400130704
172500130704       ENDSR;
172600130702
172700130702       //--------------------------------------------------------------
172800130702       //?Aggiornamento/Inserimento record nel file TABEL00F.          ?
172900130702       //--------------------------------------------------------------
173000130702       BEGSR  sr_UpdateAll;
173100130702
173200130702         // -?Aggiornamento Note/Rubrica Commerciali?
173300130709         exsr  sr_UpdAZNTC;
173400130702
173500130702         // -?Aggiornamento Anagrafica Commerciali?
173600130708         exsr  sr_UpdAZCMM;
173700130702
173800130704         // -?Aggiornamento Anagrafica del resto della famiglia?
173900130703         if  $Video = 'W2'  and  W02agg = 'SI';
174000130702           exsr  sr_UpdFAMIGLIA;
174100130702         endif;
174200141212
174300141212         // -?Verifica n° commerciali inseriti?
174400141212         //  ?(ci sono *pgm che intabellano i commerciali in schiere di?
174500141212         //  ?5.000 elementi)?
174600141212         if  (iMK41opz = *zero  or  iMK41opz = 3  or  iMK41opz = 7)  and
174700141212             wTotCmm >= c_Max_Cmm_W;
174800141212           reset  TRUL0Sds;
174900141212           i0Stla  = 'L';
175000141212           i0Sski  = 'AGENTI COMM.LI  ';
175100141212           i0Sfile = 'AZCMM00F  ';
175200141212           i0Sele  = c_Max_Cmm_W;
175300141212           i0Spie  = wTotCmm + 1;
175400141212           i0Spgm  = SDSpgm;
175500141212           i0Ssif  = KNSIF;
175600141212           i0Sute  = SDSusr;
175700141212           kpjbu   = TRUL0Sds;
175800141212           trul0Sr ( kpjba );
175900141212         endif;
176000130702
176100130702       ENDSR;
176200130702
176300130702       //--------------------------------------------------------------
176400130709       //?Aggiornamento record nel file AZNTC00F                       ?
176500130702       //--------------------------------------------------------------
176600130709       BEGSR  sr_UpdAZNTC;
176700130702
176800130708         // -?NOMINATIVO commerciale (tnt "01")?
176900130709         k_NTCcmm = V1Ccod;
177000130709         k_NTCtnt = '01';
177100130709         chain  %kds(k02azntc01)  AZNTC000;
177200130702
177300130710         // -?Reperito record da cancellare o?
177400130710         //  ?record da inserire / aggiornare?
177500130709         IF  (%found(AZNTC01L)  and  iMK41opz =  4)  or
177600130704                                     iMK41opz <> 4;
177700130702
177800130702           // -?Non reperito record da scrivere o aggiornare?
177900130702           //  ?=> impostazione dei campi chiave?
178000130710           if  Not %found(AZNTC01L)  and  iMK41opz <> 4;
178100130709             clear  AZNTC000;
178200130709             NTCcmm = k_NTCcmm;
178300130709             NTCtnt = k_NTCtnt;
178400130702           endif;
178500130702
178600130702           // -?Impostazione campi nel record?
178700130709           NTCrnt = V1Crnt01;
178800130702
178900130702           // -?AGGIORNAMENTO?
179000130710           Select;
179100130710             When  iMK41opz = 4;
179200130710               if  %found(AZNTC01L);
179300130710                 //_______________
179400130710                 delete  AZNTC000;
179500130710                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
179600130710               endif;
179700130723             When  %found(AZNTC01L)      and  NTCrnt =  *blank;
179800130723               //_______________
179900130723               delete  AZNTC000;
180000130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
180100130723             When  %found(AZNTC01L)      and  NTCrnt <> *blank;
180200130723               //_______________
180300130723               update  AZNTC000;
180400130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
180500130723             When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
180600130710               //_______________
180700130710               write   AZNTC000;
180800130710               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
180900130710           EndSl;
181000130702
181100130702         ENDIF;
181200130702
181300130708         // -?E-MAIL commerciale (tnt "02")?
181400130709         k_NTCcmm = V1Ccod;
181500130709         k_NTCtnt = '02';
181600130709         chain  %kds(k02azntc01)  AZNTC000;
181700130702
181800130710         // -?Reperito record da cancellare o?
181900130710         //  ?record da inserire / aggiornare?
182000130709         IF  (%found(AZNTC01L)  and  iMK41opz =  4)  or
182100130704                                     iMK41opz <> 4;
182200130702
182300130702           // -?Non reperito record da scrivere o aggiornare?
182400130702           //  ?=> impostazione dei campi chiave?
182500130710           if  Not %found(AZNTC01L)  and  iMK41opz <> 4;
182600130709             clear  AZNTC000;
182700130709             NTCcmm = k_NTCcmm;
182800130709             NTCtnt = k_NTCtnt;
182900130702           endif;
183000130702
183100130702           // -?Impostazione campi nel record?
183200130709           NTCrnt = V1Crnt02;
183300130702
183400130702           // -?AGGIORNAMENTO?
183500130710           Select;
183600130710             When  iMK41opz = 4;
183700130710               if  %found(AZNTC01L);
183800130710                 //_______________
183900130710                 delete  AZNTC000;
184000130710                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
184100130710               endif;
184200130723             When  %found(AZNTC01L)      and  NTCrnt =  *blank;
184300130723               //_______________
184400130723               delete  AZNTC000;
184500130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
184600130723             When  %found(AZNTC01L)      and  NTCrnt <> *blank;
184700130723               //_______________
184800130723               update  AZNTC000;
184900130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
185000130723             When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
185100130723               //_______________
185200130723               write   AZNTC000;
185300130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
185400130710           EndSl;
185500130702
185600130702         ENDIF;
185700130702
185800130702       ENDSR;
185900130702
186000130702       //--------------------------------------------------------------
186100130708       //?Aggiornamento record nel file AZCMM00F.                      ?
186200130702       //--------------------------------------------------------------
186300130708       BEGSR  sr_UpdAZCMM;
186400130702
186500130708         k_CMMcod = V1Ccod;
186600130708         chain  %kds(k01azcmm01)  AZCMM000;
186700130702
186800130702         // -?Non reperito record da annullare => uscire?
186900130709         if  Not %found(AZCMM01L)  and  iMK41opz =  4;
187000130702           leavesr;
187100130702         endif;
187200130702
187300130702         // -?Non reperito record da scrivere o aggiornare?
187400130702         //  ?=> impostazione dei campi chiave?
187500130710         if  Not %found(AZCMM01L)  and  iMK41opz <> 4;
187600130708           clear  AZCMM000;
187700130708           CMMcod = k_CMMcod;
187800130702         endif;
187900130702
188000130702         // -?Impostazione campi nel record?
188100130710         If  iMK41opz <> 4;
188200130708           CMMdes   = V1Cdes;
188300130708           CMMpar   = V1Cpar;
188400130708           CMMfun   = V1Cfun;
188500130708           CMMpli   = V1Cpli;
188600130708           CMMple   = V1Cple;
188700130708           CMMdtIni = wDataIni;
188800130708           CMMdtFin = wDataFin;
188900130708           CMMdip   = V1Cdip;
189000130708           CMMsoc   = V1Csoc;
189100130708           if  V1Cfil > *zero;
189200130708             CMMfil = V1Cfil;
189300130705           else;
189400130708             clear  CMMfil;
189500130705           endif;
189600130708           //clear  CMMfil10;        ?- Per ora: NON utilizzato?
189700130708           CMMuni   = V1Cuni;
189800130710           if  Not %found(AZCMM01L);
189900130710             CMMdtIns = wDate;
190000130710           else;
190100130710             CMMdtMod = wDate;
190200130710           endif;
190300130710           CMMute   = KNMUS;
190400130710         EndIf;
190500130702
190600130702         // -?AGGIORNAMENTO?
190700130710         Select;
190800130710           When  iMK41opz = 4;
190900130710             if  %found(AZCMM01L);
191000130710               //_______________
191100130710               delete  AZCMM000;
191200130710               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191300130710             endif;
191400130710           When  Not %found(AZCMM01L);
191500130710             //_______________
191600130710             write   AZCMM000;
191700130710             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191800130710           Other;
191900130710             //_______________
192000130710             update  AZCMM000;
192100130710             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
192200130710         EndSl;
192300130702
192400130702       ENDSR;
192500130702
192600130702       //--------------------------------------------------------------
192700130709       //?Aggiornamento records nei file AZCMM00F e AZNTC00F           ?
192800130702       //?per tutti i codici con lo stesso commerciale unificante      ?
192900130702       //--------------------------------------------------------------
193000130702       BEGSR  sr_UpdFAMIGLIA;
193100130702
193200130708         xx = %lookup( V1Cuni : $CmmUni );
193300130702
193400130702         DoW  xx > *zero;
193500130702
193600130708           If  $Cmm(xx) <> V1Ccod;
193700130702
193800130708             // -?Aggiornamento dati anarafici in AZCMM00F?
193900141218             If  V1Cfun   <> SaveFUN    or    //?Funzione aziendale?
194000141218                 ds_DtAtt <> SaveDtAtt  or    //?Date start & end attività?
194100141218                 ds_DIP   <> SaveDIP    or    //?Dipendente?
194200141218                 V1Cfil   <> SaveFIL    or    //?Filiale appartenenza?
194300141218                 ds_NTC   <> SaveNTC;         //?Descriz. & e-Mail comm.le?
194400130702
194500130708               chain  ($Cmm(xx))  AZCMM000;
194600130708               if  %found(AZCMM01L);
194700130702
194800130704                 if  V1Cfun <> SaveFUN;
194900130708                   CMMfun = V1Cfun;
195000130704                 endif;
195100130704                 if  ds_DtAtt <> SaveDtAtt;
195200130708                   CMMdtIni = wDataIni;
195300130708                   CMMdtFin = wDataFin;
195400130704                 endif;
195500130702                 if  ds_DIP <> SaveDIP;
195600130708                   CMMdip = V1Cdip;
195700130708                   CMMsoc = V1Csoc;
195800130702                 endif;
195900130704                 if  V1Cfil <> SaveFIL;
196000130708                   CMMfil = V1Cfil;
196100130702                 endif;
196200130719
196300130719                 CMMdtMod = wDate;
196400130719                 CMMute   = KNMUS;
196500130704
196600130704                 //_______________
196700130708                 update  AZCMM000;
196800130704                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
196900130702
197000130702               endif;
197100130702
197200130702             EndIf;
197300130702
197400130709             // -?Aggiornamento Note/Rubrica in AZNTC00F?
197500141218             If  ds_NTC <> SaveNTC;      //?Descriz. & e-Mail comm.le?
197600130704
197700130704               // -?Nominativo commerciale: Tipo Nota "01"?
197800130709               k_NTCcmm = $Cmm(xx);
197900130709               k_NTCtnt = '01';
198000130709               chain  %kds(k02azntc01)  AZNTC000;
198100130702
198200130709               if  Not %found(AZNTC01L);
198300130709                 clear  AZNTC000;
198400141218                 NTCcmm = k_NTCcmm;           //?Commerciale?
198500141218                 NTCtnt = k_NTCtnt;           //?Tipo Nota = "01"?
198600130702               endif;
198700130702
198800130709               NTCrnt = V1Crnt01;
198900130719
199000130719               CMMdtMod = wDate;
199100130719               CMMute   = KNMUS;
199200130702
199300130723               Select;
199400130723                 When  %found(AZNTC01L)      and  NTCrnt =  *blank;
199500130723                   //_______________
199600130723                   delete  AZNTC000;
199700130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
199800130723                 When  %found(AZNTC01L)      and  NTCrnt <> *blank;
199900130723                   //_______________
200000130723                   update  AZNTC000;
200100130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
200200130723                 When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
200300130723                   //_______________
200400130723                   write   AZNTC000;
200500130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
200600130723               EndSl;
200700130704
200800130704               // -?e-Mail commerciale: Tipo Nota "02"?
200900130709               k_NTCcmm = $Cmm(xx);
201000130709               k_NTCtnt = '02';
201100130709               chain  %kds(k02azntc01)  AZNTC000;
201200130704
201300130709               if  Not %found(AZNTC01L);
201400130709                 clear  AZNTC000;
201500141218                 NTCcmm = k_NTCcmm;           //?Commerciale?
201600141218                 NTCtnt = k_NTCtnt;           //?Tipo Nota = "02"?
201700130704               endif;
201800130704
201900130709               NTCrnt = V1Crnt02;
202000130704
202100130723               Select;
202200130723                 When  %found(AZNTC01L)      and  NTCrnt =  *blank;
202300130723                   //_______________
202400130723                   delete  AZNTC000;
202500130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
202600130723                 When  %found(AZNTC01L)      and  NTCrnt <> *blank;
202700130723                   //_______________
202800130723                   update  AZNTC000;
202900130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
203000130723                 When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
203100130723                   //_______________
203200130723                   write   AZNTC000;
203300130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
203400130723               EndSl;
203500130702
203600130702             EndIf;
203700130702
203800130702           EndIf;
203900130702
204000130708           xx = %lookup( V1Cuni : $CmmUni : xx+1 );
204100130702
204200130702         EndDo;
204300130702
204400130702       ENDSR;
204500130702
204600130702       //--------------------------------------------------------------
204700130702       //?Operazioni finali.                                           ?
204800130702       //--------------------------------------------------------------
204900130702       BEGSR  sr_RoutEnd;
205000130702
205100130702         // -?Restituzine parametri?
205200130704         kpjbu = TRMK41ds;
205300130702
205400130702         // -?Uscita dal *pgm?
205500130702         return;
205600130702
205700130702       ENDSR;
205800130702
205900130702      /end-free
206000130702
206100130702       //--------------------------------------------------------------
206200130702       //?Schiere a tempo di compilazione.                             ?
206300130702       //--------------------------------------------------------------
206400130702
206500130702** -?$Opz / $OpzD?(Opzione richiesta)?
20660013070800   IMMISSIONE
20670013070802    MODIFICA
20680013070803     COPIA
20690013070804  ANNULLAMENTO  DISATTIVAZIONE
20700013070205VISUALIZZAZIONE
20710013070807   RIPRISTINO   RIATTIVAZIONE
207200130702** -?$Msg?(Messaggi di errore)?----------------------------------------------*
207300130708Opzione non valida.                                                            1
207400130708Codice commerciale inesistente                                                 2
207500130708Codice commerciale momentaneamente allocato: riprovare più tardi               3
207600130708Codice commerciale obbligatorio                                                4
207700130708Codice commerciale già esistente                                               5
207800130708Commerciale di filiale errata                                                  6
207900130708Commerciale con stesso unificante già presente in filiale                      7
208000130708Descrizione commerciale obbligatoria                                           8
208100130709Unificante obbligatorio                                                        9
208200130709Unificante inesistente                                                        10
208300130704ATTENZIONE: inserita particolarità.   Premere INVIO per forzare.              11
208400130704ATTENZIONE: NON è stata inserita la particolarità.  Premere INVIO per forzare.12
208500130708Inserire la funzione aziendale                                                13
208600130708Funzione aziendale errata                                                     14
208700130708Filiale appartenenza errata                                                   15
208800130708La percentuale deve essere minore o uguale a 100                              16
208900130708Se inserito cod dipendente,inserire anche società di appartenenza             17
209000130708Oltre alla società, inserire anche cod dipendente                             18
209100130708Società errata                                                                19
209200130708Codice dipendente inesistente per la società indicata                         20
209300130708Indirizzo e-Mail mancante o errato                                            21
209400130708Data formalmente errata                                                       22
209500130708Data obbligatoria                                                             23
209600130708La data inizio attività non può essere precedente a quella odierna            24
209700130708La data inizio attività è già trascorsa e NON può essere modificata           25
209800130708Data decorrenza attività superiore a data fine attività                       26
209900130708Nessuna filiale gestibile dall'utente                                         27
210000130708Codice commerciale errato                                                     28
210100130708Commerciale unificante NON in gestione                                        29
210200130708Commerciale NON unificante                                                    30
210300130712Codice filiale obbligatorio                                                   31
210400130712Filiale errata                                                                32
210500130712NON reperibile un nuovo codice commerciale per la filiale indicata            33
