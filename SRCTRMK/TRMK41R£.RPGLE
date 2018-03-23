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
003300141217
003400141217       // -?Tabella "01"?
003500141217     fTABEL00F  Uf A e           k disk
003600141217     f                                     extfile(wTABEL)  usropn
003700141217       // -?Tabella "HTM"?
003800141217     fTNTBE01L  Uf A e           k disk
003900141217     f                                     extfile(wTNTBE)  usropn
004000130702
004100130702       // -?Video Gestione?
004200130702     fTRMK41D   cf   e             workstn
004300130702     f                                     indds(IndDspF)
004400130702     f                                     infds(InfDspF)
004500130702
004600130702       //--------------------------------------------------------------
004700130702       //?Definizione costanti.                                        ?
004800130702       //--------------------------------------------------------------
004900141217
005000141217       // -?Codice tabella in gestione?
005100141217     d c_Tab           c                   const('01')
005200141217
005300141217       // -?Codice utente?
005400141217     d c_Kut           c                   const(1)
005500130702
005600130702       // -?@brt.it?
005700130702     d c_atBrt         c                   const('@brt.it')
005800130702
005900130702       // -?Data 31/12/2039 (formato "EUR")?
006000130702     d c_MaxDate_Eur   c                   const(31122039)
006100130702
006200130702       // -?Costante per controllo "caratteri solo numerici"?
006300130702     d c_Digits        c                   const('0123456789')
006400130702
006500130702       // -?Tasti funzionali a video?
006600130702     d c_F01           c                   const(x'31')
006700130702     d c_F02           c                   const(x'32')
006800130702     d c_F03           c                   const(x'33')
006900130702     d c_F04           c                   const(x'34')
007000130702     d c_F05           c                   const(x'35')
007100130702     d c_F06           c                   const(x'36')
007200130702     d c_F07           c                   const(x'37')
007300130702     d c_F08           c                   const(x'38')
007400130702     d c_F09           c                   const(x'39')
007500130702     d c_F10           c                   const(x'3A')
007600130702     d c_F11           c                   const(x'3B')
007700130702     d c_F12           c                   const(x'3C')
007800130702     d c_F13           c                   const(x'B1')
007900130702     d c_F14           c                   const(x'B2')
008000130702     d c_F15           c                   const(x'B3')
008100130702     d c_F16           c                   const(x'B4')
008200130702     d c_F17           c                   const(x'B5')
008300130702     d c_F18           c                   const(x'B6')
008400130702     d c_F19           c                   const(x'B7')
008500130702     d c_F20           c                   const(x'B8')
008600130702     d c_F21           c                   const(x'B9')
008700130702     d c_F22           c                   const(x'BA')
008800130702     d c_F23           c                   const(x'BB')
008900130702     d c_F24           c                   const(x'BC')
009000130702     d c_Enter         c                   const(x'F1')
009100130702     d c_RollDown      c                   const(x'F4')
009200130702     d c_RollUp        c                   const(x'F5')
009300141217
009400141217       // -?Costanti per la definizione delle schiere con i nomi?
009500141217       //  ?degli iSeries da elaborare e delle relative librerie?
009600141217     d c_NrSyst        c                   const(2)
009700141217     d c_NrLibr        c                   const(2)
009800130702
009900130702       //--------------------------------------------------------------
010000130702       //?Definizione schiere.                                         ?
010100130702       //--------------------------------------------------------------
010200141217
010300141217       // -?iSeries  &  Librerie con entrambi i file tabelle?
010400141217     d $iSystem        s                   like(currSysNetA)
010500141217     d                                     dim(c_NrSyst)
010600141217     d                                     ctdata   perrcd( 1)
010700141217     d $Libraries      s                   like(ds_Libr)
010800141217     d                                     dim(c_NrSyst)
010900141217     d                                     alt($iSystem)
011000130702
011100130702       // -?Opzioni e relative decodifiche?
011200130702     d $Opz            s                   like(iMK41opz)
011300130702     d                                     dim(6)
011400130702     d                                     ctdata   perrcd(1)
011500130702     d $OpzD           s                   like(V1Topz)
011600130702     d                                     dim(6)
011700130702     d                                     alt($Opz)
011800130702
011900130702       // -?Messaggi di errore?
012000130712     d $Msg            s             78    dim(33) ctdata perrcd(1)
012100130702
012200141212       // -?Dati dei Commerciali?
012300150108     d c_Max_Cmm_W     c                   const(9000)
012400141212     d c_Max_Cmm       c                   const(9999)
012500130708     d $Cmm            s                   like(CMMcod) dim(c_Max_Cmm)
012600130702     d                                     inz
012700130708     d $CmmDes         s                   like(CMMdes) dim(c_Max_Cmm)
012800130702     d                                     inz
012900130708     d $CmmUni         s                   like(CMMuni) dim(c_Max_Cmm)
013000130702     d                                     inz
013100130702
013200130702       // -?Filiali con commerciale unificante?
013300141212     d c_Max_Cmm_F     c                   const(999)
013400141212     d $Cmu_Fil        s                   like(CMMfil) dim(c_Max_Cmm_F)
013500130702     d                                     inz
013600130704       // -?1° comm.le di filiale x comm.le unificante (COD+DES)?
013700141212     d $Cmu_Cmm        s        +     8    like(CMMdes) dim(c_Max_Cmm_F)
013800130702     d                                     inz
013900130702
014000130702       //--------------------------------------------------------------
014100130702       //?Definizione aree dati.                                       ?
014200130702       //--------------------------------------------------------------
014300130702
014400130702       // -?Dati utente?
014500130702     d §AzUte        e ds                  extname(AZUTE00F)
014600130702     d                                     dtaara
014700130702     d §DatiUte      e ds                  extname(dDatiUte)
014800130702     d                                     dtaara
014900130702
015000130702       //--------------------------------------------------------------
015100130702       //?Definizione strutture dati.                                  ?
015200130702       //--------------------------------------------------------------
015300130702
015400130702       // -?Status ds?
015500130702     d Status         sds
015600130702     d   SDSpgm          *proc
015700141212     d   SDSusr              254    263
015800130702
015900130702       // -?InfDS?
016000130702     d InfDspF         ds
016100130702     d   dsp_aid             369    369a
016200130702
016300130702       // -?Indicatori su DspF?
016400130702     d IndDspF         ds                  inz
016500130702         // -?Abilitazione tasti funzionali?
016600130702     d   F3Attivo                      n   overlay(IndDspF : 03)
016700130702     d*//F5Attivo                      n   overlay(IndDspF : 05)
016800130702     d   F6Attivo                      n   overlay(IndDspF : 06)
016900130702     d   F12Attivo                     n   overlay(IndDspF : 12)
017000130702     d*//F16Attivo                     n   overlay(IndDspF : 16)
017100130702         // -?Emissione messaggio di errore?
017200130702     d   ErrMessage                    n   overlay(IndDspF : 28)
017300130710         // -?Indicatori per Attributi di visualizzazione?
017400130702     d   ProtectKey                    n   overlay(IndDspF : 40)
017500130702     d   ProtectFld                    n   overlay(IndDspF : 41)
017600130705     d   ProtectDtIni                  n   overlay(IndDspF : 42)
017700130704     d   VisualFil                     n   overlay(IndDspF : 43)
017800130702         // -?Posizionamento cursore & Segnalazione errore?
017900130708     d   PosCurCOD                     n   overlay(IndDspF : 50)
018000130708     d   PosCurDES                     n   overlay(IndDspF : 51)
018100130702     d   PosCurUNI                     n   overlay(IndDspF : 52)
018200130702     d   PosCurPAR                     n   overlay(IndDspF : 53)
018300130702     d   PosCurFUN                     n   overlay(IndDspF : 54)
018400130702     d   PosCurPLI                     n   overlay(IndDspF : 55)
018500130702     d   PosCurPLE                     n   overlay(IndDspF : 56)
018600130702     d   PosCurDTINI                   n   overlay(IndDspF : 57)
018700130702     d   PosCurDTFIN                   n   overlay(IndDspF : 58)
018800130702     d   PosCurDIP                     n   overlay(IndDspF : 59)
018900130702     d   PosCurSOC                     n   overlay(IndDspF : 60)
019000130702     d   PosCurFIL                     n   overlay(IndDspF : 61)
019100130705     d   PosCurRNT01                   n   overlay(IndDspF : 62)
019200130705     d   PosCurRNT02                   n   overlay(IndDspF : 63)
019300130705     d   PosCurFILw1                   n   overlay(IndDspF : 71)
019400130702         // -?Riemissione videata?
019500130702     d   ErrGenerico                   n   overlay(IndDspF : 99)
019600130704
019700130704       // -?Date decorrenza e fine attività?
019800130704     d ds_DtAtt        ds                  inz
019900130704     d   V1DtIni                           inz
020000130704     d   V1DtFin                           inz
020100130702
020200130702       // -?Codice dipendente del commerciale e relativa società?
020300130702     d ds_DIP          ds                  inz
020400130702     d   V1Cdip                            inz
020500130702     d   V1Csoc                            inz
020600130702
020700130702       // -?Descrizione commerciale e relativo indirizzo e-mail?
020800130709     d ds_NTC          ds                  inz
020900130705     d   V1Crnt01                          inz
021000130705     d   V1Crnt02                          inz
021100141217
021200141217       // -?Ridefinizione elenco librerie in elaborare le tabelle?
021300141217     d ds_Libr         ds                  inz
021400141217     d   $Libr                       10    dim(c_NrLibr) inz
021500141217
021600141217       // -?Parametri ricevuti?
021700141217     d KPJBA         e ds
021800141217     d prm_AZCMM_ds  e ds                  extname(AZCMM00F)
021900141217     d TRMK41ds      e ds                  inz
022000141217
022100141217       // -?Tabelle usate:?
022200141217       // ·?"dFUN" = Funzione aziendale?
022300141217     d dFUN          e ds                  inz
022400141217
022500141217       // ·?"dHTM" = Default per testo HTML?
022600141217     d dHTM          e ds                  inz
022700141217       // ·?"01" = Agenti Commerciali?
022800141217     d ds01          e ds                  inz
022900141217     d   §01dipAlfa                   5    overlay(ds01 : 39)
023000141217     d Wds01         e ds                  extname(ds01)
023100141217     d                                     prefix(W)
023200141217     d                                     inz
023300141217
023400141217       // -?Descrizioni dell'organigramma:?
023500141217     d Og143         e ds                  inz  qualified
023600141217
023700141217       // -?Dati record principale della tabella?
023800141217     d TNTBEds       e ds                  inz  extname(TNTBE00F)
023900141217     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
024000141217     d                                          prefix(TBX : 3)
024100130702
024200130702       //--------------------------------------------------------------
024300130702       //?Definizione variabili globali.                               ?
024400130702       //--------------------------------------------------------------
024500130702
024600130702       // -?Flags booleani?
024700130702     d $Fine           s               n   inz
024800130704     d $Forza1         s               n   inz
024900130704     d $Forza2         s               n   inz
025000130702     d $InzD01         s               n   inz(*on)
025100130702     d $InzW01         s               n   inz(*on)
025200130703     d $InzW02         s               n   inz(*on)
025300130702
025400130702       // -?Indici di schiera?
025500130702     d xx              s              5  0 inz
025600130702
025700130702       // -?Variabili per la gestione del video?
025800130702     d $Video          s              2    inz('D1')
025900130702     d Save_dsp_aid    s                   like(dsp_aid)  inz
026000130702
026100130702       // -?Controllo cambiamenti tra i dati gestiti a video:?
026200130708       // ·?Codice commerciale?
026300130702     d SaveUNI         s                   like(V1Cuni)   inz(*hival)
026400130704       // ·?Particolarità?
026500130704     d SavePAR         s                   like(V1Cpar)   inz(*hival)
026600130704       // ·?Funzione aziendale?
026700130704     d SaveFUN         s                   like(V1Cfun)   inz
026800130704       // ·?Date decorrenza e scadenza attività?
026900130704     d SaveDtAtt       s                   like(ds_DtAtt) inz(*zero)
027000130702       // ·?Codice dipendente?
027100130702     d SaveDIP         s                   like(ds_Dip)   inz
027200130704       // ·?Filiale di appartenenza
027300130704     d SaveFIL         s                   like(V1Cfil)   inz
027400130702       // ·?Descrizione commerciale e relativo indirizzo e-mail?
027500130709     d SaveNTC         s                   like(ds_NTC)   inz
027600130703
027700130703       // -?Nuovo codice commerciale in cui copiare i dati?
027800130708     d wNEWcmm         s                   like(CMMcod)  inz
027900130703
028000130703       // -?Progressivo per nuovo codice commerciale (xxx.yNNN)?
028100130703     d wProgr          s              3  0 inz
028200141212
028300141212       // -?Numero totale di Commerciali inseriti in AZCMM?
028400141212     d wTotCmm         s              5  0 inz
028500130703
028600130703       // -?Codice abilitazioni utente?
028700130703     d wAbi            s                   like(UTEaut)  inz
028800130702
028900130702       // -?Campi di comodo?
029000130709     d wNTCrnt01       s                   like(NTCrnt)  inz
029100130709     d wNTCrnt02       s                   like(NTCrnt)  inz
029200130708     d wFil            s                   like(CMMfil)  inz
029300170330     d wFilD           s                   like(ORGdes)  inz
029400130702     d wDate           s              8  0 inz
029500130702     d wDataIni        s              8  0 inz
029600130702     d wDataFin        s              8  0 inz
029700130705     d wDate_ISO       s               d   inz  datfmt(*iso)
029800141217     d w_iSystem       s              1  0 inz
029900141217     d w_SisInf        s              3  0 inz
030000141217
030100141217       // -?Nome del sistema?
030200141217     d currSysNeta     s              8a   inz
030300141217
030400141217       // -?Nomi estesi dei file da aprire (di Sede, NON di Filiale)?
030500141217     d wTABEL          s             21    inz('GAITRAGRU /TABEL00F  ')
030600141217     d wTNTBE          s             21    inz('GAITRAGRU /TNTBE01L  ')
030700130702
030800130702       //--------------------------------------------------------------
030900130702       //?Definizione prototipi procedure.                             ?
031000130702       //--------------------------------------------------------------
031100130702
031200130702       // -?Reperimento dati utente?
031300130702     d TIBS34ds      e ds                  inz
031400130702      /copy gaitrasrc/srcProtoPR,TIBS34R
031500130703       // -?Flags Abilitazioni utenti aziendali?
031600130703     d dUte01        e ds                  inz
031700130705
031800130705       // -?Verifica se commerciale disattivabile?
031900130705     d TRMK42ds      e ds                  inz
032000130705      /copy gaitrasrc/srcProtoPr,TRMK42R
032100130703
032200130703       // -?Caricamento filiali gestite?
032300130703     d TRUL31ds      e ds                  inz
032400130703     d   $POg                 10    759    inz   dim(250)
032500130703      /copy gaitrasrc/srcProtoPr,TRUL31R
032600141212
032700141212       // -?Controllo n° record (commerciali) inseriti?
032800141212     d TRUL0Sds      e ds                  inz
032900141212      /copy gaitrasrc/srcProtoPr,TRUL0SR
033000130702
033100130702       // -?Controllo formale / Inversione data?
033200130702     d WLBdat          ds                  inz
033300130702     d   G02dat                1      8  0 inz
033400130702     d   G02inv                9     16  0 inz
033500130702     d   G02err               17     17    inz('3')
033600130702     d   G02tgi               18     22  0 inz
033700130702      /copy gaitrasrc/srcProtoPR,XSRDA8
033800130702
033900130702       // -?Ricerca/Controllo tabelle (TNTBE)?
034000130702     d TIBS02ds      e ds                  inz
034100130702     d   T02mod      e                     inz('R')
034200130702     d   T02cod      e                     inz('FUN')
034300130702      /copy gaitrasrc/srcProtoPR,TIBS02R
034400130702
034500130702       // -?Reperimento dati tabelle?
034600130704      *///copy gaitrasrc/srcProtoPI,TRULTAB
034700130704      *///copy gaitrasrc/srcProtoPR,TRULTAB
034800130704
034900130704       // -?Reperimento dati Anagrafica Società?
035000130704      /copy gaitrasrc/srcConst,TIBSSOCR
035100130705     d*//prmRqsOpCode    s             10a
035200130705     d*//prmRpyOpCode    s             10a
035300130705     d*//prmRpyIdMsg     s             10i 0
035400130705     d*//prmRqsFormato   s             10a
035500130705     d*//prmRqsDataSize  s             10i 0
035600130705     d*//prmRpyFormato   s             10a
035700130705     d*//prmRpyDataSize  s             10i 0
035800130704     d tibsSocI0     e ds                  qualified  inz
035900130704     d tibsSocO0     e ds                  qualified  inz
036000130705      /Define   DFTACTGRP_NO
036100130704      /copy gaitrasrc/SrcProtoPR,TIBSSOCR
036200130705      /UnDefine DFTACTGRP_NO
036300130704
036400130704       // -?Reperimento dati Anagrafica Dipendenti?
036500130704     d TRMZ47ds      e ds                  inz
036600130704     d trmz47r         pr                  extpgm('TRMZ47R')
036700130704     d   kpjba                             likeds(KPJBA)
036800130702
036900130702       // -?Controllo formale indirizzi e-mail?
037000130702     d dsEMAIL       e ds                  inz
037100130702      /copy gaitrasrc/srcProtoPR,XEMAIL
037200141217
037300141217       // -?Reperimento NETA sistema AS/400 corrente?
037400141217      /copy gaitrasrc/srcProtoPR,UBRTVNETA
037500130702
037600130702       // -?Parametri API QCAPCMD (Process Commands)?
037700130702     d Qcmd            s           2048    inz  varying
037800130702      /copy qSysInc/qRpgleSrc,QCAPCMD
037900130702       // -?API QCAPCMD (Process Commands)?
038000130702      /copy gaitrasrc/srcProtoPR,QCAPCMD
038100130702
038200130702       // -?Parametri gestione errori API.?
038300130702      /copy qsysinc/qrpglesrc,QUSEC
038400130702
038500130702       //--------------------------------------------------------------
038600130702       //?Definizione key-list.                                        ?
038700130702       //--------------------------------------------------------------
038800130702
038900130702       // -?File AZSOC01L: Anagrafica Società?
039000130702     d k01azsoc01    e ds                  extname(AZSOC01L : *key)
039100130702     d                                     prefix(k_)   inz
039200130702
039300130702       // -?File AZDAN01L: Anagrafica Dipendenti?
039400130702     d k02azdan01    e ds                  extname(AZDAN01L : *key)
039500130702     d                                     prefix(k_)   inz
039600130702
039700130708       // -?File AZCMM01L: Anagrafica Commerciali?
039800130708     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
039900130702     d                                     prefix(k_)   inz
040000130702
040100130709       // -?File AZNTC01L: Note/Rubrica Commerciali?
040200130709     d k02azntc01    e ds                  extname(AZNTC01L : *key)
040300130702     d                                     prefix(k_)   inz
040400141217
040500141217       // -?File TABEL00F (tab. "01")?
040600141217     d keyTABEL00    e ds                  extname(TABEL00F : *key)
040700141217     d                                     prefix(k_)   inz
040800141217
040900141217       // -?File TNTBE01L (tab. "HTM")?
041000141217     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
041100141217     d                                     prefix(k_)   inz
041200130702
041300130702       //--------------------------------------------------------------
041400130702       //?M A I N - L I N E                                            ?
041500130702       //--------------------------------------------------------------
041600130702
041700130702     c     *Entry        plist
041800130702     c                   parm                    KPJBA
041900130708     c                   parm                    prm_AZCMM_ds
042000130709     c*//                parm                    prm_AZNTC01_ds
042100130709     c*//                parm                    prm_AZNTC02_ds
042200130702
042300130702      /free
042400130702
042500130702       // -?Operazioni iniziali?
042600130702       exsr sr_RoutInz;
042700130702
042800130702       // -?Gestione videate?
042900130702       DoW  $Fine = *off;
043000130702
043100130702         select;
043200130702
043300130702           // -?Gestione videata D1 (gestione dati del commerciale)?
043400130702           when $Video = 'D1';
043500130702             exsr sr_GesD01;
043600130705
043700130705           // -?Gestione window W1 (richiesta filiale per cui copiare)?
043800130705           when $Video = 'W1';
043900130705             exsr sr_GesW01;
044000130702
044100130702           // -?? ? ? ? ??
044200130702           other;
044300130702             $Fine = *on;
044400130702
044500130702         endsl;
044600130702
044700130702       EndDo;
044800130702
044900130702       // -?Operazioni finali?
045000130702       exsr sr_RoutEnd;
045100130702
045200130702       //--------------------------------------------------------------
045300130702       //?Operazioni iniziali.                                         ?
045400130702       //--------------------------------------------------------------
045500130702       BEGSR  sr_RoutInz;
045600130702
045700130702         *inLR = *on;
045800141217
045900141217         // -?Verifica del sistema AS/400 corrente?
046000141217         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
046100141217           $Fine = *on;
046200141217           leavesr;
046300141217         endif;
046400141217
046500141217         // -?Impostazione elenco librerie in cui gestire le tabelle?
046600141217         //  ?(a seconda del sistema in cui si stà lavorando)?
046700141217         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
046800141217         if  w_iSystem = *zero;
046900141217           $Fine = *on;
047000141217           leavesr;
047100141217         endif;
047200141217
047300130702
047400130702         // -?Reperimento dati job?
047500130702         exsr  sr_DatiJob;
047600130711         // -?Se ho errori nei dati utente esco dal pgm?
047700130711         if  DUTerr = 'E';
047800130711           $Fine = *on;
047900130711           leavesr;
048000130711         endif;
048100130702
048200130702         // -?Reperimento data odierna?
048300130702         wDate = %int( %subst( %char( %dec( %timestamp() ) )
048400130702                               : 1 : 8 ) );
048500130705         wDate_ISO = %date();
048600130702
048700130702         // -?Impostazione nome programma a video?
048800130702         V1Tpgm = SDSpgm;
048900141217
049000141217
049100141217         // -?Apertura archivio TABEL00F del s.i. di SEDE?
049200141217         w_SisInf = 1;
049300141217         exsr  sr_OpenFileTab;
049400141217
049500141217         // -?Aggancio dati generali della tabella "HTM"?
049600141217         clear  xTNTBEds;
049700141217         clear  keyTNTBE01;
049800141217         k_TBEke1 = *zero;
049900141217         %subst( k_TBEke1 : %len(k_TBEke1) - 3 + 1 : 3 ) = 'HTM';
050000141217         chain(n)  %kds(keyTNTBE01)  TNTBE000;
050100141217         if  %found(TNTBE01L)  and  TBEatb = *blank;
050200141217           xTNTBEds = TNTBEds;
050300141217         endif;
050400141217
050500141217         // -?Impostazione campi "fissi"?
050600141217         k_TBLkut = c_Kut;
050700141217         k_TBLcod = c_Tab;
050800141217         k_TBEcod = 'HTM';
050900141217         k_TBEke2 = 'CC';
051000141217
051100130702
051200130702         // -?Impostazione parametri ricevuti?
051300130702         TRMK41ds = KPJBU;
051400130702
051500130702         // -?Pulizia eventuali parametri di output (se ricevuti)?
051600130704         oMK41err = *off;
051700130704         clear  oMK41msg;
051800130702         if  %parms() > 1;
051900130708           clear  prm_AZCMM_ds;
052000130709           //clear  prm_AZNTC01_ds;
052100130709           //clear  prm_AZNTC02_ds;
052200130702         endif;
052300130702
052400130702         // -?Decodifica opzione richiesta?
052500130702         xx = %lookup( iMK41opz : $Opz );
052600130702         if  xx > *zero;
052700130702           V1Topz = $OpzD(xx);
052800130702         else;
052900130702           oMK41err = *on;
053000130702           oMK41msg = $Msg(01);
053100130704           $Fine  = *on;
053200130702           leavesr;
053300130702         endif;
053400130703
053500130703         // -?Controlli relativi alla Disattivazione?
053600130703         If  iMK41opz = 4;
053700130704           exsr  sr_CallTRMK42;
053800130703           if  ErrGenerico;
053900130703             $Fine = *on;
054000130703             leavesr;
054100130703           endif;
054200130703         EndIf;
054300130702
054400130702         // -?Caricamento commerciali?
054500130702         clear  xx;
054600130708         clear  $Cmm;
054700130708         clear  $CmmDes;
054800130708         clear  $CmmUni;
054900130708         setll  (*loval)  AZCMM000;
055000130708         read(n)  AZCMM000;
055100130708         DoW  Not %eof(AZCMM01L);
055200130708           If  CMMdtFin > wDate;
055300130702             xx  += 1;
055400130708             $Cmm(xx)    = CMMcod;
055500130708             $CmmDes(xx) = CMMdes;
055600130708             $CmmUni(xx) = CMMuni;
055700130702           EndIf;
055800130708           read(n)  AZCMM000;
055900130702         EndDo;
056000141212         wTotCmm = xx;
056100130705
056200130705         // -?Impostazione videata?
056300130705         if  iMK41opz = 3;
056400130705           $Video  = 'W1';
056500130705           $InzW01 = *on;
056600130705         else;
056700130705           $Video  = 'D1';
056800130705           $InzD01 = *on;
056900130705         endif;
057000130702
057100130702       ENDSR;
057200130702
057300130702       //--------------------------------------------------------------
057400130702       //?Reperimento Dati del job (Utente/Operativi).                 ?
057500130702       //--------------------------------------------------------------
057600130702       BEGSR  sr_DatiJob;
057700130702
057800130702         in(e) §AzUte;
057900130702         if NOT %error;
058000130702           in(e) §DatiUte;
058100130702         endif;
058200130702         if %error or RSut = *blank;
058300130702           tibs34r ( tibs34ds );
058400130702           in §AzUte;
058500130702           in §DatiUte;
058600130702         endif;
058700130702
058800130702       ENDSR;
058900130703
059000130703       //--------------------------------------------------------------
059100130703       //?Controllo se il commerciale è disattivabile.                 ?
059200130703       //--------------------------------------------------------------
059300130705       BEGSR  sr_CallTRMK42;
059400130704
059500130704         reset  TRMK42ds;
059600130708         iMK42cmm = iMK41cmm;
059700130704         kpjbu = TRMK42ds;
059800130704
059900130704         trmk42r ( kpjba );
060000130704
060100130704         TRMK42ds = kpjbu;
060200130704         if  oMK42err <> *off;
060300130704           oMK41err = *on;
060400130704           oMK41msg = oMK42msg;
060500130704           ErrGenerico = *on;
060600130704         endif;
060700130703
060800130703       ENDSR;
060900130703
061000130703       //--------------------------------------------------------------
061100130703       //?Richiesta del codice filiale in cui copiare il commerciale.  ?
061200130703       //--------------------------------------------------------------
061300130703       BEGSR  sr_GesW01;
061400130703
061500130703         // -?Inizializzazione videata (window)?
061600130703         if  $InzW01 = *on;
061700130703           exsr  sr_InzW01;
061800130703           if  $Fine = *on;
061900130703             leavesr;
062000130703           endif;
062100130703           $InzW01 = *off;
062200130703         endif;
062300130703
062400130705         // -?Emissione testata (window)?
062500130705         write  MK41WT1;
062600130703
062700130705         // -?Emissione videata?
062800130703         exfmt  MK41W01;
062900130703
063000130703         clear  VIDmsg;
063100130703         reset  ErrMessage;
063200130703         reset  ErrGenerico;
063300130703
063400130703         SELECT;
063500130703
063600130703           // -?F3=Fine?
063700130703           WHEN  dsp_aid = c_F03;
063800130703             oMK41fxx = '03';
063900130703             $Fine = *on;
064000130703             leavesr;
064100130703
064200130703           // -?F12=Ritorno?
064300130703           WHEN  dsp_aid = c_F12;
064400130703             oMK41fxx = '12';
064500130703             $Fine = *on;
064600130703             leavesr;
064700130703
064800130703           // -?Invio / F6=Conferma?
064900130703           OTHER;
065000130703             exsr  sr_CtrW01;
065100130703             // -?Impostazione dati e passaggio alla videata D01?
065200130703             If  NOT  ErrGenerico   and   dsp_aid = c_F06;
065300130703               $InzD01 = *on;
065400130703               $Video  = 'D1';
065500130703             EndIf;
065600130703
065700130703         ENDSL;
065800130703
065900130703       ENDSR;
066000130703
066100130703       //--------------------------------------------------------------
066200130703       //?Inizializzazione window W01.                                 ?
066300130703       //--------------------------------------------------------------
066400130703       BEGSR  sr_InzW01;
066500130703
066600130703         // -?Caricamento filiali gestibili dall'utente?
066700130703         clear  wAbi;
066800130703         select;
066900130703           // -?Se ho errori nei dati utente esco dal pgm?
067000130711           //  ?(controllo già eseguito nella subr. sr_RoutInz)?
067100130711           //when  DUTerr = 'E';
067200130711           //  $Fine = *on;
067300130703           // -?Se c'è l'abilitazione: prendo quella?
067400130703           when  UTEaut <> *blank;
067500130703             dUTE01 = UTEfaf;
067600130703             if  §UTEgtc <> *blank;
067700130703               wAbi = §UTEpot;
067800130703             else;
067900130703               wAbi = UTEaut;
068000130703             endif;
068100130703           // -?se sede: abilitazione aziendale?
068200130703           when  DUTlpo =  'S';
068300130703             wAbi = 'AZ';
068400130703           // -?se 1° livello: abilitazioni al terminal?
068500130703           when  DUTlpo =  '1';
068600130703             wAbi = 'TP';
068700130703           // -?se 2° livello: abilitazioni alla filiale?
068800130703           when  DUTlpo =  '2';
068900130703             wAbi = 'PO';
069000130703         endsl;
069100170331         clear  TRUL31ds;
069200170331         If  DUTlpo <> 'S';
069300170331           I31abi = wAbi;
069400170331           I31cpo = DUTpou;
069500170331           trul31r ( KPJBA : TRUL31ds );
069600170331           if  O31pog <= *zero;
069700170331             oMK41err = *on;
069800170331             oMK41msg = $Msg(27);
069900170331             $Fine = *on;
070000170331           endif;
070100170331         EndIf;
070200130705
070300130705         Select;
070400130705
070500130705           // -?Codice commerciale errato (xxx.x999)?
070600130708           When  %subst( %editc( iMK41cmm : 'X' ) : 5 : 3 ) = *all'9';
070700130705             oMK41err = *on;
070800130705             oMK41msg = $Msg(28);
070900130705             $Fine = *on;
071000130705
071100130705           // -?Codice commerciale di filiale NON gestibile dall'utente?
071200130705           //  ?(se di filiale)?
071300170331           When  DUTlpo <> 'S'   and
071400170331                 %lookup ( %subst( %editc( iMK41cmm : 'X' )
071500130705                           : 1 : 3 ) : $POg ) = *zero;
071600130705             oMK41err = *on;
071700130705             oMK41msg = $Msg(29);
071800130705             exsr  sr_RoutEnd;
071900130705
072000130705         EndSl;
072100130703
072200130703         // -?Impostazione campi a video?
072300130712         clear  MK41W01;
072400130712
072500130708         chain(N)  (iMK41cmm)  AZCMM000;
072600130705
072700130708         W1Ccmm = iMK41cmm;
072800130705
072900130705         Select;
073000130705           // -?Codice commerciale NON reperito?
073100130708           When  Not %found(AZCMM01L);
073200130705             //W1Dcmm = *all'? ';
073300130705             oMK41err = *on;
073400130705             oMK41msg = $Msg(02);
073500130705             exsr  sr_RoutEnd;
073600130705           // -?Codice commerciale NON unificante?
073700130708           when  iMK41cmm <> CMMuni;
073800130705             oMK41err = *on;
073900130705             oMK41msg = $Msg(30);
074000130705             exsr  sr_RoutEnd;
074100130705           Other;
074200130708             W1Dcmm = CMMdes;
074300130705         EndSl;
074400130703
074500130703         // -?Intabellamento dati dei commerciali con unificante W1CCMM?
074600130704         exsr  sr_Intab_Fil_Cmm;
074700130703
074800130703       ENDSR;
074900130703
075000130703       //--------------------------------------------------------------
075100130703       //?Controllo dati window W01.                                   ?
075200130703       //--------------------------------------------------------------
075300130703       BEGSR  sr_CtrW01;
075400130703
075500170330         clear  W1Dfil;
075600170330
075700130703         // -?Codice filiale obbligatorio?
075800130703         if  W1Cfil = *zero;
075900130712           VIDmsg = $Msg(31);
076000130703           PosCurFILw1 = *on;
076100130703           ErrMessage  = *on;
076200130703           ErrGenerico = *on;
076300130703           leavesr;
076400130703         endif;
076500130703
076600130703         W1Cfil = %abs(W1Cfil);
076700130703
076800130703         // -?Filiale errata?
076900170330         wFIL   = W1Cfil;
077000170330         exsr  sr_CtrlFIL;
077100170330         W1Dfil = wFILd;
077200170330         if  ErrMessage  = *on;
077300170330           VIDmsg = $Msg(32);
077400170330           PosCurFILw1 = *on;
077500170330           PosCurFIL   = *off;
077600170330           leavesr;
077700170330         endif;
077800130703
077900130703         // -?Unificante già presente per la filiale in cui copiare?
078000130703         xx = %lookup( W1Cfil : $Cmu_Fil );
078100130703         if  xx > *zero;
078200130712           VIDmsg = %trimr($Msg(07)) + ' '
078300130712                  + %subst( %editc( W1Cfil : 'X' ) : 1 : 3 )
078400130712                  + ': ' + $Cmu_Cmm(xx);
078500130703           PosCurFILw1 = *on;
078600130703           ErrMessage  = *on;
078700130703           ErrGenerico = *on;
078800130703           leavesr;
078900130703         endif;
079000130703
079100130703         // -?Ricerca nuovo codice commerciale?
079200130703         wProgr = 1;
079300130708         k_CMMcod = ( W1Cfil * 10000 ) +
079400130705                    ( %int( %subst( %editc( W1Ccmm : 'X' )
079500130705                                    : 4 : 1 ) ) * 1000 ) +
079600130705                    wProgr;
079700130708         setll  %kds(k01azcmm01)  AZCMM000;
079800130708         read(N)  AZCMM000;
079900130708         Dow  Not %eof(AZCMM01L)   and
080000130708              %subst( %trim( %editc( CMMcod : 'X' ) ) : 1 : 4 ) =
080100130708              %subst( %trim( %editc( k_CMMcod : 'X' ) ) : 1 : 4 );
080200130708           if  wProgr <> %int( %subst( %trim( %editc( CMMcod : 'X' ) )
080300130703                                       : 5 : 3 ) );
080400130708             //wNEWcmm = %int( %subst( %trim( %editc( CMMcod : 'X' ) )
080500130703             //                        : 1 : 4 ) ) * 1000 + wProgr;
080600130703             leave;
080700130703           endif;
080800130703           if  wProgr = *hival;
080900130712             VIDmsg = $Msg(33);
081000130703             PosCurFIL   = *on;
081100130703             ErrMessage  = *on;
081200130703             ErrGenerico = *on;
081300130703             leavesr;
081400130703           endif;
081500130703           wProgr += 1;
081600130708           read  AZCMM000;
081700130703         EndDo;
081800130703
081900130708         wNEWcmm = %int( %subst( %trim( %editc( k_CMMcod : 'X' ) )
082000130703                                 : 1 : 4 ) ) * 1000 + wProgr;
082100130703         $Video  = 'D1';
082200130703         $InzD01 = *on;
082300130703
082400130703       ENDSR;
082500130702
082600130702       //--------------------------------------------------------------
082700130702       //?Gestione videata D01.                                        ?
082800130702       //--------------------------------------------------------------
082900130702       BEGSR  sr_GesD01;
083000130702
083100130702         // -?Inizializzazione videata?
083200130702         if  $InzD01 = *on;
083300130702           exsr  sr_InzD01;
083400130702           $InzD01 = *off;
083500130702         endif;
083600130702
083700130702         // -?(Ri)Abilitazione tasti funzionali?
083800130702         exsr  sr_AbilFxxD01;
083900130702
084000130702         // -?Emissione Testata e Piede?
084100130702         write  MK41T01;
084200130702         write  MK41P01;
084300130702
084400130702         // -?Emissione videata?
084500130702         if  iMK41opz <> 5;
084600130702           exfmt  MK41D01;
084700130702         else;
084800130702           write  MK41D01;
084900130702           exfmt  Protect;
085000130702         endif;
085100130702
085200130702         clear  VIDmsg;
085300130702         reset  ErrMessage;
085400130702         reset  ErrGenerico;
085500130702
085600130702         SELECT;
085700130702
085800130702           // -?F3=Fine?
085900130702           WHEN  dsp_aid = c_F03;
086000130708             unlock  AZCMM01L;
086100130709             unlock  AZNTC01L;
086200130702             oMK41fxx = '03';
086300130702             $Fine  = *on;
086400130702
086500130702           // -?F12=Ritorno?
086600130702           WHEN  dsp_aid = c_F12;
086700130708             unlock  AZCMM01L;
086800130709             unlock  AZNTC01L;
086900130702             oMK41fxx = '02';
087000130702             $Fine = *on;
087100130702
087200130702           // -?Invio in Visualizzazione?
087300130702           //WHEN  iMK41opz  = 5   and
087400130702           //      dsp_aid = c_Enter;
087500130702           //  $Fine = *on;
087600130702
087700130703           // -?Invio / F6=Aggiornamento?
087800130702           OTHER;
087900130702             // -?Controlli NON eseguiti SE richiesta Disabilitazione?
088000130702             if  iMK41opz = 4;
088100130719               //exsr  sr_CallTRMK42;    ?- già eseguito in sr_RoutInz?
088200130702             else;
088300130702               exsr  sr_CtrD01;
088400130702             endif;
088500130702             if  ErrGenerico = *on;
088600130702               leavesr;
088700130702             endif;
088800130702             // -?Verifica se modificati dei dati allineabili in tutta?
088900130702             //  ?la famiglia del commerciale (SE UNIFICANTE)?
089000130702             If  dsp_aid = c_F06  and
089100130702                 (iMK41opz = *zero  or  iMK41opz = 2   or
089200130705                                        iMK41opz = 3)  and
089300130708                 V1Ccod  = V1Cuni;
089400130705               $InzW02 = *on;
089500130703               exsr  sr_GesW02;
089600130702               if  ErrGenerico = *on  or  dsp_aid = c_F12;
089700130702                 leavesr;
089800130702               endif;
089900130702             EndIf;
090000130702             // -?Aggiornamento?
090100130702             If  dsp_aid = c_F06;
090200130702               exsr  sr_UpdateAll;
090300130702               if  ErrGenerico = *on;
090400130702                 leavesr;
090500130702               endif;
090600130702               // ·?Uscita dal programma?
090700130702               $Fine = *on;
090800130702             EndIf;
090900130702
091000130702         ENDSL;
091100130702
091200130702       ENDSR;
091300130702
091400130702       //--------------------------------------------------------------
091500130702       //?Inizializzazione videata D01.                                ?
091600130702       //--------------------------------------------------------------
091700130702       BEGSR  sr_InzD01;
091800130702
091900130702         clear  MK41D01;
092000130705         clear  $Forza1;
092100130705         clear  $Forza2;
092200130705         clear  Save_dsp_aid;
092300130702
092400130702         // -?Impostazioni di protezione:?
092500130702         ProtectDtIni = *on;
092600130702         Select;
092700130702           // ·?Inserimento: protezione nulla?
092800130702           When  iMK41opz = *zero;
092900130702             ProtectKey   = *off;
093000130702             ProtectFld   = *off;
093100130702             ProtectDtIni = *off;
093200130702           // ·?Modifica e Riabilitazione: protezione campi chiave?
093300130702           //  ?(e data inizio attività, MA si veda anche dopo...)?
093400141217           When  iMK41opz = 2  or  iMK41opz = 7;
093500130702             ProtectKey   = *on;
093600130702             //ProtectDtIni = *on;      ?(già così)?
093700130702           // ·?Copia: protezione altri campi?
093800130702           //  ?(tranne la data inizio attività)?
093900130702           When  iMK41opz = 3;
094000130702             ProtectFld   = *on;
094100130702             ProtectDtIni = *off;
094200130702           // ·?Disabilitazione e Visualizzazione: protezione totale?
094300130702           //When  iMK41opz = 4  or  iMK41opz = 5;
094400130702           Other;
094500130702             ProtectKey = *on;
094600130702             ProtectFld = *on;
094700130705             //ProtectDtIni = *on;      ?(già così)?
094800130702         EndSl;
094900130702
095000130702         //*//  ?Utente EDP: data sempre sprotetta?
095100130702         //*if  %subst(Knmus:1:3) = 'EDP';
095200130702         //*  ProtectDtIni = *off;
095300130702         //*endif;
095400130702
095500130704         //  ?Utente EDP: Filiale Appartenenza SEMPRE visualizzata?
095600130702         if  %subst(Knmus:1:3) = 'EDP';
095700130704           VisualFIL = *on;
095800130702         endif;
095900130702
096000130704
096100130708         // -?Reperimento dati commerciale?
096200130708         k_CMMcod = iMK41cmm;
096300130702         Select;
096400130702           // ·?Inserimento: non reperisce nessun dato?
096500130702           When  iMK41opz = *zero;
096600130702             V1DtIni = *Date;
096700130702             V1DtFin = c_MaxDate_Eur;
096800130702             leavesr;
096900130702           // ·?Copia o Visualizzazione?
097000130702           When  iMK41opz = 3  or  iMK41opz = 5;
097100130708             chain(N)  %kds(k01azcmm01)  AZCMM000;
097200130702           // ·?Modifica, Annullamento o Ripristino?
097300130702           Other;
097400130708             chain(E)  %kds(k01azcmm01)  AZCMM000;
097500130702         EndSl;
097600130704
097700130702
097800130702         select;
097900130702           // ·?Record NON trovato?
098000130708           when  Not %found(AZCMM01L);
098100130702             $Fine = *on;
098200130705             oMK41err = *on;
098300130705             oMK41msg = $Msg(02);
098400130702             exsr  sr_RoutEnd;
098500130702           // ·?Record allocato?
098600130702           when  iMK41opz <> 3  and  iMK41opz <> 5  and  %error();
098700130702             $Fine = *on;
098800130705             oMK41err = *on;
098900130705             oMK41msg = $Msg(03);
099000130702             exsr  sr_RoutEnd;
099100130702         endsl;
099200130704
099300130702
099400130702         // -?Sprotezione Data Inizio Attività?
099500130702         //  ?SE  modifica  &  Data Inizio Attività > *date?
099600130708         if  iMK41opz = 2  and  CMMDtIni > wDate;
099700130705           ProtectDtIni = *off;
099800130702         endif;
099900130704
100000130702
100100130709         // -?Reperimento dati in file AZNTC?
100200130709         clear  wNTCrnt01;
100300130709         clear  wNTCrnt02;
100400130709         k_NTCcmm = iMK41cmm;
100500130704
100600130709         k_NTCtnt = '01';
100700130702         Select;
100800130704           // ·?Immissione?
100900130702           When  iMK41opz = *zero;
101000130704           // ·?Copia o Visualizzazione?
101100130702           When  iMK41opz = 3  or  iMK41opz = 5;
101200130709             chain(N)  %kds(k02azntc01)  AZNTC000;
101300130704           // ·?Modifica, Disattivazione o Riattivazione?
101400130702           Other;
101500130709             chain  %kds(k02azntC01)  AZNTC000;
101600130702         EndSl;
101700130709         if  iMK41opz > *zero  and  %found(AZNTC01L);
101800130709           wNTCrnt01 = NTCrnt;
101900130702         endif;
102000130702
102100130709         k_NTCtnt = '02';
102200130702         Select;
102300130704           // ·?Immissione?
102400130702           When  iMK41opz = *zero;
102500130704           // ·?Copia o Visualizzazione?
102600130702           When  iMK41opz = 3  or  iMK41opz = 5;
102700130709             chain(N)  %kds(k02azntc01)  AZNTC000;
102800130704           // ·?Modifica, Disattivazione o Riattivazione?
102900130702           Other;
103000130709             chain  %kds(k02azntc01)  AZNTC000;
103100130702         EndSl;
103200130709         if  iMK41opz > *zero  and  %found(AZNTC01L);
103300130709           wNTCrnt02 = NTCrnt;
103400130702         endif;
103500130702
103600130704
103700130702         // -?Impostazioni campi a video?
103800130703         if  iMK41opz = 3;
103900130708           V1Ccod = wNEWcmm;      // -?Cod. Commerciale?
104000130708           //V1Cfil = W1Cfil;       // -?Fil. Appartenenza?
104100130702         else;
104200130708           V1Ccod = CMMcod;       // -?Cod. Commerciale?
104300130708           V1Cpli = CMMpli;       // -?% di lavoro Italia?
104400130708           V1Cple = CMMple;       // -?% di lavoro Estero?
104500130708           //V1Cfil = CMMfil;       // -?Fil. Appartenenza?
104600130702         endif;
104700130702         // -?Descrizione?
104800130708         V1Cdes = CMMdes;
104900130702         // -?Commerciale unificante?
105000130708         V1Cuni = CMMuni;
105100130702         // -?Particolarità di Attività?
105200130708         V1Cpar = CMMpar;
105300130704         // -?Funzione aziendale?
105400130708         V1Cfun = CMMfun;
105500130702         // -?Data decorrenza attività?
105600130702         Select;
105700130705           // ·?Copia: si imposta la data odierna (*Date)?
105800130702           When  iMK41opz = 3;
105900130704             V1DtIni = *date;
106000130702           // ·?Altrimenti si visualizza la data presente?
106100130708           When  CMMdtIni > *zero;
106200130702             reset  WLBdat;
106300130708             G02inv  = CMMdtIni;
106400130702             xsrda8 ( WLBdat );
106500130702             V1DtIni = G02dat;
106600130702         EndSl;
106700130702         // -?Data fine attività?
106800130708         Select;
106900130708         //  // ·?Copia: si imposta *Max?
107000130708         //  When  iMK41opz = 3;
107100130708         //    V1DtFin = c_MaxDate_Eur;
107200130708         //  // ·?Disabilitazione: si imposta la data odierna (*Date)?
107300130708         //  When  iMK41opz = 4;
107400130708         //    V1DtFin = *Date;
107500130708         //  // ·?Riabilitazione: si imposta *Max?
107600130708         //  When  iMK41opz = 7;
107700130708         //    V1DtFin = c_MaxDate_Eur;
107800130702           // ·?Altrimenti si visualizza la data presente?
107900130708           When  CMMdtFin > *zero;
108000130702             reset  WLBdat;
108100130708             G02inv  = CMMdtFin;
108200130702             xsrda8 ( WLBdat );
108300130702             V1DtFin = G02dat;
108400130702         EndSl;
108500130704         // -?Dipendente: Codice e Società?
108600130708         if  CMMdip > *zero;
108700130708           V1Cdip = CMMdip;
108800130708           V1Csoc = CMMsoc;
108900130704         endif;
109000130708
109100130704         // -?Data inserimento?
109200130704         reset  WLBdat;
109300130708         G02inv  = CMMdtIns;
109400130704         xsrda8 ( WLBdat );
109500130704         V1DtIns = G02dat;
109600130704         // -?Data ultima modifica?
109700130704         reset  WLBdat;
109800130708         G02inv  = CMMdtMod;
109900130704         xsrda8 ( WLBdat );
110000130704         V1DtMod = G02dat;
110100130704         // -?Utente Inserimento/Ultima modifica?
110200130708         V1Dute = CMMute;
110300130702
110400130709         // -?Dati da note "01" e "02" di AZNTC00F?
110500130709         V1Crnt01 = wNTCrnt01;
110600130709         V1Crnt02 = wNTCrnt02;
110700130702
110800130702         // -?Decodifica commerciale unificante?
110900130705         exsr  sr_CtrlUNI;
111000130702         reset  ErrMessage;
111100130702         reset  ErrGenerico;
111200130702         // -?Decodifica funzione aziendale?
111300130705         exsr  sr_CheckFUN;
111400130702         reset  ErrMessage;
111500130702         reset  ErrGenerico;
111600130708         //// -?Decodifica codice del commerciale (dipendente)?
111700130708         //exsr  sr_CtrlDIP;
111800130708         //reset  ErrMessage;
111900130708         //reset  ErrGenerico;
112000130705         // -?Decodifica società del commerciale (dipendente)?
112100130705         exsr  sr_CtrlSOC;
112200130705         reset  ErrMessage;
112300130705         reset  ErrGenerico;
112400130702         // -?Decodifica filiale appartenenza?
112500130702         clear V1Dfil;
112600130704         If  V1Cfil > *zero;
112700130704           chain  (V1Cfil)  AZORG;
112800130704           if  %found(AZORG01L);
112900130702             V1Dfil = ORGdes;
113000130704           endif;
113100130704         EndIf;
113200130702
113300130702         clear  VIDmsg;
113400130702         %subst(IndDspF : 50) = *off;
113500130702
113600130702         // -?Salvataggio dati per verificare eventuali modifiche?
113700130704         reset  SaveUNI;
113800130704         reset  SavePAR;
113900130702         SaveFUN   = V1Cfun;
114000130702         SaveDIP   = ds_DIP;
114100130702         SaveDtAtt = ds_DtAtt;
114200130709         SaveNTC   = ds_NTC;
114300130702         SaveFIL   = V1Cfil;
114400130702
114500130702       ENDSR;
114600130702
114700130702       //--------------------------------------------------------------
114800130702       //?Abilitazione tasti funzionali in P01 (per D01).              ?
114900130702       //--------------------------------------------------------------
115000130702       BEGSR  sr_AbilFxxD01;
115100130702
115200130702         // ->?F3 = Fine?
115300130702         F3Attivo = *on;
115400130702
115500130702         // ->?F6 = Conferma?
115600130703         F6Attivo = (iMK41opz <> 5);
115700130702
115800130702         // ->?F12 = Ritorno?
115900130702         F12Attivo = *on;
116000130702
116100130702       ENDSR;
116200130702
116300130702       //--------------------------------------------------------------
116400130702       //?Controllo dati videata D01.                                  ?
116500130702       //--------------------------------------------------------------
116600130702       BEGSR  sr_CtrD01;
116700130702
116800130704         // -?Spegnimento indicatori di errore?
116900130702         %subst(IndDspF : 50) = *off;
117000130702
117100130702         // -?Controllo codice commerciale?
117200130702         //  ?(SOLO SE inserimento o copia)?
117300130703         If  iMK41opz = *zero  or  iMK41opz = 3;
117400130708           exsr  sr_CtrlCMM;
117500130702           if  ErrGenerico;
117600130702             leavesr;
117700130702           endif;
117800130702         EndIf;
117900130702
118000130705         // -?Ragione sociale commerciale obbligatoria?
118100130708         If  V1Cdes = *blank;
118200130704           VIDmsg = $Msg(08);
118300130708           PosCurDES   = *on;
118400130702           ErrMessage  = *on;
118500130702           ErrGenerico = *on;
118600130702           leavesr;
118700130702         EndIf;
118800130702
118900130702         // -?Controllo codice commerciale unificante?
119000130705         exsr  sr_CtrlUNI;
119100130702         if  ErrGenerico;
119200130702           leavesr;
119300130702         endif;
119400130702
119500130702         // -?Controllo congruenza particolarità con codice?
119600130705         exsr  sr_CtrlPAR;
119700130702         if  ErrGenerico;
119800130702           leavesr;
119900130702         endif;
120000130702
120100130702         // -?Controllo funzione aziendale?
120200130705         exsr  sr_CtrlFUN;
120300130702         if  ErrGenerico;
120400130702           leavesr;
120500130702         endif;
120600130702
120700130702         // -?% di lavoro Italia obbligatoria?
120800130702         //  ?La percentuale DEVE essere positiva.?
120900130702         //  ?Non si sa perchè, ma può superare il 100% quindi?
121000130702         //  ?si controlla solo il valore positivo?
121100130704         If  V1Cpli < *zero;
121200130704           VIDmsg = $Msg(16);
121300130705           PosCurPLI   = *on;
121400130702           ErrMessage  = *on;
121500130702           ErrGenerico = *on;
121600130702           leavesr;
121700130702         EndIf;
121800130702
121900130702         // -?% di lavoro Estero obbligatoria?
122000130702         //  ?(questa può anche essere *zero, ma NON negativa)?
122100130704         If  V1Cple < *zero;
122200130704           VIDmsg = $Msg(16);
122300130705           PosCurPLE   = *on;
122400130702           ErrMessage  = *on;
122500130702           ErrGenerico = *on;
122600130702           leavesr;
122700130702         EndIf;
122800130704
122900130704         // -?Controllo data decorrenza attività?
123000130705         clear  wDataIni;
123100130704         If  V1DtIni <> *zero;
123200130704           clear  WLBdat;
123300130704           G02dat = V1DtIni;
123400130704           xsrda8 ( WLBdat );
123500130704           if  G02err = *on;
123600130704             VIDmsg = $Msg(22);
123700130704             PosCurDtINI = *on;
123800130704             ErrMessage  = *on;
123900130704             ErrGenerico = *on;
124000130704             leavesr;
124100130704           endif;
124200130704           V1dtIni  = G02dat;
124300130704           wDataIni = G02inv;
124400130704         EndIf;
124500130704
124600130704         if  iMK41opz = *zero  or  iMK41opz = 2
124700130704                               or  iMK41opz = 3;
124800130704           select;
124900130704             // -?Obbligatoria?
125000130705             when  V1DtIni = *zero;
125100130704               VIDmsg = $Msg(23);
125200130704               PosCurDtINI = *on;
125300130704               ErrMessage  = *on;
125400130704               ErrGenerico = *on;
125500130704               leavesr;
125600130704             // -?In Immissione/Copia: NON può essere PRECEDENTE?
125700130705             when  wDataIni < wDate  and
125800130704                  (iMK41opz = *zero  or  iMK41opz = 3);
125900130704               VIDmsg = $Msg(24);
126000130704               PosCurDtINI = *on;
126100130704               ErrMessage  = *on;
126200130704               ErrGenerico = *on;
126300130704               leavesr;
126400130704             // -?In Manutenzione (SE futura): NON può essese PRECEDENTE?
126500130705             when  wDataIni <  wDate  and
126600130704                   iMK41opz =  2      and
126700130708                   CMMDtIni >= wDate  and
126800130708                   wDataIni <> CMMDtIni;
126900130704               VIDmsg = $Msg(24);
127000130704               PosCurDtINI = *on;
127100130704               ErrMessage  = *on;
127200130704               ErrGenerico = *on;
127300130704               leavesr;
127400130704             // -?In Manutenzione (modificata): può essere solo FUTURA?
127500130705             //  ?(in realtà il campo V1DtIni dovrebbe essere protetto)?
127600130705             when  wDataIni <  wDate  and
127700130704                   iMK41opz =  2      and
127800130708                   wDataIni <> CMMDtIni;
127900130704               VIDmsg = $Msg(25);
128000130704               PosCurDtINI = *on;
128100130704               ErrMessage  = *on;
128200130704               ErrGenerico = *on;
128300130704               leavesr;
128400130704           endsl;
128500130704         endif;
128600130704
128700130704         // -?Controllo data scadenza attività?
128800130704         clear  wDataFin;
128900130704         If  V1DtFin <> *zero;
129000130704           clear  WLBdat;
129100130704           G02dat = V1DtFin;
129200130704           xsrda8 ( WLBdat );
129300130704           if  G02err = *on;
129400130704             VIDmsg = $Msg(22);
129500130704             PosCurDtFIN = *on;
129600130704             ErrMessage  = *on;
129700130704             ErrGenerico = *on;
129800130704             leavesr;
129900130704           endif;
130000130704           V1DtFin  = G02dat;
130100130704           wDataFin = G02inv;
130200130704         EndIf;
130300130704
130400130704         // -?Data decorrenza > Data scadenza?
130500130704         If  wDataIni > wDataFin   and   wDataFin > *zero;
130600130704           VIDmsg = $Msg(26);
130700130704           PosCurDtINI = *on;
130800130704           ErrMessage  = *on;
130900130704           ErrGenerico = *on;
131000130704           leavesr;
131100130704         endif;
131200130704
131300130704         // -?Codice e Società dipendente?
131400130708         //clear  V1Ddip;
131500130704         clear  V1Dsoc;
131600130702         Select;
131700130702           When  V1Cdip <> *zero  and  V1Csoc =  *blank;
131800130704             VIDmsg = $Msg(17);
131900130702             PosCurSOC   = *on;
132000130702             ErrMessage  = *on;
132100130702             ErrGenerico = *on;
132200130702             leavesr;
132300130702           When  V1Cdip  = *zero  and  V1Csoc <> *blank;
132400130704             VIDmsg = $Msg(18);
132500130702             PosCurDIP   = *on;
132600130702             ErrMessage  = *on;
132700130702             ErrGenerico = *on;
132800130702             leavesr;
132900130702         EndSl;
133000130704
133100130708         //// -?Controllo/Decodifica Dipendente?
133200130708         //exsr  sr_CtrlDIP;
133300130708         //if  ErrGenerico;
133400130708         //  leavesr;
133500130708         //endif;
133600130704
133700130704         // -?Controllo/Decodifica Società?
133800130705         exsr  sr_CtrlSOC;
133900130705         if  ErrGenerico;
134000130705           leavesr;
134100130705         endif;
134200130704
134300130704         // -?Filiale appartenenza?
134400170330         wFIL   = V1Cfil;
134500130705         exsr  sr_CtrlFIL;
134600170330         V1Dfil = wFILd;
134700130705         if  ErrGenerico;
134800130705           leavesr;
134900130705         endif;
135000130704
135100130702         // -?Dati per testi HTML: descrizione ed indirizzo e-mail?
135200130702         //  ?(non obbligatori se particolarità impostata?
135300130702         Select;
135400130705           When  V1Crnt01 = *blank  and  V1Cpar = *blank;
135500130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
135600130704                 //                 and
135700130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
135800130704             VIDmsg = $Msg(08);
135900130705             PosCurRNT01 = *on;
136000130702             ErrMessage  = *on;
136100130702             ErrGenerico = *on;
136200130702             leavesr;
136300130705           When  V1Crnt02 = *blank  and  V1Cpar   = *blank;
136400130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
136500130704                 //                 and
136600130708                 //%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
136700130704             VIDmsg = $Msg(21);
136800130705             PosCurRNT02 = *on;
136900130702             ErrMessage  = *on;
137000130702             ErrGenerico = *on;
137100130702             leavesr;
137200130702         EndSl;
137300130702
137400130702         // -?Controllo validità indirizzo e-mail?
137500130705         If  V1Crnt02 <> *blank;
137600130702           clear  dsEMAIL;
137700130705           §EMLindI = %trimr(V1Crnt02) + c_atBrt;
137800130702           xemail ( dsEMAIL );
137900130702           if  §EMLerrO <> *off;
138000130704             //VIDmsg = $Msg(21);
138100130702             VIDmsg = §EMLmsgO;
138200130705             PosCurRNT02 = *on;
138300130702             ErrMessage  = *on;
138400130702             ErrGenerico = *on;
138500130702             leavesr;
138600130702           endif;
138700130702         EndIf;
138800130702
138900130702       ENDSR;
139000130702
139100130702       //--------------------------------------------------------------
139200130703       //?Gestione window W02.                                         ?
139300130702       //?(Richiesta di allineamento variazioni su tutta la "famiglia")?
139400130702       //--------------------------------------------------------------
139500130703       BEGSR  sr_GesW02;
139600130702
139700130702         // -?Verifica se emettere la window per richiedere?
139800130702         //  ?l'allineamento delle modifiche a tutti i commerciali?
139900130702         //  ?della famiglia (con lo stesso unificante)?
140000130702         select;
140100130711           // -?Inserimento di un nuovo unificante:?
140200130711           //  ?> Nessun allineamento richiesto <?
140300130711           when  (iMK41opz = *zero  or  iMK41opz = 3)  and
140400130711                 V1Ccod = V1Cuni;
140500130711             clear  MK41W02;
140600130711             //$Video = 'D1';         ?(ancora/già così)?
140700130702           // -?Variata la Funzione aziendale?
140800130702           when  V1Cfun   <> SaveFUN;
140900130703             $Video = 'W2';
141000130704           // -?Variate le Date decorrenza e/o fine attività?
141100130704           when  ds_DtAtt <> SaveDtAtt;
141200130704             $Video = 'W2';
141300130702           // -?Variato il Dipendente?
141400130702           when  ds_DIP   <> SaveDIP;
141500130703             $Video = 'W2';
141600130704           // -?Variata la Filiale di Appartenenza
141700130704           when  V1Cfil   <> SaveFIL;
141800130704             $Video = 'W2';
141900130704           // -?Variati i Dati per testi HTML?
142000130709           when  ds_NTC   <> SaveNTC;
142100130704             $Video = 'W2';
142200130702           // -?> Nessun allineamento <?
142300130702           other;
142400130711             clear  MK41W02;
142500130702             //$Video = 'D1';         ?(ancora/già così)?
142600130702         endsl;
142700130702
142800130703         if  $Video <> 'W2';
142900130702           leavesr;
143000130702         endif;
143100130702
143200130703         if  $InzW02;
143300130702           Save_dsp_aid = dsp_aid;
143400130705           clear  MK41W02;
143500130703           W02agg  = 'SI';
143600130703           $InzW02 = *off;
143700130702         endif;
143800130702
143900130703         exfmt  MK41W02;
144000130702
144100130702         Select;
144200130702
144300130702           // -?Invio => Prosegue con l'aggiornamento (F6)?
144400130702           When  dsp_aid = c_Enter;
144500130702             dsp_aid = Save_dsp_aid;
144600130702
144700130702           //  ?F12   => Ritorna alla videata "D1"?
144800130702           When  dsp_aid = c_F12;
144900130702             ErrGenerico = *on;
145000130702             $Video = 'D1';
145100130702             leavesr;
145200130702
145300130702         EndSl;
145400130702
145500130702       ENDSR;
145600130703
145700130703       //--------------------------------------------------------------
145800130708       //?Controllo del codice Commerciale (SOLO IN INSERIMENTO)       ?
145900130703       //--------------------------------------------------------------
146000130708       BEGSR  sr_CtrlCMM;
146100130704
146200130703         // -?Codice obbligatorio?
146300130708         if  V1Ccod <= *zero;
146400130703           VIDmsg = $Msg(04);
146500130708           PosCurCOD   = *on;
146600130703           ErrMessage  = *on;
146700130703           ErrGenerico = *on;
146800130703           leavesr;
146900130703         endif;
147000130703
147100130708         // -?Controllo (in)esistenza in AZCMM?
147200130708         setll  (V1Ccod)  AZCMM000;
147300130708         if  %equal(AZCMM01L);
147400130703           VIDmsg = $Msg(05);
147500130708           PosCurCOD   = *on;
147600130703           ErrMessage  = *on;
147700130703           ErrGenerico = *on;
147800130703           leavesr;
147900130703         endif;
148000130703
148100130703         // -?Controllo primi 3 bytes del codice:?
148200170330         //  ?devono essere una filiale (anche Logistica)?
148300170330         clear  Og143;
148400130708         wFIL = V1Ccod / 10000;
148500170330         if  wFIL = *zero;
148600130703           VIDmsg = $Msg(06);
148700130708           PosCurCOD   = *on;
148800130703           ErrMessage  = *on;
148900130703           ErrGenerico = *on;
149000130703           leavesr;
149100170330         else;
149200170330           exsr  sr_CtrlFIL;
149300170330           if  ErrGenerico = *on;
149400170330             PosCurCOD = *on;
149500170330             PosCurFIL = *off;
149600170330             leavesr;
149700170330           endif;
149800130703         endif;
149900130703
150000130704         // -?Intabellamento dati dei commerciali con unificante V1CUNI?
150100130704         If  V1Cuni <> SaveUNI;
150200130704           SaveUNI = V1Cuni;
150300130712           W1Ccmm  = V1Cuni;
150400130704           exsr  sr_Intab_Fil_Cmm;
150500130704         EndIf;
150600130703
150700130703         // -?Unificante già presente per la filiale in cui?
150800130703         //  ?inserire/copiare?
150900130708         wFIL = V1Ccod / 10000;
151000130704         xx = %lookup( wFIL : $Cmu_Fil );
151100130703         if  xx > *zero;
151200130704           VIDmsg = %trimr($Msg(07)) + ' '
151300130708                  + %subst( %editc( V1Ccod : 'X' ) : 1 : 3 )
151400130703                  + ': ' + $Cmu_Cmm(xx);
151500130708           PosCurCOD   = *on;
151600130703           ErrMessage  = *on;
151700130703           ErrGenerico = *on;
151800130703           leavesr;
151900130703         endif;
152000130703
152100130703       ENDSR;
152200130703
152300130703       //--------------------------------------------------------------
152400130703       //?Controllo del Commerciale Unificante                         ?
152500130703       //--------------------------------------------------------------
152600130705       BEGSR  sr_CtrlUNI;
152700130703
152800130704         clear  V1Duni;
152900130703
153000130703         Select;
153100130703
153200130703           // -?Codice unificante obbligatorio?
153300130704           When  V1Cuni = *zero;
153400130704             VIDmsg = $Msg(09);
153500130704             PosCurUNI   = *on;
153600130703             ErrMessage  = *on;
153700130703             ErrGenerico = *on;
153800130703             leavesr;
153900130703
154000130703           // -?Codice unificante errato?
154100130708           When  V1Cuni <> V1Ccod;
154200130708             xx = %lookup( V1Cuni : $Cmm );
154300130703             if  xx = *zero;
154400130704               VIDmsg = $Msg(10);
154500130704               PosCurUNI   = *on;
154600130703               ErrMessage  = *on;
154700130703               ErrGenerico = *on;
154800130703               leavesr;
154900130703             endif;
155000130708             V1Duni = $CmmDes(xx);
155100130705
155200130705           // -?Codice unificante se stesso?
155300130705           Other;
155400130705             select;
155500130705               when  iMK41opz = *zero  or  iMK41opz = 3;
155600130708                 V1Duni = V1Cdes;
155700130705               other;
155800130708                 V1Duni = CMMdes;
155900130705             endsl;
156000130703
156100130703         EndSl;
156200130703
156300130703       ENDSR;
156400130703
156500130703       //--------------------------------------------------------------
156600130703       //?Controllo congruenza Particolarità codice comm.le                    ?
156700130703       //--------------------------------------------------------------
156800130705       BEGSR  sr_CtrlPAR;
156900130704
157000130704         select;
157100130704
157200130704           When  V1Cpar = SavePAR;
157300130704
157400130704           When  V1Cpar <> *blank  and
157500130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
157600130704                                   and
157700130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
157800130704             SavePAR = V1Cpar;
157900130704             if  not $Forza1;
158000130704               VIDmsg = $Msg(11);
158100130704               PosCurPAR   = *on;
158200130704               ErrMessage  = *on;
158300130704               ErrGenerico = *on;
158400130705               $Forza1 = *on;
158500130704               leavesr;
158600130704             endif;
158700130703
158800130704           When  V1Cpar = *blank   and
158900130708                 (%subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) = '0000'
159000130704                                   or
159100130708                  %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) = '0999');
159200130704             SavePAR = V1Cpar;
159300130704             if  not $Forza2;
159400130704               VIDmsg = $Msg(12);
159500130704               PosCurPAR   = *on;
159600130704               ErrMessage  = *on;
159700130704               ErrGenerico = *on;
159800130705               $Forza2 = *on;
159900130704               leavesr;
160000130704             endif;
160100130704
160200130704         EndSl;
160300130703
160400130703       ENDSR;
160500130703
160600130703       //--------------------------------------------------------------
160700130703       //?Controllo della Funzione Aziendale                           ?
160800130703       //--------------------------------------------------------------
160900130705       BEGSR  sr_CtrlFUN;
161000130703
161100130703         clear  V1Dfun;
161200130703         clear  dFUN;
161300130703
161400130703         Select;
161500130703
161600130703           // -?Obbligatoria?
161700130703           //  ?(NON lo è se "fittizia" - 0000-0999)?
161800130703           When  V1Cfun = *blank  and
161900130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0000'
162000130703                                  and
162100130708                 %subst( %editc( V1Ccod : 'X' ) : 4 : 4 ) <> '0999';
162200130704             VIDmsg = $Msg(13);
162300130703             PosCurFUN   = *on;
162400130703             ErrMessage  = *on;
162500130703             ErrGenerico = *on;
162600130703             leavesr;
162700130703
162800130703           // -?Ricerca?
162900130703           When  %scan( '?' : V1Cfun ) > *zero;
163000130703             reset  TIBS02ds;
163100130703             T02mod = 'R';
163200130703             T02sif = knsif;
163300130703             TNTBE_RicercaControllo (kpjba : TIBS02ds);
163400130703             if  T02err = *blank;
163500130703               dFUN   = T02uni;
163600130703               V1Cfun = T02ke1;
163700130703               V1Dfun = §FUNdes;
163800130703             endif;
163900130703             ErrGenerico = *on;
164000130703             leavesr;
164100130703
164200130703           // -?Controllo?
164300130703           When  V1Cfun <> *blank;
164400130705             exsr  sr_CheckFUN;
164500130703             if  ErrGenerico;
164600130703               leavesr;
164700130703             endif;
164800130703
164900130703         EndSl;
165000130703
165100130703       ENDSR;
165200130704
165300130704       //--------------------------------------------------------------
165400130705       //?Decodifica della Funzione Aziendale                           ?
165500130704       //--------------------------------------------------------------
165600130705       BEGSR  sr_CheckFUN;
165700130704
165800130704         clear  dFUN;
165900130704
166000130704         reset  TIBS02ds;
166100130704         T02mod = 'C';
166200130704         T02sif = knsif;
166300130704         T02ke1 = V1Cfun;
166400130704
166500130704         TNTBE_RicercaControllo (kpjba : TIBS02ds);
166600130704
166700130704         if  T02err <> *blank;
166800130704           VIDmsg = $Msg(14);
166900130704           PosCurFUN   = *on;
167000130704           ErrMessage  = *on;
167100130704           ErrGenerico = *on;
167200130704           leavesr;
167300130704         endif;
167400130704
167500130704         dFUN = T02uni;
167600130704
167700130704         V1Dfun = §FUNdes;
167800130704
167900130704       ENDSR;
168000130705
168100130705       //--------------------------------------------------------------
168200130705       //?Controllo Codice del dipendente                              ?
168300130705       //--------------------------------------------------------------
168400130708       //BEGSR  sr_CtrlDIP;
168500130708       //
168600130708       //  clear  V1Ddip;
168700130708       //
168800130708       //  // -?Società NON immessa?
168900130708       //  if  V1Cdip = *zero;
169000130708       //    leavesr;
169100130708       //  endif;
169200130708       //
169300130708       //  clear  TRMZ47ds;
169400130708       //
169500130708       //  D47soc = V1Csoc;
169600130708       //  D47mat = V1Cdip;
169700130708       //  kpjbu = TRMZ47ds;
169800130708       //
169900130708       //  trmz47r ( kpjba );
170000130708       //
170100130708       //  TRMZ47ds = kpjbu;
170200130708       //  If  D47errO <> *blank;
170300130708       //
170400130708       //    V1Ddip = *all'? ';
170500130708       //    VIDmsg = $Msg(20);
170600130708       //    PosCurDIP   = *on;
170700130708       //    ErrMessage  = *on;
170800130708       //    ErrGenerico = *on;
170900130708       //    leavesr;
171000130708       //
171100130708       //  Else;
171200130708       //
171300130708       //    V1Ddip = %trim(D47cogO) + ' ' + %trim(D47nomO);
171400130708       //
171500130708       //  EndIf;
171600130708       //
171700130708       //ENDSR;
171800130705
171900130705       //--------------------------------------------------------------
172000130705       //?Controllo Società del dipendente                             ?
172100130705       //--------------------------------------------------------------
172200130705       BEGSR  sr_CtrlSOC;
172300130705
172400130705         clear  V1Dsoc;
172500130705
172600130705         // -?Società NON immessa?
172700130705         if  V1Csoc = *blank;
172800130705           leavesr;
172900130705         endif;
173000130705
173100130705         // -?Apertura del programma.?
173200130705         Societa_Init();
173300130705
173400130705         // -?Decodifica.?
173500130705         clear  tibsSOCo0;
173600130705         if  Societa_NewSocieta ( V1Csoc ) >= *zero;
173700130705           tibsSOCo0 = Societa_GetAnagrafica ( 'TIBSSOCO0' );
173800130705         endif;
173900130705
174000130705         // -?Chiusura del programma.?
174100130705         Societa_Finalize();
174200130705
174300130705         // -?Valorizzazione descrizione a video.?
174400130705         If  tibsSOCo0.IdSocieta <> *blank;
174500130708           //V1Dsoc = tibsSOCo0.RagSocBrev;
174600130705         Else;
174700130705           V1Dsoc = *all'? ';
174800130705           VIDmsg = $Msg(19);
174900130705           PosCurSOC   = *on;
175000130705           ErrMessage  = *on;
175100130705           ErrGenerico = *on;
175200130705           leavesr;
175300130705         EndIf;
175400130705
175500130705       ENDSR;
175600130703
175700130703       //--------------------------------------------------------------
175800130705       //?Controllo Filiale di apprtenenza                             ?
175900130703       //--------------------------------------------------------------
176000130705       BEGSR  sr_CtrlFIL;
176100130703
176200130704         clear Og143;
176300170330         clear wFILd;
176400130705
176500130705         // -?Filiale NON immessa?
176600170330         if  wFIL = *zero;
176700130705           leavesr;
176800130705         endif;
176900130704
177000130703         // -?Controllo?
177100170330         ORGfil = wFIL;
177200130703         chain  (ORGfil)  AZORG;
177300130704
177400130704         If  not %found(AZORG01L);
177500130704           VIDmsg = $Msg(15);
177600130704           PosCurFIL   = *on;
177700130703           ErrMessage  = *on;
177800130703           ErrGenerico = *on;
177900130703           leavesr;
178000130704         EndIf;
178100130704
178200170330         wFILd  = ORGdes;
178300170330         Og143  = ORGde3;
178400170330
178500170330         If  ORGfva <> *blank  or  (
178600170330             ORGfag <> 'A'    and  ORGfag <> 'F'  and
178700170330             Og143.§OGntw <> 'LOG' );
178800130704           VIDmsg = $Msg(15);
178900130704           PosCurFIL   = *on;
179000130703           ErrMessage  = *on;
179100130703           ErrGenerico = *on;
179200130703           leavesr;
179300130704         EndIf;
179400130703
179500130703       ENDSR;
179600130704
179700130704       //--------------------------------------------------------------
179800130704       //?Intabellamento Filiali e 1° Comm.le per Comm.le Unificante   ?
179900130704       //--------------------------------------------------------------
180000130704       BEGSR  sr_Intab_FIL_CMM;
180100130704
180200130704         clear  xx;
180300130704         clear  $Cmu_Fil;
180400130704         clear  $Cmu_Cmm;
180500130704
180600130708         setll  (*loval)  AZCMM000;
180700130708         read(N)  AZCMM000;
180800130704
180900130708         DoW  Not  %eof(AZCMM01L);
181000130704
181100130708           wFIL = CMMcod / 10000;
181200130704
181300130712           if  CMMuni = W1Ccmm   and
181400130704               %lookup( wFIL : $CmU_Fil ) = *zero;
181500130704
181600130704             xx += 1;
181700130704             $Cmu_Fil(xx) = wFIL;
181800130708             $Cmu_Cmm(xx) = %editc( CMMcod : 'X' );
181900130708                        //+ '-' + CMMdes;
182000130704           endif;
182100130704
182200130708           read(N)  AZCMM000;
182300130704
182400130704         EndDo;
182500130704
182600130704       ENDSR;
182700130702
182800130702       //--------------------------------------------------------------
182900130702       //?Aggiornamento/Inserimento record nel file TABEL00F.          ?
183000130702       //--------------------------------------------------------------
183100130702       BEGSR  sr_UpdateAll;
183200141218
183300141218         // -?Aggiornamento Tabelle "01" = Commerciali?
183400141218         //                     ?e "HTM" = Note/Rubrica?
183500141218         exsr  sr_UpdTab01eHTM;
183600130702
183700130702         // -?Aggiornamento Note/Rubrica Commerciali?
183800130709         exsr  sr_UpdAZNTC;
183900130702
184000130702         // -?Aggiornamento Anagrafica Commerciali?
184100130708         exsr  sr_UpdAZCMM;
184200130702
184300130704         // -?Aggiornamento Anagrafica del resto della famiglia?
184400130703         if  $Video = 'W2'  and  W02agg = 'SI';
184500130702           exsr  sr_UpdFAMIGLIA;
184600130702         endif;
184700141212
184800141212         // -?Verifica n° commerciali inseriti?
184900141212         //  ?(ci sono *pgm che intabellano i commerciali in schiere di?
185000141212         //  ?5.000 elementi)?
185100141212         if  (iMK41opz = *zero  or  iMK41opz = 3  or  iMK41opz = 7)  and
185200141212             wTotCmm >= c_Max_Cmm_W;
185300141212           reset  TRUL0Sds;
185400141212           i0Stla  = 'L';
185500141212           i0Sski  = 'AGENTI COMM.LI  ';
185600141212           i0Sfile = 'AZCMM00F  ';
185700141212           i0Sele  = c_Max_Cmm_W;
185800141212           i0Spie  = wTotCmm + 1;
185900141212           i0Spgm  = SDSpgm;
186000141212           i0Ssif  = KNSIF;
186100141212           i0Sute  = SDSusr;
186200141212           kpjbu   = TRUL0Sds;
186300141212           trul0Sr ( kpjba );
186400141212         endif;
186500130702
186600130702       ENDSR;
186700130702
186800130702       //--------------------------------------------------------------
186900130709       //?Aggiornamento record nel file AZNTC00F                       ?
187000130702       //--------------------------------------------------------------
187100130709       BEGSR  sr_UpdAZNTC;
187200130702
187300130708         // -?NOMINATIVO commerciale (tnt "01")?
187400130709         k_NTCcmm = V1Ccod;
187500130709         k_NTCtnt = '01';
187600130709         chain  %kds(k02azntc01)  AZNTC000;
187700130702
187800130710         // -?Reperito record da cancellare o?
187900130710         //  ?record da inserire / aggiornare?
188000130709         IF  (%found(AZNTC01L)  and  iMK41opz =  4)  or
188100130704                                     iMK41opz <> 4;
188200130702
188300130702           // -?Non reperito record da scrivere o aggiornare?
188400130702           //  ?=> impostazione dei campi chiave?
188500130710           if  Not %found(AZNTC01L)  and  iMK41opz <> 4;
188600130709             clear  AZNTC000;
188700130709             NTCcmm = k_NTCcmm;
188800130709             NTCtnt = k_NTCtnt;
188900130702           endif;
189000130702
189100130702           // -?Impostazione campi nel record?
189200130709           NTCrnt = V1Crnt01;
189300130702
189400130702           // -?AGGIORNAMENTO?
189500130710           Select;
189600130710             When  iMK41opz = 4;
189700130710               if  %found(AZNTC01L);
189800130710                 //_______________
189900130710                 delete  AZNTC000;
190000130710                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
190100130710               endif;
190200130723             When  %found(AZNTC01L)      and  NTCrnt =  *blank;
190300130723               //_______________
190400130723               delete  AZNTC000;
190500130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
190600130723             When  %found(AZNTC01L)      and  NTCrnt <> *blank;
190700130723               //_______________
190800130723               update  AZNTC000;
190900130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191000130723             When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
191100130710               //_______________
191200130710               write   AZNTC000;
191300130710               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191400130710           EndSl;
191500130702
191600130702         ENDIF;
191700130702
191800130708         // -?E-MAIL commerciale (tnt "02")?
191900130709         k_NTCcmm = V1Ccod;
192000130709         k_NTCtnt = '02';
192100130709         chain  %kds(k02azntc01)  AZNTC000;
192200130702
192300130710         // -?Reperito record da cancellare o?
192400130710         //  ?record da inserire / aggiornare?
192500130709         IF  (%found(AZNTC01L)  and  iMK41opz =  4)  or
192600130704                                     iMK41opz <> 4;
192700130702
192800130702           // -?Non reperito record da scrivere o aggiornare?
192900130702           //  ?=> impostazione dei campi chiave?
193000130710           if  Not %found(AZNTC01L)  and  iMK41opz <> 4;
193100130709             clear  AZNTC000;
193200130709             NTCcmm = k_NTCcmm;
193300130709             NTCtnt = k_NTCtnt;
193400130702           endif;
193500130702
193600130702           // -?Impostazione campi nel record?
193700130709           NTCrnt = V1Crnt02;
193800130702
193900130702           // -?AGGIORNAMENTO?
194000130710           Select;
194100130710             When  iMK41opz = 4;
194200130710               if  %found(AZNTC01L);
194300130710                 //_______________
194400130710                 delete  AZNTC000;
194500130710                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
194600130710               endif;
194700130723             When  %found(AZNTC01L)      and  NTCrnt =  *blank;
194800130723               //_______________
194900130723               delete  AZNTC000;
195000130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
195100130723             When  %found(AZNTC01L)      and  NTCrnt <> *blank;
195200130723               //_______________
195300130723               update  AZNTC000;
195400130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
195500130723             When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
195600130723               //_______________
195700130723               write   AZNTC000;
195800130723               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
195900130710           EndSl;
196000130702
196100130702         ENDIF;
196200130702
196300130702       ENDSR;
196400130702
196500130702       //--------------------------------------------------------------
196600130708       //?Aggiornamento record nel file AZCMM00F.                      ?
196700130702       //--------------------------------------------------------------
196800130708       BEGSR  sr_UpdAZCMM;
196900130702
197000130708         k_CMMcod = V1Ccod;
197100130708         chain  %kds(k01azcmm01)  AZCMM000;
197200130702
197300130702         // -?Non reperito record da annullare => uscire?
197400130709         if  Not %found(AZCMM01L)  and  iMK41opz =  4;
197500130702           leavesr;
197600130702         endif;
197700130702
197800130702         // -?Non reperito record da scrivere o aggiornare?
197900130702         //  ?=> impostazione dei campi chiave?
198000130710         if  Not %found(AZCMM01L)  and  iMK41opz <> 4;
198100130708           clear  AZCMM000;
198200130708           CMMcod = k_CMMcod;
198300130702         endif;
198400130702
198500130702         // -?Impostazione campi nel record?
198600130710         If  iMK41opz <> 4;
198700130708           CMMdes   = V1Cdes;
198800130708           CMMpar   = V1Cpar;
198900130708           CMMfun   = V1Cfun;
199000130708           CMMpli   = V1Cpli;
199100130708           CMMple   = V1Cple;
199200130708           CMMdtIni = wDataIni;
199300130708           CMMdtFin = wDataFin;
199400130708           CMMdip   = V1Cdip;
199500130708           CMMsoc   = V1Csoc;
199600130708           if  V1Cfil > *zero;
199700130708             CMMfil = V1Cfil;
199800130705           else;
199900130708             clear  CMMfil;
200000130705           endif;
200100130708           //clear  CMMfil10;        ?- Per ora: NON utilizzato?
200200130708           CMMuni   = V1Cuni;
200300130710           if  Not %found(AZCMM01L);
200400130710             CMMdtIns = wDate;
200500130710           else;
200600130710             CMMdtMod = wDate;
200700130710           endif;
200800130710           CMMute   = KNMUS;
200900130710         EndIf;
201000130702
201100130702         // -?AGGIORNAMENTO?
201200130710         Select;
201300130710           When  iMK41opz = 4;
201400130710             if  %found(AZCMM01L);
201500130710               //_______________
201600130710               delete  AZCMM000;
201700130710               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
201800130710             endif;
201900130710           When  Not %found(AZCMM01L);
202000130710             //_______________
202100130710             write   AZCMM000;
202200130710             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
202300130710           Other;
202400130710             //_______________
202500130710             update  AZCMM000;
202600130710             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
202700130710         EndSl;
202800130702
202900130702       ENDSR;
203000130702
203100130702       //--------------------------------------------------------------
203200130709       //?Aggiornamento records nei file AZCMM00F e AZNTC00F           ?
203300130702       //?per tutti i codici con lo stesso commerciale unificante      ?
203400130702       //--------------------------------------------------------------
203500130702       BEGSR  sr_UpdFAMIGLIA;
203600130702
203700130708         xx = %lookup( V1Cuni : $CmmUni );
203800130702
203900130702         DoW  xx > *zero;
204000130702
204100130708           If  $Cmm(xx) <> V1Ccod;
204200130702
204300130708             // -?Aggiornamento dati anarafici in AZCMM00F?
204400141218             If  V1Cfun   <> SaveFUN    or    //?Funzione aziendale?
204500141218                 ds_DtAtt <> SaveDtAtt  or    //?Date start & end attività?
204600141218                 ds_DIP   <> SaveDIP    or    //?Dipendente?
204700141218                 V1Cfil   <> SaveFIL    or    //?Filiale appartenenza?
204800141218                 ds_NTC   <> SaveNTC;         //?Descriz. & e-Mail comm.le?
204900130702
205000130708               chain  ($Cmm(xx))  AZCMM000;
205100130708               if  %found(AZCMM01L);
205200130702
205300130704                 if  V1Cfun <> SaveFUN;
205400130708                   CMMfun = V1Cfun;
205500130704                 endif;
205600130704                 if  ds_DtAtt <> SaveDtAtt;
205700130708                   CMMdtIni = wDataIni;
205800130708                   CMMdtFin = wDataFin;
205900130704                 endif;
206000130702                 if  ds_DIP <> SaveDIP;
206100130708                   CMMdip = V1Cdip;
206200130708                   CMMsoc = V1Csoc;
206300130702                 endif;
206400130704                 if  V1Cfil <> SaveFIL;
206500130708                   CMMfil = V1Cfil;
206600130702                 endif;
206700130719
206800130719                 CMMdtMod = wDate;
206900130719                 CMMute   = KNMUS;
207000130704
207100130704                 //_______________
207200130708                 update  AZCMM000;
207300130704                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
207400130702
207500130702               endif;
207600130702
207700130702             EndIf;
207800130702
207900130709             // -?Aggiornamento Note/Rubrica in AZNTC00F?
208000141218             If  ds_NTC <> SaveNTC;      //?Descriz. & e-Mail comm.le?
208100130704
208200130704               // -?Nominativo commerciale: Tipo Nota "01"?
208300130709               k_NTCcmm = $Cmm(xx);
208400130709               k_NTCtnt = '01';
208500130709               chain  %kds(k02azntc01)  AZNTC000;
208600130702
208700130709               if  Not %found(AZNTC01L);
208800130709                 clear  AZNTC000;
208900141218                 NTCcmm = k_NTCcmm;           //?Commerciale?
209000141218                 NTCtnt = k_NTCtnt;           //?Tipo Nota = "01"?
209100130702               endif;
209200130702
209300130709               NTCrnt = V1Crnt01;
209400130719
209500130719               CMMdtMod = wDate;
209600130719               CMMute   = KNMUS;
209700130702
209800130723               Select;
209900130723                 When  %found(AZNTC01L)      and  NTCrnt =  *blank;
210000130723                   //_______________
210100130723                   delete  AZNTC000;
210200130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210300130723                 When  %found(AZNTC01L)      and  NTCrnt <> *blank;
210400130723                   //_______________
210500130723                   update  AZNTC000;
210600130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210700130723                 When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
210800130723                   //_______________
210900130723                   write   AZNTC000;
211000130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
211100130723               EndSl;
211200130704
211300130704               // -?e-Mail commerciale: Tipo Nota "02"?
211400130709               k_NTCcmm = $Cmm(xx);
211500130709               k_NTCtnt = '02';
211600130709               chain  %kds(k02azntc01)  AZNTC000;
211700130704
211800130709               if  Not %found(AZNTC01L);
211900130709                 clear  AZNTC000;
212000141218                 NTCcmm = k_NTCcmm;           //?Commerciale?
212100141218                 NTCtnt = k_NTCtnt;           //?Tipo Nota = "02"?
212200130704               endif;
212300130704
212400130709               NTCrnt = V1Crnt02;
212500130704
212600130723               Select;
212700130723                 When  %found(AZNTC01L)      and  NTCrnt =  *blank;
212800130723                   //_______________
212900130723                   delete  AZNTC000;
213000130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
213100130723                 When  %found(AZNTC01L)      and  NTCrnt <> *blank;
213200130723                   //_______________
213300130723                   update  AZNTC000;
213400130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
213500130723                 When  Not %found(AZNTC01L)  and  NTCrnt <> *blank;
213600130723                   //_______________
213700130723                   write   AZNTC000;
213800130723                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
213900130723               EndSl;
214000130702
214100130702             EndIf;
214200130702
214300130702           EndIf;
214400130702
214500130708           xx = %lookup( V1Cuni : $CmmUni : xx+1 );
214600130702
214700130702         EndDo;
214800130702
214900130702       ENDSR;
215000141217
215100141217       //--------------------------------------------------------------
215200141217       //?Aggiornamento/Inserimento record nel file TABEL00F.          ?
215300141217       //--------------------------------------------------------------
215400141217       BEGSR  sr_UpdTab01eHTM;
215500141217
215600141217         // -?Ciclo di elaborazione per ogni sistema informativo?
215700141217         For  w_SisInf = 1  To  %elem($Libr);
215800141217
215900141217           // -?Apertura degli archivi?
216000141217           if  w_SisInf > 1;
216100141217             exsr  sr_OpenFileTab;
216200141217           endif;
216300141217
216400141217           // -?Aggiornamento tab. "HTM"?
216500141217           exsr  sr_UpdTNTBE;
216600141217
216700141217           // -?Aggiornamento tab. "01"?
216800141217           exsr  sr_UpdTABEL;
216900141217
217000141217           // -?Aggiornamento tab. "01" e "HTM"?
217100141217           //  ?del resto della famiglia?
217200141217           if  $Video = 'W2'  and  W02agg = 'SI';
217300141217             exsr  sr_UpdFAMIGLIAtab;
217400141217           endif;
217500141217
217600141217         EndFor;
217700141217
217800141217       ENDSR;
217900141217
218000141217       //--------------------------------------------------------------
218100141217       //?Aggiornamento record nel file TNTBE00F                       ?
218200141217       //--------------------------------------------------------------
218300141217       BEGSR  sr_UpdTNTBE;
218400141217
218500141217         //? N.B.                                                      ?
218600141217         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
218700141217         //   trasmissione (vedi flag TBEFTR).                        ?
218800141217         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
218900141217         //   subito nello stesso file di entrambi i S.I. - in due    ?
219000141217         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
219100141217
219200141217         // -?Aggiornam. tab. "HTM" = Default per testo HTML?   ?
219300141217
219400141217         k_TBEke1 = %editc( V1Ccod : 'X' );
219500141217         chain  %kds( keyTNTBE01 : 3 )  TNTBE000;
219600141217
219700141217         // -?Non reperito record da annullare => uscire?
219800150108         if  Not %found(TNTBE01L)  and  iMK41opz =  4;
219900141217           leavesr;
220000141217         endif;
220100141217
220200141217         // -?Non reperito record da scrivere o aggiornare?
220300141217         //  ?=> impostazione dei campi chiave?
220400141217         if  Not %found(TNTBE01L);
220500141217           clear  TNTBE000;
220600141218           TBEcod = k_TBEcod;            //?"HTM"?
220700141218           TBEke1 = k_TBEke1;            //?%editc(V1Cage:'X')?
220800141218           TBEke2 = k_TBEke2;            //?"CC"?
220900141217           //clear  TBEsif;
221000141217           //clear  TBElin;
221100141217         endif;
221200141217
221300141217         // -?Impostazione campi nel record?
221400141217         TBEapl = 'MA';
221500141217
221600141217         clear  dHTM;
221700141217         §HTMnmc = V1Crnt01;             //?Nominativo Comm.le?
221800141217         §HTMind = V1Crnt02;             //?E-Mail Comm.le?
221900141217         TBEuni = dHTM;
222000141217
222100141217         Select;
222200141217           // -?Disattivazione?
222300141217           When  iMK41opz = 4;
222400141217             TBEatb = 'A';
222500141217             clear  TBEftr;
222600141217             //clear  TBEdtr;
222700141217           //// -?Riattivazione?
222800141217           //When  iMK41opz = 7;
222900141217           //  clear  TBEatb;
223000141217           //  clear  TBRftr;
223100141217           //  TBEdtr = wDate;
223200141217           // -?Inserimento / Copia?
223300141217           When  Not %found(TNTBE01L);
223400141217             clear  TBEatb;
223500141217             if  w_SisInf = 1;
223600141217               TBEftr = 'T';
223700141217             else;
223800141217               TBEftr = 'R';
223900141217             endif;
224000141217             TBEdtr = wDate;
224100141217           // -?Modifica?
224200141217           When  %found(TNTBE01L);
224300150121             clear  TBEatb;
224400141217             if  w_SisInf = 1;
224500141217               TBEftr = 'T';
224600141217             else;
224700141217               TBEftr = 'R';
224800141217             endif;
224900141217             TBEdtr = wDate;
225000141217         EndSl;
225100141217
225200141217         TBEftt = 'S';
225300141217         clear  TBEflt;
225400141217
225500141217         // -?AGGIORNAMENTO?
225600141217         if  %found(TNTBE01L);
225700141217           //_______________
225800141217           update  TNTBE000;
225900141217           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
226000141217         else;
226100141217           //_______________
226200141217           write   TNTBE000;
226300141217           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
226400141217         endif;
226500141217
226600141217       ENDSR;
226700141217
226800141217       //--------------------------------------------------------------
226900141217       //?Aggiornamento record nel file TABEL00F.                      ?
227000141217       //--------------------------------------------------------------
227100141217       BEGSR  sr_UpdTABEL;
227200141217
227300141217         //? N.B.                                                      ?
227400141217         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
227500141217         //   trasmissione (vedi flag TBLFTR).                        ?
227600141217         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
227700141217         //   subito nello stesso file di entrambi i S.I. - in due    ?
227800141217         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
227900141217
228000141217         // -?Aggiornam. tab. "01" = Agenti Commerciali?   ?
228100141217
228200141217         k_TBLkey = %editc( V1Ccod : 'X' );
228300141217         chain  %kds( keyTABEL00 )  TABEL;
228400141217
228500141217         // -?Non reperito record da annullare => uscire?
228600141217         if  Not %found(TABEL00F)  and  iMK41opz =  4;
228700141217           leavesr;
228800141217         endif;
228900141217
229000141217         // -?Non reperito record da scrivere o aggiornare?
229100141217         //  ?=> impostazione dei campi chiave?
229200141217         if  Not %found(TABEL00F)  and  iMK41opz <> 4;
229300141217           clear  TABEL;
229400141218           TBLkut = k_TBLkut;            //?c_Kut (1)?
229500141218           TBLcod = k_TBLcod;            //?c_Tab ("01")?
229600141218           TBLkey = k_TBLkey;            //?%editc(V1Cage:'X')?
229700141217         endif;
229800141217
229900141217         // -?Impostazione campi nel record?
230000141217         exsr  sr_RieDs01;
230100141217         TBLuni = ds01;
230200141217
230300141217         TBLftt = '1';
230400141217         clear  TBLflt;
230500141217
230600141217         Select;
230700141217           // -?Disattivazione?
230800141217           When  iMK41opz = 4;
230900141217             TBLflg = '*';
231000141217             clear  TBLftr;
231100141217             clear  TBLdtr;
231200141217           //// -?Riattivazione?
231300141217           //When  iMK41opz = 7;
231400141217           //  clear  TBLflg;
231500141217           //  clear  TBLftr;
231600141217           //  clear  TBLdtr;
231700141217           // -?Inserimento / Copia?
231800141217           When  Not %found(TABEL00F);
231900141217             if  w_SisInf = 1;
232000141217               TBLftr = 'T';
232100141217             else;
232200141217               TBLftr = 'R';
232300141217             endif;
232400141217             TBLdtr = %int( %subst( %editc(wDate : 'X') :
232500141217                                    %len(wDate) - %len(TBLdtr) + 1 :
232600141217                                    %len(TBLdtr) ) );
232700141217           // -?Modifica?
232800141217           When  %found(TABEL00F);
232900150121             clear  TBLflg;
233000141217             if  w_SisInf = 1;
233100141217               TBLftr = 'T';
233200141217             else;
233300141217               TBLftr = 'R';
233400141217             endif;
233500141217             TBLdtr = %int( %subst( %editc(wDate : 'X') :
233600141217                                    %len(wDate) - %len(TBLdtr) + 1 :
233700141217                                    %len(TBLdtr) ) );
233800141217         EndSl;
233900141217
234000141217         // -?AGGIORNAMENTO?
234100141217         if  %found(TABEL00F);
234200141217           //____________
234300141217           update  TABEL;
234400141217           //¯¯¯¯¯¯¯¯¯¯¯¯
234500141217         else;
234600141217           //____________
234700141217           write   TABEL;
234800141217           //¯¯¯¯¯¯¯¯¯¯¯¯
234900141217         endif;
235000141217
235100141217       ENDSR;
235200141217
235300141217       //--------------------------------------------------------------
235400141217       //?Riempimento struttura dati ds01.                             ?
235500141217       //--------------------------------------------------------------
235600141217       BEGSR  sr_RieDs01;
235700141217
235800141217         clear  ds01;
235900141217
236000141217         §01age = V1Cdes;
236100141217         §01rgf = V1Cuni;
236200141217         §01par = V1Cpar;
236300141217         §01pcf = V1Cpli;
236400141217         §01pce = V1Cple;
236500141217         §01dip = V1Cdip;
236600141217         §01soc = V1Csoc;
236700141217         §01dtDec = %editc( wDataIni : 'X' );
236800141217         §01dtFin = %editc( wDataFin : 'X' );
236900141217         if  V1Cfil > *zero;
237000141217           §01fil = %editc( V1Cfil : 'X' );
237100141217         else;
237200141217           clear  §01fil;
237300141217         endif;
237400141217         §01fun = V1Cfun;
237500141217
237600141217       ENDSR;
237700141217
237800141217       //--------------------------------------------------------------
237900141217       //?Aggiornamento records nei file TABEL00F e TNTBE00F           ?
238000141217       //?per tutti i codici con lo stesso commerciale unificante      ?
238100141217       //--------------------------------------------------------------
238200141217       BEGSR  sr_UpdFAMIGLIAtab;
238300141217
238400141217         xx = %lookup( V1Cuni : $CmmUni );
238500141217
238600141217         DoW  xx > *zero;
238700141217
238800141217           If  $Cmm(xx) <> V1Ccod;
238900141217
239000141217             // -?Aggionamento tab. "01" su TABEL00F?
239100141218             IF  V1Cfun   <> SaveFUN  or      //?Funzione aziendale?
239200141218                 V1Cfil   <> SaveFIL  or      //?Filiale appartenenza?
239300141218                 ds_DIP   <> SaveDIP  or      //?Dipendente?
239400141218                 ds_DtAtt <> SaveDtAtt;       //?Date start & end attività?
239500141217
239600141217               k_TBLkey = %editc( $Cmm(xx) : 'X' );
239700141217               chain  %kds(keyTABEL00)  TABEL;
239800141217
239900141217               If  %found(TABEL00F);
240000141217
240100141217                 ds01 = TBLuni;
240200141217                 if  ds_DIP <> SaveDIP;
240300141217                   §01dip = V1Cdip;
240400141217                   §01soc = V1Csoc;
240500141217                 endif;
240600141217                 if  V1Cfun <> SaveFUN;
240700141217                   §01fun = V1Cfun;
240800141217                 endif;
240900141217                 if  V1Cfil <> SaveFIL;
241000141217                   §01fil = %editc( V1Cfil : 'X' );
241100141217                 endif;
241200141217                 if  ds_DtAtt <> SaveDtAtt;
241300141217                   §01dtDec = %editc( wDataIni : 'X' );
241400141217                   §01dtFin = %editc( wDataFin : 'X' );
241500141217                 endif;
241600141217                 TBLuni = ds01;
241700141217                 if  w_SisInf = 1;
241800141217                   TBLftr = 'T';
241900141217                 else;
242000141217                   TBLftr = 'R';
242100141217                 endif;
242200141217                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
242300141217                                        %len(wDate) - %len(TBLdtr) + 1 :
242400141217                                        %len(TBLdtr) ) );
242500141217                 //____________
242600141217                 update  TABEL;
242700141217                 //¯¯¯¯¯¯¯¯¯¯¯¯
242800141217
242900141217               EndIf;
243000141217
243100141217             ENDIF;
243200141217
243300141217             // -?Aggionamento tab. "HTM" su TNTBE00F?
243400141218             IF  ds_NTC <> SaveNTC;           //?Descriz. & e-mail comm.le?
243500141217
243600141217               k_TBEke1 = %editc( $Cmm(xx) : 'X' );
243700141217               chain  %kds(keyTNTBE01)  TNTBE000;
243800141217
243900141217               if  Not %found(TNTBE01L);
244000141217                 clear  TNTBE000;
244100141218                 TBEcod = k_TBEcod;           //?"HTM"?
244200141218                 TBEke1 = k_TBEke1;           //?Commerciale?
244300141218                 TBEke2 = k_TBEke2;           //?"CC"?
244400141217                 //clear  TBEsif;
244500141217                 //clear  TBElin;
244600141217                 TBEapl = 'MA';
244700141217                 TBEftt = 'S';
244800141217                 //clear  TBEflt;
244900141217               endif;
245000141217
245100141217               clear  dHTM;
245200141217               §HTMnmc = V1Crnt01;
245300141217               §HTMind = V1Crnt02;
245400141217               TBEuni = dHTM;
245500141217               if  w_SisInf = 1;
245600141217                 TBEftr = 'T';
245700141217               else;
245800141217                 TBEftr = 'R';
245900141217               endif;
246000141217               TBEdtr = wDate;
246100141217
246200141217               if  %found(TNTBE01L);
246300141217                 //_______________
246400141217                 update  TNTBE000;
246500141217                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
246600141217               else;
246700141217                 //_______________
246800141217                 write   TNTBE000;
246900141217                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
247000141217               endif;
247100141217
247200141217             ENDIF;
247300141217
247400141217           EndIf;
247500141217
247600141217           xx = %lookup( V1Cuni : $CmmUni : xx+1 );
247700141217
247800141217         EndDo;
247900141217
248000141217       ENDSR;
248100141217
248200141217       //--------------------------------------------------------------
248300141217       //?Apertura dei files tabelle nel sistema informativo impostato.?
248400141217       //--------------------------------------------------------------
248500141217       BEGSR  sr_OpenFileTab;
248600141217
248700141217         // -?Chiusura (eventuale) archivi?
248800141217         if  %open(TABEL00F);
248900141217           close  TABEL00F;
249000141217         endif;
249100141217         if  %open(TNTBE01L);
249200141217           close  TNTBE01L;
249300141217         endif;
249400141217
249500141217         // -?Apertura archivi?
249600141217         ds_Libr  = $Libraries(w_iSystem);
249700141217         wTABEL = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
249800141217         open  TABEL00F;
249900141217         wTNTBE = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
250000141217         open  TNTBE01L;
250100141217
250200141217       ENDSR;
250300130702
250400130702       //--------------------------------------------------------------
250500130702       //?Operazioni finali.                                           ?
250600130702       //--------------------------------------------------------------
250700130702       BEGSR  sr_RoutEnd;
250800130702
250900130702         // -?Restituzine parametri?
251000130704         kpjbu = TRMK41ds;
251100130702
251200130702         // -?Uscita dal *pgm?
251300130702         return;
251400130702
251500130702       ENDSR;
251600130702
251700130702      /end-free
251800130702
251900130702       //--------------------------------------------------------------
252000130702       //?Schiere a tempo di compilazione.                             ?
252100130702       //--------------------------------------------------------------
252200130702
252300141217** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
252400141217SETRAS  GAITRAGRU FILTRAGRU
252500141217AS888   GAITRAGRPSFILTRAGRPF
252600130702** -?$Opz / $OpzD?(Opzione richiesta)?
25270013070800   IMMISSIONE
25280013070802    MODIFICA
25290013070803     COPIA
25300013070804  ANNULLAMENTO  DISATTIVAZIONE
25310013070205VISUALIZZAZIONE
25320013070807   RIPRISTINO   RIATTIVAZIONE
253300130702** -?$Msg?(Messaggi di errore)?----------------------------------------------*
253400130708Opzione non valida.                                                            1
253500130708Codice commerciale inesistente                                                 2
253600130708Codice commerciale momentaneamente allocato: riprovare più tardi               3
253700130708Codice commerciale obbligatorio                                                4
253800130708Codice commerciale già esistente                                               5
253900130708Commerciale di filiale errata                                                  6
254000130708Commerciale con stesso unificante già presente in filiale                      7
254100130708Descrizione commerciale obbligatoria                                           8
254200130709Unificante obbligatorio                                                        9
254300130709Unificante inesistente                                                        10
254400130704ATTENZIONE: inserita particolarità.   Premere INVIO per forzare.              11
254500130704ATTENZIONE: NON è stata inserita la particolarità.  Premere INVIO per forzare.12
254600130708Inserire la funzione aziendale                                                13
254700130708Funzione aziendale errata                                                     14
254800130708Filiale appartenenza errata                                                   15
254900130708La percentuale deve essere minore o uguale a 100                              16
255000130708Se inserito cod dipendente,inserire anche società di appartenenza             17
255100130708Oltre alla società, inserire anche cod dipendente                             18
255200130708Società errata                                                                19
255300130708Codice dipendente inesistente per la società indicata                         20
255400130708Indirizzo e-Mail mancante o errato                                            21
255500130708Data formalmente errata                                                       22
255600130708Data obbligatoria                                                             23
255700130708La data inizio attività non può essere precedente a quella odierna            24
255800130708La data inizio attività è già trascorsa e NON può essere modificata           25
255900130708Data decorrenza attività superiore a data fine attività                       26
256000130708Nessuna filiale gestibile dall'utente                                         27
256100130708Codice commerciale errato                                                     28
256200130708Commerciale unificante NON in gestione                                        29
256300130708Commerciale NON unificante                                                    30
256400130712Codice filiale obbligatorio                                                   31
256500130712Filiale errata                                                                32
256600130712NON reperibile un nuovo codice commerciale per la filiale indicata            33
