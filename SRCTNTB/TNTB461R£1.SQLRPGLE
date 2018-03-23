000100120326       //==============================================================
000200120326       //?TNTB461R * Selezione tab. "LAC" = Clienti x Ritorno Immagini.?
000300120326       //==============================================================
000400141128
000500141128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141204     h dftactgrp(*no) actgrp(*new)
000700120326
000800120326       //--------------------------------------------------------------
000900120326       //?Dichiarazione file.                                          ?
001000120326       //--------------------------------------------------------------
001100120326
001200120326       // -?Tabelle?
001300120326     fTNTBE01L  if   e           k disk
001400120326
001500120326       // -?Video?
001600120326     fTNTB461D  cf   e             workstn
001700120326     f                                     sfile(TB461S01 : S01nrr)
001800120326     f                                     indds(IndDspF)
001900120326     f                                     infds(InfDspF)
002000120326
002100120326       //--------------------------------------------------------------
002200120326       //?Definizione costanti.                                        ?
002300120326       //--------------------------------------------------------------
002400141117
002500141117       // - Costante per controllo "caratteri solo numerici"
002600141117     d c_Digits        c                   const('0123456789')
002700120326
002800120326       // -?Codice tabella in gestione?
002900120326     d c_Tab           c                   const('LAC')
003000120326
003100120326       // -?Numero massimo di record gestibili?
003200120326     d c_MaxRec        c                   const(9999)
003300120326
003400120326       // -?Tasti funzionali a video?
003500120326     d c_F01           c                   const(x'31')
003600120326     d c_F02           c                   const(x'32')
003700120326     d c_F03           c                   const(x'33')
003800120326     d c_F04           c                   const(x'34')
003900120326     d c_F05           c                   const(x'35')
004000120326     d c_F06           c                   const(x'36')
004100120326     d c_F07           c                   const(x'37')
004200120326     d c_F08           c                   const(x'38')
004300120326     d c_F09           c                   const(x'39')
004400120326     d c_F10           c                   const(x'3A')
004500120326     d c_F11           c                   const(x'3B')
004600120326     d c_F12           c                   const(x'3C')
004700120326     d c_F13           c                   const(x'B1')
004800120326     d c_F14           c                   const(x'B2')
004900120326     d c_F15           c                   const(x'B3')
005000120326     d c_F16           c                   const(x'B4')
005100120326     d c_F17           c                   const(x'B5')
005200120326     d c_F18           c                   const(x'B6')
005300120326     d c_F19           c                   const(x'B7')
005400120326     d c_F20           c                   const(x'B8')
005500120326     d c_F21           c                   const(x'B9')
005600120326     d c_F22           c                   const(x'BA')
005700120326     d c_F23           c                   const(x'BB')
005800120326     d c_F24           c                   const(x'BC')
005900120326     d c_Enter         c                   const(x'F1')
006000120326     d c_RollDown      c                   const(x'F4')
006100120326     d c_RollUp        c                   const(x'F5')
006200120326
006300120326       //--------------------------------------------------------------
006400120326       //?Definizione schiere.                                         ?
006500120326       //--------------------------------------------------------------
006600120326
006700120326       // -?Messaggi di errore?
006800141117     d $Msg            s             78    dim(10)  ctdata  perrcd(1)
006900120326
007000120326       //--------------------------------------------------------------
007100120326       //?Definizione aree dati.                                       ?
007200120326       //--------------------------------------------------------------
007300120326
007400120326       // -?Dati utente?
007500120326     d §AzUte        e ds                  extname(AZUTE00F)
007600120326     d                                     dtaara
007700120326     d §DatiUte      e ds                  extname(dDatiUte)
007800120326     d                                     dtaara
007900120326
008000120326       //--------------------------------------------------------------
008100120326       //?Definizione strutture dati.                                  ?
008200120326       //--------------------------------------------------------------
008300120326
008400120326       // -?Status ds?
008500120326     d Status         sds
008600120326     d   SDSpgm          *proc
008700120326
008800120326       // -?InfDS?
008900120326     d InfDspF         ds
009000120326     d   dsp_aid             369    369a
009100120326     d   sfl_rrn             376    377i 0
009200120326     d   min_nrr             378    379i 0
009300120326     d   num_rcds            380    381i 0
009400120326
009500120326       // -?Indicatori su DspF?
009600120326     d IndDspF         ds                  inz
009700120326         // -?Abilitazione tasti funzionali?
009800141202     d   AbilitaF08                    n   overlay(IndDspF : 08)
009900141202     d   AbilitaF10                    n   overlay(IndDspF : 10)
010000141204     d   AbilitaF11                    n   overlay(IndDspF : 11)
010100120326         // -?Emissione messaggio di errore?
010200120326     d   ErrMessage                    n   overlay(IndDspF : 28)
010300120326         // -?Indicatori di gestione del subfile?
010400120326     d   SflDsp_N                      n   overlay(IndDspF : 30)
010500120326     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
010600120326     d   SflNxtChg                     n   overlay(IndDspF : 32)
010700120326     d   SflEnd                        n   overlay(IndDspF : 33)
010800120326         // -?Indicatori per Attibuti di visualizzazione?
010900120326         //  ?o   Condizionamento visualizzazione campi?
011000120326     d   ProtectOPZ                    n   overlay(IndDspF : 41)
011100120326     d   Ord_x_KSU                     n   overlay(IndDspF : 42)
011200120326         // -?Posizionamento cursore & segnalazione errore?
011300120326     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011400141117     d   PosCurCCM                     n   overlay(IndDspF : 51)
011500120326         //   -?Riemissione videata?
011600120326     d   ErrGenerico                   n   overlay(IndDspF : 99)
011700120326
011800120326       // -?Parametri ricevuti?
011900120326     d KPJBA         e ds
012000120326     d Param01         ds
012100120326       // ·?Codice          7/0 In?
012200120326     d  P01cod                        7  0 inz
012300120326       // ·?Ordinamento     1   In   (*blank = Codice;?
012400120326       //                            ?"1" = Padre/Codice;)?
012500120326     d  P01ord                        1
012600120326       // ·?Padre           7/0 In?
012700120326     d  P01ksu                        7  0 inz
012800120326       // ·?Chiave tabella  7   Out?
012900120326     d  P01ke1                        7
013000120326       // ·?KE2            15   Out?
013100120326     d  P01ke2                       15
013200120326       // ·?Codice ritorno  1   Out  (tasto funzionale di uscita)?
013300120326     d  P01rit                        1
013400120326
013500120326       // -?Tab. LAC = Clienti per ritorno immagini?
013600120326     d dLAC          e ds                  inz
013700120327     d  sk_Sch                71     82    dim(12)
013800141128
013900141128       // - Parametri per Ricerca/controllo tabelle
014000141128     d TIBS02ds      e ds                  inz
014100141117
014200141117       // - Pgm gestione tab. LAC
014300141117     d TNTB46DS      e ds                  inz
014400141117
014500141117       // - Controllo/Decodifica cliente
014600141117     d TIBS69ds      e ds                  qualified  inz
014700141117     d ds_CNACO      e ds                  extname(CNACO00F)
014800141117     d                                     qualified  inz
014900141117     d ds_CNIND      e ds                  extname(CNIND00F)
015000141117     d                                     qualified  inz
015100141117     d ds_CNCLP      e ds                  extname(CNCLP00F)
015200141117     d                                     qualified  inz
015300141117     d ds_FNCLS      e ds                  extname(FNCLS00F)
015400141117     d                                     qualified  inz
015500141201
015600141201       // - Interrogazione Voce c/e 199
015700141201     d TREC60DS      e ds                  inz
015800141201
015900141128       // - Tabella "TAL" = Tipo addebito dichiarato
016000141128     d dTAL          e ds                  inz
016100141201
016200141201       // - File Conto Economico
016300141201     d ECEVD00F      e ds                  extname(ECEVD00F)
016400120326
016500120326       //--------------------------------------------------------------
016600120326       //?Definizione variabili globali.                               ?
016700120326       //--------------------------------------------------------------
016800120326
016900120326       // -?Parametri (parziali) in Input?
017000120326     d P01opz3         s              1
017100120326
017200120326       // -?Flags booleani?
017300141128     d Richiamato      s               n   inz(*off)
017400141204     d RichiamatoU     s               n   inz(*off)
017500120326     d $Fine           s               n   inz
017600120326     d $EoF            s               n   inz
017700141117     d $InzS01         s               n   inz(*on)
017800120326     d $RecOK          s               n   inz
017900120327
018000120327       // -?Indici di schiera?
018100120327     d x2              s              2  0 inz
018200120326
018300120326       // -?Variabili per la gestione del video?
018400141117     d $Video          s              2    inz('S1')
018500120326     d S01nrr          s                   like(C1RcdNbr) inz
018600120326     d SavS01csr       s                   like(C1RcdNbr) inz
018700141128
018800141128       // -?Campi di comodo
018900141128     d wCodksc         s              7s 0 inz
019000141128     d wCompCE         s              5s 3 inz
019100141128     d woggi           s              8s 0 inz
019200120326
019300120326       // -?Variabili per la gestione del singolo campo alla variazione?
019400120326       //  ?dello stesso?
019500120326     d SavC1Cksc       s                   like(C1Cksc)   inz(*hival)
019600120326     d SavC1Cksu       s                   like(C1Cksu)   inz(*hival)
019700120327     d SavW1Cdes       s                   like(W1Cdes)   inz
019800120326
019900120326       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
020000120326
020100120326       //--------------------------------------------------------------
020200120326       // -?Constants?
020300120326       //--------------------------------------------------------------
020400120326       // -?MaxKey - Maximum number of key fields allowed by this program?
020500120326     d MaxKey          c                   const(2)
020600120326       // -?Sort order:?
020700120326       //  ?1 = Sort in an ascending sequence?
020800120326       //  ?2 = Sort in a descending sequence?
020900120326     d Ascendente      c                   const(1)
021000120326     d Discendente     c                   const(2)
021100120326       // -?Key data type:?
021200120326       //  ? 0 = Signed binary?
021300120326       //  ? 2 = Signed zoned decimal?
021400120326       //  ? 3 = Signed packed decimal?
021500120326       //  ? 6 = Character with no national language sort sequence applied,?
021600120326       //  ?     if specified?
021700120326       //  ? 7 = Unsigned packed decimal?
021800120326       //  ?     All numbers will have the sign forced positive ('F'X).?
021900120326       //  ? 8 = Unsigned zoned decimal?
022000120326       //  ?     All numbers will have the sign forced positive ('F'X).?
022100120326       //  ? 9 = Unsigned binary?
022200120326       //  ?14 = Date in form of DD/MM/YY?
022300120326       //  ?15 = Date in form of DD.MM.YYYY?
022400120326     d Numero          c                   const(2)
022500120326     d Carattere       c                   const(6)
022600120326       //
022700120326     d Put             c                   const(1)
022800120326     d EndPut          c                   const(2)
022900120326     d Get             c                   const(3)
023000120326
023100120326       //--------------------------------------------------------------
023200120326       // -?Data Structures?
023300120326       //  ?SflRcd     - The entire subfile record to pass to the sort?
023400120326       //  ?QLGSCB     - The sort request block for the QLGSORT API?
023500120326       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
023600120326       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
023700120326       //  ?QUSEC      - Error structure for the QLGSORT API?
023800120326       //--------------------------------------------------------------
023900120326     d SflRcd          ds                  inz
024000120327     d  S1Copz                             inz
024100120326     d  S1Ccod                             inz
024200120326     d  S1Cdes                             inz
024300120326     d  S1Cksu                             inz
024400120326     d  S1Caut                             inz
024500120326     d  S1Cfmi                             inz
024600141117     d  S1Ctad                             inz
024700141202     d  S1comp                             inz
024800141117     d  S1Ctpadd                           inz
024900141202     d  S1Ccompce                          inz
025000141202     d  S1Ctadu                            inz
025100120326     d  S1Cschgg                           inz
025200141128     d  S1periodo                          inz
025300141117     d  S1Cele                             inz
025400121217     d  S1merge                            inz
025500121217     d  S1Cla2                             inz
025600120326     d  Selected                      1a   inz
025700120326      /copy QSYSINC/QRPGLESRC,QLGSORT
025800120326     d QLGKL                         16    dim(MaxKey)
025900120326     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
026000120326     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
026100120326     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
026200120326     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
026300120326      /copy QSYSINC/QRPGLESRC,QLGSRTIO
026400120326      /copy QSYSINC/QRPGLESRC,QUSEC
026500120326
026600120326       //--------------------------------------------------------------
026700120326       // -?Standalone fields?
026800120326       //  ?Nrr        - Relative record number for subfile?
026900120326       //  ?SizeList   - Size of list?
027000120326       //  ?ReturnSize - Size of list returned by sort API?
027100120326       //--------------------------------------------------------------
027200120326     d NotUsed         s             16a   inz
027300120326     d ReturnSize      s              9b 0 inz
027400120326     d SizeList        s              9b 0 inz
027500120326     d RrnLast         s              5i 0 inz
027600120326     d Nrr             s              5i 0 inz
027700120326
027800120326       //--------------------------------------------------------------
027900120326       //?Definizione prototipi procedure.                             ?
028000120326       //--------------------------------------------------------------
028100141128
028200141128       // - Ricerca/Controllo tabelle
028300141128      /copy gaitrasrc/srcProtoPR,TIBS02R
028400141117
028500141117       // -?Gestione tabella LAC
028600141117     d TNTB46R         pr                  extpgm('TNTB46R')
028700141117     d  kpjba                              likeds(KPJBA)
028800120424
028900120424       // -?Conta quanti CD al giorno
029000120424     d TNTB463R        pr                  extpgm('TNTB463R')
029100120424     d  kpjba                              likeds(KPJBA)
029200120326
029300120326       // -?Reperimento dati utente?
029400120326     d TIBS34ds      e ds                  inz
029500120326      /copy gaitrasrc/srcProtoPR,TIBS34R
029600141117
029700141117       // - Controllo/Decodifica cliente
029800141117      /copy gaitrasrc/srcProtoPR,TIBS69R
029900120326
030000120326       // -?Ordinamento subfile?
030100120326      /copy gaitrasrc/srcProtoPr,QLGSRTIO
030200141201
030300141201       // -?Int. C/E Voce 199      ?
030400141201     d TREC63R         pr                  extpgm('TREC63R')
030500141201     d  kpjba                              likeds(KPJBA)
030600120326
030700120326       //--------------------------------------------------------------
030800120326       //?Definizione key-list.                                        ?
030900120326       //--------------------------------------------------------------
031000120326
031100120326       // -?File TNTBE01L?
031200120326     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
031300120326     d                                     prefix(k_)   inz
031400120326
031500120326       //--------------------------------------------------------------
031600120326       //?Riepilogo indicatori utilizzati.                             ?
031700120326       //--------------------------------------------------------------
031800120326
031900120326
032000120326       //--------------------------------------------------------------
032100120326       //?M A I N - L I N E                                            ?
032200120326       //--------------------------------------------------------------
032300120326
032400120326     c     *Entry        plist
032500120326     c                   parm                    KPJBA
032600120326     c                   parm                    P01opz3
032700120326
032800120326      /free
032900120326
033000120326       // -?Operazioni iniziali?
033100120326       exsr sr_RoutInz;
033200120326
033300120326       // -?Ciclo di gestione del file video?
033400120326       DoW  $Fine = *off;
033500120326         select;
033600120326           // -?Gestione videata S1 (subfile)?
033700120326           when $Video = 'S1';
033800120326             exsr sr_GesS01;
033900120326           other;
034000120326             $Fine = *on;
034100120326         endsl;
034200120326       enddo;
034300120326
034400120326       // -?Operazioni finali?
034500120326       exsr sr_RoutEnd;
034600120326
034700120326       //--------------------------------------------------------------
034800120326       //?Operazioni iniziali.                                         ?
034900120326       //--------------------------------------------------------------
035000120326       BEGSR  sr_RoutInz;
035100141128
035200141128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
035300120327
035400120327         // -?Impostazione chiusura?
035500120327         *inLR = *on;
035600120327
035700120327         // -?Reperimento dati job?
035800120327         exsr  sr_DatiJob;
035900120327
036000120327         // -?Impostazione nome programma a video?
036100120327         V1Tpgm = SDSpgm;
036200120326
036300120327         // -?Impostazione parametri ricevuti?
036400120326         Param01 = kpjbu;
036500120327         clear  P01ke1;
036600120327         clear  P01ke2;
036700120326         clear  P01rit;
036800120326
036900120326         // -?Verifica se ricevuti altri parametri?
037000141117         if  %parms() = 1;
037100141117           C1Dopz = '2=Modifica, 3=Copia, 5=Visualizza';
037200141117           clear Param01;
037300141128           Richiamato = *off;
037400120326         else;
037500120326           C1Dopz = '1=Scelta';
037600141128           Richiamato = *on;
037700141204           IF  P01opz3 = 'U';
037800141204             RichiamatoU = *on;
037900141204           ELSE;
038000141204             RichiamatoU = *off;
038100141204           ENDIF;
038200120326         endif;
038300120326
038400120326         // -?Impostazione iniziale / pulizia dei campi chiave?
038500120327         Ord_x_KSU = (P01ord = '1');
038600120326         clear  k05tntbe01;
038700120326         k_TBEcod = c_Tab;
038800141128
038900141128         // Imposto la data di oggi
039000141128         woggi = %dec(%date());
039100120326
039200120326       ENDSR;
039300120327
039400120327       //--------------------------------------------------------------
039500120327       //?Reperimento Dati del job (Utente/Operativi).                 ?
039600120327       //--------------------------------------------------------------
039700120327       BEGSR  sr_DatiJob;
039800120327
039900120327         in(e) §AzUte;
040000120327         if NOT %error;
040100120327           in(e) §DatiUte;
040200120327         endif;
040300120327         if %error or RSut = *blank;
040400120327           tibs34r ( tibs34ds );
040500120327           in §AzUte;
040600120327           in §DatiUte;
040700120327         endif;
040800120327
040900120327       ENDSR;
041000120326
041100120326       //--------------------------------------------------------------
041200120326       //?Gestione subfile S01.                                        ?
041300120326       //--------------------------------------------------------------
041400120326       BEGSR  sr_GesS01;
041500120326
041600120326         // -?Inizializzazione videata?
041700120326         if  $InzS01 = *on;
041800120326           exsr  sr_InzS01;
041900120326           $InzS01 = *off;
042000120326         endif;
042100120326
042200120326         // -?Emissione Testata & Piede?
042300120326         write  TB461T01;
042400120326         write  TB461P01;
042500141201
042600141201         IF  SAVS01csr > 0;
042700141201           C1Csrrrn = SAVS01csr;
042800141201           clear SAVS01csr;
042900141201         ENDIF;
043000120326
043100120326         // -?Posizionamento cursore?
043200120326         if  C1CsrRrn > *zero;
043300120326           C1RcdNbr = C1CsrRrn;
043400120326         else;
043500120326           C1RcdNbr = 1;
043600120326           write  TB461S00;            //?(no rec.)?
043700120326         endif;
043800120326
043900120326         // -?Emissione videata?
044000120326         exfmt  TB461C01;
044100120326
044200120326         reset  ErrMessage;
044300120326         reset  ErrGenerico;
044400120326         clear  V1Dmsg;
044500120326
044600120326         select;
044700120326
044800120326           // -?F3=Fine?
044900120326           when  dsp_aid = c_F03;
045000120326             P01rit = 'C';
045100120326             $Fine  = *on;
045200120327
045300120327           // -?F4=Ricerca per descrizione?
045400120327           when  dsp_aid = c_F04;
045500120327             exsr  sr_F04S01;
045600120424
045700120424           // -?F7=n. CD per giorno?
045800120424           when  dsp_aid = c_F07;
045900120424             exsr sr_F07S01;
046000141201
046100141201           // -?F8=Int. C/E 199?
046200141201           when  dsp_aid = c_F08;
046300141201             exsr sr_F08S01;
046400141117
046500141117           // -?F10=Immizzione?
046600141117           when  dsp_aid = c_F10;
046700141117             exsr  sr_F10S01;
046800120326
046900120326           // -?F11=Ordinamento?
047000120326           when  dsp_aid = c_F11;
047100120326             exsr  sr_F11S01;
047200120326
047300120326           // -?F12=Ritorno?
047400120326           when  dsp_aid = c_F12;
047500141117             IF  %parms = 1;
047600141201               P01rit = 'C';
047700141201               $Fine  = *on;
047800141117             ELSE;
047900120326             P01rit = 'L';
048000120326             $Fine  = *on;
048100141117             ENDIF;
048200120326
048300120326           // -?Invio?
048400120326           other;
048500120326             // -?Gestione opzioni?
048600120327             if  S01nrr > *zero;
048700120327               exsr  sr_OpzS01;
048800120327               if  ErrGenerico;
048900120327                 leavesr;
049000120327               endif;
049100120327             endif;
049200120326
049300120326             // -?Richiesto posizionamento?
049400120326             if  (Not Ord_x_KSU   and   C1Cksc <> SavC1Cksc)   OR
049500120326                 (Ord_x_KSU       and   C1Cksu <> SavC1Cksu);
049600120326               $InzS01 = *on;
049700120326             endif;
049800120326
049900120326         endsl;
050000120326
050100120326       ENDSR;
050200120326
050300120326       //--------------------------------------------------------------
050400120326       //?Inizializzazione subfile S01.                                ?
050500120326       //--------------------------------------------------------------
050600120326       BEGSR  sr_InzS01;
050700120326
050800120326         // -?Spegnimento degli indicatori di errore?
050900120326         %subst(IndDspF : 50) = *off;
051000120326
051100120326         // -?Pulizia subfile control?
051200120327         if  Not Ord_x_KSU;
051300120327           clear  C1Cksu;
051400120327           reset  SavC1Cksu;
051500120326         else;
051600120326           clear  C1Cksc;
051700120326           reset  SavC1Cksc;
051800120326         endif;
051900120326
052000120326         // -?Pulizia subfile?
052100120326         SflDsp_N    = *on;
052200120326         SflDspCtl_N = *on;
052300120326         write  TB461C01;
052400120326         SflDspCtl_N = *off;
052500120326         SflEnd      = *off;
052600120326
052700120326         clear  C1RcdNbr;
052800120326         clear  C1CsrRrn;
052900120326         clear  S01nrr;
053000120326         clear  V1Dmsg;
053100120326         ErrMessage  = *off;
053200120326         ErrGenerico = *off;
053300120326
053400120326         // -?Caricamento completo dei dati nel subfile?
053500120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
053600120326         //  ?l'eventuale ordinamento)?
053700120326         setll  %kds(k05tntbe01)  TNTBE000;
053800120326         reade  %kds(k05tntbe01 : 1)  TNTBE000;
053900120326         $EoF = (%eof(TNTBE01L));
054000120326         exsr  sr_CaricaS01;
054100120326
054200120326         // -?Impaginazione subfile?
054300120326         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
054400120326         if S01nrr > *zero;
054500120326           C1RcdNbr  = 1;
054600120326           C1CsrRrn  = 1;
054700120326         else;
054800120326           clear C1RcdNbr;
054900120326         endif;
055000120326
055100120326         // -?Ordinamento subfile (se premuto F11)?
055200120326         if  Ord_x_KSU   and   S01nrr >  *zero;
055300120326           exsr  sr_SortSfl;
055400120326         endif;
055500141202
055600141202       // -?Se richiamato no F8 e no F10
055700141202         IF  Richiamato;
055800141202           AbilitaF08 = *off;
055900141202           AbilitaF10 = *off;
056000141202         ELSE;
056100141202           AbilitaF08 = *on;
056200141202           AbilitaF10 = *on;
056300141202         ENDIF;
056400141204
056500141204       // -?Se richiamato per Unificante no F11
056600141204         IF  RichiamatoU;
056700141204           AbilitaF11 = *off;
056800141204         ELSE;
056900141204           AbilitaF11 = *on;
057000141204         ENDIF;
057100120326
057200120326       ENDSR;
057300120326
057400120326       //--------------------------------------------------------------
057500120326       //?Caricamento completo del subfile S01                         ?
057600120326       //--------------------------------------------------------------
057700120326       BEGSR  sr_CaricaS01;
057800120326
057900120326         clear  S01nrr;
058000120326         SflNxtChg = *off;
058100120326
058200141117         // -?Ciclo di caricamento?
058300120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
058400120326         //  ?l'eventuale ordinamento)?
058500120326         DoW  Not $EoF;
058600120326
058700120326           // -?Selezione singolo record file TNTBE00F?
058800120326           exsr  sr_SelezRec;
058900120326           if  $EoF;
059000120326             leave;
059100120326           endif;
059200120326
059300120326           // -?Registrazione singolo record nel subfile?
059400120326           if  $RecOK;
059500120326
059600120326             // ·?Impostazione campi?
059700120326             clear  TB461S01;
059800120326             S1Ccod = %int( %subst( TBEke1 : 1 : 7 ) );
059900141128             wCodKsc = S1Ccod;
060000120326             S1Cdes = §LACrag;
060100120326             S1Cksu = §LACksu;
060200141128
060300141128             SELECT;
060400141128             WHEN  §LACaut = 'N';
060500141128               S1Caut = 'NO';
060600141128             WHEN  §LACaut = 'S';
060700141128               S1Caut = 'CD';
060800141128             WHEN  §LACaut = 'A';
060900141128               S1Caut = 'FTP';
061000141128             ENDSL;
061100141128
061200120326             S1Cfmi = §LACfmi;
061300141128
061400141128             SELECT;
061500141128             WHEN  §LACtad = 'N';
061600141128               S1Ctad = 'NO';
061700141128             WHEN  §LACtad = 'S';
061800141128               S1Ctad = 'SI';
061900141128             WHEN  §LACtad = 'V';
062000141128               S1Ctad = 'VARIA 0';
062100141128             ENDSL;
062200141128
062300141128             exec sql
062400141128             select EVDitr into :wCompCE
062500141128             FROM ecevd00f
062600141128             WHERE EVDcli = :wCodKsc and
062700141128                   EVDvoc = 199 and
062800141128                   EVDttv = 'K' and
062900141128                   EVDddt <= :woggi and
063000141128                   EVDdst >= :woggi;
063100141128             IF  sqlcod = 100 or sqlcod < 0;
063200141128               clear wCompCE;
063300141128             ELSE;
063400141128               S1comp = %editc(wCompCE:'J');
063500141128             ENDIF;
063600141128
063700141201             IF  §LACtpadd <> *blanks;
063800141128             clear  TIBS02DS;
063900141128             T02cod = 'TAL';
064000141128             T02mod = 'C';
064100141128             T02ke1 = §LACtpadd;
064200141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
064300141128             IF  T02err = *blank;
064400141128               dTAL = T02uni;
064500141128             ENDIF;
064600141128             S1Ctpadd = §TALdesb;
064700141201             ENDIF;
064800141128
064900141128             S1Ccompce = §LACcompce;
065000141128
065100141128             SELECT;
065200141128             WHEN  §LACtadu = 'I';
065300141128               S1Ctadu = 'IMMAGINE';
065400141128             WHEN  §LACtadu = 'S';
065500141128               S1Ctadu = 'SETTIMANA';
065600141128             WHEN  §LACtadu = 'J';
065700141128               S1Ctadu = 'JOB';
065800141128             WHEN  §LACtadu = 'M';
065900141128               S1Ctadu = 'MESE';
066000141128             ENDSL;
066100141117
066200120326             S1Cschgg = §LACschgg;
066300141128
066400141128             SELECT;
066500141128             WHEN  §LACmesi = 1;
066600141128               S1periodo = 'MENSILE';
066700141128             WHEN  §LACmesi = 2;
066800141128               S1periodo = 'BIMESTRALE';
066900141201             WHEN  §LACmesi = 3;
067000141201               S1periodo = 'TRIMESTRALE';
067100141128             WHEN  §LACmesi = 4;
067200141128               S1periodo = 'QUADRIMESTRALE';
067300141128             WHEN  §LACmesi = 6;
067400141128               S1periodo = 'SEMESTRALE';
067500141128             WHEN  §LACmesi = 12;
067600141128               S1periodo = 'ANNUALE';
067700141128             ENDSL;
067800141117
067900141117             S1Cele  = §LACele;
068000121217             S1merge = §LACmerge;
068100121217             S1Cla2  = §LACla2;
068200120326
068300120326             // ·?Scrittura record nel subfile?
068400120326             S01nrr += 1;
068500120326             write  TB461S01;
068600120326
068700120326           EndIf;
068800120326
068900120326           // -?Lettura record successivo?
069000120326           reade  %kds(k05tntbe01 : 1)  TNTBE000;
069100120326           $EoF = (%eof(TNTBE01L));
069200120326
069300120326         EndDo;
069400120326
069500120326         // -?Memorizzazione posizionamento effettuato?
069600120326         if  Not Ord_x_KSU;
069700120326           SavC1Cksc = C1Cksc;
069800120326         else;
069900120326           SavC1Cksu = C1Cksu;
070000120326         endif;
070100120326
070200120326         // -?Visualizzazione del SFL (se ci sono dati)?
070300120326         SflDsp_N = (S01nrr <= *zero);
070400120326
070500120326         // -?Attivazione del SFLEND?
070600120326         SflEnd = ( $EoF );
070700120326
070800120326         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
070900120326         if  S01nrr > *zero;
071000120326           C1RcdNbr = 1;
071100120326         else;
071200120326           clear  C1RcdNbr;
071300120326         endif;
071400120326
071500120326         C1CsrRrn = C1RcdNbr;
071600120326
071700120326       ENDSR;
071800120326
071900120326       //--------------------------------------------------------------
072000141117       //?Selezione del singolo record letto dalla tabella.      ?
072100120326       //--------------------------------------------------------------
072200120326       BEGSR  sr_SelezRec;
072300120326
072400120326         reset  $RecOK;
072500120326
072600120326         dLAC = TBEuni;
072700120326
072800120326         select;
072900120327
073000120327           // -?Selezione per cliente?
073100120327           when  P01cod <> *zero   and
073200120327                 %editc( P01cod : 'X' ) <> %trim(TBEke1);
073300120327             leavesr;
073400120326
073500120326           // -?Selezione per cliente unificante?
073600120326           when  P01ksu <> *zero   and   P01ksu <> §LACksu;
073700120326             leavesr;
073800141204
073900141204           // -?Selezione per cliente unificante?
074000141204           when  P01ksu = *zeros  and  RichiamatoU  and
074100141204                 %subst(TBEke1:1:7) <> %editc(§LACksu:'X');
074200141204             leavesr;
074300120326
074400120326           // -?Omissione records annullati?
074500141205           when  TBEatb <> *blank;
074600141205             leavesr;
074700120326
074800120326           // -?Richiesto posizionamento per cliente:?
074900120326           //  ?si scartano i clienti con codice inferiore?
075000120327           when  Not Ord_x_KSU   and   TBEke1 < %editc( C1Cksc : 'X' );
075100120326             leavesr;
075200120326
075300120326           // -?Richiesto posizionamento per cliente unificante:?
075400120326           //  ?si scartano i clienti con unificante inferiore?
075500120326           when  Ord_x_KSU   and   §LACksu < C1Cksu;
075600120326             leavesr;
075700120327
075800120327           // -?Selezione per descrizione (ragione sociale cliente)?
075900120327           when  W1Cdes <> *blank   and
076000120327                 %scan( %trimr(SavW1Cdes) : §LACrag ) = *zero;
076100120327             leavesr;
076200120326
076300120326         endsl;
076400120326
076500120326         $RecOK = *on;
076600120326
076700120326       ENDSR;
076800120327
076900120327       //--------------------------------------------------------------
077000120327       //?Gestione tasto funzionale F04 da videata C01 / S01           ?
077100120327       //?F4-Ricerca per descrizione (ragione sociale)                 ?
077200120327       //--------------------------------------------------------------
077300120327       BEGSR  sr_F04S01;
077400120327
077500120327         // -?Emissione window W01?
077600120327         exfmt  TB461W01;
077700120327
077800120327         reset  ErrMessage;
077900120327         reset  ErrGenerico;
078000120327         clear  V1Dmsg;
078100120327
078200120327         Select;
078300120327
078400120327           // -?F12=Ritorno?
078500120327           When  dsp_aid = c_F12;
078600120327             $Video = 'S1';
078700120327             leavesr;
078800120327
078900120327           // -?Invio?
079000120327           Other;
079100120327             if  W1Cdes <> SavW1Cdes;
079200120327               $InzS01   = *on;
079300120327               SavW1Cdes = W1Cdes;
079400120327             endif;
079500120327             if  W1Cdes = *blank;
079600120327               clear  C1Ddes;
079700120327             else;
079800120327               C1Ddes = 'Ricerca: ' + %trim(W1Cdes);
079900120327             endif;
080000120327             leavesr;
080100120327
080200120327         EndSl;
080300120327
080400120327       ENDSR;
080500120424
080600120424       //--------------------------------------------------------------
080700120424       //?Gestione tasto funzionale F07 da videata C01 / S01           ?
080800120424       //?F07-Conta quanti cd al giorno                                ?
080900120424       //--------------------------------------------------------------
081000120424       BEGSR  sr_F07S01;
081100120424
081200120424         TNTB463R (kpjba);
081300120424
081400120424       ENDSR;
081500141201
081600141201       //--------------------------------------------------------------
081700141201       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
081800141201       //?F08-Int. C/E voce 199                                        ?
081900141201       //--------------------------------------------------------------
082000141201       BEGSR  sr_F08S01;
082100141201
082200141201         clear TREC60DS;
082300141201         D60op0 = ' 05';
082400141201         D60voc = 199;
082500141201         kpjbu = TREC60DS;
082600141201         TREC63R (kpjba);
082700141201
082800141201       ENDSR;
082900141117
083000141117       //--------------------------------------------------------------
083100141117       //?Gestione tasto funzionale F10 da videata C01 / S01           ?
083200141117       //?F10-Immissione                                   ?
083300141117       //--------------------------------------------------------------
083400141117       BEGSR  sr_F10S01;
083500141117
083600141117         clear TNTB46DS;
083700141117         kpjbu = TNTB46DS;
083800141117         tntb46r (kpjba);
083900141117         TNTB46DS = kpjbu;
084000141117         IF  B46fxx = '1';
084100141117           $Fine = *on;
084200141117           leavesr;
084300141117         ENDIF;
084400141117         $InzS01 = *on;
084500141117
084600141117       ENDSR;
084700120326
084800120326       //--------------------------------------------------------------
084900120326       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
085000120326       //?F11-Cambia ordinamento sfl                                   ?
085100120326       //--------------------------------------------------------------
085200120326       BEGSR  sr_F11S01;
085300120326
085400120326         Ord_x_KSU = (Not Ord_x_KSU);
085500120326
085600120326         select;
085700120326           // -?Subfile vuoto: nessun record da ordinare?
085800120326           when  S01nrr = *zero;
085900120326           // -?Subfile già posizionato: occorre ricaricarlo?
086000120326           when  C1Cksc <> *zero   or   C1Cksu <> *zero;
086100120326             $InzS01 = *on;
086200120326           // -?Altrimenti: basta riordinarlo?
086300120326           other;
086400120326             exsr  sr_SortSfl;
086500120326         endsl;
086600120326
086700120326         if  Not Ord_x_KSU;
086800120326           clear  C1Cksc;
086900120326           clear  SavC1Cksc;
087000120326         else;
087100120326           clear  C1Cksu;
087200120326           clear  SavC1Cksu;
087300120326         endif;
087400120326
087500120326       ENDSR;
087600120326
087700120326       //--------------------------------------------------------------
087800120326       //?Ordinamento subfile                                          ?
087900120326       //?  This subroutine sorts the subfile records.                 ?
088000120326       //--------------------------------------------------------------
088100120326       BEGSR  sr_SortSfl;
088200120326
088300120326         RrnLast  = S01nrr;
088400120326
088500120326         C1RcdNbr = 1;
088600120326         clear  C1CsrRrn;
088700120326         clear  S01nrr;
088800120326         clear  V1Dmsg;
088900120326         ErrMessage  = *off;
089000120326         SflNxtChg   = *off;
089100120326         ErrGenerico = *off;
089200120326         %subst(IndDspF : 50) = *off;
089300120326
089400120326         //?___________________________________________________________?
089500120326         //?Initialize the key fields to sort on.?
089600120326         //?There is one extra field not in the subfile -- Selected --?
089700120326         //?that is added to the record. This field is used to place?
089800120326         //?selected records in front of nonselected records.?
089900120326         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
090000120326
090100120326         clear  QLGKL;
090200120326         clear  QLGSKL;
090300120326         clear  QLGSCB;
090400120326         clear  QLGSCB00;
090500120326
090600120326         // -?Gestione della scelta ordinamento:?
090700120326         // -?Ordinamento per Padre/Codice?
090800120326         If  Ord_x_KSU = *on;
090900120326           // ·?2 campi chiave?
091000120326           QLGNBRK  = 2;
091100120326           // ·?1° campo: padre (S1Cksu)?
091200120326           QLGSP    = 1 + %size(S1Copz) + %size(S1Ccod) + %size(S1Cdes);
091300120326           QLGSS    = %size(S1Cksu);
091400120326           QLGDT    = Carattere;
091500120326           QLGSO    = Ascendente;
091600120326           QLGKL(1) = QLGSKL;
091700120326           // ·?2° campo: codice (S1Ccod)?
091800120326           QLGSP    = 1 + %size(S1Copz);
091900120326           QLGSS    = %size(S1Ccod);
092000120326           QLGDT    = Carattere;
092100120326           QLGSO    = Ascendente;
092200120326           QLGKL(2) = QLGSKL;
092300120326
092400120326         // -?Ordinamento per Cliente?
092500120326         Else;
092600120326           // ·?1 campi chiave?
092700120326           QLGNBRK  = 1;
092800120326           // ·?1° campo: cliente (S1Ccli)?
092900120326           //            ?a posizone    2,  7 byte, char, ascending.?
093000120326           QLGSP    = 1 + %size(S1Copz);
093100120326           QLGSS    = %size(S1Ccod);
093200120326           QLGDT    = Carattere;
093300120326           QLGSO    = Ascendente;
093400120326           QLGKL(1) = QLGSKL;
093500120326
093600120326         EndIf;
093700120326
093800120326         // -?Load other sort parameters.?
093900120326         QLGLB   = 80 + 16 * MaxKey;
094000120326         QLGRL   = %size(SflRcd) - 1;
094100120326         QLGRT   = 8;
094200120326         QLGOKL  = 80;
094300120326         QLGLKE  = 16;
094400120326         QLGLSS  = 290;
094500120326         // -?Initialize Sort I/O API fields.?
094600120326         QLGRL00 = QLGRL;
094700120326         QLGRC00 = 1;
094800120326         clear  QUSEI;
094900120326         QUSBPRV = %size(QUSEC);
095000120326
095100120326         // -?First step - Initialize the sort routine.?
095200120326         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
095300120326                      SizeList : ReturnSize : QUSEC );
095400120326
095500120326         // -?Next step - Write records to I/O routine.?
095600120326         QLGRT00  = Put;
095700120326         For  Nrr = 1  To  RrnLast;
095800120326           chain  Nrr  TB461S01;
095900120326           // -?Solo le righe con Selected = 'Y' sono riordinate,?
096000120326           //  ?quindi per fare un ordinamento di tutte le righe?
096100120326           //  ?metto 'Y' sempre.?
096200120326           Selected = 'Y';
096300120326           clear  QUSEI;
096400120326           QUSBPRV  = %size(QUSEC);
096500120326           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
096600120326                         SizeList : NotUsed : QUSEC );
096700120326         EndFor;
096800120326
096900120326         // -?Next step - Signal end of input, clear subfile for reload.?
097000120326         QLGRT00 = EndPut;
097100120326         clear  QUSEI;
097200120326         QUSBPRV = %size(QUSEC);
097300120326         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
097400120326                       SizeList : NotUsed : QUSEC );
097500120326
097600120326         // -?Pulizia subfile?
097700120326         SflDsp_N    = *on;
097800120326         SflDspCtl_N = *on;
097900120326         write  TB461C01;
098000120326         SflDspCtl_N = *off;
098100120326         SflEnd      = *off;
098200120326
098300120326         // -?Final step - Write the records back to the subfile.?
098400120326         QLGRT00  = Get;
098500120326         For  Nrr = 1  To RrnLast;
098600120326           clear  QUSEI;
098700120326           QUSBPRV = %size(QUSEC);
098800120326           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
098900120326                         QLGRL00  : NotUsed : QUSEC );
099000120326           // -?Ripristino indicatori utilizzati nel sfl rec?
099100120326           SflNxtChg = (S1Copz <> *blank);
099200120326           S01nrr = Nrr;
099300120326           write  TB461S01;
099400120326         EndFor;
099500120326
099600120326         C1CsrRrn = 1;
099700120326
099800120326         // -?Visualizzazione del SFL (se ci sono dati)?
099900120326         SflDsp_N = (S01nrr <= *zero);
100000120326
100100120326         // -?Attivazione del SFLEND?
100200120326         SflEnd = $EoF;
100300120326
100400120326       ENDSR;
100500120326
100600120326       //--------------------------------------------------------------
100700120326       //?Gestione opzioni subfile S01.                                ?
100800120326       //--------------------------------------------------------------
100900120326       BEGSR  sr_OpzS01;
101000120326
101100120326         clear  SavS01csr;
101200120326
101300120326         // -?Ciclo di lettura subfile?
101400120326         readc  TB461S01;
101500120326
101600120326         DoW  Not %eof(TNTB461D);
101700120326
101800120326           SflNxtChg = *off;
101900120326           %subst(IndDspF : 50) = *off;
102000120326           C1RcdNbr = S01nrr;
102100120326
102200120326           select;
102300120326
102400120326             // -?Opz. 1=Selezione?
102500141128             when  S1Copz = '1'   and   Not $Fine and Richiamato;
102600120326               clear  P01rit;
102700120326               P01ke1 = %editc( S1Ccod : 'X' );
102800120326               clear  P01ke2;
102900120326               $Fine   = *on;
103000120327
103100120327             // -?Opz. 3=Copia?
103200141117             // -?Opz. 2=Modifica/5=Visualizza?
103300141117               WHEN  (S1Copz = '2' or S1Copz = '3' or S1Copz = '5') and
103400141128                      not $Fine and not Richiamato;
103500141117               clear TNTB46DS;
103600141117               B46opz = S1Copz;
103700141117               B46ke1 = %editc(S1Ccod:'X');
103800141117               kpjbu = TNTB46DS;
103900141117               tntb46r (kpjba);
104000141117               TNTB46DS = kpjbu;
104100141117               IF  B46fxx = '1';
104200141117                 $Fine = *on;
104300141117               ENDIF;
104400120326
104500120326             // -?Nessuna opzione?
104600120326             when  S1Copz = *blank;
104700120327
104800120327             // -?Ammessa una sola opzione?
104900120327             when  S1Copz <> *blank   and   $Fine;
105000120327               ErrMessage  = *on;
105100120327               ErrGenerico = *on;
105200120327               PosCurOPZ   = *on;
105300120327               V1Dmsg = $Msg(02);
105400120327               $Fine  = *off;
105500120326
105600120326             // -?Opzione errata?
105700120326             other;
105800120326               ErrMessage  = *on;
105900120326               ErrGenerico = *on;
106000120326               PosCurOPZ   = *on;
106100120326               V1Dmsg = $Msg(01);
106200120326               $Fine  = *off;
106300120326
106400120326           endsl;
106500120326
106600120326           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
106700120326           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
106800120326           if  S1Copz  <> *blank   and
106900120326              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
107000120326             SavS01csr   = C1RcdNbr;
107100120326           endif;
107200120326
107300120326           // -?Aggiornamento sfl?
107400120326           select;
107500120326             when  ErrMessage;
107600120326               SflNxtChg = *on;
107700120326               C1CsrRrn  = C1RcdNbr;
107800120326             when  ErrGenerico;
107900120326               C1CsrRrn  = C1RcdNbr;
108000120326               if  S1Copz <> '1';
108100120326                 clear  S1Copz;
108200120326               endif;
108300120326             other;
108400120326               if  S1Copz <> '1';
108500120326                 clear  S1Copz;
108600120326               endif;
108700120326           endsl;
108800120326
108900120326           if  S1Copz <> *blank;
109000120326             SflNxtChg = *on;
109100120326           endif;
109200120326
109300120326           UPDATE  TB461S01;
109400120326
109500120326           if  ErrGenerico   or   ErrMessage;
109600120326             leave;
109700120326           endif;
109800120326
109900120326           readc  TB461S01;
110000120326
110100120326         ENDDO;
110200120326
110300120326         // -?Riposizionam. cursore sul record contenente la prima opz.?
110400120326         //  ?(se non sono stati rilevati errori)?
110500120326         if  NOT ErrMessage   and   NOT ErrGenerico   and
110600120326             SavS01csr > *zero;
110700120326           C1CsrRrn = SavS01csr;
110800141201           $InzS01 = *on;
110900120326         endif;
111000141201
111100120326
111200120326       ENDSR;
111300120326
111400120326       //--------------------------------------------------------------
111500120326       //?Operazioni finali.                                           ?
111600120326       //--------------------------------------------------------------
111700120326       BEGSR  sr_RoutEnd;
111800120327
111900120327         // -?Restituzione parametri?
112000120327         kpjbu = Param01;
112100120326
112200120326         // -?"Chiusura" *pgm?
112300120326         return;
112400120326
112500120326       ENDSR;
112600120326
112700120326      /end-free
112800120326
112900120326       //--------------------------------------------------------------
113000120326       //?Schiere a tempo di compilazione.                             ?
113100120326       //--------------------------------------------------------------
113200120326
113300120326** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
113400120326Opzione errata                                                                 1
113500120326Ammessa la selezione di un solo cliente                                        2
113600141117Immettere il codice cliente                                                    3
113700141117Immettere SOLO caratteri numerici                                              4
113800141117Codice errato                                                                  5
113900141117Cliente annullato                                                              6
