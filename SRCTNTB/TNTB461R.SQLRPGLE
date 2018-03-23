000100151130       //==============================================================
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
011400120326         //   -?Riemissione videata?
011500120326     d   ErrGenerico                   n   overlay(IndDspF : 99)
011600120326
011700120326       // -?Parametri ricevuti?
011800120326     d KPJBA         e ds
011900120326     d Param01         ds
012000120326       // ·?Codice          7/0 In?
012100120326     d  P01cod                        7  0 inz
012200120326       // ·?Ordinamento     1   In   (*blank = Codice;?
012300120326       //                            ?"1" = Padre/Codice;)?
012400120326     d  P01ord                        1
012500120326       // ·?Padre           7/0 In?
012600120326     d  P01ksu                        7  0 inz
012700120326       // ·?Chiave tabella  7   Out?
012800120326     d  P01ke1                        7
012900120326       // ·?KE2            15   Out?
013000120326     d  P01ke2                       15
013100120326       // ·?Codice ritorno  1   Out  (tasto funzionale di uscita)?
013200120326     d  P01rit                        1
013300120326
013400120326       // -?Tab. LAC = Clienti per ritorno immagini?
013500120326     d dLAC          e ds                  inz
013600120327     d  sk_Sch                71     82    dim(12)
013700141128
013800141128       // - Parametri per Ricerca/controllo tabelle
013900141128     d TIBS02ds      e ds                  inz
014000141117
014100141117       // - Pgm gestione tab. LAC
014200141117     d TNTB46DS      e ds                  inz
014300141117
014400141117       // - Controllo/Decodifica cliente
014500141117     d TIBS69ds      e ds                  qualified  inz
014600141117     d ds_CNACO      e ds                  extname(CNACO00F)
014700141117     d                                     qualified  inz
014800141117     d ds_CNIND      e ds                  extname(CNIND00F)
014900141117     d                                     qualified  inz
015000141117     d ds_CNCLP      e ds                  extname(CNCLP00F)
015100141117     d                                     qualified  inz
015200141117     d ds_FNCLS      e ds                  extname(FNCLS00F)
015300141117     d                                     qualified  inz
015400141201
015500141201       // - Interrogazione Voce c/e 199
015600141201     d TREC60DS      e ds                  inz
015700141201
015800141128       // - Tabella "TAL" = Tipo addebito dichiarato
015900141128     d dTAL          e ds                  inz
016000141201
016100141201       // - File Conto Economico
016200141201     d ECEVD00F      e ds                  extname(ECEVD00F)
016300120326
016400120326       //--------------------------------------------------------------
016500120326       //?Definizione variabili globali.                               ?
016600120326       //--------------------------------------------------------------
016700120326
016800120326       // -?Parametri (parziali) in Input?
016900120326     d P01opz3         s              1
017000120326
017100120326       // -?Flags booleani?
017200141128     d Richiamato      s               n   inz(*off)
017300141204     d RichiamatoU     s               n   inz(*off)
017400120326     d $Fine           s               n   inz
017500120326     d $EoF            s               n   inz
017600141117     d $InzS01         s               n   inz(*on)
017700120326     d $RecOK          s               n   inz
017800120327
017900120327       // -?Indici di schiera?
018000120327     d x2              s              2  0 inz
018100120326
018200120326       // -?Variabili per la gestione del video?
018300141117     d $Video          s              2    inz('S1')
018400120326     d S01nrr          s                   like(C1RcdNbr) inz
018500120326     d SavS01csr       s                   like(C1RcdNbr) inz
018600141128
018700141128       // -?Campi di comodo
018800141128     d wCodksc         s              7s 0 inz
018900141128     d wCompCE         s              5s 3 inz
019000141128     d woggi           s              8s 0 inz
019100120326
019200120326       // -?Variabili per la gestione del singolo campo alla variazione?
019300120326       //  ?dello stesso?
019400120326     d SavC1Cksc       s                   like(C1Cksc)   inz(*hival)
019500120326     d SavC1Cksu       s                   like(C1Cksu)   inz(*hival)
019600120327     d SavW1Cdes       s                   like(W1Cdes)   inz
019700160516     d SavW1Ctpela     s                   like(W1Ctpela) inz
019800160516     d SavW1Cschgg     s                   like(W1Cschgg) inz
019900120326
020000120326       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
020100120326
020200120326       //--------------------------------------------------------------
020300120326       // -?Constants?
020400120326       //--------------------------------------------------------------
020500120326       // -?MaxKey - Maximum number of key fields allowed by this program?
020600120326     d MaxKey          c                   const(2)
020700120326       // -?Sort order:?
020800120326       //  ?1 = Sort in an ascending sequence?
020900120326       //  ?2 = Sort in a descending sequence?
021000120326     d Ascendente      c                   const(1)
021100120326     d Discendente     c                   const(2)
021200120326       // -?Key data type:?
021300120326       //  ? 0 = Signed binary?
021400120326       //  ? 2 = Signed zoned decimal?
021500120326       //  ? 3 = Signed packed decimal?
021600120326       //  ? 6 = Character with no national language sort sequence applied,?
021700120326       //  ?     if specified?
021800120326       //  ? 7 = Unsigned packed decimal?
021900120326       //  ?     All numbers will have the sign forced positive ('F'X).?
022000120326       //  ? 8 = Unsigned zoned decimal?
022100120326       //  ?     All numbers will have the sign forced positive ('F'X).?
022200120326       //  ? 9 = Unsigned binary?
022300120326       //  ?14 = Date in form of DD/MM/YY?
022400120326       //  ?15 = Date in form of DD.MM.YYYY?
022500120326     d Numero          c                   const(2)
022600120326     d Carattere       c                   const(6)
022700120326       //
022800120326     d Put             c                   const(1)
022900120326     d EndPut          c                   const(2)
023000120326     d Get             c                   const(3)
023100120326
023200120326       //--------------------------------------------------------------
023300120326       // -?Data Structures?
023400120326       //  ?SflRcd     - The entire subfile record to pass to the sort?
023500120326       //  ?QLGSCB     - The sort request block for the QLGSORT API?
023600120326       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
023700120326       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
023800120326       //  ?QUSEC      - Error structure for the QLGSORT API?
023900120326       //--------------------------------------------------------------
024000120326     d SflRcd          ds                  inz
024100120327     d  S1Copz                             inz
024200120326     d  S1Ccod                             inz
024300120326     d  S1Cdes                             inz
024400120326     d  S1Cksu                             inz
024500120326     d  S1Caut                             inz
024600120326     d  S1Cfmi                             inz
024700141117     d  S1Ctad                             inz
024800141202     d  S1comp                             inz
024900141117     d  S1Ctpadd                           inz
025000141202     d  S1Ccompce                          inz
025100141202     d  S1Ctadu                            inz
025200120326     d  S1Cschgg                           inz
025300141128     d  S1periodo                          inz
025400141117     d  S1Cele                             inz
025500121217     d  S1merge                            inz
025600121217     d  S1Cla2                             inz
025700120326     d  Selected                      1a   inz
025800120326      /copy QSYSINC/QRPGLESRC,QLGSORT
025900120326     d QLGKL                         16    dim(MaxKey)
026000120326     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
026100120326     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
026200120326     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
026300120326     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
026400120326      /copy QSYSINC/QRPGLESRC,QLGSRTIO
026500120326      /copy QSYSINC/QRPGLESRC,QUSEC
026600120326
026700120326       //--------------------------------------------------------------
026800120326       // -?Standalone fields?
026900120326       //  ?Nrr        - Relative record number for subfile?
027000120326       //  ?SizeList   - Size of list?
027100120326       //  ?ReturnSize - Size of list returned by sort API?
027200120326       //--------------------------------------------------------------
027300120326     d NotUsed         s             16a   inz
027400120326     d ReturnSize      s              9b 0 inz
027500120326     d SizeList        s              9b 0 inz
027600120326     d RrnLast         s              5i 0 inz
027700120326     d Nrr             s              5i 0 inz
027800120326
027900120326       //--------------------------------------------------------------
028000120326       //?Definizione prototipi procedure.                             ?
028100120326       //--------------------------------------------------------------
028200141128
028300141128       // - Ricerca/Controllo tabelle
028400141128      /copy gaitrasrc/srcProtoPR,TIBS02R
028500141117
028600141117       // -?Gestione tabella LAC
028700141117     d TNTB46R         pr                  extpgm('TNTB46R')
028800141117     d  kpjba                              likeds(KPJBA)
028900120424
029000120424       // -?Conta quanti CD al giorno
029100120424     d TNTB463R        pr                  extpgm('TNTB463R')
029200120424     d  kpjba                              likeds(KPJBA)
029300120326
029400120326       // -?Reperimento dati utente?
029500120326     d TIBS34ds      e ds                  inz
029600120326      /copy gaitrasrc/srcProtoPR,TIBS34R
029700141117
029800141117       // - Controllo/Decodifica cliente
029900141117      /copy gaitrasrc/srcProtoPR,TIBS69R
030000120326
030100120326       // -?Ordinamento subfile?
030200120326      /copy gaitrasrc/srcProtoPr,QLGSRTIO
030300141201
030400141201       // -?Int. C/E Voce 199      ?
030500141201     d TREC63R         pr                  extpgm('TREC63R')
030600141201     d  kpjba                              likeds(KPJBA)
030700120326
030800120326       //--------------------------------------------------------------
030900120326       //?Definizione key-list.                                        ?
031000120326       //--------------------------------------------------------------
031100120326
031200120326       // -?File TNTBE01L?
031300120326     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
031400120326     d                                     prefix(k_)   inz
031500120326
031600120326       //--------------------------------------------------------------
031700120326       //?Riepilogo indicatori utilizzati.                             ?
031800120326       //--------------------------------------------------------------
031900120326
032000120326
032100120326       //--------------------------------------------------------------
032200120326       //?M A I N - L I N E                                            ?
032300120326       //--------------------------------------------------------------
032400120326
032500120326     c     *Entry        plist
032600120326     c                   parm                    KPJBA
032700120326     c                   parm                    P01opz3
032800120326
032900120326      /free
033000120326
033100120326       // -?Operazioni iniziali?
033200120326       exsr sr_RoutInz;
033300120326
033400120326       // -?Ciclo di gestione del file video?
033500120326       DoW  $Fine = *off;
033600120326         select;
033700120326           // -?Gestione videata S1 (subfile)?
033800120326           when $Video = 'S1';
033900120326             exsr sr_GesS01;
034000120326           other;
034100120326             $Fine = *on;
034200120326         endsl;
034300120326       enddo;
034400120326
034500120326       // -?Operazioni finali?
034600120326       exsr sr_RoutEnd;
034700120326
034800120326       //--------------------------------------------------------------
034900120326       //?Operazioni iniziali.                                         ?
035000120326       //--------------------------------------------------------------
035100120326       BEGSR  sr_RoutInz;
035200141128
035300141128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
035400120327
035500120327         // -?Impostazione chiusura?
035600120327         *inLR = *on;
035700120327
035800120327         // -?Reperimento dati job?
035900120327         exsr  sr_DatiJob;
036000120327
036100120327         // -?Impostazione nome programma a video?
036200120327         V1Tpgm = SDSpgm;
036300120326
036400120327         // -?Impostazione parametri ricevuti?
036500120326         Param01 = kpjbu;
036600120327         clear  P01ke1;
036700120327         clear  P01ke2;
036800120326         clear  P01rit;
036900120326
037000120326         // -?Verifica se ricevuti altri parametri?
037100141117         if  %parms() = 1;
037200151130           C1Dopz = '2=Modifica, 3=Copia, 5=Visualizza, 7=Ripristina';
037300141117           clear Param01;
037400141128           Richiamato = *off;
037500120326         else;
037600120326           C1Dopz = '1=Scelta';
037700141128           Richiamato = *on;
037800141204           IF  P01opz3 = 'U';
037900141204             RichiamatoU = *on;
038000141204           ELSE;
038100141204             RichiamatoU = *off;
038200141204           ENDIF;
038300120326         endif;
038400120326
038500120326         // -?Impostazione iniziale / pulizia dei campi chiave?
038600120327         Ord_x_KSU = (P01ord = '1');
038700120326         clear  k05tntbe01;
038800120326         k_TBEcod = c_Tab;
038900141128
039000141128         // Imposto la data di oggi
039100141128         woggi = %dec(%date());
039200120326
039300120326       ENDSR;
039400120327
039500120327       //--------------------------------------------------------------
039600120327       //?Reperimento Dati del job (Utente/Operativi).                 ?
039700120327       //--------------------------------------------------------------
039800120327       BEGSR  sr_DatiJob;
039900120327
040000120327         in(e) §AzUte;
040100120327         if NOT %error;
040200120327           in(e) §DatiUte;
040300120327         endif;
040400120327         if %error or RSut = *blank;
040500120327           tibs34r ( tibs34ds );
040600120327           in §AzUte;
040700120327           in §DatiUte;
040800120327         endif;
040900120327
041000120327       ENDSR;
041100120326
041200120326       //--------------------------------------------------------------
041300120326       //?Gestione subfile S01.                                        ?
041400120326       //--------------------------------------------------------------
041500120326       BEGSR  sr_GesS01;
041600120326
041700120326         // -?Inizializzazione videata?
041800120326         if  $InzS01 = *on;
041900120326           exsr  sr_InzS01;
042000120326           $InzS01 = *off;
042100120326         endif;
042200120326
042300120326         // -?Emissione Testata & Piede?
042400120326         write  TB461T01;
042500120326         write  TB461P01;
042600141201
042700141201         IF  SAVS01csr > 0;
042800141201           C1Csrrrn = SAVS01csr;
042900141201           clear SAVS01csr;
043000141201         ENDIF;
043100120326
043200120326         // -?Posizionamento cursore?
043300120326         if  C1CsrRrn > *zero;
043400120326           C1RcdNbr = C1CsrRrn;
043500120326         else;
043600120326           C1RcdNbr = 1;
043700120326           write  TB461S00;            //?(no rec.)?
043800120326         endif;
043900120326
044000120326         // -?Emissione videata?
044100120326         exfmt  TB461C01;
044200120326
044300120326         reset  ErrMessage;
044400120326         reset  ErrGenerico;
044500120326         clear  V1Dmsg;
044600120326
044700120326         select;
044800120326
044900120326           // -?F3=Fine?
045000120326           when  dsp_aid = c_F03;
045100120326             P01rit = 'C';
045200120326             $Fine  = *on;
045300120327
045400120327           // -?F4=Ricerca per descrizione?
045500120327           when  dsp_aid = c_F04;
045600120327             exsr  sr_F04S01;
045700120424
045800120424           // -?F7=n. CD per giorno?
045900120424           when  dsp_aid = c_F07;
046000120424             exsr sr_F07S01;
046100141201
046200141201           // -?F8=Int. C/E 199?
046300141201           when  dsp_aid = c_F08;
046400141201             exsr sr_F08S01;
046500141117
046600141117           // -?F10=Immizzione?
046700141117           when  dsp_aid = c_F10;
046800141117             exsr  sr_F10S01;
046900120326
047000120326           // -?F11=Ordinamento?
047100120326           when  dsp_aid = c_F11;
047200120326             exsr  sr_F11S01;
047300120326
047400120326           // -?F12=Ritorno?
047500120326           when  dsp_aid = c_F12;
047600141117             IF  %parms = 1;
047700141201               P01rit = 'C';
047800141201               $Fine  = *on;
047900141117             ELSE;
048000120326             P01rit = 'L';
048100120326             $Fine  = *on;
048200141117             ENDIF;
048300120326
048400120326           // -?Invio?
048500120326           other;
048600120326             // -?Gestione opzioni?
048700120327             if  S01nrr > *zero;
048800120327               exsr  sr_OpzS01;
048900120327               if  ErrGenerico;
049000120327                 leavesr;
049100120327               endif;
049200120327             endif;
049300120326
049400120326             // -?Richiesto posizionamento?
049500120326             if  (Not Ord_x_KSU   and   C1Cksc <> SavC1Cksc)   OR
049600120326                 (Ord_x_KSU       and   C1Cksu <> SavC1Cksu);
049700120326               $InzS01 = *on;
049800120326             endif;
049900120326
050000120326         endsl;
050100120326
050200120326       ENDSR;
050300120326
050400120326       //--------------------------------------------------------------
050500120326       //?Inizializzazione subfile S01.                                ?
050600120326       //--------------------------------------------------------------
050700120326       BEGSR  sr_InzS01;
050800120326
050900120326         // -?Spegnimento degli indicatori di errore?
051000120326         %subst(IndDspF : 50) = *off;
051100120326
051200120326         // -?Pulizia subfile control?
051300120327         if  Not Ord_x_KSU;
051400120327           clear  C1Cksu;
051500120327           reset  SavC1Cksu;
051600120326         else;
051700120326           clear  C1Cksc;
051800120326           reset  SavC1Cksc;
051900120326         endif;
052000120326
052100120326         // -?Pulizia subfile?
052200120326         SflDsp_N    = *on;
052300120326         SflDspCtl_N = *on;
052400120326         write  TB461C01;
052500120326         SflDspCtl_N = *off;
052600120326         SflEnd      = *off;
052700120326
052800120326         clear  C1RcdNbr;
052900120326         clear  C1CsrRrn;
053000120326         clear  S01nrr;
053100120326         clear  V1Dmsg;
053200120326         ErrMessage  = *off;
053300120326         ErrGenerico = *off;
053400120326
053500120326         // -?Caricamento completo dei dati nel subfile?
053600120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
053700120326         //  ?l'eventuale ordinamento)?
053800120326         setll  %kds(k05tntbe01)  TNTBE000;
053900120326         reade  %kds(k05tntbe01 : 1)  TNTBE000;
054000120326         $EoF = (%eof(TNTBE01L));
054100120326         exsr  sr_CaricaS01;
054200120326
054300120326         // -?Impaginazione subfile?
054400120326         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
054500120326         if S01nrr > *zero;
054600120326           C1RcdNbr  = 1;
054700120326           C1CsrRrn  = 1;
054800120326         else;
054900120326           clear C1RcdNbr;
055000120326         endif;
055100120326
055200120326         // -?Ordinamento subfile (se premuto F11)?
055300120326         if  Ord_x_KSU   and   S01nrr >  *zero;
055400120326           exsr  sr_SortSfl;
055500120326         endif;
055600141202
055700141202       // -?Se richiamato no F8 e no F10
055800141202         IF  Richiamato;
055900141202           AbilitaF08 = *off;
056000141202           AbilitaF10 = *off;
056100141202         ELSE;
056200141202           AbilitaF08 = *on;
056300141202           AbilitaF10 = *on;
056400141202         ENDIF;
056500141204
056600141204       // -?Se richiamato per Unificante no F11
056700141204         IF  RichiamatoU;
056800141204           AbilitaF11 = *off;
056900141204         ELSE;
057000141204           AbilitaF11 = *on;
057100141204         ENDIF;
057200120326
057300120326       ENDSR;
057400120326
057500120326       //--------------------------------------------------------------
057600120326       //?Caricamento completo del subfile S01                         ?
057700120326       //--------------------------------------------------------------
057800120326       BEGSR  sr_CaricaS01;
057900120326
058000120326         clear  S01nrr;
058100120326         SflNxtChg = *off;
058200120326
058300141117         // -?Ciclo di caricamento?
058400120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
058500120326         //  ?l'eventuale ordinamento)?
058600120326         DoW  Not $EoF;
058700120326
058800120326           // -?Selezione singolo record file TNTBE00F?
058900120326           exsr  sr_SelezRec;
059000120326           if  $EoF;
059100120326             leave;
059200120326           endif;
059300120326
059400120326           // -?Registrazione singolo record nel subfile?
059500120326           if  $RecOK;
059600120326
059700120326             // ·?Impostazione campi?
059800120326             clear  TB461S01;
059900120326             S1Ccod = %int( %subst( TBEke1 : 1 : 7 ) );
060000141128             wCodKsc = S1Ccod;
060100120326             S1Cdes = §LACrag;
060200151130             S1CATB = TBEATB;
060300120326             S1Cksu = §LACksu;
060400141128
060500141128             SELECT;
060600141128             WHEN  §LACaut = 'N';
060700141128               S1Caut = 'NO';
060800141128             WHEN  §LACaut = 'S';
060900141128               S1Caut = 'CD';
061000141128             WHEN  §LACaut = 'A';
061100141128               S1Caut = 'FTP';
061200141128             ENDSL;
061300141128
061400120326             S1Cfmi = §LACfmi;
061500141128
061600141128             SELECT;
061700141128             WHEN  §LACtad = 'N';
061800141128               S1Ctad = 'NO';
061900141128             WHEN  §LACtad = 'S';
062000141128               S1Ctad = 'SI';
062100141128             WHEN  §LACtad = 'V';
062200141128               S1Ctad = 'VARIA 0';
062300141128             ENDSL;
062400141128
062500141128             exec sql
062600141128             select EVDitr into :wCompCE
062700141128             FROM ecevd00f
062800141128             WHERE EVDcli = :wCodKsc and
062900141128                   EVDvoc = 199 and
063000141128                   EVDttv = 'K' and
063100141128                   EVDddt <= :woggi and
063200141128                   EVDdst >= :woggi;
063300141128             IF  sqlcod = 100 or sqlcod < 0;
063400141128               clear wCompCE;
063500141128             ELSE;
063600141128               S1comp = %editc(wCompCE:'J');
063700141128             ENDIF;
063800141128
063900141201             IF  §LACtpadd <> *blanks;
064000141128             clear  TIBS02DS;
064100141128             T02cod = 'TAL';
064200141128             T02mod = 'C';
064300141128             T02ke1 = §LACtpadd;
064400141128             TNTBE_RicercaControllo(kpjba : tibs02ds);
064500141128             IF  T02err = *blank;
064600141128               dTAL = T02uni;
064700141128             ENDIF;
064800141128             S1Ctpadd = §TALdesb;
064900141201             ENDIF;
065000141128
065100141128             S1Ccompce = §LACcompce;
065200141128
065300141128             SELECT;
065400141128             WHEN  §LACtadu = 'I';
065500141128               S1Ctadu = 'IMMAGINE';
065600141128             WHEN  §LACtadu = 'S';
065700141128               S1Ctadu = 'SETTIMANA';
065800141128             WHEN  §LACtadu = 'J';
065900141128               S1Ctadu = 'JOB';
066000141128             WHEN  §LACtadu = 'M';
066100141128               S1Ctadu = 'MESE';
066200141128             ENDSL;
066300141117
066400120326             S1Cschgg = §LACschgg;
066500141128
066600141128             SELECT;
066700141128             WHEN  §LACmesi = 1;
066800141128               S1periodo = 'MENSILE';
066900141128             WHEN  §LACmesi = 2;
067000141128               S1periodo = 'BIMESTRALE';
067100141201             WHEN  §LACmesi = 3;
067200141201               S1periodo = 'TRIMESTRALE';
067300141128             WHEN  §LACmesi = 4;
067400141128               S1periodo = 'QUADRIMESTRALE';
067500141128             WHEN  §LACmesi = 6;
067600141128               S1periodo = 'SEMESTRALE';
067700141128             WHEN  §LACmesi = 12;
067800141128               S1periodo = 'ANNUALE';
067900141128             ENDSL;
068000141117
068100141117             S1Cele  = §LACele;
068200121217             S1merge = §LACmerge;
068300121217             S1Cla2  = §LACla2;
068400120326
068500120326             // ·?Scrittura record nel subfile?
068600120326             S01nrr += 1;
068700120326             write  TB461S01;
068800120326
068900120326           EndIf;
069000120326
069100120326           // -?Lettura record successivo?
069200120326           reade  %kds(k05tntbe01 : 1)  TNTBE000;
069300120326           $EoF = (%eof(TNTBE01L));
069400120326
069500120326         EndDo;
069600120326
069700120326         // -?Memorizzazione posizionamento effettuato?
069800120326         if  Not Ord_x_KSU;
069900120326           SavC1Cksc = C1Cksc;
070000120326         else;
070100120326           SavC1Cksu = C1Cksu;
070200120326         endif;
070300120326
070400120326         // -?Visualizzazione del SFL (se ci sono dati)?
070500120326         SflDsp_N = (S01nrr <= *zero);
070600120326
070700120326         // -?Attivazione del SFLEND?
070800120326         SflEnd = ( $EoF );
070900120326
071000120326         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
071100120326         if  S01nrr > *zero;
071200120326           C1RcdNbr = 1;
071300120326         else;
071400120326           clear  C1RcdNbr;
071500120326         endif;
071600120326
071700120326         C1CsrRrn = C1RcdNbr;
071800120326
071900120326       ENDSR;
072000120326
072100120326       //--------------------------------------------------------------
072200141117       //?Selezione del singolo record letto dalla tabella.      ?
072300120326       //--------------------------------------------------------------
072400120326       BEGSR  sr_SelezRec;
072500120326
072600120326         reset  $RecOK;
072700120326
072800120326         dLAC = TBEuni;
072900120326
073000120326         select;
073100120327
073200120327           // -?Selezione per cliente?
073300120327           when  P01cod <> *zero   and
073400120327                 %editc( P01cod : 'X' ) <> %trim(TBEke1);
073500120327             leavesr;
073600120326
073700120326           // -?Selezione per cliente unificante?
073800120326           when  P01ksu <> *zero   and   P01ksu <> §LACksu;
073900120326             leavesr;
074000141204
074100141204           // -?Selezione per cliente unificante?
074200141204           when  P01ksu = *zeros  and  RichiamatoU  and
074300141204                 %subst(TBEke1:1:7) <> %editc(§LACksu:'X');
074400141204             leavesr;
074500120326
074600120326           // -?Richiesto posizionamento per cliente:?
074700120326           //  ?si scartano i clienti con codice inferiore?
074800120327           when  Not Ord_x_KSU   and   TBEke1 < %editc( C1Cksc : 'X' );
074900120326             leavesr;
075000120326
075100120326           // -?Richiesto posizionamento per cliente unificante:?
075200120326           //  ?si scartano i clienti con unificante inferiore?
075300120326           when  Ord_x_KSU   and   §LACksu < C1Cksu;
075400120326             leavesr;
075500120327
075600160516           // -?Selezione per ricerca?
075700160516           WHEN  W1Cdes <> *blanks or W1Ctpela <> *blanks or
075800160516                 W1Cschgg <> *zeros;
075900160516             IF  W1Cdes <> *blank   and
076000120327                 %scan( %trimr(SavW1Cdes) : §LACrag ) = *zero;
076100160516               leavesr;
076200160516             ENDIF;
076300160516             IF  W1Ctpela <> *blanks and savW1Ctpela = 'CD ' and
076400160516                 §LACaut <> 'S';
076500160516               leavesr;
076600160516             ENDIF;
076700160516             IF  W1Ctpela <> *blanks and savW1Ctpela = 'FTP' and
076800160516                 §LACaut <> 'A';
076900160516               leavesr;
077000160516             ENDIF;
077100160516             IF  W1Ctpela <> *blanks and savW1Ctpela = 'NO ' and
077200160516                 §LACaut <> 'N';
077300160516               leavesr;
077400160516             ENDIF;
077500160516             IF  W1Cschgg <> *zeros and savW1Cschgg <> §LACschgg;
077600160516               leavesr;
077700160516             ENDIF;
077800120326
077900120326         endsl;
078000120326
078100120326         $RecOK = *on;
078200120326
078300120326       ENDSR;
078400120327
078500120327       //--------------------------------------------------------------
078600120327       //?Gestione tasto funzionale F04 da videata C01 / S01           ?
078700160516       //?F4-Ricerca
078800120327       //--------------------------------------------------------------
078900120327       BEGSR  sr_F04S01;
079000120327
079100120327         // -?Emissione window W01?
079200120327         exfmt  TB461W01;
079300120327
079400120327         reset  ErrMessage;
079500120327         reset  ErrGenerico;
079600120327         clear  V1Dmsg;
079700120327
079800120327         Select;
079900120327
080000120327           // -?F12=Ritorno?
080100120327           When  dsp_aid = c_F12;
080200120327             $Video = 'S1';
080300120327             leavesr;
080400120327
080500120327           // -?Invio?
080600120327           Other;
080700160516             if  W1Cdes <> SavW1Cdes or W1Ctpela <> savW1Ctpela or
080800160516                 W1Cschgg <> savW1Cschgg;
080900120327               $InzS01   = *on;
081000120327               SavW1Cdes = W1Cdes;
081100160516               SavW1Ctpela = W1Ctpela;
081200160516               SavW1Cschgg = W1Cschgg;
081300120327             endif;
081400160516             if  W1Cdes = *blank and W1Ctpela = *blanks and
081500160516                 W1Cschgg = 0;
081600120327               clear  C1Ddes;
081700120327             else;
081800160516               C1Ddes = 'Ricerca:';
081900160516               IF  W1Cdes <> *blanks;
082000160516                 C1Ddes = %trim(C1Ddes) + ' Rag.Soc. ' + '''' +
082100160516                 %trim(W1Cdes) + '''';
082200160516               ENDIF;
082300160516               IF  W1Ctpela <> *blanks;
082400160516                 C1Ddes = %trim(C1Ddes) + ' Tipo Elab. ' + '''' +
082500160516                 W1Ctpela + '''';
082600160516               ENDIF;
082700160516               IF  W1Cschgg <> *zeros;
082800160516                 C1Ddes = %trim(C1Ddes) + ' Giorno Sched. ' +
082900160516                          '''' + %editc(W1Cschgg:'X') + '''';
083000160516               ENDIF;
083100120327             endif;
083200120327             leavesr;
083300120327
083400120327         EndSl;
083500120327
083600120327       ENDSR;
083700120424
083800120424       //--------------------------------------------------------------
083900120424       //?Gestione tasto funzionale F07 da videata C01 / S01           ?
084000120424       //?F07-Conta quanti cd al giorno                                ?
084100120424       //--------------------------------------------------------------
084200120424       BEGSR  sr_F07S01;
084300120424
084400120424         TNTB463R (kpjba);
084500120424
084600120424       ENDSR;
084700141201
084800141201       //--------------------------------------------------------------
084900141201       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
085000141201       //?F08-Int. C/E voce 199                                        ?
085100141201       //--------------------------------------------------------------
085200141201       BEGSR  sr_F08S01;
085300141201
085400141201         clear TREC60DS;
085500141201         D60op0 = ' 05';
085600141201         D60voc = 199;
085700141201         kpjbu = TREC60DS;
085800141201         TREC63R (kpjba);
085900141201
086000141201       ENDSR;
086100141117
086200141117       //--------------------------------------------------------------
086300141117       //?Gestione tasto funzionale F10 da videata C01 / S01           ?
086400141117       //?F10-Immissione                                   ?
086500141117       //--------------------------------------------------------------
086600141117       BEGSR  sr_F10S01;
086700141117
086800141117         clear TNTB46DS;
086900141117         kpjbu = TNTB46DS;
087000141117         tntb46r (kpjba);
087100141117         TNTB46DS = kpjbu;
087200141117         IF  B46fxx = '1';
087300141117           $Fine = *on;
087400141117           leavesr;
087500141117         ENDIF;
087600141117         $InzS01 = *on;
087700141117
087800141117       ENDSR;
087900120326
088000120326       //--------------------------------------------------------------
088100120326       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
088200120326       //?F11-Cambia ordinamento sfl                                   ?
088300120326       //--------------------------------------------------------------
088400120326       BEGSR  sr_F11S01;
088500120326
088600120326         Ord_x_KSU = (Not Ord_x_KSU);
088700120326
088800120326         select;
088900120326           // -?Subfile vuoto: nessun record da ordinare?
089000120326           when  S01nrr = *zero;
089100120326           // -?Subfile già posizionato: occorre ricaricarlo?
089200120326           when  C1Cksc <> *zero   or   C1Cksu <> *zero;
089300120326             $InzS01 = *on;
089400120326           // -?Altrimenti: basta riordinarlo?
089500120326           other;
089600120326             exsr  sr_SortSfl;
089700120326         endsl;
089800120326
089900120326         if  Not Ord_x_KSU;
090000120326           clear  C1Cksc;
090100120326           clear  SavC1Cksc;
090200120326         else;
090300120326           clear  C1Cksu;
090400120326           clear  SavC1Cksu;
090500120326         endif;
090600120326
090700120326       ENDSR;
090800120326
090900120326       //--------------------------------------------------------------
091000120326       //?Ordinamento subfile                                          ?
091100120326       //?  This subroutine sorts the subfile records.                 ?
091200120326       //--------------------------------------------------------------
091300120326       BEGSR  sr_SortSfl;
091400120326
091500120326         RrnLast  = S01nrr;
091600120326
091700120326         C1RcdNbr = 1;
091800120326         clear  C1CsrRrn;
091900120326         clear  S01nrr;
092000120326         clear  V1Dmsg;
092100120326         ErrMessage  = *off;
092200120326         SflNxtChg   = *off;
092300120326         ErrGenerico = *off;
092400120326         %subst(IndDspF : 50) = *off;
092500120326
092600120326         //?___________________________________________________________?
092700120326         //?Initialize the key fields to sort on.?
092800120326         //?There is one extra field not in the subfile -- Selected --?
092900120326         //?that is added to the record. This field is used to place?
093000120326         //?selected records in front of nonselected records.?
093100120326         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
093200120326
093300120326         clear  QLGKL;
093400120326         clear  QLGSKL;
093500120326         clear  QLGSCB;
093600120326         clear  QLGSCB00;
093700120326
093800120326         // -?Gestione della scelta ordinamento:?
093900120326         // -?Ordinamento per Padre/Codice?
094000120326         If  Ord_x_KSU = *on;
094100120326           // ·?2 campi chiave?
094200120326           QLGNBRK  = 2;
094300120326           // ·?1° campo: padre (S1Cksu)?
094400120326           QLGSP    = 1 + %size(S1Copz) + %size(S1Ccod) + %size(S1Cdes);
094500120326           QLGSS    = %size(S1Cksu);
094600120326           QLGDT    = Carattere;
094700120326           QLGSO    = Ascendente;
094800120326           QLGKL(1) = QLGSKL;
094900120326           // ·?2° campo: codice (S1Ccod)?
095000120326           QLGSP    = 1 + %size(S1Copz);
095100120326           QLGSS    = %size(S1Ccod);
095200120326           QLGDT    = Carattere;
095300120326           QLGSO    = Ascendente;
095400120326           QLGKL(2) = QLGSKL;
095500120326
095600120326         // -?Ordinamento per Cliente?
095700120326         Else;
095800120326           // ·?1 campi chiave?
095900120326           QLGNBRK  = 1;
096000120326           // ·?1° campo: cliente (S1Ccli)?
096100120326           //            ?a posizone    2,  7 byte, char, ascending.?
096200120326           QLGSP    = 1 + %size(S1Copz);
096300120326           QLGSS    = %size(S1Ccod);
096400120326           QLGDT    = Carattere;
096500120326           QLGSO    = Ascendente;
096600120326           QLGKL(1) = QLGSKL;
096700120326
096800120326         EndIf;
096900120326
097000120326         // -?Load other sort parameters.?
097100120326         QLGLB   = 80 + 16 * MaxKey;
097200120326         QLGRL   = %size(SflRcd) - 1;
097300120326         QLGRT   = 8;
097400120326         QLGOKL  = 80;
097500120326         QLGLKE  = 16;
097600120326         QLGLSS  = 290;
097700120326         // -?Initialize Sort I/O API fields.?
097800120326         QLGRL00 = QLGRL;
097900120326         QLGRC00 = 1;
098000120326         clear  QUSEI;
098100120326         QUSBPRV = %size(QUSEC);
098200120326
098300120326         // -?First step - Initialize the sort routine.?
098400120326         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
098500120326                      SizeList : ReturnSize : QUSEC );
098600120326
098700120326         // -?Next step - Write records to I/O routine.?
098800120326         QLGRT00  = Put;
098900120326         For  Nrr = 1  To  RrnLast;
099000120326           chain  Nrr  TB461S01;
099100120326           // -?Solo le righe con Selected = 'Y' sono riordinate,?
099200120326           //  ?quindi per fare un ordinamento di tutte le righe?
099300120326           //  ?metto 'Y' sempre.?
099400120326           Selected = 'Y';
099500120326           clear  QUSEI;
099600120326           QUSBPRV  = %size(QUSEC);
099700120326           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
099800120326                         SizeList : NotUsed : QUSEC );
099900120326         EndFor;
100000120326
100100120326         // -?Next step - Signal end of input, clear subfile for reload.?
100200120326         QLGRT00 = EndPut;
100300120326         clear  QUSEI;
100400120326         QUSBPRV = %size(QUSEC);
100500120326         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
100600120326                       SizeList : NotUsed : QUSEC );
100700120326
100800120326         // -?Pulizia subfile?
100900120326         SflDsp_N    = *on;
101000120326         SflDspCtl_N = *on;
101100120326         write  TB461C01;
101200120326         SflDspCtl_N = *off;
101300120326         SflEnd      = *off;
101400120326
101500120326         // -?Final step - Write the records back to the subfile.?
101600120326         QLGRT00  = Get;
101700120326         For  Nrr = 1  To RrnLast;
101800120326           clear  QUSEI;
101900120326           QUSBPRV = %size(QUSEC);
102000120326           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
102100120326                         QLGRL00  : NotUsed : QUSEC );
102200120326           // -?Ripristino indicatori utilizzati nel sfl rec?
102300120326           SflNxtChg = (S1Copz <> *blank);
102400120326           S01nrr = Nrr;
102500120326           write  TB461S01;
102600120326         EndFor;
102700120326
102800120326         C1CsrRrn = 1;
102900120326
103000120326         // -?Visualizzazione del SFL (se ci sono dati)?
103100120326         SflDsp_N = (S01nrr <= *zero);
103200120326
103300120326         // -?Attivazione del SFLEND?
103400120326         SflEnd = $EoF;
103500120326
103600120326       ENDSR;
103700120326
103800120326       //--------------------------------------------------------------
103900120326       //?Gestione opzioni subfile S01.                                ?
104000120326       //--------------------------------------------------------------
104100120326       BEGSR  sr_OpzS01;
104200120326
104300120326         clear  SavS01csr;
104400120326
104500120326         // -?Ciclo di lettura subfile?
104600120326         readc  TB461S01;
104700120326
104800120326         DoW  Not %eof(TNTB461D);
104900120326
105000120326           SflNxtChg = *off;
105100120326           %subst(IndDspF : 50) = *off;
105200120326           C1RcdNbr = S01nrr;
105300120326
105400120326           select;
105500120326
105600120326             // -?Opz. 1=Selezione?
105700141128             when  S1Copz = '1'   and   Not $Fine and Richiamato;
105800120326               clear  P01rit;
105900120326               P01ke1 = %editc( S1Ccod : 'X' );
106000120326               clear  P01ke2;
106100120326               $Fine   = *on;
106200120327
106300120327             // -?Opz. 3=Copia?
106400141117             // -?Opz. 2=Modifica/5=Visualizza?
106500141117               WHEN  (S1Copz = '2' or S1Copz = '3' or S1Copz = '5') and
106600141128                      not $Fine and not Richiamato;
106700151130               IF S1COPZ = '5' or S1CATB = *blank;
106800141117               clear TNTB46DS;
106900141117               B46opz = S1Copz;
107000141117               B46ke1 = %editc(S1Ccod:'X');
107100141117               kpjbu = TNTB46DS;
107200141117               tntb46r (kpjba);
107300141117               TNTB46DS = kpjbu;
107400141117               IF  B46fxx = '1';
107500141117                 $Fine = *on;
107600141117               ENDIF;
107700151130               ELSE;
107800151130                 IF S1CATB <> *blank;
107900151130                   ErrMessage  = *on;
108000151130                   ErrGenerico = *on;
108100151130                   PosCurOPZ   = *on;
108200151130                   V1Dmsg = $Msg(08);
108300151130                   $Fine  = *off;
108400151130                 ENDIF;
108500151130               ENDIF;
108600151130
108700151130             // -?Opz. 7=Ripristino
108800151130               WHEN  S1Copz = '7' and
108900151130                      not $Fine and not Richiamato;
109000151130                 IF S1CATB <> *blank;
109100151130                   clear TNTB46DS;
109200151130                   B46opz = S1Copz;
109300151130                   B46ke1 = %editc(S1Ccod:'X');
109400151130                   kpjbu = TNTB46DS;
109500151130                   tntb46r (kpjba);
109600151130                   TNTB46DS = kpjbu;
109700151130                   IF  B46fxx = '1';
109800151130                     $Fine = *on;
109900151130                   ENDIF;
110000151130                 ELSE;
110100151130                   ErrMessage  = *on;
110200151130                   ErrGenerico = *on;
110300151130                   PosCurOPZ   = *on;
110400151130                   V1Dmsg = $Msg(07);
110500151130                   $Fine  = *off;
110600151130                 ENDIF;
110700120326
110800120326             // -?Nessuna opzione?
110900120326             when  S1Copz = *blank;
111000120327
111100120327             // -?Ammessa una sola opzione?
111200120327             when  S1Copz <> *blank   and   $Fine;
111300120327               ErrMessage  = *on;
111400120327               ErrGenerico = *on;
111500120327               PosCurOPZ   = *on;
111600120327               V1Dmsg = $Msg(02);
111700120327               $Fine  = *off;
111800120326
111900120326             // -?Opzione errata?
112000120326             other;
112100120326               ErrMessage  = *on;
112200120326               ErrGenerico = *on;
112300120326               PosCurOPZ   = *on;
112400120326               V1Dmsg = $Msg(01);
112500120326               $Fine  = *off;
112600120326
112700120326           endsl;
112800120326
112900120326           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
113000120326           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
113100120326           if  S1Copz  <> *blank   and
113200120326              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
113300120326             SavS01csr   = C1RcdNbr;
113400120326           endif;
113500120326
113600120326           // -?Aggiornamento sfl?
113700120326           select;
113800120326             when  ErrMessage;
113900120326               SflNxtChg = *on;
114000120326               C1CsrRrn  = C1RcdNbr;
114100120326             when  ErrGenerico;
114200120326               C1CsrRrn  = C1RcdNbr;
114300120326               if  S1Copz <> '1';
114400120326                 clear  S1Copz;
114500120326               endif;
114600120326             other;
114700120326               if  S1Copz <> '1';
114800120326                 clear  S1Copz;
114900120326               endif;
115000120326           endsl;
115100120326
115200120326           if  S1Copz <> *blank;
115300120326             SflNxtChg = *on;
115400120326           endif;
115500120326
115600120326           UPDATE  TB461S01;
115700120326
115800120326           if  ErrGenerico   or   ErrMessage;
115900120326             leave;
116000120326           endif;
116100120326
116200120326           readc  TB461S01;
116300120326
116400120326         ENDDO;
116500120326
116600120326         // -?Riposizionam. cursore sul record contenente la prima opz.?
116700120326         //  ?(se non sono stati rilevati errori)?
116800120326         if  NOT ErrMessage   and   NOT ErrGenerico   and
116900120326             SavS01csr > *zero;
117000120326           C1CsrRrn = SavS01csr;
117100141201           $InzS01 = *on;
117200120326         endif;
117300141201
117400120326
117500120326       ENDSR;
117600120326
117700120326       //--------------------------------------------------------------
117800120326       //?Operazioni finali.                                           ?
117900120326       //--------------------------------------------------------------
118000120326       BEGSR  sr_RoutEnd;
118100120327
118200120327         // -?Restituzione parametri?
118300120327         kpjbu = Param01;
118400120326
118500120326         // -?"Chiusura" *pgm?
118600120326         return;
118700120326
118800120326       ENDSR;
118900120326
119000120326      /end-free
119100120326
119200120326       //--------------------------------------------------------------
119300120326       //?Schiere a tempo di compilazione.                             ?
119400120326       //--------------------------------------------------------------
119500120326
119600120326** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
119700120326Opzione errata                                                                 1
119800120326Ammessa la selezione di un solo cliente                                        2
119900141117Immettere il codice cliente                                                    3
120000141117Immettere SOLO caratteri numerici                                              4
120100141117Codice errato                                                                  5
120200141117Cliente annullato                                                              6
120300151130Ripristino valido solo per codici annullati                                    7
120400151130Modifica e copia valide solo per codici NON annullati                          8
