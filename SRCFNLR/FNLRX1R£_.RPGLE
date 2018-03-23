000100131030       //==============================================================
000200131121       //?Gestione Telefonate ad Autotrasportatori.                    ?
000300131030       //==============================================================
000400131030
000500131030       //--------------------------------------------------------------
000600131030       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700131030       //--------------------------------------------------------------
000800131030
000900131030     /*PRM dbgview(*source)
001000131030     /*END
001100131030
001200131030       //--------------------------------------------------------------
001300131030       //?Specifiche di controllo.                                     ?
001400131030       //--------------------------------------------------------------
001500131030
001600131030     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001700131030
001800131030       //--------------------------------------------------------------
001900131030       //?Dichiarazione file.                                          ?
002000131030       //--------------------------------------------------------------
002100131030
002200131030       // -?Organigramma?
002300131030     fAZORG01L  if   e           k disk
002400131030
002500131121       // -?Gestione telefonate ad AUT?
002600140205     fFICAU02L  if   e           k disk
002700131030
002800131030       // -?Anagrafica Autotrasportatori/Cooperative?
002900131121     fFIAPD01L  if   e           k disk
003000131121
003100131121       // -?Estensione Anagrafica Autotrasportatori - dati aggiuntivi?
003200131204     f*//FIAPD41L  if   e           k disk
003300131121
003400131121       // -?Bolle Arrivi: Testata?
003500131212     f*//FNARB01L  if   e           k disk
003600131121
003700131121       // -?Distinte Uscita AUT Consegne/Ritiri?
003800131204     fFIDST09L  if   e           k disk
003900131121
004000131121       // -?Ordini Ritiro Merce?
004100131212     f*//FNORM01L  if   e           k disk
004200131030
004300131030       // -?Video?
004400131104     fFNLRX1D   cf   e             workstn
004500131104     f                                     sfile(LRX1S01 : S01nrr)
004600131030     f                                     indds(IndDspF)
004700131030     f                                     infds(InfDspF)
004800131030
004900131030       //--------------------------------------------------------------
005000131030       //?Definizione costanti.                                        ?
005100131030       //--------------------------------------------------------------
005200131030
005300131121       // -?Tipi Oggetto Telefonate?
005400131121     d c_Consegna      c                   const('C')
005500131121     d c_Ritiro        c                   const('R')
005600131121
005700131121       // -?Numero di record in ciascuna pagina di subfile?
005800131121     d c_SflPag        c                   const(16)
005900131104
006000131104       // -?Numero massimo di record gestibili nel subfile?
006100131104     d c_MaxRec        c                   const(9999)
006200131121
006300131121       // -?Costante per controllo "caratteri solo numerici"?
006400131121     d c_Digits        c                   const('0123456789')
006500140121
006600140121       // -?Costanti per conversione caratteri maiuscoli in minuscoli e viceversa?
006700140121     d c_Up            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
006800140121     d c_Lo            c                   const('abcdefghijklmnopqrstuvwxyz')
006900131030
007000131030       // -?Tasti funzionali a video?
007100131030     d c_F01           c                   const(x'31')
007200131030     d c_F02           c                   const(x'32')
007300131030     d c_F03           c                   const(x'33')
007400131030     d c_F04           c                   const(x'34')
007500131030     d c_F05           c                   const(x'35')
007600131030     d c_F06           c                   const(x'36')
007700131030     d c_F07           c                   const(x'37')
007800131030     d c_F08           c                   const(x'38')
007900131030     d c_F09           c                   const(x'39')
008000131030     d c_F10           c                   const(x'3A')
008100131030     d c_F11           c                   const(x'3B')
008200131030     d c_F12           c                   const(x'3C')
008300131030     d c_F13           c                   const(x'B1')
008400131030     d c_F14           c                   const(x'B2')
008500131030     d c_F15           c                   const(x'B3')
008600131030     d c_F16           c                   const(x'B4')
008700131030     d c_F17           c                   const(x'B5')
008800131030     d c_F18           c                   const(x'B6')
008900131030     d c_F19           c                   const(x'B7')
009000131030     d c_F20           c                   const(x'B8')
009100131030     d c_F21           c                   const(x'B9')
009200131030     d c_F22           c                   const(x'BA')
009300131030     d c_F23           c                   const(x'BB')
009400131030     d c_F24           c                   const(x'BC')
009500131030     d c_Enter         c                   const(x'F1')
009600131030     d c_RollDown      c                   const(x'F4')
009700131030     d c_RollUp        c                   const(x'F5')
009800131030
009900131030       //--------------------------------------------------------------
010000131030       //?Definizione schiere.                                         ?
010100131030       //--------------------------------------------------------------
010200131030
010300131030       // -?Messaggi di errore?
010400131204     d sk_Msg          s             78    dim(14)  ctdata  perrcd( 1)
010500131030
010600131030       //--------------------------------------------------------------
010700131030       //?Definizione aree dati.                                       ?
010800131030       //--------------------------------------------------------------
010900131030
011000131030       // -?Dati utente?
011100131030     d §AzUte        e ds                  extname(AZUTE00F)
011200131030     d                                     dtaara
011300131030     d §DatiUte      e ds                  extname(dDatiUte)
011400131030     d                                     dtaara
011500131030
011600131030       //--------------------------------------------------------------
011700131030       //?Definizione strutture dati.                                  ?
011800131030       //--------------------------------------------------------------
011900131030
012000131030       // -?Status ds?
012100131030     d Status         sds
012200131030     d   SDSpgmName      *proc
012300131030
012400131030       // -?InfDS?
012500131030     d InfDspF         ds
012600131030     d   dsp_aid             369    369a
012700131030     d   sfl_rrn             376    377i 0
012800131030     d   min_nrr             378    379i 0
012900131030     d   num_rcds            380    381i 0
013000131030
013100131030       // -?Indicatori su DspF?
013200131030     d IndDspF         ds                  inz
013300131030         // -?Abilitazione tasti funzionali?
013400131205     d   F11Attivo                     n   overlay(IndDspF : 11)
013500131125     d   F12Attivo                     n   overlay(IndDspF : 12)
013600131030     d   F18Attivo                     n   overlay(IndDspF : 18)
013700131030         // -?Emissione messaggio di errore?
013800131030     d   ErrMessage                    n   overlay(IndDspF : 28)
013900131030         // -?Indicatori di gestione del subfile 1?
014000131030     d   SflDsp_N                      n   overlay(IndDspF : 30)
014100131030     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
014200131030     d   SflNxtChg                     n   overlay(IndDspF : 32)
014300131030     d   SflEnd                        n   overlay(IndDspF : 33)
014400131030         // -?Indicatori per Attibuti di visualizzazione?
014500131203     d   ProtectFGS                    n   overlay(IndDspF : 40)
014600131204     d   VisualTDIS                    n   overlay(IndDspF : 41)
014700131204     d   Ord_x_Dist                    n   overlay(IndDspF : 42)
014800131030         // -?Posizionamento cursore & segnalazione errore?
014900131121     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015000131030     d   PosCurFGS                     n   overlay(IndDspF : 51)
015100131204     d   PosCurNDCI                    n   overlay(IndDspF : 52)
015200131204     d   PosCurNDCF                    n   overlay(IndDspF : 53)
015300131204     d   PosCurAUT                     n   overlay(IndDspF : 54)
015400131030         //   -?Riemissione videata?
015500131030     d   ErrGenerico                   n   overlay(IndDspF : 99)
015600131121
015700131121       // -?DS per Numero Spedizione?
015800131125     d ds_Spedizione   ds                  inz   qualified
015900131125     d   wLNP                         3s 0 inz
016000131125     d   wNRS                         2s 0 inz
016100131125     d   wNSP                         7s 0 inz
016200131125     d   wAAS                         4s 0 inz
016300131121
016400131121       // -?DS per Numero O.R.M.?
016500131211     d ds_ORM          ds                  inz   qualified
016600131211     d   wPOE                         3s 0 inz
016700131211     d   wNSR                         2s 0 inz
016800131211     d   wNOR                         7s 0 inz
016900131211     d   wNRV                         2s 0 inz
017000140207
017100140207       // -?Parametri ricevuti?
017200140207     d ds_FilOkTel     ds           256    inz
017300140207     d   sk_FilOkTel           1    255    dim(85)
017400131030
017500131030       // -?Parametri ricevuti?
017600131030     d KPJBA         e ds
017700131204
017800131204       // -?150ª descrizione dell'organigramma?
017900131204     d Og150         e ds
018000131121
018100131121       // -?DS Tipo record "TEL" di FIAPD40F?
018200131204     d*// dAPD4tel      e ds                  inz   qualified
018300140121
018400140121       // -?DS campo DSTFLR di FIDST00F?
018500140121     d dDSTflr       e ds                  inz   qualified
018600131030
018700131030       //--------------------------------------------------------------
018800131030       //?Definizione variabili globali.                               ?
018900131030       //--------------------------------------------------------------
019000131030
019100131030       // -?Flags booleani?
019200131030     d $Fine           s               n   inz
019300131030     d $EoF            s               n   inz
019400131030     d $InzD01         s               n   inz(*on)
019500131030     d $InzS01         s               n   inz(*on)
019600131121     d $RecOK          s               n   inz
019700131204     d $DistChiusa     s               n   inz
019800131204     d $DistChiusaC    s               n   inz
019900131204     d $DistChiusaR    s               n   inz
020000131030
020100131030       // -?Variabili per la gestione del video?
020200131030     d $Video          s              2    inz('D1')
020300131030     d S01nrr          s                   like(C1RcdNbr) inz
020400131030     d SavS01csr       s                   like(C1RcdNbr) inz
020500131205     d SavMAXnrr       s                   like(C1RcdNbr) inz
020600131203     d wOrdSfl         s              1  0 inz
020700131203
020800131203       // -?Variabili per la gestione del singolo campo alla variazione?
020900131203       //  ?dello stesso?
021000131203     d SavC1Daut       s                   like(C1Daut)   inz(*hival)
021100131204
021200131204       // -?Campi di comodo?
021300131204     d wDate           s              8  0 inz
021400131204     d wFGS            s                   like(DSTfgs)   inz
021500131204     d wNDC            s                   like(DSTnfv)   inz
021600131204     d W1Caut          s                   like(DSTpdr)   inz
021700131204     d W1ndcI          s                   like(V1ndcF)   inz
021800131203
021900131203       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
022000131203
022100131203       //--------------------------------------------------------------
022200131203       // -?Constants?
022300131203       //--------------------------------------------------------------
022400131203       // -?MaxKey - Maximum number of key fields allowed by this program?
022500131203     d MaxKey          c                   const(2)
022600131203       // -?Sort order:?
022700131203       //  ?1 = Sort in an ascending sequence?
022800131203       //  ?2 = Sort in a descending sequence?
022900131203     d Ascendente      c                   const(1)
023000131203     d Discendente     c                   const(2)
023100131203       // -?Key data type:?
023200131203       //  ? 0 = Signed binary?
023300131203       //  ? 2 = Signed zoned decimal?
023400131203       //  ? 3 = Signed packed decimal?
023500131203       //  ? 6 = Character with no national language sort sequence applied,?
023600131203       //  ?     if specified?
023700131203       //  ? 7 = Unsigned packed decimal?
023800131203       //  ?     All numbers will have the sign forced positive ('F'X).?
023900131203       //  ? 8 = Unsigned zoned decimal?
024000131203       //  ?     All numbers will have the sign forced positive ('F'X).?
024100131203       //  ? 9 = Unsigned binary?
024200131203       //  ?14 = Date in form of DD/MM/YY?
024300131203       //  ?15 = Date in form of DD.MM.YYYY?
024400131203     d Numero          c                   const(2)
024500131203     d Carattere       c                   const(6)
024600131203       //
024700131203     d Put             c                   const(1)
024800131203     d EndPut          c                   const(2)
024900131203     d Get             c                   const(3)
025000131203
025100131203       //--------------------------------------------------------------
025200131203       // -?Data Structures?
025300131203       //--------------------------------------------------------------
025400131203       //  ?SflRcd     - The entire subfile record to pass to the sort?
025500131203     d SflRcd          ds                  inz
025600131203     d   S1Hfgs                            inz
025700131203     d   S1HdtIns                          inz
025800131203     d   S1Htel                            inz
025900131203     d   S01opz                            inz
026000131203     d   S01pdr                            inz
026100131203     d   S01pdrD                           inz
026200131203     d   S01dst                            inz
026300131203     d   S01nrCo                           inz
026400131203     d   S01nrRi                           inz
026500131203     d   S01pda                            inz
026600140121     d   S01tst                            inz
026700131203     d   Selected                     1a   inz
026800131203       //  ?QLGSCB     - The sort request block for the QLGSORT API?
026900131203      /copy QSYSINC/QRPGLESRC,QLGSORT
027000131203       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
027100131203     d QLGKL                         16    dim(MaxKey)
027200131203     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
027300131203     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
027400131203     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
027500131203     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
027600131203       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
027700131203      /copy QSYSINC/QRPGLESRC,QLGSRTIO
027800131203       //  ?QUSEC      - Error structure for the QLGSORT API?
027900131203      /copy QSYSINC/QRPGLESRC,QUSEC
028000131203
028100131203       //--------------------------------------------------------------
028200131203       // -?Standalone fields?
028300131203       //--------------------------------------------------------------
028400131203     d NotUsed         s             16a   inz
028500131203       //  ?ReturnSize - Size of list returned by sort API?
028600131203     d ReturnSize      s              9b 0 inz
028700131203       //  ?SizeList   - Size of list?
028800131203     d SizeList        s              9b 0 inz
028900131203       //  ?Nrr        - Relative record number for subfile?
029000131203     d Nrr             s              5i 0 inz
029100131203     d RrnLast         s              5i 0 inz
029200131030
029300131030       //--------------------------------------------------------------
029400131030       //?Definizione prototipi procedure.                             ?
029500131030       //--------------------------------------------------------------
029600131030
029700131030       // -?Reperimento dati utente?
029800131030     d TIBS34ds      e ds                  inz
029900131030      /copy gaitrasrc/srcProtoPR,TIBS34R
030000131121
030100131121       // -?Caricamento filiali gestite dall'utente?
030200131121     d TRUL31ds      e ds                  inz
030300131121     d   sk_POg               10    759    inz   dim(250)
030400131121      /copy gaitrasrc/srcProtoPR,TRUL31R
030500140207
030600140207       // -?Parametri per Ricerca/Controllo tabelle (TNTBE)?
030700140207     d TIBS02ds      e ds                  inz
030800140207     d   T02tla      e                     inz('L')
030900140207     d   T02mod      e                     inz('C')
031000140207     d   T02cod      e                     inz('VPO')
031100140207     d   T02ke1      e                     inz('DECOFI816TEL')
031200140207       // -?Ricerca/Controllo tabelle (TNTBE)?
031300140207      /copy gaitrasrc/srcProtoPR,TIBS02R
031400131204
031500131204       // -?Interrogazione/Ricerca Distinte di Consegna?
031600140113     d sk_FV6          s              6p 0 inz   dim(600)
031700131204     d FIDG31ds      e ds                  inz        qualified
031800131204     d   FDGric      e                     inz('S')
031900131204     d   FDGtfv      e                     inz('2')
032000131204     d   FDGtpv      e                     inz('P')
032100131204     d   FDGopz      e                     inz('1')
032200131204      /copy gaitrasrc/srcProtoPR,FIDG31R
032300131204
032400131204       // -?Selezione AUT?
032500131204     d FNLV24ds      e ds                  inz        qualified
032600131204     d   D24tip      e                     inz('A')
032700131204     d   D24flg      e                     inz('R')
032800131204      /copy gaitrasrc/srcProtoPr,FNLV24R
032900131204
033000131204       // -?Controllo Fase della Distinta?
033100131204     d FIOR90ds      e ds                  inz        qualified
033200131204     d   OR90tla     e                     inz('C')
033300131204      /copy gaitrasrc/srcProtoPR,FIOR90R
033400131121
033500131121       // -?Gestione Telefonate AUT (dettaglio)?
033600131121     d FNLRX2ds      e ds                  inz        qualified
033700131121     d   oX2upd      e                     inz(*off)
033800131121     d   oX2err      e                     inz(*off)
033900131121      /copy gaitrasrc/srcProtoPR,FNLRX2R
034000131203
034100131203       // -?Ordinamento subfile?
034200131203      /copy gaitrasrc/srcProtoPr,QLGSRTIO
034300131030
034400131030       //--------------------------------------------------------------
034500131030       //?Definizione key-list.                                        ?
034600131030       //--------------------------------------------------------------
034700131030
034800131030       // -?File AZORG01L?
034900131030     d keyAZORG01    e ds                  extname(AZORG01L : *key)
035000131030     d                                     prefix(k_)   inz
035100131030
035200140205       // -?File FICAU02L?
035300140205     d keyFICAU02    e ds                  extname(FICAU02L : *key)
035400131030     d                                     prefix(k_)   inz
035500131030
035600131030       // -?File FIAPD01L?
035700131030     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
035800131030     d                                     prefix(k_)   inz
035900131121
036000131121       // -?File FIAPD41L?
036100131204     d*// keyFIAPD41    e ds                  extname(FIAPD41L : *key)
036200131204     d*//                                     prefix(k_)   inz
036300131121
036400131204       // -?File FIDST09L?
036500131204     d keyFIDST09    e ds                  extname(FIDST09L : *key)
036600131121     d                                     prefix(k_)   inz
036700131212
036800131212       // -?File FNARB01L?
036900131212     d*// keyFNARB01    e ds                  extname(FNARB01L : *key)
037000131212     d*//                                     prefix(k_)   inz
037100131211
037200131211       // -?File FNORM01L?
037300131212     d*// keyFNORM01    e ds                  extname(FNORM01L : *key)
037400131212     d*//                                     prefix(k_)   inz
037500131030
037600131030       //--------------------------------------------------------------
037700131030       //?Riepilogo indicatori utilizzati.                             ?
037800131030       //--------------------------------------------------------------
037900131030
038000131030
038100131030       //--------------------------------------------------------------
038200131030       //?M A I N - L I N E                                            ?
038300131030       //--------------------------------------------------------------
038400131030
038500131030     c     *Entry        plist
038600131030     c                   parm                    KPJBA
038700131030
038800131030      /free
038900131030
039000131030       // -?Operazioni iniziali?
039100131030       exsr sr_RoutInz;
039200131030
039300131030       // -?Ciclo di gestione del file video?
039400131030       DoW  $Fine = *off;
039500131030
039600131030         select;
039700131104
039800131104           // -?Richiesta parametri iniziali?
039900131104           when $Video = 'D1';
040000131104             exsr  sr_GesD01;
040100131030
040200131030           // -?Gestione videata S1 (subfile x filiale)?
040300131030           when $Video = 'S1';
040400131030             exsr sr_GesS01;
040500131030
040600131030           // -?? ? ??
040700131030           other;
040800131030             $Fine = *on;
040900131030
041000131030         endsl;
041100131030
041200131030       EndDo;
041300131030
041400131030       // -?Operazioni finali?
041500131030       exsr sr_RoutEnd;
041600131030
041700131030       //--------------------------------------------------------------
041800131030       //?Operazioni iniziali.                                         ?
041900131030       //--------------------------------------------------------------
042000131030       BEGSR  sr_RoutInz;
042100131030
042200131030         // -?Impostazione chiusura?
042300131030         *inLR = *on;
042400131030
042500131030         // -?Reperimento dati job?
042600131030         exsr  sr_DatiJob;
042700131104
042800131104         // -?Impostazione nome programma a video?
042900131104         V1Tpgm = SDSpgmName;
043000131204
043100131204         // -?Reperimento data odierna?
043200131204         wDate = %int( %subst( %char( %dec( %timestamp() ) )
043300131204                               : 1 : 8 ) );
043400140207
043500140207
043600140207         // -?Caricamento filiali abilitate al progetto 816 Telefonate AUT?
043700140207         reset  TIBS02DS;
043800140207         //T02mod = 'C';            ?(già così)?
043900140207         //T02cod = 'VPO';          ?(già così)?
044000140207         //T02ke1 = 'DECOFI816TEL'; ?(già così)?
044100140207         T02sif = KNSIF;
044200140207         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
044300140207         if  T02err = *blanks;
044400140207           ds_FilOkTel = T02uni;
044500140207         endif;
044600140207
044700140207         // -?Ctrl SE la filiale distinta è attiva per le telefonate AUT?
044800140207         if  sk_FilOkTel(1) <> '999'  and
044900140207             %lookup( %editc( DUTpou : 'X' ) : sk_FilOkTel ) = *zero;
045000140207           $Fine = *on;
045100140207           leavesr;
045200140207         endif;
045300140207
045400131121
045500131121         // -?Caricamento filiali gestibili dall'utente?
045600131121         clear  TRUL31ds;
045700131121         I31abi = UTEaut;
045800131121         I31cdi = DUTdis;
045900131121         I31car = DUTare;
046000131121         I31cpo = DUTpou;
046100131204         trul31r ( kpjbA : TRUL31ds );
046200131121         // -?In caso di errore si uscirà DOPO l'emissione della?
046300131121         //  ?videata D01?
046400131121         //if  O31pog <= *zero;
046500131121         //  $Fine = *on;
046600131121         //endif;
046700131030
046800131030         // -?Pulizia / impostazione iniziale dei campi chiave?
046900131030         clear  keyAZORG01;
047000140205         clear  keyFICAU02;
047100131030         clear  keyFIAPD01;
047200131204         //clear  keyFIAPD41;
047300131204         clear  keyFIDST09;
047400131212         //clear  keyFNARB01;
047500131212         //clear  keyFNORM01;
047600131121         k_APDtip   = 'A';
047700131204         //k_APD4tipA = 'A';
047800131204         //k_APD4trd  = 'TEL';
047900131121         k_DSTnpg   = 4;
048000131030
048100131030       ENDSR;
048200131030
048300131030       //--------------------------------------------------------------
048400131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
048500131030       //--------------------------------------------------------------
048600131030       BEGSR  sr_DatiJob;
048700131030
048800131030         in(e) §AzUte;
048900131030         if NOT %error;
049000131030           in(e) §DatiUte;
049100131030         endif;
049200131030         if %error or RSut = *blank;
049300131030           tibs34r ( tibs34ds );
049400131030           in §AzUte;
049500131030           in §DatiUte;
049600131030         endif;
049700131030
049800131030       ENDSR;
049900131030
050000131030       //--------------------------------------------------------------
050100131030       //?Gestione videata D01.                                        ?
050200131030       //--------------------------------------------------------------
050300131030       BEGSR  sr_GesD01;
050400131030
050500131030         // -?Inizializzazione videata?
050600131030         if  $InzD01 = *on;
050700131030           exsr  sr_InzD01;
050800131030           $InzD01 = *off;
050900131030         endif;
051000131030
051100131030         // -?(Dis)Abilitazione tasti funzionali?
051200131104         F18Attivo = (ProtectFGS = *on);
051300131030
051400131030         // -?Emissione videata?
051500131104         write  LRX1T01;
051600131104         exfmt  LRX1D01;
051700131030
051800131030         clear  V1Dmsg;
051900131030         reset  ErrMessage;
052000131030         reset  ErrGenerico;
052100131030
052200131030         SELECT;
052300131121
052400131121           // -?Nessuna filiale gestibile dall'utente?
052500131121           WHEN  O31pog <= *zero;
052600131121             $Fine = *on;
052700131030
052800131030           // -?F3=Fine?
052900131030           WHEN  dsp_aid = c_F03;
053000131030             $Fine = *on;
053100131030
053200131104           // -?F18=Cambio Filiale?
053300131104           WHEN  dsp_aid = c_F18;
053400131030             ProtectFGS = *off;
053500131030
053600131030           // -?Invio?
053700131030           OTHER;
053800131030             exsr  sr_CtrD01;
053900131030             if  ErrGenerico = *on;
054000131030               leavesr;
054100131030             endif;
054200131030             ProtectFGS = *on;
054300131030             $Video  = 'S1';
054400131030             $InzS01 = *on;
054500131030
054600131030         ENDSL;
054700131030
054800131030       ENDSR;
054900131104
055000131104       //--------------------------------------------------------------
055100131104       //?Inizializzazione videata D01.                                ?
055200131104       //--------------------------------------------------------------
055300131104       BEGSR  sr_InzD01;
055400131104
055500131104         clear  LRX1D01;
055600131104
055700131121         V1Cfgs = DUTpou;
055800131204         //V1Dfgs = RSut;
055900131204         V1Dfgs = DUTdpo;
056000131204         V1ndcF = *hival;
056100131204
056200131204         // -?Decodifica Filiale Gestione?
056300131204         exsr  sr_CtrD01;
056400131204         %subst(IndDspF : 50) = *off;
056500131204         reset  ErrGenerico;
056600131204         reset  ErrMessage;
056700131204         clear  V1Dmsg;
056800131205
056900131205         // -?Impostazione Attributi di Visualizzazione?
057000131205         ProtectFGS = *on;
057100131205         VisualTDIS = (§OGpkm = 'S');
057200131104
057300131104       ENDSR;
057400131104
057500131104       //--------------------------------------------------------------
057600131104       //?Controllo dati nella videata D01.                            ?
057700131104       //--------------------------------------------------------------
057800131104       BEGSR  sr_CtrD01;
057900131204
058000131104         // -?Spegnimento indicatori di posizionamento cursore?
058100131205         %subst(IndDspF : 50) = *off;
058200131204
058300131204         clear  Og150;
058400131121
058500131121         // -?Nessuna filiale gestibile dall'utente?
058600131121         if  O31pog <= *zero;
058700131121           ErrMessage  = *on;
058800131121           ErrGenerico = *on;
058900131121           V1Dmsg = sk_Msg(01);
059000131121           leavesr;
059100131121         endif;
059200131104
059300131104         // -?Controllo filiale in gestione?
059400131104         clear  V1Dfgs;
059500131104         select;
059600131121           // -?obbligatoria?
059700131121           when  V1Cfgs  = *zero;
059800131104             ErrGenerico = *on;
059900131104             ErrMessage  = *on;
060000131104             PosCurFGS   = *on;
060100131121             V1Dmsg = sk_Msg(02);
060200131104             leavesr;
060300131121           // -?controllo/decodifica?
060400131104           other;
060500131104             chain ( V1Cfgs )  AZORG;
060600131104             if  %found(AZORG01L)   and   ORGfva = *blank;
060700131104               V1Dfgs = ORGdes;
060800131204               Og150  = ORGdf0;
060900131121               if  %lookup( %editc( V1Cfgs : 'X' ) : sk_POg ) = *zero;
061000131121                 ErrGenerico = *on;
061100131121                 ErrMessage  = *on;
061200131121                 PosCurFGS   = *on;
061300131121                 V1Dmsg = sk_Msg(03);
061400131121                 leavesr;
061500131121               endif;
061600131104             else;
061700131121               V1Dfgs = *all'? ';
061800131104               ErrGenerico = *on;
061900131104               ErrMessage  = *on;
062000131104               PosCurFGS   = *on;
062100131121               V1Dmsg = sk_Msg(03);
062200131104               leavesr;
062300131104             endif;
062400131104         endsl;
062500131204
062600131204         // -?Controllo range di distinte / autotrasportatore?
062700131204         //  ?Se inserito uno, non può essere inserito anche l'altro?
062800131204         if  V1Caut > *zero  and
062900131204            (V1ndcI > *zero  or  (V1ndcF > *zero  and  V1ndcF <> *hival));
063000131204           ErrGenerico = *on;
063100131204           ErrMessage  = *on;
063200131204           PosCurNDCI  = *on;
063300131204           V1Dmsg = sk_Msg(04);
063400131204           leavesr;
063500131204         endif;
063600131204
063700131204         // -?Controllo Numeri Distinta?
063800131204         exsr  sr_CtrlDIST;
063900131204         if  ErrGenerico = *on;
064000131204           leavesr;
064100131204         endif;
064200131104
064300131204         // -?Controllo Autotrasportatore?
064400131204         exsr  sr_CtrlAUT;
064500131204         if  ErrGenerico = *on;
064600131204           leavesr;
064700131204         endif;
064800131104
064900131104       ENDSR;
065000131204
065100131204       //--------------------------------------------------------------
065200131204       //?Controllo Numeri Distinta.                                   ?
065300131204       //--------------------------------------------------------------
065400131204       BEGSR  sr_CtrlDIST;
065500140121
065600140121         clear  dDSTflr;
065700131204
065800131204         // -?Numero Distinta Dal?
065900131204         clear  W1ndcI;
066000131204         if V1ndcI = *zero;
066100131204           clear  V1ndcI;
066200131204         endif;
066300131204
066400131204         IF V1ndcI <> *blank;
066500131204
066600131204           // -?Ricerca?
066700131204           If  %scan( '?' : V1ndcI ) > *zero;
066800131204             reset  FIDG31ds;
066900131204             FIDG31ds.FDGfgs = V1Cfgs;
067000131204             FIDG31ds.FDGgda = wDate;
067100131204             FIDG31ds.FDGgal = wDate;
067200131211             //FIDG31ds.FDGric = 'S';      ?(già così)?
067300131211             //FIDH31ds.FDGtfv = '2';      ?(già così)?
067400131211             //FIDG31ds.FDGtpv = 'P';      ?(già così)?
067500131211             //FIDG31ds.FDGopz = '1';      ?(già così)?
067600131204             sk_FV6 = *hival;
067700131204
067800131204             kpjbU  = FIDG31ds;
067900131204
068000131204             FIDG31R ( kpjbA : sk_FV6 );
068100131204
068200131204             FIDG31ds = kpjbU;
068300131204
068400131204             if  FIDG31ds.FDGnfv = *zero;
068500131204               clear  FIDG31ds;
068600131204               clear  V1ndcI;
068700131204             else;
068800131204               V1ndcI = %editc( %dec( FIDG31ds.FDGnfv ) : 'X' );
068900131204             endif;
069000131204
069100131204             ErrGenerico = *on;
069200131204             PosCurNDCI  = *on;
069300131204             leavesr;
069400131204
069500131204           EndIf;
069600131204
069700131204           // -?Controllo numericità?
069800131204           if  %check( c_Digits : V1ndcI) > *zero;
069900131204             ErrGenerico = *on;
070000131204             ErrMessage  = *on;
070100131204             PosCurNDCI  = *on;
070200131204             V1Dmsg = sk_Msg(05);
070300131204             leavesr;
070400131204           endif;
070500131204
070600131204           W1ndcI = %int( V1ndcI );
070700131204
070800131204           // -?Controllo esistenza?
070900131204           k_DSTfgs = V1Cfgs;
071000131204           k_DSTnfv = W1ndcI;
071100131204           chain  %kds( keyFIDST09 )  FIDST000;
071200131204           if  Not %found(FIDST09L)  or  DSTatb <> *blank;
071300131204             ErrGenerico = *on;
071400131204             ErrMessage  = *on;
071500131204             PosCurNDCI  = *on;
071600131204             V1Dmsg = sk_Msg(06);
071700131204             leavesr;
071800131204           endif;
071900131204
072000131204           // -?Impostazione distinta finale = distinta iniziale?
072100131204           //  ?(SE NON impostata)?
072200131204           if  V1ndcF = *zero;
072300131204             V1ndcF = W1ndcI;
072400131204           endif;
072500131204
072600131204         ENDIF;
072700131204
072800131204         // -?Numero Distinta Al?
072900131204         // -?Deve essere impostato: con distinta  o  a 999999?
073000140205         if  W1ndcI = *zero   and  V1ndcF = *zero;
073100140205           V1ndcF = *hival;
073200140205         endif;
073300131204         if  V1ndcF <= *zero;
073400131204           ErrGenerico = *on;
073500131204           ErrMessage  = *on;
073600131204           PosCurNDCF  = *on;
073700131204           V1Dmsg = sk_Msg(07);
073800131204           leavesr;
073900131204         endif;
074000131204
074100131204         // -?Controllo compatibilità Range?
074200131204         if  W1ndcI > V1ndcF;
074300131204           ErrGenerico = *on;
074400131204           ErrMessage  = *on;
074500131204           PosCurNDCF  = *on;
074600131204           V1Dmsg = sk_Msg(08);
074700131204           leavesr;
074800131204         endif;
074900131204
075000131204         // -?Richiesta una sola distinta:?
075100131204         //  ?controllo SE distinta chiusa per consegne e ritiri?
075200131204         If  W1ndcI = V1ndcF;
075300131204           wFGS = V1Cfgs;
075400131204           wNDC = V1ndcF;
075500131204           exsr  sr_CtrlDistChius;
075600131204           if  $DistChiusa;
075700131204             ErrGenerico = *on;
075800131204             ErrMessage  = *on;
075900131204             PosCurNDCI  = *on;
076000131204             V1Dmsg = sk_Msg(09);
076100131204             leavesr;
076200131204           endif;
076300131204         EndIf;
076400131204
076500131204       ENDSR;
076600131204
076700131204       //--------------------------------------------------------------
076800131204       //?Controllo Autotrasportatore.                                 ?
076900131204       //--------------------------------------------------------------
077000131204       BEGSR  sr_CtrlDistChius;
077100131204
077200131204         $DistChiusa  = *off;
077300131204         $DistChiusaC = *off;
077400131204         $DistChiusaR = *off;
077500131204
077600140203         // -?SE c'è la fase RCH o la RQE è chiusa x i?RITIRI?
077700131204         reset  FIOR90ds;
077800131211         //FIOR90ds.OR90tla = 'C';         ?(già così)?
077900131204         FIOR90ds.OR90fas = 'RCH';
078000131204         FIOR90ds.OR90fgs = V1Cfgs;
078100131204         FIOR90ds.OR90ndc = V1ndcF;
078200131204
078300131204         FIOR90R ( kpjbA : FIOR90ds );
078400131204
078500131204         // -?SE trova la fase RCH: imposta che è chiusa?
078600131204         if  FIOR90ds.OR90ndcok <> *blank;
078700131204           $DistChiusaR = *on;
078800131204         endif;
078900131204
079000131204         // -?SE non trova la fase RCH: prova con la fase RQE?
079100131204         If  FIOR90ds.OR90ndcok = *blank;
079200131204
079300131204           reset  FIOR90ds;
079400131211           //FIOR90ds.OR90tla = 'C';       ?(già così)?
079500131204           FIOR90ds.OR90fas = 'RQE';
079600131204           FIOR90ds.OR90fgs = V1Cfgs;
079700131204           FIOR90ds.OR90ndc = V1ndcF;
079800131204
079900131204           FIOR90R ( kpjbA : FIOR90ds );
080000131204
080100131204           // -?SE trova la fase RQE: imposta che è chiusa?
080200131204           if  FIOR90ds.OR90ndcok <> *blank;
080300131204             $DistChiusaR = *on;
080400131204           endif;
080500131204
080600131204         EndIf;
080700131204
080800140203         // -?SE c'è la fase CRI: è chiusa x le?CONSEGNE?
080900131204         reset  FIOR90ds;
081000131211         //FIOR90ds.OR90tla = 'C';         ?(già così)?
081100131204         FIOR90ds.OR90fas = 'CRI';
081200131204         FIOR90ds.OR90fgs = V1Cfgs;
081300131204         FIOR90ds.OR90ndc = V1ndcF;
081400131204
081500131204         FIOR90R ( kpjbA : FIOR90ds );
081600131204
081700131204         // -?SE trova la fase CRI: imposto che è chiusa?
081800131204         if  FIOR90ds.OR90ndcok <> *blank;
081900131204           $DistChiusaC = *on;
082000131204         endif;
082100131204
082200131204         // -?SE non trovo la fase CRI: controllo se chiusa con flag?
082300131204         if  FIOR90ds.OR90ndcok = *blank  and  DSTfcf = 'S';
082400131204           $DistChiusaC = *on;
082500131204         endif;
082600131204
082700131204         // -?SE la distinta è chiusa x le consegne e x i ritiri:?
082800131204         //  ?imposta il flag?
082900131204         if  $DistChiusaR  and  $DistChiusaC;
083000131204           $DistChiusa = *on;
083100131204         endif;
083200131204
083300131204       ENDSR;
083400131204
083500131204       //--------------------------------------------------------------
083600131204       //?Controllo Autotrasportatore.                                 ?
083700131204       //--------------------------------------------------------------
083800131204       BEGSR  sr_CtrlAUT;
083900131204
084000131204         clear  W1Caut;
084100131204         clear  V1Daut;
084200131204
084300131204         // -?Autotrasportatore NON impostato?
084400131204         if  V1Caut = *zero;
084500131204           clear  V1Caut;
084600131204         endif;
084700131204         if  V1Caut = *blank;
084800131204           leavesr;
084900131204         endif;
085000131204
085100131204         // -?Ricerca?
085200131204         If  %scan( '?' : V1Caut ) > *zero;
085300131204
085400131204           clear  V1Caut;
085500131204           clear  V1Daut;
085600131204
085700131204           reset  FNLV24ds;
085800131204           FNLV24ds.D24fil = DUTpou;
085900131211           //FNLV24ds.D24tip = 'A';        ?(già così)?
086000131211           //FNLV24ds.D24flg = 'R';        ?(già così)?
086100131204
086200131204           kpjbU = FNLV24ds;
086300131204
086400131204           FNLV24R ( kpjbA );
086500131204
086600131204           FNLV24ds = kpjbU;
086700131204
086800131204           if FNLV24ds.D24pdr <> *zero;
086900131204             V1Caut = %editc( %int( FNLV24ds.D24pdr ) : 'X' );
087000131204             V1Daut = FNLV24ds.D24rsc;
087100131204           endif;
087200131204
087300131204           ErrGenerico = *on;
087400131204           PosCurAUT   = *on;
087500131204           leavesr;
087600131204
087700131204         EndIf;
087800131204
087900131204         // -?Controllo autotrasportatore?
088000131204         IF  V1Caut <> *blank  and  V1Caut <> *zero;
088100131204
088200131204           // -?Controllo numericità?
088300131204           if  %check( c_digits : V1Caut ) > *zero;
088400131204             ErrGenerico = *on;
088500131204             ErrMessage  = *on;
088600131204             PosCurAUT   = *on;
088700131204             V1Dmsg = sk_Msg(10);
088800131204             leavesr;
088900131204            endif;
089000131204
089100131204           // -?Controllo esistenza?
089200131211           //k_APDtip = 'A';          ?(già così)?
089300131204           k_APDpdr = %int( V1Caut );
089400131204           chain %kds(keyFIAPD01)  FIAPD000;
089500131204           if  %found(FIAPD01L)  and  APDatb = *blank;
089600131204             V1Daut = APDrsc;
089700131204           else;
089800131204             ErrGenerico = *on;
089900131204             ErrMessage  = *on;
090000131204             PosCurAUT   = *on;
090100131204             V1Dmsg = sk_Msg(11);
090200131204             leavesr;
090300131204           endif;
090400131204
090500131204           W1Caut = %int( V1Caut );
090600131204
090700131204         ENDIF;
090800131204
090900131204         // -?L'autotrasportatore deve essere in gestione all'utente?
091000131204         //  ?in base alla filiale gestione impostata a video?
091100131204         if  V1Cfgs <> %int( %subst( V1Caut : 1 : 3 ) );
091200131204           ErrGenerico = *on;
091300131204           ErrMessage  = *on;
091400131204           PosCurAUT   = *on;
091500131204           V1Dmsg = sk_Msg(12);
091600131204           leavesr;
091700131204         endif;
091800131204
091900131204       ENDSR;
092000131030
092100131030       //--------------------------------------------------------------
092200131030       //?Gestione subfile S01.                                        ?
092300131030       //--------------------------------------------------------------
092400131030       BEGSR  sr_GesS01;
092500131030
092600131030         // -?Inizializzazione videata?
092700131030         if  $InzS01 = *on;
092800131030           exsr  sr_InzS01;
092900131030           $InzS01 = *off;
093000131030         endif;
093100131125
093200131125         // -?(Dis)Abilitazione tasti funzionali?
093300131205         F11Attivo = (SavMAXnrr > *zero);
093400131127         F12Attivo = *on;
093500131204         //F12Attivo = (kpjbU = *blank);?(al ritorno da F12 non è più *blank)?
093600131030
093700131030         // -?Emissione Testata & Piede?
093800131104         write  LRX1T01;
093900131104         write  LRX1P01;
094000131030
094100131030         // -?Posizionamento cursore?
094200131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
094300131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
094400131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
094500131030         //  ?premuto l'ultimo tasto.?
094600131030         if  C1CsrRrn > *zero;
094700131030           C1RcdNbr = C1CsrRrn;
094800131030         else;
094900131030           C1RcdNbr = 1;
095000131127           write  LRX1S00;               //?(no rec.)?
095100131030         endif;
095200131030
095300131030         // -?Emissione videata?
095400131104         exfmt  LRX1C01;
095500131030
095600131030         reset  ErrGenerico;
095700131030         reset  ErrMessage;
095800131030         clear  V1Dmsg;
095900131030
096000131030         Select;
096100131030
096200131030           // -?F3=Fine?
096300131030           When  dsp_aid = c_F03;
096400131030             $Fine = *on;
096500131030
096600131030           // -?F5=Rivisualizza?
096700131030           When  dsp_aid = c_F05;
096800131030             $InzS01 = *on;
096900131203
097000131205           // -?F11=Ordinamento Sfl?
097100131205           When  dsp_aid = c_F11;
097200131205             exsr  sr_F11S01;
097300131104
097400131104           // -?F12=Ritorno?
097500131104           When  dsp_aid = c_F12;
097600131104             $Video = 'D1';
097700131104
097800131104           // -?Roll-Up?
097900131104           WHEN  dsp_aid = c_RollUp;
098000131211             If  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
098100131104               ErrGenerico = *on;
098200131104               ErrMessage  = *on;
098300131204               V1Dmsg = sk_Msg(13);
098400131104             EndIf;
098500131030
098600131030           // -?SubFile vuoto?
098700131205           When  SavMAXnrr = *zero;
098800131203             // -?Richiesto posizionamento?
098900131204             if  (wOrdSfl = *zero  and   C1Daut <> SavC1Daut);
099000131203               $InzS01 = *on;
099100131203             endif;
099200131030
099300131030           // -?Invio?
099400131030           Other;
099500131030             // -?Gestione opzioni?
099600131030             exsr  sr_OpzS01;
099700131030             if  ErrGenerico;
099800131030               leavesr;
099900131030             endif;
100000131203             // -?Richiesto posizionamento?
100100131204             if  (wOrdSfl = *zero  and   C1Daut <> SavC1Daut);
100200131203               $InzS01 = *on;
100300131203             endif;
100400131030
100500131030         endsl;
100600131030
100700131030       ENDSR;
100800131030
100900131030       //--------------------------------------------------------------
101000131030       //?Inizializzazione subfile S01.                                ?
101100131030       //--------------------------------------------------------------
101200131030       BEGSR  sr_InzS01;
101300131030
101400131030         // -?Spegnimento degli indicatori di errore?
101500131030         %subst(IndDspF : 50) = *off;
101600131125
101700131125         // -?Impostazione campi di "testata"?
101800131125         C1Cfgs = V1Cfgs;
101900131125         C1Dfgs = V1Dfgs;
102000131203
102100131203         // -?Pulizia subfile control?
102200131204         if  wOrdSfl = 1;
102300131203           clear  C1Daut;
102400131203           reset  SavC1Daut;
102500131203         endif;
102600131030
102700131030         // -?Pulizia subfile?
102800131030         SflDsp_N    = *on;
102900131030         SflDspCtl_N = *on;
103000131104         write  LRX1C01;
103100131030         SflDspCtl_N = *off;
103200131030         SflEnd      = *off;
103300131030
103400131030         clear  C1RcdNbr;
103500131030         clear  C1CsrRrn;
103600131030         clear  S01nrr;
103700131030         clear  V1Dmsg;
103800131030         ErrMessage  = *off;
103900131030         ErrGenerico = *off;
104000131030
104100131203         // -?Caricamento completo dei dati nel subfile?
104200131203         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
104300140203         //  ?l'eventuale ordinamento)?
104400140205         clear  keyFICAU02;
104500131121         k_CAUfgs = V1Cfgs;
104600140205         setll  %kds(keyFICAU02 : 1)  FICAU000;
104700140205         reade  %kds(keyFICAU02 : 1)  FICAU000;
104800140205         $EoF = ( %eof(FICAU02L) );
104900131203         exsr  sr_CaricaS01;
105000131030
105100131030         // -?Impaginazione subfile?
105200131203         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
105300131030         if S01nrr > *zero;
105400131030           C1RcdNbr  = 1;
105500131030         else;
105600131104           clear  C1RcdNbr;
105700131030         endif;
105800131205         C1CsrRrn = C1RcdNbr;
105900131203
106000131203         // -?Ordinamento subfile?
106100131203         if  S01nrr > *zero;
106200131203           exsr  sr_SortSfl;
106300131203         endif;
106400131204
106500131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
106600131204         //  ?visualizzabili nel subfile?
106700131204         if  S01nrr >= c_MaxRec  and  Not $EoF;
106800131204           ErrGenerico = *on;
106900131204           ErrMessage  = *on;
107000131204           V1Dmsg = sk_Msg(13);
107100131204         endif;
107200131030
107300131030       ENDSR;
107400131030
107500131030       //--------------------------------------------------------------
107600131203       //?Caricamento completo del subfile S01                         ?
107700131030       //--------------------------------------------------------------
107800131203       BEGSR  sr_CaricaS01;
107900131104
108000131203         clear  S01nrr;
108100131203         SflNxtChg = *off;
108200131121
108300131121         clear  LRX1S01;
108400131030
108500131203         // -?Ciclo di caricamento completo di dati nel subfile?
108600131203         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
108700131203         //  ?l'eventuale ordinamento)?
108800131212         DoW  S01nrr < c_MaxRec  and  Not $EoF;
108900131104
109000131104           // -?Selezione singolo record?
109100131104           exsr  sr_SelezRec;
109200131104
109300131104           // -?Caricamento dati nel record del subfile?
109400131104           If  $RecOK;
109500131104
109600131203             // -?...a rottura di Autotrasportatore / Distinta?
109700131121             If  ( CAUpdr <> S01pdr  or
109800131126                   CAUnfv <> S01dst );
109900131211               // -?Scrittura record nel subfile?
110000131211               //  ?dell'autotrasportatore/distinta precedente?
110100131125               if  S01pdr <> *zero;
110200131203                 Ord_x_Dist = (wOrdSfl = 1);
110300131125                 S01nrr += 1;
110400131125                 write  LRX1S01;
110500131125               endif;
110600131121               // -?Impostazione nuovi campi "chiave" nel subfile?
110700131121               exsr  sr_NewRecS01;
110800131121             EndIf;
110900131104
111000131125             // -?Incremento n° consegne / n° ritiri?
111100131121             select;
111200131121               when  CAUtor = c_Consegna;
111300131121                 S01nrco += 1;
111400131121               when  CAUtor = c_Ritiro;
111500131121                 S01nrri += 1;
111600131121             endsl;
111700131104
111800131104           EndIf;
111900131030
112000131104           // -?Lettura record successivo?
112100140205           reade  %kds(keyFICAU02 : 1)  FICAU000;
112200140205           $EoF = ( %eof(FICAU02L) );
112300131030
112400131030         EndDo;
112500131204
112600131121
112700131121         // -?Scrittura dell'ultimo rec. (a totale per AUT) nel subfile?
112800131121         if  S01pdr <> *zero;
112900131203           Ord_x_Dist = (wOrdSfl = 1);
113000131121           S01nrr += 1;
113100131121           write  LRX1S01;
113200131121         endif;
113300131204
113400131205
113500131205         // -?Memorizzazione n° rec. caricati nel subfile?
113600131205         //  ?(servirà all'ordinamento)?
113700131205         SavMAXnrr = S01nrr;
113800131203
113900131205         // -?Memorizzazione eventuale posizionamento effettuato?
114000131204         if  wOrdSfl = *zero;
114100131203           SavC1Daut = C1Daut;
114200131203         endif;
114300131030
114400131030         // -?Visualizzazione del SFL (se ci sono dati)?
114500131030         SflDsp_N = (S01nrr <= *zero);
114600131030
114700131030         // -?Attivazione del SFLEND?
114800131030         SflEnd = ( $EoF );
114900131030
115000131030       ENDSR;
115100131121
115200131121       //--------------------------------------------------------------
115300131121       //?Selezione del singolo record in input                        ?
115400131121       //--------------------------------------------------------------
115500131121       BEGSR  sr_SelezRec;
115600131121
115700131121         $RecOK = *off;
115800131204
115900131204         // -?Reperimento dati della Distinta Consegna?
116000131121
116100131121         Select;
116200131121
116300131121           // -?Filiale Gestione diversa?
116400131121           //  ?(IMPOSSIBILE perchè in chiave)?
116500131121           //When  CAUfgs <> V1Cfgs;
116600131121           //  setll  (*hival)  FICAU000;
116700131121           //  leavesr;
116800131121
116900131121           // -?Record Annullato?
117000131121           When  CAUatb <> *blank;
117100131121             leavesr;
117200131205
117300131205           // -?Distinta NON nel range selezionato?
117400131205           When  W1ndcI > *zero  and
117500131205                (CAUnfv < W1ndcI  or  CAUnfv > V1ndcF);
117600131205             leavesr;
117700131121
117800131121           // -?Autotrasportatore diverso da quello selezionato?
117900131204           When  W1Caut <> *zero  and  CAUpdr <> W1Caut;
118000131121             leavesr;
118100131121
118200131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
118300131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
118400131121             leavesr;
118500131126
118600131126           // -?Solo Telefonate SENZA risposta?
118700131126           When  CAUflt <> 'T';
118800131126             leavesr;
118900131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
119000131126             leavesr;
119100131121
119200131121         EndSl;
119300131204
119400131204
119500131204         // -?Reperimento dati dell'Autotrasportatore?
119600131204         //  ?(SE richiesto un posizionamento)?
119700131204         IF  wOrdSfl = *zero  and  C1Daut <> *blank;
119800131204
119900131204           If  CAUpdr <> k_APDpdr;
120000131204             k_APDpdr = CAUpdr;
120100131204             chain  %kds( keyFIAPD01 )  FIAPD000;
120200131204             if  Not %found(FIAPD01L)  or  APDatb <> *blank;
120300131204               APDrsc = *all'? ';
120400131204             endif;
120500131204           EndIf;
120600131204
120700131204           // -?Richiesto posizionamento per Autotrasportatore?
120800131204           if  APDrsc < C1Daut;
120900131204             leavesr;
121000131204           endif;
121100131204
121200131204         ENDIF;
121300131204
121400131204
121500140121
121600131204         // -?Reperimento dati della distinta?
121700131211         //  ?(PER reperirne il flag PDA   &?
121800131211         //  ? PER filtrare i dati in base ad eventuali parzializzazioni)?
121900131212         k_DSTfgs = CAUfgs;
122000131212         k_DSTnfv = CAUnfv;
122100131212         chain  %kds( keyFIDST09 )  FIDST000;
122200131212         if  Not  %found(FIDST09L);
122300131212           clear  DSTpda;
122400131212           clear  DSTtpm;
122500131212           clear  DSTfpp;
122600140121           clear  DSTflr;
122700131212         endif;
122800140121
122900140121         dDSTflr = DSTflr;
123000131211
123100131211         Select;
123200131211
123300131211           // -?Tipo Distinta diverso da quello selezionato?
123400131211           When  CAUtor = c_Consegna  and
123500131211                 V1Tdis = 'P'         and  DSTtpm <> *blank;
123600131211             leavesr;
123700131211           When  CAUtor = c_Consegna  and
123800131211                 V1Tdis = 'M'         and  DSTtpm <> V1Tdis;
123900131211             leavesr;
124000131211
124100131211           // -?Tipo Uscita diverso da quello selezionato?
124200131211           When  CAUtor =  c_Consegna  and
124300131211                 V1Tusc <> *blank      and  DSTfpp <> V1Tusc;
124400131211             leavesr;
124500131211
124600131211         EndSl;
124700131204
124800131204
124900131204         // -?Verifica SE Distinta chiusa per consegne e ritiri?
125000131204         //  ?(FIDST09L già chainato nella subr. sr_NewRecS01)?
125100131211         If  CAUtor = c_Consegna;
125200131204           wFGS = DSTfgs;
125300131204           wNDC = DSTnfv;
125400131204           exsr  sr_CtrlDistChius;
125500131204           if  $DistChiusa;
125600131204             leavesr;
125700131204           endif;
125800131211         EndIf;
125900131121
126000131204
126100131121         // -?Telefonata da caricare nel subfile?
126200131121         $RecOK = *on;
126300131121
126400131121       ENDSR;
126500131121
126600131121       //--------------------------------------------------------------
126700131121       //?Impostazione nuovi campi nel subfile S01 (a rottura)         ?
126800131121       //--------------------------------------------------------------
126900131121       BEGSR  sr_NewRecS01;
127000131121
127100131121         // -?Pulizia record del subfile?
127200131121         clear  LRX1S01;
127300131121
127400131211         select;
127500131211           when  CAUtor = c_Consegna;
127600131211             ds_Spedizione = CAUogg;
127700131211           when  CAUtor = c_Ritiro;
127800131211             ds_ORM = CAUogg;
127900131211         endsl;
128000131121
128100131204         // -?Decodifica Autotrasportatore?
128200131204         //  ?(SE non già fatto)?
128300131204         If  CAUpdr <> k_APDpdr;
128400131204           k_APDpdr = CAUpdr;
128500131204           chain  %kds( keyFIAPD01 )  FIAPD000;
128600131204           if  Not %found(FIAPD01L)  or  APDatb <> *blank;
128700131204             APDrsc = *all'? ';
128800131204           endif;
128900131204         EndIf;
129000131121
129100131204         // -?Reperimento Num. Telefono Autotrasportatore?
129200131204         //  ?(campo HIDDEN nel subfile - per ora NON usato)?
129300131204         //clear  dAPD4tel;
129400131204         //k_APD4pdr = CAUpdr;
129500131204         //chain  %kds( keyFIAPD41 )  FIAPD400;
129600131204         //if  %found(FIAPD41L)  and  APD4atb = *blank;
129700131204         //  dAPD4tel = APD4uni;
129800131204         //endif;
129900131204
130000131209
130100131212         // -?Distinta già reperita nella subr. "sr_SelezRec"?
130200131211         //SELECT;
130300131211         //
130400131211         //  // -?SPEDIZIONE?
130500131211         //  WHEN  CAUtor = c_Consegna;
130600131211         //
130700131211         //    If  k_ARBaas <> ds_Spedizione.wAAS  or
130800131211         //        k_ARBlnp <> ds_Spedizione.wLNP  or
130900131211         //        k_ARBnrs <> ds_Spedizione.wNRS  or
131000131211         //        k_ARBnsp <> ds_Spedizione.wNSP;
131100131211         //
131200131211         //      // -?Reperimento distinta dalla spedizione?
131300131211         //      clear  keyFNARB01;
131400131211         //      k_ARBaas = ds_Spedizione.wAAS;
131500131211         //      k_ARBlnp = ds_Spedizione.wLNP;
131600131211         //      k_ARBnrs = ds_Spedizione.wNRS;
131700131211         //      k_ARBnsp = ds_Spedizione.wNSP;
131800131211         //      chain  %kds( keyFNARB01 )  FNARB000;
131900131211         //      if  %found(FNARB01L);
132000131211         //
132100131211         //        // -?Reperimento Flag PDA della Distinta?
132200131211         //        if  ARBndc <> k_DSTnfv;
132300131211         //          k_DSTfgs = (ARBpdc / 10000);
132400131211         //          k_DSTnfv = ARBndc;
132500131211         //          chain  %kds( keyFIDST09 )  FIDST000;
132600131211         //          if  Not  %found(FIDST09L);
132700131211         //            clear  DSTpda;
132800131211         //            clear  DSTtpm;
132900131211         //            clear  DSTfpp;
133000131211         //          endif;
133100131211         //        endif;
133200131211         //
133300131211         //      endif;
133400131211         //
133500131211         //    EndIf;
133600131211         //
133700131211         //  // -?O.R.M.?
133800131211         //  WHEN  CAUtor = c_Ritiro;
133900131211         //
134000131211         //    If  keyFNORM01 <> ds_ORM;
134100131211         //
134200131211         //      // -?Reperimento distinta dall'O.R.M.?
134300131211         //      clear  keyFNORM01;
134400131211         //      k_ORMpoe = ds_ORM.wPOE;
134500131211         //      k_ORMnsr = ds_ORM.wNSR;
134600131211         //      k_ORMnor = ds_ORM.wNOR;
134700131211         //      k_ORMnrv = ds_ORM.wNRV;
134800131211         //      chain  %kds( keyFNORM01 )  FNORM000;
134900131211         //      if  %found(FNORM01L);
135000131211         //
135100131211         //        // -?Reperimento Flag PDA della Distinta?
135200131211         //        if  ORMndc <> k_DSTnfv;
135300131211         //          k_DSTfgs = ORMpor;
135400131211         //          k_DSTnfv = ORMndc;
135500131211         //          chain  %kds( keyFIDST09 )  FIDST000;
135600131211         //          if  Not  %found(FIDST09L);
135700131211         //            clear  DSTpda;
135800131211         //            clear  DSTtpm;
135900131211         //            clear  DSTfpp;
136000131211         //          endif;
136100131211         //        endif;
136200131211         //
136300131211         //      endif;
136400131211         //
136500131211         //    EndIf;
136600131211         //
136700131211         //ENDSL;
136800131125
136900131203
137000131203         // -?Impostazione nuovi campi nel subfile?
137100131203         S1Hfgs   = CAUfgs;
137200131203         S1HdtIns = %subst( CAUdtOins : 1 : 8 );
137300131204         //S1Htel   = dAPD4tel.§APD4tel;
137400131203         S01pdr   = CAUpdr;
137500131121         S01pdrD  = APDrsc;
137600131212         //select;
137700131212         //  when  CAUtor = 'C';
137800131212         //    S01dst = ARBndc;
137900131212         //  when  CAUtor = 'R';
138000131212         //    S01dst = ORMndc;
138100131212         //endsl;
138200131212         S01dst   = CAUnfv;
138300131204         S01pda   = DSTpda;
138400140121         S01tst   = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
138500131121
138600131121       ENDSR;
138700131203
138800131203       //--------------------------------------------------------------
138900131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
139000131205       //?F11-Cambia ordinamento sfl                                   ?
139100131203       //--------------------------------------------------------------
139200131205       BEGSR  sr_F11S01;
139300131203
139400131203         if  wOrdSfl < 1;
139500131203           wOrdSfl += 1;
139600131203         else;
139700131203           clear  wOrdSfl;
139800131203         endif;
139900131203
140000131203         select;
140100131203           // -?Subfile già posizionato: occorre ricaricarlo?
140200131203           when  C1Daut <> *blank;
140300131203             $InzS01 = *on;
140400131203           // -?Subfile vuoto: nessun record da ordinare?
140500131205           when  SavMAXnrr = *zero;
140600131203           // -?Altrimenti: basta riordinarlo?
140700131203           other;
140800131203             exsr  sr_SortSfl;
140900131203         endsl;
141000131203
141100131203         clear  C1Daut;
141200131203         clear  SavC1Daut;
141300131203
141400131203       ENDSR;
141500131203
141600131203       //--------------------------------------------------------------
141700131203       //?Ordinamento subfile                                          ?
141800131203       //?  This subroutine sorts the subfile records.                 ?
141900131203       //--------------------------------------------------------------
142000131203       BEGSR  sr_SortSfl;
142100131203
142200131205         RrnLast  = SavMAXnrr;
142300131203
142400131203         C1RcdNbr = 1;
142500131203         clear  C1CsrRrn;
142600131203         clear  S01nrr;
142700131203         clear  V1Dmsg;
142800131203         ErrMessage  = *off;
142900131203         SflNxtChg   = *off;
143000131203         ErrGenerico = *off;
143100131203         %subst(IndDspF : 50) = *off;
143200131203
143300131203         //?___________________________________________________________?
143400131203         //?Initialize the key fields to sort on.?
143500131203         //?There is one extra field not in the subfile -- Selected --?
143600131203         //?that is added to the record. This field is used to place?
143700131203         //?selected records in front of nonselected records.?
143800131203         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
143900131203
144000131203         clear  QLGKL;
144100131203         clear  QLGSKL;
144200131203         clear  QLGSCB;
144300131203         clear  QLGSCB00;
144400131203
144500131203         // -?Gestione della scelta ordinamento:?
144600131203         Select;
144700131203
144800131203           //  ?Ordinamento per Descrizione Autotrasportatore?
144900131204           When  wOrdSfl = *zero;
145000131203             // -?2 campi chiave?
145100131203             QLGNBRK  = 2;
145200131203             // -?1° campo: Descrizione Autotrasportatore (S01pdrD)?
145300131212             //            ?a posizone   36, 35 byte, char, ascending.?
145400131212             QLGSP    = 36;
145500131203             QLGSS    = %size(S01pdrD);
145600131203             QLGDT    = Carattere;
145700131203             QLGSO    = Ascendente;
145800131203             QLGKL(1) = QLGSKL;
145900131203             // -?2° campo: Codice Autotrasportatore (S01pdr)?
146000131212             //            ?a posizone   29,  7 byte, num., ascending.?
146100131212             QLGSP    = 29;
146200131203             QLGSS    = %size(S01pdr);
146300131203             QLGDT    = Carattere;
146400131203             QLGSO    = Ascendente;
146500131203             QLGKL(2) = QLGSKL;
146600131203
146700131203           //  ?Ordinamento per Numero Distinta?
146800131204           When  wOrdSfl = 1;
146900131203             // -?2 campi chiave?
147000131203             QLGNBRK  = 2;
147100131203             // -?1° campo: Numero Distinta (S01dst)?
147200131212             //            ?a posizone   71,  6 byte, num., ascending.?
147300131212             QLGSP    = 71;
147400131203             QLGSS    = %size(S01dst);
147500131203             QLGDT    = Carattere;
147600131203             QLGSO    = Ascendente;
147700131203             QLGKL(1) = QLGSKL;
147800131203             // -?2° campo: Descrizione Autotrasportatore (S01pdrD)?
147900131212             //            ?a posizone   36, 35 byte, char, ascending.?
148000131212             QLGSP    = 36;
148100131203             QLGSS    = %size(S01pdrD);
148200131203             QLGDT    = Carattere;
148300131203             QLGSO    = Ascendente;
148400131203             QLGKL(2) = QLGSKL;
148500131203
148600131203         EndSl;
148700131203
148800131203         // -?Load other sort parameters.?
148900131203         QLGLB   = 80 + 16 * MaxKey;
149000131203         QLGRL   = %size(SflRcd) - 1;
149100131203         QLGRT   = 8;
149200131203         QLGOKL  = 80;
149300131203         QLGLKE  = 16;
149400131203         QLGLSS  = 290;
149500131203         // -?Initialize Sort I/O API fields.?
149600131203         QLGRL00 = QLGRL;
149700131203         QLGRC00 = 1;
149800131203         clear  QUSEI;
149900131203         QUSBPRV = %size(QUSEC);
150000131203
150100131203         // -?First step - Initialize the sort routine.?
150200131203         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
150300131203                      SizeList : ReturnSize : QUSEC );
150400131203
150500131203         // -?Next step - Write records to I/O routine.?
150600131203         QLGRT00  = Put;
150700131203         For  Nrr = 1  To  RrnLast;
150800131203           chain  Nrr  LRX1S01;
150900131203           // -?Solo le righe con Selected = 'Y' sono riordinate,?
151000131203           //  ?quindi per fare un ordinamento di tutte le righe?
151100131203           //  ?metto 'Y' sempre.?
151200131203           Selected = 'Y';
151300131203           clear  QUSEI;
151400131203           QUSBPRV  = %size(QUSEC);
151500131203           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
151600131203                         SizeList : NotUsed : QUSEC );
151700131203         EndFor;
151800131203
151900131203         // -?Next step - Signal end of input, clear subfile for reload.?
152000131203         QLGRT00 = EndPut;
152100131203         clear  QUSEI;
152200131203         QUSBPRV = %size(QUSEC);
152300131203         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
152400131203                       SizeList : NotUsed : QUSEC );
152500131203
152600131203         // -?Pulizia subfile?
152700131203         SflDsp_N    = *on;
152800131203         SflDspCtl_N = *on;
152900131204         write  LRX1C01;
153000131203         SflDspCtl_N = *off;
153100131203         SflEnd      = *off;
153200131203
153300131203         // -?Final step - Write the records back to the subfile.?
153400131203         QLGRT00  = Get;
153500131203         For  Nrr = 1  To RrnLast;
153600131203           clear  QUSEI;
153700131203           QUSBPRV = %size(QUSEC);
153800131203           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
153900131203                         QLGRL00  : NotUsed : QUSEC );
154000131203           // -?Ripristino indicatori utilizzati nel sfl rec?
154100131204           SflNxtChg  = (S01opz <> *zero);
154200131204           Ord_x_Dist = (wOrdSfl = 1);
154300131203           S01nrr = Nrr;
154400131203           write  LRX1S01;
154500131203         EndFor;
154600131203
154700131203         C1CsrRrn = 1;
154800131203
154900131203         // -?Visualizzazione del SFL (se ci sono dati)?
155000131203         SflDsp_N = (S01nrr <= *zero);
155100131203
155200131203         // -?Attivazione del SFLEND?
155300131203         SflEnd = $EoF;
155400131204
155500131205         // -?Impostazione F11 nel piede del subfile?
155600131204         select;
155700131204           when  S01nrr  = *zero;
155800131205             clear  V1PF11;
155900131204           when  wOrdSfl = 0;
156000131205             V1PF11 = 'F11=Ord. x Num.Dist.';
156100131204           when  wOrdSfl = 1;
156200131205             V1PF11 = 'F11=Ord. x Autotrasp';
156300131204         endsl;
156400131203
156500131203       ENDSR;
156600131030
156700131030       //--------------------------------------------------------------
156800131104       //?Gestione opzioni del subfile S01                             ?
156900131030       //--------------------------------------------------------------
157000131030       BEGSR  sr_OpzS01;
157100131030
157200131030         clear  SavS01csr;
157300131030
157400131030         // -?Ciclo di lettura subfile?
157500131104         readc  LRX1S01;
157600131030
157700131104         DoW  Not %eof(FNLRX1D);
157800131030
157900131030           SflNxtChg = *off;
158000131030           %subst(IndDspF : 50) = *off;
158100131030           C1RcdNbr = S01nrr;
158200131030
158300131030           select;
158400131030
158500131030             // -?Nessuna opzione?
158600131121             when  S01opz = *zero;
158700131104
158800131104             // -?Opz. 1=Gestione?
158900131121             when  S01opz = 1;
159000131104               exsr  sr_Call_FNLRX2;
159100131030
159200131030             // -?Opzione errata?
159300131030             other;
159400131030               ErrGenerico = *on;
159500131030               ErrMessage  = *on;
159600131030               PosCurOPZ   = *on;
159700131204               V1Dmsg = sk_Msg(14);
159800131030
159900131030           endsl;
160000131030
160100131030           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
160200131030           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
160300131121           if  S01opz  <> *zero    and
160400131030              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
160500131030             SavS01csr   = C1RcdNbr;
160600131030           endif;
160700131030
160800131030           // -?Aggiornamento sfl?
160900131030           select;
161000131030             when  ErrMessage;
161100131030               SflNxtChg = *on;
161200131030               C1CsrRrn  = C1RcdNbr;
161300131030             when  ErrGenerico;
161400131030               C1CsrRrn  = C1RcdNbr;
161500131121               clear  S01opz;
161600131030             other;
161700131121               clear  S01opz;
161800131030           endsl;
161900131206
162000131206           Ord_x_Dist = (wOrdSfl = 1);
162100131030
162200131104           UPDATE  LRX1S01;
162300131030
162400131030           if  ErrGenerico   or   ErrMessage;
162500131030             leave;
162600131030           endif;
162700131030
162800131104           readc  LRX1S01;
162900131030
163000131030         ENDDO;
163100131030
163200131030         // -?Riposizionam. cursore sul record contenente la prima opz.?
163300131030         //  ?(se non sono stati rilevati errori)?
163400131030         if  NOT ErrMessage   and   NOT ErrGenerico   and
163500131030             SavS01csr > *zero;
163600131030           C1CsrRrn = SavS01csr;
163700131030         endif;
163800131030
163900131030       ENDSR;
164000131030
164100131030       //--------------------------------------------------------------
164200131104       //?Richiamo *pgm FNLRX2R (Gestione Contatti Autotrasportatore)  ?
164300131030       //--------------------------------------------------------------
164400131104       BEGSR  sr_Call_FNLRX2;
164500131030
164600131121         reset  FNLRX2ds;
164700131121         FNLRX2ds.iX2fgs = S1Hfgs;
164800131205         FNLRX2ds.iX2aut = S01pdr;
164900131205         FNLRX2ds.iX2dst = S01dst;
165000131104
165100131204         kpjbU    = FNLRX2ds;
165200131104
165300131204         FNLRX2R ( kpjbA );
165400131104
165500131204         FNLRX2ds = kpjbU;
165600131104
165700131125         // -?Aggiornata almeno una telefonata?
165800131125         if  FNLRX2ds.oX2upd = *on;
165900131125           $InzS01 = *on;
166000131125         endif;
166100131125         // -?Ultima uscita con F3=Fine?
166200131125         if  FNLRX2ds.oX2fxx = '03';
166300131125           ErrGenerico = *on;
166400131125         endif;
166500131030
166600131030       ENDSR;
166700131030
166800131030       //--------------------------------------------------------------
166900131030       //?Operazioni finali.                                           ?
167000131030       //--------------------------------------------------------------
167100131030       BEGSR  sr_RoutEnd;
167200131030
167300131121         // -?Uscita dal *pgm?
167400131030         return;
167500131030
167600131030       ENDSR;
167700131030
167800131030      /end-free
167900131030
168000131121** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
168100131121Nessuna filiale gestibile dall'utente.  Premere "Invio" per uscire.            1
168200131121Filiale in gestione obbligatoria                                               2
168300131121Filiale in gestione errata                                                     3
168400131204La selezione è possibile per Distinta o per AUT, non entrambe                  4
168500131204Numero distinta errato: contiene caratteri NON numerici                        5
168600131204Numero distinta inesistente o annullato                                        6
168700131204Numero distinta obbligatorio                                                   7
168800131204Limiti incompatibili: numero distinta al minore del numero distinta dal        8
168900131204Distinta già chiusa                                                            9
169000131204Autotrasportatore errato: contiene caratteri NON numerici                     10
169100131204Autotrasportatore inesistente o annullato                                     11
169200131204Autotrasportatore non in gestione all'utente                                  12
169300131204Superato il numero massimo di record caricabili a video: contattare il CED    13
169400131204Opzione errata                                                                14
