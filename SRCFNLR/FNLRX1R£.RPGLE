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
017000131030
017100131030       // -?Parametri ricevuti?
017200131030     d KPJBA         e ds
017300131204
017400131204       // -?150ª descrizione dell'organigramma?
017500131204     d Og150         e ds
017600131121
017700131121       // -?DS Tipo record "TEL" di FIAPD40F?
017800131204     d*// dAPD4tel      e ds                  inz   qualified
017900140121
018000140121       // -?DS campo DSTFLR di FIDST00F?
018100140121     d dDSTflr       e ds                  inz   qualified
018200131030
018300131030       //--------------------------------------------------------------
018400131030       //?Definizione variabili globali.                               ?
018500131030       //--------------------------------------------------------------
018600131030
018700131030       // -?Flags booleani?
018800131030     d $Fine           s               n   inz
018900131030     d $EoF            s               n   inz
019000131030     d $InzD01         s               n   inz(*on)
019100131030     d $InzS01         s               n   inz(*on)
019200131121     d $RecOK          s               n   inz
019300131204     d $DistChiusa     s               n   inz
019400131204     d $DistChiusaC    s               n   inz
019500131204     d $DistChiusaR    s               n   inz
019600131030
019700131030       // -?Variabili per la gestione del video?
019800131030     d $Video          s              2    inz('D1')
019900131030     d S01nrr          s                   like(C1RcdNbr) inz
020000131030     d SavS01csr       s                   like(C1RcdNbr) inz
020100131205     d SavMAXnrr       s                   like(C1RcdNbr) inz
020200131203     d wOrdSfl         s              1  0 inz
020300131203
020400131203       // -?Variabili per la gestione del singolo campo alla variazione?
020500131203       //  ?dello stesso?
020600131203     d SavC1Daut       s                   like(C1Daut)   inz(*hival)
020700131204
020800131204       // -?Campi di comodo?
020900131204     d wDate           s              8  0 inz
021000131204     d wFGS            s                   like(DSTfgs)   inz
021100131204     d wNDC            s                   like(DSTnfv)   inz
021200131204     d W1Caut          s                   like(DSTpdr)   inz
021300131204     d W1ndcI          s                   like(V1ndcF)   inz
021400131203
021500131203       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
021600131203
021700131203       //--------------------------------------------------------------
021800131203       // -?Constants?
021900131203       //--------------------------------------------------------------
022000131203       // -?MaxKey - Maximum number of key fields allowed by this program?
022100131203     d MaxKey          c                   const(2)
022200131203       // -?Sort order:?
022300131203       //  ?1 = Sort in an ascending sequence?
022400131203       //  ?2 = Sort in a descending sequence?
022500131203     d Ascendente      c                   const(1)
022600131203     d Discendente     c                   const(2)
022700131203       // -?Key data type:?
022800131203       //  ? 0 = Signed binary?
022900131203       //  ? 2 = Signed zoned decimal?
023000131203       //  ? 3 = Signed packed decimal?
023100131203       //  ? 6 = Character with no national language sort sequence applied,?
023200131203       //  ?     if specified?
023300131203       //  ? 7 = Unsigned packed decimal?
023400131203       //  ?     All numbers will have the sign forced positive ('F'X).?
023500131203       //  ? 8 = Unsigned zoned decimal?
023600131203       //  ?     All numbers will have the sign forced positive ('F'X).?
023700131203       //  ? 9 = Unsigned binary?
023800131203       //  ?14 = Date in form of DD/MM/YY?
023900131203       //  ?15 = Date in form of DD.MM.YYYY?
024000131203     d Numero          c                   const(2)
024100131203     d Carattere       c                   const(6)
024200131203       //
024300131203     d Put             c                   const(1)
024400131203     d EndPut          c                   const(2)
024500131203     d Get             c                   const(3)
024600131203
024700131203       //--------------------------------------------------------------
024800131203       // -?Data Structures?
024900131203       //--------------------------------------------------------------
025000131203       //  ?SflRcd     - The entire subfile record to pass to the sort?
025100131203     d SflRcd          ds                  inz
025200131203     d   S1Hfgs                            inz
025300131203     d   S1HdtIns                          inz
025400131203     d   S1Htel                            inz
025500131203     d   S01opz                            inz
025600131203     d   S01pdr                            inz
025700131203     d   S01pdrD                           inz
025800131203     d   S01dst                            inz
025900131203     d   S01nrCo                           inz
026000131203     d   S01nrRi                           inz
026100131203     d   S01pda                            inz
026200140121     d   S01tst                            inz
026300131203     d   Selected                     1a   inz
026400131203       //  ?QLGSCB     - The sort request block for the QLGSORT API?
026500131203      /copy QSYSINC/QRPGLESRC,QLGSORT
026600131203       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
026700131203     d QLGKL                         16    dim(MaxKey)
026800131203     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
026900131203     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
027000131203     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
027100131203     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
027200131203       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
027300131203      /copy QSYSINC/QRPGLESRC,QLGSRTIO
027400131203       //  ?QUSEC      - Error structure for the QLGSORT API?
027500131203      /copy QSYSINC/QRPGLESRC,QUSEC
027600131203
027700131203       //--------------------------------------------------------------
027800131203       // -?Standalone fields?
027900131203       //--------------------------------------------------------------
028000131203     d NotUsed         s             16a   inz
028100131203       //  ?ReturnSize - Size of list returned by sort API?
028200131203     d ReturnSize      s              9b 0 inz
028300131203       //  ?SizeList   - Size of list?
028400131203     d SizeList        s              9b 0 inz
028500131203       //  ?Nrr        - Relative record number for subfile?
028600131203     d Nrr             s              5i 0 inz
028700131203     d RrnLast         s              5i 0 inz
028800131030
028900131030       //--------------------------------------------------------------
029000131030       //?Definizione prototipi procedure.                             ?
029100131030       //--------------------------------------------------------------
029200131030
029300131030       // -?Reperimento dati utente?
029400131030     d TIBS34ds      e ds                  inz
029500131030      /copy gaitrasrc/srcProtoPR,TIBS34R
029600131121
029700131121       // -?Caricamento filiali gestite dall'utente?
029800131121     d TRUL31ds      e ds                  inz
029900131121     d   sk_POg               10    759    inz   dim(250)
030000131121      /copy gaitrasrc/srcProtoPR,TRUL31R
030100131204
030200131204       // -?Interrogazione/Ricerca Distinte di Consegna?
030300140113     d sk_FV6          s              6p 0 inz   dim(600)
030400131204     d FIDG31ds      e ds                  inz        qualified
030500131204     d   FDGric      e                     inz('S')
030600131204     d   FDGtfv      e                     inz('2')
030700131204     d   FDGtpv      e                     inz('P')
030800131204     d   FDGopz      e                     inz('1')
030900131204      /copy gaitrasrc/srcProtoPR,FIDG31R
031000131204
031100131204       // -?Selezione AUT?
031200131204     d FNLV24ds      e ds                  inz        qualified
031300131204     d   D24tip      e                     inz('A')
031400131204     d   D24flg      e                     inz('R')
031500131204      /copy gaitrasrc/srcProtoPr,FNLV24R
031600131204
031700131204       // -?Controllo Fase della Distinta?
031800131204     d FIOR90ds      e ds                  inz        qualified
031900131204     d   OR90tla     e                     inz('C')
032000131204      /copy gaitrasrc/srcProtoPR,FIOR90R
032100131121
032200131121       // -?Gestione Telefonate AUT (dettaglio)?
032300131121     d FNLRX2ds      e ds                  inz        qualified
032400131121     d   oX2upd      e                     inz(*off)
032500131121     d   oX2err      e                     inz(*off)
032600131121      /copy gaitrasrc/srcProtoPR,FNLRX2R
032700131203
032800131203       // -?Ordinamento subfile?
032900131203      /copy gaitrasrc/srcProtoPr,QLGSRTIO
033000131030
033100131030       //--------------------------------------------------------------
033200131030       //?Definizione key-list.                                        ?
033300131030       //--------------------------------------------------------------
033400131030
033500131030       // -?File AZORG01L?
033600131030     d keyAZORG01    e ds                  extname(AZORG01L : *key)
033700131030     d                                     prefix(k_)   inz
033800131030
033900140205       // -?File FICAU02L?
034000140205     d keyFICAU02    e ds                  extname(FICAU02L : *key)
034100131030     d                                     prefix(k_)   inz
034200131030
034300131030       // -?File FIAPD01L?
034400131030     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
034500131030     d                                     prefix(k_)   inz
034600131121
034700131121       // -?File FIAPD41L?
034800131204     d*// keyFIAPD41    e ds                  extname(FIAPD41L : *key)
034900131204     d*//                                     prefix(k_)   inz
035000131121
035100131204       // -?File FIDST09L?
035200131204     d keyFIDST09    e ds                  extname(FIDST09L : *key)
035300131121     d                                     prefix(k_)   inz
035400131212
035500131212       // -?File FNARB01L?
035600131212     d*// keyFNARB01    e ds                  extname(FNARB01L : *key)
035700131212     d*//                                     prefix(k_)   inz
035800131211
035900131211       // -?File FNORM01L?
036000131212     d*// keyFNORM01    e ds                  extname(FNORM01L : *key)
036100131212     d*//                                     prefix(k_)   inz
036200131030
036300131030       //--------------------------------------------------------------
036400131030       //?Riepilogo indicatori utilizzati.                             ?
036500131030       //--------------------------------------------------------------
036600131030
036700131030
036800131030       //--------------------------------------------------------------
036900131030       //?M A I N - L I N E                                            ?
037000131030       //--------------------------------------------------------------
037100131030
037200131030     c     *Entry        plist
037300131030     c                   parm                    KPJBA
037400131030
037500131030      /free
037600131030
037700131030       // -?Operazioni iniziali?
037800131030       exsr sr_RoutInz;
037900131030
038000131030       // -?Ciclo di gestione del file video?
038100131030       DoW  $Fine = *off;
038200131030
038300131030         select;
038400131104
038500131104           // -?Richiesta parametri iniziali?
038600131104           when $Video = 'D1';
038700131104             exsr  sr_GesD01;
038800131030
038900131030           // -?Gestione videata S1 (subfile x filiale)?
039000131030           when $Video = 'S1';
039100131030             exsr sr_GesS01;
039200131030
039300131030           // -?? ? ??
039400131030           other;
039500131030             $Fine = *on;
039600131030
039700131030         endsl;
039800131030
039900131030       EndDo;
040000131030
040100131030       // -?Operazioni finali?
040200131030       exsr sr_RoutEnd;
040300131030
040400131030       //--------------------------------------------------------------
040500131030       //?Operazioni iniziali.                                         ?
040600131030       //--------------------------------------------------------------
040700131030       BEGSR  sr_RoutInz;
040800131030
040900131030         // -?Impostazione chiusura?
041000131030         *inLR = *on;
041100131030
041200131030         // -?Reperimento dati job?
041300131030         exsr  sr_DatiJob;
041400131104
041500131104         // -?Impostazione nome programma a video?
041600131104         V1Tpgm = SDSpgmName;
041700131204
041800131204         // -?Reperimento data odierna?
041900131204         wDate = %int( %subst( %char( %dec( %timestamp() ) )
042000131204                               : 1 : 8 ) );
042100131121
042200131121         // -?Caricamento filiali gestibili dall'utente?
042300131121         clear  TRUL31ds;
042400131121         I31abi = UTEaut;
042500131121         I31cdi = DUTdis;
042600131121         I31car = DUTare;
042700131121         I31cpo = DUTpou;
042800131204         trul31r ( kpjbA : TRUL31ds );
042900131121         // -?In caso di errore si uscirà DOPO l'emissione della?
043000131121         //  ?videata D01?
043100131121         //if  O31pog <= *zero;
043200131121         //  $Fine = *on;
043300131121         //endif;
043400131030
043500131030         // -?Pulizia / impostazione iniziale dei campi chiave?
043600131030         clear  keyAZORG01;
043700140205         clear  keyFICAU02;
043800131030         clear  keyFIAPD01;
043900131204         //clear  keyFIAPD41;
044000131204         clear  keyFIDST09;
044100131212         //clear  keyFNARB01;
044200131212         //clear  keyFNORM01;
044300131121         k_APDtip   = 'A';
044400131204         //k_APD4tipA = 'A';
044500131204         //k_APD4trd  = 'TEL';
044600131121         k_DSTnpg   = 4;
044700131030
044800131030       ENDSR;
044900131030
045000131030       //--------------------------------------------------------------
045100131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
045200131030       //--------------------------------------------------------------
045300131030       BEGSR  sr_DatiJob;
045400131030
045500131030         in(e) §AzUte;
045600131030         if NOT %error;
045700131030           in(e) §DatiUte;
045800131030         endif;
045900131030         if %error or RSut = *blank;
046000131030           tibs34r ( tibs34ds );
046100131030           in §AzUte;
046200131030           in §DatiUte;
046300131030         endif;
046400131030
046500131030       ENDSR;
046600131030
046700131030       //--------------------------------------------------------------
046800131030       //?Gestione videata D01.                                        ?
046900131030       //--------------------------------------------------------------
047000131030       BEGSR  sr_GesD01;
047100131030
047200131030         // -?Inizializzazione videata?
047300131030         if  $InzD01 = *on;
047400131030           exsr  sr_InzD01;
047500131030           $InzD01 = *off;
047600131030         endif;
047700131030
047800131030         // -?(Dis)Abilitazione tasti funzionali?
047900131104         F18Attivo = (ProtectFGS = *on);
048000131030
048100131030         // -?Emissione videata?
048200131104         write  LRX1T01;
048300131104         exfmt  LRX1D01;
048400131030
048500131030         clear  V1Dmsg;
048600131030         reset  ErrMessage;
048700131030         reset  ErrGenerico;
048800131030
048900131030         SELECT;
049000131121
049100131121           // -?Nessuna filiale gestibile dall'utente?
049200131121           WHEN  O31pog <= *zero;
049300131121             $Fine = *on;
049400131030
049500131030           // -?F3=Fine?
049600131030           WHEN  dsp_aid = c_F03;
049700131030             $Fine = *on;
049800131030
049900131104           // -?F18=Cambio Filiale?
050000131104           WHEN  dsp_aid = c_F18;
050100131030             ProtectFGS = *off;
050200131030
050300131030           // -?Invio?
050400131030           OTHER;
050500131030             exsr  sr_CtrD01;
050600131030             if  ErrGenerico = *on;
050700131030               leavesr;
050800131030             endif;
050900131030             ProtectFGS = *on;
051000131030             $Video  = 'S1';
051100131030             $InzS01 = *on;
051200131030
051300131030         ENDSL;
051400131030
051500131030       ENDSR;
051600131104
051700131104       //--------------------------------------------------------------
051800131104       //?Inizializzazione videata D01.                                ?
051900131104       //--------------------------------------------------------------
052000131104       BEGSR  sr_InzD01;
052100131104
052200131104         clear  LRX1D01;
052300131104
052400131121         V1Cfgs = DUTpou;
052500131204         //V1Dfgs = RSut;
052600131204         V1Dfgs = DUTdpo;
052700131204         V1ndcF = *hival;
052800131204
052900131204         // -?Decodifica Filiale Gestione?
053000131204         exsr  sr_CtrD01;
053100131204         %subst(IndDspF : 50) = *off;
053200131204         reset  ErrGenerico;
053300131204         reset  ErrMessage;
053400131204         clear  V1Dmsg;
053500131205
053600131205         // -?Impostazione Attributi di Visualizzazione?
053700131205         ProtectFGS = *on;
053800131205         VisualTDIS = (§OGpkm = 'S');
053900131104
054000131104       ENDSR;
054100131104
054200131104       //--------------------------------------------------------------
054300131104       //?Controllo dati nella videata D01.                            ?
054400131104       //--------------------------------------------------------------
054500131104       BEGSR  sr_CtrD01;
054600131204
054700131104         // -?Spegnimento indicatori di posizionamento cursore?
054800131205         %subst(IndDspF : 50) = *off;
054900131204
055000131204         clear  Og150;
055100131121
055200131121         // -?Nessuna filiale gestibile dall'utente?
055300131121         if  O31pog <= *zero;
055400131121           ErrMessage  = *on;
055500131121           ErrGenerico = *on;
055600131121           V1Dmsg = sk_Msg(01);
055700131121           leavesr;
055800131121         endif;
055900131104
056000131104         // -?Controllo filiale in gestione?
056100131104         clear  V1Dfgs;
056200131104         select;
056300131121           // -?obbligatoria?
056400131121           when  V1Cfgs  = *zero;
056500131104             ErrGenerico = *on;
056600131104             ErrMessage  = *on;
056700131104             PosCurFGS   = *on;
056800131121             V1Dmsg = sk_Msg(02);
056900131104             leavesr;
057000131121           // -?controllo/decodifica?
057100131104           other;
057200131104             chain ( V1Cfgs )  AZORG;
057300131104             if  %found(AZORG01L)   and   ORGfva = *blank;
057400131104               V1Dfgs = ORGdes;
057500131204               Og150  = ORGdf0;
057600131121               if  %lookup( %editc( V1Cfgs : 'X' ) : sk_POg ) = *zero;
057700131121                 ErrGenerico = *on;
057800131121                 ErrMessage  = *on;
057900131121                 PosCurFGS   = *on;
058000131121                 V1Dmsg = sk_Msg(03);
058100131121                 leavesr;
058200131121               endif;
058300131104             else;
058400131121               V1Dfgs = *all'? ';
058500131104               ErrGenerico = *on;
058600131104               ErrMessage  = *on;
058700131104               PosCurFGS   = *on;
058800131121               V1Dmsg = sk_Msg(03);
058900131104               leavesr;
059000131104             endif;
059100131104         endsl;
059200131204
059300131204         // -?Controllo range di distinte / autotrasportatore?
059400131204         //  ?Se inserito uno, non può essere inserito anche l'altro?
059500131204         if  V1Caut > *zero  and
059600131204            (V1ndcI > *zero  or  (V1ndcF > *zero  and  V1ndcF <> *hival));
059700131204           ErrGenerico = *on;
059800131204           ErrMessage  = *on;
059900131204           PosCurNDCI  = *on;
060000131204           V1Dmsg = sk_Msg(04);
060100131204           leavesr;
060200131204         endif;
060300131204
060400131204         // -?Controllo Numeri Distinta?
060500131204         exsr  sr_CtrlDIST;
060600131204         if  ErrGenerico = *on;
060700131204           leavesr;
060800131204         endif;
060900131104
061000131204         // -?Controllo Autotrasportatore?
061100131204         exsr  sr_CtrlAUT;
061200131204         if  ErrGenerico = *on;
061300131204           leavesr;
061400131204         endif;
061500131104
061600131104       ENDSR;
061700131204
061800131204       //--------------------------------------------------------------
061900131204       //?Controllo Numeri Distinta.                                   ?
062000131204       //--------------------------------------------------------------
062100131204       BEGSR  sr_CtrlDIST;
062200140121
062300140121         clear  dDSTflr;
062400131204
062500131204         // -?Numero Distinta Dal?
062600131204         clear  W1ndcI;
062700131204         if V1ndcI = *zero;
062800131204           clear  V1ndcI;
062900131204         endif;
063000131204
063100131204         IF V1ndcI <> *blank;
063200131204
063300131204           // -?Ricerca?
063400131204           If  %scan( '?' : V1ndcI ) > *zero;
063500131204             reset  FIDG31ds;
063600131204             FIDG31ds.FDGfgs = V1Cfgs;
063700131204             FIDG31ds.FDGgda = wDate;
063800131204             FIDG31ds.FDGgal = wDate;
063900131211             //FIDG31ds.FDGric = 'S';      ?(già così)?
064000131211             //FIDH31ds.FDGtfv = '2';      ?(già così)?
064100131211             //FIDG31ds.FDGtpv = 'P';      ?(già così)?
064200131211             //FIDG31ds.FDGopz = '1';      ?(già così)?
064300131204             sk_FV6 = *hival;
064400131204
064500131204             kpjbU  = FIDG31ds;
064600131204
064700131204             FIDG31R ( kpjbA : sk_FV6 );
064800131204
064900131204             FIDG31ds = kpjbU;
065000131204
065100131204             if  FIDG31ds.FDGnfv = *zero;
065200131204               clear  FIDG31ds;
065300131204               clear  V1ndcI;
065400131204             else;
065500131204               V1ndcI = %editc( %dec( FIDG31ds.FDGnfv ) : 'X' );
065600131204             endif;
065700131204
065800131204             ErrGenerico = *on;
065900131204             PosCurNDCI  = *on;
066000131204             leavesr;
066100131204
066200131204           EndIf;
066300131204
066400131204           // -?Controllo numericità?
066500131204           if  %check( c_Digits : V1ndcI) > *zero;
066600131204             ErrGenerico = *on;
066700131204             ErrMessage  = *on;
066800131204             PosCurNDCI  = *on;
066900131204             V1Dmsg = sk_Msg(05);
067000131204             leavesr;
067100131204           endif;
067200131204
067300131204           W1ndcI = %int( V1ndcI );
067400131204
067500131204           // -?Controllo esistenza?
067600131204           k_DSTfgs = V1Cfgs;
067700131204           k_DSTnfv = W1ndcI;
067800131204           chain  %kds( keyFIDST09 )  FIDST000;
067900131204           if  Not %found(FIDST09L)  or  DSTatb <> *blank;
068000131204             ErrGenerico = *on;
068100131204             ErrMessage  = *on;
068200131204             PosCurNDCI  = *on;
068300131204             V1Dmsg = sk_Msg(06);
068400131204             leavesr;
068500131204           endif;
068600131204
068700131204           // -?Impostazione distinta finale = distinta iniziale?
068800131204           //  ?(SE NON impostata)?
068900131204           if  V1ndcF = *zero;
069000131204             V1ndcF = W1ndcI;
069100131204           endif;
069200131204
069300131204         ENDIF;
069400131204
069500131204         // -?Numero Distinta Al?
069600131204         // -?Deve essere impostato: con distinta  o  a 999999?
069700140205         if  W1ndcI = *zero   and  V1ndcF = *zero;
069800140205           V1ndcF = *hival;
069900140205         endif;
070000131204         if  V1ndcF <= *zero;
070100131204           ErrGenerico = *on;
070200131204           ErrMessage  = *on;
070300131204           PosCurNDCF  = *on;
070400131204           V1Dmsg = sk_Msg(07);
070500131204           leavesr;
070600131204         endif;
070700131204
070800131204         // -?Controllo compatibilità Range?
070900131204         if  W1ndcI > V1ndcF;
071000131204           ErrGenerico = *on;
071100131204           ErrMessage  = *on;
071200131204           PosCurNDCF  = *on;
071300131204           V1Dmsg = sk_Msg(08);
071400131204           leavesr;
071500131204         endif;
071600131204
071700131204         // -?Richiesta una sola distinta:?
071800131204         //  ?controllo SE distinta chiusa per consegne e ritiri?
071900131204         If  W1ndcI = V1ndcF;
072000131204           wFGS = V1Cfgs;
072100131204           wNDC = V1ndcF;
072200131204           exsr  sr_CtrlDistChius;
072300131204           if  $DistChiusa;
072400131204             ErrGenerico = *on;
072500131204             ErrMessage  = *on;
072600131204             PosCurNDCI  = *on;
072700131204             V1Dmsg = sk_Msg(09);
072800131204             leavesr;
072900131204           endif;
073000131204         EndIf;
073100131204
073200131204       ENDSR;
073300131204
073400131204       //--------------------------------------------------------------
073500131204       //?Controllo Autotrasportatore.                                 ?
073600131204       //--------------------------------------------------------------
073700131204       BEGSR  sr_CtrlDistChius;
073800131204
073900131204         $DistChiusa  = *off;
074000131204         $DistChiusaC = *off;
074100131204         $DistChiusaR = *off;
074200131204
074300140203         // -?SE c'è la fase RCH o la RQE è chiusa x i?RITIRI?
074400131204         reset  FIOR90ds;
074500131211         //FIOR90ds.OR90tla = 'C';         ?(già così)?
074600131204         FIOR90ds.OR90fas = 'RCH';
074700131204         FIOR90ds.OR90fgs = V1Cfgs;
074800131204         FIOR90ds.OR90ndc = V1ndcF;
074900131204
075000131204         FIOR90R ( kpjbA : FIOR90ds );
075100131204
075200131204         // -?SE trova la fase RCH: imposta che è chiusa?
075300131204         if  FIOR90ds.OR90ndcok <> *blank;
075400131204           $DistChiusaR = *on;
075500131204         endif;
075600131204
075700131204         // -?SE non trova la fase RCH: prova con la fase RQE?
075800131204         If  FIOR90ds.OR90ndcok = *blank;
075900131204
076000131204           reset  FIOR90ds;
076100131211           //FIOR90ds.OR90tla = 'C';       ?(già così)?
076200131204           FIOR90ds.OR90fas = 'RQE';
076300131204           FIOR90ds.OR90fgs = V1Cfgs;
076400131204           FIOR90ds.OR90ndc = V1ndcF;
076500131204
076600131204           FIOR90R ( kpjbA : FIOR90ds );
076700131204
076800131204           // -?SE trova la fase RQE: imposta che è chiusa?
076900131204           if  FIOR90ds.OR90ndcok <> *blank;
077000131204             $DistChiusaR = *on;
077100131204           endif;
077200131204
077300131204         EndIf;
077400131204
077500140203         // -?SE c'è la fase CRI: è chiusa x le?CONSEGNE?
077600131204         reset  FIOR90ds;
077700131211         //FIOR90ds.OR90tla = 'C';         ?(già così)?
077800131204         FIOR90ds.OR90fas = 'CRI';
077900131204         FIOR90ds.OR90fgs = V1Cfgs;
078000131204         FIOR90ds.OR90ndc = V1ndcF;
078100131204
078200131204         FIOR90R ( kpjbA : FIOR90ds );
078300131204
078400131204         // -?SE trova la fase CRI: imposto che è chiusa?
078500131204         if  FIOR90ds.OR90ndcok <> *blank;
078600131204           $DistChiusaC = *on;
078700131204         endif;
078800131204
078900131204         // -?SE non trovo la fase CRI: controllo se chiusa con flag?
079000131204         if  FIOR90ds.OR90ndcok = *blank  and  DSTfcf = 'S';
079100131204           $DistChiusaC = *on;
079200131204         endif;
079300131204
079400131204         // -?SE la distinta è chiusa x le consegne e x i ritiri:?
079500131204         //  ?imposta il flag?
079600131204         if  $DistChiusaR  and  $DistChiusaC;
079700131204           $DistChiusa = *on;
079800131204         endif;
079900131204
080000131204       ENDSR;
080100131204
080200131204       //--------------------------------------------------------------
080300131204       //?Controllo Autotrasportatore.                                 ?
080400131204       //--------------------------------------------------------------
080500131204       BEGSR  sr_CtrlAUT;
080600131204
080700131204         clear  W1Caut;
080800131204         clear  V1Daut;
080900131204
081000131204         // -?Autotrasportatore NON impostato?
081100131204         if  V1Caut = *zero;
081200131204           clear  V1Caut;
081300131204         endif;
081400131204         if  V1Caut = *blank;
081500131204           leavesr;
081600131204         endif;
081700131204
081800131204         // -?Ricerca?
081900131204         If  %scan( '?' : V1Caut ) > *zero;
082000131204
082100131204           clear  V1Caut;
082200131204           clear  V1Daut;
082300131204
082400131204           reset  FNLV24ds;
082500131204           FNLV24ds.D24fil = DUTpou;
082600131211           //FNLV24ds.D24tip = 'A';        ?(già così)?
082700131211           //FNLV24ds.D24flg = 'R';        ?(già così)?
082800131204
082900131204           kpjbU = FNLV24ds;
083000131204
083100131204           FNLV24R ( kpjbA );
083200131204
083300131204           FNLV24ds = kpjbU;
083400131204
083500131204           if FNLV24ds.D24pdr <> *zero;
083600131204             V1Caut = %editc( %int( FNLV24ds.D24pdr ) : 'X' );
083700131204             V1Daut = FNLV24ds.D24rsc;
083800131204           endif;
083900131204
084000131204           ErrGenerico = *on;
084100131204           PosCurAUT   = *on;
084200131204           leavesr;
084300131204
084400131204         EndIf;
084500131204
084600131204         // -?Controllo autotrasportatore?
084700131204         IF  V1Caut <> *blank  and  V1Caut <> *zero;
084800131204
084900131204           // -?Controllo numericità?
085000131204           if  %check( c_digits : V1Caut ) > *zero;
085100131204             ErrGenerico = *on;
085200131204             ErrMessage  = *on;
085300131204             PosCurAUT   = *on;
085400131204             V1Dmsg = sk_Msg(10);
085500131204             leavesr;
085600131204            endif;
085700131204
085800131204           // -?Controllo esistenza?
085900131211           //k_APDtip = 'A';          ?(già così)?
086000131204           k_APDpdr = %int( V1Caut );
086100131204           chain %kds(keyFIAPD01)  FIAPD000;
086200131204           if  %found(FIAPD01L)  and  APDatb = *blank;
086300131204             V1Daut = APDrsc;
086400131204           else;
086500131204             ErrGenerico = *on;
086600131204             ErrMessage  = *on;
086700131204             PosCurAUT   = *on;
086800131204             V1Dmsg = sk_Msg(11);
086900131204             leavesr;
087000131204           endif;
087100131204
087200131204           W1Caut = %int( V1Caut );
087300131204
087400131204         ENDIF;
087500131204
087600131204         // -?L'autotrasportatore deve essere in gestione all'utente?
087700131204         //  ?in base alla filiale gestione impostata a video?
087800131204         if  V1Cfgs <> %int( %subst( V1Caut : 1 : 3 ) );
087900131204           ErrGenerico = *on;
088000131204           ErrMessage  = *on;
088100131204           PosCurAUT   = *on;
088200131204           V1Dmsg = sk_Msg(12);
088300131204           leavesr;
088400131204         endif;
088500131204
088600131204       ENDSR;
088700131030
088800131030       //--------------------------------------------------------------
088900131030       //?Gestione subfile S01.                                        ?
089000131030       //--------------------------------------------------------------
089100131030       BEGSR  sr_GesS01;
089200131030
089300131030         // -?Inizializzazione videata?
089400131030         if  $InzS01 = *on;
089500131030           exsr  sr_InzS01;
089600131030           $InzS01 = *off;
089700131030         endif;
089800131125
089900131125         // -?(Dis)Abilitazione tasti funzionali?
090000131205         F11Attivo = (SavMAXnrr > *zero);
090100131127         F12Attivo = *on;
090200131204         //F12Attivo = (kpjbU = *blank);?(al ritorno da F12 non è più *blank)?
090300131030
090400131030         // -?Emissione Testata & Piede?
090500131104         write  LRX1T01;
090600131104         write  LRX1P01;
090700131030
090800131030         // -?Posizionamento cursore?
090900131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
091000131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
091100131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
091200131030         //  ?premuto l'ultimo tasto.?
091300131030         if  C1CsrRrn > *zero;
091400131030           C1RcdNbr = C1CsrRrn;
091500131030         else;
091600131030           C1RcdNbr = 1;
091700131127           write  LRX1S00;               //?(no rec.)?
091800131030         endif;
091900131030
092000131030         // -?Emissione videata?
092100131104         exfmt  LRX1C01;
092200131030
092300131030         reset  ErrGenerico;
092400131030         reset  ErrMessage;
092500131030         clear  V1Dmsg;
092600131030
092700131030         Select;
092800131030
092900131030           // -?F3=Fine?
093000131030           When  dsp_aid = c_F03;
093100131030             $Fine = *on;
093200131030
093300131030           // -?F5=Rivisualizza?
093400131030           When  dsp_aid = c_F05;
093500131030             $InzS01 = *on;
093600131203
093700131205           // -?F11=Ordinamento Sfl?
093800131205           When  dsp_aid = c_F11;
093900131205             exsr  sr_F11S01;
094000131104
094100131104           // -?F12=Ritorno?
094200131104           When  dsp_aid = c_F12;
094300131104             $Video = 'D1';
094400131104
094500131104           // -?Roll-Up?
094600131104           WHEN  dsp_aid = c_RollUp;
094700131211             If  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
094800131104               ErrGenerico = *on;
094900131104               ErrMessage  = *on;
095000131204               V1Dmsg = sk_Msg(13);
095100131104             EndIf;
095200131030
095300131030           // -?SubFile vuoto?
095400131205           When  SavMAXnrr = *zero;
095500131203             // -?Richiesto posizionamento?
095600131204             if  (wOrdSfl = *zero  and   C1Daut <> SavC1Daut);
095700131203               $InzS01 = *on;
095800131203             endif;
095900131030
096000131030           // -?Invio?
096100131030           Other;
096200131030             // -?Gestione opzioni?
096300131030             exsr  sr_OpzS01;
096400131030             if  ErrGenerico;
096500131030               leavesr;
096600131030             endif;
096700131203             // -?Richiesto posizionamento?
096800131204             if  (wOrdSfl = *zero  and   C1Daut <> SavC1Daut);
096900131203               $InzS01 = *on;
097000131203             endif;
097100131030
097200131030         endsl;
097300131030
097400131030       ENDSR;
097500131030
097600131030       //--------------------------------------------------------------
097700131030       //?Inizializzazione subfile S01.                                ?
097800131030       //--------------------------------------------------------------
097900131030       BEGSR  sr_InzS01;
098000131030
098100131030         // -?Spegnimento degli indicatori di errore?
098200131030         %subst(IndDspF : 50) = *off;
098300131125
098400131125         // -?Impostazione campi di "testata"?
098500131125         C1Cfgs = V1Cfgs;
098600131125         C1Dfgs = V1Dfgs;
098700131203
098800131203         // -?Pulizia subfile control?
098900131204         if  wOrdSfl = 1;
099000131203           clear  C1Daut;
099100131203           reset  SavC1Daut;
099200131203         endif;
099300131030
099400131030         // -?Pulizia subfile?
099500131030         SflDsp_N    = *on;
099600131030         SflDspCtl_N = *on;
099700131104         write  LRX1C01;
099800131030         SflDspCtl_N = *off;
099900131030         SflEnd      = *off;
100000131030
100100131030         clear  C1RcdNbr;
100200131030         clear  C1CsrRrn;
100300131030         clear  S01nrr;
100400131030         clear  V1Dmsg;
100500131030         ErrMessage  = *off;
100600131030         ErrGenerico = *off;
100700131030
100800131203         // -?Caricamento completo dei dati nel subfile?
100900131203         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
101000140203         //  ?l'eventuale ordinamento)?
101100140205         clear  keyFICAU02;
101200131121         k_CAUfgs = V1Cfgs;
101300140205         setll  %kds(keyFICAU02 : 1)  FICAU000;
101400140205         reade  %kds(keyFICAU02 : 1)  FICAU000;
101500140205         $EoF = ( %eof(FICAU02L) );
101600131203         exsr  sr_CaricaS01;
101700131030
101800131030         // -?Impaginazione subfile?
101900131203         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
102000131030         if S01nrr > *zero;
102100131030           C1RcdNbr  = 1;
102200131030         else;
102300131104           clear  C1RcdNbr;
102400131030         endif;
102500131205         C1CsrRrn = C1RcdNbr;
102600131203
102700131203         // -?Ordinamento subfile?
102800131203         if  S01nrr > *zero;
102900131203           exsr  sr_SortSfl;
103000131203         endif;
103100131204
103200131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
103300131204         //  ?visualizzabili nel subfile?
103400131204         if  S01nrr >= c_MaxRec  and  Not $EoF;
103500131204           ErrGenerico = *on;
103600131204           ErrMessage  = *on;
103700131204           V1Dmsg = sk_Msg(13);
103800131204         endif;
103900131030
104000131030       ENDSR;
104100131030
104200131030       //--------------------------------------------------------------
104300131203       //?Caricamento completo del subfile S01                         ?
104400131030       //--------------------------------------------------------------
104500131203       BEGSR  sr_CaricaS01;
104600131104
104700131203         clear  S01nrr;
104800131203         SflNxtChg = *off;
104900131121
105000131121         clear  LRX1S01;
105100131030
105200131203         // -?Ciclo di caricamento completo di dati nel subfile?
105300131203         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
105400131203         //  ?l'eventuale ordinamento)?
105500131212         DoW  S01nrr < c_MaxRec  and  Not $EoF;
105600131104
105700131104           // -?Selezione singolo record?
105800131104           exsr  sr_SelezRec;
105900131104
106000131104           // -?Caricamento dati nel record del subfile?
106100131104           If  $RecOK;
106200131104
106300131203             // -?...a rottura di Autotrasportatore / Distinta?
106400131121             If  ( CAUpdr <> S01pdr  or
106500131126                   CAUnfv <> S01dst );
106600131211               // -?Scrittura record nel subfile?
106700131211               //  ?dell'autotrasportatore/distinta precedente?
106800131125               if  S01pdr <> *zero;
106900131203                 Ord_x_Dist = (wOrdSfl = 1);
107000131125                 S01nrr += 1;
107100131125                 write  LRX1S01;
107200131125               endif;
107300131121               // -?Impostazione nuovi campi "chiave" nel subfile?
107400131121               exsr  sr_NewRecS01;
107500131121             EndIf;
107600131104
107700131125             // -?Incremento n° consegne / n° ritiri?
107800131121             select;
107900131121               when  CAUtor = c_Consegna;
108000131121                 S01nrco += 1;
108100131121               when  CAUtor = c_Ritiro;
108200131121                 S01nrri += 1;
108300131121             endsl;
108400131104
108500131104           EndIf;
108600131030
108700131104           // -?Lettura record successivo?
108800140205           reade  %kds(keyFICAU02 : 1)  FICAU000;
108900140205           $EoF = ( %eof(FICAU02L) );
109000131030
109100131030         EndDo;
109200131204
109300131121
109400131121         // -?Scrittura dell'ultimo rec. (a totale per AUT) nel subfile?
109500131121         if  S01pdr <> *zero;
109600131203           Ord_x_Dist = (wOrdSfl = 1);
109700131121           S01nrr += 1;
109800131121           write  LRX1S01;
109900131121         endif;
110000131204
110100131205
110200131205         // -?Memorizzazione n° rec. caricati nel subfile?
110300131205         //  ?(servirà all'ordinamento)?
110400131205         SavMAXnrr = S01nrr;
110500131203
110600131205         // -?Memorizzazione eventuale posizionamento effettuato?
110700131204         if  wOrdSfl = *zero;
110800131203           SavC1Daut = C1Daut;
110900131203         endif;
111000131030
111100131030         // -?Visualizzazione del SFL (se ci sono dati)?
111200131030         SflDsp_N = (S01nrr <= *zero);
111300131030
111400131030         // -?Attivazione del SFLEND?
111500131030         SflEnd = ( $EoF );
111600131030
111700131030       ENDSR;
111800131121
111900131121       //--------------------------------------------------------------
112000131121       //?Selezione del singolo record in input                        ?
112100131121       //--------------------------------------------------------------
112200131121       BEGSR  sr_SelezRec;
112300131121
112400131121         $RecOK = *off;
112500131204
112600131204         // -?Reperimento dati della Distinta Consegna?
112700131121
112800131121         Select;
112900131121
113000131121           // -?Filiale Gestione diversa?
113100131121           //  ?(IMPOSSIBILE perchè in chiave)?
113200131121           //When  CAUfgs <> V1Cfgs;
113300131121           //  setll  (*hival)  FICAU000;
113400131121           //  leavesr;
113500131121
113600131121           // -?Record Annullato?
113700131121           When  CAUatb <> *blank;
113800131121             leavesr;
113900131205
114000131205           // -?Distinta NON nel range selezionato?
114100131205           When  W1ndcI > *zero  and
114200131205                (CAUnfv < W1ndcI  or  CAUnfv > V1ndcF);
114300131205             leavesr;
114400131121
114500131121           // -?Autotrasportatore diverso da quello selezionato?
114600131204           When  W1Caut <> *zero  and  CAUpdr <> W1Caut;
114700131121             leavesr;
114800131121
114900131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
115000131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
115100131121             leavesr;
115200131126
115300131126           // -?Solo Telefonate SENZA risposta?
115400131126           When  CAUflt <> 'T';
115500131126             leavesr;
115600131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
115700131126             leavesr;
115800131121
115900131121         EndSl;
116000131204
116100131204
116200131204         // -?Reperimento dati dell'Autotrasportatore?
116300131204         //  ?(SE richiesto un posizionamento)?
116400131204         IF  wOrdSfl = *zero  and  C1Daut <> *blank;
116500131204
116600131204           If  CAUpdr <> k_APDpdr;
116700131204             k_APDpdr = CAUpdr;
116800131204             chain  %kds( keyFIAPD01 )  FIAPD000;
116900131204             if  Not %found(FIAPD01L)  or  APDatb <> *blank;
117000131204               APDrsc = *all'? ';
117100131204             endif;
117200131204           EndIf;
117300131204
117400131204           // -?Richiesto posizionamento per Autotrasportatore?
117500131204           if  APDrsc < C1Daut;
117600131204             leavesr;
117700131204           endif;
117800131204
117900131204         ENDIF;
118000131204
118100131204
118200140121
118300131204         // -?Reperimento dati della distinta?
118400131211         //  ?(PER reperirne il flag PDA   &?
118500131211         //  ? PER filtrare i dati in base ad eventuali parzializzazioni)?
118600131212         k_DSTfgs = CAUfgs;
118700131212         k_DSTnfv = CAUnfv;
118800131212         chain  %kds( keyFIDST09 )  FIDST000;
118900131212         if  Not  %found(FIDST09L);
119000131212           clear  DSTpda;
119100131212           clear  DSTtpm;
119200131212           clear  DSTfpp;
119300140121           clear  DSTflr;
119400131212         endif;
119500140121
119600140121         dDSTflr = DSTflr;
119700131211
119800131211         Select;
119900131211
120000131211           // -?Tipo Distinta diverso da quello selezionato?
120100131211           When  CAUtor = c_Consegna  and
120200131211                 V1Tdis = 'P'         and  DSTtpm <> *blank;
120300131211             leavesr;
120400131211           When  CAUtor = c_Consegna  and
120500131211                 V1Tdis = 'M'         and  DSTtpm <> V1Tdis;
120600131211             leavesr;
120700131211
120800131211           // -?Tipo Uscita diverso da quello selezionato?
120900131211           When  CAUtor =  c_Consegna  and
121000131211                 V1Tusc <> *blank      and  DSTfpp <> V1Tusc;
121100131211             leavesr;
121200131211
121300131211         EndSl;
121400131204
121500131204
121600131204         // -?Verifica SE Distinta chiusa per consegne e ritiri?
121700131204         //  ?(FIDST09L già chainato nella subr. sr_NewRecS01)?
121800131211         If  CAUtor = c_Consegna;
121900131204           wFGS = DSTfgs;
122000131204           wNDC = DSTnfv;
122100131204           exsr  sr_CtrlDistChius;
122200131204           if  $DistChiusa;
122300131204             leavesr;
122400131204           endif;
122500131211         EndIf;
122600131121
122700131204
122800131121         // -?Telefonata da caricare nel subfile?
122900131121         $RecOK = *on;
123000131121
123100131121       ENDSR;
123200131121
123300131121       //--------------------------------------------------------------
123400131121       //?Impostazione nuovi campi nel subfile S01 (a rottura)         ?
123500131121       //--------------------------------------------------------------
123600131121       BEGSR  sr_NewRecS01;
123700131121
123800131121         // -?Pulizia record del subfile?
123900131121         clear  LRX1S01;
124000131121
124100131211         select;
124200131211           when  CAUtor = c_Consegna;
124300131211             ds_Spedizione = CAUogg;
124400131211           when  CAUtor = c_Ritiro;
124500131211             ds_ORM = CAUogg;
124600131211         endsl;
124700131121
124800131204         // -?Decodifica Autotrasportatore?
124900131204         //  ?(SE non già fatto)?
125000131204         If  CAUpdr <> k_APDpdr;
125100131204           k_APDpdr = CAUpdr;
125200131204           chain  %kds( keyFIAPD01 )  FIAPD000;
125300131204           if  Not %found(FIAPD01L)  or  APDatb <> *blank;
125400131204             APDrsc = *all'? ';
125500131204           endif;
125600131204         EndIf;
125700131121
125800131204         // -?Reperimento Num. Telefono Autotrasportatore?
125900131204         //  ?(campo HIDDEN nel subfile - per ora NON usato)?
126000131204         //clear  dAPD4tel;
126100131204         //k_APD4pdr = CAUpdr;
126200131204         //chain  %kds( keyFIAPD41 )  FIAPD400;
126300131204         //if  %found(FIAPD41L)  and  APD4atb = *blank;
126400131204         //  dAPD4tel = APD4uni;
126500131204         //endif;
126600131204
126700131209
126800131212         // -?Distinta già reperita nella subr. "sr_SelezRec"?
126900131211         //SELECT;
127000131211         //
127100131211         //  // -?SPEDIZIONE?
127200131211         //  WHEN  CAUtor = c_Consegna;
127300131211         //
127400131211         //    If  k_ARBaas <> ds_Spedizione.wAAS  or
127500131211         //        k_ARBlnp <> ds_Spedizione.wLNP  or
127600131211         //        k_ARBnrs <> ds_Spedizione.wNRS  or
127700131211         //        k_ARBnsp <> ds_Spedizione.wNSP;
127800131211         //
127900131211         //      // -?Reperimento distinta dalla spedizione?
128000131211         //      clear  keyFNARB01;
128100131211         //      k_ARBaas = ds_Spedizione.wAAS;
128200131211         //      k_ARBlnp = ds_Spedizione.wLNP;
128300131211         //      k_ARBnrs = ds_Spedizione.wNRS;
128400131211         //      k_ARBnsp = ds_Spedizione.wNSP;
128500131211         //      chain  %kds( keyFNARB01 )  FNARB000;
128600131211         //      if  %found(FNARB01L);
128700131211         //
128800131211         //        // -?Reperimento Flag PDA della Distinta?
128900131211         //        if  ARBndc <> k_DSTnfv;
129000131211         //          k_DSTfgs = (ARBpdc / 10000);
129100131211         //          k_DSTnfv = ARBndc;
129200131211         //          chain  %kds( keyFIDST09 )  FIDST000;
129300131211         //          if  Not  %found(FIDST09L);
129400131211         //            clear  DSTpda;
129500131211         //            clear  DSTtpm;
129600131211         //            clear  DSTfpp;
129700131211         //          endif;
129800131211         //        endif;
129900131211         //
130000131211         //      endif;
130100131211         //
130200131211         //    EndIf;
130300131211         //
130400131211         //  // -?O.R.M.?
130500131211         //  WHEN  CAUtor = c_Ritiro;
130600131211         //
130700131211         //    If  keyFNORM01 <> ds_ORM;
130800131211         //
130900131211         //      // -?Reperimento distinta dall'O.R.M.?
131000131211         //      clear  keyFNORM01;
131100131211         //      k_ORMpoe = ds_ORM.wPOE;
131200131211         //      k_ORMnsr = ds_ORM.wNSR;
131300131211         //      k_ORMnor = ds_ORM.wNOR;
131400131211         //      k_ORMnrv = ds_ORM.wNRV;
131500131211         //      chain  %kds( keyFNORM01 )  FNORM000;
131600131211         //      if  %found(FNORM01L);
131700131211         //
131800131211         //        // -?Reperimento Flag PDA della Distinta?
131900131211         //        if  ORMndc <> k_DSTnfv;
132000131211         //          k_DSTfgs = ORMpor;
132100131211         //          k_DSTnfv = ORMndc;
132200131211         //          chain  %kds( keyFIDST09 )  FIDST000;
132300131211         //          if  Not  %found(FIDST09L);
132400131211         //            clear  DSTpda;
132500131211         //            clear  DSTtpm;
132600131211         //            clear  DSTfpp;
132700131211         //          endif;
132800131211         //        endif;
132900131211         //
133000131211         //      endif;
133100131211         //
133200131211         //    EndIf;
133300131211         //
133400131211         //ENDSL;
133500131125
133600131203
133700131203         // -?Impostazione nuovi campi nel subfile?
133800131203         S1Hfgs   = CAUfgs;
133900131203         S1HdtIns = %subst( CAUdtOins : 1 : 8 );
134000131204         //S1Htel   = dAPD4tel.§APD4tel;
134100131203         S01pdr   = CAUpdr;
134200131121         S01pdrD  = APDrsc;
134300131212         //select;
134400131212         //  when  CAUtor = 'C';
134500131212         //    S01dst = ARBndc;
134600131212         //  when  CAUtor = 'R';
134700131212         //    S01dst = ORMndc;
134800131212         //endsl;
134900131212         S01dst   = CAUnfv;
135000131204         S01pda   = DSTpda;
135100140121         S01tst   = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
135200131121
135300131121       ENDSR;
135400131203
135500131203       //--------------------------------------------------------------
135600131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
135700131205       //?F11-Cambia ordinamento sfl                                   ?
135800131203       //--------------------------------------------------------------
135900131205       BEGSR  sr_F11S01;
136000131203
136100131203         if  wOrdSfl < 1;
136200131203           wOrdSfl += 1;
136300131203         else;
136400131203           clear  wOrdSfl;
136500131203         endif;
136600131203
136700131203         select;
136800131203           // -?Subfile già posizionato: occorre ricaricarlo?
136900131203           when  C1Daut <> *blank;
137000131203             $InzS01 = *on;
137100131203           // -?Subfile vuoto: nessun record da ordinare?
137200131205           when  SavMAXnrr = *zero;
137300131203           // -?Altrimenti: basta riordinarlo?
137400131203           other;
137500131203             exsr  sr_SortSfl;
137600131203         endsl;
137700131203
137800131203         clear  C1Daut;
137900131203         clear  SavC1Daut;
138000131203
138100131203       ENDSR;
138200131203
138300131203       //--------------------------------------------------------------
138400131203       //?Ordinamento subfile                                          ?
138500131203       //?  This subroutine sorts the subfile records.                 ?
138600131203       //--------------------------------------------------------------
138700131203       BEGSR  sr_SortSfl;
138800131203
138900131205         RrnLast  = SavMAXnrr;
139000131203
139100131203         C1RcdNbr = 1;
139200131203         clear  C1CsrRrn;
139300131203         clear  S01nrr;
139400131203         clear  V1Dmsg;
139500131203         ErrMessage  = *off;
139600131203         SflNxtChg   = *off;
139700131203         ErrGenerico = *off;
139800131203         %subst(IndDspF : 50) = *off;
139900131203
140000131203         //?___________________________________________________________?
140100131203         //?Initialize the key fields to sort on.?
140200131203         //?There is one extra field not in the subfile -- Selected --?
140300131203         //?that is added to the record. This field is used to place?
140400131203         //?selected records in front of nonselected records.?
140500131203         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
140600131203
140700131203         clear  QLGKL;
140800131203         clear  QLGSKL;
140900131203         clear  QLGSCB;
141000131203         clear  QLGSCB00;
141100131203
141200131203         // -?Gestione della scelta ordinamento:?
141300131203         Select;
141400131203
141500131203           //  ?Ordinamento per Descrizione Autotrasportatore?
141600131204           When  wOrdSfl = *zero;
141700131203             // -?2 campi chiave?
141800131203             QLGNBRK  = 2;
141900131203             // -?1° campo: Descrizione Autotrasportatore (S01pdrD)?
142000131212             //            ?a posizone   36, 35 byte, char, ascending.?
142100131212             QLGSP    = 36;
142200131203             QLGSS    = %size(S01pdrD);
142300131203             QLGDT    = Carattere;
142400131203             QLGSO    = Ascendente;
142500131203             QLGKL(1) = QLGSKL;
142600131203             // -?2° campo: Codice Autotrasportatore (S01pdr)?
142700131212             //            ?a posizone   29,  7 byte, num., ascending.?
142800131212             QLGSP    = 29;
142900131203             QLGSS    = %size(S01pdr);
143000131203             QLGDT    = Carattere;
143100131203             QLGSO    = Ascendente;
143200131203             QLGKL(2) = QLGSKL;
143300131203
143400131203           //  ?Ordinamento per Numero Distinta?
143500131204           When  wOrdSfl = 1;
143600131203             // -?2 campi chiave?
143700131203             QLGNBRK  = 2;
143800131203             // -?1° campo: Numero Distinta (S01dst)?
143900131212             //            ?a posizone   71,  6 byte, num., ascending.?
144000131212             QLGSP    = 71;
144100131203             QLGSS    = %size(S01dst);
144200131203             QLGDT    = Carattere;
144300131203             QLGSO    = Ascendente;
144400131203             QLGKL(1) = QLGSKL;
144500131203             // -?2° campo: Descrizione Autotrasportatore (S01pdrD)?
144600131212             //            ?a posizone   36, 35 byte, char, ascending.?
144700131212             QLGSP    = 36;
144800131203             QLGSS    = %size(S01pdrD);
144900131203             QLGDT    = Carattere;
145000131203             QLGSO    = Ascendente;
145100131203             QLGKL(2) = QLGSKL;
145200131203
145300131203         EndSl;
145400131203
145500131203         // -?Load other sort parameters.?
145600131203         QLGLB   = 80 + 16 * MaxKey;
145700131203         QLGRL   = %size(SflRcd) - 1;
145800131203         QLGRT   = 8;
145900131203         QLGOKL  = 80;
146000131203         QLGLKE  = 16;
146100131203         QLGLSS  = 290;
146200131203         // -?Initialize Sort I/O API fields.?
146300131203         QLGRL00 = QLGRL;
146400131203         QLGRC00 = 1;
146500131203         clear  QUSEI;
146600131203         QUSBPRV = %size(QUSEC);
146700131203
146800131203         // -?First step - Initialize the sort routine.?
146900131203         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
147000131203                      SizeList : ReturnSize : QUSEC );
147100131203
147200131203         // -?Next step - Write records to I/O routine.?
147300131203         QLGRT00  = Put;
147400131203         For  Nrr = 1  To  RrnLast;
147500131203           chain  Nrr  LRX1S01;
147600131203           // -?Solo le righe con Selected = 'Y' sono riordinate,?
147700131203           //  ?quindi per fare un ordinamento di tutte le righe?
147800131203           //  ?metto 'Y' sempre.?
147900131203           Selected = 'Y';
148000131203           clear  QUSEI;
148100131203           QUSBPRV  = %size(QUSEC);
148200131203           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
148300131203                         SizeList : NotUsed : QUSEC );
148400131203         EndFor;
148500131203
148600131203         // -?Next step - Signal end of input, clear subfile for reload.?
148700131203         QLGRT00 = EndPut;
148800131203         clear  QUSEI;
148900131203         QUSBPRV = %size(QUSEC);
149000131203         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
149100131203                       SizeList : NotUsed : QUSEC );
149200131203
149300131203         // -?Pulizia subfile?
149400131203         SflDsp_N    = *on;
149500131203         SflDspCtl_N = *on;
149600131204         write  LRX1C01;
149700131203         SflDspCtl_N = *off;
149800131203         SflEnd      = *off;
149900131203
150000131203         // -?Final step - Write the records back to the subfile.?
150100131203         QLGRT00  = Get;
150200131203         For  Nrr = 1  To RrnLast;
150300131203           clear  QUSEI;
150400131203           QUSBPRV = %size(QUSEC);
150500131203           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
150600131203                         QLGRL00  : NotUsed : QUSEC );
150700131203           // -?Ripristino indicatori utilizzati nel sfl rec?
150800131204           SflNxtChg  = (S01opz <> *zero);
150900131204           Ord_x_Dist = (wOrdSfl = 1);
151000131203           S01nrr = Nrr;
151100131203           write  LRX1S01;
151200131203         EndFor;
151300131203
151400131203         C1CsrRrn = 1;
151500131203
151600131203         // -?Visualizzazione del SFL (se ci sono dati)?
151700131203         SflDsp_N = (S01nrr <= *zero);
151800131203
151900131203         // -?Attivazione del SFLEND?
152000131203         SflEnd = $EoF;
152100131204
152200131205         // -?Impostazione F11 nel piede del subfile?
152300131204         select;
152400131204           when  S01nrr  = *zero;
152500131205             clear  V1PF11;
152600131204           when  wOrdSfl = 0;
152700131205             V1PF11 = 'F11=Ord. x Num.Dist.';
152800131204           when  wOrdSfl = 1;
152900131205             V1PF11 = 'F11=Ord. x Autotrasp';
153000131204         endsl;
153100131203
153200131203       ENDSR;
153300131030
153400131030       //--------------------------------------------------------------
153500131104       //?Gestione opzioni del subfile S01                             ?
153600131030       //--------------------------------------------------------------
153700131030       BEGSR  sr_OpzS01;
153800131030
153900131030         clear  SavS01csr;
154000131030
154100131030         // -?Ciclo di lettura subfile?
154200131104         readc  LRX1S01;
154300131030
154400131104         DoW  Not %eof(FNLRX1D);
154500131030
154600131030           SflNxtChg = *off;
154700131030           %subst(IndDspF : 50) = *off;
154800131030           C1RcdNbr = S01nrr;
154900131030
155000131030           select;
155100131030
155200131030             // -?Nessuna opzione?
155300131121             when  S01opz = *zero;
155400131104
155500131104             // -?Opz. 1=Gestione?
155600131121             when  S01opz = 1;
155700131104               exsr  sr_Call_FNLRX2;
155800131030
155900131030             // -?Opzione errata?
156000131030             other;
156100131030               ErrGenerico = *on;
156200131030               ErrMessage  = *on;
156300131030               PosCurOPZ   = *on;
156400131204               V1Dmsg = sk_Msg(14);
156500131030
156600131030           endsl;
156700131030
156800131030           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
156900131030           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
157000131121           if  S01opz  <> *zero    and
157100131030              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
157200131030             SavS01csr   = C1RcdNbr;
157300131030           endif;
157400131030
157500131030           // -?Aggiornamento sfl?
157600131030           select;
157700131030             when  ErrMessage;
157800131030               SflNxtChg = *on;
157900131030               C1CsrRrn  = C1RcdNbr;
158000131030             when  ErrGenerico;
158100131030               C1CsrRrn  = C1RcdNbr;
158200131121               clear  S01opz;
158300131030             other;
158400131121               clear  S01opz;
158500131030           endsl;
158600131206
158700131206           Ord_x_Dist = (wOrdSfl = 1);
158800131030
158900131104           UPDATE  LRX1S01;
159000131030
159100131030           if  ErrGenerico   or   ErrMessage;
159200131030             leave;
159300131030           endif;
159400131030
159500131104           readc  LRX1S01;
159600131030
159700131030         ENDDO;
159800131030
159900131030         // -?Riposizionam. cursore sul record contenente la prima opz.?
160000131030         //  ?(se non sono stati rilevati errori)?
160100131030         if  NOT ErrMessage   and   NOT ErrGenerico   and
160200131030             SavS01csr > *zero;
160300131030           C1CsrRrn = SavS01csr;
160400131030         endif;
160500131030
160600131030       ENDSR;
160700131030
160800131030       //--------------------------------------------------------------
160900131104       //?Richiamo *pgm FNLRX2R (Gestione Contatti Autotrasportatore)  ?
161000131030       //--------------------------------------------------------------
161100131104       BEGSR  sr_Call_FNLRX2;
161200131030
161300131121         reset  FNLRX2ds;
161400131121         FNLRX2ds.iX2fgs = S1Hfgs;
161500131205         FNLRX2ds.iX2aut = S01pdr;
161600131205         FNLRX2ds.iX2dst = S01dst;
161700131104
161800131204         kpjbU    = FNLRX2ds;
161900131104
162000131204         FNLRX2R ( kpjbA );
162100131104
162200131204         FNLRX2ds = kpjbU;
162300131104
162400131125         // -?Aggiornata almeno una telefonata?
162500131125         if  FNLRX2ds.oX2upd = *on;
162600131125           $InzS01 = *on;
162700131125         endif;
162800131125         // -?Ultima uscita con F3=Fine?
162900131125         if  FNLRX2ds.oX2fxx = '03';
163000131125           ErrGenerico = *on;
163100131125         endif;
163200131030
163300131030       ENDSR;
163400131030
163500131030       //--------------------------------------------------------------
163600131030       //?Operazioni finali.                                           ?
163700131030       //--------------------------------------------------------------
163800131030       BEGSR  sr_RoutEnd;
163900131030
164000131121         // -?Uscita dal *pgm?
164100131030         return;
164200131030
164300131030       ENDSR;
164400131030
164500131030      /end-free
164600131030
164700131121** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
164800131121Nessuna filiale gestibile dall'utente.  Premere "Invio" per uscire.            1
164900131121Filiale in gestione obbligatoria                                               2
165000131121Filiale in gestione errata                                                     3
165100131204La selezione è possibile per Distinta o per AUT, non entrambe                  4
165200131204Numero distinta errato: contiene caratteri NON numerici                        5
165300131204Numero distinta inesistente o annullato                                        6
165400131204Numero distinta obbligatorio                                                   7
165500131204Limiti incompatibili: numero distinta al minore del numero distinta dal        8
165600131204Distinta già chiusa                                                            9
165700131204Autotrasportatore errato: contiene caratteri NON numerici                     10
165800131204Autotrasportatore inesistente o annullato                                     11
165900131204Autotrasportatore non in gestione all'utente                                  12
166000131204Superato il numero massimo di record caricabili a video: contattare il CED    13
166100131204Opzione errata                                                                14
