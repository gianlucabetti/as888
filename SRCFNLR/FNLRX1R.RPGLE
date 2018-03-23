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
010400160506     d sk_Msg          s             78    dim(15)  ctdata  perrcd( 1)
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
014800160506     d   VisualTUR                     n   overlay(IndDspF : 43)
014900131030         // -?Posizionamento cursore & segnalazione errore?
015000131121     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015100131030     d   PosCurFGS                     n   overlay(IndDspF : 51)
015200131204     d   PosCurNDCI                    n   overlay(IndDspF : 52)
015300131204     d   PosCurNDCF                    n   overlay(IndDspF : 53)
015400131204     d   PosCurAUT                     n   overlay(IndDspF : 54)
015500160506     d   PosCurTdis                    n   overlay(IndDspF : 55)
015600131030         //   -?Riemissione videata?
015700131030     d   ErrGenerico                   n   overlay(IndDspF : 99)
015800131121
015900131121       // -?DS per Numero Spedizione?
016000131125     d ds_Spedizione   ds                  inz   qualified
016100131125     d   wLNP                         3s 0 inz
016200131125     d   wNRS                         2s 0 inz
016300131125     d   wNSP                         7s 0 inz
016400131125     d   wAAS                         4s 0 inz
016500131121
016600131121       // -?DS per Numero O.R.M.?
016700131211     d ds_ORM          ds                  inz   qualified
016800131211     d   wPOE                         3s 0 inz
016900131211     d   wNSR                         2s 0 inz
017000131211     d   wNOR                         7s 0 inz
017100131211     d   wNRV                         2s 0 inz
017200131030
017300131030       // -?Parametri ricevuti?
017400131030     d KPJBA         e ds
017500131204
017600131204       // -?150ª descrizione dell'organigramma?
017700131204     d Og150         e ds
017800131121
017900131121       // -?DS Tipo record "TEL" di FIAPD40F?
018000131204     d*// dAPD4tel      e ds                  inz   qualified
018100140121
018200140121       // -?DS campo DSTFLR di FIDST00F?
018300140121     d dDSTflr       e ds                  inz   qualified
018400131030
018500131030       //--------------------------------------------------------------
018600131030       //?Definizione variabili globali.                               ?
018700131030       //--------------------------------------------------------------
018800131030
018900131030       // -?Flags booleani?
019000131030     d $Fine           s               n   inz
019100131030     d $EoF            s               n   inz
019200131030     d $InzD01         s               n   inz(*on)
019300131030     d $InzS01         s               n   inz(*on)
019400131121     d $RecOK          s               n   inz
019500131204     d $DistChiusa     s               n   inz
019600131204     d $DistChiusaC    s               n   inz
019700131204     d $DistChiusaR    s               n   inz
019800131030
019900131030       // -?Variabili per la gestione del video?
020000131030     d $Video          s              2    inz('D1')
020100131030     d S01nrr          s                   like(C1RcdNbr) inz
020200131030     d SavS01csr       s                   like(C1RcdNbr) inz
020300131205     d SavMAXnrr       s                   like(C1RcdNbr) inz
020400131203     d wOrdSfl         s              1  0 inz
020500131203
020600131203       // -?Variabili per la gestione del singolo campo alla variazione?
020700131203       //  ?dello stesso?
020800131203     d SavC1Daut       s                   like(C1Daut)   inz(*hival)
020900131204
021000131204       // -?Campi di comodo?
021100131204     d wDate           s              8  0 inz
021200131204     d wFGS            s                   like(DSTfgs)   inz
021300131204     d wNDC            s                   like(DSTnfv)   inz
021400131204     d W1Caut          s                   like(DSTpdr)   inz
021500131204     d W1ndcI          s                   like(V1ndcF)   inz
021600131203
021700131203       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
021800131203
021900131203       //--------------------------------------------------------------
022000131203       // -?Constants?
022100131203       //--------------------------------------------------------------
022200131203       // -?MaxKey - Maximum number of key fields allowed by this program?
022300131203     d MaxKey          c                   const(2)
022400131203       // -?Sort order:?
022500131203       //  ?1 = Sort in an ascending sequence?
022600131203       //  ?2 = Sort in a descending sequence?
022700131203     d Ascendente      c                   const(1)
022800131203     d Discendente     c                   const(2)
022900131203       // -?Key data type:?
023000131203       //  ? 0 = Signed binary?
023100131203       //  ? 2 = Signed zoned decimal?
023200131203       //  ? 3 = Signed packed decimal?
023300131203       //  ? 6 = Character with no national language sort sequence applied,?
023400131203       //  ?     if specified?
023500131203       //  ? 7 = Unsigned packed decimal?
023600131203       //  ?     All numbers will have the sign forced positive ('F'X).?
023700131203       //  ? 8 = Unsigned zoned decimal?
023800131203       //  ?     All numbers will have the sign forced positive ('F'X).?
023900131203       //  ? 9 = Unsigned binary?
024000131203       //  ?14 = Date in form of DD/MM/YY?
024100131203       //  ?15 = Date in form of DD.MM.YYYY?
024200131203     d Numero          c                   const(2)
024300131203     d Carattere       c                   const(6)
024400131203       //
024500131203     d Put             c                   const(1)
024600131203     d EndPut          c                   const(2)
024700131203     d Get             c                   const(3)
024800131203
024900131203       //--------------------------------------------------------------
025000131203       // -?Data Structures?
025100131203       //--------------------------------------------------------------
025200131203       //  ?SflRcd     - The entire subfile record to pass to the sort?
025300131203     d SflRcd          ds                  inz
025400131203     d   S1Hfgs                            inz
025500131203     d   S1HdtIns                          inz
025600131203     d   S1Htel                            inz
025700131203     d   S01opz                            inz
025800131203     d   S01pdr                            inz
025900131203     d   S01pdrD                           inz
026000131203     d   S01dst                            inz
026100131203     d   S01nrCo                           inz
026200131203     d   S01nrRi                           inz
026300131203     d   S01pda                            inz
026400140121     d   S01tst                            inz
026500131203     d   Selected                     1a   inz
026600131203       //  ?QLGSCB     - The sort request block for the QLGSORT API?
026700131203      /copy QSYSINC/QRPGLESRC,QLGSORT
026800131203       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
026900131203     d QLGKL                         16    dim(MaxKey)
027000131203     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
027100131203     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
027200131203     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
027300131203     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
027400131203       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
027500131203      /copy QSYSINC/QRPGLESRC,QLGSRTIO
027600131203       //  ?QUSEC      - Error structure for the QLGSORT API?
027700131203      /copy QSYSINC/QRPGLESRC,QUSEC
027800131203
027900131203       //--------------------------------------------------------------
028000131203       // -?Standalone fields?
028100131203       //--------------------------------------------------------------
028200131203     d NotUsed         s             16a   inz
028300131203       //  ?ReturnSize - Size of list returned by sort API?
028400131203     d ReturnSize      s              9b 0 inz
028500131203       //  ?SizeList   - Size of list?
028600131203     d SizeList        s              9b 0 inz
028700131203       //  ?Nrr        - Relative record number for subfile?
028800131203     d Nrr             s              5i 0 inz
028900131203     d RrnLast         s              5i 0 inz
029000131030
029100131030       //--------------------------------------------------------------
029200131030       //?Definizione prototipi procedure.                             ?
029300131030       //--------------------------------------------------------------
029400131030
029500131030       // -?Reperimento dati utente?
029600131030     d TIBS34ds      e ds                  inz
029700131030      /copy gaitrasrc/srcProtoPR,TIBS34R
029800131121
029900131121       // -?Caricamento filiali gestite dall'utente?
030000131121     d TRUL31ds      e ds                  inz
030100131121     d   sk_POg               10    759    inz   dim(250)
030200131121      /copy gaitrasrc/srcProtoPR,TRUL31R
030300131204
030400131204       // -?Interrogazione/Ricerca Distinte di Consegna?
030500140113     d sk_FV6          s              6p 0 inz   dim(600)
030600131204     d FIDG31ds      e ds                  inz        qualified
030700131204     d   FDGric      e                     inz('S')
030800131204     d   FDGtfv      e                     inz('2')
030900131204     d   FDGtpv      e                     inz('P')
031000131204     d   FDGopz      e                     inz('1')
031100131204      /copy gaitrasrc/srcProtoPR,FIDG31R
031200131204
031300131204       // -?Selezione AUT?
031400131204     d FNLV24ds      e ds                  inz        qualified
031500131204     d   D24tip      e                     inz('A')
031600131204     d   D24flg      e                     inz('R')
031700131204      /copy gaitrasrc/srcProtoPr,FNLV24R
031800131204
031900131204       // -?Controllo Fase della Distinta?
032000131204     d FIOR90ds      e ds                  inz        qualified
032100131204     d   OR90tla     e                     inz('C')
032200131204      /copy gaitrasrc/srcProtoPR,FIOR90R
032300131121
032400131121       // -?Gestione Telefonate AUT (dettaglio)?
032500131121     d FNLRX2ds      e ds                  inz        qualified
032600131121     d   oX2upd      e                     inz(*off)
032700131121     d   oX2err      e                     inz(*off)
032800131121      /copy gaitrasrc/srcProtoPR,FNLRX2R
032900131203
033000131203       // -?Ordinamento subfile?
033100131203      /copy gaitrasrc/srcProtoPr,QLGSRTIO
033200131030
033300131030       //--------------------------------------------------------------
033400131030       //?Definizione key-list.                                        ?
033500131030       //--------------------------------------------------------------
033600131030
033700131030       // -?File AZORG01L?
033800131030     d keyAZORG01    e ds                  extname(AZORG01L : *key)
033900131030     d                                     prefix(k_)   inz
034000131030
034100140205       // -?File FICAU02L?
034200140205     d keyFICAU02    e ds                  extname(FICAU02L : *key)
034300131030     d                                     prefix(k_)   inz
034400131030
034500131030       // -?File FIAPD01L?
034600131030     d keyFIAPD01    e ds                  extname(FIAPD01L : *key)
034700131030     d                                     prefix(k_)   inz
034800131121
034900131121       // -?File FIAPD41L?
035000131204     d*// keyFIAPD41    e ds                  extname(FIAPD41L : *key)
035100131204     d*//                                     prefix(k_)   inz
035200131121
035300131204       // -?File FIDST09L?
035400131204     d keyFIDST09    e ds                  extname(FIDST09L : *key)
035500131121     d                                     prefix(k_)   inz
035600131212
035700131212       // -?File FNARB01L?
035800131212     d*// keyFNARB01    e ds                  extname(FNARB01L : *key)
035900131212     d*//                                     prefix(k_)   inz
036000131211
036100131211       // -?File FNORM01L?
036200131212     d*// keyFNORM01    e ds                  extname(FNORM01L : *key)
036300131212     d*//                                     prefix(k_)   inz
036400131030
036500131030       //--------------------------------------------------------------
036600131030       //?Riepilogo indicatori utilizzati.                             ?
036700131030       //--------------------------------------------------------------
036800131030
036900131030
037000131030       //--------------------------------------------------------------
037100131030       //?M A I N - L I N E                                            ?
037200131030       //--------------------------------------------------------------
037300131030
037400131030     c     *Entry        plist
037500131030     c                   parm                    KPJBA
037600131030
037700131030      /free
037800131030
037900131030       // -?Operazioni iniziali?
038000131030       exsr sr_RoutInz;
038100131030
038200131030       // -?Ciclo di gestione del file video?
038300131030       DoW  $Fine = *off;
038400131030
038500131030         select;
038600131104
038700131104           // -?Richiesta parametri iniziali?
038800131104           when $Video = 'D1';
038900131104             exsr  sr_GesD01;
039000131030
039100131030           // -?Gestione videata S1 (subfile x filiale)?
039200131030           when $Video = 'S1';
039300131030             exsr sr_GesS01;
039400131030
039500131030           // -?? ? ??
039600131030           other;
039700131030             $Fine = *on;
039800131030
039900131030         endsl;
040000131030
040100131030       EndDo;
040200131030
040300131030       // -?Operazioni finali?
040400131030       exsr sr_RoutEnd;
040500131030
040600131030       //--------------------------------------------------------------
040700131030       //?Operazioni iniziali.                                         ?
040800131030       //--------------------------------------------------------------
040900131030       BEGSR  sr_RoutInz;
041000131030
041100131030         // -?Impostazione chiusura?
041200131030         *inLR = *on;
041300131030
041400131030         // -?Reperimento dati job?
041500131030         exsr  sr_DatiJob;
041600131104
041700131104         // -?Impostazione nome programma a video?
041800131104         V1Tpgm = SDSpgmName;
041900131204
042000131204         // -?Reperimento data odierna?
042100131204         wDate = %int( %subst( %char( %dec( %timestamp() ) )
042200131204                               : 1 : 8 ) );
042300131121
042400131121         // -?Caricamento filiali gestibili dall'utente?
042500131121         clear  TRUL31ds;
042600131121         I31abi = UTEaut;
042700131121         I31cdi = DUTdis;
042800131121         I31car = DUTare;
042900131121         I31cpo = DUTpou;
043000131204         trul31r ( kpjbA : TRUL31ds );
043100131121         // -?In caso di errore si uscirà DOPO l'emissione della?
043200131121         //  ?videata D01?
043300131121         //if  O31pog <= *zero;
043400131121         //  $Fine = *on;
043500131121         //endif;
043600131030
043700131030         // -?Pulizia / impostazione iniziale dei campi chiave?
043800131030         clear  keyAZORG01;
043900140205         clear  keyFICAU02;
044000131030         clear  keyFIAPD01;
044100131204         //clear  keyFIAPD41;
044200131204         clear  keyFIDST09;
044300131212         //clear  keyFNARB01;
044400131212         //clear  keyFNORM01;
044500131121         k_APDtip   = 'A';
044600131204         //k_APD4tipA = 'A';
044700131204         //k_APD4trd  = 'TEL';
044800131121         k_DSTnpg   = 4;
044900131030
045000131030       ENDSR;
045100131030
045200131030       //--------------------------------------------------------------
045300131030       //?Reperimento Dati del job (Utente/Operativi).                 ?
045400131030       //--------------------------------------------------------------
045500131030       BEGSR  sr_DatiJob;
045600131030
045700131030         in(e) §AzUte;
045800131030         if NOT %error;
045900131030           in(e) §DatiUte;
046000131030         endif;
046100131030         if %error or RSut = *blank;
046200131030           tibs34r ( tibs34ds );
046300131030           in §AzUte;
046400131030           in §DatiUte;
046500131030         endif;
046600131030
046700131030       ENDSR;
046800131030
046900131030       //--------------------------------------------------------------
047000131030       //?Gestione videata D01.                                        ?
047100131030       //--------------------------------------------------------------
047200131030       BEGSR  sr_GesD01;
047300131030
047400131030         // -?Inizializzazione videata?
047500131030         if  $InzD01 = *on;
047600131030           exsr  sr_InzD01;
047700131030           $InzD01 = *off;
047800131030         endif;
047900131030
048000131030         // -?(Dis)Abilitazione tasti funzionali?
048100131104         F18Attivo = (ProtectFGS = *on);
048200131030
048300131030         // -?Emissione videata?
048400131104         write  LRX1T01;
048500131104         exfmt  LRX1D01;
048600131030
048700131030         clear  V1Dmsg;
048800131030         reset  ErrMessage;
048900131030         reset  ErrGenerico;
049000131030
049100131030         SELECT;
049200131121
049300131121           // -?Nessuna filiale gestibile dall'utente?
049400131121           WHEN  O31pog <= *zero;
049500131121             $Fine = *on;
049600131030
049700131030           // -?F3=Fine?
049800131030           WHEN  dsp_aid = c_F03;
049900131030             $Fine = *on;
050000131030
050100131104           // -?F18=Cambio Filiale?
050200131104           WHEN  dsp_aid = c_F18;
050300131030             ProtectFGS = *off;
050400131030
050500131030           // -?Invio?
050600131030           OTHER;
050700131030             exsr  sr_CtrD01;
050800131030             if  ErrGenerico = *on;
050900131030               leavesr;
051000131030             endif;
051100131030             ProtectFGS = *on;
051200131030             $Video  = 'S1';
051300131030             $InzS01 = *on;
051400131030
051500131030         ENDSL;
051600131030
051700131030       ENDSR;
051800131104
051900131104       //--------------------------------------------------------------
052000131104       //?Inizializzazione videata D01.                                ?
052100131104       //--------------------------------------------------------------
052200131104       BEGSR  sr_InzD01;
052300131104
052400131104         clear  LRX1D01;
052500131104
052600131121         V1Cfgs = DUTpou;
052700131204         //V1Dfgs = RSut;
052800131204         V1Dfgs = DUTdpo;
052900131204         V1ndcF = *hival;
053000131204
053100131204         // -?Decodifica Filiale Gestione?
053200131204         exsr  sr_CtrD01;
053300131204         %subst(IndDspF : 50) = *off;
053400131204         reset  ErrGenerico;
053500131204         reset  ErrMessage;
053600131204         clear  V1Dmsg;
053700131205
053800131205         // -?Impostazione Attributi di Visualizzazione?
053900131205         ProtectFGS = *on;
054000131104
054100131104       ENDSR;
054200131104
054300131104       //--------------------------------------------------------------
054400131104       //?Controllo dati nella videata D01.                            ?
054500131104       //--------------------------------------------------------------
054600131104       BEGSR  sr_CtrD01;
054700131204
054800131104         // -?Spegnimento indicatori di posizionamento cursore?
054900131205         %subst(IndDspF : 50) = *off;
055000131204
055100131204         clear  Og150;
055200131121
055300131121         // -?Nessuna filiale gestibile dall'utente?
055400131121         if  O31pog <= *zero;
055500131121           ErrMessage  = *on;
055600131121           ErrGenerico = *on;
055700131121           V1Dmsg = sk_Msg(01);
055800131121           leavesr;
055900131121         endif;
056000131104
056100131104         // -?Controllo filiale in gestione?
056200131104         clear  V1Dfgs;
056300131104         select;
056400131121           // -?obbligatoria?
056500131121           when  V1Cfgs  = *zero;
056600131104             ErrGenerico = *on;
056700131104             ErrMessage  = *on;
056800131104             PosCurFGS   = *on;
056900131121             V1Dmsg = sk_Msg(02);
057000131104             leavesr;
057100131121           // -?controllo/decodifica?
057200131104           other;
057300131104             chain ( V1Cfgs )  AZORG;
057400131104             if  %found(AZORG01L)   and   ORGfva = *blank;
057500131104               V1Dfgs = ORGdes;
057600131204               Og150  = ORGdf0;
057700131121               if  %lookup( %editc( V1Cfgs : 'X' ) : sk_POg ) = *zero;
057800131121                 ErrGenerico = *on;
057900131121                 ErrMessage  = *on;
058000131121                 PosCurFGS   = *on;
058100131121                 V1Dmsg = sk_Msg(03);
058200131121                 leavesr;
058300131121               endif;
058400160512
058500160512                 VisualTDIS = (§OGpkm = 'S');
058600160512                 VisualTUR  = (§OGturno = 'S');
058700160512
058800160512                 if VisualTUR;
058900160512                   if  H1Cfgs <> V1Cfgs;
059000160512                     H1Cfgs = V1Cfgs;
059100160512                     V1tur1 = '1';
059200160512                     V1tur2 = '2';
059300160512                   endif;
059400160512                 else;
059500160512                   clear H1Cfgs;
059600160512                   clear V1tur1;
059700160512                   clear V1tur2;
059800160512                 endif;
059900160512
060000131104             else;
060100131121               V1Dfgs = *all'? ';
060200131104               ErrGenerico = *on;
060300131104               ErrMessage  = *on;
060400131104               PosCurFGS   = *on;
060500131121               V1Dmsg = sk_Msg(03);
060600131104               leavesr;
060700131104             endif;
060800131104         endsl;
060900131204
061000131204         // -?Controllo range di distinte / autotrasportatore?
061100131204         //  ?Se inserito uno, non può essere inserito anche l'altro?
061200131204         if  V1Caut > *zero  and
061300131204            (V1ndcI > *zero  or  (V1ndcF > *zero  and  V1ndcF <> *hival));
061400131204           ErrGenerico = *on;
061500131204           ErrMessage  = *on;
061600131204           PosCurNDCI  = *on;
061700131204           V1Dmsg = sk_Msg(04);
061800131204           leavesr;
061900131204         endif;
062000160506           // -?Con Messaggerie non si può scegliere il turno
062100160506         If   V1tDIS = 'M' and (V1TUR1 <> ' ' or V1TUR2 <> ' ');
062200160506             ErrGenerico = *on;
062300160506             ErrMessage  = *on;
062400160506             PosCurTDIS  = *on;
062500160506             V1Dmsg = sk_Msg(15);
062600160506             leavesr;
062700160506         endif;
062800131204
062900131204         // -?Controllo Numeri Distinta?
063000131204         exsr  sr_CtrlDIST;
063100131204         if  ErrGenerico = *on;
063200131204           leavesr;
063300131204         endif;
063400131104
063500131204         // -?Controllo Autotrasportatore?
063600131204         exsr  sr_CtrlAUT;
063700131204         if  ErrGenerico = *on;
063800131204           leavesr;
063900131204         endif;
064000131104
064100131104       ENDSR;
064200131204
064300131204       //--------------------------------------------------------------
064400131204       //?Controllo Numeri Distinta.                                   ?
064500131204       //--------------------------------------------------------------
064600131204       BEGSR  sr_CtrlDIST;
064700140121
064800140121         clear  dDSTflr;
064900131204
065000131204         // -?Numero Distinta Dal?
065100131204         clear  W1ndcI;
065200131204         if V1ndcI = *zero;
065300131204           clear  V1ndcI;
065400131204         endif;
065500131204
065600131204         IF V1ndcI <> *blank;
065700131204
065800131204           // -?Ricerca?
065900131204           If  %scan( '?' : V1ndcI ) > *zero;
066000131204             reset  FIDG31ds;
066100131204             FIDG31ds.FDGfgs = V1Cfgs;
066200131204             FIDG31ds.FDGgda = wDate;
066300131204             FIDG31ds.FDGgal = wDate;
066400160506             FIDG31ds.FDGtdis= v1tdis;
066500160506             FIDG31ds.FDGtur1= v1tur1;
066600160506             FIDG31ds.FDGtur2= v1tur2;
066700131211             //FIDG31ds.FDGric = 'S';      ?(già così)?
066800131211             //FIDH31ds.FDGtfv = '2';      ?(già così)?
066900131211             //FIDG31ds.FDGtpv = 'P';      ?(già così)?
067000131211             //FIDG31ds.FDGopz = '1';      ?(già così)?
067100131204             sk_FV6 = *hival;
067200131204
067300131204             kpjbU  = FIDG31ds;
067400131204
067500131204             FIDG31R ( kpjbA : sk_FV6 );
067600131204
067700131204             FIDG31ds = kpjbU;
067800131204
067900131204             if  FIDG31ds.FDGnfv = *zero;
068000131204               clear  FIDG31ds;
068100131204               clear  V1ndcI;
068200131204             else;
068300131204               V1ndcI = %editc( %dec( FIDG31ds.FDGnfv ) : 'X' );
068400131204             endif;
068500131204
068600131204             ErrGenerico = *on;
068700131204             PosCurNDCI  = *on;
068800131204             leavesr;
068900131204
069000131204           EndIf;
069100131204
069200131204           // -?Controllo numericità?
069300131204           if  %check( c_Digits : V1ndcI) > *zero;
069400131204             ErrGenerico = *on;
069500131204             ErrMessage  = *on;
069600131204             PosCurNDCI  = *on;
069700131204             V1Dmsg = sk_Msg(05);
069800131204             leavesr;
069900131204           endif;
070000131204
070100131204           W1ndcI = %int( V1ndcI );
070200131204
070300131204           // -?Controllo esistenza?
070400131204           k_DSTfgs = V1Cfgs;
070500131204           k_DSTnfv = W1ndcI;
070600131204           chain  %kds( keyFIDST09 )  FIDST000;
070700131204           if  Not %found(FIDST09L)  or  DSTatb <> *blank;
070800131204             ErrGenerico = *on;
070900131204             ErrMessage  = *on;
071000131204             PosCurNDCI  = *on;
071100131204             V1Dmsg = sk_Msg(06);
071200131204             leavesr;
071300131204           endif;
071400131204
071500131204           // -?Impostazione distinta finale = distinta iniziale?
071600131204           //  ?(SE NON impostata)?
071700131204           if  V1ndcF = *zero;
071800131204             V1ndcF = W1ndcI;
071900131204           endif;
072000131204
072100131204         ENDIF;
072200131204
072300131204         // -?Numero Distinta Al?
072400131204         // -?Deve essere impostato: con distinta  o  a 999999?
072500140205         if  W1ndcI = *zero   and  V1ndcF = *zero;
072600140205           V1ndcF = *hival;
072700140205         endif;
072800131204         if  V1ndcF <= *zero;
072900131204           ErrGenerico = *on;
073000131204           ErrMessage  = *on;
073100131204           PosCurNDCF  = *on;
073200131204           V1Dmsg = sk_Msg(07);
073300131204           leavesr;
073400131204         endif;
073500131204
073600131204         // -?Controllo compatibilità Range?
073700131204         if  W1ndcI > V1ndcF;
073800131204           ErrGenerico = *on;
073900131204           ErrMessage  = *on;
074000131204           PosCurNDCF  = *on;
074100131204           V1Dmsg = sk_Msg(08);
074200131204           leavesr;
074300131204         endif;
074400131204
074500131204         // -?Richiesta una sola distinta:?
074600131204         //  ?controllo SE distinta chiusa per consegne e ritiri?
074700131204         If  W1ndcI = V1ndcF;
074800131204           wFGS = V1Cfgs;
074900131204           wNDC = V1ndcF;
075000131204           exsr  sr_CtrlDistChius;
075100131204           if  $DistChiusa;
075200131204             ErrGenerico = *on;
075300131204             ErrMessage  = *on;
075400131204             PosCurNDCI  = *on;
075500131204             V1Dmsg = sk_Msg(09);
075600131204             leavesr;
075700131204           endif;
075800131204         EndIf;
075900131204
076000131204       ENDSR;
076100131204
076200131204       //--------------------------------------------------------------
076300131204       //?Controllo Autotrasportatore.                                 ?
076400131204       //--------------------------------------------------------------
076500131204       BEGSR  sr_CtrlDistChius;
076600131204
076700131204         $DistChiusa  = *off;
076800131204         $DistChiusaC = *off;
076900131204         $DistChiusaR = *off;
077000131204
077100140203         // -?SE c'è la fase RCH o la RQE è chiusa x i?RITIRI?
077200131204         reset  FIOR90ds;
077300131211         //FIOR90ds.OR90tla = 'C';         ?(già così)?
077400131204         FIOR90ds.OR90fas = 'RCH';
077500131204         FIOR90ds.OR90fgs = V1Cfgs;
077600131204         FIOR90ds.OR90ndc = V1ndcF;
077700131204
077800131204         FIOR90R ( kpjbA : FIOR90ds );
077900131204
078000131204         // -?SE trova la fase RCH: imposta che è chiusa?
078100131204         if  FIOR90ds.OR90ndcok <> *blank;
078200131204           $DistChiusaR = *on;
078300131204         endif;
078400131204
078500131204         // -?SE non trova la fase RCH: prova con la fase RQE?
078600131204         If  FIOR90ds.OR90ndcok = *blank;
078700131204
078800131204           reset  FIOR90ds;
078900131211           //FIOR90ds.OR90tla = 'C';       ?(già così)?
079000131204           FIOR90ds.OR90fas = 'RQE';
079100131204           FIOR90ds.OR90fgs = V1Cfgs;
079200131204           FIOR90ds.OR90ndc = V1ndcF;
079300131204
079400131204           FIOR90R ( kpjbA : FIOR90ds );
079500131204
079600131204           // -?SE trova la fase RQE: imposta che è chiusa?
079700131204           if  FIOR90ds.OR90ndcok <> *blank;
079800131204             $DistChiusaR = *on;
079900131204           endif;
080000131204
080100131204         EndIf;
080200131204
080300140203         // -?SE c'è la fase CRI: è chiusa x le?CONSEGNE?
080400131204         reset  FIOR90ds;
080500131211         //FIOR90ds.OR90tla = 'C';         ?(già così)?
080600131204         FIOR90ds.OR90fas = 'CRI';
080700131204         FIOR90ds.OR90fgs = V1Cfgs;
080800131204         FIOR90ds.OR90ndc = V1ndcF;
080900131204
081000131204         FIOR90R ( kpjbA : FIOR90ds );
081100131204
081200131204         // -?SE trova la fase CRI: imposto che è chiusa?
081300131204         if  FIOR90ds.OR90ndcok <> *blank;
081400131204           $DistChiusaC = *on;
081500131204         endif;
081600131204
081700131204         // -?SE non trovo la fase CRI: controllo se chiusa con flag?
081800131204         if  FIOR90ds.OR90ndcok = *blank  and  DSTfcf = 'S';
081900131204           $DistChiusaC = *on;
082000131204         endif;
082100131204
082200131204         // -?SE la distinta è chiusa x le consegne e x i ritiri:?
082300131204         //  ?imposta il flag?
082400131204         if  $DistChiusaR  and  $DistChiusaC;
082500131204           $DistChiusa = *on;
082600131204         endif;
082700131204
082800131204       ENDSR;
082900131204
083000131204       //--------------------------------------------------------------
083100131204       //?Controllo Autotrasportatore.                                 ?
083200131204       //--------------------------------------------------------------
083300131204       BEGSR  sr_CtrlAUT;
083400131204
083500131204         clear  W1Caut;
083600131204         clear  V1Daut;
083700131204
083800131204         // -?Autotrasportatore NON impostato?
083900131204         if  V1Caut = *zero;
084000131204           clear  V1Caut;
084100131204         endif;
084200131204         if  V1Caut = *blank;
084300131204           leavesr;
084400131204         endif;
084500131204
084600131204         // -?Ricerca?
084700131204         If  %scan( '?' : V1Caut ) > *zero;
084800131204
084900131204           clear  V1Caut;
085000131204           clear  V1Daut;
085100131204
085200131204           reset  FNLV24ds;
085300131204           FNLV24ds.D24fil = DUTpou;
085400131211           //FNLV24ds.D24tip = 'A';        ?(già così)?
085500131211           //FNLV24ds.D24flg = 'R';        ?(già così)?
085600131204
085700131204           kpjbU = FNLV24ds;
085800131204
085900131204           FNLV24R ( kpjbA );
086000131204
086100131204           FNLV24ds = kpjbU;
086200131204
086300131204           if FNLV24ds.D24pdr <> *zero;
086400131204             V1Caut = %editc( %int( FNLV24ds.D24pdr ) : 'X' );
086500131204             V1Daut = FNLV24ds.D24rsc;
086600131204           endif;
086700131204
086800131204           ErrGenerico = *on;
086900131204           PosCurAUT   = *on;
087000131204           leavesr;
087100131204
087200131204         EndIf;
087300131204
087400131204         // -?Controllo autotrasportatore?
087500131204         IF  V1Caut <> *blank  and  V1Caut <> *zero;
087600131204
087700131204           // -?Controllo numericità?
087800131204           if  %check( c_digits : V1Caut ) > *zero;
087900131204             ErrGenerico = *on;
088000131204             ErrMessage  = *on;
088100131204             PosCurAUT   = *on;
088200131204             V1Dmsg = sk_Msg(10);
088300131204             leavesr;
088400131204            endif;
088500131204
088600131204           // -?Controllo esistenza?
088700131211           //k_APDtip = 'A';          ?(già così)?
088800131204           k_APDpdr = %int( V1Caut );
088900131204           chain %kds(keyFIAPD01)  FIAPD000;
089000131204           if  %found(FIAPD01L)  and  APDatb = *blank;
089100131204             V1Daut = APDrsc;
089200131204           else;
089300131204             ErrGenerico = *on;
089400131204             ErrMessage  = *on;
089500131204             PosCurAUT   = *on;
089600131204             V1Dmsg = sk_Msg(11);
089700131204             leavesr;
089800131204           endif;
089900131204
090000131204           W1Caut = %int( V1Caut );
090100131204
090200131204         ENDIF;
090300131204
090400131204         // -?L'autotrasportatore deve essere in gestione all'utente?
090500131204         //  ?in base alla filiale gestione impostata a video?
090600131204         if  V1Cfgs <> %int( %subst( V1Caut : 1 : 3 ) );
090700131204           ErrGenerico = *on;
090800131204           ErrMessage  = *on;
090900131204           PosCurAUT   = *on;
091000131204           V1Dmsg = sk_Msg(12);
091100131204           leavesr;
091200131204         endif;
091300131204
091400131204       ENDSR;
091500131030
091600131030       //--------------------------------------------------------------
091700131030       //?Gestione subfile S01.                                        ?
091800131030       //--------------------------------------------------------------
091900131030       BEGSR  sr_GesS01;
092000131030
092100131030         // -?Inizializzazione videata?
092200131030         if  $InzS01 = *on;
092300131030           exsr  sr_InzS01;
092400131030           $InzS01 = *off;
092500131030         endif;
092600131125
092700131125         // -?(Dis)Abilitazione tasti funzionali?
092800131205         F11Attivo = (SavMAXnrr > *zero);
092900131127         F12Attivo = *on;
093000131204         //F12Attivo = (kpjbU = *blank);?(al ritorno da F12 non è più *blank)?
093100131030
093200131030         // -?Emissione Testata & Piede?
093300131104         write  LRX1T01;
093400131104         write  LRX1P01;
093500131030
093600131030         // -?Posizionamento cursore?
093700131030         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
093800131030         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
093900131030         //  ?si visualizza la pagina che vedeva l'utente quando ha?
094000131030         //  ?premuto l'ultimo tasto.?
094100131030         if  C1CsrRrn > *zero;
094200131030           C1RcdNbr = C1CsrRrn;
094300131030         else;
094400131030           C1RcdNbr = 1;
094500131127           write  LRX1S00;               //?(no rec.)?
094600131030         endif;
094700131030
094800131030         // -?Emissione videata?
094900131104         exfmt  LRX1C01;
095000131030
095100131030         reset  ErrGenerico;
095200131030         reset  ErrMessage;
095300131030         clear  V1Dmsg;
095400131030
095500131030         Select;
095600131030
095700131030           // -?F3=Fine?
095800131030           When  dsp_aid = c_F03;
095900131030             $Fine = *on;
096000131030
096100131030           // -?F5=Rivisualizza?
096200131030           When  dsp_aid = c_F05;
096300131030             $InzS01 = *on;
096400131203
096500131205           // -?F11=Ordinamento Sfl?
096600131205           When  dsp_aid = c_F11;
096700131205             exsr  sr_F11S01;
096800131104
096900131104           // -?F12=Ritorno?
097000131104           When  dsp_aid = c_F12;
097100131104             $Video = 'D1';
097200131104
097300131104           // -?Roll-Up?
097400131104           WHEN  dsp_aid = c_RollUp;
097500131211             If  SavMAXnrr >= c_MaxRec  and  Not  $EoF;
097600131104               ErrGenerico = *on;
097700131104               ErrMessage  = *on;
097800131204               V1Dmsg = sk_Msg(13);
097900131104             EndIf;
098000131030
098100131030           // -?SubFile vuoto?
098200131205           When  SavMAXnrr = *zero;
098300131203             // -?Richiesto posizionamento?
098400131204             if  (wOrdSfl = *zero  and   C1Daut <> SavC1Daut);
098500131203               $InzS01 = *on;
098600131203             endif;
098700131030
098800131030           // -?Invio?
098900131030           Other;
099000131030             // -?Gestione opzioni?
099100131030             exsr  sr_OpzS01;
099200131030             if  ErrGenerico;
099300131030               leavesr;
099400131030             endif;
099500131203             // -?Richiesto posizionamento?
099600131204             if  (wOrdSfl = *zero  and   C1Daut <> SavC1Daut);
099700131203               $InzS01 = *on;
099800131203             endif;
099900131030
100000131030         endsl;
100100131030
100200131030       ENDSR;
100300131030
100400131030       //--------------------------------------------------------------
100500131030       //?Inizializzazione subfile S01.                                ?
100600131030       //--------------------------------------------------------------
100700131030       BEGSR  sr_InzS01;
100800131030
100900131030         // -?Spegnimento degli indicatori di errore?
101000131030         %subst(IndDspF : 50) = *off;
101100131125
101200131125         // -?Impostazione campi di "testata"?
101300131125         C1Cfgs = V1Cfgs;
101400131125         C1Dfgs = V1Dfgs;
101500131203
101600131203         // -?Pulizia subfile control?
101700131204         if  wOrdSfl = 1;
101800131203           clear  C1Daut;
101900131203           reset  SavC1Daut;
102000131203         endif;
102100131030
102200131030         // -?Pulizia subfile?
102300131030         SflDsp_N    = *on;
102400131030         SflDspCtl_N = *on;
102500131104         write  LRX1C01;
102600131030         SflDspCtl_N = *off;
102700131030         SflEnd      = *off;
102800131030
102900131030         clear  C1RcdNbr;
103000131030         clear  C1CsrRrn;
103100131030         clear  S01nrr;
103200131030         clear  V1Dmsg;
103300131030         ErrMessage  = *off;
103400131030         ErrGenerico = *off;
103500131030
103600131203         // -?Caricamento completo dei dati nel subfile?
103700131203         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
103800140203         //  ?l'eventuale ordinamento)?
103900140205         clear  keyFICAU02;
104000131121         k_CAUfgs = V1Cfgs;
104100140205         setll  %kds(keyFICAU02 : 1)  FICAU000;
104200140205         reade  %kds(keyFICAU02 : 1)  FICAU000;
104300140205         $EoF = ( %eof(FICAU02L) );
104400131203         exsr  sr_CaricaS01;
104500131030
104600131030         // -?Impaginazione subfile?
104700131203         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
104800131030         if S01nrr > *zero;
104900131030           C1RcdNbr  = 1;
105000131030         else;
105100131104           clear  C1RcdNbr;
105200131030         endif;
105300131205         C1CsrRrn = C1RcdNbr;
105400131203
105500131203         // -?Ordinamento subfile?
105600131203         if  S01nrr > *zero;
105700131203           exsr  sr_SortSfl;
105800131203         endif;
105900131204
106000131204         // -?Eventuale emissione *msg di superamento limite max di rec.?
106100131204         //  ?visualizzabili nel subfile?
106200131204         if  S01nrr >= c_MaxRec  and  Not $EoF;
106300131204           ErrGenerico = *on;
106400131204           ErrMessage  = *on;
106500131204           V1Dmsg = sk_Msg(13);
106600131204         endif;
106700131030
106800131030       ENDSR;
106900131030
107000131030       //--------------------------------------------------------------
107100131203       //?Caricamento completo del subfile S01                         ?
107200131030       //--------------------------------------------------------------
107300131203       BEGSR  sr_CaricaS01;
107400131104
107500131203         clear  S01nrr;
107600131203         SflNxtChg = *off;
107700131121
107800131121         clear  LRX1S01;
107900131030
108000131203         // -?Ciclo di caricamento completo di dati nel subfile?
108100131203         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
108200131203         //  ?l'eventuale ordinamento)?
108300131212         DoW  S01nrr < c_MaxRec  and  Not $EoF;
108400131104
108500131104           // -?Selezione singolo record?
108600131104           exsr  sr_SelezRec;
108700131104
108800131104           // -?Caricamento dati nel record del subfile?
108900131104           If  $RecOK;
109000131104
109100131203             // -?...a rottura di Autotrasportatore / Distinta?
109200131121             If  ( CAUpdr <> S01pdr  or
109300131126                   CAUnfv <> S01dst );
109400131211               // -?Scrittura record nel subfile?
109500131211               //  ?dell'autotrasportatore/distinta precedente?
109600131125               if  S01pdr <> *zero;
109700131203                 Ord_x_Dist = (wOrdSfl = 1);
109800131125                 S01nrr += 1;
109900131125                 write  LRX1S01;
110000131125               endif;
110100131121               // -?Impostazione nuovi campi "chiave" nel subfile?
110200131121               exsr  sr_NewRecS01;
110300131121             EndIf;
110400131104
110500131125             // -?Incremento n° consegne / n° ritiri?
110600131121             select;
110700131121               when  CAUtor = c_Consegna;
110800131121                 S01nrco += 1;
110900131121               when  CAUtor = c_Ritiro;
111000131121                 S01nrri += 1;
111100131121             endsl;
111200131104
111300131104           EndIf;
111400131030
111500131104           // -?Lettura record successivo?
111600140205           reade  %kds(keyFICAU02 : 1)  FICAU000;
111700140205           $EoF = ( %eof(FICAU02L) );
111800131030
111900131030         EndDo;
112000131204
112100131121
112200131121         // -?Scrittura dell'ultimo rec. (a totale per AUT) nel subfile?
112300131121         if  S01pdr <> *zero;
112400131203           Ord_x_Dist = (wOrdSfl = 1);
112500131121           S01nrr += 1;
112600131121           write  LRX1S01;
112700131121         endif;
112800131204
112900131205
113000131205         // -?Memorizzazione n° rec. caricati nel subfile?
113100131205         //  ?(servirà all'ordinamento)?
113200131205         SavMAXnrr = S01nrr;
113300131203
113400131205         // -?Memorizzazione eventuale posizionamento effettuato?
113500131204         if  wOrdSfl = *zero;
113600131203           SavC1Daut = C1Daut;
113700131203         endif;
113800131030
113900131030         // -?Visualizzazione del SFL (se ci sono dati)?
114000131030         SflDsp_N = (S01nrr <= *zero);
114100131030
114200131030         // -?Attivazione del SFLEND?
114300131030         SflEnd = ( $EoF );
114400131030
114500131030       ENDSR;
114600131121
114700131121       //--------------------------------------------------------------
114800131121       //?Selezione del singolo record in input                        ?
114900131121       //--------------------------------------------------------------
115000131121       BEGSR  sr_SelezRec;
115100131121
115200131121         $RecOK = *off;
115300131204
115400131204         // -?Reperimento dati della Distinta Consegna?
115500131121
115600131121         Select;
115700131121
115800131121           // -?Filiale Gestione diversa?
115900131121           //  ?(IMPOSSIBILE perchè in chiave)?
116000131121           //When  CAUfgs <> V1Cfgs;
116100131121           //  setll  (*hival)  FICAU000;
116200131121           //  leavesr;
116300131121
116400131121           // -?Record Annullato?
116500131121           When  CAUatb <> *blank;
116600131121             leavesr;
116700131205
116800131205           // -?Distinta NON nel range selezionato?
116900131205           When  W1ndcI > *zero  and
117000131205                (CAUnfv < W1ndcI  or  CAUnfv > V1ndcF);
117100131205             leavesr;
117200131121
117300131121           // -?Autotrasportatore diverso da quello selezionato?
117400131204           When  W1Caut <> *zero  and  CAUpdr <> W1Caut;
117500131121             leavesr;
117600131121
117700131121           // -?Ammessi Tipi Oggetto "C" (consegna) e "R" (ritiro)?
117800131121           When  CAUtor <> c_Consegna  and  CAUtor <> c_Ritiro;
117900131121             leavesr;
118000131126
118100131126           // -?Solo Telefonate SENZA risposta?
118200131126           When  CAUflt <> 'T';
118300131126             leavesr;
118400131126           When  CAUdtOtel <> *blank  and  CAUdtOtel <> *zero;
118500131126             leavesr;
118600131121
118700131121         EndSl;
118800131204
118900131204
119000131204         // -?Reperimento dati dell'Autotrasportatore?
119100131204         //  ?(SE richiesto un posizionamento)?
119200131204         IF  wOrdSfl = *zero  and  C1Daut <> *blank;
119300131204
119400131204           If  CAUpdr <> k_APDpdr;
119500131204             k_APDpdr = CAUpdr;
119600131204             chain  %kds( keyFIAPD01 )  FIAPD000;
119700131204             if  Not %found(FIAPD01L)  or  APDatb <> *blank;
119800131204               APDrsc = *all'? ';
119900131204             endif;
120000131204           EndIf;
120100131204
120200131204           // -?Richiesto posizionamento per Autotrasportatore?
120300131204           if  APDrsc < C1Daut;
120400131204             leavesr;
120500131204           endif;
120600131204
120700131204         ENDIF;
120800131204
120900131204
121000140121
121100131204         // -?Reperimento dati della distinta?
121200131211         //  ?(PER reperirne il flag PDA   &?
121300131211         //  ? PER filtrare i dati in base ad eventuali parzializzazioni)?
121400131212         k_DSTfgs = CAUfgs;
121500131212         k_DSTnfv = CAUnfv;
121600131212         chain  %kds( keyFIDST09 )  FIDST000;
121700131212         if  Not  %found(FIDST09L);
121800131212           clear  DSTpda;
121900131212           clear  DSTtpm;
122000131212           clear  DSTfpp;
122100140121           clear  DSTflr;
122200131212         endif;
122300140121
122400140121         dDSTflr = DSTflr;
122500131211
122600131211         Select;
122700131211
122800131211           // -?Tipo Distinta diverso da quello selezionato?
122900131211           When  CAUtor = c_Consegna  and
123000131211                 V1Tdis = 'P'         and  DSTtpm <> *blank;
123100131211             leavesr;
123200131211           When  CAUtor = c_Consegna  and
123300131211                 V1Tdis = 'M'         and  DSTtpm <> V1Tdis;
123400131211             leavesr;
123500131211
123600131211           // -?Tipo Uscita diverso da quello selezionato?
123700131211           When  CAUtor =  c_Consegna  and
123800131211                 V1Tusc <> *blank      and  DSTfpp <> V1Tusc;
123900131211             leavesr;
124000131211
124100131211         EndSl;
124200131204
124300131204
124400131204         // -?Verifica SE Distinta chiusa per consegne e ritiri?
124500131204         //  ?(FIDST09L già chainato nella subr. sr_NewRecS01)?
124600131211         If  CAUtor = c_Consegna;
124700131204           wFGS = DSTfgs;
124800131204           wNDC = DSTnfv;
124900131204           exsr  sr_CtrlDistChius;
125000131204           if  $DistChiusa;
125100131204             leavesr;
125200131204           endif;
125300131211         EndIf;
125400131121
125500131204
125600131121         // -?Telefonata da caricare nel subfile?
125700131121         $RecOK = *on;
125800131121
125900131121       ENDSR;
126000131121
126100131121       //--------------------------------------------------------------
126200131121       //?Impostazione nuovi campi nel subfile S01 (a rottura)         ?
126300131121       //--------------------------------------------------------------
126400131121       BEGSR  sr_NewRecS01;
126500131121
126600131121         // -?Pulizia record del subfile?
126700131121         clear  LRX1S01;
126800131121
126900131211         select;
127000131211           when  CAUtor = c_Consegna;
127100131211             ds_Spedizione = CAUogg;
127200131211           when  CAUtor = c_Ritiro;
127300131211             ds_ORM = CAUogg;
127400131211         endsl;
127500131121
127600131204         // -?Decodifica Autotrasportatore?
127700131204         //  ?(SE non già fatto)?
127800131204         If  CAUpdr <> k_APDpdr;
127900131204           k_APDpdr = CAUpdr;
128000131204           chain  %kds( keyFIAPD01 )  FIAPD000;
128100131204           if  Not %found(FIAPD01L)  or  APDatb <> *blank;
128200131204             APDrsc = *all'? ';
128300131204           endif;
128400131204         EndIf;
128500131121
128600131204         // -?Reperimento Num. Telefono Autotrasportatore?
128700131204         //  ?(campo HIDDEN nel subfile - per ora NON usato)?
128800131204         //clear  dAPD4tel;
128900131204         //k_APD4pdr = CAUpdr;
129000131204         //chain  %kds( keyFIAPD41 )  FIAPD400;
129100131204         //if  %found(FIAPD41L)  and  APD4atb = *blank;
129200131204         //  dAPD4tel = APD4uni;
129300131204         //endif;
129400131204
129500131209
129600131212         // -?Distinta già reperita nella subr. "sr_SelezRec"?
129700131211         //SELECT;
129800131211         //
129900131211         //  // -?SPEDIZIONE?
130000131211         //  WHEN  CAUtor = c_Consegna;
130100131211         //
130200131211         //    If  k_ARBaas <> ds_Spedizione.wAAS  or
130300131211         //        k_ARBlnp <> ds_Spedizione.wLNP  or
130400131211         //        k_ARBnrs <> ds_Spedizione.wNRS  or
130500131211         //        k_ARBnsp <> ds_Spedizione.wNSP;
130600131211         //
130700131211         //      // -?Reperimento distinta dalla spedizione?
130800131211         //      clear  keyFNARB01;
130900131211         //      k_ARBaas = ds_Spedizione.wAAS;
131000131211         //      k_ARBlnp = ds_Spedizione.wLNP;
131100131211         //      k_ARBnrs = ds_Spedizione.wNRS;
131200131211         //      k_ARBnsp = ds_Spedizione.wNSP;
131300131211         //      chain  %kds( keyFNARB01 )  FNARB000;
131400131211         //      if  %found(FNARB01L);
131500131211         //
131600131211         //        // -?Reperimento Flag PDA della Distinta?
131700131211         //        if  ARBndc <> k_DSTnfv;
131800131211         //          k_DSTfgs = (ARBpdc / 10000);
131900131211         //          k_DSTnfv = ARBndc;
132000131211         //          chain  %kds( keyFIDST09 )  FIDST000;
132100131211         //          if  Not  %found(FIDST09L);
132200131211         //            clear  DSTpda;
132300131211         //            clear  DSTtpm;
132400131211         //            clear  DSTfpp;
132500131211         //          endif;
132600131211         //        endif;
132700131211         //
132800131211         //      endif;
132900131211         //
133000131211         //    EndIf;
133100131211         //
133200131211         //  // -?O.R.M.?
133300131211         //  WHEN  CAUtor = c_Ritiro;
133400131211         //
133500131211         //    If  keyFNORM01 <> ds_ORM;
133600131211         //
133700131211         //      // -?Reperimento distinta dall'O.R.M.?
133800131211         //      clear  keyFNORM01;
133900131211         //      k_ORMpoe = ds_ORM.wPOE;
134000131211         //      k_ORMnsr = ds_ORM.wNSR;
134100131211         //      k_ORMnor = ds_ORM.wNOR;
134200131211         //      k_ORMnrv = ds_ORM.wNRV;
134300131211         //      chain  %kds( keyFNORM01 )  FNORM000;
134400131211         //      if  %found(FNORM01L);
134500131211         //
134600131211         //        // -?Reperimento Flag PDA della Distinta?
134700131211         //        if  ORMndc <> k_DSTnfv;
134800131211         //          k_DSTfgs = ORMpor;
134900131211         //          k_DSTnfv = ORMndc;
135000131211         //          chain  %kds( keyFIDST09 )  FIDST000;
135100131211         //          if  Not  %found(FIDST09L);
135200131211         //            clear  DSTpda;
135300131211         //            clear  DSTtpm;
135400131211         //            clear  DSTfpp;
135500131211         //          endif;
135600131211         //        endif;
135700131211         //
135800131211         //      endif;
135900131211         //
136000131211         //    EndIf;
136100131211         //
136200131211         //ENDSL;
136300131125
136400131203
136500131203         // -?Impostazione nuovi campi nel subfile?
136600131203         S1Hfgs   = CAUfgs;
136700131203         S1HdtIns = %subst( CAUdtOins : 1 : 8 );
136800131204         //S1Htel   = dAPD4tel.§APD4tel;
136900131203         S01pdr   = CAUpdr;
137000131121         S01pdrD  = APDrsc;
137100131212         //select;
137200131212         //  when  CAUtor = 'C';
137300131212         //    S01dst = ARBndc;
137400131212         //  when  CAUtor = 'R';
137500131212         //    S01dst = ORMndc;
137600131212         //endsl;
137700131212         S01dst   = CAUnfv;
137800131204         S01pda   = DSTpda;
137900140121         S01tst   = %xlate( c_Up : c_Lo : dDSTflr.§DSTtstPda );
138000131121
138100131121       ENDSR;
138200131203
138300131203       //--------------------------------------------------------------
138400131205       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
138500131205       //?F11-Cambia ordinamento sfl                                   ?
138600131203       //--------------------------------------------------------------
138700131205       BEGSR  sr_F11S01;
138800131203
138900131203         if  wOrdSfl < 1;
139000131203           wOrdSfl += 1;
139100131203         else;
139200131203           clear  wOrdSfl;
139300131203         endif;
139400131203
139500131203         select;
139600131203           // -?Subfile già posizionato: occorre ricaricarlo?
139700131203           when  C1Daut <> *blank;
139800131203             $InzS01 = *on;
139900131203           // -?Subfile vuoto: nessun record da ordinare?
140000131205           when  SavMAXnrr = *zero;
140100131203           // -?Altrimenti: basta riordinarlo?
140200131203           other;
140300131203             exsr  sr_SortSfl;
140400131203         endsl;
140500131203
140600131203         clear  C1Daut;
140700131203         clear  SavC1Daut;
140800131203
140900131203       ENDSR;
141000131203
141100131203       //--------------------------------------------------------------
141200131203       //?Ordinamento subfile                                          ?
141300131203       //?  This subroutine sorts the subfile records.                 ?
141400131203       //--------------------------------------------------------------
141500131203       BEGSR  sr_SortSfl;
141600131203
141700131205         RrnLast  = SavMAXnrr;
141800131203
141900131203         C1RcdNbr = 1;
142000131203         clear  C1CsrRrn;
142100131203         clear  S01nrr;
142200131203         clear  V1Dmsg;
142300131203         ErrMessage  = *off;
142400131203         SflNxtChg   = *off;
142500131203         ErrGenerico = *off;
142600131203         %subst(IndDspF : 50) = *off;
142700131203
142800131203         //?___________________________________________________________?
142900131203         //?Initialize the key fields to sort on.?
143000131203         //?There is one extra field not in the subfile -- Selected --?
143100131203         //?that is added to the record. This field is used to place?
143200131203         //?selected records in front of nonselected records.?
143300131203         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
143400131203
143500131203         clear  QLGKL;
143600131203         clear  QLGSKL;
143700131203         clear  QLGSCB;
143800131203         clear  QLGSCB00;
143900131203
144000131203         // -?Gestione della scelta ordinamento:?
144100131203         Select;
144200131203
144300131203           //  ?Ordinamento per Descrizione Autotrasportatore?
144400131204           When  wOrdSfl = *zero;
144500131203             // -?2 campi chiave?
144600131203             QLGNBRK  = 2;
144700131203             // -?1° campo: Descrizione Autotrasportatore (S01pdrD)?
144800131212             //            ?a posizone   36, 35 byte, char, ascending.?
144900131212             QLGSP    = 36;
145000131203             QLGSS    = %size(S01pdrD);
145100131203             QLGDT    = Carattere;
145200131203             QLGSO    = Ascendente;
145300131203             QLGKL(1) = QLGSKL;
145400131203             // -?2° campo: Codice Autotrasportatore (S01pdr)?
145500131212             //            ?a posizone   29,  7 byte, num., ascending.?
145600131212             QLGSP    = 29;
145700131203             QLGSS    = %size(S01pdr);
145800131203             QLGDT    = Carattere;
145900131203             QLGSO    = Ascendente;
146000131203             QLGKL(2) = QLGSKL;
146100131203
146200131203           //  ?Ordinamento per Numero Distinta?
146300131204           When  wOrdSfl = 1;
146400131203             // -?2 campi chiave?
146500131203             QLGNBRK  = 2;
146600131203             // -?1° campo: Numero Distinta (S01dst)?
146700131212             //            ?a posizone   71,  6 byte, num., ascending.?
146800131212             QLGSP    = 71;
146900131203             QLGSS    = %size(S01dst);
147000131203             QLGDT    = Carattere;
147100131203             QLGSO    = Ascendente;
147200131203             QLGKL(1) = QLGSKL;
147300131203             // -?2° campo: Descrizione Autotrasportatore (S01pdrD)?
147400131212             //            ?a posizone   36, 35 byte, char, ascending.?
147500131212             QLGSP    = 36;
147600131203             QLGSS    = %size(S01pdrD);
147700131203             QLGDT    = Carattere;
147800131203             QLGSO    = Ascendente;
147900131203             QLGKL(2) = QLGSKL;
148000131203
148100131203         EndSl;
148200131203
148300131203         // -?Load other sort parameters.?
148400131203         QLGLB   = 80 + 16 * MaxKey;
148500131203         QLGRL   = %size(SflRcd) - 1;
148600131203         QLGRT   = 8;
148700131203         QLGOKL  = 80;
148800131203         QLGLKE  = 16;
148900131203         QLGLSS  = 290;
149000131203         // -?Initialize Sort I/O API fields.?
149100131203         QLGRL00 = QLGRL;
149200131203         QLGRC00 = 1;
149300131203         clear  QUSEI;
149400131203         QUSBPRV = %size(QUSEC);
149500131203
149600131203         // -?First step - Initialize the sort routine.?
149700131203         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
149800131203                      SizeList : ReturnSize : QUSEC );
149900131203
150000131203         // -?Next step - Write records to I/O routine.?
150100131203         QLGRT00  = Put;
150200131203         For  Nrr = 1  To  RrnLast;
150300131203           chain  Nrr  LRX1S01;
150400131203           // -?Solo le righe con Selected = 'Y' sono riordinate,?
150500131203           //  ?quindi per fare un ordinamento di tutte le righe?
150600131203           //  ?metto 'Y' sempre.?
150700131203           Selected = 'Y';
150800131203           clear  QUSEI;
150900131203           QUSBPRV  = %size(QUSEC);
151000131203           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
151100131203                         SizeList : NotUsed : QUSEC );
151200131203         EndFor;
151300131203
151400131203         // -?Next step - Signal end of input, clear subfile for reload.?
151500131203         QLGRT00 = EndPut;
151600131203         clear  QUSEI;
151700131203         QUSBPRV = %size(QUSEC);
151800131203         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
151900131203                       SizeList : NotUsed : QUSEC );
152000131203
152100131203         // -?Pulizia subfile?
152200131203         SflDsp_N    = *on;
152300131203         SflDspCtl_N = *on;
152400131204         write  LRX1C01;
152500131203         SflDspCtl_N = *off;
152600131203         SflEnd      = *off;
152700131203
152800131203         // -?Final step - Write the records back to the subfile.?
152900131203         QLGRT00  = Get;
153000131203         For  Nrr = 1  To RrnLast;
153100131203           clear  QUSEI;
153200131203           QUSBPRV = %size(QUSEC);
153300131203           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
153400131203                         QLGRL00  : NotUsed : QUSEC );
153500131203           // -?Ripristino indicatori utilizzati nel sfl rec?
153600131204           SflNxtChg  = (S01opz <> *zero);
153700131204           Ord_x_Dist = (wOrdSfl = 1);
153800131203           S01nrr = Nrr;
153900131203           write  LRX1S01;
154000131203         EndFor;
154100131203
154200131203         C1CsrRrn = 1;
154300131203
154400131203         // -?Visualizzazione del SFL (se ci sono dati)?
154500131203         SflDsp_N = (S01nrr <= *zero);
154600131203
154700131203         // -?Attivazione del SFLEND?
154800131203         SflEnd = $EoF;
154900131204
155000131205         // -?Impostazione F11 nel piede del subfile?
155100131204         select;
155200131204           when  S01nrr  = *zero;
155300131205             clear  V1PF11;
155400131204           when  wOrdSfl = 0;
155500131205             V1PF11 = 'F11=Ord. x Num.Dist.';
155600131204           when  wOrdSfl = 1;
155700131205             V1PF11 = 'F11=Ord. x Autotrasp';
155800131204         endsl;
155900131203
156000131203       ENDSR;
156100131030
156200131030       //--------------------------------------------------------------
156300131104       //?Gestione opzioni del subfile S01                             ?
156400131030       //--------------------------------------------------------------
156500131030       BEGSR  sr_OpzS01;
156600131030
156700131030         clear  SavS01csr;
156800131030
156900131030         // -?Ciclo di lettura subfile?
157000131104         readc  LRX1S01;
157100131030
157200131104         DoW  Not %eof(FNLRX1D);
157300131030
157400131030           SflNxtChg = *off;
157500131030           %subst(IndDspF : 50) = *off;
157600131030           C1RcdNbr = S01nrr;
157700131030
157800131030           select;
157900131030
158000131030             // -?Nessuna opzione?
158100131121             when  S01opz = *zero;
158200131104
158300131104             // -?Opz. 1=Gestione?
158400131121             when  S01opz = 1;
158500131104               exsr  sr_Call_FNLRX2;
158600131030
158700131030             // -?Opzione errata?
158800131030             other;
158900131030               ErrGenerico = *on;
159000131030               ErrMessage  = *on;
159100131030               PosCurOPZ   = *on;
159200131204               V1Dmsg = sk_Msg(14);
159300131030
159400131030           endsl;
159500131030
159600131030           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
159700131030           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
159800131121           if  S01opz  <> *zero    and
159900131030              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
160000131030             SavS01csr   = C1RcdNbr;
160100131030           endif;
160200131030
160300131030           // -?Aggiornamento sfl?
160400131030           select;
160500131030             when  ErrMessage;
160600131030               SflNxtChg = *on;
160700131030               C1CsrRrn  = C1RcdNbr;
160800131030             when  ErrGenerico;
160900131030               C1CsrRrn  = C1RcdNbr;
161000131121               clear  S01opz;
161100131030             other;
161200131121               clear  S01opz;
161300131030           endsl;
161400131206
161500131206           Ord_x_Dist = (wOrdSfl = 1);
161600131030
161700131104           UPDATE  LRX1S01;
161800131030
161900131030           if  ErrGenerico   or   ErrMessage;
162000131030             leave;
162100131030           endif;
162200131030
162300131104           readc  LRX1S01;
162400131030
162500131030         ENDDO;
162600131030
162700131030         // -?Riposizionam. cursore sul record contenente la prima opz.?
162800131030         //  ?(se non sono stati rilevati errori)?
162900131030         if  NOT ErrMessage   and   NOT ErrGenerico   and
163000131030             SavS01csr > *zero;
163100131030           C1CsrRrn = SavS01csr;
163200131030         endif;
163300131030
163400131030       ENDSR;
163500131030
163600131030       //--------------------------------------------------------------
163700131104       //?Richiamo *pgm FNLRX2R (Gestione Contatti Autotrasportatore)  ?
163800131030       //--------------------------------------------------------------
163900131104       BEGSR  sr_Call_FNLRX2;
164000131030
164100131121         reset  FNLRX2ds;
164200131121         FNLRX2ds.iX2fgs = S1Hfgs;
164300131205         FNLRX2ds.iX2aut = S01pdr;
164400131205         FNLRX2ds.iX2dst = S01dst;
164500131104
164600131204         kpjbU    = FNLRX2ds;
164700131104
164800131204         FNLRX2R ( kpjbA );
164900131104
165000131204         FNLRX2ds = kpjbU;
165100131104
165200131125         // -?Aggiornata almeno una telefonata?
165300131125         if  FNLRX2ds.oX2upd = *on;
165400131125           $InzS01 = *on;
165500131125         endif;
165600131125         // -?Ultima uscita con F3=Fine?
165700131125         if  FNLRX2ds.oX2fxx = '03';
165800131125           ErrGenerico = *on;
165900131125         endif;
166000131030
166100131030       ENDSR;
166200131030
166300131030       //--------------------------------------------------------------
166400131030       //?Operazioni finali.                                           ?
166500131030       //--------------------------------------------------------------
166600131030       BEGSR  sr_RoutEnd;
166700131030
166800131121         // -?Uscita dal *pgm?
166900131030         return;
167000131030
167100131030       ENDSR;
167200131030
167300131030      /end-free
167400131030
167500131121** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
167600131121Nessuna filiale gestibile dall'utente.  Premere "Invio" per uscire.            1
167700131121Filiale in gestione obbligatoria                                               2
167800131121Filiale in gestione errata                                                     3
167900131204La selezione è possibile per Distinta o per AUT, non entrambe                  4
168000131204Numero distinta errato: contiene caratteri NON numerici                        5
168100131204Numero distinta inesistente o annullato                                        6
168200131204Numero distinta obbligatorio                                                   7
168300131204Limiti incompatibili: numero distinta al minore del numero distinta dal        8
168400131204Distinta già chiusa                                                            9
168500131204Autotrasportatore errato: contiene caratteri NON numerici                     10
168600131204Autotrasportatore inesistente o annullato                                     11
168700131204Autotrasportatore non in gestione all'utente                                  12
168800131204Superato il numero massimo di record caricabili a video: contattare il CED    13
168900131204Opzione errata                                                                14
169000160506Per le Messaggerie non è possibile impostare il Turno                         15
