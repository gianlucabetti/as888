000100120627       //==============================================================
000200120627       //?Gestione tabella "CT" (Codici Tassazione)                    ?
000300120627       //==============================================================
000400120627
000500120627       //--------------------------------------------------------------
000600120627       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120627       //--------------------------------------------------------------
000800120627
000900120627     /*PRM  dbgview(*source)
001000120627     /*END
001100120627
001200120627       //--------------------------------------------------------------
001300120627       //?Specifiche di controllo.                                     ?
001400120627       //--------------------------------------------------------------
001500120627
001600060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700120629     h dftactgrp(*no)
001800120629     h bnddir('TRUL')
001900060418
002000120627       //--------------------------------------------------------------
002100120627       //?Dichiarazione file.                                          ?
002200120627       //--------------------------------------------------------------
002300060418
002400120627       // -?Tabelle?
002500120627     fTABEL00F  Uf A e           k disk
002600120627
002700120627       // -?Anagrafico Lingue?
002800120627     fAZLIN01L  if   e           k disk
002900120627
003000120627       // -?Video?
003100120627     fTRTB09D   cf   e             workstn
003200120627     f                                     indds(IndDspF)
003300120627     f                                     infds(InfDspF)
003400120627     f                                     sfile(TB09S01:S01nrr)
003500120627     f                                     sfile(TB09S02:S02nrr)
003600120627
003700120627       //--------------------------------------------------------------
003800120627       //?Definizione costanti.                                        ?
003900120627       //--------------------------------------------------------------
004000120627
004100120627       // -?Codice tabella in gestione?
004200120629     d c_Tab           c                   const('CT')
004300120627
004400120627       // -?Codice utente?
004500120627     d c_Kut           c                   const(1)
004600120627
004700120627       // -?Opzioni a video?
004800120628     d c_Opz_G         c                   const('2=Modifica, +
004900120627     d                                            3=Copia, +
005000120627     d                                            4=Annullam./Ripristino, +
005100120627     d                                            5=Visualizzaz.')
005200120628     d c_Opz_S         c                   const('1=Scelta.')
005300120627
005400120627       // -?Numero di record in ciascuna videata di subfile?
005500120711     d c_SflPag        c                   const(15)
005600120627     d c_SflPag2       c                   const(10)
005700120627
005800120627       // -?Numero massimo di record gestibili?
005900120627     d c_MaxRec        c                   const(9999)
006000120627
006100120627       // -?Tasti funzionali a video?
006200120627     d c_F01           c                   const(x'31')
006300120627     d c_F02           c                   const(x'32')
006400120627     d c_F03           c                   const(x'33')
006500120627     d c_F04           c                   const(x'34')
006600120627     d c_F05           c                   const(x'35')
006700120627     d c_F06           c                   const(x'36')
006800120627     d c_F07           c                   const(x'37')
006900120627     d c_F08           c                   const(x'38')
007000120627     d c_F09           c                   const(x'39')
007100120627     d c_F10           c                   const(x'3A')
007200120627     d c_F11           c                   const(x'3B')
007300120627     d c_F12           c                   const(x'3C')
007400120627     d c_F13           c                   const(x'B1')
007500120627     d c_F14           c                   const(x'B2')
007600120627     d c_F15           c                   const(x'B3')
007700120627     d c_F16           c                   const(x'B4')
007800120627     d c_F17           c                   const(x'B5')
007900120627     d c_F18           c                   const(x'B6')
008000120627     d c_F19           c                   const(x'B7')
008100120627     d c_F20           c                   const(x'B8')
008200120627     d c_F21           c                   const(x'B9')
008300120627     d c_F22           c                   const(x'BA')
008400120627     d c_F23           c                   const(x'BB')
008500120627     d c_F24           c                   const(x'BC')
008600120627     d c_Enter         c                   const(x'F1')
008700120627     d c_RollDown      c                   const(x'F4')
008800120627     d c_RollUp        c                   const(x'F5')
008900120627
009000120627       //--------------------------------------------------------------
009100120627       //?Definizione schiere.                                         ?
009200120627       //--------------------------------------------------------------
009300120627
009400120628       // -?Messaggi di errore?
009500120705     d $Msg            s             78    dim(12)  ctdata  perrcd(1)
009600120628
009700120628       // -?Opzioni abilitate?
009800120628     d $Opz            s               n   dim( 5)  inz
009900120627
010000120627       //--------------------------------------------------------------
010100120627       //?Definizione aree dati.                                       ?
010200120627       //--------------------------------------------------------------
010300120627
010400120627       // -?Dati utente?
010500120627     d §AzUte        e ds                  extname(AZUTE00F)
010600120627     d                                     dtaara
010700120627     d §DatiUte      e ds                  extname(dDatiUte)
010800120627     d                                     dtaara
010900120627
011000120627       //--------------------------------------------------------------
011100120627       //?Definizione strutture dati.                                  ?
011200120627       //--------------------------------------------------------------
011300120627
011400120627       // -?Status ds?
011500120627     d Status         sds
011600120627     d   SDSpgm          *proc
011700120627
011800120627       // -?InfDS?
011900120627     d InfDspF         ds
012000120627     d   dsp_aid             369    369a
012100120627     d   sfl_rrn             376    377i 0
012200120627     d   min_nrr             378    379i 0
012300120627     d   num_rcds            380    381i 0
012400120627
012500120627       // -?Indicatori su DspF?
012600120627     d IndDspF         ds                  inz
012700120627         // -?Abilitazione tasti funzionali?
012800120711     d   F3Attivo                      n   overlay(IndDspF : 03)
012900120627     d   F5Attivo                      n   overlay(IndDspF : 05)
013000120627     d   F6Attivo                      n   overlay(IndDspF : 06)
013100120629     d   F8Attivo                      n   overlay(IndDspF : 08)
013200120627     d   F10Attivo                     n   overlay(IndDspF : 10)
013300120627     d   F12Attivo                     n   overlay(IndDspF : 12)
013400120627     d   F13Attivo                     n   overlay(IndDspF : 13)
013500120627         // -?Emissione messaggio di errore?
013600120627     d   ErrMessage                    n   overlay(IndDspF : 28)
013700120627         // -?Indicatori di gestione del subfile?
013800120627     d   SflDsp_N                      n   overlay(IndDspF : 30)
013900120627     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
014000120627     d   SflNxtChg                     n   overlay(IndDspF : 32)
014100120627     d   SflEnd                        n   overlay(IndDspF : 33)
014200120627     d   WSflDsp_N                     n   overlay(IndDspF : 35)
014300120627     d   WSflDspCtl_N                  n   overlay(IndDspF : 36)
014400120627     d*//WSflNxtChg                    n   overlay(IndDspF : 37)
014500120627     d   WSflEnd                       n   overlay(IndDspF : 38)
014600120627         // -?Indicatori per Attibuti di visualizzazione?
014700120627         //  ?o   Condizionamento visualizzazione campi?
014800120627     d   Ord_x_COD                     n   overlay(IndDspF : 40)
014900120627     d   Ord_x_DES                     n   overlay(IndDspF : 41)
015000120627     d   Ord_x_NaRePr                  n   overlay(IndDspF : 42)
015100120627     d   Ord_x_COR                     n   overlay(IndDspF : 43)
015200120628     d   Tass_Annull                   n   overlay(IndDspF : 47)
015300120628     d   Protect_Des                   n   overlay(IndDspF : 48)
015400120628     d   Protect_Cod                   n   overlay(IndDspF : 49)
015500120627         // -?Posizionamento cursore & Segnalazione errore?
015600120627     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015700120627     d   PosCurCOD                     n   overlay(IndDspF : 51)
015800120627     d   PosCurDES                     n   overlay(IndDspF : 52)
015900120627     d   PosCurPRV                     n   overlay(IndDspF : 53)
016000120627     d   PosCurRAP                     n   overlay(IndDspF : 54)
016100120627     d   PosCurCOR                     n   overlay(IndDspF : 55)
016200120627     d   PosCurNAR                     n   overlay(IndDspF : 56)
016300120627     d   PosCurCAP                     n   overlay(IndDspF : 57)
016400120627     d   PosCurW1des                   n   overlay(IndDspF : 81)
016500120627         // -?Riemissione videata?
016600120627     d   ErrGenerico                   n   overlay(IndDspF : 99)
016700120627
016800120627       // -?Parametri ricevuti?
016900120627     d KPJBA         e ds
017000120627     d TRTB09ds      e ds                  inz
017100120627
017200120627       // -?Tab. "CT" = Codici Tassazione?
017300120627     d dsCT          e ds                  inz
017400120705     d dsCT_rap      e ds                  inz   extname(dsCT)
017500120705     d                                           prefix(rap)
017600120627       // -?Tab. "PR" = Provincie?
017700120627     d dsPR          e ds                  inz
017800120627       // -?Tab. "15" = Nazioni?
017900120627     d ds15          e ds                  inz
018000120627
018100120627       //--------------------------------------------------------------
018200120627       //?Definizione variabili globali.                               ?
018300120627       //--------------------------------------------------------------
018400120627
018500120627       // -?Flags booleani?
018600120711     d $Called         s               n   inz
018700120627     d $Fine           s               n   inz
018800120627     d $EoF            s               n   inz
018900120711     d $InzD01         s               n   inz(*on)
019000120627     d $InzS01         s               n   inz(*on)
019100120627     d $InzD02         s               n   inz(*on)
019200120627     d $InzW01         s               n   inz(*on)
019300120628     d $InzS02         s               n   inz(*on)
019400120627     d $RecOK          s               n   inz
019500120702     d $F09D02         s               n   inz
019600120702     d $ForzaOpz       s               n   inz
019700120627
019800120627       // -?Variabili per la gestione del video?
019900120712     d $Video          s              2    inz('S1')
020000120629     d*// $VideoPrec      s                   like($Video)   inz
020100120627     d S01nrr          s                   like(C1RcdNbr) inz
020200120627     d S02nrr          s                   like(C2RcdNbr) inz
020300120710     d S01nrrMax       s                   like(S01nrr)   inz
020400120627     d SavS01csr       s                   like(C1RcdNbr) inz
020500120628     d SavS1Copz       s                   like(S1Copz)   inz
020600120703     d SavS01cs1       s                   like(C1RcdNbr) inz
020700120703     d SavS1Cop1       s                   like(S1Copz)   inz
020800120702     d SavS01cs2       s                   like(C1RcdNbr) inz
020900120703     d*// SavS1Cop2       s                   like(S1Copz)   inz
021000120702     d SavS02nrr       s                   like(S02nrr)   inz
021100120629     d SavTBLuni       s                   like(TBLuni)   inz
021200120629     d SavC1Ccod       s                   like(C1Ccod)   inz
021300120629     d SavC1Cdes       s                   like(C1Cdes)   inz
021400120629     d SavC1Cnar       s                   like(C1Cnar)   inz
021500120629     d SavC1Crap       s                   like(C1Crap)   inz
021600120629     d SavC1Cprv       s                   like(C1Cprv)   inz
021700120629     d SavC1Ccor       s                   like(C1Ccor)   inz
021800120628     d wOrdSfl         s              1  0 inz
021900120702     d w_SflPag        s              3  0 inz
022000120702     d wPag            s              4  0 inz
022100120702     d wRig            s              3  0 inz
022200120628
022300120628       // -?Campi di comodo?
022400120712     d wDate_DMY       s               d   datfmt(*dmy)  inz
022500120711     d wTB09cmd        s              1    inz
022600120711     d*// wTB09opz        s              2  0 inz
022700120627
022800120627       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
022900120627
023000120627       //--------------------------------------------------------------
023100120627       // -?Constants?
023200120627       //--------------------------------------------------------------
023300120627       // -?MaxKey - Maximum number of key fields allowed by this program?
023400120629     d MaxKey          c                   const(4)
023500120627       // -?Sort order:?
023600120627       //  ?1 = Sort in an ascending sequence?
023700120627       //  ?2 = Sort in a descending sequence?
023800120627     d Ascendente      c                   const(1)
023900120627     d Discendente     c                   const(2)
024000120627       // -?Key data type:?
024100120627       //  ? 0 = Signed binary?
024200120627       //  ? 2 = Signed zoned decimal?
024300120627       //  ? 3 = Signed packed decimal?
024400120627       //  ? 6 = Character with no national language sort sequence applied,?
024500120627       //  ?     if specified?
024600120627       //  ? 7 = Unsigned packed decimal?
024700120627       //  ?     All numbers will have the sign forced positive ('F'X).?
024800120627       //  ? 8 = Unsigned zoned decimal?
024900120627       //  ?     All numbers will have the sign forced positive ('F'X).?
025000120627       //  ? 9 = Unsigned binary?
025100120627       //  ?14 = Date in form of DD/MM/YY?
025200120627       //  ?15 = Date in form of DD.MM.YYYY?
025300120627     d Numero          c                   const(2)
025400120627     d Carattere       c                   const(6)
025500120627       //
025600120627     d Put             c                   const(1)
025700120627     d EndPut          c                   const(2)
025800120627     d Get             c                   const(3)
025900120627
026000120627       //--------------------------------------------------------------
026100120627       // -?Data Structures?
026200120627       //  ?SflRcd     - The entire subfile record to pass to the sort?
026300120627       //  ?QLGSCB     - The sort request block for the QLGSORT API?
026400120627       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
026500120627       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
026600120627       //  ?QUSEC      - Error structure for the QLGSORT API?
026700120627       //--------------------------------------------------------------
026800120627     d SflRcd          ds                  inz
026900120628     d   S1Ccod                            inz
027000120628     d   S1Cdes                            inz
027100120628     d   S1Cnar                            inz
027200120628     d   S1Crap                            inz
027300120628     d   S1Cprv                            inz
027400120628     d   S1Ccor2                           inz
027500120702     d   S1Hcor                            inz
027600120628     d   S1Ccor1                           inz
027700120702     d   S1Cfcr                            inz
027800120628     d   S1Cuta                            inz
027900120628     d   S1Cutf                            inz
028000120705     d   S1Csta                            inz
028100120628     d   S1Catb                            inz
028200120628     d   S1Copz                            inz
028300120628     d   Selected                     1a   inz
028400120627      /copy QSYSINC/QRPGLESRC,QLGSORT
028500120627     d QLGKL                         16    dim(MaxKey)
028600120628     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
028700120628     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
028800120628     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
028900120628     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
029000120627      /copy QSYSINC/QRPGLESRC,QLGSRTIO
029100120627      /copy QSYSINC/QRPGLESRC,QUSEC
029200120627
029300120627       //--------------------------------------------------------------
029400120627       // -?Standalone fields?
029500120627       //  ?Nrr        - Relative record number for subfile?
029600120627       //  ?SizeList   - Size of list?
029700120627       //  ?ReturnSize - Size of list returned by sort API?
029800120627       //--------------------------------------------------------------
029900120627     d NotUsed         s             16a   inz
030000170606     d ReturnSize      s             10i 0 inz
030100170606     d SizeList        s             10i 0 inz
030200120627     d RrnLast         s              5i 0 inz
030300120627     d Nrr             s              5i 0 inz
030400060510
030500120627       //--------------------------------------------------------------
030600120627       //?Definizione prototipi procedure.                             ?
030700120627       //--------------------------------------------------------------
030800120627
030900120627       // -?Reperimento dati utente?
031000120627     d TIBS34ds      e ds                  inz
031100120627      /copy gaitrasrc/srcProtoPR,TIBS34R
031200060523
031300120627       // -?Ricerca tabella TABEL00F?
031400120629     d idElemento      s              8    inz
031500120629     d tastoFunzionaleUscita...
031600120629     d                 s              1    inz
031700120627      /copy gaitrasrc/srcProtoPR,TRUL19R
031800120627
031900120627       // -?Reperimento dati tabelle?
032000120627      /copy gaitrasrc/srcProtoPI,TRULTAB
032100120627      /copy gaitrasrc/srcProtoPR,TRULTAB
032200120627
032300120627       // -?Ordinamento subfile?
032400120627      /copy gaitrasrc/srcProtoPr,QLGSRTIO
032500120627
032600120627       //--------------------------------------------------------------
032700120627       //?Definizione key-list.                                        ?
032800120627       //--------------------------------------------------------------
032900120627
033000120627       // -?File TABEL00F?
033100120627     d k03tabel00    e ds                  extname(TABEL00F : *key)
033200120627     d                                     prefix(k_)   inz
033300120627
033400120627       //--------------------------------------------------------------
033500120627       //?Riepilogo indicatori utilizzati.                             ?
033600120627       //--------------------------------------------------------------
033700120627       // --
033800120627       //--------------------------------------------------------------
033900120627
034000120627       //--------------------------------------------------------------
034100120627       //?M A I N - L I N E                                            ?
034200120627       //--------------------------------------------------------------
034300060510
034400120627     c     *Entry        plist
034500120627     c                   parm                    KPJBA
034600120627
034700060510      /free
034800060510
034900120627       // -?Operazioni iniziali?
035000120627       exsr  sr_RoutInz;
035100120627
035200120627       // -?Ciclo di gestione del file video?
035300120627       DoW  Not  $Fine;
035400120627         select;
035500120627           // -?Gestione videata S1 (subfile Cod.Tassazione)?
035600120627           when  $Video = 'S1';
035700120627             exsr  sr_GesS01;
035800120627           // -?Gestione videata D2 (gestione singolo Cod.Tassazione)?
035900120629           //  ?- Inserimento -?
036000120629           when  $Video = 'D2';
036100120629             exsr  sr_GesD02;
036200120711           // -?Gestione videata D1 (parzializzazioni)?
036300120711           when  $Video = 'D1';
036400120711             exsr  sr_GesD01;
036500120627           // -?Gestione videata S2 (subfile Lingue)?
036600120629           //  ?(videata gestita direttamente dalla videata D02)?
036700120629           //when  $Video = 'S2';
036800120629           //  exsr  sr_GesS02;
036900120627           // -?Gestione videata W1 (trasmissione dati)?
037000120629           //  ?(videata gestita direttamente dalla videata D02)?
037100120629           //when  $Video = 'W1';
037200120629           //  exsr  sr_GesW01;
037300120627           // -?? ? ? ? ??
037400120627           other;
037500120627             $Fine = *on;
037600120627         endsl;
037700120627       enddo;
037800120627
037900120627       // -?Operazioni finali?
038000120627       exsr  sr_RoutEnd;
038100060510
038200120627       //--------------------------------------------------------------
038300120627       //?Operazioni iniziali.                                         ?
038400120627       //--------------------------------------------------------------
038500120627       BEGSR  sr_RoutInz;
038600120627
038700120627         // -?Impostazione chiusura?
038800120627         *inLR = *on;
038900120627
039000120627         // -?Impostazione parametri ricevuti?
039100120711         TRTB09ds  = KPJBU;
039200120711         clear  TB09des;
039300120711         clear  TB09fxx;
039400120711         TB09esito = *off;
039500120711         //clear  wTB09opz;
039600120628
039700120628         // -?Controllo parametri ricevuti?
039800120711         select;
039900120711           when  TB09fun <> *blank  and  TB09fun <> '1';
040000120711             TB09esito = *on;
040100120711           when  TB09est <> *blank  and  TB09est <> 'E'  and
040200120711                 TB09est <> 'I';
040300120711             TB09esito = *on;
040400120711           when  TB09uta <> *blank  and  TB09uta <> 'N'  and
040500120711                 TB09uta <> 'S';
040600120711             TB09esito = *on;
040700120711           when  TB09utf <> *blank  and  TB09utf <> 'N'  and
040800120711                 TB09utf <> 'S';
040900120711             TB09esito = *on;
041000120711           when  TB09utc <> *blank  and  TB09utc <> 'N'  and
041100120711                 TB09utc <> 'S';
041200120711             TB09esito = *on;
041300120711         endsl;
041400120711         if  TB09esito = *on;
041500120711           exsr  sr_RoutEnd;
041600120711         endif;
041700120627
041800120627         // -?Reperimento dati job?
041900120627         exsr  sr_DatiJob;
042000120627
042100120627         // -?Impostazione nome programma a video?
042200120627         V1Tpgm = SDSpgm;
042300120627
042400120627         // -?Impostazione iniziale / pulizia dei campi chiave?
042500120629         clear  k03tabel00;
042600120628         k_TBLkut = c_Kut;
042700120627         k_TBLcod = c_Tab;
042800120627         wOrdSfl = *zero;
042900120712         if  TB09ord >= '1'  and  TB09ord <= '4';
043000120712           wOrdSfl = %int(TB09ord);
043100120712         endif;
043200120711
043300120711         // -?Impostazione videata iniziale "S1" se ricevuti parametri?
043400120711         $Called = (TB09fun  = '1'     or  TB09est <> *blank  or
043500120711                    TB09uta <> *blank  or  TB09utf <> *blank  or
043600120711                    TB09utc <> *blank  or  TB09cod <> *blank);
043700120711         if  $Called;
043800120711           exsr  sr_InzD01;
043900120712           //$Video = 'S1';        ?(già così)?
044000120711         endif;
044100060510
044200120627       ENDSR;
044300120627
044400120627       //--------------------------------------------------------------
044500120627       //?Reperimento Dati del job (Utente/Operativi).                 ?
044600120627       //--------------------------------------------------------------
044700120627       BEGSR  sr_DatiJob;
044800120627
044900120627         in(e) §AzUte;
045000120627         if NOT %error;
045100120627           in(e) §DatiUte;
045200120627         endif;
045300120627         if %error or RSut = *blank;
045400120627           tibs34r ( tibs34ds );
045500120627           in §AzUte;
045600120627           in §DatiUte;
045700120627         endif;
045800120627
045900120627       ENDSR;
046000120711
046100120711       //--------------------------------------------------------------
046200120711       //?Gestione subfile D01.                                        ?
046300120711       //--------------------------------------------------------------
046400120711       BEGSR  sr_GesD01;
046500120711
046600120711         // -?Inizializzazione videata?
046700120711         if  $InzD01 = *on;
046800120711           exsr  sr_InzD01;
046900120711           $InzD01 = *off;
047000120711         endif;
047100120711
047200120711         // -?Emissione Testata?
047300120711         write  TB09T01;
047400120711
047500120711         // -?Emissione videata (e piede)?
047600120711         exfmt  TB09D01;
047700120711
047800120711         reset  ErrGenerico;
047900120711         reset  ErrMessage;
048000120711         clear  V1Dmsg;
048100120711
048200120711         select;
048300120711
048400120711           // -?F3 = Fine?
048500120711           when  dsp_aid = c_F03;
048600120711             $Fine = *on;
048700120711             TB09fxx = 'C';
048800120711
048900120711           // -?Invio?
049000120711           OTHER;
049100120711             exsr  sr_CtrD01;
049200120711             if  ErrGenerico;
049300120711               leavesr;
049400120711             endif;
049500120711             $Video = 'S1';
049600120711             reset  $InzS01;
049700120711
049800120711         ENDSL;
049900120711
050000120711       ENDSR;
050100120711
050200120711       //--------------------------------------------------------------
050300120711       //?Inizializzazione videata D01.                                ?
050400120711       //--------------------------------------------------------------
050500120711       BEGSR  sr_InzD01;
050600120711
050700120711         clear  TB09D01;
050800120711
050900120711         // -?Impostazione parametri ricevuti?
051000120711         V1Cest = TB09est;
051100120711         V1Cuta = TB09uta;
051200120711         V1Cutf = TB09utf;
051300120711         V1Cutc = TB09utc;
051400120711
051500120711       ENDSR;
051600120711
051700120711       //--------------------------------------------------------------
051800120711       //?Controllo dati nella videata D01.                            ?
051900120711       //--------------------------------------------------------------
052000120711       BEGSR  sr_CtrD01;
052100120711
052200120711         %subst(IndDspF : 50) = *off;
052300120711
052400120711       ENDSR;
052500120627
052600120627       //--------------------------------------------------------------
052700120627       //?Gestione subfile S01.                                        ?
052800120627       //--------------------------------------------------------------
052900120627       BEGSR  sr_GesS01;
053000120627
053100120627         // -?Inizializzazione videata?
053200120627         if  $InzS01 = *on;
053300120627           exsr  sr_InzS01;
053400120627           $InzS01 = *off;
053500120627         endif;
053600120627
053700120627         // -?Emissione Testata & Piede?
053800120627         write  TB09T01;
053900120629         write  TB09Z01;
054000120627
054100120627         // -?Posizionamento cursore?
054200120627         if  C1CsrRrn > *zero;
054300120627           C1RcdNbr = C1CsrRrn;
054400120627         else;
054500120627           C1RcdNbr = 1;
054600120627           write  TB09S00;             //?(no rec.)?
054700120627         endif;
054800120627
054900120627         // -?Emissione videata?
055000120627         exfmt  TB09C01;
055100120627
055200120627         reset  ErrGenerico;
055300120628         reset  ErrMessage;
055400120627         clear  V1Dmsg;
055500120629
055600120711         clear  wTB09cmd;
055700120703
055800120703         if  dsp_aid <> c_F03  and
055900120703             dsp_aid <> c_F13;
056000120703           clear  SavS01csr;
056100120703           clear  SavS1Copz;
056200120703           clear  SavS01cs1;
056300120703           clear  SavS1Cop1;
056400120703           clear  SavS01cs2;
056500120703           //clear  SavS1Cop2;
056600120703         endif;
056700120627
056800120627         select;
056900120627
057000120627           // -?F3=Fine?
057100120627           when  dsp_aid = c_F03;
057200120627             $Fine = *on;
057300120711             TB09fxx = 'C';
057400120627
057500120627           // -?F5=Aggiornamento?
057600120627           WHEN  dsp_aid = c_F05;
057700120627             reset  $InzS01;
057800120627
057900120627           // -?F8=Ordinamento Sfl?
058000120627           WHEN  dsp_aid = c_F08;
058100120702             reset  $ForzaOpz;
058200120628             exsr  sr_OrdS01;
058300120627
058400120627           // -?F10=Inserimento?
058500120627           WHEN  dsp_aid = c_F10;
058600120702             reset  $ForzaOpz;
058700120629             $Video = 'D2';
058800120627             reset  $InzD02;
058900120711             wTB09cmd = 'J';
059000120711
059100120711           // -?F12=Ritorno?
059200120712           //when  dsp_aid = c_F12;
059300120712           //  if  Not $Called;
059400120712           //    $Video  = 'D1';
059500120712           //  else;
059600120712           //    $Fine = *on;
059700120723           //    TB09fxx = 'L';
059800120712           //  endif;
059900120712           when  dsp_aid = c_F12  and  $Called;
060000120712             $Fine = *on;
060100120723             TB09fxx = 'L';
060200120627
060300120627           // -?F13=Ripetizione Opzione?
060400120627           WHEN  dsp_aid = c_F13;
060500120629             exsr  sr_DupOpzS01;
060600120627
060700120627           // -?Invio?
060800120627           OTHER;
060900120702             reset  $ForzaOpz;
061000120628             // -?Gestione opzioni?
061100120628             if  S01nrr > *zero;
061200120628               exsr  sr_OpzS01;
061300120628               if  ErrGenerico;
061400120628                 leavesr;
061500120628               endif;
061600120628             endif;
061700120628             // -?Posizionamento nel subfile?
061800120628             //  ?(da gestire sempre: il subfile si potrebbe esere?
061900120628             //  ?svuotato dopo l'ultimo posizionamento richiesto)?
062000131011             if  (Ord_x_COD     and    C1Ccod <> SavC1Ccod)    OR
062100131011                 (Ord_x_DES     and    C1Cdes <> SavC1Cdes)    OR
062200131011                 (Ord_x_NaRePr  and   (C1Cnar <> SavC1Cnar
062300131011                                 or    C1Crap <> SavC1Crap
062400131011                                 or    C1Cprv <> SavC1Cprv))   OR
062500131011                 (Ord_x_COR     and    C1Ccor <> SavC1Ccor);
062600120628               $InzS01 = *on;
062700120628             endif;
062800120627
062900120627         ENDSL;
063000120627
063100120627       ENDSR;
063200120627
063300120627       //--------------------------------------------------------------
063400120627       //?Inizializzazione subfile S01.                                ?
063500120627       //--------------------------------------------------------------
063600120627       BEGSR  sr_InzS01;
063700120627
063800120627         // -?Spegnimento degli indicatori di errore?
063900120627         %subst(IndDspF : 50) = *off;
064000120627
064100120627         // -?Pulizia subfile?
064200120627         SflDsp_N    = *on;
064300120627         SflDspCtl_N = *on;
064400120628         write  TB09C01;
064500120627         SflDspCtl_N = *off;
064600120627         SflEnd      = *off;
064700120627
064800120627         clear  C1RcdNbr;
064900120627         clear  C1CsrRrn;
065000120627         clear  S01nrr;
065100120710         clear  S01nrrMax;
065200120627         clear  V1Dmsg;
065300120627         ErrGenerico = *off;
065400120628         ErrMessage  = *off;
065500120628         $EoF = *off;
065600120627
065700120629         // -?Caricamento completo (NON per pagina) dei dati nel subfile?
065800120628         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
065900120628         //  ?l'eventuale ordinamento)?
066000120711         if  TB09cod <> *blank;
066100120711           C1Ccod = TB09cod;
066200120711         endif;
066300120629         k_TBLkut = c_Kut;
066400120628         if  wOrdSfl = *zero  and  C1Ccod <> *blank;
066500120628           k_TBLkey = C1Ccod;
066600120628           setll  %kds(k03tabel00)  TABEL;
066700120628         else;
066800120628           clear  k_TBLkey;
066900120628           setll  %kds(k03tabel00 : 2)  TABEL;
067000120628         endif;
067100120711         reade(N)  %kds(k03tabel00 : 2)  TABEL;
067200120628         $EoF = (%eof(TABEL00F));
067300120628         exsr  sr_CaricaS01;
067400120710
067500120710         // -?Memorizzazione Nrr ultimo record caricato?
067600120711         //  ?(servirà per ripristinarlo DOPO la gestione di?
067700120711         //  ?un'opzione, PRIMA di un nuovo ordinamento del subfile)?
067800120710         S01nrrMax = S01nrr;
067900120627
068000120627         // -?Impaginazione subfile?
068100120628         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
068200120627         if S01nrr > *zero;
068300120627           C1RcdNbr  = 1;
068400120627           C1CsrRrn  = 1;
068500120627         else;
068600120628           clear  C1RcdNbr;
068700120627         endif;
068800120628
068900120628         // -?Ordinamento subfile (se premuto F8)?
069000120629         if  S01nrr > *zero  and  wOrdSfl > *zero;
069100120628           exsr  sr_SortSfl;
069200120629         endif;
069300120629
069400120629         // -?Impostazione richiesta di posizionamento nel subfile?
069500120629         Ord_x_COD    = (S01nrr > *zero  and  wOrdSfl = 0);
069600120629         Ord_x_DES    = (S01nrr > *zero  and  wOrdSfl = 1);
069700120629         Ord_x_NaRePr = (S01nrr > *zero  and  wOrdSfl = 2);
069800120629         Ord_x_COR    = (S01nrr > *zero  and  wOrdSfl = 3);
069900120627
070000120629         // -?(Dis)Abilitazione tasti funzionali?
070100120628         exsr  sr_AbilFxxS01;
070200120627
070300120628         // -?Impostazione opzioni a video?
070400120702         reset  $ForzaOpz;
070500120628         clear  $Opz;
070600120628         Select;
070700120627           // -?Opz. "1" = Ricerca e scelta?
070800120628           When  TB09fun = '1';
070900120628             C1Dopz = c_Opz_S;
071000120628             $Opz(01) = *on;
071100120628           // -?Opz. "2" = Modifica,?
071200120628           //       ?"3" = Copia,?
071300120628           //       ?"4" = Annullamento/Ripristino,?
071400120628           //       ?"5" = Visualizzazione.?
071500120628           When  TB09fun = *blank;
071600120628             C1Dopz = c_Opz_G;
071700120628             $Opz(02) = *on;
071800120628             $Opz(03) = *on;
071900120628             $Opz(04) = *on;
072000120628             $Opz(05) = *on;
072100120628         EndSl;
072200120628
072300120628       ENDSR;
072400120628
072500120628       //--------------------------------------------------------------
072600120628       //?Caricamento completo del subfile S01                         ?
072700120628       //--------------------------------------------------------------
072800120628       BEGSR  sr_CaricaS01;
072900120629
073000120629         // -?Memorizzazione posizionamento effettuato?
073100120629         Select;
073200120629           When  dsp_aid = c_F05;
073300120629           When  wOrdSfl = 0;
073400120629             SavC1Ccod = C1Ccod;
073500120629             clear  SavC1Cdes;
073600120629             clear  SavC1Cnar;
073700120629             clear  SavC1Crap;
073800120629             clear  SavC1Cprv;
073900120629             clear  SavC1Ccor;
074000120629           When  wOrdSfl = 1;
074100120629             SavC1Cdes = C1Cdes;
074200120629             clear  SavC1Ccod;
074300120629             clear  SavC1Cnar;
074400120629             clear  SavC1Crap;
074500120629             clear  SavC1Cprv;
074600120629             clear  SavC1Ccor;
074700120629           When  wOrdSfl = 2;
074800120629             SavC1Cnar = C1Cnar;
074900120629             SavC1Crap = C1Crap;
075000120629             SavC1Cprv = C1Cprv;
075100120629             clear  SavC1Ccod;
075200120629             clear  SavC1Cdes;
075300120629             clear  SavC1Ccor;
075400120629           When  wOrdSfl = 3;
075500120629             SavC1Ccor = C1Ccor;
075600120629             clear  SavC1Ccod;
075700120629             clear  SavC1Cdes;
075800120629             clear  SavC1Cnar;
075900120629             clear  SavC1Crap;
076000120629             clear  SavC1Cprv;
076100120629         EndSl;
076200120628
076300120628         clear  S01nrr;
076400120628         SflNxtChg = *off;
076500120628
076600120628         // -?Ciclo di caricamento dati da tab. "CT"?
076700120628         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
076800120628         //  ?l'eventuale ordinamento)?
076900120628         DoW  Not $EoF;
077000120628
077100120628           // -?Selezione singolo record file TABEL00F?
077200120628           exsr  sr_SelezRec;
077300120628           if  $EoF;
077400120628             leave;
077500120628           endif;
077600120628
077700120628           // -?Registrazione singolo record nel subfile?
077800120628           if  $RecOK;
077900120628
078000120628             // ·?Impostazione campi?
078100120702             dsCT = TBLuni;
078200120628             clear  TB09S01;
078300120628             S1Ccod  = TBLkey;
078400120628             S1Cdes  = §CTdes;
078500120628             S1Cnar  = §CTnar;
078600120628             S1Crap  = §CTrap;
078700120628             S1Cprv  = §CTprv;
078800120628             S1Ccor2 = %subst( §CTcor : 1 : %len(S1Ccor2) );
078900120628             S1Ccor1 = %subst( §CTcor : %len(S1Ccor2) + 1
079000120628                                      : %len(S1Ccor1) );
079100120702             S1Cfcr = §CTfcr;
079200120702             //if  §CTfcr = 'R';
079300120702             //  S1Cfcr= 'S';
079400120702             //else;
079500120702             //  S1Cfcr = 'N';
079600120702             //endif;
079700120628             if  §CTuta = 'N';
079800120629               S1Cuta = §CTuta;
079900120628             else;
080000120629               S1Cuta = 'S';
080100120628             endif;
080200120628             if  §CTutf = 'S';
080300120629               S1Cutf = §CTutf;
080400120628             else;
080500120629               S1Cutf = 'N';
080600120628             endif;
080700120705             if  §CTsta = 'N';
080800120705               S1Csta = §CTsta;
080900120705             else;
081000120705               S1Csta = 'S';
081100120705             endif;
081200120628
081300120628             if  TBLflg <> *blank;
081400120628               S1Catb  = 'A';
081500120628             endif;
081600120702
081700120702             // -?Campi hidden?
081800120702             if  S1Ccor1 = '9';
081900120702               S1Hcor = *loval;
082000120702             else;
082100120702               S1Hcor = §CTcor;
082200120702             endif;
082300120629
082400120629             // -?Impostazione attributi di visualizzazione?
082500120629             Ord_x_COD    = (wOrdSfl = 0);
082600120629             Ord_x_DES    = (wOrdSfl = 1);
082700120629             Ord_x_NaRePr = (wOrdSfl = 2);
082800120629             Ord_x_COR    = (wOrdSfl = 3);
082900120628
083000120628             // ·?Scrittura record nel subfile?
083100120628             S01nrr += 1;
083200120628             write  TB09S01;
083300120628
083400120628           EndIf;
083500120628
083600120628           // -?Lettura record successivo?
083700120711           reade(N)  %kds(k03tabel00 : 2)  TABEL;
083800120628           $EoF = (%eof(TABEL00F));
083900120628
084000120628         EndDo;
084100120628
084200120628         // -?Visualizzazione del SFL (se ci sono dati)?
084300120628         SflDsp_N = (S01nrr <= *zero);
084400120628
084500120628         // -?Emissione messaggio di segnalazione raggiungimento limite?
084600120628         if  S01nrr >= c_MaxRec   and   Not %eof(TABEL00F);
084700120628           ErrGenerico = *on;
084800120628           ErrMessage  = *on;
084900120628           V1Dmsg = $Msg(01);
085000120628           leavesr;
085100120628         endif;
085200120628
085300120628         // -?Attivazione del SFLEND?
085400120628         SflEnd = ( $EoF );
085500120628
085600120628         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
085700120628         if  S01nrr > *zero;
085800120628           C1RcdNbr = 1;
085900120628         else;
086000120628           clear  C1RcdNbr;
086100120628         endif;
086200120628
086300120628         C1CsrRrn = C1RcdNbr;
086400120628
086500120628       ENDSR;
086600120628
086700120628       //--------------------------------------------------------------
086800120628       //?Selezione del singolo record letto dalla tabella "3EW".      ?
086900120628       //--------------------------------------------------------------
087000120628       BEGSR  sr_SelezRec;
087100120628
087200120628         reset  $RecOK;
087300120628
087400120628         dsCT = TBLuni;
087500120628
087600120628         select;
087700120628
087800120628           // -?Superato il limite massimo di record registrabili nel?
087900120628           //  ?subfile?
088000120628           when  S01nrr >= c_MaxRec;
088100120628             $EoF = *on;
088200120628             leavesr;
088300120711
088400120711           // -?Ricevuto singolo codice tassazione?
088500120711           when  TB09cod <> *blank  and  TBLkey > TB09cod;
088600120711             $EoF = *on;
088700120711             leavesr;
088800120711
088900120711           // -?Parzializzazioni inserite o ricevute?
089000120711           when  (V1Cest = 'I'  and  §CTest = 'E')  or
089100120711                 (V1Cest = 'E'  and  §CTest = *blank);
089200120711             leavesr;
089300120711           when  (V1Cuta = 'S'  and  §CTuta = 'N')  or
089400120711                 (V1Cuta = 'N'  and  §CTuta = *blank);
089500120711             leavesr;
089600120711           when  (V1Cutf = 'S'  and  §CTutf = *blank)  or
089700120711                 (V1Cutf = 'N'  and  §CTutf = 'S');
089800120711             leavesr;
089900120711           when  (V1Cutc = 'S'  and  §CTutc = 'N')  or
090000120711                 (V1Cutc = 'N'  and  §CTutc = 'S');
090100120711             leavesr;
090200120628
090300120628           // -?Richiesto posizionamento per codice tassazione:?
090400120628           //  ?si scartano i record con codice inferiore?
090500131011           when  wOrdSfl = 0  and  TBLkey < C1Ccod;
090600120628             leavesr;
090700120628
090800120628           // -?Richiesto posizionamento per descrizione tassazione:?
090900120628           //  ?si scartano i record con descrizione inferiore?
091000131011           when  wOrdSfl = 1  and  §CTdes < C1Cdes;
091100120628             leavesr;
091200120628
091300120628           // -?Richiesto posizionamento per Nazione/Regione/Provincia:?
091400120628           //  ?si scartano i record con Nazione/Regione/Provincia inferiore?
091500120629           when  wOrdSfl = 2  and  ( §CTnar + §CTrap + §CTprv ) <
091600131011                                   ( C1Cnar + C1Crap + C1Cprv );
091700120628             leavesr;
091800120628
091900120628           // -?Richiesto posizionamento per codice ordinamento:?
092000120628           //  ?si scartano i record con codice ordinamento inferiore?
092100131011           when  wOrdSfl = 3  and  §CTcor < C1Ccor;
092200120628             leavesr;
092300120628
092400120628         endsl;
092500120628
092600120628         $RecOK = *on;
092700120628
092800120628       ENDSR;
092900120628
093000120628       //--------------------------------------------------------------
093100120628       //?Abilitazione tasti funzionali in C01.                        ?
093200120628       //--------------------------------------------------------------
093300120628       BEGSR  sr_AbilFxxS01;
093400120628
093500120711         // -?F3 = Fine?
093600120711         F3Attivo  = (Not $Called);
093700120711
093800120628         // -?F5 = Refresh?
093900120628         F5Attivo  = *on;
094000120628
094100120628         // -?F8 = Ordinamento x ...?
094200120628         F8Attivo  = (S01nrr > *zero);
094300120628
094400120628         // -?Impostazione F8 nel piede del subfile?
094500120628         select;
094600120628           when  Not F8Attivo;
094700120629             clear  V1ZF08;
094800120628           when  Ord_x_COD;
094900120629             V1ZF08 = 'F8=Ord. x Descriz.  ';
095000120628           when  Ord_x_DES;
095100120629             V1ZF08 = 'F8=Ord. x Naz/Reg/Pr';
095200120628           when  Ord_x_NaRePr;
095300120711             V1ZF08 = 'F8=Ord. x Ord.Stampa';
095400120628           when  Ord_x_COR;
095500120629             V1ZF08 = 'F8=Ord. x Cod.Tassaz';
095600120628         endsl;
095700120628
095800120628         // -?F10 = Inserimento?
095900120628         F10Attivo = (TB09fun <> '1');
096000120712
096100120712         // -?F12 = Ritorno?
096200120712         F12Attivo = ($Called);
096300120628
096400120628         // -?F13 = Ripetizione?
096500120711         F13Attivo = (TB09fun <> '1'  and  S01nrrMax > 1);
096600120628
096700120628       ENDSR;
096800120628
096900120628       //--------------------------------------------------------------
097000120628       //?Gestione tasto funzionale F8 da videata C01 / S01            ?
097100120628       //?F8=Ordinamento subfile                                       ?
097200120628       //--------------------------------------------------------------
097300120628       BEGSR  sr_OrdS01;
097400120628
097500120629         // -?Subfile vuoto: nessun record da ordinare?
097600120711         if  S01nrrMax = *zero;
097700120629           leavesr;
097800120629         endif;
097900120711
098000120711         // -?REimpostazione del totale Nrr del subfile?
098100120711         //  ?(potrebbe essere qullo del rec. con l'ultima opzione?
098200120711         //  ?elaborata)?
098300120711         S01nrr = S01nrrMax;
098400120629
098500120629         // -?Comunque: bisogna riordinarlo?
098600120628         if  wOrdSfl < 3;
098700120628           wOrdSfl += 1;
098800120628         else;
098900120628           clear  wOrdSfl;
099000120628         endif;
099100120629
099200120629         // -?Impostazione richiesta di posizionamento nel subfile?
099300120629         Ord_x_COD    = (wOrdSfl = 0);
099400120629         Ord_x_DES    = (wOrdSfl = 1);
099500120629         Ord_x_NaRePr = (wOrdSfl = 2);
099600120629         Ord_x_COR    = (wOrdSfl = 3);
099700120628
099800120629         // -?Pulizia campi di posizionamento?
099900131011         select;
100000131011           when  wOrdSfl = 0;
100100131011             clear  C1Ccod;
100200131011           when  wOrdSfl = 1;
100300131011             clear  C1Cdes;
100400131011           when  wOrdSfl = 2;
100500131011             clear  C1Cnar;
100600131011             clear  C1Crap;
100700131011             clear  C1Cprv;
100800131011           when  wOrdSfl = 3;
100900131011             clear  C1Ccor;
101000131011         endsl;
101100120629
101200120629         // -?Subfile già posizionato: occorre ricaricarlo?
101300120629         if  SavC1Ccod <> *blank  or  SavC1Cdes <> *blank  or
101400120629             ( SavC1Cnar + SavC1Crap + SavC1Cprv ) <> *blank  or
101500120629             SavC1Ccor <> *blank;
101600120629           $InzS01 = *on;
101700120629         else;
101800120629           exsr  sr_SortSfl;
101900120629           exsr  sr_AbilFxxS01;
102000120629         endif;
102100120628
102200120628       ENDSR;
102300120628
102400120628       //--------------------------------------------------------------
102500120628       //?Ordinamento subfile S01.                                     ?
102600120628       //?» This subroutine sorts the subfile records. «               ?
102700120628       //--------------------------------------------------------------
102800120628       BEGSR  sr_SortSfl;
102900120628
103000120628         RrnLast  = S01nrr;
103100120628
103200120628         C1RcdNbr = 1;
103300120628         clear  C1CsrRrn;
103400120628         clear  S01nrr;
103500120628         clear  V1Dmsg;
103600120628         ErrGenerico = *off;
103700120628         ErrMessage  = *off;
103800120628         SflNxtChg   = *off;
103900120628         %subst(IndDspF : 50) = *off;
104000120628
104100120628         //?___________________________________________________________?
104200120628         //?Initialize the key fields to sort on.?
104300120628         //?There is one extra field not in the subfile -- Selected --?
104400120628         //?that is added to the record. This field is used to place?
104500120628         //?selected records in front of nonselected records.?
104600120628         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
104700120628
104800120628         clear  QLGKL;
104900120628         clear  QLGSKL;
105000120628         clear  QLGSCB;
105100120628         clear  QLGSCB00;
105200120628
105300120628         // -?Gestione della scelta ordinamento:?
105400120628         Select;
105500120628
105600120628           // ·?Ordinamento per Codice Tassazione?
105700120628           When  Ord_x_COD;
105800120628             // ·?n° campi chiave?
105900120628             QLGNBRK  = 1;
106000120628             // ·?1° campo: codice tassazione?
106100120628             QLGSP    = 1;
106200120628             QLGSS    = %size(S1Ccod);
106300120628             QLGDT    = Carattere;
106400120628             QLGSO    = Ascendente;
106500120628             QLGKL(1) = QLGSKL;
106600120628
106700120628           // ·?Ordinamento per Descrizione Tassazione?
106800120628           When  Ord_x_DES;
106900120628             // ·?n° campi chiave?
107000120628             QLGNBRK  = 1;
107100120628             // ·?1° campo: descrizione tassazione?
107200120628             QLGSP    = %size(S1Ccod) + 1;
107300120628             QLGSS    = %size(S1Cdes);
107400120628             QLGDT    = Carattere;
107500120628             QLGSO    = Ascendente;
107600120628             QLGKL(1) = QLGSKL;
107700120628
107800120628           // ·?Ordinamento per Nazione/Regione/Provincia?
107900120628           When  Ord_x_NaRePr;
108000120628             // ·?n° campi chiave?
108100120629             QLGNBRK  = 4;
108200120628             // ·?1° campo: nazione?
108300120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + 1;
108400120628             QLGSS    = %size(S1Cnar);
108500120628             QLGDT    = Carattere;
108600120628             QLGSO    = Ascendente;
108700120628             QLGKL(1) = QLGSKL;
108800120628             // ·?2° campo: regione?
108900120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
109000120628                        1;
109100120628             QLGSS    = %size(S1Crap);
109200120628             QLGDT    = Carattere;
109300120628             QLGSO    = Ascendente;
109400120628             QLGKL(2) = QLGSKL;
109500120628             // ·?3° campo: provincia?
109600120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
109700120628                        %size(S1Crap) + 1;
109800120628             QLGSS    = %size(S1Cprv);
109900120628             QLGDT    = Carattere;
110000120628             QLGSO    = Ascendente;
110100120628             QLGKL(3) = QLGSKL;
110200120628             // ·?4° campo: codice ordinamento?
110300120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
110400120628                        %size(S1Crap) + %size(S1Cprv) + 1;
110500120702             QLGSS    = %size(S1Ccor2) + %size(S1Hcor);
110600120628             QLGDT    = Carattere;
110700120628             QLGSO    = Ascendente;
110800120629             QLGKL(4) = QLGSKL;
110900120628
111000120629           // ·?Ordinamento per Codice Ordinamento?
111100120629           When  Ord_x_COR;
111200120629             // ·?n° campi chiave?
111300120629             QLGNBRK  = 2;
111400120629             // ·?1° campo: codice ordinamento (1°: 2 bytes)?
111500120629             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
111600120629                        %size(S1Crap) + %size(S1Cprv) + 1;
111700120629             QLGSS    = %size(S1Ccor2);
111800120629             QLGDT    = Carattere;
111900120629             QLGSO    = Ascendente;
112000120629             QLGKL(1) = QLGSKL;
112100120629             // ·?2° campo: codice ordinamento (2°: 1 byte)?
112200120629             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
112300120702                        %size(S1Crap) + %size(S1Cprv) + %size(S1Ccor2) +
112400120629                        1;
112500120702             QLGSS    = %size(S1Hcor);
112600120629             QLGDT    = Carattere;
112700120629             QLGSO    = Ascendente;
112800120629             QLGKL(2) = QLGSKL;
112900120628
113000120628         EndSl;
113100120628
113200120628         // -?Load other sort parameters.?
113300120628         QLGLB   = 80 + 16 * MaxKey;
113400120628         QLGRL   = %size(SflRcd) - 1;
113500120628         QLGRT   = 8;
113600120628         QLGOKL  = 80;
113700120628         QLGLKE  = 16;
113800120628         QLGLSS  = 290;
113900120628         // -?Initialize Sort I/O API fields.?
114000120628         QLGRL00 = QLGRL;
114100120628         QLGRC00 = 1;
114200120628         clear  QUSEI;
114300120628         QUSBPRV = %size(QUSEC);
114400120628
114500120628         // -?First step - Initialize the sort routine.?
114600120628         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
114700120628                      SizeList : ReturnSize : QUSEC );
114800120628
114900120628         // -?Next step - Write records to I/O routine.?
115000120628         QLGRT00  = Put;
115100120628         For  Nrr = 1  To  RrnLast;
115200120628           chain  Nrr  TB09S01;
115300120628           // -?Solo le righe con Selected = 'Y' sono riordinate,?
115400120628           //  ?quindi per fare un ordinamento di tutte le righe?
115500120628           //  ?metto 'Y' sempre.?
115600120628           Selected = 'Y';
115700120628           clear  QUSEI;
115800120628           QUSBPRV  = %size(QUSEC);
115900120628           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
116000120628                         SizeList : NotUsed : QUSEC );
116100120628         EndFor;
116200120628
116300120628         // -?Next step - Signal end of input, clear subfile for reload.?
116400120628         QLGRT00 = EndPut;
116500120628         clear  QUSEI;
116600120628         QUSBPRV = %size(QUSEC);
116700120628         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
116800120628                       SizeList : NotUsed : QUSEC );
116900120628
117000120628         // -?Pulizia subfile?
117100120628         SflDsp_N    = *on;
117200120628         SflDspCtl_N = *on;
117300120628         write  TB09C01;
117400120628         SflDspCtl_N = *off;
117500120628         SflEnd      = *off;
117600120628
117700120628         // -?Final step - Write the records back to the subfile.?
117800120628         QLGRT00  = Get;
117900120628         For  Nrr = 1  To RrnLast;
118000120628           clear  QUSEI;
118100120628           QUSBPRV = %size(QUSEC);
118200120628           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
118300120628                         QLGRL00  : NotUsed : QUSEC );
118400120629           // -?Ripristino indicatori utilizzati nel sfl rec?
118500120629           SflNxtChg = (S1Copz <> *zero);
118600120629           Ord_x_COD    = (wOrdSfl = 0);
118700120629           Ord_x_DES    = (wOrdSfl = 1);
118800120629           Ord_x_NaRePr = (wOrdSfl = 2);
118900120629           Ord_x_COR    = (wOrdSfl = 3);
119000120629           // -?Scrivo record nel subfile?
119100120628           S01nrr = Nrr;
119200120628           write  TB09S01;
119300120628         EndFor;
119400120628
119500120628         C1CsrRrn = 1;
119600120628
119700120628         // -?Visualizzazione del SFL (se ci sono dati)?
119800120628         SflDsp_N = (S01nrr <= *zero);
119900120628
120000120628         // -?Attivazione del SFLEND?
120100120628         SflEnd = $EoF;
120200120628
120300120628       ENDSR;
120400120628
120500120628       //--------------------------------------------------------------
120600120628       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
120700120628       //?F13=Ripetizione opzione                                      ?
120800120628       //--------------------------------------------------------------
120900120628       BEGSR  sr_DupOpzS01;
121000120702
121100120703         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
121200120703         //  ?dalla posizione del cursore?
121300120702         wPag = C1CsrRrn / c_SflPag;
121400120702         wRig = %rem( C1CsrRrn : c_SflPag);
121500120703         if  wRig > *zero;
121600120703           wRig = (wPag * c_SflPag) + 1;
121700120703         else;                           // -?ultimo rec. della pag.?
121800120703           wRig = (wPag * c_SflPag) - c_SflPag + 1;
121900120702         endif;
122000120703         SavS01csr = wRig;
122100120628
122200120702         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
122300120702
122400120702         // -?Reperimento della 1ª opzione inserita?
122500120702         //  ?nella pagina dov'è il cursore?
122600120702         readc  TB09S01;
122700120702         DoW  Not %eof(TRTB09D)  and  S1Copz <> *zero  and
122800120702              S01nrr < SavS01csr;
122900120702           SflNxtChg = *on;
123000120702           update  TB09S01;
123100120702           readc  TB09S01;
123200120702         EndDo;
123300120702         if  Not %eof(TRTB09D)  and  S1Copz <> *zero;
123400120702           SflNxtChg = *on;
123500120702           update  TB09S01;
123600120702         endif;
123700120702
123800120703         // -?Nessuna opzione nella pagina?
123900120703         if  %eof(TRTB09D)         or
124000120703             S01nrr <  SavS01csr   or
124100120702             S01nrr > (SavS01csr + c_SflPag - 1);
124200120702           C1RcdNbr  = SavS01csr;
124300120702           C1CsrRrn  = SavS01csr;
124400120703           reset  $ForzaOpz;
124500120703           reset  SavS1Copz;
124600120703           clear  SavS01cs1;
124700120703           clear  SavS01cs2;
124800120703           clear  SavS1Cop1;
124900120703           //clear  SavS1Cop2;
125000120702           ErrGenerico = *on;
125100120702           ErrMessage  = *on;
125200120702           PosCurOPZ   = *on;
125300120702           V1Dmsg = $Msg(02);
125400120702           leavesr;
125500120702         endif;
125600120702
125700120703         // -?Memorizzazione 1ª opzione nella pagina?
125800120702         SavS01csr = S01nrr;
125900120702         SavS1Copz = S1Copz;
126000120703         if  SavS01csr <> SavS01cs1  or
126100120703             SavS1Copz <> SavS1Cop1;
126200120703           clear  $ForzaOpz;
126300120703         endif;
126400120703         SavS01cs1 = S01nrr;
126500120703         SavS1Cop1 = S1Copz;
126600120702
126700120702         // -?Reperimento delle altre eventuali opzioni?
126800120702         readc  TB09S01;
126900120703         DOW  Not %eof(TRTB09D);
127000120703
127100120703           // ·?Aggiornamento subfile?
127200120703           if  S1Copz <> *zero;
127300120703             SflNxtChg = *on;
127400120703             update  TB09S01;
127500120703           endif;
127600120703
127700120703           // ·?Reperita una 2ª opzione?
127800120703           Select;
127900120703
128000120703             // ·?Vuota!?
128100120703             When  S1Copz = *zero;
128200120703
128300120703             // ·?1ª opzione nella pagina del cursore  e?
128400120703             //  ?2ª opzione (diversa) in una pagina successiva?
128500120703             //  ?(messaggio di avviso di "ricopertura" - se la 2ª?
128600120703             //  ?opzione è diversa)?
128700120703             When  (SavS01csr >= wRig              and
128800120703                    SavS01csr <  wRig + c_SflPag   and
128900120703                    S01nrr    >= wRig + c_SflPag)  and
129000120703                    S1Copz    <> SavS1Copz         and
129100120703                    Not $ForzaOpz;
129200120703               $ForzaOpz = *on;
129300120703               SavS01cs2 = S01nrr;
129400120703               //SavS1Cop2 = S1Copz;
129500120703               C1RcdNbr  = SavS01csr;
129600120703               C1CsrRrn  = SavS01csr;
129700120703               ErrGenerico = *on;
129800120703               ErrMessage  = *on;
129900120703               PosCurOPZ   = *on;
130000120703               V1Dmsg = $Msg(03);
130100120703               leavesr;
130200120703
130300120703             // ·?2ª opzione sul record dove è posizionato il cursore?
130400120703             //  ?(la nuova da considerare per la duplicazione)?
130500120703             When  C1CsrRrn = S01nrr;
130600120703               SavS01csr = S01nrr;
130700120703               SavS1Copz = S1Copz;
130800120703
130900120703             // ·?Più opzioni nella stessa videata e?
131000120703             //  ?cursore su nessuna delle opzioni?
131100120703             //  ?(riposizionare il cursore)?
131200120703             //  ?Se stessa opzione in videate diverse: vince la?
131300120703             //  ?posizione della prima opzione.?
131400120703             When  C1CsrRrn <> SavS01csr  and
131500120703                   C1CsrRrn <> S01nrr     and
131600120703                   S01nrr   <  wRig + c_SflPag;
131700120703               SavS01cs2 = S01nrr;
131800120703               //SavS1Cop2 = S1Copz;
131900120703               C1RcdNbr  = SavS01csr;
132000120703               C1CsrRrn  = SavS01csr;
132100120703               ErrGenerico = *on;
132200120703               ErrMessage  = *on;
132300120703               PosCurOPZ   = *on;
132400120703               V1Dmsg = $Msg(04);
132500120703               leavesr;
132600120703
132700120703           EndSl;
132800120703
132900120703           readc  TB09S01;
133000120703
133100120703         ENDDO;
133200120703
133300120703         // ·?Opzione NON abilitata?
133400120703         if  SavS1Copz  > %elem($Opz)  or
133500120703            (SavS1Copz <= %elem($Opz)  and  $Opz(SavS1Copz) = *off);
133600120703           C1RcdNbr  = SavS01csr;
133700120703           C1CsrRrn  = SavS01csr;
133800120703           ErrGenerico = *on;
133900120703           ErrMessage  = *on;
134000120703           PosCurOPZ   = *on;
134100120703           V1Dmsg = $Msg(05);
134200120703           leavesr;
134300120703         endif;
134400120628
134500120628         // -?Ripetizione opzione sul resto del subfile?
134600120628         S01nrr = SavS01csr + 1;
134700120628         chain  S01nrr  TB09S01;
134800120628         DoW  %found(TRTB09D);
134900120628           SflNxtChg = *on;
135000120628           S1Copz = SavS1Copz;
135100120628           update  TB09S01;
135200120628           S01nrr += 1;
135300120628           chain  S01nrr  TB09S01;
135400120628         EndDo;
135500120628
135600120628       ENDSR;
135700120628
135800120628       //--------------------------------------------------------------
135900120628       //?Gestione opzioni del subfile S01                             ?
136000120628       //--------------------------------------------------------------
136100120628       BEGSR  sr_OpzS01;
136200120628
136300120628         clear  SavS01csr;
136400120628
136500120628         // -?Ciclo di lettura subfile?
136600120628         readc  TB09S01;
136700120628
136800120628         DoW  Not %eof(TRTB09D);
136900120628
137000120628           SflNxtChg = *off;
137100120628           %subst(IndDspF : 50) = *off;
137200120628           C1RcdNbr = S01nrr;
137300120711           //clear  wTB09opz;
137400120628
137500120628           Select;
137600120628
137700120628             // -?Nessuna opzione?
137800120629             When  S1Copz = *zero;
137900120628
138000120628             // -?Opzione NON abilitata?
138100120628             When  S1Copz  > %elem($Opz)  or
138200120628                  (S1Copz <= %elem($Opz)  and  $Opz(S1Copz) = *off);
138300120628               ErrGenerico = *on;
138400120628               ErrMessage  = *on;
138500120628               PosCurOPZ   = *on;
138600120703               V1Dmsg = $Msg(05);
138700120628
138800120628             // -?Opz. 1 = Selezione.?
138900120628             When  S1Copz = 1  and  Not $Fine;
139000120628               $Fine = *on;
139100120628               TB09cod = S1Ccod;
139200120628               TB09des = S1Cdes;
139300120711               //wTB09opz = S1Copz;
139400120628
139500120628             // -?Opz. 1 = Selezione già reperita?
139600120628             //  ?(ammessa una sola selezione)?
139700120628             when  S1Copz = 1  and  $Fine;
139800120628               ErrGenerico = *on;
139900120628               ErrMessage  = *on;
140000120628               PosCurOPZ   = *on;
140100120703               V1Dmsg = $Msg(06);
140200120628               $Fine  = *off;
140300120628
140400120628             // -?Opz. 2 = Modifica,?
140500120628             //       ?3 = Copia,?
140600120628             //       ?4 = Annullamento / Ripristino,?
140700120628             //       ?5 = Visualizzazione.?
140800120628             When  S1Copz = 2  or
140900120628                   S1Copz = 3  or
141000120628                   S1Copz = 4  or
141100120628                   S1Copz = 5;
141200120628               $Video  = 'D2';
141300120628               $InzD02 = *on;
141400120711               //wTB09opz = S1Copz;
141500120629               DoW  $Video = 'D2';
141600120628                 exsr  sr_GesD02;
141700120629                 if  $Fine = *on;
141800120629                   ErrGenerico = *on;
141900120629                   leavesr;
142000120629                 endif;
142100120628               EndDo;
142200120629               clear  V1Topz;
142300120628               // -?Se F12=Ritorno: esce dalla lettura del subfile?
142400120723               if  TB09fxx = 'L';
142500120628                 ErrGenerico = *on;
142600120628               endif;
142700120628
142800120628             // -?Opzione errata?
142900120628             Other;
143000120628               ErrGenerico = *on;
143100120628               ErrMessage  = *on;
143200120628               PosCurOPZ   = *on;
143300120703               V1Dmsg = $Msg(05);
143400120628
143500120628           EndSl;
143600120628
143700120628           // -?Memorizzazione nrr del subfile con la 1ª opzione per?
143800120628           //  ?riposizionarvicisi il cursore dopo il tasto "Invio"?
143900120628           if  S1Copz  <> *zero   and
144000120628               (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
144100120628             SavS01csr = C1RcdNbr;
144200120628           endif;
144300120628
144400120628           // -?Aggiornamento subfile?
144500120628           select;
144600120628             when  ErrMessage;
144700120628               SflNxtChg = *on;
144800120628               C1CsrRrn  = C1RcdNbr;
144900120628             when  ErrGenerico;
145000120628               C1CsrRrn  = C1RcdNbr;
145100120628               clear  S1Copz;
145200120628             other;
145300120628               clear  S1Copz;
145400120628           endsl;
145500120628
145600120628           UPDATE  TB09S01;
145700120628
145800120628           if  ErrGenerico   or   ErrMessage;
145900120628             leave;
146000120628           endif;
146100120628
146200120628           readc  TB09S01;
146300120628
146400120628         ENDDO;
146500120628
146600120628         // -?Riposiziono il cursore sul record contenente la prima opz.?
146700120628         //  ?(se non sono stati rilevati errori)?
146800120628         if  NOT ErrMessage   and   NOT ErrGenerico   and
146900120628             SavS01csr > *zero;
147000120628           C1CsrRrn = SavS01csr;
147100120628         endif;
147200120628
147300120628       ENDSR;
147400120628
147500120628       //--------------------------------------------------------------
147600120628       //?Gestione videata D02.                                        ?
147700120628       //--------------------------------------------------------------
147800120628       BEGSR  sr_GesD02;
147900120628
148000120628         // -?Inizializzazione videata?
148100120628         if  $InzD02 = *on;
148200120628           exsr  sr_InzD02;
148300120628           $InzD02 = *off;
148400120628         endif;
148500120628
148600120628         // -?Impostazione attributi di visualizzazione?
148700120628         exsr  sr_AttrVisD02;
148800120628
148900120628         // -?(Ri)Abilitazione tasti funzionali?
149000120628         exsr  sr_AbilFxxD02;
149100120628
149200120628         // -?Emissione Testata?
149300120628         write  TB09T01;
149400120628
149500120628         // -?Emissione videata (e piede)?
149600120711         //if  (wTB09opz = 4  and  TBLflg = *blank)  or
149700120711         //     wTB09opz = 5;
149800120711         if  (S1Copz = 4  and  TBLflg = *blank)  or
149900120711              S1Copz = 5;
150000120628           write  TB09D02;
150100120628           exfmt  Protect;
150200120628         else;
150300120628           exfmt  TB09D02;
150400120628         endif;
150500120628
150600120628         reset  ErrGenerico;
150700120628         reset  ErrMessage;
150800120628         clear  V1Dmsg;
150900120711
151000120711         clear  TB09fxx;
151100120628
151200120628         select;
151300120628
151400120628           // -?F3 = Fine?
151500120628           when  dsp_aid = c_F03;
151600120711             unlock  TABEL00F;
151700120628             $Fine = *on;
151800120711             TB09fxx = 'C';
151900120628
152000120628           // -?F8 = Successivo?
152100120628           WHEN  dsp_aid = c_F08;
152200120629             $Video = 'S1';
152300120628
152400120628           // -?F9 = Traduzione?
152500120628           WHEN  dsp_aid = c_F09;
152600120629             $Video  = 'S2';
152700120628             $InzS02 = *on;
152800120702             $F09D02 = *on;
152900120629             DoW  $Video = 'S2';
153000120628               exsr  sr_GesS02;
153100120628             EndDo;
153200120628
153300120628           // -?F12 = Ritorno?
153400120628           WHEN  dsp_aid = c_F12;
153500120711             unlock  TABEL00F;
153600120723             $Video  = 'S1';
153700120711             TB09fxx = 'L';
153800120628
153900120629           // -?Invio, F6 = Conferma?
154000120628           OTHER;
154100120628             // -?Controlli eseguiti SE NON richiesto annullamento?
154200120629             if  S1Copz <> 4;
154300120628               exsr  sr_CtrD02;
154400120628               if  ErrGenerico;
154500120628                 leavesr;
154600120628               endif;
154700120628             endif;
154800120629
154900120629             // -?F6 = Aggiornamento?
155000120629             IF  dsp_aid = c_F06;
155100120629
155200120629               // -?Richiesta dati di trasmissione?
155300120629               $Video  = 'W1';
155400120629               $InzW01 = *on;
155500120629               DoW  $Video = 'W1';
155600120629                 exsr  sr_GesW01;
155700120629               EndDo;
155800120629
155900120629             ENDIF;
156000120628
156100120628         ENDSL;
156200120628
156300120628       ENDSR;
156400120628
156500120628       //--------------------------------------------------------------
156600120628       //?Inizializzazione videata D02.                                ?
156700120628       //--------------------------------------------------------------
156800120628       BEGSR  sr_InzD02;
156900120629
157000120629         clear  TB09D02;
157100120628
157200120628         // -?Reperimento dati tabella (SE NON inserimento)?
157300120629         clear  dsCT;
157400120711         if  wTB09cmd <> 'J';
157500120702           k_TBLkut = c_Kut;
157600120628           k_TBLkey = S1Ccod;
157700120712           if  S1Copz = 5;
157800120712             chain(N)  %kds(k03tabel00)  TABEL;
157900120712           else;
158000120712             chain  %kds(k03tabel00)  TABEL;
158100120712           endif;
158200120629           if  %found(TABEL00F);
158300120629             dsCT = TBLuni;
158400120629           endif;
158500120628         endif;
158600120628
158700120628         // -?Impostazione testata?
158800120628         clear  V1Topz;
158900120628         select;
159000120711           when  wTB09cmd = 'J';
159100120628             //V1Topz = '  IMMISSIONE   ';
159200120628             V1Topz = '  INSERIMENTO  ';
159300120629             leavesr;
159400120711           //when  wTB09opz = 2;
159500120711           when  S1Copz = 2;
159600120628             V1Topz = '   MODIFICA    ';
159700120711           //when  wTB09opz = 3;
159800120711           when  S1Copz = 3;
159900120628             V1Topz = '     COPIA     ';
160000120711           //when  wTB09opz = 4  and  TBLflg = *blank;
160100120711           when  S1Copz = 4  and  TBLflg = *blank;
160200120628             V1Topz = ' ANNULLAMENTO  ';
160300120711           //when  wTB09opz = 4  and  TBLflg = '*';
160400120711           when  S1Copz = 4  and  TBLflg = '*';
160500120628             V1Topz = '  RIPRISTINO   ';
160600120711           //when  wTB09opz = 5;
160700120711           when  S1Copz = 5;
160800120628             V1Topz = 'VISUALIZZAZIONE';
160900120628           other;
161000120628             V1Topz = ' ? ? ? ? ? ? ? ';
161100120628         endsl;
161200120628
161300120628         // -?Impostazione dettaglio?
161400120628         V2Ccod  = TBLkey;
161500120628         V2Cdes  = §CTdes;
161600120628         V2Cprv  = §CTprv;
161700120628         V2Cfcr  = §CTfcr;
161800120628         V2Crap  = §CTrap;
161900120628         V2Ccor2 = %subst( §CTcor : 1 : %len(V2Ccor2) );
162000120628         V2Ccor1 = %subst( §CTcor : %len(V2Ccor2) + 1
162100120628                                  : %len(V2Ccor1) );
162200120705         //V2Cfai  = §CTfai;        // -?TOLTO (non più usato)?
162300120628         V2Cest  = §CTest;
162400120628         V2Cnar  = §CTnar;
162500120628         V2Ccap  = §CTcap;
162600120628         V2Cutc  = §CTutc;
162700120628         V2Crct  = §CTrct;
162800120628         V2C§§5  = §CT§§5;
162900120705         //V2Cfdk  = §CTfdk;        // -?TOLTO (non più usato)?
163000120628         V2Cuta  = §CTuta;
163100120628         V2Cutf  = §CTutf;
163200120628         V2Csta  = §CTsta;
163201170607         V2CislCLR = §CTislCLR;
163202170607
163300120628
163400120628         // -?Decodifica Provincia?
163500120628         clear dsPR;
163600120628         if  §CTprv <> *blank;
163700120628           ds_TNTBE.TBEke1 = §CTprv;
163800120628           if  getTabella ( c_Tabel : 'PR'  : ds_TNTBE.TBEke1 :
163900120628                            *omit   : *omit : *omit :
164000120628                            *omit : *omit :
164100120628                            *omit : *omit : *omit : *omit :
164200120628                            *omit : *omit : *omit : *omit :
164300120628                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
164400120628                          = *zero      AND
164500120628               ds_TNTBE.TBEatb = *blank;
164600120628             dsPR   = ds_TNTBE.TBEuni;
164700120628           else;
164800120628             §PRdes = *all'? ';
164900120628           endif;
165000120628         EndIf;
165100120628         V2Dprv = §PRdes;
165200120628
165300120628         // -?Decodifica Nazione?
165400120705         clear ds15;
165500120628         If  §CTnar <> *blank;
165600120628           ds_TNTBE.TBEke1 = §CTnar;
165700120628           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
165800120628                            *omit   : *omit : *omit :
165900120628                            *omit : *omit :
166000120628                            *omit : *omit : *omit : *omit :
166100120628                            *omit : *omit : *omit : *omit :
166200120628                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
166300120628                          = *zero      AND
166400120628               ds_TNTBE.TBEatb = *blank;
166500120628             ds15   = ds_TNTBE.TBEuni;
166600120628           else;
166700120628             §15des = *all'? ';
166800120628           endif;
166900120628         EndIf;
167000120628         V2Dnar = §15des;
167100120705
167200120705         // -?Decodifica Regione o Nazione di appartenenza?
167300120705         clear dsCT_rap;
167400120705         If  §CTrap <> *blank;
167500120705           ds_TNTBE.TBEke1 = §CTrap;
167600120705           if  getTabella ( c_Tabel : 'CT'  : ds_TNTBE.TBEke1 :
167700120705                            *omit   : *omit : *omit :
167800120705                            *omit : *omit :
167900120705                            *omit : *omit : *omit : *omit :
168000120705                            *omit : *omit : *omit : *omit :
168100120705                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
168200120705                          = *zero      AND
168300120705               ds_TNTBE.TBEatb = *blank;
168400120705             dsCT_rap  = ds_TNTBE.TBEuni;
168500120705           else;
168600120705             rap§CTdes = *all'? ';
168700120705           endif;
168800120705         EndIf;
168900120705         V2Drap = rap§CTdes;
169000120712
169100120712         // -?Decodifica Codice Nazione?
169200120712         //If  V2Cnar <> *blank;
169300120712           ds_TNTBE.TBEke1 = V2Cnar;
169400120712           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
169500120712                            *omit   : *omit : *omit :
169600120712                            *omit : *omit :
169700120712                            *omit : *omit : *omit : *omit :
169800120712                            *omit : *omit : *omit : *omit :
169900120712                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
170000120712                          = *zero      AND
170100120712               ds_TNTBE.TBEatb = *blank;
170200120712             ds15   = ds_TNTBE.TBEuni;
170300120712             V2Dnar = §15des;
170400120712           else;
170500120712             V2Dnar = *all'? ';
170600120712           endif;
170700120712         //EndIf;
170800120628
170900120628       ENDSR;
171000120628
171100120628       //--------------------------------------------------------------
171200120628       //?Impostazione attributi di visualizzazione in D02.            ?
171300120628       //--------------------------------------------------------------
171400120628       BEGSR  sr_AttrVisD02;
171500120628
171600120628         // -?Protezione campo chiave (SE NON immissione o copia)?
171700120711         //Protect_Cod = (wTB09cmd <> 'J'  and  wTB09opz <> 3);
171800120711         Protect_Cod = (wTB09cmd <> 'J'  and  S1Copz <> 3);
171900120628
172000120628         // -?Codice Tassazione annullato in tabella?
172100120628         Tass_Annull = (%found(TABEL00F)  and  TBLflg <> *blank);
172200120628
172300120628       ENDSR;
172400120628
172500120628       //--------------------------------------------------------------
172600120628       //?Abilitazione tasti funzionali in D02.                        ?
172700120628       //--------------------------------------------------------------
172800120628       BEGSR  sr_AbilFxxD02;
172900120628
173000120628         // -?F6 = Conferma?
173100120711         //F6Attivo  = (wTB09opz <> 5);
173200120711         F6Attivo  = (S1Copz <> 5);
173300120628
173400120628         // -?F8 = Successivo?
173500120628         F8Attivo  = *on;
173600120628
173700120628       ENDSR;
173800120628
173900120628       //--------------------------------------------------------------
174000120628       //?Controllo dati nella videata D02.                            ?
174100120628       //--------------------------------------------------------------
174200120628       BEGSR  sr_CtrD02;
174300120628
174400120628         %subst(IndDspF : 50) = *off;
174500120629         k_TBLkut = c_Kut;
174600120629         //k_TBLcod = c_Tab;       ?(già così)?
174700120629         k_TBLkey = V2Ccod;
174800120629
174900120629         // -?Codice Tassazione?
175000120629         Select;
175100120629           // ·?Obbligatorio?
175200120629           When  V2Ccod = *blank;
175300120629             ErrGenerico = *on;
175400120629             ErrMessage  = *on;
175500120629             PosCurCOD   = *on;
175600120703             V1Dmsg = $Msg(07);
175700120629             leavesr;
175800120629           // ·?SE immissione o copia: controllo inesistenza nuovo?
175900120711           //When  wTB09cmd = 'J'  or  wTB09opz = 3;
176000120711           When  wTB09cmd = 'J'  or  S1Copz = 3;
176100120629             setll  %kds(k03tabel00)  TABEL;
176200120629             if  %equal(TABEL00F);
176300120629               ErrGenerico = *on;
176400120629               ErrMessage  = *on;
176500120629               PosCurCOD   = *on;
176600120703               V1Dmsg = $Msg(08);
176700120629               leavesr;
176800120629             endif;
176900120629         EndSl;
177000120629
177100120629         // -?Descrizione obbligatoria?
177200120629         if  V2Cdes = *blank;
177300120629           ErrGenerico = *on;
177400120629           ErrMessage  = *on;
177500120629           PosCurDES   = *on;
177600120703           V1Dmsg = $Msg(09);
177700120629           leavesr;
177800120629         endif;
177900120629
178000120629         // -?Provincia: ricerca?
178100120629         If  %scan( '?' : V2Cprv ) > *zero;
178200120629           TABEL_Ricerca( 'PR' :
178300120629                          *blank :
178400120629                          idElemento :
178500120629                          tastoFunzionaleUscita );
178600120629           if  tastoFunzionaleUscita = *blank;
178700120629             V2Cprv = idElemento;
178800120629           endif;
178900120629           PosCurPRV = *on;
179000120629         EndIf;
179100120629         // -?Provincia: controllo?
179200120629         clear  V2Dprv;
179300120629         If  V2Cprv <> *blank;
179400120629           ds_TNTBE.TBEke1 = V2Cprv;
179500120629           if  getTabella ( c_Tabel : 'PR'  : ds_TNTBE.TBEke1 :
179600120629                            *omit   : *omit : *omit :
179700120629                            *omit : *omit :
179800120629                            *omit : *omit : *omit : *omit :
179900120629                            *omit : *omit : *omit : *omit :
180000120629                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
180100120629                          = *zero      AND
180200120629               ds_TNTBE.TBEatb = *blank;
180300120629             dsPR   = ds_TNTBE.TBEuni;
180400120629             V2Dprv = §PRdes;
180500120629           else;
180600120629             V2Dprv = *all'? ';
180700120629             ErrGenerico = *on;
180800120629             ErrMessage  = *on;
180900120629             PosCurPRV   = *on;
181000120703             V1Dmsg = $Msg(10);
181100120629             leavesr;
181200120629           endif;
181300120629         EndIf;
181400120629
181500120705         // -?Sigla Regione/Nazione di appartenenza: controllo?
181600120705         clear  V2Drap;
181700120705         Select;
181800120705           When  V2Crap = *blank;
181900120711           //When  V2Crap = V2Ccod  and  (wTB09cmd = 'J'
182000120711           //                        or   wTB09opz  = 3);
182100120705           When  V2Crap = V2Ccod;
182200120705             V2Drap = V2Cdes;
182300120705           Other;
182400120705             ds_TNTBE.TBEke1 = V2Crap;
182500120705             if  getTabella ( c_Tabel : 'CT'  : ds_TNTBE.TBEke1 :
182600120705                              *omit   : *omit : *omit :
182700120705                              *omit : *omit :
182800120705                              *omit : *omit : *omit : *omit :
182900120705                              *omit : *omit : *omit : *omit :
183000120705                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
183100120705                            = *zero      AND
183200120705                 ds_TNTBE.TBEatb = *blank;
183300120705               dsCT_rap = ds_TNTBE.TBEuni;
183400120705               V2Drap   = rap§CTdes;
183500120705             else;
183600120705               V2Drap = *all'? ';
183700120705               ErrGenerico = *on;
183800120705               ErrMessage  = *on;
183900120705               PosCurRAP   = *on;
184000120705               V1Dmsg = $Msg(11);
184100120705               leavesr;
184200120705             endif;
184300120705         EndSl;
184400120705
184500120629         // -?Nazione: ricerca?
184600120629         If  %scan( '?' : V2Cnar ) > *zero;
184700120629           TABEL_Ricerca( '15' :
184800120629                          *blank :
184900120629                          idElemento :
185000120629                          tastoFunzionaleUscita );
185100120629           if  tastoFunzionaleUscita = *blank;
185200120629             V2Cnar = idElemento;
185300120629           endif;
185400120629           PosCurNAR = *on;
185500120629         EndIf;
185600120629         // -?Nazione: controllo?
185700120629         clear  V2Dnar;
185800120712         //If  V2Cnar <> *blank;
185900120629           ds_TNTBE.TBEke1 = V2Cnar;
186000120629           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
186100120629                            *omit   : *omit : *omit :
186200120629                            *omit : *omit :
186300120629                            *omit : *omit : *omit : *omit :
186400120629                            *omit : *omit : *omit : *omit :
186500120629                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
186600120629                          = *zero      AND
186700120629               ds_TNTBE.TBEatb = *blank;
186800120629             ds15   = ds_TNTBE.TBEuni;
186900120629             V2Dnar = §15des;
187000120629           else;
187100120629             V2Dnar = *all'? ';
187200120629             ErrGenerico = *on;
187300120629             ErrMessage  = *on;
187400120629             PosCurNAR   = *on;
187500120705             V1Dmsg = $Msg(12);
187600120629             leavesr;
187700120629           endif;
187800120712         //EndIf;
187900120628
188000120628       ENDSR;
188100120628
188200120628       //--------------------------------------------------------------
188300120628       //?Gestione window W01.                                         ?
188400120628       //--------------------------------------------------------------
188500120628       BEGSR  sr_GesW01;
188600120628
188700120628         // -?Inizializzazione videata?
188800120628         if  $InzW01 = *on;
188900120628           exsr  sr_InzW01;
189000120628           $InzW01 = *off;
189100120628         endif;
189200120628
189300120628         // -?Emissione Window?
189400120628         exfmt  TB09W01;
189500120628
189600120628         reset  ErrGenerico;
189700120628         reset  ErrMessage;
189800120628         clear  V1Dmsg;
189900120628
190000120628         select;
190100120628
190200120628           // -?F12 = Ritorno?
190300120628           WHEN  dsp_aid = c_F12;
190400120723             $Video  = 'D2';
190500120711             TB09fxx = 'L';
190600120628
190700120628           // -?Invio, F6 = Conferma?
190800120628           OTHER;
190900120628             exsr  sr_CtrW01;
191000120628             if  ErrGenerico;
191100120628               leavesr;
191200120628             endif;
191300120628             // -?F6 = Aggiornamento?
191400120628             If  dsp_aid = c_F06;
191500120628               exsr  sr_UpdTABEL;
191600120628               if  ErrGenerico = *on;
191700120628                 leavesr;
191800120628               endif;
191900120629               // ·?Ritorno al subfile?
192000120629               $Video  = 'S1';
192100120629               $InzS01 = *on;
192200120628             EndIf;
192300120628
192400120628         ENDSL;
192500120628
192600120628       ENDSR;
192700120628
192800120628       //--------------------------------------------------------------
192900120628       //?Inizializzazione window W01.                                 ?
193000120628       //--------------------------------------------------------------
193100120628       BEGSR  sr_InzW01;
193200120628
193300120628         clear  TB09W01;
193400120628
193500120628         // -?Se non immissione: imposta i campi del file?
193600120711         If  wTB09cmd <> 'J';
193700120628
193800120628           W01ftt = TBLftt;
193900120628           W01flt = TBLflt;
194000120628           W01ftr = TBLftr;
194100120712           // ·?Conversione data (se NON immissione o copia)?
194200120712           if  wTB09cmd <> 'J'  and  S1Copz <> 3
194300120712                                and  TBLdtr  > *zero;
194400120629             wDate_DMY = %date(TBLdtr : *ymd);
194500120629             W01dtr    = %dec( wDate_DMY );
194600120628           endif;
194700120628
194800120628         EndIf;
194900120628
195000120628       ENDSR;
195100120628
195200120628       //--------------------------------------------------------------
195300120628       //?Controllo dati nella window W01.                             ?
195400120628       //--------------------------------------------------------------
195500120628       BEGSR  sr_CtrW01;
195600120628
195700120628         %subst(IndDspF : 50) = *off;
195800120628
195900120628       ENDSR;
196000120629
196100120629       //--------------------------------------------------------------
196200120629       //?Aggiornamento record nel file TABEL00F.                      ?
196300120629       //--------------------------------------------------------------
196400120629       BEGSR  sr_UpdTABEL;
196500120629
196600120629         // -?RI-aggancio record da aggiornare?
196700120629         k_TBLkut = c_Kut;
196800120629         //k_TBLcod = c_Tab;       ?(già così)?
196900120629         k_TBLkey = V2Ccod;
197000120629
197100120629         chain  %kds(k03tabel00)  TABEL;
197200120629
197300120629         // -?Impostazione campi-chiave SE record NON trovato?
197400120629         if  Not %found(TABEL00F);
197500120629           TBLkut = k_TBLkut;
197600120629           TBLcod = k_TBLcod;
197700120629           TBLkey = k_TBLkey;
197800120629         endif;
197900120629
198000120629
198100120629         // -?Annullamento / Ripristino?
198200120711         //If  wTB09opz = 4;
198300120711         If  S1Copz = 4;
198400120629           select;
198500120629             // ·?Annullamento?
198600120629             when  TBLflg = *blank;
198700120629               TBLflg = '*';
198800120629             // ·?Ripristino?
198900120629             when  TBLflg <> *blank;
199000120629               clear  TBLflg;
199100120629           endsl;
199200120629         EndIf;
199300120629
199400120629
199500120629         // -?Riempimento struttura dati dsCT?
199600120629         clear  dsCT;
199700120629
199800120629         §CTdes = V2Cdes;
199900120629         §CTprv = V2Cprv;
200000120629         §CTfcr = V2Cfcr;
200100120629         §CTrap = V2Crap;
200200120629         //%subst( §CTcor : 1 : 2 ) = V2Ccor2;
200300120629         //%subst( §CTcor : 3 : 1 ) = V2Ccor1;
200400120629         §CTcor = V2Ccor2 + V2Ccor1;
200500120705         //§CTfai = V2Cfai;         // -?TOLTO (non più usato)?
200600120705         //§CTfdk = V2Cfdk;         // -?TOLTO (non più usato)?
200700120702         §CT§§5 = V2C§§5;
200800120629         §CTest = V2Cest;
200900120629         §CTnar = V2Cnar;
201000120629         §CTcap = V2Ccap;
201100120629         §CTutc = V2Cutc;
201200120629         §CTutf = V2Cutf;
201300120629         §CTuta = V2Cuta;
201400120629         §CTsta = V2Csta;
201500120629         §CTrct = V2Crct;
201501170607         §CTislCLR = V2CislCLR;
201502170607
201503170607
201504170607
201600120629
201700120629         TBLuni = dsCT;
201800120629
201900120629         TBLftt = W01ftt;
202000120629         TBLflt = W01flt;
202100120629         clear  TBLftr;
202200120629         clear  TBLdtr;
202300120629
202400120629
202500120629         // -?Aggiornamento / Inserimento?
202600120629         If  %found(TABEL00F);
202700120629           //____________
202800120629           update  TABEL;
202900120629           //¯¯¯¯¯¯¯¯¯¯¯¯
203000120629         Else;
203100120629           //____________
203200120629           write   TABEL;
203300120629           //¯¯¯¯¯¯¯¯¯¯¯¯
203400120629         EndIf;
203500120629
203600120629         // -?Se Opz. 4 = Annullamento / Ripristino?
203700120629         //  ?Vanno aggiornate automaticamente anche le descrizioni?
203800120629         //  ?in lingua?
203900120711         //IF  wTB09opz = 4;
204000120711         IF  S1Copz = 4;
204100120629
204200120629           LINtabel = 1;
204300120629           setgt  LINtabel  AZLIN000;
204400120629           read  AZLIN000;
204500120629
204600120629           DoW  Not %eof(AZLIN01L);
204700120629
204800120629             // ·?Aggiornamento?
204900120629             k_TBLkut = LINtabel;
205000120629             chain  %kds(k03tabel00)  TABEL;
205100120629             if  %found(TABEL00F);
205200120629               select;
205300120629                 when  TBLflg <> *blank;
205400120629                   clear TBLflg;
205500120629                 when  TBLflg =  *blank;
205600120629                   TBLflg = '*';
205700120629               endsl;
205800120629               //____________
205900120629               update  TABEL;
206000120629               //¯¯¯¯¯¯¯¯¯¯¯¯
206100120629             endif;
206200120629
206300120629             read  AZLIN000;
206400120629
206500120629           EndDo;
206600120629
206700120629         // -?Altrimenti (Opz. <> 4):?
206800120629         //  ?Aggiornate delle descrizioni in lingua?
206900120629         ELSE;
207000120629
207100120702           $F09D02 = *off;
207200120629           $Video  = 'S2';
207300120629           $InzS02 = *on;
207400120629
207500120629           DoW  $Video = 'S2';
207600120629             exsr  sr_GesS02;
207700120629           EndDo;
207800120629
207900120629         ENDIF;
208000120629
208100120629       ENDSR;
208200120629
208300120629       //--------------------------------------------------------------
208400120629       //?Gestione subfile S02.                                        ?
208500120629       //--------------------------------------------------------------
208600120629       BEGSR  sr_GesS02;
208700120629
208800120629         // -?Inizializzazione videata?
208900120629         if  $InzS02 = *on;
209000120629           exsr  sr_InzS02;
209100120629           $InzS02 = *off;
209200120629         endif;
209300120629
209400120629         // -?Emissione Window (con Piede)?
209500120629         write  TB09Z02;
209600120629
209700120629         // -?Posizionamento cursore?
209800120629         if  C2RcdNbr <= *zero;
209900120629           C2RcdNbr = 1;
210000120629         endif;
210100120629
210200120629         // -?Emissione videata?
210300120629         exfmt  TB09C02;
210400120629
210500120629         reset  ErrGenerico;
210600120629         reset  ErrMessage;
210700120629         clear  V1Dmsg;
210800120629
210900120629         select;
211000120629
211100120629           // -?F12=Ritorno?
211200120629           when  dsp_aid = c_F12;
211300120629             $Video = 'D2';
211400120629             leavesr;
211500120629
211600120629           // -?Invio / F6 = Conferma?
211700120629           OTHER;
211800120702             If  Not $F09D02;
211900120629               // -?Controlli (SE NON sola visualizzazione)?
212000120629               exsr  sr_CtrS02;
212100120629               if  ErrGenerico;
212200120629                 leavesr;
212300120629               endif;
212400120629               // -?Aggiornamento (SE premuto F6)?
212500120629               if  dsp_aid = c_F06;
212600120629                 exsr  sr_updTABling;
212700120629               endif;
212800120629             EndIf;
212900120629
213000120629         ENDSL;
213100120629
213200120629       ENDSR;
213300120629
213400120629       //--------------------------------------------------------------
213500120629       //?Inizializzazione subfile S02.                                ?
213600120629       //--------------------------------------------------------------
213700120629       BEGSR  sr_InzS02;
213800120629
213900120629         // -?Spegnimento degli indicatori di errore?
214000120629         %subst(IndDspF : 50) = *off;
214100120629
214200120629         // -?Pulizia subfile?
214300120629         wSflDsp_N    = *on;
214400120629         wSflDspCtl_N = *on;
214500120629         write  TB09C02;
214600120629         wSflDspCtl_N = *off;
214700120629         wSflEnd      = *off;
214800120629
214900120629         //wSflNxtChg = *off;
215000120629
215100120629         clear  SavS02nrr;
215200120629         clear  C2RcdNbr;
215300120629         clear  S02nrr;
215400120629         clear  V1Dmsg;
215500120629         ErrGenerico = *off;
215600120629         ErrMessage  = *off;
215700120629
215800120629         // -?Impostazione dati in testata?
215900120629         C2Ccod = V2Ccod;
216000120629         C2Cdes = V2Cdes;
216100120629
216200120629         // -?Caricamento completo dei dati nel subfile?
216300120629         //  ?(per ogni lingua)?
216400120629         setgt  (c_Kut)  AZLIN000;
216500120629         read  AZLIN000;
216600120629
216700120629         DoW  Not %eof(AZLIN01L);
216800120629
216900120629           // ·?Reperimento dati in lingua da tab. "CT"?
217000120629           k_TBLkut = LINtabel;
217100120629           //k_TBLcod = c_Tab;     ?(già così)?
217200120629           k_TBLkey = V2Ccod;
217300120629           chain(N)  %kds(k03tabel00)  TABEL;
217400120629
217500120629           // ·?Impostazione campi?
217600120629           clear  TB09S02;
217700120702           F12Attivo = ($F09D02 = *on);
217800120629           S2HcdLin = LINtabel;
217900120629           S2Dlin   = LINdesIta;
218000120629           if  Not %found(TABEL00F);
218100120629             clear  S2Cdes;
218200120629           else;
218300120629             dsCT = TBLuni;
218400120629             S2Cdes = §CTdes;
218500120629           endif;
218600120629
218700120629           // ·?Scrittura record nel subfile?
218800120629           S02nrr += 1;
218900120629           write  TB09S02;
219000120629
219100120629           // ·?Lettura record di lingua successivo?
219200120629           read  AZLIN000;
219300120629
219400120629         EndDo;
219500120629
219600120629         // -?Salvataggio del n° lingue scritte nel subfile?
219700120629         SavS02nrr = S02nrr;
219800120629
219900120629         // -?Visualizzazione del SFL (se ci sono dati)?
220000120629         wSflDsp_N = (S02nrr <= *zero);
220100120629
220200120629         // -?Attivazione del SFLEND?
220300120629         wSflEnd = ( %eof(AZLIN01L) );
220400120629
220500120629         // -?Impaginazione subfile?
220600120629         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
220700120629         if S02nrr > *zero;
220800120629           C2RcdNbr  = 1;
220900120629         else;
221000120629           clear  C2RcdNbr;
221100120629         endif;
221200120629
221300120629         // -?(Dis)Abilitazione tasti funzionali?
221400120702         F6Attivo  = ($F09D02 = *off);
221500120702         F12Attivo = ($F09D02 = *on);
221600120629
221700120629       ENDSR;
221800120629
221900120629       //--------------------------------------------------------------
222000120629       //?Controllo dati nel subfile S02.                              ?
222100120629       //--------------------------------------------------------------
222200120629       BEGSR  sr_CtrS02;
222300120629
222400120629         // -?Ciclo di controllo per ogni lingua?
222500120629         For  Nrr = 1  To  SavS02nrr;
222600120629
222700120629           chain  Nrr  TB09S02;
222800120629
222900120629           If  %found(TRTB09D);
223000120629
223100120629             %subst(IndDspF : 50) = *off;
223200120629
223300120629             // -?Descrizione (in lingua) obbligatoria?
223400120629             if  S2Cdes = *blank;
223500120629               ErrGenerico = *on;
223600120629               ErrMessage  = *on;
223700120629               PosCurW1des = *on;
223800120703               V1Dmsg = $Msg(09);
223900120629             endif;
224000120629
224100120629             // -?Posizionamento cursore sul rec. in errore?
224200120629             if  ErrGenerico;
224300120629               C2RcdNbr = Nrr;
224400120629               update  TB09S02;
224500120629               leave;
224600120629             endif;
224700120629
224800120629           EndIf;
224900120629
225000120629         EndFor;
225100120629
225200120629       ENDSR;
225300120629
225400120629       //--------------------------------------------------------------
225500120629       //?Aggiornamento record nel file TABEL00F (in lingua).          ?
225600120629       //--------------------------------------------------------------
225700120629       BEGSR  sr_UpdTABling;
225800120629
225900120629         // -?Reperimento dei dati della tabella in Italiano?
226000120629         clear  SavTBLuni;
226100120629         k_TBLkut = c_Kut;
226200120629         k_TBLcod = c_Tab;
226300120629         k_TBLkey = V2Ccod;
226400120629         chain(N)  %kds(k03tabel00)  TABEL;
226500120629         if  %found(TABEL00F);
226600120629           SavTBLuni = TBLuni;
226700120629         endif;
226800120629
226900120629         // -?Ciclo di lettura del subfile  e?
227000120629         //  ?aggiornamento/scrittura dei dati in lingua?
227100120629         For  Nrr = 1  To  SavS02nrr;
227200120629
227300120629           chain  Nrr  TB09S02;
227400120629
227500120629           if  Not %found(TRTB09D);
227600120629             iter;
227700120629           endif;
227800120629
227900120629           // -?Aggancio record da scrivere/aggiornare?
228000120629           k_TBLkut = S2HcdLin;
228100120629           chain  %kds(k03tabel00)  TABEL;
228200120629
228300120629           dsCT   = SavTBLuni;
228400120629           §CTdes = S2Cdes;
228500120629           TBLuni = dsCT;
228600120629           TBLftt = W01ftt;
228700120629           clear  TBLftr;
228800120629           clear  TBLdtr;
228900120629
229000120629           if  NOt %found(TABEL00F);
229100120629             TBLkut = k_TBLkut;
229200120629             TBLcod = c_Tab;
229300120629             TBLkey = V2Ccod;
229400120629             //___________
229500120629             write  TABEL;
229600120629             //¯¯¯¯¯¯¯¯¯¯¯
229700120629           else;
229800120629             //___________
229900120629             update TABEL;
230000120629             //¯¯¯¯¯¯¯¯¯¯¯
230100120629           endif;
230200120629
230300120629         EndFor;
230400120629
230500120629         $Video  = 'S1';
230600120629         $InzS01 = *on;
230700120629
230800120629       ENDSR;
230900120628
231000120628       //--------------------------------------------------------------
231100120628       //?Operazioni finali.                                           ?
231200120628       //--------------------------------------------------------------
231300120628       BEGSR  sr_RoutEnd;
231400120705
231500120705         // -?Chiusura applicazioni utilizzate?
231600120705         cloTabella();
231700120628
231800120628         // -?Restituzione parametri al chiamante?
231900120628         kpjbu = TRTB09ds;
232000120628
232100120628         // -?Uscita dal *pgm?
232200120628         return;
232300120628
232400120628       ENDSR;
232500060510
232600060510      /end-free
232700120627
232800120627       //--------------------------------------------------------------
232900120627       //?Definizione schiere a tempo di compilazione                  ?
233000120627       //--------------------------------------------------------------
233100120627
233200120627** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
233300120628SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)         1
233400120628Immettere un'opzione su questa pagina prima di premere F13                     2
233500120702Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 3
233600120703Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 4
233700120703Opzione errata                                                                 5
233800120703Ammessa la selezione di un solo codice tassazione                              6
233900120703Immettere il codice tassazione                                                 7
234000120703Codice Tassazione già esistente                                                8
234100120703Immettere la descrizione                                                       9
234200120703Provincia errata                                                              10
234300120705Sigla Regione di appartenenza errata                                          11
234400120705Nazione errata                                                                12
