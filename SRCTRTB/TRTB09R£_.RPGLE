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
030000120627     d ReturnSize      s              9b 0 inz
030100120627     d SizeList        s              9b 0 inz
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
062000120629             if  (Ord_x_COD     and    C1Ccod <> *blank
062100120629                                and    C1Ccod <> SavC1Ccod)    OR
062200120629                 (Ord_x_DES     and    C1Cdes <> *blank
062300120629                                and    C1Cdes <> SavC1Cdes)    OR
062400120629                 (Ord_x_NaRePr  and  ((C1Cnar <> *blank
062500120629                                and    C1Cnar <> SavC1Cnar)
062600120629                                 or   (C1Crap <> *blank
062700120629                                and    C1Crap <> SavC1Crap)
062800120629                                 or   (C1Cprv <> *blank
062900120629                                and    C1Cprv <> SavC1Cprv)))  OR
063000120629                 (Ord_x_COR     and    C1Ccor <> *blank
063100120629                                and    C1Ccor <> SavC1Ccor);
063200120628               $InzS01 = *on;
063300120628             endif;
063400120627
063500120627         ENDSL;
063600120627
063700120627       ENDSR;
063800120627
063900120627       //--------------------------------------------------------------
064000120627       //?Inizializzazione subfile S01.                                ?
064100120627       //--------------------------------------------------------------
064200120627       BEGSR  sr_InzS01;
064300120627
064400120627         // -?Spegnimento degli indicatori di errore?
064500120627         %subst(IndDspF : 50) = *off;
064600120627
064700120627         // -?Pulizia subfile?
064800120627         SflDsp_N    = *on;
064900120627         SflDspCtl_N = *on;
065000120628         write  TB09C01;
065100120627         SflDspCtl_N = *off;
065200120627         SflEnd      = *off;
065300120627
065400120627         clear  C1RcdNbr;
065500120627         clear  C1CsrRrn;
065600120627         clear  S01nrr;
065700120710         clear  S01nrrMax;
065800120627         clear  V1Dmsg;
065900120627         ErrGenerico = *off;
066000120628         ErrMessage  = *off;
066100120628         $EoF = *off;
066200120627
066300120629         // -?Caricamento completo (NON per pagina) dei dati nel subfile?
066400120628         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
066500120628         //  ?l'eventuale ordinamento)?
066600120711         if  TB09cod <> *blank;
066700120711           C1Ccod = TB09cod;
066800120711         endif;
066900120629         k_TBLkut = c_Kut;
067000120628         if  wOrdSfl = *zero  and  C1Ccod <> *blank;
067100120628           k_TBLkey = C1Ccod;
067200120628           setll  %kds(k03tabel00)  TABEL;
067300120628         else;
067400120628           clear  k_TBLkey;
067500120628           setll  %kds(k03tabel00 : 2)  TABEL;
067600120628         endif;
067700120711         reade(N)  %kds(k03tabel00 : 2)  TABEL;
067800120628         $EoF = (%eof(TABEL00F));
067900120628         exsr  sr_CaricaS01;
068000120710
068100120710         // -?Memorizzazione Nrr ultimo record caricato?
068200120711         //  ?(servirà per ripristinarlo DOPO la gestione di?
068300120711         //  ?un'opzione, PRIMA di un nuovo ordinamento del subfile)?
068400120710         S01nrrMax = S01nrr;
068500120627
068600120627         // -?Impaginazione subfile?
068700120628         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
068800120627         if S01nrr > *zero;
068900120627           C1RcdNbr  = 1;
069000120627           C1CsrRrn  = 1;
069100120627         else;
069200120628           clear  C1RcdNbr;
069300120627         endif;
069400120628
069500120628         // -?Ordinamento subfile (se premuto F8)?
069600120629         if  S01nrr > *zero  and  wOrdSfl > *zero;
069700120628           exsr  sr_SortSfl;
069800120629         endif;
069900120629
070000120629         // -?Impostazione richiesta di posizionamento nel subfile?
070100120629         Ord_x_COD    = (S01nrr > *zero  and  wOrdSfl = 0);
070200120629         Ord_x_DES    = (S01nrr > *zero  and  wOrdSfl = 1);
070300120629         Ord_x_NaRePr = (S01nrr > *zero  and  wOrdSfl = 2);
070400120629         Ord_x_COR    = (S01nrr > *zero  and  wOrdSfl = 3);
070500120627
070600120629         // -?(Dis)Abilitazione tasti funzionali?
070700120628         exsr  sr_AbilFxxS01;
070800120627
070900120628         // -?Impostazione opzioni a video?
071000120702         reset  $ForzaOpz;
071100120628         clear  $Opz;
071200120628         Select;
071300120627           // -?Opz. "1" = Ricerca e scelta?
071400120628           When  TB09fun = '1';
071500120628             C1Dopz = c_Opz_S;
071600120628             $Opz(01) = *on;
071700120628           // -?Opz. "2" = Modifica,?
071800120628           //       ?"3" = Copia,?
071900120628           //       ?"4" = Annullamento/Ripristino,?
072000120628           //       ?"5" = Visualizzazione.?
072100120628           When  TB09fun = *blank;
072200120628             C1Dopz = c_Opz_G;
072300120628             $Opz(02) = *on;
072400120628             $Opz(03) = *on;
072500120628             $Opz(04) = *on;
072600120628             $Opz(05) = *on;
072700120628         EndSl;
072800120628
072900120628       ENDSR;
073000120628
073100120628       //--------------------------------------------------------------
073200120628       //?Caricamento completo del subfile S01                         ?
073300120628       //--------------------------------------------------------------
073400120628       BEGSR  sr_CaricaS01;
073500120629
073600120629         // -?Memorizzazione posizionamento effettuato?
073700120629         Select;
073800120629           When  dsp_aid = c_F05;
073900120629           When  wOrdSfl = 0;
074000120629             SavC1Ccod = C1Ccod;
074100120629             clear  SavC1Cdes;
074200120629             clear  SavC1Cnar;
074300120629             clear  SavC1Crap;
074400120629             clear  SavC1Cprv;
074500120629             clear  SavC1Ccor;
074600120629           When  wOrdSfl = 1;
074700120629             SavC1Cdes = C1Cdes;
074800120629             clear  SavC1Ccod;
074900120629             clear  SavC1Cnar;
075000120629             clear  SavC1Crap;
075100120629             clear  SavC1Cprv;
075200120629             clear  SavC1Ccor;
075300120629           When  wOrdSfl = 2;
075400120629             SavC1Cnar = C1Cnar;
075500120629             SavC1Crap = C1Crap;
075600120629             SavC1Cprv = C1Cprv;
075700120629             clear  SavC1Ccod;
075800120629             clear  SavC1Cdes;
075900120629             clear  SavC1Ccor;
076000120629           When  wOrdSfl = 3;
076100120629             SavC1Ccor = C1Ccor;
076200120629             clear  SavC1Ccod;
076300120629             clear  SavC1Cdes;
076400120629             clear  SavC1Cnar;
076500120629             clear  SavC1Crap;
076600120629             clear  SavC1Cprv;
076700120629         EndSl;
076800120628
076900120628         clear  S01nrr;
077000120628         SflNxtChg = *off;
077100120628
077200120628         // -?Ciclo di caricamento dati da tab. "CT"?
077300120628         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
077400120628         //  ?l'eventuale ordinamento)?
077500120628         DoW  Not $EoF;
077600120628
077700120628           // -?Selezione singolo record file TABEL00F?
077800120628           exsr  sr_SelezRec;
077900120628           if  $EoF;
078000120628             leave;
078100120628           endif;
078200120628
078300120628           // -?Registrazione singolo record nel subfile?
078400120628           if  $RecOK;
078500120628
078600120628             // ·?Impostazione campi?
078700120702             dsCT = TBLuni;
078800120628             clear  TB09S01;
078900120628             S1Ccod  = TBLkey;
079000120628             S1Cdes  = §CTdes;
079100120628             S1Cnar  = §CTnar;
079200120628             S1Crap  = §CTrap;
079300120628             S1Cprv  = §CTprv;
079400120628             S1Ccor2 = %subst( §CTcor : 1 : %len(S1Ccor2) );
079500120628             S1Ccor1 = %subst( §CTcor : %len(S1Ccor2) + 1
079600120628                                      : %len(S1Ccor1) );
079700120702             S1Cfcr = §CTfcr;
079800120702             //if  §CTfcr = 'R';
079900120702             //  S1Cfcr= 'S';
080000120702             //else;
080100120702             //  S1Cfcr = 'N';
080200120702             //endif;
080300120628             if  §CTuta = 'N';
080400120629               S1Cuta = §CTuta;
080500120628             else;
080600120629               S1Cuta = 'S';
080700120628             endif;
080800120628             if  §CTutf = 'S';
080900120629               S1Cutf = §CTutf;
081000120628             else;
081100120629               S1Cutf = 'N';
081200120628             endif;
081300120705             if  §CTsta = 'N';
081400120705               S1Csta = §CTsta;
081500120705             else;
081600120705               S1Csta = 'S';
081700120705             endif;
081800120628
081900120628             if  TBLflg <> *blank;
082000120628               S1Catb  = 'A';
082100120628             endif;
082200120702
082300120702             // -?Campi hidden?
082400120702             if  S1Ccor1 = '9';
082500120702               S1Hcor = *loval;
082600120702             else;
082700120702               S1Hcor = §CTcor;
082800120702             endif;
082900120629
083000120629             // -?Impostazione attributi di visualizzazione?
083100120629             Ord_x_COD    = (wOrdSfl = 0);
083200120629             Ord_x_DES    = (wOrdSfl = 1);
083300120629             Ord_x_NaRePr = (wOrdSfl = 2);
083400120629             Ord_x_COR    = (wOrdSfl = 3);
083500120628
083600120628             // ·?Scrittura record nel subfile?
083700120628             S01nrr += 1;
083800120628             write  TB09S01;
083900120628
084000120628           EndIf;
084100120628
084200120628           // -?Lettura record successivo?
084300120711           reade(N)  %kds(k03tabel00 : 2)  TABEL;
084400120628           $EoF = (%eof(TABEL00F));
084500120628
084600120628         EndDo;
084700120628
084800120628         // -?Visualizzazione del SFL (se ci sono dati)?
084900120628         SflDsp_N = (S01nrr <= *zero);
085000120628
085100120628         // -?Emissione messaggio di segnalazione raggiungimento limite?
085200120628         if  S01nrr >= c_MaxRec   and   Not %eof(TABEL00F);
085300120628           ErrGenerico = *on;
085400120628           ErrMessage  = *on;
085500120628           V1Dmsg = $Msg(01);
085600120628           leavesr;
085700120628         endif;
085800120628
085900120628         // -?Attivazione del SFLEND?
086000120628         SflEnd = ( $EoF );
086100120628
086200120628         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
086300120628         if  S01nrr > *zero;
086400120628           C1RcdNbr = 1;
086500120628         else;
086600120628           clear  C1RcdNbr;
086700120628         endif;
086800120628
086900120628         C1CsrRrn = C1RcdNbr;
087000120628
087100120628       ENDSR;
087200120628
087300120628       //--------------------------------------------------------------
087400120628       //?Selezione del singolo record letto dalla tabella "3EW".      ?
087500120628       //--------------------------------------------------------------
087600120628       BEGSR  sr_SelezRec;
087700120628
087800120628         reset  $RecOK;
087900120628
088000120628         dsCT = TBLuni;
088100120628
088200120628         select;
088300120628
088400120628           // -?Superato il limite massimo di record registrabili nel?
088500120628           //  ?subfile?
088600120628           when  S01nrr >= c_MaxRec;
088700120628             $EoF = *on;
088800120628             leavesr;
088900120711
089000120711           // -?Ricevuto singolo codice tassazione?
089100120711           when  TB09cod <> *blank  and  TBLkey > TB09cod;
089200120711             $EoF = *on;
089300120711             leavesr;
089400120711
089500120711           // -?Parzializzazioni inserite o ricevute?
089600120711           when  (V1Cest = 'I'  and  §CTest = 'E')  or
089700120711                 (V1Cest = 'E'  and  §CTest = *blank);
089800120711             leavesr;
089900120711           when  (V1Cuta = 'S'  and  §CTuta = 'N')  or
090000120711                 (V1Cuta = 'N'  and  §CTuta = *blank);
090100120711             leavesr;
090200120711           when  (V1Cutf = 'S'  and  §CTutf = *blank)  or
090300120711                 (V1Cutf = 'N'  and  §CTutf = 'S');
090400120711             leavesr;
090500120711           when  (V1Cutc = 'S'  and  §CTutc = 'N')  or
090600120711                 (V1Cutc = 'N'  and  §CTutc = 'S');
090700120711             leavesr;
090800120628
090900120628           // -?Richiesto posizionamento per codice tassazione:?
091000120628           //  ?si scartano i record con codice inferiore?
091100120629           when  wOrdSfl = 0  and  TBLkey < SavC1Ccod;
091200120628             leavesr;
091300120628
091400120628           // -?Richiesto posizionamento per descrizione tassazione:?
091500120628           //  ?si scartano i record con descrizione inferiore?
091600120629           when  wOrdSfl = 1  and  §CTdes < SavC1Cdes;
091700120628             leavesr;
091800120628
091900120628           // -?Richiesto posizionamento per Nazione/Regione/Provincia:?
092000120628           //  ?si scartano i record con Nazione/Regione/Provincia inferiore?
092100120629           when  wOrdSfl = 2  and  ( §CTnar + §CTrap + §CTprv ) <
092200120629                                   ( SavC1Cnar + SavC1Crap + SavC1Cprv );
092300120628             leavesr;
092400120628
092500120628           // -?Richiesto posizionamento per codice ordinamento:?
092600120628           //  ?si scartano i record con codice ordinamento inferiore?
092700120629           when  wOrdSfl = 3  and  §CTcor < SavC1Ccor;
092800120628             leavesr;
092900120628
093000120628         endsl;
093100120628
093200120628         $RecOK = *on;
093300120628
093400120628       ENDSR;
093500120628
093600120628       //--------------------------------------------------------------
093700120628       //?Abilitazione tasti funzionali in C01.                        ?
093800120628       //--------------------------------------------------------------
093900120628       BEGSR  sr_AbilFxxS01;
094000120628
094100120711         // -?F3 = Fine?
094200120711         F3Attivo  = (Not $Called);
094300120711
094400120628         // -?F5 = Refresh?
094500120628         F5Attivo  = *on;
094600120628
094700120628         // -?F8 = Ordinamento x ...?
094800120628         F8Attivo  = (S01nrr > *zero);
094900120628
095000120628         // -?Impostazione F8 nel piede del subfile?
095100120628         select;
095200120628           when  Not F8Attivo;
095300120629             clear  V1ZF08;
095400120628           when  Ord_x_COD;
095500120629             V1ZF08 = 'F8=Ord. x Descriz.  ';
095600120628           when  Ord_x_DES;
095700120629             V1ZF08 = 'F8=Ord. x Naz/Reg/Pr';
095800120628           when  Ord_x_NaRePr;
095900120711             V1ZF08 = 'F8=Ord. x Ord.Stampa';
096000120628           when  Ord_x_COR;
096100120629             V1ZF08 = 'F8=Ord. x Cod.Tassaz';
096200120628         endsl;
096300120628
096400120628         // -?F10 = Inserimento?
096500120628         F10Attivo = (TB09fun <> '1');
096600120712
096700120712         // -?F12 = Ritorno?
096800120712         F12Attivo = ($Called);
096900120628
097000120628         // -?F13 = Ripetizione?
097100120711         F13Attivo = (TB09fun <> '1'  and  S01nrrMax > 1);
097200120628
097300120628       ENDSR;
097400120628
097500120628       //--------------------------------------------------------------
097600120628       //?Gestione tasto funzionale F8 da videata C01 / S01            ?
097700120628       //?F8=Ordinamento subfile                                       ?
097800120628       //--------------------------------------------------------------
097900120628       BEGSR  sr_OrdS01;
098000120628
098100120629         // -?Subfile vuoto: nessun record da ordinare?
098200120711         if  S01nrrMax = *zero;
098300120629           leavesr;
098400120629         endif;
098500120711
098600120711         // -?REimpostazione del totale Nrr del subfile?
098700120711         //  ?(potrebbe essere qullo del rec. con l'ultima opzione?
098800120711         //  ?elaborata)?
098900120711         S01nrr = S01nrrMax;
099000120629
099100120629         // -?Comunque: bisogna riordinarlo?
099200120628         if  wOrdSfl < 3;
099300120628           wOrdSfl += 1;
099400120628         else;
099500120628           clear  wOrdSfl;
099600120628         endif;
099700120629
099800120629         // -?Impostazione richiesta di posizionamento nel subfile?
099900120629         Ord_x_COD    = (wOrdSfl = 0);
100000120629         Ord_x_DES    = (wOrdSfl = 1);
100100120629         Ord_x_NaRePr = (wOrdSfl = 2);
100200120629         Ord_x_COR    = (wOrdSfl = 3);
100300120628
100400120629         // -?Pulizia campi di posizionamento?
100500120629         //  ?(SONO SOLO DI INPUT)?
100600120629         //select;
100700120629         //  when  wOrdSfl = 0;
100800120629         //    clear  C1Ccod;
100900120629         //  when  wOrdSfl = 1;
101000120629         //    clear  C1Cdes;
101100120629         //  when  wOrdSfl = 2;
101200120629         //    clear  C1Cnar;
101300120629         //    clear  C1Crap;
101400120629         //    clear  C1Cprv;
101500120629         //  when  wOrdSfl = 3;
101600120629         //    clear  C1Ccor;
101700120629         //endsl;
101800120629
101900120629         // -?Subfile già posizionato: occorre ricaricarlo?
102000120629         if  SavC1Ccod <> *blank  or  SavC1Cdes <> *blank  or
102100120629             ( SavC1Cnar + SavC1Crap + SavC1Cprv ) <> *blank  or
102200120629             SavC1Ccor <> *blank;
102300120629           $InzS01 = *on;
102400120629         else;
102500120629           exsr  sr_SortSfl;
102600120629           exsr  sr_AbilFxxS01;
102700120629         endif;
102800120628
102900120628       ENDSR;
103000120628
103100120628       //--------------------------------------------------------------
103200120628       //?Ordinamento subfile S01.                                     ?
103300120628       //?» This subroutine sorts the subfile records. «               ?
103400120628       //--------------------------------------------------------------
103500120628       BEGSR  sr_SortSfl;
103600120628
103700120628         RrnLast  = S01nrr;
103800120628
103900120628         C1RcdNbr = 1;
104000120628         clear  C1CsrRrn;
104100120628         clear  S01nrr;
104200120628         clear  V1Dmsg;
104300120628         ErrGenerico = *off;
104400120628         ErrMessage  = *off;
104500120628         SflNxtChg   = *off;
104600120628         %subst(IndDspF : 50) = *off;
104700120628
104800120628         //?___________________________________________________________?
104900120628         //?Initialize the key fields to sort on.?
105000120628         //?There is one extra field not in the subfile -- Selected --?
105100120628         //?that is added to the record. This field is used to place?
105200120628         //?selected records in front of nonselected records.?
105300120628         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
105400120628
105500120628         clear  QLGKL;
105600120628         clear  QLGSKL;
105700120628         clear  QLGSCB;
105800120628         clear  QLGSCB00;
105900120628
106000120628         // -?Gestione della scelta ordinamento:?
106100120628         Select;
106200120628
106300120628           // ·?Ordinamento per Codice Tassazione?
106400120628           When  Ord_x_COD;
106500120628             // ·?n° campi chiave?
106600120628             QLGNBRK  = 1;
106700120628             // ·?1° campo: codice tassazione?
106800120628             QLGSP    = 1;
106900120628             QLGSS    = %size(S1Ccod);
107000120628             QLGDT    = Carattere;
107100120628             QLGSO    = Ascendente;
107200120628             QLGKL(1) = QLGSKL;
107300120628
107400120628           // ·?Ordinamento per Descrizione Tassazione?
107500120628           When  Ord_x_DES;
107600120628             // ·?n° campi chiave?
107700120628             QLGNBRK  = 1;
107800120628             // ·?1° campo: descrizione tassazione?
107900120628             QLGSP    = %size(S1Ccod) + 1;
108000120628             QLGSS    = %size(S1Cdes);
108100120628             QLGDT    = Carattere;
108200120628             QLGSO    = Ascendente;
108300120628             QLGKL(1) = QLGSKL;
108400120628
108500120628           // ·?Ordinamento per Nazione/Regione/Provincia?
108600120628           When  Ord_x_NaRePr;
108700120628             // ·?n° campi chiave?
108800120629             QLGNBRK  = 4;
108900120628             // ·?1° campo: nazione?
109000120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + 1;
109100120628             QLGSS    = %size(S1Cnar);
109200120628             QLGDT    = Carattere;
109300120628             QLGSO    = Ascendente;
109400120628             QLGKL(1) = QLGSKL;
109500120628             // ·?2° campo: regione?
109600120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
109700120628                        1;
109800120628             QLGSS    = %size(S1Crap);
109900120628             QLGDT    = Carattere;
110000120628             QLGSO    = Ascendente;
110100120628             QLGKL(2) = QLGSKL;
110200120628             // ·?3° campo: provincia?
110300120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
110400120628                        %size(S1Crap) + 1;
110500120628             QLGSS    = %size(S1Cprv);
110600120628             QLGDT    = Carattere;
110700120628             QLGSO    = Ascendente;
110800120628             QLGKL(3) = QLGSKL;
110900120628             // ·?4° campo: codice ordinamento?
111000120628             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
111100120628                        %size(S1Crap) + %size(S1Cprv) + 1;
111200120702             QLGSS    = %size(S1Ccor2) + %size(S1Hcor);
111300120628             QLGDT    = Carattere;
111400120628             QLGSO    = Ascendente;
111500120629             QLGKL(4) = QLGSKL;
111600120628
111700120629           // ·?Ordinamento per Codice Ordinamento?
111800120629           When  Ord_x_COR;
111900120629             // ·?n° campi chiave?
112000120629             QLGNBRK  = 2;
112100120629             // ·?1° campo: codice ordinamento (1°: 2 bytes)?
112200120629             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
112300120629                        %size(S1Crap) + %size(S1Cprv) + 1;
112400120629             QLGSS    = %size(S1Ccor2);
112500120629             QLGDT    = Carattere;
112600120629             QLGSO    = Ascendente;
112700120629             QLGKL(1) = QLGSKL;
112800120629             // ·?2° campo: codice ordinamento (2°: 1 byte)?
112900120629             QLGSP    = %size(S1Ccod) + %size(S1Cdes) + %size(S1Cnar) +
113000120702                        %size(S1Crap) + %size(S1Cprv) + %size(S1Ccor2) +
113100120629                        1;
113200120702             QLGSS    = %size(S1Hcor);
113300120629             QLGDT    = Carattere;
113400120629             QLGSO    = Ascendente;
113500120629             QLGKL(2) = QLGSKL;
113600120628
113700120628         EndSl;
113800120628
113900120628         // -?Load other sort parameters.?
114000120628         QLGLB   = 80 + 16 * MaxKey;
114100120628         QLGRL   = %size(SflRcd) - 1;
114200120628         QLGRT   = 8;
114300120628         QLGOKL  = 80;
114400120628         QLGLKE  = 16;
114500120628         QLGLSS  = 290;
114600120628         // -?Initialize Sort I/O API fields.?
114700120628         QLGRL00 = QLGRL;
114800120628         QLGRC00 = 1;
114900120628         clear  QUSEI;
115000120628         QUSBPRV = %size(QUSEC);
115100120628
115200120628         // -?First step - Initialize the sort routine.?
115300120628         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
115400120628                      SizeList : ReturnSize : QUSEC );
115500120628
115600120628         // -?Next step - Write records to I/O routine.?
115700120628         QLGRT00  = Put;
115800120628         For  Nrr = 1  To  RrnLast;
115900120628           chain  Nrr  TB09S01;
116000120628           // -?Solo le righe con Selected = 'Y' sono riordinate,?
116100120628           //  ?quindi per fare un ordinamento di tutte le righe?
116200120628           //  ?metto 'Y' sempre.?
116300120628           Selected = 'Y';
116400120628           clear  QUSEI;
116500120628           QUSBPRV  = %size(QUSEC);
116600120628           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
116700120628                         SizeList : NotUsed : QUSEC );
116800120628         EndFor;
116900120628
117000120628         // -?Next step - Signal end of input, clear subfile for reload.?
117100120628         QLGRT00 = EndPut;
117200120628         clear  QUSEI;
117300120628         QUSBPRV = %size(QUSEC);
117400120628         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
117500120628                       SizeList : NotUsed : QUSEC );
117600120628
117700120628         // -?Pulizia subfile?
117800120628         SflDsp_N    = *on;
117900120628         SflDspCtl_N = *on;
118000120628         write  TB09C01;
118100120628         SflDspCtl_N = *off;
118200120628         SflEnd      = *off;
118300120628
118400120628         // -?Final step - Write the records back to the subfile.?
118500120628         QLGRT00  = Get;
118600120628         For  Nrr = 1  To RrnLast;
118700120628           clear  QUSEI;
118800120628           QUSBPRV = %size(QUSEC);
118900120628           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
119000120628                         QLGRL00  : NotUsed : QUSEC );
119100120629           // -?Ripristino indicatori utilizzati nel sfl rec?
119200120629           SflNxtChg = (S1Copz <> *zero);
119300120629           Ord_x_COD    = (wOrdSfl = 0);
119400120629           Ord_x_DES    = (wOrdSfl = 1);
119500120629           Ord_x_NaRePr = (wOrdSfl = 2);
119600120629           Ord_x_COR    = (wOrdSfl = 3);
119700120629           // -?Scrivo record nel subfile?
119800120628           S01nrr = Nrr;
119900120628           write  TB09S01;
120000120628         EndFor;
120100120628
120200120628         C1CsrRrn = 1;
120300120628
120400120628         // -?Visualizzazione del SFL (se ci sono dati)?
120500120628         SflDsp_N = (S01nrr <= *zero);
120600120628
120700120628         // -?Attivazione del SFLEND?
120800120628         SflEnd = $EoF;
120900120628
121000120628       ENDSR;
121100120628
121200120628       //--------------------------------------------------------------
121300120628       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
121400120628       //?F13=Ripetizione opzione                                      ?
121500120628       //--------------------------------------------------------------
121600120628       BEGSR  sr_DupOpzS01;
121700120702
121800120703         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
121900120703         //  ?dalla posizione del cursore?
122000120702         wPag = C1CsrRrn / c_SflPag;
122100120702         wRig = %rem( C1CsrRrn : c_SflPag);
122200120703         if  wRig > *zero;
122300120703           wRig = (wPag * c_SflPag) + 1;
122400120703         else;                           // -?ultimo rec. della pag.?
122500120703           wRig = (wPag * c_SflPag) - c_SflPag + 1;
122600120702         endif;
122700120703         SavS01csr = wRig;
122800120628
122900120702         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
123000120702
123100120702         // -?Reperimento della 1ª opzione inserita?
123200120702         //  ?nella pagina dov'è il cursore?
123300120702         readc  TB09S01;
123400120702         DoW  Not %eof(TRTB09D)  and  S1Copz <> *zero  and
123500120702              S01nrr < SavS01csr;
123600120702           SflNxtChg = *on;
123700120702           update  TB09S01;
123800120702           readc  TB09S01;
123900120702         EndDo;
124000120702         if  Not %eof(TRTB09D)  and  S1Copz <> *zero;
124100120702           SflNxtChg = *on;
124200120702           update  TB09S01;
124300120702         endif;
124400120702
124500120703         // -?Nessuna opzione nella pagina?
124600120703         if  %eof(TRTB09D)         or
124700120703             S01nrr <  SavS01csr   or
124800120702             S01nrr > (SavS01csr + c_SflPag - 1);
124900120702           C1RcdNbr  = SavS01csr;
125000120702           C1CsrRrn  = SavS01csr;
125100120703           reset  $ForzaOpz;
125200120703           reset  SavS1Copz;
125300120703           clear  SavS01cs1;
125400120703           clear  SavS01cs2;
125500120703           clear  SavS1Cop1;
125600120703           //clear  SavS1Cop2;
125700120702           ErrGenerico = *on;
125800120702           ErrMessage  = *on;
125900120702           PosCurOPZ   = *on;
126000120702           V1Dmsg = $Msg(02);
126100120702           leavesr;
126200120702         endif;
126300120702
126400120703         // -?Memorizzazione 1ª opzione nella pagina?
126500120702         SavS01csr = S01nrr;
126600120702         SavS1Copz = S1Copz;
126700120703         if  SavS01csr <> SavS01cs1  or
126800120703             SavS1Copz <> SavS1Cop1;
126900120703           clear  $ForzaOpz;
127000120703         endif;
127100120703         SavS01cs1 = S01nrr;
127200120703         SavS1Cop1 = S1Copz;
127300120702
127400120702         // -?Reperimento delle altre eventuali opzioni?
127500120702         readc  TB09S01;
127600120703         DOW  Not %eof(TRTB09D);
127700120703
127800120703           // ·?Aggiornamento subfile?
127900120703           if  S1Copz <> *zero;
128000120703             SflNxtChg = *on;
128100120703             update  TB09S01;
128200120703           endif;
128300120703
128400120703           // ·?Reperita una 2ª opzione?
128500120703           Select;
128600120703
128700120703             // ·?Vuota!?
128800120703             When  S1Copz = *zero;
128900120703
129000120703             // ·?1ª opzione nella pagina del cursore  e?
129100120703             //  ?2ª opzione (diversa) in una pagina successiva?
129200120703             //  ?(messaggio di avviso di "ricopertura" - se la 2ª?
129300120703             //  ?opzione è diversa)?
129400120703             When  (SavS01csr >= wRig              and
129500120703                    SavS01csr <  wRig + c_SflPag   and
129600120703                    S01nrr    >= wRig + c_SflPag)  and
129700120703                    S1Copz    <> SavS1Copz         and
129800120703                    Not $ForzaOpz;
129900120703               $ForzaOpz = *on;
130000120703               SavS01cs2 = S01nrr;
130100120703               //SavS1Cop2 = S1Copz;
130200120703               C1RcdNbr  = SavS01csr;
130300120703               C1CsrRrn  = SavS01csr;
130400120703               ErrGenerico = *on;
130500120703               ErrMessage  = *on;
130600120703               PosCurOPZ   = *on;
130700120703               V1Dmsg = $Msg(03);
130800120703               leavesr;
130900120703
131000120703             // ·?2ª opzione sul record dove è posizionato il cursore?
131100120703             //  ?(la nuova da considerare per la duplicazione)?
131200120703             When  C1CsrRrn = S01nrr;
131300120703               SavS01csr = S01nrr;
131400120703               SavS1Copz = S1Copz;
131500120703
131600120703             // ·?Più opzioni nella stessa videata e?
131700120703             //  ?cursore su nessuna delle opzioni?
131800120703             //  ?(riposizionare il cursore)?
131900120703             //  ?Se stessa opzione in videate diverse: vince la?
132000120703             //  ?posizione della prima opzione.?
132100120703             When  C1CsrRrn <> SavS01csr  and
132200120703                   C1CsrRrn <> S01nrr     and
132300120703                   S01nrr   <  wRig + c_SflPag;
132400120703               SavS01cs2 = S01nrr;
132500120703               //SavS1Cop2 = S1Copz;
132600120703               C1RcdNbr  = SavS01csr;
132700120703               C1CsrRrn  = SavS01csr;
132800120703               ErrGenerico = *on;
132900120703               ErrMessage  = *on;
133000120703               PosCurOPZ   = *on;
133100120703               V1Dmsg = $Msg(04);
133200120703               leavesr;
133300120703
133400120703           EndSl;
133500120703
133600120703           readc  TB09S01;
133700120703
133800120703         ENDDO;
133900120703
134000120703         // ·?Opzione NON abilitata?
134100120703         if  SavS1Copz  > %elem($Opz)  or
134200120703            (SavS1Copz <= %elem($Opz)  and  $Opz(SavS1Copz) = *off);
134300120703           C1RcdNbr  = SavS01csr;
134400120703           C1CsrRrn  = SavS01csr;
134500120703           ErrGenerico = *on;
134600120703           ErrMessage  = *on;
134700120703           PosCurOPZ   = *on;
134800120703           V1Dmsg = $Msg(05);
134900120703           leavesr;
135000120703         endif;
135100120628
135200120628         // -?Ripetizione opzione sul resto del subfile?
135300120628         S01nrr = SavS01csr + 1;
135400120628         chain  S01nrr  TB09S01;
135500120628         DoW  %found(TRTB09D);
135600120628           SflNxtChg = *on;
135700120628           S1Copz = SavS1Copz;
135800120628           update  TB09S01;
135900120628           S01nrr += 1;
136000120628           chain  S01nrr  TB09S01;
136100120628         EndDo;
136200120628
136300120628       ENDSR;
136400120628
136500120628       //--------------------------------------------------------------
136600120628       //?Gestione opzioni del subfile S01                             ?
136700120628       //--------------------------------------------------------------
136800120628       BEGSR  sr_OpzS01;
136900120628
137000120628         clear  SavS01csr;
137100120628
137200120628         // -?Ciclo di lettura subfile?
137300120628         readc  TB09S01;
137400120628
137500120628         DoW  Not %eof(TRTB09D);
137600120628
137700120628           SflNxtChg = *off;
137800120628           %subst(IndDspF : 50) = *off;
137900120628           C1RcdNbr = S01nrr;
138000120711           //clear  wTB09opz;
138100120628
138200120628           Select;
138300120628
138400120628             // -?Nessuna opzione?
138500120629             When  S1Copz = *zero;
138600120628
138700120628             // -?Opzione NON abilitata?
138800120628             When  S1Copz  > %elem($Opz)  or
138900120628                  (S1Copz <= %elem($Opz)  and  $Opz(S1Copz) = *off);
139000120628               ErrGenerico = *on;
139100120628               ErrMessage  = *on;
139200120628               PosCurOPZ   = *on;
139300120703               V1Dmsg = $Msg(05);
139400120628
139500120628             // -?Opz. 1 = Selezione.?
139600120628             When  S1Copz = 1  and  Not $Fine;
139700120628               $Fine = *on;
139800120628               TB09cod = S1Ccod;
139900120628               TB09des = S1Cdes;
140000120711               //wTB09opz = S1Copz;
140100120628
140200120628             // -?Opz. 1 = Selezione già reperita?
140300120628             //  ?(ammessa una sola selezione)?
140400120628             when  S1Copz = 1  and  $Fine;
140500120628               ErrGenerico = *on;
140600120628               ErrMessage  = *on;
140700120628               PosCurOPZ   = *on;
140800120703               V1Dmsg = $Msg(06);
140900120628               $Fine  = *off;
141000120628
141100120628             // -?Opz. 2 = Modifica,?
141200120628             //       ?3 = Copia,?
141300120628             //       ?4 = Annullamento / Ripristino,?
141400120628             //       ?5 = Visualizzazione.?
141500120628             When  S1Copz = 2  or
141600120628                   S1Copz = 3  or
141700120628                   S1Copz = 4  or
141800120628                   S1Copz = 5;
141900120628               $Video  = 'D2';
142000120628               $InzD02 = *on;
142100120711               //wTB09opz = S1Copz;
142200120629               DoW  $Video = 'D2';
142300120628                 exsr  sr_GesD02;
142400120629                 if  $Fine = *on;
142500120629                   ErrGenerico = *on;
142600120629                   leavesr;
142700120629                 endif;
142800120628               EndDo;
142900120629               clear  V1Topz;
143000120628               // -?Se F12=Ritorno: esce dalla lettura del subfile?
143100120723               if  TB09fxx = 'L';
143200120628                 ErrGenerico = *on;
143300120628               endif;
143400120628
143500120628             // -?Opzione errata?
143600120628             Other;
143700120628               ErrGenerico = *on;
143800120628               ErrMessage  = *on;
143900120628               PosCurOPZ   = *on;
144000120703               V1Dmsg = $Msg(05);
144100120628
144200120628           EndSl;
144300120628
144400120628           // -?Memorizzazione nrr del subfile con la 1ª opzione per?
144500120628           //  ?riposizionarvicisi il cursore dopo il tasto "Invio"?
144600120628           if  S1Copz  <> *zero   and
144700120628               (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
144800120628             SavS01csr = C1RcdNbr;
144900120628           endif;
145000120628
145100120628           // -?Aggiornamento subfile?
145200120628           select;
145300120628             when  ErrMessage;
145400120628               SflNxtChg = *on;
145500120628               C1CsrRrn  = C1RcdNbr;
145600120628             when  ErrGenerico;
145700120628               C1CsrRrn  = C1RcdNbr;
145800120628               clear  S1Copz;
145900120628             other;
146000120628               clear  S1Copz;
146100120628           endsl;
146200120628
146300120628           UPDATE  TB09S01;
146400120628
146500120628           if  ErrGenerico   or   ErrMessage;
146600120628             leave;
146700120628           endif;
146800120628
146900120628           readc  TB09S01;
147000120628
147100120628         ENDDO;
147200120628
147300120628         // -?Riposiziono il cursore sul record contenente la prima opz.?
147400120628         //  ?(se non sono stati rilevati errori)?
147500120628         if  NOT ErrMessage   and   NOT ErrGenerico   and
147600120628             SavS01csr > *zero;
147700120628           C1CsrRrn = SavS01csr;
147800120628         endif;
147900120628
148000120628       ENDSR;
148100120628
148200120628       //--------------------------------------------------------------
148300120628       //?Gestione videata D02.                                        ?
148400120628       //--------------------------------------------------------------
148500120628       BEGSR  sr_GesD02;
148600120628
148700120628         // -?Inizializzazione videata?
148800120628         if  $InzD02 = *on;
148900120628           exsr  sr_InzD02;
149000120628           $InzD02 = *off;
149100120628         endif;
149200120628
149300120628         // -?Impostazione attributi di visualizzazione?
149400120628         exsr  sr_AttrVisD02;
149500120628
149600120628         // -?(Ri)Abilitazione tasti funzionali?
149700120628         exsr  sr_AbilFxxD02;
149800120628
149900120628         // -?Emissione Testata?
150000120628         write  TB09T01;
150100120628
150200120628         // -?Emissione videata (e piede)?
150300120711         //if  (wTB09opz = 4  and  TBLflg = *blank)  or
150400120711         //     wTB09opz = 5;
150500120711         if  (S1Copz = 4  and  TBLflg = *blank)  or
150600120711              S1Copz = 5;
150700120628           write  TB09D02;
150800120628           exfmt  Protect;
150900120628         else;
151000120628           exfmt  TB09D02;
151100120628         endif;
151200120628
151300120628         reset  ErrGenerico;
151400120628         reset  ErrMessage;
151500120628         clear  V1Dmsg;
151600120711
151700120711         clear  TB09fxx;
151800120628
151900120628         select;
152000120628
152100120628           // -?F3 = Fine?
152200120628           when  dsp_aid = c_F03;
152300120711             unlock  TABEL00F;
152400120628             $Fine = *on;
152500120711             TB09fxx = 'C';
152600120628
152700120628           // -?F8 = Successivo?
152800120628           WHEN  dsp_aid = c_F08;
152900120629             $Video = 'S1';
153000120628
153100120628           // -?F9 = Traduzione?
153200120628           WHEN  dsp_aid = c_F09;
153300120629             $Video  = 'S2';
153400120628             $InzS02 = *on;
153500120702             $F09D02 = *on;
153600120629             DoW  $Video = 'S2';
153700120628               exsr  sr_GesS02;
153800120628             EndDo;
153900120628
154000120628           // -?F12 = Ritorno?
154100120628           WHEN  dsp_aid = c_F12;
154200120711             unlock  TABEL00F;
154300120723             $Video  = 'S1';
154400120711             TB09fxx = 'L';
154500120628
154600120629           // -?Invio, F6 = Conferma?
154700120628           OTHER;
154800120628             // -?Controlli eseguiti SE NON richiesto annullamento?
154900120629             if  S1Copz <> 4;
155000120628               exsr  sr_CtrD02;
155100120628               if  ErrGenerico;
155200120628                 leavesr;
155300120628               endif;
155400120628             endif;
155500120629
155600120629             // -?F6 = Aggiornamento?
155700120629             IF  dsp_aid = c_F06;
155800120629
155900120629               // -?Richiesta dati di trasmissione?
156000120629               $Video  = 'W1';
156100120629               $InzW01 = *on;
156200120629               DoW  $Video = 'W1';
156300120629                 exsr  sr_GesW01;
156400120629               EndDo;
156500120629
156600120629             ENDIF;
156700120628
156800120628         ENDSL;
156900120628
157000120628       ENDSR;
157100120628
157200120628       //--------------------------------------------------------------
157300120628       //?Inizializzazione videata D02.                                ?
157400120628       //--------------------------------------------------------------
157500120628       BEGSR  sr_InzD02;
157600120629
157700120629         clear  TB09D02;
157800120628
157900120628         // -?Reperimento dati tabella (SE NON inserimento)?
158000120629         clear  dsCT;
158100120711         if  wTB09cmd <> 'J';
158200120702           k_TBLkut = c_Kut;
158300120628           k_TBLkey = S1Ccod;
158400120712           if  S1Copz = 5;
158500120712             chain(N)  %kds(k03tabel00)  TABEL;
158600120712           else;
158700120712             chain  %kds(k03tabel00)  TABEL;
158800120712           endif;
158900120629           if  %found(TABEL00F);
159000120629             dsCT = TBLuni;
159100120629           endif;
159200120628         endif;
159300120628
159400120628         // -?Impostazione testata?
159500120628         clear  V1Topz;
159600120628         select;
159700120711           when  wTB09cmd = 'J';
159800120628             //V1Topz = '  IMMISSIONE   ';
159900120628             V1Topz = '  INSERIMENTO  ';
160000120629             leavesr;
160100120711           //when  wTB09opz = 2;
160200120711           when  S1Copz = 2;
160300120628             V1Topz = '   MODIFICA    ';
160400120711           //when  wTB09opz = 3;
160500120711           when  S1Copz = 3;
160600120628             V1Topz = '     COPIA     ';
160700120711           //when  wTB09opz = 4  and  TBLflg = *blank;
160800120711           when  S1Copz = 4  and  TBLflg = *blank;
160900120628             V1Topz = ' ANNULLAMENTO  ';
161000120711           //when  wTB09opz = 4  and  TBLflg = '*';
161100120711           when  S1Copz = 4  and  TBLflg = '*';
161200120628             V1Topz = '  RIPRISTINO   ';
161300120711           //when  wTB09opz = 5;
161400120711           when  S1Copz = 5;
161500120628             V1Topz = 'VISUALIZZAZIONE';
161600120628           other;
161700120628             V1Topz = ' ? ? ? ? ? ? ? ';
161800120628         endsl;
161900120628
162000120628         // -?Impostazione dettaglio?
162100120628         V2Ccod  = TBLkey;
162200120628         V2Cdes  = §CTdes;
162300120628         V2Cprv  = §CTprv;
162400120628         V2Cfcr  = §CTfcr;
162500120628         V2Crap  = §CTrap;
162600120628         V2Ccor2 = %subst( §CTcor : 1 : %len(V2Ccor2) );
162700120628         V2Ccor1 = %subst( §CTcor : %len(V2Ccor2) + 1
162800120628                                  : %len(V2Ccor1) );
162900120705         //V2Cfai  = §CTfai;        // -?TOLTO (non più usato)?
163000120628         V2Cest  = §CTest;
163100120628         V2Cnar  = §CTnar;
163200120628         V2Ccap  = §CTcap;
163300120628         V2Cutc  = §CTutc;
163400120628         V2Crct  = §CTrct;
163500120628         V2C§§5  = §CT§§5;
163600120705         //V2Cfdk  = §CTfdk;        // -?TOLTO (non più usato)?
163700120628         V2Cuta  = §CTuta;
163800120628         V2Cutf  = §CTutf;
163900120628         V2Csta  = §CTsta;
164000120628
164100120628         // -?Decodifica Provincia?
164200120628         clear dsPR;
164300120628         if  §CTprv <> *blank;
164400120628           ds_TNTBE.TBEke1 = §CTprv;
164500120628           if  getTabella ( c_Tabel : 'PR'  : ds_TNTBE.TBEke1 :
164600120628                            *omit   : *omit : *omit :
164700120628                            *omit : *omit :
164800120628                            *omit : *omit : *omit : *omit :
164900120628                            *omit : *omit : *omit : *omit :
165000120628                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
165100120628                          = *zero      AND
165200120628               ds_TNTBE.TBEatb = *blank;
165300120628             dsPR   = ds_TNTBE.TBEuni;
165400120628           else;
165500120628             §PRdes = *all'? ';
165600120628           endif;
165700120628         EndIf;
165800120628         V2Dprv = §PRdes;
165900120628
166000120628         // -?Decodifica Nazione?
166100120705         clear ds15;
166200120628         If  §CTnar <> *blank;
166300120628           ds_TNTBE.TBEke1 = §CTnar;
166400120628           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
166500120628                            *omit   : *omit : *omit :
166600120628                            *omit : *omit :
166700120628                            *omit : *omit : *omit : *omit :
166800120628                            *omit : *omit : *omit : *omit :
166900120628                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
167000120628                          = *zero      AND
167100120628               ds_TNTBE.TBEatb = *blank;
167200120628             ds15   = ds_TNTBE.TBEuni;
167300120628           else;
167400120628             §15des = *all'? ';
167500120628           endif;
167600120628         EndIf;
167700120628         V2Dnar = §15des;
167800120705
167900120705         // -?Decodifica Regione o Nazione di appartenenza?
168000120705         clear dsCT_rap;
168100120705         If  §CTrap <> *blank;
168200120705           ds_TNTBE.TBEke1 = §CTrap;
168300120705           if  getTabella ( c_Tabel : 'CT'  : ds_TNTBE.TBEke1 :
168400120705                            *omit   : *omit : *omit :
168500120705                            *omit : *omit :
168600120705                            *omit : *omit : *omit : *omit :
168700120705                            *omit : *omit : *omit : *omit :
168800120705                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
168900120705                          = *zero      AND
169000120705               ds_TNTBE.TBEatb = *blank;
169100120705             dsCT_rap  = ds_TNTBE.TBEuni;
169200120705           else;
169300120705             rap§CTdes = *all'? ';
169400120705           endif;
169500120705         EndIf;
169600120705         V2Drap = rap§CTdes;
169700120712
169800120712         // -?Decodifica Codice Nazione?
169900120712         //If  V2Cnar <> *blank;
170000120712           ds_TNTBE.TBEke1 = V2Cnar;
170100120712           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
170200120712                            *omit   : *omit : *omit :
170300120712                            *omit : *omit :
170400120712                            *omit : *omit : *omit : *omit :
170500120712                            *omit : *omit : *omit : *omit :
170600120712                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
170700120712                          = *zero      AND
170800120712               ds_TNTBE.TBEatb = *blank;
170900120712             ds15   = ds_TNTBE.TBEuni;
171000120712             V2Dnar = §15des;
171100120712           else;
171200120712             V2Dnar = *all'? ';
171300120712           endif;
171400120712         //EndIf;
171500120628
171600120628       ENDSR;
171700120628
171800120628       //--------------------------------------------------------------
171900120628       //?Impostazione attributi di visualizzazione in D02.            ?
172000120628       //--------------------------------------------------------------
172100120628       BEGSR  sr_AttrVisD02;
172200120628
172300120628         // -?Protezione campo chiave (SE NON immissione o copia)?
172400120711         //Protect_Cod = (wTB09cmd <> 'J'  and  wTB09opz <> 3);
172500120711         Protect_Cod = (wTB09cmd <> 'J'  and  S1Copz <> 3);
172600120628
172700120628         // -?Codice Tassazione annullato in tabella?
172800120628         Tass_Annull = (%found(TABEL00F)  and  TBLflg <> *blank);
172900120628
173000120628       ENDSR;
173100120628
173200120628       //--------------------------------------------------------------
173300120628       //?Abilitazione tasti funzionali in D02.                        ?
173400120628       //--------------------------------------------------------------
173500120628       BEGSR  sr_AbilFxxD02;
173600120628
173700120628         // -?F6 = Conferma?
173800120711         //F6Attivo  = (wTB09opz <> 5);
173900120711         F6Attivo  = (S1Copz <> 5);
174000120628
174100120628         // -?F8 = Successivo?
174200120628         F8Attivo  = *on;
174300120628
174400120628       ENDSR;
174500120628
174600120628       //--------------------------------------------------------------
174700120628       //?Controllo dati nella videata D02.                            ?
174800120628       //--------------------------------------------------------------
174900120628       BEGSR  sr_CtrD02;
175000120628
175100120628         %subst(IndDspF : 50) = *off;
175200120629         k_TBLkut = c_Kut;
175300120629         //k_TBLcod = c_Tab;       ?(già così)?
175400120629         k_TBLkey = V2Ccod;
175500120629
175600120629         // -?Codice Tassazione?
175700120629         Select;
175800120629           // ·?Obbligatorio?
175900120629           When  V2Ccod = *blank;
176000120629             ErrGenerico = *on;
176100120629             ErrMessage  = *on;
176200120629             PosCurCOD   = *on;
176300120703             V1Dmsg = $Msg(07);
176400120629             leavesr;
176500120629           // ·?SE immissione o copia: controllo inesistenza nuovo?
176600120711           //When  wTB09cmd = 'J'  or  wTB09opz = 3;
176700120711           When  wTB09cmd = 'J'  or  S1Copz = 3;
176800120629             setll  %kds(k03tabel00)  TABEL;
176900120629             if  %equal(TABEL00F);
177000120629               ErrGenerico = *on;
177100120629               ErrMessage  = *on;
177200120629               PosCurCOD   = *on;
177300120703               V1Dmsg = $Msg(08);
177400120629               leavesr;
177500120629             endif;
177600120629         EndSl;
177700120629
177800120629         // -?Descrizione obbligatoria?
177900120629         if  V2Cdes = *blank;
178000120629           ErrGenerico = *on;
178100120629           ErrMessage  = *on;
178200120629           PosCurDES   = *on;
178300120703           V1Dmsg = $Msg(09);
178400120629           leavesr;
178500120629         endif;
178600120629
178700120629         // -?Provincia: ricerca?
178800120629         If  %scan( '?' : V2Cprv ) > *zero;
178900120629           TABEL_Ricerca( 'PR' :
179000120629                          *blank :
179100120629                          idElemento :
179200120629                          tastoFunzionaleUscita );
179300120629           if  tastoFunzionaleUscita = *blank;
179400120629             V2Cprv = idElemento;
179500120629           endif;
179600120629           PosCurPRV = *on;
179700120629         EndIf;
179800120629         // -?Provincia: controllo?
179900120629         clear  V2Dprv;
180000120629         If  V2Cprv <> *blank;
180100120629           ds_TNTBE.TBEke1 = V2Cprv;
180200120629           if  getTabella ( c_Tabel : 'PR'  : ds_TNTBE.TBEke1 :
180300120629                            *omit   : *omit : *omit :
180400120629                            *omit : *omit :
180500120629                            *omit : *omit : *omit : *omit :
180600120629                            *omit : *omit : *omit : *omit :
180700120629                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
180800120629                          = *zero      AND
180900120629               ds_TNTBE.TBEatb = *blank;
181000120629             dsPR   = ds_TNTBE.TBEuni;
181100120629             V2Dprv = §PRdes;
181200120629           else;
181300120629             V2Dprv = *all'? ';
181400120629             ErrGenerico = *on;
181500120629             ErrMessage  = *on;
181600120629             PosCurPRV   = *on;
181700120703             V1Dmsg = $Msg(10);
181800120629             leavesr;
181900120629           endif;
182000120629         EndIf;
182100120629
182200120705         // -?Sigla Regione/Nazione di appartenenza: controllo?
182300120705         clear  V2Drap;
182400120705         Select;
182500120705           When  V2Crap = *blank;
182600120711           //When  V2Crap = V2Ccod  and  (wTB09cmd = 'J'
182700120711           //                        or   wTB09opz  = 3);
182800120705           When  V2Crap = V2Ccod;
182900120705             V2Drap = V2Cdes;
183000120705           Other;
183100120705             ds_TNTBE.TBEke1 = V2Crap;
183200120705             if  getTabella ( c_Tabel : 'CT'  : ds_TNTBE.TBEke1 :
183300120705                              *omit   : *omit : *omit :
183400120705                              *omit : *omit :
183500120705                              *omit : *omit : *omit : *omit :
183600120705                              *omit : *omit : *omit : *omit :
183700120705                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
183800120705                            = *zero      AND
183900120705                 ds_TNTBE.TBEatb = *blank;
184000120705               dsCT_rap = ds_TNTBE.TBEuni;
184100120705               V2Drap   = rap§CTdes;
184200120705             else;
184300120705               V2Drap = *all'? ';
184400120705               ErrGenerico = *on;
184500120705               ErrMessage  = *on;
184600120705               PosCurRAP   = *on;
184700120705               V1Dmsg = $Msg(11);
184800120705               leavesr;
184900120705             endif;
185000120705         EndSl;
185100120705
185200120629         // -?Nazione: ricerca?
185300120629         If  %scan( '?' : V2Cnar ) > *zero;
185400120629           TABEL_Ricerca( '15' :
185500120629                          *blank :
185600120629                          idElemento :
185700120629                          tastoFunzionaleUscita );
185800120629           if  tastoFunzionaleUscita = *blank;
185900120629             V2Cnar = idElemento;
186000120629           endif;
186100120629           PosCurNAR = *on;
186200120629         EndIf;
186300120629         // -?Nazione: controllo?
186400120629         clear  V2Dnar;
186500120712         //If  V2Cnar <> *blank;
186600120629           ds_TNTBE.TBEke1 = V2Cnar;
186700120629           if  getTabella ( c_Tabel : '15'  : ds_TNTBE.TBEke1 :
186800120629                            *omit   : *omit : *omit :
186900120629                            *omit : *omit :
187000120629                            *omit : *omit : *omit : *omit :
187100120629                            *omit : *omit : *omit : *omit :
187200120629                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
187300120629                          = *zero      AND
187400120629               ds_TNTBE.TBEatb = *blank;
187500120629             ds15   = ds_TNTBE.TBEuni;
187600120629             V2Dnar = §15des;
187700120629           else;
187800120629             V2Dnar = *all'? ';
187900120629             ErrGenerico = *on;
188000120629             ErrMessage  = *on;
188100120629             PosCurNAR   = *on;
188200120705             V1Dmsg = $Msg(12);
188300120629             leavesr;
188400120629           endif;
188500120712         //EndIf;
188600120628
188700120628       ENDSR;
188800120628
188900120628       //--------------------------------------------------------------
189000120628       //?Gestione window W01.                                         ?
189100120628       //--------------------------------------------------------------
189200120628       BEGSR  sr_GesW01;
189300120628
189400120628         // -?Inizializzazione videata?
189500120628         if  $InzW01 = *on;
189600120628           exsr  sr_InzW01;
189700120628           $InzW01 = *off;
189800120628         endif;
189900120628
190000120628         // -?Emissione Window?
190100120628         exfmt  TB09W01;
190200120628
190300120628         reset  ErrGenerico;
190400120628         reset  ErrMessage;
190500120628         clear  V1Dmsg;
190600120628
190700120628         select;
190800120628
190900120628           // -?F12 = Ritorno?
191000120628           WHEN  dsp_aid = c_F12;
191100120723             $Video  = 'D2';
191200120711             TB09fxx = 'L';
191300120628
191400120628           // -?Invio, F6 = Conferma?
191500120628           OTHER;
191600120628             exsr  sr_CtrW01;
191700120628             if  ErrGenerico;
191800120628               leavesr;
191900120628             endif;
192000120628             // -?F6 = Aggiornamento?
192100120628             If  dsp_aid = c_F06;
192200120628               exsr  sr_UpdTABEL;
192300120628               if  ErrGenerico = *on;
192400120628                 leavesr;
192500120628               endif;
192600120629               // ·?Ritorno al subfile?
192700120629               $Video  = 'S1';
192800120629               $InzS01 = *on;
192900120702               C1Ccod  = SavC1Ccod;
193000120702               C1Cdes  = SavC1Cdes;
193100120702               C1Cnar  = SavC1Cnar;
193200120702               C1Crap  = SavC1Crap;
193300120702               C1Cprv  = SavC1Cprv;
193400120702               C1Ccor  = SavC1Ccor;
193500120628             EndIf;
193600120628
193700120628         ENDSL;
193800120628
193900120628       ENDSR;
194000120628
194100120628       //--------------------------------------------------------------
194200120628       //?Inizializzazione window W01.                                 ?
194300120628       //--------------------------------------------------------------
194400120628       BEGSR  sr_InzW01;
194500120628
194600120628         clear  TB09W01;
194700120628
194800120628         // -?Se non immissione: imposta i campi del file?
194900120711         If  wTB09cmd <> 'J';
195000120628
195100120628           W01ftt = TBLftt;
195200120628           W01flt = TBLflt;
195300120628           W01ftr = TBLftr;
195400120712           // ·?Conversione data (se NON immissione o copia)?
195500120712           if  wTB09cmd <> 'J'  and  S1Copz <> 3
195600120712                                and  TBLdtr  > *zero;
195700120629             wDate_DMY = %date(TBLdtr : *ymd);
195800120629             W01dtr    = %dec( wDate_DMY );
195900120628           endif;
196000120628
196100120628         EndIf;
196200120628
196300120628       ENDSR;
196400120628
196500120628       //--------------------------------------------------------------
196600120628       //?Controllo dati nella window W01.                             ?
196700120628       //--------------------------------------------------------------
196800120628       BEGSR  sr_CtrW01;
196900120628
197000120628         %subst(IndDspF : 50) = *off;
197100120628
197200120628       ENDSR;
197300120629
197400120629       //--------------------------------------------------------------
197500120629       //?Aggiornamento record nel file TABEL00F.                      ?
197600120629       //--------------------------------------------------------------
197700120629       BEGSR  sr_UpdTABEL;
197800120629
197900120629         // -?RI-aggancio record da aggiornare?
198000120629         k_TBLkut = c_Kut;
198100120629         //k_TBLcod = c_Tab;       ?(già così)?
198200120629         k_TBLkey = V2Ccod;
198300120629
198400120629         chain  %kds(k03tabel00)  TABEL;
198500120629
198600120629         // -?Impostazione campi-chiave SE record NON trovato?
198700120629         if  Not %found(TABEL00F);
198800120629           TBLkut = k_TBLkut;
198900120629           TBLcod = k_TBLcod;
199000120629           TBLkey = k_TBLkey;
199100120629         endif;
199200120629
199300120629
199400120629         // -?Annullamento / Ripristino?
199500120711         //If  wTB09opz = 4;
199600120711         If  S1Copz = 4;
199700120629           select;
199800120629             // ·?Annullamento?
199900120629             when  TBLflg = *blank;
200000120629               TBLflg = '*';
200100120629             // ·?Ripristino?
200200120629             when  TBLflg <> *blank;
200300120629               clear  TBLflg;
200400120629           endsl;
200500120629         EndIf;
200600120629
200700120629
200800120629         // -?Riempimento struttura dati dsCT?
200900120629         clear  dsCT;
201000120629
201100120629         §CTdes = V2Cdes;
201200120629         §CTprv = V2Cprv;
201300120629         §CTfcr = V2Cfcr;
201400120629         §CTrap = V2Crap;
201500120629         //%subst( §CTcor : 1 : 2 ) = V2Ccor2;
201600120629         //%subst( §CTcor : 3 : 1 ) = V2Ccor1;
201700120629         §CTcor = V2Ccor2 + V2Ccor1;
201800120705         //§CTfai = V2Cfai;         // -?TOLTO (non più usato)?
201900120705         //§CTfdk = V2Cfdk;         // -?TOLTO (non più usato)?
202000120702         §CT§§5 = V2C§§5;
202100120629         §CTest = V2Cest;
202200120629         §CTnar = V2Cnar;
202300120629         §CTcap = V2Ccap;
202400120629         §CTutc = V2Cutc;
202500120629         §CTutf = V2Cutf;
202600120629         §CTuta = V2Cuta;
202700120629         §CTsta = V2Csta;
202800120629         §CTrct = V2Crct;
202900120629
203000120629         TBLuni = dsCT;
203100120629
203200120629         TBLftt = W01ftt;
203300120629         TBLflt = W01flt;
203400120629         clear  TBLftr;
203500120629         clear  TBLdtr;
203600120629
203700120629
203800120629         // -?Aggiornamento / Inserimento?
203900120629         If  %found(TABEL00F);
204000120629           //____________
204100120629           update  TABEL;
204200120629           //¯¯¯¯¯¯¯¯¯¯¯¯
204300120629         Else;
204400120629           //____________
204500120629           write   TABEL;
204600120629           //¯¯¯¯¯¯¯¯¯¯¯¯
204700120629         EndIf;
204800120629
204900120629         // -?Se Opz. 4 = Annullamento / Ripristino?
205000120629         //  ?Vanno aggiornate automaticamente anche le descrizioni?
205100120629         //  ?in lingua?
205200120711         //IF  wTB09opz = 4;
205300120711         IF  S1Copz = 4;
205400120629
205500120629           LINtabel = 1;
205600120629           setgt  LINtabel  AZLIN000;
205700120629           read  AZLIN000;
205800120629
205900120629           DoW  Not %eof(AZLIN01L);
206000120629
206100120629             // ·?Aggiornamento?
206200120629             k_TBLkut = LINtabel;
206300120629             chain  %kds(k03tabel00)  TABEL;
206400120629             if  %found(TABEL00F);
206500120629               select;
206600120629                 when  TBLflg <> *blank;
206700120629                   clear TBLflg;
206800120629                 when  TBLflg =  *blank;
206900120629                   TBLflg = '*';
207000120629               endsl;
207100120629               //____________
207200120629               update  TABEL;
207300120629               //¯¯¯¯¯¯¯¯¯¯¯¯
207400120629             endif;
207500120629
207600120629             read  AZLIN000;
207700120629
207800120629           EndDo;
207900120629
208000120629         // -?Altrimenti (Opz. <> 4):?
208100120629         //  ?Aggiornate delle descrizioni in lingua?
208200120629         ELSE;
208300120629
208400120702           $F09D02 = *off;
208500120629           $Video  = 'S2';
208600120629           $InzS02 = *on;
208700120629
208800120629           DoW  $Video = 'S2';
208900120629             exsr  sr_GesS02;
209000120629           EndDo;
209100120629
209200120629         ENDIF;
209300120629
209400120629       ENDSR;
209500120629
209600120629       //--------------------------------------------------------------
209700120629       //?Gestione subfile S02.                                        ?
209800120629       //--------------------------------------------------------------
209900120629       BEGSR  sr_GesS02;
210000120629
210100120629         // -?Inizializzazione videata?
210200120629         if  $InzS02 = *on;
210300120629           exsr  sr_InzS02;
210400120629           $InzS02 = *off;
210500120629         endif;
210600120629
210700120629         // -?Emissione Window (con Piede)?
210800120629         write  TB09Z02;
210900120629
211000120629         // -?Posizionamento cursore?
211100120629         if  C2RcdNbr <= *zero;
211200120629           C2RcdNbr = 1;
211300120629         endif;
211400120629
211500120629         // -?Emissione videata?
211600120629         exfmt  TB09C02;
211700120629
211800120629         reset  ErrGenerico;
211900120629         reset  ErrMessage;
212000120629         clear  V1Dmsg;
212100120629
212200120629         select;
212300120629
212400120629           // -?F12=Ritorno?
212500120629           when  dsp_aid = c_F12;
212600120629             $Video = 'D2';
212700120629             leavesr;
212800120629
212900120629           // -?Invio / F6 = Conferma?
213000120629           OTHER;
213100120702             If  Not $F09D02;
213200120629               // -?Controlli (SE NON sola visualizzazione)?
213300120629               exsr  sr_CtrS02;
213400120629               if  ErrGenerico;
213500120629                 leavesr;
213600120629               endif;
213700120629               // -?Aggiornamento (SE premuto F6)?
213800120629               if  dsp_aid = c_F06;
213900120629                 exsr  sr_updTABling;
214000120629               endif;
214100120629             EndIf;
214200120629
214300120629         ENDSL;
214400120629
214500120629       ENDSR;
214600120629
214700120629       //--------------------------------------------------------------
214800120629       //?Inizializzazione subfile S02.                                ?
214900120629       //--------------------------------------------------------------
215000120629       BEGSR  sr_InzS02;
215100120629
215200120629         // -?Spegnimento degli indicatori di errore?
215300120629         %subst(IndDspF : 50) = *off;
215400120629
215500120629         // -?Pulizia subfile?
215600120629         wSflDsp_N    = *on;
215700120629         wSflDspCtl_N = *on;
215800120629         write  TB09C02;
215900120629         wSflDspCtl_N = *off;
216000120629         wSflEnd      = *off;
216100120629
216200120629         //wSflNxtChg = *off;
216300120629
216400120629         clear  SavS02nrr;
216500120629         clear  C2RcdNbr;
216600120629         clear  S02nrr;
216700120629         clear  V1Dmsg;
216800120629         ErrGenerico = *off;
216900120629         ErrMessage  = *off;
217000120629
217100120629         // -?Impostazione dati in testata?
217200120629         C2Ccod = V2Ccod;
217300120629         C2Cdes = V2Cdes;
217400120629
217500120629         // -?Caricamento completo dei dati nel subfile?
217600120629         //  ?(per ogni lingua)?
217700120629         setgt  (c_Kut)  AZLIN000;
217800120629         read  AZLIN000;
217900120629
218000120629         DoW  Not %eof(AZLIN01L);
218100120629
218200120629           // ·?Reperimento dati in lingua da tab. "CT"?
218300120629           k_TBLkut = LINtabel;
218400120629           //k_TBLcod = c_Tab;     ?(già così)?
218500120629           k_TBLkey = V2Ccod;
218600120629           chain(N)  %kds(k03tabel00)  TABEL;
218700120629
218800120629           // ·?Impostazione campi?
218900120629           clear  TB09S02;
219000120702           F12Attivo = ($F09D02 = *on);
219100120629           S2HcdLin = LINtabel;
219200120629           S2Dlin   = LINdesIta;
219300120629           if  Not %found(TABEL00F);
219400120629             clear  S2Cdes;
219500120629           else;
219600120629             dsCT = TBLuni;
219700120629             S2Cdes = §CTdes;
219800120629           endif;
219900120629
220000120629           // ·?Scrittura record nel subfile?
220100120629           S02nrr += 1;
220200120629           write  TB09S02;
220300120629
220400120629           // ·?Lettura record di lingua successivo?
220500120629           read  AZLIN000;
220600120629
220700120629         EndDo;
220800120629
220900120629         // -?Salvataggio del n° lingue scritte nel subfile?
221000120629         SavS02nrr = S02nrr;
221100120629
221200120629         // -?Visualizzazione del SFL (se ci sono dati)?
221300120629         wSflDsp_N = (S02nrr <= *zero);
221400120629
221500120629         // -?Attivazione del SFLEND?
221600120629         wSflEnd = ( %eof(AZLIN01L) );
221700120629
221800120629         // -?Impaginazione subfile?
221900120629         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
222000120629         if S02nrr > *zero;
222100120629           C2RcdNbr  = 1;
222200120629         else;
222300120629           clear  C2RcdNbr;
222400120629         endif;
222500120629
222600120629         // -?(Dis)Abilitazione tasti funzionali?
222700120702         F6Attivo  = ($F09D02 = *off);
222800120702         F12Attivo = ($F09D02 = *on);
222900120629
223000120629       ENDSR;
223100120629
223200120629       //--------------------------------------------------------------
223300120629       //?Controllo dati nel subfile S02.                              ?
223400120629       //--------------------------------------------------------------
223500120629       BEGSR  sr_CtrS02;
223600120629
223700120629         // -?Ciclo di controllo per ogni lingua?
223800120629         For  Nrr = 1  To  SavS02nrr;
223900120629
224000120629           chain  Nrr  TB09S02;
224100120629
224200120629           If  %found(TRTB09D);
224300120629
224400120629             %subst(IndDspF : 50) = *off;
224500120629
224600120629             // -?Descrizione (in lingua) obbligatoria?
224700120629             if  S2Cdes = *blank;
224800120629               ErrGenerico = *on;
224900120629               ErrMessage  = *on;
225000120629               PosCurW1des = *on;
225100120703               V1Dmsg = $Msg(09);
225200120629             endif;
225300120629
225400120629             // -?Posizionamento cursore sul rec. in errore?
225500120629             if  ErrGenerico;
225600120629               C2RcdNbr = Nrr;
225700120629               update  TB09S02;
225800120629               leave;
225900120629             endif;
226000120629
226100120629           EndIf;
226200120629
226300120629         EndFor;
226400120629
226500120629       ENDSR;
226600120629
226700120629       //--------------------------------------------------------------
226800120629       //?Aggiornamento record nel file TABEL00F (in lingua).          ?
226900120629       //--------------------------------------------------------------
227000120629       BEGSR  sr_UpdTABling;
227100120629
227200120629         // -?Reperimento dei dati della tabella in Italiano?
227300120629         clear  SavTBLuni;
227400120629         k_TBLkut = c_Kut;
227500120629         k_TBLcod = c_Tab;
227600120629         k_TBLkey = V2Ccod;
227700120629         chain(N)  %kds(k03tabel00)  TABEL;
227800120629         if  %found(TABEL00F);
227900120629           SavTBLuni = TBLuni;
228000120629         endif;
228100120629
228200120629         // -?Ciclo di lettura del subfile  e?
228300120629         //  ?aggiornamento/scrittura dei dati in lingua?
228400120629         For  Nrr = 1  To  SavS02nrr;
228500120629
228600120629           chain  Nrr  TB09S02;
228700120629
228800120629           if  Not %found(TRTB09D);
228900120629             iter;
229000120629           endif;
229100120629
229200120629           // -?Aggancio record da scrivere/aggiornare?
229300120629           k_TBLkut = S2HcdLin;
229400120629           chain  %kds(k03tabel00)  TABEL;
229500120629
229600120629           dsCT   = SavTBLuni;
229700120629           §CTdes = S2Cdes;
229800120629           TBLuni = dsCT;
229900120629           TBLftt = W01ftt;
230000120629           clear  TBLftr;
230100120629           clear  TBLdtr;
230200120629
230300120629           if  NOt %found(TABEL00F);
230400120629             TBLkut = k_TBLkut;
230500120629             TBLcod = c_Tab;
230600120629             TBLkey = V2Ccod;
230700120629             //___________
230800120629             write  TABEL;
230900120629             //¯¯¯¯¯¯¯¯¯¯¯
231000120629           else;
231100120629             //___________
231200120629             update TABEL;
231300120629             //¯¯¯¯¯¯¯¯¯¯¯
231400120629           endif;
231500120629
231600120629         EndFor;
231700120629
231800120629         $Video  = 'S1';
231900120629         $InzS01 = *on;
232000120629
232100120629       ENDSR;
232200120628
232300120628       //--------------------------------------------------------------
232400120628       //?Operazioni finali.                                           ?
232500120628       //--------------------------------------------------------------
232600120628       BEGSR  sr_RoutEnd;
232700120705
232800120705         // -?Chiusura applicazioni utilizzate?
232900120705         cloTabella();
233000120628
233100120628         // -?Restituzione parametri al chiamante?
233200120628         kpjbu = TRTB09ds;
233300120628
233400120628         // -?Uscita dal *pgm?
233500120628         return;
233600120628
233700120628       ENDSR;
233800060510
233900060510      /end-free
234000120627
234100120627       //--------------------------------------------------------------
234200120627       //?Definizione schiere a tempo di compilazione                  ?
234300120627       //--------------------------------------------------------------
234400120627
234500120627** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
234600120628SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)         1
234700120628Immettere un'opzione su questa pagina prima di premere F13                     2
234800120702Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 3
234900120703Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 4
235000120703Opzione errata                                                                 5
235100120703Ammessa la selezione di un solo codice tassazione                              6
235200120703Immettere il codice tassazione                                                 7
235300120703Codice Tassazione già esistente                                                8
235400120703Immettere la descrizione                                                       9
235500120703Provincia errata                                                              10
235600120705Sigla Regione di appartenenza errata                                          11
235700120705Nazione errata                                                                12
