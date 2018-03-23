000100120817       //==============================================================
000200120817       //?TNTB79R2 * Annullamento fisico record tab. "3EW" con flag    ?
000300120817       //?           "Annullamento Applicativo" valorizzato.           ?
000400120817       //==============================================================
000500120817
000600120817       //--------------------------------------------------------------
000700120817       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800120817       //--------------------------------------------------------------
000900120817
001000120817     /*PRM dbgview(*source)
001100120817     /*END
001200120817
001300120817       //--------------------------------------------------------------
001400120817       //?Specifiche di controllo.                                     ?
001500120817       //--------------------------------------------------------------
001600120817
001700120817     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800120911     h dftactgrp(*no)
001900121129     h bnddir('UBBNDDIR':'TRUL')
002000120817
002100120817       //--------------------------------------------------------------
002200120817       //?Dichiarazione file.                                          ?
002300120817       //--------------------------------------------------------------
002400120817
002500120817       // -?Organigramma?
002600120817     fAZORG01L  if   e           k disk
002700120817
002800120817       // -?Tabelle?
002900120911     fTNTBE01L  Uf   e           k disk    extfile(wLibFile)
003000120911     f                                     usropn
003100120817
003200120817       // -?Video?
003300120911     fTNTB79D2  cf   e             workstn
003400120817     f                                     sfile(TB79S01 : S01nrr)
003500120817     f                                     indds(IndDspF)
003600120817     f                                     infds(InfDspF)
003700120817
003800120817       //--------------------------------------------------------------
003900120817       //?Definizione costanti.                                        ?
004000120817       //--------------------------------------------------------------
004100120817
004200120817       // -?Codice tabella in gestione?
004300120817     d c_Tab           c                   const('3EW')
004400120817
004500120817       // -?Numero di record in ciascuna videata di subfile?
004600120817     d c_SflPag        c                   const(15)
004700120817
004800120817       // -?Numero massimo di record gestibili?
004900120817     d c_MaxRec        c                   const(9999)
005000120912
005100120912       // -?Costante per controllo "caratteri solo numerici"?
005200120912     d c_Digits        c                   const('0123456789')
005300120912
005400120912       // -?Attributi di visualizzazione?
005500120912     d c_DA_Norm       c                   const(x'A0')
005600120912     d c_DA_RI         c                   const(x'A1')
005700120912     d c_DA_BL         c                   const(x'A8')
005800120912     d c_DA_RI_BL      c                   const(x'A9')
005900120817
006000120817       // -?Tasti funzionali a video?
006100120817     d c_F01           c                   const(x'31')
006200120817     d c_F02           c                   const(x'32')
006300120817     d c_F03           c                   const(x'33')
006400120817     d c_F04           c                   const(x'34')
006500120817     d c_F05           c                   const(x'35')
006600120817     d c_F06           c                   const(x'36')
006700120817     d c_F07           c                   const(x'37')
006800120817     d c_F08           c                   const(x'38')
006900120817     d c_F09           c                   const(x'39')
007000120817     d c_F10           c                   const(x'3A')
007100120817     d c_F11           c                   const(x'3B')
007200120817     d c_F12           c                   const(x'3C')
007300120817     d c_F13           c                   const(x'B1')
007400120817     d c_F14           c                   const(x'B2')
007500120817     d c_F15           c                   const(x'B3')
007600120817     d c_F16           c                   const(x'B4')
007700120817     d c_F17           c                   const(x'B5')
007800120817     d c_F18           c                   const(x'B6')
007900120817     d c_F19           c                   const(x'B7')
008000120817     d c_F20           c                   const(x'B8')
008100120817     d c_F21           c                   const(x'B9')
008200120817     d c_F22           c                   const(x'BA')
008300120817     d c_F23           c                   const(x'BB')
008400120817     d c_F24           c                   const(x'BC')
008500120817     d c_Enter         c                   const(x'F1')
008600120817     d c_RollDown      c                   const(x'F4')
008700120817     d c_RollUp        c                   const(x'F5')
008800120911
008900120911       // -?Costanti per la definizione delle schiere con i nomi?
009000120911       //  ?degli iSeries da elaborare e delle relative librerie?
009100120911     d c_NrSyst        c                   const(2)
009200120911     d c_NrLibr        c                   const(2)
009300120817
009400120817       //--------------------------------------------------------------
009500120817       //?Definizione schiere.                                         ?
009600120817       //--------------------------------------------------------------
009700120911
009800120911       // -?iSeries  &  Librerie con entrambi i file tabelle?
009900120911     d $iSystem        s                   like(currSysNetA)
010000120911     d                                     dim(c_NrSyst)
010100120911     d                                     ctdata   perrcd( 1)
010200120911     d $Libraries      s                   like(ds_Libr)
010300120911     d                                     dim(c_NrSyst)
010400120911     d                                     alt($iSystem)
010500120817
010600120817       // -?Messaggi di errore?
010700120912     d $Msg            s             78    dim( 6)  ctdata  perrcd(1)
010800120817
010900120817       //--------------------------------------------------------------
011000120817       //?Definizione aree dati.                                       ?
011100120817       //--------------------------------------------------------------
011200120817
011300120817       // -?Dati utente?
011400120817     d §AzUte        e ds                  extname(AZUTE00F)
011500120817     d                                     dtaara
011600120817     d §DatiUte      e ds                  extname(dDatiUte)
011700120817     d                                     dtaara
011800120817
011900120817       //--------------------------------------------------------------
012000120817       //?Definizione strutture dati.                                  ?
012100120817       //--------------------------------------------------------------
012200120817
012300120817       // -?Status ds?
012400120817     d Status         sds
012500120817     d   SDSpgm          *proc
012600120817
012700120817       // -?InfDS?
012800120817     d InfDspF         ds
012900120817     d   dsp_aid             369    369a
013000120912     d*//dsp_nrg             370    370a
013100120912     d*//dsp_ncl             371    371a
013200120817     d   sfl_rrn             376    377i 0
013300120817     d   min_nrr             378    379i 0
013400120817     d   num_rcds            380    381i 0
013500120912
013600120912      *// // -?Posizione cursore?
013700120912     d*// Cursor          ds
013800120912     d*//  RiRi                   1      2b 0 inz
013900120912     d*//  R$$                    2      2
014000120912     d*//  CoCo                   3      4b 0 inz
014100120912     d*//  C$$                    4      4
014200120817
014300120817       // -?Indicatori su DspF?
014400120817     d IndDspF         ds                  inz
014500120817         // -?Abilitazione tasti funzionali?
014600120911     d   F3Attivo                      n   overlay(IndDspF : 03)
014700121129     d   F9Attivo                      n   overlay(IndDspF : 09)
014800120911     d   F12Attivo                     n   overlay(IndDspF : 12)
014900120817         // -?Emissione messaggio di errore?
015000120817     d   ErrMessage                    n   overlay(IndDspF : 28)
015100120817         // -?Indicatori di gestione del subfile?
015200120817     d   SflDsp_N                      n   overlay(IndDspF : 30)
015300120817     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
015400120817     d   SflNxtChg                     n   overlay(IndDspF : 32)
015500120817     d   SflEnd                        n   overlay(IndDspF : 33)
015600120817         // -?Indicatori per Attibuti di visualizzazione?
015700120817         //  ?o   Condizionamento visualizzazione campi?
015800120817     d   No_Posiz                      n   overlay(IndDspF : 40)
015900120817     d   Ord_x_SUN                     n   overlay(IndDspF : 41)
016000120911     d   Ord_x_DAA                     n   overlay(IndDspF : 42)
016100120817         // -?Posizionamento cursore & segnalazione errore?
016200120817     d   PosCurOPZ                     n   overlay(IndDspF : 50)
016300120911     d   PosCurKSU                     n   overlay(IndDspF : 51)
016400120911     d   PosCurFLS                     n   overlay(IndDspF : 52)
016500120911     d   PosCurDAA                     n   overlay(IndDspF : 53)
016600120817         //   -?Riemissione videata?
016700120817     d   ErrGenerico                   n   overlay(IndDspF : 99)
016800120911
016900120911       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
017000120911     d ds_Libr         ds                  inz
017100120911     d  $Libr                        10    dim(c_NrLibr) inz
017200120817
017300120817       // -?Parametri ricevuti?
017400120817     d KPJBA         e ds
017500120817
017600120817       // -?Tab. 3EW = Dati assegnati alla "postazione" in abilitazione?
017700120817       //             ?ad EasyWeb?
017800120817     d d3EW          e ds                  inz
017900121129       // -?Tab. 3C  = Clienti con dischetto (che inviano bolle in partenza)?
018000121129     d ds3C          e ds                  inz  qualified
018100120911
018200120911       // -?Dati record principale della tabella?
018300120911     d TNTBEds       e ds                  inz  extname(TNTBE00F)
018400120911     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
018500120911     d                                          prefix(TBX:3)
018600120817
018700120817       //--------------------------------------------------------------
018800120817       //?Definizione variabili globali.                               ?
018900120817       //--------------------------------------------------------------
019000120817
019100120817       // -?Flags booleani?
019200120817     d $Fine           s               n   inz
019300120817     d $EoF            s               n   inz
019400120817     d $InzD01         s               n   inz(*on)
019500120817     d $InzS01         s               n   inz(*on)
019600120817     d $RecOK          s               n   inz
019700120912     d $Conferma       s               n   inz
019800120817
019900120817       // -?Variabili per la gestione del video?
020000120912     d $Video          s              2    inz('D1')
020100120911     d*// W_SflPag        s              3  0 inz
020200120920     d wPag            s              4  0 inz
020300120920     d wRig            s              3  0 inz
020400120817     d S01nrr          s                   like(C1RcdNbr) inz
020500120817     d SavS01csr       s                   like(C1RcdNbr) inz
020600120912     d wDspAid_W1      s                   like(dsp_aid)  inz
020700120912     d W1rig           s              3  0 inz(10)
020800120912     d W1col           s              3  0 inz(02)
020900120817
021000120817       // -?Variabili per la gestione del singolo campo alla variazione?
021100120817       //  ?dello stesso?
021200120817     d SavC1Cksu       s                   like(C1Cksu)   inz(*hival)
021300120817     d SavC1Csun       s                   like(C1Csun)   inz(*hival)
021400120817     d SavC1Cdaa       s                   like(C1Cdaa)   inz(*hival)
021500120911
021600120911       // -?Nome esteso Libreria/File dei 2 file tabella?
021700120911     d wLibFile        s             21a   inz
021800120911
021900120911       // -?Variabili di comodo?
022000120911     d w_iSystem       s              1  0 inz
022100120911     d w_SisInf        s              3  0 inz
022200120912     d wCountOpz4      s              5  0 inz
022300120911     d wDate_Eur       s               d   datfmt(*eur)   inz
022400120911     d W1Cdaa          s                   like(§3EWdaa)  inz
022500120912     d W2Cdaa          s                   like(§3EWdaa)  inz
022600120817
022700120817       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
022800120817
022900120817       //--------------------------------------------------------------
023000120817       // -?Constants?
023100120817       //--------------------------------------------------------------
023200120817       // -?MaxKey - Maximum number of key fields allowed by this program?
023300120817     d MaxKey          c                   const(2)
023400120817       // -?Sort order:?
023500120817       //  ?1 = Sort in an ascending sequence?
023600120817       //  ?2 = Sort in a descending sequence?
023700120817     d Ascendente      c                   const(1)
023800120817     d Discendente     c                   const(2)
023900120817       // -?Key data type:?
024000120817       //  ? 0 = Signed binary?
024100120817       //  ? 2 = Signed zoned decimal?
024200120817       //  ? 3 = Signed packed decimal?
024300120817       //  ? 6 = Character with no national language sort sequence applied,?
024400120817       //  ?     if specified?
024500120817       //  ? 7 = Unsigned packed decimal?
024600120817       //  ?     All numbers will have the sign forced positive ('F'X).?
024700120817       //  ? 8 = Unsigned zoned decimal?
024800120817       //  ?     All numbers will have the sign forced positive ('F'X).?
024900120817       //  ? 9 = Unsigned binary?
025000120817       //  ?14 = Date in form of DD/MM/YY?
025100120817       //  ?15 = Date in form of DD.MM.YYYY?
025200120817     d Numero          c                   const(2)
025300120817     d Carattere       c                   const(6)
025400120817       //
025500120817     d Put             c                   const(1)
025600120817     d EndPut          c                   const(2)
025700120817     d Get             c                   const(3)
025800120817
025900120817       //--------------------------------------------------------------
026000120817       // -?Data Structures?
026100120911       //--------------------------------------------------------------
026200120911       //  ?SflRcd     - The entire subfile record to pass to the sort?
026300120817     d SflRcd          ds                  inz
026400120817     d   S1Cksu                            inz
026500120817     d   S1Dksu                            inz
026600120817     d   S1Csun                            inz
026700120817     d   H1Cdaa                            inz
026800120817     d   S1Copz                            inz
026900121129     d   S1Clnp                            inz
027000120817     d   S1Cfls                            inz
027100120817     d   S1Cnrs                            inz
027200120817     d   S1Cnsi                            inz
027300120817     d   S1Cnsf                            inz
027400120817     d   S1Cdaa                            inz
027500121129     d   S1Ccba                            inz
027600121129     d   S1Cf3C                            inz
027700120817     d   Selected                     1a   inz
027800120911       //  ?QLGSCB     - The sort request block for the QLGSORT API?
027900120817      /copy QSYSINC/QRPGLESRC,QLGSORT
028000120911       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
028100120817     d QLGKL                         16    dim(MaxKey)
028200120817     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
028300120817     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
028400120817     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
028500120817     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
028600120911       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
028700120817      /copy QSYSINC/QRPGLESRC,QLGSRTIO
028800120911       //  ?QUSEC      - Error structure for the QLGSORT API?
028900120817      /copy QSYSINC/QRPGLESRC,QUSEC
029000120817
029100120817       //--------------------------------------------------------------
029200120817       // -?Standalone fields?
029300120817       //--------------------------------------------------------------
029400120817     d NotUsed         s             16a   inz
029500120911       //  ?ReturnSize - Size of list returned by sort API?
029600120817     d ReturnSize      s              9b 0 inz
029700120911       //  ?SizeList   - Size of list?
029800120817     d SizeList        s              9b 0 inz
029900120911       //  ?Nrr        - Relative record number for subfile?
030000120817     d Nrr             s              5i 0 inz
030100120911     d RrnLast         s              5i 0 inz
030200120817
030300120817       //--------------------------------------------------------------
030400120817       //?Definizione prototipi procedure.                             ?
030500120817       //--------------------------------------------------------------
030600120911
030700120911       // -?Reperimento NETA sistema AS/400 corrente?
030800120911     d currSysNeta     s              8a   inz
030900120911      /copy gaitrasrc/srcProtoPR,UBRTVNETA
031000120817
031100120817       // -?Reperimento dati utente?
031200120817     d TIBS34ds      e ds                  inz
031300120817      /copy gaitrasrc/srcProtoPR,TIBS34R
031400120817
031500120817       // -?Selezione cliente da tab. "3EW"?
031600120911     d TNTB79ds      e ds                  inz   qualified
031700120817      /copy gaitrasrc/srcProtoPR,TNTB79R1
031800121129
031900121129       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
032000121129     d c_TABEL         c                   const('1')
032100121129       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
032200121129     d c_TNTBE         c                   const('2')
032300121129       // -?Reperimento dati tabelle?
032400121129      /copy gaitrasrc/srcProtoPR,TRULTAB
032500121129
032600121129       // -?Reperimento legami in tab. "3C"?
032700121129      /copy gaitrasrc/srcProtoPI,UBLEG3C
032800121129      /copy gaitrasrc/srcProtoPR,UBLEG3C
032900120817
033000120817       // -?Controllo/Decodifica cliente?
033100120817      /copy gaitrasrc/srcProtoPI,TIBS69R
033200120817      /copy gaitrasrc/srcProtoPR,TIBS69R
033300120817
033400120817       // -?Controllo/Inversione data?
033500120817     d WLBdat          ds                  inz
033600120817     d  G08dat                        8  0 inz
033700120817     d  G08inv                        8  0 inz
033800120817     d  G08err                        1    inz('3')
033900120817     d  G08tgi                        5  0 inz
034000120817      /copy gaitrasrc/srcProtoPR,XSRDA8
034100120817
034200120911       // -?API Ordinamento subfile?
034300120817      /copy gaitrasrc/srcProtoPr,QLGSRTIO
034400120817
034500120817       //--------------------------------------------------------------
034600120817       //?Definizione key-list.                                        ?
034700120817       //--------------------------------------------------------------
034800120817
034900120817       // -?File TNTBE01L?
035000120817     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
035100120817     d                                     prefix(k_)   inz
035200120817
035300120817       //--------------------------------------------------------------
035400120817       //?Riepilogo indicatori utilizzati.                             ?
035500120817       //--------------------------------------------------------------
035600120817
035700120817
035800120817       //--------------------------------------------------------------
035900120817       //?M A I N - L I N E                                            ?
036000120817       //--------------------------------------------------------------
036100120817
036200120817     c     *Entry        plist
036300120817     c                   parm                    KPJBA
036400120817
036500120817      /free
036600120817
036700120817       // -?Operazioni iniziali?
036800120817       exsr sr_RoutInz;
036900120817
037000120817       // -?Ciclo di gestione del file video?
037100120817       DoW  $Fine = *off;
037200120817         select;
037300120817           // -?Gestione videata D1 (filtro)?
037400120817           when $Video = 'D1';
037500120817             exsr sr_GesD01;
037600120817           // -?Gestione videata S1 (subfile)?
037700120817           when $Video = 'S1';
037800120817             exsr sr_GesS01;
037900120817           // -?? ? ??
038000120817           other;
038100120817             $Fine = *on;
038200120817         endsl;
038300120817       enddo;
038400120817
038500120817       // -?Operazioni finali?
038600120817       exsr sr_RoutEnd;
038700120817
038800120817       //--------------------------------------------------------------
038900120817       //?*InzSR.                                                      ?
039000120817       //--------------------------------------------------------------
039100120817       BEGSR  *InzSr;
039200120817
039300120817         // -?Reperimento dati job?
039400120817         exsr  sr_DatiJob;
039500120817
039600120817         // -?Impostazione nome programma a video?
039700120817         V1Tpgm = SDSpgm;
039800120817
039900120817       ENDSR;
040000120817
040100120817       //--------------------------------------------------------------
040200120817       //?Reperimento Dati del job (Utente/Operativi).                 ?
040300120817       //--------------------------------------------------------------
040400120817       BEGSR  sr_DatiJob;
040500120817
040600120817         in(e) §AzUte;
040700120817         if NOT %error;
040800120817           in(e) §DatiUte;
040900120817         endif;
041000120817         if %error or RSut = *blank;
041100120817           tibs34r ( tibs34ds );
041200120817           in §AzUte;
041300120817           in §DatiUte;
041400120817         endif;
041500120817
041600120817       ENDSR;
041700120817
041800120817       //--------------------------------------------------------------
041900120817       //?Operazioni iniziali.                                         ?
042000120817       //--------------------------------------------------------------
042100120817       BEGSR  sr_RoutInz;
042200120817
042300120817         *inLR = *on;
042400120911
042500120911         // -?Verifica del sistema AS/400 corrente?
042600120911         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
042700120911           $Fine = *on;
042800120911           leavesr;
042900120911         endif;
043000120911
043100120911         // -?Impostazione elenco librerie in cui gestire le tabelle?
043200120911         //  ?(a seconda del sistema in cui si stà lavorando)?
043300120911         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
043400120911         if  w_iSystem = *zero;
043500120911           $Fine = *on;
043600120911           leavesr;
043700120911         endif;
043800120911
043900120911         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
044000120911         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
044100120911           w_SisInf = 1;
044200120911           exsr  sr_OpenFileTab;
044300120911         endif;
044400120911
044500120911         // -?Aggancio dati generali della tabella in esame?
044600120911         clear  k_TBEcod;
044700120911         k_TBEke1 = *zero;
044800120911         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
044900120911                = c_Tab;
045000120911         clear  k_TBEke2;
045100120911         clear  k_TBElin;
045200120911         k_TBEsif = KNSIF;
045300120911         chain(n)  %kds(k05tntbe01)  TNTBE000;
045400120911         if  not  %found(TNTBE01L);
045500120911           clear  k_TBEsif;
045600120911           chain(n)  %kds(k05tntbe01)  TNTBE000;
045700120911         endif;
045800120911         if  %found(TNTBE01L);
045900120911           xTNTBEds = TNTBEds;
046000120911         else;
046100120911           clear  xTNTBEds;
046200120911         endif;
046300120817
046400120817         // -?Impostazione iniziale / pulizia dei campi chiave?
046500120817         clear  k05tntbe01;
046600120817         k_TBEcod = c_Tab;
046700120911         Ord_x_SUN = *off;
046800120911         Ord_x_DAA = *off;
046900120817
047000120817       ENDSR;
047100120817
047200120817       //--------------------------------------------------------------
047300120817       //?Gestione videata D01.                                        ?
047400120817       //--------------------------------------------------------------
047500120817       BEGSR  sr_GesD01;
047600120817
047700120817         // -?Inizializzazione videata?
047800120817         if  $InzD01 = *on;
047900120817           exsr  sr_InzD01;
048000120817           $InzD01 = *off;
048100120817         endif;
048200120817
048300120817         // -?Emissione Testata?
048400120817         write  TB79T01;
048500120817
048600120817         // -?Emissione videata?
048700120817         exfmt  TB79D01;
048800120817
048900120817         reset  ErrMessage;
049000120817         reset  ErrGenerico;
049100120817         clear  V1Dmsg;
049200120817
049300120817         select;
049400120817
049500120817           // -?F3=Fine?
049600120817           when  dsp_aid = c_F03;
049700120817             $Fine = *on;
049800120817
049900120817           // -?Invio?
050000120817           other;
050100120817             exsr  sr_CtrD01;
050200120817             if  not  ErrGenerico;
050300120817               $Video = 'S1';
050400120817               reset  $InzS01;
050500120817             endif;
050600120817
050700120817         endsl;
050800120817
050900120817       ENDSR;
051000120817
051100120817       //--------------------------------------------------------------
051200120817       //?Inizializzazione videata D01.                                ?
051300120817       //--------------------------------------------------------------
051400120817       BEGSR  sr_InzD01;
051500120817
051600120817         // -?Pulizia videata?
051700120817         clear  TB79D01;
051800120911
051900120911         // -?Abilitazione tasti funzionali?
052000120911         F3Attivo  = *on;
052100120911         F12Attivo = *off;
052200120817
052300120817       ENDSR;
052400120817
052500120817       //--------------------------------------------------------------
052600120817       //?Controllo dati immessi nella videata D01.                    ?
052700120817       //--------------------------------------------------------------
052800120817       BEGSR  sr_CtrD01;
052900120817
053000120817         // -?Spegnimento degli indicatori di errore?
053100120817         %subst(IndDspF : 50) = *off;
053200120912
053300120912         clear  V1Dksu;
053400120912         if  V1Cksu = *zero;
053500120912           clear  V1Cksu;
053600120912         endif;
053700120817
053800120912         SELECT;
053900120912
054000120912           // -?Ricerca dati già inseriti?
054100120912           WHEN  %scan('?' : V1Cksu) > *zero;
054200120912             clear  V1Cksu;
054300120912             clear  TNTB79ds;
054400120912             tntb79r1 ( kpjba : TNTB79ds );
054500120912             if  TNTB79ds.o3EWfxx = *blank;
054600120912               V1Cksu = TNTB79ds.o3EWksu;
054700120912             endif;
054800120912             errGenerico = *on;
054900120912             PosCurKSU   = *on;
055000120912             leavesr;
055100120912
055200120912           // -?Controllo immisione solo caratteri numerici (come KSU)?
055300120912           WHEN  V1Cksu <> *blank   and
055400120912                 %check( c_Digits : V1Cksu ) > *zero;
055500120912             V1Dmsg = $Msg(01);
055600120912             PosCurKSU   = *on;
055700120912             ErrMessage  = *on;
055800120912             ErrGenerico = *on;
055900120912             leavesr;
056000120817
056100120912           // -?Controllo/Decodifica cliente (KSU)?
056200120912           WHEN  V1Cksu > *zero;
056300120912             clear  TIBS69ds;
056400120912             I69sif = knsif;
056500120912             I69kcc = DUTkci;
056600120912             I69kac = %int( %subst( V1Cksu : 2 ) );
056700120912             tibs69r( tibs69ds :
056800120912                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
056900120912             if  O69err = *on;
057000120912               V1Dmsg = $Msg(01);
057100120912               PosCurKSU   = *on;
057200120912               ErrMessage  = *on;
057300120912               ErrGenerico = *on;
057400120912               leavesr;
057500120912             endif;
057600120912             V1Dksu = %subst( ACOrag : 1 : %len(V1Dksu) );
057700120912
057800120912         ENDSL;
057900120817
058000120817         // -?Controllo/Decodifica filiale segnacollo (FLS)?
058100120817         clear  V1Dfls;
058200120912         IF  V1Cfls <> *zero;
058300120817           chain  (V1Cfls)  AZORG;
058400120911           if  Not %found(AZORG01L)  or  ORGfva <> *blank;
058500120817             V1Dmsg = $Msg(02);
058600120817             PosCurFLS   = *on;
058700120817             ErrMessage  = *on;
058800120817             ErrGenerico = *on;
058900120817             leavesr;
059000120817           endif;
059100120911           V1Dfls = ORGdes;
059200120817         ENDIF;
059300120817
059400120817         // -?Controllo data annullamento limite (DAA)?
059500120911         clear  W1Cdaa;
059600120817         IF  V1Cdaa > *zero;
059700120817           clear WLBdat;
059800120817           G08dat = V1Cdaa;
059900120817           XSRDA8(WLBdat);
060000120817           if  G08err = *on;
060100120817             ErrGenerico = *on;
060200120817             ErrMessage  = *on;
060300120817             PosCurDAA   = *on;
060400120911             V1Dmsg = $Msg(03);
060500120817             leavesr;
060600120911           endif;
060700120911           V1Cdaa = G08dat;
060800120911           W1Cdaa = %editc( G08inv : 'X' );
060900120817         ENDIF;
061000120817
061100120817       ENDSR;
061200120817
061300120817       //--------------------------------------------------------------
061400120817       //?Gestione subfile S01.                                        ?
061500120817       //--------------------------------------------------------------
061600120817       BEGSR  sr_GesS01;
061700120817
061800120817         // -?Inizializzazione videata?
061900120817         if  $InzS01 = *on;
062000120817           exsr  sr_InzS01;
062100120817           $InzS01 = *off;
062200120817         endif;
062300120817
062400120817         // -?Emissione Testata & Piede?
062500120817         write  TB79T01;
062600120817         write  TB79P01;
062700120817
062800120817         // -?Posizionamento cursore?
062900120817         if  C1CsrRrn > *zero;
063000120817           C1RcdNbr = C1CsrRrn;
063100120817         else;
063200120817           C1RcdNbr = 1;
063300120817           write  TB79S00;             //?(no rec.)?
063400120817         endif;
063500120817
063600120817         // -?Emissione videata?
063700120817         exfmt  TB79C01;
063800120817
063900120817         reset  ErrMessage;
064000120817         reset  ErrGenerico;
064100120817         clear  V1Dmsg;
064200120817
064300120817         select;
064400120817
064500120817           // -?F3=Fine?
064600120817           when  dsp_aid = c_F03;
064700120817             $Fine = *on;
064800120817
064900120817           // -?F5=Rivisualizza?
065000120817           when  dsp_aid = c_F05;
065100120817             $InzS01 = *on;
065200120817
065300121217           // -?F8=Ordinamento?
065400121217           when  dsp_aid = c_F08;
065500121217             exsr  sr_F08S01;
065600120817
065700120817           // -?F12=Ritorno?
065800120817           when  dsp_aid = c_F12;
065900120817             $Video = 'D1';
066000120817
066100120912           // -?SubFile vuoto?
066200120912           when  S01nrr = *zero;
066300120912             // -?Richiesto posizionamento?
066400120912             if  (Not Ord_x_SUN   and   Not Ord_x_DAA   and
066500120912                                        C1Cksu <> SavC1Cksu)   OR
066600120912                 (Ord_x_SUN       and   C1Csun <> SavC1Csun)   OR
066700120912                 (Ord_x_DAA       and   C1Cdaa <> SavC1Cdaa);
066800120912               $InzS01 = *on;
066900120912             endif;
067000120817
067100120817           // -?Invio?
067200120817           other;
067300120817             // -?Gestione opzioni?
067400120817             exsr  sr_OpzS01;
067500120817             if  ErrGenerico;
067600120817               leavesr;
067700120817             endif;
067800120817             // -?Richiesto posizionamento?
067900120911             if  (Not Ord_x_SUN   and   Not Ord_x_DAA   and
068000120817                                        C1Cksu <> SavC1Cksu)   OR
068100120817                 (Ord_x_SUN       and   C1Csun <> SavC1Csun)   OR
068200120911                 (Ord_x_DAA       and   C1Cdaa <> SavC1Cdaa);
068300120817               $InzS01 = *on;
068400120817             endif;
068500120817
068600120817         endsl;
068700120817
068800120817       ENDSR;
068900120817
069000120817       //--------------------------------------------------------------
069100120817       //?Inizializzazione subfile S01.                                ?
069200120817       //--------------------------------------------------------------
069300120817       BEGSR  sr_InzS01;
069400120817
069500120817         // -?Spegnimento degli indicatori di errore?
069600120817         %subst(IndDspF : 50) = *off;
069700120911
069800120911         // -?Abilitazione tasti funzionali?
069900120911         F3Attivo  = *on;
070000120911         F12Attivo = *on;
070100120817
070200120817         // -?Pulizia subfile control?
070300120817         select;
070400120817           when  Ord_x_SUN;
070500120817             clear  C1Cksu;
070600120817             clear  C1Cdaa;
070700120817             reset  SavC1Cksu;
070800120817             reset  SavC1Cdaa;
070900120911           when  Ord_x_DAA;
071000120817             clear  C1Cksu;
071100120817             clear  C1Csun;
071200120817             reset  SavC1Cksu;
071300120817             reset  SavC1Csun;
071400120817           when  Not Ord_x_SUN  and  Not Ord_x_DAA;
071500120817             clear  C1Csun;
071600120817             clear  C1Cdaa;
071700120817             reset  SavC1Csun;
071800120817             reset  SavC1Cdaa;
071900120911         endsl;
072000120912         if  C1Cksu = *zero;
072100120912           clear  C1Cksu;
072200120912         endif;
072300120912         if  C1Csun = *zero;
072400120912           clear  C1Csun;
072500120912         endif;
072600120912
072700120912         // -?Controllo data annullamento limite (DAA)?
072800120912         clear  W2Cdaa;
072900120912         IF  C1Cdaa > *zero;
073000120912           clear WLBdat;
073100120912           G08dat = C1Cdaa;
073200120912           XSRDA8(WLBdat);
073300120912           if  G08err = *on;
073400120912             ErrGenerico = *on;
073500120912             ErrMessage  = *on;
073600120912             PosCurDAA   = *on;
073700120912             V1Dmsg = $Msg(06);
073800120912             leavesr;
073900120912           endif;
074000120912           C1Cdaa = G08dat;
074100120912           W2Cdaa = %editc( G08inv : 'X' );
074200120912         ENDIF;
074300120817
074400120817         // -?Pulizia subfile?
074500120817         SflDsp_N    = *on;
074600120817         SflDspCtl_N = *on;
074700120817         write  TB79C01;
074800120817         SflDspCtl_N = *off;
074900120817         SflEnd      = *off;
075000120817
075100120817         clear  C1RcdNbr;
075200120817         clear  C1CsrRrn;
075300120817         clear  S01nrr;
075400120817         clear  V1Dmsg;
075500120817         ErrMessage  = *off;
075600120817         ErrGenerico = *off;
075700120911
075800120911         // -?Apertura file TABEL00F del 1° S.I. (sede)?
075900120911         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
076000120911           w_SisInf = 1;
076100120911           exsr  sr_OpenFileTab;
076200120911         endif;
076300120817
076400120817         // -?Caricamento completo dei dati nel subfile?
076500120817         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
076600120817         //  ?l'eventuale ordinamento)?
076700120920         clear  k05tntbe01;
076800120920         k_TBEcod = c_Tab;
076900120920         k_TBEke1 = V1Cksu;
077000120817         setll  %kds(k05tntbe01)  TNTBE000;
077100120911         reade(N)  %kds(k05tntbe01 : 1)  TNTBE000;
077200120817         $EoF = (%eof(TNTBE01L));
077300120817         exsr  sr_CaricaS01;
077400120817
077500120817         // -?Impaginazione subfile?
077600120817         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
077700120817         if S01nrr > *zero;
077800120817           C1RcdNbr  = 1;
077900120817           C1CsrRrn  = 1;
078000120817         else;
078100120817           clear C1RcdNbr;
078200120817         endif;
078300120817
078400120817         // -?Impostazione indicatore x la gestione del posizionamento?
078500121217         //  ?nel subfile (e abilitazione F8)?
078600120912         No_Posiz = (S01nrr = *zero   and
078700120912                     C1Cksu = *blank  and
078800120912                     C1Csun = *blank  and
078900120912                     C1Cdaa = *zero);
079000121129
079100121129         // -?(Dis)Abilitazione tasti funzionali?
079200121129         F9Attivo  = (S01nrr > *zero);
079300120817
079400121217         // -?Ordinamento subfile (se premuto F8)?
079500120817         if  S01nrr > *zero  and
079600120817            (Ord_x_SUN   or   Ord_x_DAA);
079700120817           exsr  sr_SortSfl;
079800120817         endif;
079900120817
080000120817       ENDSR;
080100120817
080200120817       //--------------------------------------------------------------
080300120817       //?Caricamento completo del subfile S01                         ?
080400120817       //--------------------------------------------------------------
080500120817       BEGSR  sr_CaricaS01;
080600120817
080700120817         clear  S01nrr;
080800121205         clear  P1NTrec;
080900121205         clear  P1NTsc;
081000120817         SflNxtChg = *off;
081100120911
081200120817
081300120817         // -?Ciclo di caricamento dati da tab. "3EW"?
081400120817         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
081500120817         //  ?l'eventuale ordinamento)?
081600120911         DOW  Not $EoF;
081700120817
081800120817           // -?Selezione singolo record file TNTBE00F?
081900120817           exsr  sr_SelezRec;
082000120817           if  $EoF;
082100120817             leave;
082200120817           endif;
082300120817
082400120817           // -?Registrazione singolo record nel subfile?
082500120911           If  $RecOK;
082600120817
082700120817             // ·?Impostazione campi?
082800120817             clear  TB79S01;
082900120817             d3EW = TBEuni;
083000120817             H1Cdaa = §3EWdaa;
083100120817             S1Cksu = TBEke1;
083200120817             S1Csun = TBEke2;
083300120817             S1Clnp = §3EWlnp;
083400120817             S1Cfls = §3EWfls;
083500120817             S1Cnrs = §3EWnrs;
083600120817             S1Cnsi = §3EWnsi;
083700120817             S1Cnsf = §3EWnsf;
083800120817             if  §3EWdaa > *zero;
083900120817               reset  WLBdat;
084000120911               G08inv = %int(§3EWdaa);
084100120817               XSRDA8(WLBdat);
084200120817               if  G08err = *off;
084300120911                 wDate_Eur = %date( G08dat : *eur );
084400120911                 S1Cdaa = ( %subdt( wDate_Eur : *days ) * 10000 ) +
084500120911                          ( %subdt( wDate_Eur : *months ) * 100 ) +
084600120911                          ( %subdt( wDate_Eur : *years ) - 2000 );
084700120817               endif;
084800120817             endif;
084900120817
085000120817             // ·?Decodifica cliente unificante?
085100120817             clear  TIBS69ds;
085200120817             I69kcc = DUTkci;
085300120817             I69kac = %int( %subst( S1Cksu : 2 ) );
085400120817             I69sif = knsif;
085500120817             tibs69r ( TIBS69ds :
085600120817                       ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
085700120817             if  O69err = *blank;
085800120817               S1Dksu = %subst(ACOrag : 1 : %len(S1Dksu));
085900120817             endif;
086000121129
086100121129             // ·?Reperimento tab. "3C"?
086200121129             clear  ds3C;
086300121129             ds_TNTBE.TBEke1 = %subst( S1Cksu : 2 );
086400121129             if  getTabella ( c_Tabel : '3C'  : ds_TNTBE.TBEke1 :
086500121129                              *omit : *omit : *omit :
086600121129                              *omit : *omit :
086700121129                              *omit : *omit : *omit : *omit :
086800121129                              *omit : *omit : *omit : *omit :
086900121129                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
087000121129                            = *zero      AND
087100121129                 ds_TNTBE.TBEatb = *blank;
087200121129               ds3C   = ds_TNTBE.TBEuni;
087300121129             else;
087400121129               ds3C.§3Ccba = *all'-';
087500121129             endif;
087600121129             S1Ccba = ds3C.§3Ccba;
087700121129
087800121129             // ·?Conteggio dei figli (se unificante ancora in "3C")?
087900121129             if  ds_TNTBE.TBEatb = *blank  and  ds3C.§3Crag <> *blank  and
088000121129                 ubLeg3C_Rtv ( %int( %subst( S1Cksu : 2 ) ) :
088100121129                               ubLeg3C_FlgPF :
088200121129                               ubLeg3C_Padre :
088300121129                               ubLeg3C_SKC :
088400121129                               ubLeg3C_SKEW ) >= *zero;
088500121129               S1Cf3C = ( %lookup( *zero : sch_SKC ) - 2 );
088600121129               if  S1Cf3C < *zero;
088700121129                 S1Cf3C = *hival;
088800121129               endif;
088900121129             endif;
089000120817
089100120817             // ·?Scrittura record nel subfile?
089200120817             S01nrr += 1;
089300120817             write  TB79S01;
089400121129
089500121129             // ·?Incremento Tot. Rec. rilevati?
089600121129             P1NTrec =  S01nrr;
089700121129             P1NTsc += (§3EWnsf - §3EWnsi + 1);
089800120817
089900120817           EndIf;
090000120817
090100120817           // -?Lettura record successivo?
090200120911           reade(N)  %kds(k05tntbe01 : 1)  TNTBE000;
090300120817           $EoF = (%eof(TNTBE01L));
090400120817
090500120911         ENDDO;
090600120817
090700120911
090800120817         // -?Memorizzazione posizionamento effettuato?
090900120817         select;
091000120817           when  Not Ord_x_SUN  and  Not Ord_x_DAA;
091100120817             SavC1Cksu = C1Cksu;
091200120817           when  Ord_x_SUN;
091300120817             SavC1Csun = C1Csun;
091400120817           when  Ord_x_DAA;
091500120817             SavC1Cdaa = C1Cdaa;
091600120817         endsl;
091700120817
091800120817         // -?Visualizzazione del SFL (se ci sono dati)?
091900120817         SflDsp_N = (S01nrr <= *zero);
092000120817
092100120817         // -?Attivazione del SFLEND?
092200120817         SflEnd = ( $EoF );
092300120817
092400120817         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
092500120817         if  S01nrr > *zero;
092600120817           C1RcdNbr = 1;
092700120817         else;
092800120817           clear  C1RcdNbr;
092900120817         endif;
093000120817
093100120817         C1CsrRrn = C1RcdNbr;
093200120817
093300120817       ENDSR;
093400120817
093500120817       //--------------------------------------------------------------
093600120817       //?Selezione del singolo record letto dalla tabella "3EW".      ?
093700120817       //--------------------------------------------------------------
093800120817       BEGSR  sr_SelezRec;
093900120817
094000120817         reset  $RecOK;
094100120817
094200120817         d3EW = TBEuni;
094300120817
094400120817         select;
094500120817
094600120817           // -?Omissione records annullati?
094700120817           when  TBEatb <> *blank;
094800120817             leavesr;
094900120817
095000120817           // -?Omissione records senza annullamento applicativo?
095100120817           when  §3EWfaa = *blank;
095200120817             leavesr;
095300120817
095400120817           // -?Selezione per cliente unificante?
095500120911           when  V1Cksu > *zero   and   V1Cksu <> %trimr( TBEke1 );
095600120817             $EoF = *on;
095700120817             leavesr;
095800120817
095900120817           // -?Selezione per filiale segnacollo?
096000120911           when  V1Cfls > *zero   and   V1Cfls <> §3EWfls;
096100120817             leavesr;
096200120817
096300120817           // -?Selezione per data annullamento?
096400120817           when  V1Cdaa > *zero   and   W1Cdaa < §3EWdaa;
096500120817             leavesr;
096600120817
096700120817           // -?Richiesto posizionamento per cliente unificante:?
096800120817           //  ?si scartano i clienti con unificante inferiore?
096900120817           when  Not Ord_x_SUN  and  Not Ord_x_DAA  and  TBEke1 < C1Cksu;
097000120817             leavesr;
097100120817
097200120817           // -?Richiesto posizionamento per Strategi User Number:?
097300120817           //  ?si scartano i clienti con S.U.N. inferiore?
097400120817           when  Ord_x_SUN   and   TBEke2 < C1Csun;
097500120817             leavesr;
097600120817
097700120817           // -?Richiesto posizionamento per data annullam. applicativo:?
097800120817           //  ?si scartano i clienti con data annullam. applic. inferiore?
097900120912           when  Ord_x_DAA   and   §3EWdaa < W2Cdaa;
098000120817             leavesr;
098100120817
098200120817         endsl;
098300120817
098400120817         $RecOK = *on;
098500120817
098600120817       ENDSR;
098700120817
098800120817       //--------------------------------------------------------------
098900121217       //?Gestione tasto funzionale F8 da videata C01 / S01            ?
099000121217       //?F8-Cambia ordinamento sfl                                    ?
099100120817       //--------------------------------------------------------------
099200121217       BEGSR  sr_F08S01;
099300120817
099400120912         select;
099500120912           when  Not Ord_x_SUN  and  Not Ord_x_DAA;
099600120912             Ord_x_SUN = *on;
099700120912             Ord_x_DAA = *off;
099800120912           when  Ord_x_SUN  and  Not Ord_x_DAA;
099900120912             Ord_x_SUN = *off;
100000120912             Ord_x_DAA = *on;
100100120912           when  Not Ord_x_SUN  and  Ord_x_DAA;
100200120912             Ord_x_SUN = *off;
100300120912             Ord_x_DAA = *off;
100400120912         endsl;
100500120817
100600120817         select;
100700120817           // -?Subfile già posizionato: occorre ricaricarlo?
100800120912           when  C1Cksu <> *blank   or   C1Csun <> *blank
100900120912                                    or   C1Cdaa <> *zero;
101000120817             $InzS01 = *on;
101100120912           // -?Subfile vuoto: nessun record da ordinare?
101200120912           when  S01nrr = *zero;
101300120817           // -?Altrimenti: basta riordinarlo?
101400120817           other;
101500120817             exsr  sr_SortSfl;
101600120817         endsl;
101700120817
101800120817         select;
101900120817           when  Not Ord_x_SUN  and  Not Ord_x_DAA;
102000120817             clear  C1Cksu;
102100120817             clear  C1Cdaa;
102200120817             clear  SavC1Cksu;
102300120817             clear  SavC1Cdaa;
102400120817           when  Ord_x_SUN;
102500120817             clear  C1Csun;
102600120817             clear  C1Cdaa;
102700120817             clear  SavC1Csun;
102800120817             clear  SavC1Cdaa;
102900120817           when  Ord_x_DAA;
103000120817             clear  C1Cksu;
103100120817             clear  C1Csun;
103200120817             clear  SavC1Cksu;
103300120817             clear  SavC1Csun;
103400120817         endsl;
103500120817
103600120817       ENDSR;
103700120817
103800120817       //--------------------------------------------------------------
103900120817       //?Ordinamento subfile                                          ?
104000120817       //?  This subroutine sorts the subfile records.                 ?
104100120817       //--------------------------------------------------------------
104200120817       BEGSR  sr_SortSfl;
104300120817
104400120817         RrnLast  = S01nrr;
104500120817
104600120817         C1RcdNbr = 1;
104700120817         clear  C1CsrRrn;
104800120817         clear  S01nrr;
104900120817         clear  V1Dmsg;
105000120817         ErrMessage  = *off;
105100120817         SflNxtChg   = *off;
105200120817         ErrGenerico = *off;
105300120817         %subst(IndDspF : 50) = *off;
105400120817
105500120817         //?___________________________________________________________?
105600120817         //?Initialize the key fields to sort on.?
105700120817         //?There is one extra field not in the subfile -- Selected --?
105800120817         //?that is added to the record. This field is used to place?
105900120817         //?selected records in front of nonselected records.?
106000120817         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
106100120817
106200120817         clear  QLGKL;
106300120817         clear  QLGSKL;
106400120817         clear  QLGSCB;
106500120817         clear  QLGSCB00;
106600120817
106700120817         // -?Gestione della scelta ordinamento:?
106800120817         Select;
106900120817
107000120817           //  ?Ordinamento per Strategi User Number?
107100120817           When  Ord_x_SUN = *on;
107200120817             // -?2 campi chiave?
107300120817             QLGNBRK  = 2;
107400120817             // -?1° campo: strategi user number (S1Csun)?
107500120817             //            ?a posizone   29,  9 byte, char, ascending.?
107600120817             QLGSP    = 29;
107700120817             QLGSS    = %size(S1Csun);
107800120817             QLGDT    = Carattere;
107900120817             QLGSO    = Ascendente;
108000120817             QLGKL(1) = QLGSKL;
108100120817             // -?2° campo: cliente unificante (S1Cksu)?
108200120817             //            ?a posizone    1,  8 byte, char, ascending.?
108300120817             QLGSP    = 1;
108400120817             QLGSS    = %size(S1Cksu);
108500120817             QLGDT    = Carattere;
108600120817             QLGSO    = Ascendente;
108700120817             QLGKL(2) = QLGSKL;
108800120817
108900120817           //  ?Ordinamento per Data annullamento applicativo?
109000120817           When  Ord_x_DAA = *on;
109100120817             // -?2 campi chiave?
109200120817             QLGNBRK  = 2;
109300120817             // -?1° campo: data annullamento applicativo (H1Cdaa)?
109400120817             //            ?a posizone   38,  8 byte, char, ascending.?
109500120817             QLGSP    = 38;
109600120817             QLGSS    = %size(H1Cdaa);
109700120817             QLGDT    = Carattere;
109800120817             QLGSO    = Ascendente;
109900120817             QLGKL(1) = QLGSKL;
110000120817             // -?2° campo: cliente unificante (S1Cksu)?
110100120817             //            ?a posizone    1,  8 byte, char, ascending.?
110200120817             QLGSP    = 1;
110300120817             QLGSS    = %size(S1Cksu);
110400120817             QLGDT    = Carattere;
110500120817             QLGSO    = Ascendente;
110600120817             QLGKL(2) = QLGSKL;
110700120817
110800120817           // -?Ordinamento per Cliente Unificante?
110900120817           When  Not Ord_x_SUN   and  Not Ord_x_DAA;
111000120817             // -?2 campi chiave?
111100120817             QLGNBRK  = 2;
111200120817             // -?1° campo: cliente unificante (S1Cksu)?
111300120817             //            ?a posizone    1,  8 byte, char, ascending.?
111400120817             QLGSP    = 1;
111500120817             QLGSS    = %size(S1Cksu);
111600120817             QLGDT    = Carattere;
111700120817             QLGSO    = Ascendente;
111800120817             QLGKL(1) = QLGSKL;
111900120817             // -?2° campo: strategi user number (S1Csun)?
112000120817             //            ?a posizone   29,  9 byte, char, ascending.?
112100120817             QLGSP    = 29;
112200120817             QLGSS    = %size(S1Csun);
112300120817             QLGDT    = Carattere;
112400120817             QLGSO    = Ascendente;
112500120817             QLGKL(2) = QLGSKL;
112600120817
112700120817         EndSl;
112800120817
112900120817         // -?Load other sort parameters.?
113000120817         QLGLB   = 80 + 16 * MaxKey;
113100120817         QLGRL   = %size(SflRcd) - 1;
113200120817         QLGRT   = 8;
113300120817         QLGOKL  = 80;
113400120817         QLGLKE  = 16;
113500120817         QLGLSS  = 290;
113600120817         // -?Initialize Sort I/O API fields.?
113700120817         QLGRL00 = QLGRL;
113800120817         QLGRC00 = 1;
113900120817         clear  QUSEI;
114000120817         QUSBPRV = %size(QUSEC);
114100120817
114200120817         // -?First step - Initialize the sort routine.?
114300120817         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
114400120817                      SizeList : ReturnSize : QUSEC );
114500120817
114600120817         // -?Next step - Write records to I/O routine.?
114700120817         QLGRT00  = Put;
114800120817         For  Nrr = 1  To  RrnLast;
114900120817           chain  Nrr  TB79S01;
115000120817           // -?Solo le righe con Selected = 'Y' sono riordinate,?
115100120817           //  ?quindi per fare un ordinamento di tutte le righe?
115200120817           //  ?metto 'Y' sempre.?
115300120817           Selected = 'Y';
115400120817           clear  QUSEI;
115500120817           QUSBPRV  = %size(QUSEC);
115600120817           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
115700120817                         SizeList : NotUsed : QUSEC );
115800120817         EndFor;
115900120817
116000120817         // -?Next step - Signal end of input, clear subfile for reload.?
116100120817         QLGRT00 = EndPut;
116200120817         clear  QUSEI;
116300120817         QUSBPRV = %size(QUSEC);
116400120817         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
116500120817                       SizeList : NotUsed : QUSEC );
116600120817
116700120817         // -?Pulizia subfile?
116800120817         SflDsp_N    = *on;
116900120817         SflDspCtl_N = *on;
117000120817         write  TB79C01;
117100120817         SflDspCtl_N = *off;
117200120817         SflEnd      = *off;
117300120817
117400120817         // -?Final step - Write the records back to the subfile.?
117500120817         QLGRT00  = Get;
117600120817         For  Nrr = 1  To RrnLast;
117700120817           clear  QUSEI;
117800120817           QUSBPRV = %size(QUSEC);
117900120817           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
118000120817                         QLGRL00  : NotUsed : QUSEC );
118100120817           // -?Ripristino indicatori utilizzati nel sfl rec?
118200120817           SflNxtChg = (S1Copz <> *blank);
118300120817           S01nrr = Nrr;
118400120817           write  TB79S01;
118500120817         EndFor;
118600120817
118700120817         C1CsrRrn = 1;
118800120817
118900120817         // -?Visualizzazione del SFL (se ci sono dati)?
119000120817         SflDsp_N = (S01nrr <= *zero);
119100120817
119200120817         // -?Attivazione del SFLEND?
119300120817         SflEnd = $EoF;
119400120817
119500120817       ENDSR;
119600120817
119700120817       //--------------------------------------------------------------
119800120817       //?Gestione opzioni subfile S01.                                ?
119900120817       //--------------------------------------------------------------
120000120817       BEGSR  sr_OpzS01;
120100120817
120200120817         clear  SavS01csr;
120300120912         reset  $Conferma;
120400120817
120500120817         // -?Ciclo di lettura subfile?
120600120817         readc  TB79S01;
120700120817
120800120912         DOW  Not %eof(TNTB79D2);
120900120817
121000120817           SflNxtChg = *off;
121100120817           %subst(IndDspF : 50) = *off;
121200120817           C1RcdNbr = S01nrr;
121300120817
121400120817           select;
121500120817
121600120911             // -?Opz. 4=Annullamento?
121700120911             when  S1Copz = '4';
121800120912               if  Not $Conferma;
121900120920                 exsr  sr_GesW01;
122000120912                 if  wDspAid_W1 <> c_F06;
122100120912                   ErrGenerico = *on;
122200120912                   SflNxtChg   = *on;
122300120912                   C1CsrRrn    = C1RcdNbr;
122400120912                   SavS01csr   = C1RcdNbr;
122500120912                   SflNxtChg   = *on;
122600120912                   update  TB79S01;
122700120912                   leavesr;
122800120912                 endif;
122900120912               endif;
123000120912               clear  k05tntbe01;
123100120912               k_TBEcod = c_Tab;
123200120912               k_TBEke1 = S1Cksu;
123300120912               k_TBEke2 = S1Csun;
123400120912               chain  %kds(k05tntbe01)  TNTBE000;
123500120912               if  %found(TNTBE01L);
123600120912                 d3EW = TBEuni;
123700120912                 select;
123800120912                   when  TBEatb <> *blank;
123900120912                     $InzS01 = *on;
124000120912                   when  §3EWfaa <> *blank;
124100120912                     exsr  sr_Annull3EW;
124200120912                     $InzS01  = *on;
124300120912                   other;
124400120912                     ErrMessage  = *on;
124500120912                     ErrGenerico = *on;
124600120912                     PosCurOPZ   = *on;
124700120912                     V1Dmsg = $Msg(04);
124800120912                 endsl;
124900120912               endif;
125000120817
125100120817             // -?Nessuna opzione?
125200120817             when  S1Copz = *blank;
125300120817
125400120817             // -?Opzione errata?
125500120817             other;
125600120817               ErrMessage  = *on;
125700120817               ErrGenerico = *on;
125800120817               PosCurOPZ   = *on;
125900120911               V1Dmsg = $Msg(05);
126000120817               $Fine  = *off;
126100120817
126200120817           endsl;
126300120817
126400120817           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
126500120817           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
126600120817           if  S1Copz  <> *blank   and
126700120817              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
126800120817             SavS01csr   = C1RcdNbr;
126900120817           endif;
127000120817
127100120817           // -?Aggiornamento sfl?
127200120817           select;
127300120817             when  ErrMessage;
127400120817               SflNxtChg = *on;
127500120817               C1CsrRrn  = C1RcdNbr;
127600120817             when  ErrGenerico;
127700120817               C1CsrRrn  = C1RcdNbr;
127800120817             other;
127900120912               if  S1Copz = '4';
128000120912                 clear  S1Copz;
128100120912               endif;
128200120817           endsl;
128300120817
128400120817           if  S1Copz <> *blank;
128500120817             SflNxtChg = *on;
128600120817           endif;
128700120817
128800120817           UPDATE  TB79S01;
128900120817
129000120817           if  ErrGenerico   or   ErrMessage;
129100120817             leave;
129200120817           endif;
129300120817
129400120817           readc  TB79S01;
129500120817
129600120817         ENDDO;
129700120817
129800120817         // -?Riposizionam. cursore sul record contenente la prima opz.?
129900120817         //  ?(se non sono stati rilevati errori)?
130000120817         if  NOT ErrMessage   and   NOT ErrGenerico   and
130100120817             SavS01csr > *zero;
130200120817           C1CsrRrn = SavS01csr;
130300120817         endif;
130400120817
130500120817       ENDSR;
130600120912
130700120912       //--------------------------------------------------------------
130800120920       //?Gestione window W01:                                         ?
130900120920       //?Richiesta conferma annullamento.                             ?
131000120912       //--------------------------------------------------------------
131100120920       BEGSR  sr_GesW01;
131200120912
131300120912         //C1RcdNbr = S01nrr;      ?(già fatto)?
131400120912         $Conferma  = *on;
131500120912         wCountOpz4 = 1;
131600120920         wRig = S01nrr;
131700120912         clear  wDspAid_W1;
131800120912
131900120912         // -?Conteggio opzioni "4" immesse nel subfile?
132000120912         readc  TB79S01;
132100120912
132200120912         DOW  Not %eof(TNTB79D2);
132300120912
132400120912           // -?Aggiornamento sfl?
132500120912           if  S1Copz <> *blank;
132600120912             SflNxtChg = *on;
132700120912           endif;
132800120912           update  TB79S01;
132900120912
133000120920           // -?Incremento numerazione e?
133100120920           //  ?memorizzazione ultimo rec. con opz. "4"?
133200120912           if  S1Copz = '4';
133300120920             wRig = S01nrr;
133400120912             wCountOpz4 += 1;
133500120912           endif;
133600120912
133700120912           readc  TB79S01;
133800120912
133900120912         ENDDO;
134000120920
134100120920         // -?Pulizia window per la conferma?
134200120920         clear  TB79W01;
134300120912
134400120912         //// -?Determina Riga/Colonna per la la window?
134500120912         ////  ?in base alla posizione del corsore?
134600120912         //R$$ = dsp_NRG;
134700120912         //C$$ = dsp_NCL;
134800120912         //if  RiRi >= 10;
134900120912         //  W1rig = RiRi - 7;
135000120912         //else;
135100120912         //  W1rig = 3;
135200120912         //endif;
135300120912         ////W1col = CoCo;
135400120912         // -?Determina Riga/Colonna per la la window?
135500120920         //  ?in base al 1° e all'ultimo record con l'opzione "4"?
135600120912         //  ?(il cursore potrebbe essere OVUNQUE...)?
135700120920         //if  C1RcdNbr > *zero;
135800120920         //  wPag = C1RcdNbr / c_SflPag;
135900120920         //  wRig = C1RcdNbr - (c_SflPag * wPag);
136000120920         //endif;
136100120920         S01nrr = wRig;
136200120920         if  S01nrr > *zero;
136300120920           wPag = S01nrr / c_SflPag;
136400120920           wRig = S01nrr - (c_SflPag * wPag);
136500120920           if  wRig = *zero;
136600120920             wRig = c_SflPag;
136700120920           endif;
136800120920         else;
136900120920           wRig = 1;
137000120920         endif;
137100120920         select;
137200120920           when  wRig >= 1  and  wRig <= 10;
137300120920             W1rig = wRig + 8;
137400120920           when  wRig >  10;
137500120920             W1rig = wRig - 7 + 7;
137600120920           //other;
137700120920           //  W1rig = 15;
137800120920         endsl;
137900120920
138000120920         // -?Riposizionamento sul 1° record originale nel subfile?
138100120920         chain  C1RcdNbr  TB79S01;
138200120920
138300120920         // -?Emissione window per la conferma?
138400120920         W1Ctxt1 = 'Premere F6 per confermare l''annullamento di +
138500120920                    questi dati relativi';
138600120920         if  wCountOpz4 = 1;
138700120920           W1Ctxt1 = %trimr(W1Ctxt1) + ' al ';
138800120920           W1Ctxt2 = 'cliente selezionato.';
138900120920         else;
139000120920           W1Ctxt1 = %trimr(W1Ctxt1) + ' ai ';
139100120920           W1Ctxt2 = %trim( %editc( wCountOpz4 : '3' ) ) +
139200120920                     ' clienti selezionati.';
139300120920         endif;
139400120920         W1Ctxt3 = 'Premere F12 altrimenti.';
139500120920
139600120920         W1Ctxt1A = c_DA_BL;
139700120920         W1Ctxt2A = c_DA_BL;
139800120920         W1Ctxt3A = c_DA_Norm;
139900120912
140000120912         DoU  dsp_aid <> c_Enter;
140100120912           exfmt  TB79W01;
140200120912         EndDo;
140300120912
140400120912         wDspAid_W1 = dsp_aid;
140500120912
140600120912       ENDSR;
140700120911
140800120911       //--------------------------------------------------------------
140900120912       //?Annullamento logico record tabella "3EW"                     ?
141000120911       //--------------------------------------------------------------
141100120911       BEGSR  sr_Annull3EW;
141200120912
141300120912         //? N.B.                                                    ?
141400120912         // L'annullamento  ed  il ripristino  vanno lasciati in    ?
141500120912         //   trasmissione (vedi flag TBEFTR).                      ?
141600120912         // L'aggiornamento  e  l'inserimento  NO: vanno registrati ?
141700120912         //   subito nello stesso file di entrambi i S.I. - in due  ?
141800120912         //   cicli diversi - ma NON vanno messi in trasmissione.   ?
141900120911
142000120911         // -?Ciclo di elaborazione per ogni sistema informativo?
142100120911         For  w_SisInf = 1  To  %elem($Libr);
142200120911
142300120912           // -?Apertura del file tabelle?
142400120911           if  w_SisInf > 1;
142500120911             exsr  sr_OpenFileTab;
142600120912             clear  k05tntbe01;
142700120912             k_TBEcod = c_Tab;
142800120912             k_TBEke1 = S1Cksu;
142900120912             k_TBEke2 = S1Csun;
143000120912             //k_TBElin = TBXlin;
143100120912             //k_TBEsif = TBXsif;
143200120912             chain  %kds(k05tntbe01)  TNTBE000;
143300120911           endif;
143400120911
143500120911           // -?Annullamento tab. "3EW"?
143600120911           TBEatb = 'A';
143700120911           //TBEftt = W1ftt;
143800120911           TBEftt = 'S';
143900120911           clear  TBEftr;
144000120911           //_______________
144100120911           UPDATE  TNTBE000;
144200120911           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
144300120911
144400120912         EndFor;
144500120912
144600120912         // -?Ri-Apertura del file tabelle - di Sede?
144700120912         //  ?(prima della lettura del record successivo nel subfile)?
144800120912         w_SisInf = 1;
144900120912         exsr  sr_OpenFileTab;
145000120911
145100120911       ENDSR;
145200120911
145300120911       //--------------------------------------------------------------
145400120911       //?Apertura del file tabelle nel sistema informativo impostato. ?
145500120911       //--------------------------------------------------------------
145600120911       BEGSR  sr_OpenFileTab;
145700120911
145800120911         // -?Chiusura (eventuale) file tabelle?
145900120911         if  %open(TNTBE01L);
146000120911           close  TNTBE01L;
146100120911         endif;
146200120911
146300120911         // -?Apertura file tabelle?
146400120911         ds_Libr  = $Libraries(w_iSystem);
146500120911         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
146600120911         open  TNTBE01L;
146700120911
146800120911       ENDSR;
146900120817
147000120817       //--------------------------------------------------------------
147100120817       //?Operazioni finali.                                           ?
147200120817       //--------------------------------------------------------------
147300120817       BEGSR  sr_RoutEnd;
147400120817
147500120817         // -?Chiusura funzioni precedentemente aperte?
147600120817         reset  TIBS69ds;
147700120817         //I69tla = 'C';         ?(già così)?
147800120817         tibs69r( tibs69ds :
147900120817                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
148000121129
148100121129         cloTabella ( c_Tabel );
148200121129
148300121129         ubLeg3C_Close ();
148400120817
148500120817         // -?"Chiusura" *pgm?
148600120817         return;
148700120817
148800120817       ENDSR;
148900120817
149000120817      /end-free
149100120817
149200120817       //--------------------------------------------------------------
149300120817       //?Schiere a tempo di compilazione.                             ?
149400120817       //--------------------------------------------------------------
149500120817
149600120911** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
149700120911SETRAS  GAITRAGRU FILTRAGRU
149800120911AS888   GAITRAGRPSFILTRAGRPF
149900120911** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
150000120911Codice cliente unificante errato                                               1
150100120911Filiale segnacollo errata                                                      2
150200120911Data annullamento limite errata                                                3
150300120911Cliente SENZA annullamento applicativo: NON annullabile                        4
150400120911Opzione errata                                                                 5
150500120912Data annullamento applicativo errata                                           6
