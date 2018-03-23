000100180205       //==============================================================
000200180205       //? TNTB11R * Gestione tab. "MSL" = VdL-Layout postazionizione   ?
000300180205       //==============================================================
000400180205
000500180205       //--------------------------------------------------------------
000600180205       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180205       //--------------------------------------------------------------
000800180205
000900180205     /*PRM dbgview(*source)
001000180205     /*END
001100180205
001200180205       //--------------------------------------------------------------
001300180205       //? Specifiche di controllo.                                     ?
001400180205       //--------------------------------------------------------------
001500180205
001600180205     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700161027     h dftactgrp(*no)
001800161027     h bnddir('UBBNDDIR')
001900180205
002000180205       //--------------------------------------------------------------
002100180205       //? Dichiarazione file.                                          ?
002200180205       //--------------------------------------------------------------
002300060221
002400180205       // - ?Organigramma?
002500180205     fAZORG01L  if   e           k disk
002600180205
002700180205       // - ?Tabelle?
002800180205     fTNTBE01L  Uf A e           k disk    usropn
002900180205     f                                     extfile( wLibFile )
003000180205
003100180205       // - ?Video?
003200180205     fTNTB11D   cf   e             workstn
003300180219     f                                     indds( IndDspF )
003400180219     f                                     infds( InfDspF )
003500180205     f                                     sfile( TB11S01 : S01nrr )
003600180205     f                                     sfile( TB11S05 : S05nrr )
003700060221
003800180205       //--------------------------------------------------------------
003900180205       //? Definizione costanti.                                        ?
004000180205       //--------------------------------------------------------------
004100180205
004200180205       // - ?Codice tabella in gestione?
004300180205     d c_Tab           c                   const('MSL')
004400080411
004500180205       // - ?Numero di record in ciascuna videata di subfile?
004600161011     d c_SflPag        c                   const(17)
004700180205
004800180205       // - ?Numero massimo di record gestibili nel subfile?
004900161013     d c_SflSizMax     c                   const(80)
005000180301
005100180301       // - ?Numero massimo di Tipi Applicazione gestiti?
005200180301     d c_MaxMTP        c                   const(9)
005300161027
005400180205       // - ?Costanti per la definizione delle schiere con i nomi?
005500180205       //   ?degli iSeries da elaborare e delle relative librerie?
005600161027     d c_NrSyst        c                   const(2)
005700161027     d c_NrLibr        c                   const(2)
005800180205
005900180205       // - ?Costante per controllo "caratteri solo numerici"?
006000180205     d c_Digits        c                   const('0123456789')
006100180205
006200180205       // - ?Tasti funzionali a video?
006300180205     d c_F01           c                   const(x'31')
006400180205     d c_F02           c                   const(x'32')
006500180205     d c_F03           c                   const(x'33')
006600180205     d c_F04           c                   const(x'34')
006700180205     d c_F05           c                   const(x'35')
006800180205     d c_F06           c                   const(x'36')
006900180205     d c_F07           c                   const(x'37')
007000180205     d c_F08           c                   const(x'38')
007100180205     d c_F09           c                   const(x'39')
007200180205     d c_F10           c                   const(x'3A')
007300180205     d c_F11           c                   const(x'3B')
007400180205     d c_F12           c                   const(x'3C')
007500180205     d c_F13           c                   const(x'B1')
007600180205     d c_F14           c                   const(x'B2')
007700180205     d c_F15           c                   const(x'B3')
007800180205     d c_F16           c                   const(x'B4')
007900180205     d c_F17           c                   const(x'B5')
008000180205     d c_F18           c                   const(x'B6')
008100180205     d c_F19           c                   const(x'B7')
008200180205     d c_F20           c                   const(x'B8')
008300180205     d c_F21           c                   const(x'B9')
008400180205     d c_F22           c                   const(x'BA')
008500180205     d c_F23           c                   const(x'BB')
008600180205     d c_F24           c                   const(x'BC')
008700180205     d c_Enter         c                   const(x'F1')
008800180205     d c_RollDown      c                   const(x'F4')
008900180205     d c_RollUp        c                   const(x'F5')
009000060221
009100180205       //--------------------------------------------------------------
009200180205       //? Definizione schiere.                                         ?
009300180205       //--------------------------------------------------------------
009400161027
009500180205       // - ?iSeries  &  Librerie con il file tabella?
009600161104     d sk_iSystem      s                   like(currSysNetA)
009700161027     d                                     dim(c_NrSyst)
009800161027     d                                     ctdata   perrcd( 1)
009900161104     d sk_Libraries    s                   like(ds_Libr)
010000161027     d                                     dim(c_NrSyst)
010100161104     d                                     alt(sk_iSystem)
010200180205       // - ?Ridefinizione elenco librerie in cui elaborare la tabella?
010300161027     d ds_Libr         ds                  inz
010400161104     d   sk_Libr                     10    dim(c_NrLibr) inz
010500060221
010600180205       // - ?Messaggi di errore?
010700180301     d sk_Msg          s             78    dim(16)  ctdata  perrcd( 1)
010800161104
010900180205       // - ?Postazioni Entrata già rilevate?
011000180205     d sk_PEP          s                   like(S01pep)  inz
011100161104     d                                     dim(c_SflSizMax)
011200180219       // - ?Tipi Applicazione già rilevati?
011300180219     d sk_APL          s                   like(S01apl)  inz
011400180301     d*//·                                 dim(c_SflSizMax)
011500180301     d                                     dim(c_MaxMTP)
011600180206
011700180206       // - ?Tipi Applicazione?
011800180206     d sk_MTP          s                   like(S01apl)  inz
011900180301     d                                     dim(c_MaxMTP)
012000180206     d sk_MTPdes       s                   like(§MTPdes) inz
012100180301     d                                     dim(c_MaxMTP)
012200180206       // - ?Entrate per singolo Tipo Applicazione?
012300180206     d ds_ENT          ds                  inz
012400180206     d   sk_ENT                            like(S01pep)  inz
012500180301     d                                     dim(c_SflSizMax)
012600180206       // - ?Entrate per tutti i Tipi Applicazione?
012700180206     d sk_ENTxMTP      s                   like(ds_Ent)  inz
012800180301     d                                     dim(c_MaxMTP)
012900180301
013000180301       // - ?Comodo per segnalare buchi nella sequenza delle Entrate?
013100180301     d sk_MemoErrENT   s               n   inz
013200180301     d                                     dim(c_SflSizMax)
013300180213
013400180213       // - ?Comodo per ordinamento?
013500180213     d sk_Ord          s              2    inz
013600180213     d                                     dim(10)
013700180205
013800180205       //--------------------------------------------------------------
013900180205       //? Definizione aree dati.                                       ?
014000180205       //--------------------------------------------------------------
014100180205
014200180205       // - ?Dati utente?
014300180205     d §AzUte        e ds                  extname(AZUTE00F)
014400180205     d                                     dtaara
014500180205     d §DatiUte      e ds                  extname(dDatiUte)
014600180205     d                                     dtaara
014700180205
014800180205       //--------------------------------------------------------------
014900180205       //? Definizione strutture dati.                                  ?
015000180205       //--------------------------------------------------------------
015100060221
015200180205       // - ?Status ds?
015300180205     d Status         sds
015400180205     d   SDSpgm          *proc
015500180205
015600180205       // - ?InfDS?
015700180205     d InfDspF         ds                  qualified
015800180205     d   dsp_aid             369    369a
015900180205     d   sfl_rrn             376    377i 0
016000180205     d   min_nrr             378    379i 0
016100180205     d   num_rcds            380    381i 0
016200180205
016300180205       // - ?Indicatori su DspF?
016400180205     d IndDspF         ds                  inz
016500180205         // - ?Abilitazione tasti funzionali?
016600180206     d   $F6Attivo                     n   overlay( IndDspF : 06 )
016700180206     d   $F8Attivo                     n   overlay( IndDspF : 08 )
016800180206     d   $F9Attivo                     n   overlay( IndDspF : 09 )
016900180206     d   $F12Attivo                    n   overlay( IndDspF : 12 )
017000180205         // - ?Emissione messaggio di errore?
017100180206     d   $ErrMessage                   n   overlay( IndDspF : 28 )
017200180205         // - ?Indicatori di gestione del subfile?
017300180206     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
017400180206     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
017500180206     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
017600180206     d   $SflEnd                       n   overlay( IndDspF : 33 )
017700180206     d   $SflDsp5N                     n   overlay( IndDspF : 34 )
017800180206     d   $SflDspCtl5N                  n   overlay( IndDspF : 35 )
017900180206     d   $SflEnd5                      n   overlay( IndDspF : 36 )
018000180205         // - ?Indicatori per Attibuti di visualizzazione?
018100180206     d   $ProtAnnull                   n   overlay( IndDspF : 40 )
018200180206     d   $OrdAppl02                    n   overlay( IndDspF : 42 )
018300180206     d   $OrdAppl03                    n   overlay( IndDspF : 43 )
018400180206     d   $OrdAppl04                    n   overlay( IndDspF : 44 )
018500180206     d   $OrdAppl05                    n   overlay( IndDspF : 45 )
018600180206     d   $OrdAppl06                    n   overlay( IndDspF : 46 )
018700180205         // - ?Posizionamento cursore & segnalazione errore?
018800180206     d   $PosCurFIL                    n   overlay( IndDspF : 50 )
018900180206     d   $PosCurOPZ                    n   overlay( IndDspF : 51 )
019000180206     d   $PosCurDES                    n   overlay( IndDspF : 52 )
019100180206     d   $PosCurDE5                    n   overlay( IndDspF : 53 )
019200180206     d   $PosCurPEP                    n   overlay( IndDspF : 54 )
019300180206     d   $PosCurENT                    n   overlay( IndDspF : 55 )
019400180206     d   $PosCurAPL                    n   overlay( IndDspF : 56 )
019500180206     d   $PosCurAP5                    n   overlay( IndDspF : 57 )
019600180206     d   $PosCurPWF                    n   overlay( IndDspF : 58 )
019700180206     d   $PosCurPKE                    n   overlay( IndDspF : 59 )
019800180206     d*//$PosCurANN                    n   overlay( IndDspF : 60 )
019900180206     d   $PosCurApl1                   n   overlay( IndDspF : 61 )
020000180206     d   $PosCurApl2                   n   overlay( IndDspF : 62 )
020100180206     d   $PosCurApl3                   n   overlay( IndDspF : 63 )
020200180206     d   $PosCurApl4                   n   overlay( IndDspF : 64 )
020300180206     d   PosCurApl5                    n   overlay( IndDspF : 65 )
020400180206     d   $PosCurApl6                   n   overlay( IndDspF : 66 )
020500180205         // - ?Riemissione videata?
020600180206     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
020700180205
020800180205       // - ?Parametri ricevuti?
020900180205     d KPJBA         e ds
021000180205
021100180205       // - ?Dati record principale della tabella?
021200180219     d TNTBEds       e ds                  inz  extname( TNTBE00F )
021300180219     d xTNTBEds      e ds                  inz  extname( TNTBE00F )
021400180219     d                                          prefix( TBX : 3 )
021500010308
021600180205       // - ?Tabelle usate:?
021700180205       // · ?"MTP" = VdL-Tipo Applicazione?
021800180205     d dMTP          e ds                  inz
021900180205       // · ?"MSL" = VdL-Layout Postazioni?
022000180205     d dMSL          e ds                  inz
022100180219     d dMSL_vd       e ds                  inz  extname( dMSL )
022200180219     d                                          prefix( W )
022300180206       // · ?"MFP" = Positrova Attivo in Filiale?
022400180206     d dMFP          e ds                  inz
022500180205
022600180205       // - ?Descrizione 150 dell'ORGANIGRAMMA?
022700180206     d Og150         e ds                  inz  qualified
022800180205
022900180205       //--------------------------------------------------------------
023000180205       //? Definizione variabili globali.                               ?
023100180205       //--------------------------------------------------------------
023200060221
023300180205       // - ?Flags booleani?
023400180205     d $Fine           s               n   inz
023500180205     d $InzD01         s               n   inz(*on)
023600180205     d $InzS01         s               n   inz(*on)
023700180205     d $InzS05         s               n   inz(*on)
023800180206     d $InzW01         s               n   inz(*on)
023900180206     d $Ord_AplPrg     s               n   inz
024000180205
024100180205       // - ?Variabili per la gestione del video?
024200180205     d $Video          s              2    inz('D1')
024300180219     d S01nrr          s                   like( C1RcdNbr )  inz
024400180219     d SavS01csr       s                   like( C1RcdNbr )  inz
024500180219     d S05nrr          s                   like( C5RcdNbr )  inz
024600180205
024700180205       // - ?Indici di schiera?
024800180205     d xx              s              3  0 inz
024900180219     d yy              s              3  0 inz
025000180205
025100180205       // - ?Nome esteso Libreria/File del file tabella?
025200180205     d wLibFile        s             21a   inz
025300180205
025400180205       // - ?Campi di comodo?
025500180205     d w_iSystem       s              1  0 inz
025600180205     d w_SisInf        s              3  0 inz
025700180205     d wDate           s              8  0 inz
025800180205     d wKe2            s              3  0 inz
025900180219     d wAPL            s                   like( §MFPapl )   inz
026000180208
026100180208       //--------------------------------------------------------------
026200180213       // - ?PARAMETRI PER ORDINAMENTO SUBFILE?
026300180208       //--------------------------------------------------------------
026400180208
026500180208       //--------------------------------------------------------------
026600180213       // - ?Constants?
026700180208       //--------------------------------------------------------------
026800180213       // - ?MaxKey - Maximum number of key fields allowed by this program?
026900180219     d c_MaxKey        c                   const(5)
027000180213       // - ?Sort order:?
027100180213       //   ?1 = Sort in an ascending sequence?
027200180213       //   ?2 = Sort in a descending sequence?
027300180208     d c_Ascendente    c                   const(1)
027400180208     d c_Discendente   c                   const(2)
027500180213       // - ?Key data type:?
027600180208       //  ? 0 = Signed binary?
027700180208       //  ? 2 = Signed zoned decimal?
027800180208       //  ? 3 = Signed packed decimal?
027900180208       //  ? 6 = Character with no national language sort sequence applied,?
028000180208       //  ?     if specified?
028100180208       //  ? 7 = Unsigned packed decimal?
028200180208       //  ?     All numbers will have the sign forced positive ('F'X).?
028300180208       //  ? 8 = Unsigned zoned decimal?
028400180208       //  ?     All numbers will have the sign forced positive ('F'X).?
028500180208       //  ? 9 = Unsigned binary?
028600180208       //  ?14 = Date in form of DD/MM/YY?
028700180208       //  ?15 = Date in form of DD.MM.YYYY?
028800180208     d c_Numero        c                   const(2)
028900180208     d c_Carattere     c                   const(6)
029000180208     d c_NumIntero     c                   const(8)
029100180208       //
029200180208     d c_Put           c                   const(1)
029300180208     d c_EndPut        c                   const(2)
029400180208     d c_Get           c                   const(3)
029500180208
029600180208       //--------------------------------------------------------------
029700180213       // - ?Data Structures?
029800180213       //   ?SflRcd     - The entire subfile record to pass to the sort?
029900180213       //   ?QLGSCB     - The sort request block for the QLGSORT API?
030000180213       //   ?QLGSCB00   - The sort request block for the QLGSRTIO API?
030100180213       //   ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
030200180213       //   ?QUSEC      - Error structure for the QLGSORT API?
030300180208       //--------------------------------------------------------------
030400180208     d SflRcd          ds                  inz
030500180219     d   S1Hseq                            inz
030600180219     d   S1Hdes                            inz
030700180219     d   S1Hde5                            inz
030800180219     d   S1Hpep                            inz
030900180219     d   S1Hent                            inz
031000180219     d   S1Hapl                            inz
031100180219     d   S1Hap5                            inz
031200180219     d   S1Hpwf                            inz
031300180219     d   S1Hpke                            inz
031400180219     d   S1Hann                            inz
031500180208     d   S01opz                            inz
031600180208     d   S01des                            inz
031700180208     d   S01de5                            inz
031800180208     d   S01pep                            inz
031900180208     d   S01ent                            inz
032000180208     d   S01apl                            inz
032100180208     d   S01ap5                            inz
032200180208     d   S01pwf                            inz
032300180208     d   S01pke                            inz
032400180208     d   S01ann                            inz
032500180208     d   Selected                     1a   inz
032600180208      /copy QSYSINC/QRPGLESRC,QLGSORT
032700180208     d QLGKL                         16    dim(c_MaxKey)
032800180208     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
032900180208     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
033000180208     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
033100180208     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
033200180208      /copy QSYSINC/QRPGLESRC,QLGSRTIO
033300180208      /copy QSYSINC/QRPGLESRC,QUSEC
033400180208
033500180208       //--------------------------------------------------------------
033600180213       // - ?Standalone fields?
033700180213       //   ?Nrr        - Relative record number for subfile?
033800180213       //   ?SizeList   - Size of list?
033900180213       //   ?ReturnSize - Size of list returned by sort API?
034000180208       //--------------------------------------------------------------
034100180208     d NotUsed         s             16a   inz
034200180208     d ReturnSize      s             10i 0 inz
034300180208     d SizeList        s             10i 0 inz
034400180208     d RrnLast         s              5i 0 inz
034500180208     d Nrr             s              5i 0 inz
034600161027
034700161027       //--------------------------------------------------------------
034800180205       //? Definizione prototipi/procedure.                             ?
034900161027       //--------------------------------------------------------------
035000161027
035100180205       // - ?Reperimento NETA sistema AS/400 corrente?
035200180205     d currSysNeta     s              8a
035300161027      /copy gaitrasrc/srcProtoPR,UBRTVNETA
035400180205
035500180205       // - ?Reperimento dati utente?
035600180205     d TIBS34ds      e ds                  inz
035700180205      /copy gaitrasrc/srcProtoPR,TIBS34R
035800180205
035900180205       // - ?Parametri per Ricerca/Controllo tabelle (TNTBE)?
036000180205     d TIBS02ds      e ds                  inz
036100180205     d   T02mod      e                     inz('R')
036200180205     d   T02cod      e                     inz('MSL')
036300180205       // - ?Ricerca/Controllo tabelle (TNTBE)?
036400180205      /copy gaitrasrc/srcProtoPR,TIBS02R
036500180208
036600180208       // -?Ordinamento subfile?
036700180208      /copy gaitrasrc/srcProtoPR,QLGSRTIO
036800180205
036900180205       //--------------------------------------------------------------
037000180205       //? Definizione key-list.                                        ?
037100180205       //--------------------------------------------------------------
037200180205
037300180205       // - ?File TNTBE01L?
037400180205     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
037500180205     d                                     prefix( k_ )   inz
037600180205
037700180205       //--------------------------------------------------------------
037800180205       //? Riepilogo indicatori utilizzati.                             ?
037900180205       //--------------------------------------------------------------
038000180205
038100180205
038200180205       //--------------------------------------------------------------
038300180205       //? M A I N - L I N E                                            ?
038400180205       //--------------------------------------------------------------
038500180205
038600180205     c     *Entry        plist
038700180205     c                   parm                    KPJBA
038800180205
038900180205      /free
039000180205
039100180205       // - ?Operazioni iniziali?
039200180205       exsr sr_RoutInz;
039300180205
039400180205       // - ?Ciclo di gestione del file video?
039500180205       DoW  $Fine = *off;
039600180205
039700180205         select;
039800180205
039900180205           // - ?Richiesta chiave di accesso?
040000180205           when $Video = 'D1';
040100180205             exsr  sr_GesD01;
040200180205
040300180205           // - ?Gestione videata S1 (subfile)?
040400180205           when $Video = 'S1';
040500180205             exsr  sr_GesS01;
040600180205
040700180205           // - ?Visualizzazione videata S5 (subfile vecchio stile)?
040800180205           when $Video = 'S5';
040900180205             exsr  sr_GesS05;
041000180206
041100180206           // - ?Visualizzazione videata W1 (window x ordinamento sfl)?
041200180206           when $Video = 'W1';
041300180206             exsr  sr_GesW01;
041400180205
041500180205           // - ?? ? ??
041600180205           other;
041700180205             $Fine = *on;
041800180205
041900180205         endsl;
042000180205
042100180205       EndDo;
042200180205
042300180205       // - ?Operazioni finali?
042400180205       exsr sr_RoutEnd;
042500180205
042600180205       //--------------------------------------------------------------
042700180205       //? Operazioni iniziali.                                         ?
042800180205       //--------------------------------------------------------------
042900180205       BEGSR  sr_RoutInz;
043000180205
043100180205         // - ?Impostazione chiusura?
043200180205         *inLR = *on;
043300180205
043400180205         // - ?Verifica del sistema AS/400 corrente?
043500180205         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
043600180205           $Fine = *on;
043700180205           leavesr;
043800180205         endif;
043900180205
044000180205         // - ?Impostazione elenco librerie in cui gestire le tabelle?
044100180205         //   ?(a seconda del sistema in cui si stà lavorando)?
044200180205         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
044300180205         if  w_iSystem = *zero;
044400180205           $Fine = *on;
044500180205           leavesr;
044600180205         endif;
044700180205
044800180205         // - ?1ª apertura file TABEL00F del 1° S.I. (sede)?
044900180205         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
045000180205           w_SisInf = 1;
045100180205           exsr  sr_OpenFileTab;
045200180205         endif;
045300180205
045400180205         // - ?Reperimento data odierna?
045500180205         wDate = %int( %subst( %char( %dec( %timestamp() ) )
045600180205                               : 1 : 8 ) );
045700180205
045800180205         // - ?Reperimento dati job?
045900180205         exsr  sr_DatiJob;
046000180205
046100180205         // - ?Impostazione nome programma a video?
046200180205         V1Tpgm = SDSpgm;
046300180205
046400180205         // - ?Aggancio dati generali della tabella in esame?
046500180205         clear  k_TBEcod;
046600180205         k_TBEke1 = *zero;
046700180205         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
046800180205                = c_Tab;
046900180205         clear  k_TBEke2;
047000180205         clear  k_TBElin;
047100180205         k_TBEsif = KNSIF;
047200180206         chain(N)  %kds( keyTNTBE01 )  TNTBE000;
047300180205         if  not  %found(TNTBE01L);
047400180205           clear  k_TBEsif;
047500180206           chain(n)  %kds( keyTNTBE01 )  TNTBE000;
047600180205         endif;
047700180205         if  %found(TNTBE01L);
047800180205           xTNTBEds = TNTBEds;
047900180205         else;
048000180205           clear  xTNTBEds;
048100180205         endif;
048200180205
048300180206         // - ?Caricamento Tipi Applicazione gestiti?
048400180206         clear  sk_MTP;
048500180206         clear  keyTNTBE01;
048600180206         k_TBEcod = 'MTP';
048700180206         setll     %kds( keyTNTBE01 )  TNTBE000;
048800180206         reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
048900180206         DoW  NOT %eof(TNTBE01L);
049000180206           dMTP = TBEuni;
049100180206           if  §MTPswv = 'S';
049200180206             xx += 1;
049300180206             sk_MTP(xx)    = TBEke1;
049400180206             sk_MTPdes(xx) = §MTPdes;
049500180206           endif;
049600180206           reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
049700180206         EndDo;
049800180206
049900180205         // - ?Impostazione iniziale dei flags booleani?
050000180205         reset  $Fine;
050100180205         reset  $InzD01;
050200180205         reset  $InzS01;
050300180205         reset  $InzS05;
050400180205         reset  $Video;
050500180205
050600180205       ENDSR;
050700180205
050800180205       //--------------------------------------------------------------
050900180205       //? Reperimento Dati del job (Utente/Operativi).                 ?
051000180205       //--------------------------------------------------------------
051100180205       BEGSR  sr_DatiJob;
051200180205
051300180205         in(e) §AzUte;
051400180205         if NOT %error;
051500180205           in(e) §DatiUte;
051600180205         endif;
051700180205         if %error or RSut = *blank;
051800180205           tibs34r ( tibs34ds );
051900180205           in §AzUte;
052000180205           in §DatiUte;
052100180205         endif;
052200180205
052300180205       ENDSR;
052400180205
052500180205       //--------------------------------------------------------------
052600180205       //? Gestione videata D01.                                        ?
052700180205       //--------------------------------------------------------------
052800180205       BEGSR  sr_GesD01;
052900180205
053000180205         // - ?Inizializzazione videata?
053100180205         if  $InzD01 = *on;
053200180205           exsr  sr_InzD01;
053300180205           $InzD01 = *off;
053400180205         endif;
053500180205
053600180205         // - ?(Dis)Abilitazione tasti funzionali?
053700180205         $F6Attivo  = *off;
053800180205         $F8Attivo  = *off;
053900180205         $F9Attivo  = *off;
054000180205         $F12Attivo = *off;
054100180205
054200180205         // - ?Emissione Testata & Piede?
054300180205         write  TB11T01;
054400180205         write  TB11P01;
054500180205
054600180205         // - ?Emissione videata?
054700180205         exfmt  TB11D01;
054800180205
054900180205         clear  VIDmsg;
055000180205         reset  $ErrMessage;
055100180205         reset  $ErrGenerico;
055200180205
055300180205         SELECT;
055400180205
055500180205           // - ?F3=Fine?
055600180205           WHEN  InfDspF.dsp_aid = c_F03;
055700180205             $Fine = *on;
055800180205
055900180205           // - ?Invio?
056000180205           OTHER;
056100180205             exsr  sr_CtrD01;
056200180205             if  $ErrGenerico = *on;
056300180205               leavesr;
056400180205             endif;
056500180205             $Video  = 'S1';
056600180205             $InzS01 = *on;
056700180205
056800180205         ENDSL;
056900180205
057000180205       ENDSR;
057100180205
057200180205       //--------------------------------------------------------------
057300180205       //? Inizializzazione videata D01.                                ?
057400180205       //--------------------------------------------------------------
057500180205       BEGSR  sr_InzD01;
057600180205
057700180205         clear  TB11D01;
057800180205
057900180205         V1Cfil = '?';
058000180205
058100180205       ENDSR;
058200180205
058300180205       //--------------------------------------------------------------
058400180205       //? Controllo dati immessi nella videata D01.                    ?
058500180205       //--------------------------------------------------------------
058600180205       BEGSR  sr_CtrD01;
058700180205
058800180205         // - ?Spegnimento indicatori di posizionamento cursore?
058900180205         %subst(IndDspF : 28) = *off;
059000161027
059100180205         Select;
059200180205
059300180205           // - ?Ricerca Filiale (in tab. "MSL")?
059400180205           When  %scan('?' : V1Cfil) > *zero;
059500180205             //*·clear  V1Cfil;
059600180205             clear  V1Dfil;
059700180205             reset  TIBS02ds;
059800180205             //T02mod = 'R';                // ?(già così)?
059900180205             //T02cod = c_Tab;              // ?(già così)?
060000180205             T02sif = kNSif;
060100180205             TNTBE_RicercaControllo ( kpjba : TIBS02ds );
060200180205             if  T02err = *blank;
060300180205               V1Cfil = T02ke1;
060400180205               V1Dfil = %subst( T02uni : 1 : %len(V1Dfil) );
060500180205             endif;
060600180205             $ErrGenerico = *on;
060700180205             $PosCurFIL   = *on;
060800180205             leavesr;
060900180205
061000180205           // - ?Filiale NON inserita?
061100180205           When  V1Cfil = *blank  or  V1Cfil = *zero;
061200180205             $ErrGenerico = *on;
061300180205             $ErrMessage  = *on;
061400180205             $PosCurFIL   = *on;
061500180205             VIDmsg = sk_Msg(01);
061600180205             leavesr;
061700180205
061800180205         EndSl;
061900010601
062000180205         // - ?Controllo Filiale?
062100180205         clear  V1Dfil;
062200180205         chain  ( %int( V1CFIL ) )  AZORG;
062300180205         if  NOT %found(AZORG01L);
062400180205           $ErrGenerico = *on;
062500180205           $ErrMessage  = *on;
062600180205           $PosCurFIL   = *on;
062700180205           VIDmsg = sk_Msg(02);
062800180205           leavesr;
062900180205         endif;
063000180205
063100180205         V1Dfil = ORGdes;
063200180205
063300180205         Select;
063400180205
063500180205           When  ORGfva <> *blank;
063600180205             $ErrGenerico = *on;
063700180205             $ErrMessage  = *on;
063800180205             $PosCurFIL   = *on;
063900180205             VIDmsg = sk_Msg(03);
064000180205             leavesr;
064100180205
064200180205           When  ORGfag <> 'A'  and  ORGfag <> 'F';
064300180205             $ErrGenerico = *on;
064400180205             $ErrMessage  = *on;
064500180205             $PosCurFIL   = *on;
064600180205             VIDmsg = sk_Msg(04);
064700180205             leavesr;
064800180205
064900180205         EndSl;
065000180206
065100180206         // - ?Reperimento tab. "MFP" per la filiale?
065200180206         clear  dMFP;
065300180206         clear  keyTNTBE01;
065400180206         k_TBEcod = 'MFP';
065500180206         k_TBEke1 = V1Cfil;
065600180206         chain(N)  %kds( keyTNTBE01 )  TNTBE000;
065700180206         if  %found(TNTBE01L);
065800180206           dMFP = TBEuni;
065900180206         endif;
066000010604
066100180205       ENDSR;
066200180205
066300180205       //--------------------------------------------------------------
066400180205       //? Gestione SubFile S01.                                        ?
066500180205       //--------------------------------------------------------------
066600180205       BEGSR  sr_GesS01;
066700180205
066800180205         // - ?Inizializzazione subfile?
066900180205         if  $InzS01 = *on;
067000180205           exsr  sr_InzS01;
067100180219           exsr  sr_CtrS01;
067200180309           $InzS01 = *off;
067300180219           clear  VIDmsg;
067400180219           clear  $ErrMessage;
067500180219           clear  $ErrGenerico;
067600180205         endif;
067700180205
067800180205         // - ?(Dis)Abilitazione tasti funzionali?
067900180205         $F6Attivo  = *on;
068000180205         $F8Attivo  = *on;
068100180205         $F9Attivo  = *on;
068200180205         $F12Attivo = *on;
068300180205
068400180205         // - ?Posizionamento cursore?
068500180205         //   ?C1CsrRrn contiene il numero di riga del subfile su cui?
068600180205         //   ?era posizionato il cursore; impostandolo in C1RcdNbr?
068700180205         //   ?si visualizza la pagina che era visualizzata quando?
068800180205         //   ?l'utente ha premuto l'ultimo tasto.?
068900180205         if  C1CsrRrn > *zero;
069000180205           C1RcdNbr = C1CsrRrn;
069100180205         else;
069200180205           C1RcdNbr = 1;
069300180205         endif;
069400180205
069500180205         // - ?Emissione Testata & Piede con tasti funzionali abilitati?
069600180205         write  TB11T01;
069700180205         write  TB11P01;
069800180205
069900180205         // - ?Emissione subfile?
070000180205         exfmt  TB11C01;
070100180205
070200180205         clear  VIDmsg;
070300180205         clear  $ErrMessage;
070400180205         clear  $ErrGenerico;
070500180205
070600180205         Select;
070700180205
070800180205           // - ?F3=Fine?
070900180205           When  InfDspF.dsp_aid = c_F03;
071000180205             unlock  TNTBE01L;
071100180205             $Fine = *on;
071200180205             leavesr;
071300180206
071400180206           // - ?F8=Ordinamento per Applicazione / N° Postazione?
071500180206           When  InfDspF.dsp_aid = c_F08;
071600180206             $InzW01 = *on;
071700180206             $Video  = 'W1';
071800180205
071900180205           // - ?F9=Altra vista?
072000180205           When  InfDspF.dsp_aid = c_F09;
072100180205             $InzS05 = *on;
072200180205             $Video  = 'S5';
072300180205
072400180205           // - ?F12=Ritorno?
072500180313           When  InfDspF.dsp_aid = c_F12;
072600180205             unlock  TNTBE01L;
072700180205             $Video = 'D1';
072800180205             leavesr;
072900180205
073000180205           // - ?Invio / F6=Conferma?
073100180313           Other;
073200180205             // - ?Controllo dati?
073300180205             exsr  sr_CtrS01;
073400180205             select;
073500180205               // - ?Rilevato errore?
073600180205               when  $ErrGenerico = *on;
073700180205                 leavesr;
073800180205               // - ?F6=Conferma?
073900180205               when  InfDspF.dsp_aid = c_F06;
074000180205                 exsr  sr_WrtTNTBE;
074100180205                 if  NOT $ErrGenerico;
074200180205                   $Video = 'D1';
074300180205                   reset  $InzD01;
074400180205                   leavesr;
074500180205                 endif;
074600180205             endsl;
074700180205
074800180313         EndSl;
074900180205
075000180205       ENDSR;
075100180205
075200180205       //--------------------------------------------------------------
075300180205       //? Inizializzazione SubFile S01.                                ?
075400180205       //--------------------------------------------------------------
075500180205       BEGSR  sr_InzS01;
075600180205
075700180205         // - ?1ª apertura file tabelle del 1° S.I. (sede)?
075800180205         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
075900180205           w_SisInf = 1;
076000180205           exsr  sr_OpenFileTab;
076100180205         endif;
076200180205
076300180205         // - ?Pulizia subfile?
076400180205         clear  TB11S01;
076500180205         clear  TB11C01;
076600180205         $SflDsp_N    = *on;
076700180205         $SflDspCtl_N = *on;
076800180205         write  TB11C01;
076900180205         $SflDspCtl_N = *off;
077000180205         $SflEnd      = *off;
077100180205
077200180205         $SflNxtChg   = *off;
077300180205
077400180205         clear  C1RcdNbr;
077500180205         clear  C1CsrRrn;
077600180205         clear  S01nrr;
077700180205         clear  VIDmsg;
077800180205         $ErrMessage  = *off;
077900180205         $ErrGenerico = *off;
078000180205
078100180205         // - ?Spegnimento degli indicatori di errore?
078200180205         %subst( IndDspF : 50 ) = *off;
078300180205
078400180205         // - ?Impostazione dati nel subfile-control?
078500180205         C1Cfil = V1Cfil;
078600180205         C1Dfil = V1Dfil;
078700180205
078800180205         // - ?Ciclo di caricamento (completo) dei dati nel subfile?
078900180205         clear  S01nrr;
079000180205         $SflNxtChg = *off;
079100180205         clear  keyTNTBE01;
079200180205         k_TBEcod = c_Tab;
079300180205         k_TBEke1 = V1Cfil;
079400180205         setll     %kds( keyTNTBE01 )  TNTBE000;
079500180205         reade(N)  %kds( keyTNTBE01 : 2 )  TNTBE000;
079600180205         For  S01nrr = 1  To  c_SflSizMax;
079700180205           if  Not %eof(TNTBE01L);
079800180205             exsr  sr_CaricaS01;
079900180205             reade(N)  %kds( keyTNTBE01 : 2 )  TNTBE000;
080000180205           else;
080100180206             exsr  sr_RecVuotoS01;
080200180205           endif;
080300180205         EndFor;
080400180205
080500180205         // - ?Visualizzazione del SFL (se ci sono dati)?
080600180205         $SflDsp_N = (S01nrr <= *zero);
080700180205
080800180205         // - ?Attivazione del SFLEND?
080900180205         $SflEnd = *on;
081000180205
081100180205         // - ?Posizionamento cursore al 1° rcd della 1ª pagina?
081200180205         if  S01nrr > *zero;
081300180205           C1RcdNbr = 1;
081400180205         else;
081500180205           clear  C1RcdNbr;
081600180205         endif;
081700180205
081800180205         C1CsrRrn = C1RcdNbr;
081900180205
082000180205       ENDSR;
082100180205
082200180205       //--------------------------------------------------------------
082300180205       //? Caricamento singolo rec. di dati nel SubFile (da tab."MSL"). ?
082400180205       //--------------------------------------------------------------
082500180205       BEGSR  sr_CaricaS01;
082600180205
082700180205         dMSL   = TBEuni;
082800180205
082900180205         // - ?Eventuale caricamento dei record vuoti PRIMA di quello?
083000180205         //   ?con i dati in Input?
083100180205         wKe2 = %int( %trim( TBEke2 ) );
083200180205         DoW  S01nrr < wKe2;
083300180219           exsr  sr_RecVuotoS01;
083400180205           S01nrr += 1;
083500180205         EndDo;
083600180205
083700180205         // - ?Impostazione campi?
083800180205         clear  TB11S01;
083900180205
084000180205         // - ?Campi Hidden?
084100180205         S1Hdes = §MSLdes;
084200180205         S1Hde5 = §MSLde5;
084300180205         S1Hpep = §MSLpep;
084400180205         S1Hent = §MSLent;
084500180205         S1Hapl = §MSLapl;
084600180205         S1Hap5 = §MSLap5;
084700180205         S1Hpwf = §MSLpke;
084800180205         S1Hpke = §MSLpwf;
084900180205         S1Hann = TBEatb;
085000180205
085100180205         // - ?Campi I/O?
085200180205         S01des = §MSLdes;
085300180205         S01de5 = §MSLde5;
085400180205         evalR  S01pep = §MSLpep;
085500180205         S01ent = §MSLent;
085600180205         S01apl = §MSLapl;
085700180205         S01ap5 = §MSLap5;
085800180205         S01pke = §MSLpke;
085900180205         S01pwf = §MSLpwf;
086000180205         S01ann = TBEatb;
086100180205
086200180205         // - ?Attributi di Visualizzazione?
086300180205         $ProtAnnull = (S01ann <> *blank);
086400180205
086500180205         S01nrr = wKe2;
086600180205
086700180205         write  TB11S01;
086800180205
086900180205       ENDSR;
087000180205
087100180205       //--------------------------------------------------------------
087200180205       //? Caricamento singolo rec. vuoto nel SubFile.                  ?
087300180205       //--------------------------------------------------------------
087400180206       BEGSR  sr_RecVuotoS01;
087500180205
087600180205         clear  TB11S01;
087700180205
087800180205         write  TB11S01;
087900180205
088000180205       ENDSR;
088100180206
088200180206       //--------------------------------------------------------------
088300180206       //? Controllo dati nel SubFile S01.                              ?
088400180206       //--------------------------------------------------------------
088500180206       BEGSR  sr_CtrS01;
088600180206
088700180309         // - ?Memorizzazione nrr del sfl con la 1ª opzione per?
088800180309         //   ?riposizionarvici il cursore dopo il tasto "Invio"?
088900180309         if (SavS01csr = *zeros   or   SavS01csr <> C1CsrRrn);
089000180309           SavS01csr   = C1CsrRrn;
089100180309         endif;
089200180206
089300180206         // - ?Ciclo di lettura del SubFile?
089400180206         clear  sk_APL;
089500180206         clear  sk_PEP;
089600180206         clear  sk_ENT;
089700180206         clear  sk_ENTxMTP;
089800180206         clear  S01nrr;
089900180208         clear  RrnLast;
090000180206
090100180206         For  S01nrr = 1  To  c_SflSizMax;
090200180206
090300180206           chain  S01nrr  TB11S01;
090400180206           if  NOT %found( TNTB11D );
090500180206             leave;
090600180206           endif;
090700180206
090800180206           // - ?Controllo dati nel singolo record del subfile?
090900180206           clear  VIDmsg;
091000180206           exsr  sr_CtrRecS01;
091100180206
091200180206           // - ?Posizionamento del cursore?
091300180206           //*·$PosCurOPZ = $ErrGenerico;
091400180206           if  $ErrGenerico  or  $ErrMessage;
091500180206             C1CsrRrn = S01nrr;
091600180206           endif;
091700180206
091800180206           // - ?Attributi di Visualizzazione?
091900180206           $ProtAnnull = (S01ann <> *blank);
092000180206
092100180206           // - ?Aggiornamento SubFile?
092200180309           if  NOT $InzS01;
092300180206           update  TB11S01;
092400180309           endif;
092500180206
092600180206           if  $ErrGenerico  or  $ErrMessage;
092700180206             leave;
092800180206           endif;
092900180206
093000180206         EndFor;
093100180301
093200180301         // - ?Controllo corretta sequenza delle postazioni?
093300180309         if  NOT $InzS01       and
093400180309             NOT $ErrMessage   and   NOT $ErrGenerico;
093500180301           exsr  sr_CtrlSeqPostaz;
093600180301         endif;
093700180206
093800180206         // - ?Riposizionam. cursore sul record in cui era posizionato?
093900180206         //   ?(se non sono stati rilevati errori)?
094000180206         if  NOT $ErrMessage   and   NOT $ErrGenerico   and
094100180206             SavS01csr > *zero;
094200180206           C1CsrRrn = SavS01csr;
094300180206         endif;
094400180206
094500180206       ENDSR;
094600180206
094700180206       //--------------------------------------------------------------
094800180206       //? Controllo dati nel singolo record del SubFile S01.           ?
094900180206       //--------------------------------------------------------------
095000180206       BEGSR  sr_CtrRecS01;
095100180206
095200180206         // - ?Spegnimento degli indicatori di errore?
095300180206         %subst(IndDspF : 50) = *off;
095400180206
095500180206         // · ?Ricerca sul 1° campo Tipo Applicazione (FV 2)?
095600180206         If  %scan('?' : S01apl) > *zero;
095700180206
095800180206           if  S01opz <> *blank;
095900180206             $ErrGenerico = *on;
096000180206             $ErrMessage  = *on;
096100180206             $PosCurOPZ   = *on;
096200180206             VIDmsg = sk_Msg(15);
096300180206             leavesr;
096400180206           endif;
096500180206
096600180206           clear  TIBS02ds;
096700180206           T02mod = 'R';
096800180206           T02sif = kNSif;
096900180206           T02cod = 'MTP';
097000180206           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
097100180206           $ErrGenerico = *on;
097200180206           $PosCurAPL   = *on;
097300180206           if  T02err = *blank;
097400180206             S01apl = T02ke1;
097500180206           else;
097600180206             $ErrMessage  = *on;
097700180206             VIDmsg = T02msg;
097800180206             leavesr;
097900180206           endif;
098000180206
098100180206         EndIf;
098200180206
098300180206         // · ?Ricerca sul 2° campo Tipo Applicazione (FV 5)?
098400180206         If  %scan('?' : S01ap5) > *zero;
098500180206
098600180206           if  S01opz <> *blank;
098700180206             $ErrGenerico = *on;
098800180206             $ErrMessage  = *on;
098900180206             $PosCurOPZ   = *on;
099000180206             VIDmsg = sk_Msg(15);
099100180206             leavesr;
099200180206           endif;
099300180206
099400180206           clear  TIBS02ds;
099500180206           T02mod = 'R';
099600180206           T02sif = kNSif;
099700180206           T02cod = 'MTP';
099800180206           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
099900180206           $ErrGenerico = *on;
100000180206           $PosCurAP5   = *on;
100100180206           if  T02err = *blank;
100200180206             S01ap5 = T02ke1;
100300180206           else;
100400180206             $ErrMessage  = *on;
100500180206             VIDmsg = T02msg;
100600180206             leavesr;
100700180206           endif;
100800180206
100900180206         EndIf;
101000180206
101100180206         // - ?Controllo dati nel singolo record?
101200180206         SELECT;
101300180206
101400180206           // - ?Opz. 4 = ANNULLAMENTO?
101500180206           WHEN  S01opz = '4';
101600180206             if  S01ann = 'A';
101700180206               $ErrGenerico = *on;
101800180206               $ErrMessage  = *on;
101900180206               $PosCurOPZ   = *on;
102000180206               VIDmsg = sk_Msg(12);
102100180206               leavesr;
102200180206             endif;
102300180206             clear  S01opz;
102400180206             S01ann = 'A';
102500180206             $ProtAnnull = *on;
102600180208             RrnLast = S01nrr;
102700180206
102800180206           // - ?Opz. 5 = RIPRISTINO?
102900180206           WHEN  S01opz = '5';
103000180206             if  S01ann = *blank;
103100180206               $ErrGenerico = *on;
103200180206               $ErrMessage  = *on;
103300180206               $PosCurOPZ   = *on;
103400180206               VIDmsg = sk_Msg(13);
103500180206               leavesr;
103600180206             endif;
103700180206             clear  S01opz;
103800180206             clear  S01ann;
103900180206             $ProtAnnull = *off;
104000180208             RrnLast = S01nrr;
104100180206
104200180206           // - ?Opz. Errata?
104300180206           WHEN  S01opz <> *blank  and  S01opz <> '4'
104400180206                                   and  S01opz <> '5';
104500180206             $ErrGenerico = *on;
104600180206             $ErrMessage  = *on;
104700180206             $PosCurOPZ   = *on;
104800180206             VIDmsg = sk_Msg(14);
104900180206             leavesr;
105000180206
105100180206           // - ?Record con tutti i dati?
105200180206           WHEN  S01des <> *blank  and  S01de5 <> *blank  and
105300180206                 S01ent <> *zero   and  S01pep <> *blank  and
105400180206                 S01apl <> *blank  and  S01ap5 <> *blank;
105500180206
105600180206             // - ?S01pep (se valorizzato) DEVE avere un valore univoco?
105700180206             If  ( S01opz <> '4'  and  S01ann = *blank )  or
105800180206                 ( S01opz =  '5'  and  S01ann = 'A' );
105900180206               xx = %lookup( S01pep : sk_PEP );
106000180206               if  xx > *zero;
106100180206                 $ErrGenerico = *on;
106200180206                 $ErrMessage  = *on;
106300180206                 $PosCurPEP   = *on;
106400180206                 VIDmsg =  %trimR( sk_Msg(05) ) + ' ' +
106500180206                           %trim( %editc( xx : '3' ) );
106600180206                 leavesr;
106700180206               endif;
106800180206               xx = %lookup( *blank : sk_PEP );
106900180206               if  xx > *zero;
107000180206                 sk_PEP(xx) = S01pep;
107100180206               endif;
107200180206             EndIf;
107300180206
107400180206             // - ?S01apl (se inserito) DEVE avere un valore valido,?
107500180206             //   ?esistente in tab. "MTP" (Tipo Applicazione)?
107600180206             If  S01apl <> *blank;
107700180206
107800180206               clear  TIBS02ds;
107900180206               T02mod = 'C';
108000180206               T02sif = kNSif;
108100180206               T02cod = 'MTP';
108200180206               T02ke1 = S01apl;
108300180206               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
108400180206               if  T02err <> *blank;
108500180206                 $ErrGenerico = *on;
108600180206                 $ErrMessage  = *on;
108700180206                 $PosCurAPL   = *on;
108800180206                 VIDmsg =  sk_Msg(07);
108900180206                 leavesr;
109000180206               endif;
109100180206               // - ?Controllo della validità dell'applicazione per il VDL?
109200180206               dMTP = T02uni;
109300180206               if  §MTPswv = 'N';
109400180206                 $ErrGenerico = *on;
109500180206                 $ErrMessage  = *on;
109600180206                 $PosCurAPL   = *on;
109700180206                 VIDmsg =  sk_Msg(08);
109800180206                 leavesr;
109900180206               endif;
110000180206
110100180206               // - ?Intabellamento dei Numeri Postazione usati per?
110200180206               //   ?Tipo Applicazione?
110300180206               xx = %lookup( S01apl : sk_MTP );
110400180206               // - ?I Tipi Applicazione sono già stati intabellati:?
110500180206               //   ?questo lo trova SICURO!?
110600180206               If  xx > *zero;
110700180206                 ds_ENT = sk_ENTxMTP(xx);
110800180206                 if  sk_ENT( S01ent ) <> *blank;
110900180206                   $ErrGenerico = *on;
111000180206                   $ErrMessage  = *on;
111100180206                   $PosCurENT   = *on;
111200180206                   VIDmsg =  %trimR( sk_Msg(06) ) +
111300180206                             %trim( sk_ENT( S01ent ) ) + '"';
111400180206                   leavesr;
111500180206                 endif;
111600180206                 sk_ENT( S01ent ) = S01pep;
111700180206                 sk_ENTxMTP(xx)   = ds_ENT;
111800180206               EndIf;
111900180206
112000180206               // - ?Memorizzazione Applicazioni utilizzate?
112100180206               If  %lookup( S01apl : sk_APL ) = *zero;
112200180206                 xx = %lookup( *blank : sk_APL );
112300180206                 if  xx > *zero;
112400180206                   sk_APL(xx) = S01apl;
112500180206                 endif;
112600180206               EndIf;
112700180206
112800180206             EndIf;
112900180206
113000180206             // - ?S01ap5 (se inserito) DEVE avere un valore valido,?
113100180206             //   ?esistente in tab. "MTP" (Tipo Applicazione)?
113200180206             If  S01ap5 <> *blank;
113300180206
113400180206               clear  TIBS02ds;
113500180206               T02mod = 'C';
113600180206               T02sif = kNSif;
113700180206               T02cod = 'MTP';
113800180206               T02ke1 = S01ap5;
113900180206               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
114000180206               if  T02err <> *blank;
114100180206                 $ErrGenerico = *on;
114200180206                 $ErrMessage  = *on;
114300180206                 $PosCurAP5   = *on;
114400180206                 VIDmsg =  sk_Msg(07);
114500180206                 leavesr;
114600180206               endif;
114700180206               // - ?Controllo della validità dell'applicazione per il VDL?
114800180206               dMTP = T02uni;
114900180206               if  §MTPswv = 'N';
115000180206                 $ErrGenerico = *on;
115100180206                 $ErrMessage  = *on;
115200180206                 $PosCurAP5   = *on;
115300180206                 VIDmsg =  sk_Msg(08);
115400180206                 leavesr;
115500180206               endif;
115600180206
115700180206               // - ?Intabellamento dei Numeri Postazione usati per?
115800180206               //   ?Tipo Applicazione?
115900180206               //   ?(SE quello in FV5 è diverso da quello in FV2)?
116000180206               if  S01ap5 <> S01apl;
116100180206
116200180206                 xx = %lookup( S01ap5 : sk_MTP );
116300180206                 // - ?I Tipi Applicazione sono già stati intabellati:?
116400180206                 //   ?questo lo trova SICURO!?
116500180206                 If  xx > *zero;
116600180206                   ds_ENT = sk_ENTxMTP(xx);
116700180206                   if  sk_ENT( S01ent ) <> *blank;
116800180206                     $ErrGenerico = *on;
116900180206                     $ErrMessage  = *on;
117000180206                     $PosCurENT   = *on;
117100180206                     VIDmsg =  %trimR( sk_Msg(06) ) +
117200180206                               %trim( sk_ENT( S01ent ) ) + '"';
117300180206                     leavesr;
117400180206                   endif;
117500180206                   sk_ENT( S01ent ) = S01pep;
117600180206                   sk_ENTxMTP(xx)   = ds_ENT;
117700180206                 EndIf;
117800180206
117900180206               endif;
118000180206
118100180206               // - ?Memorizzazione Applicazioni utilizzate?
118200180206               If  %lookup( S01ap5 : sk_APL ) = *zero;
118300180206                 xx = %lookup( *blank : sk_APL );
118400180206                 if  xx > *zero;
118500180206                   sk_APL(xx) = S01ap5;
118600180206                 endif;
118700180206               EndIf;
118800180206
118900180206             EndIf;
119000180206
119100180206             // - ?Controllo che il flag "Postazione entrata picking" sia?
119200180206             //   ?impostato "S" SOLO per applicazione di FV 2 = "K"?
119300180206             if  S01pke = 'S'  and  S01apl <> 'K';
119400180206               $ErrGenerico = *on;
119500180206               $ErrMessage  = *on;
119600180206               $PosCurPKE   = *on;
119700180206               VIDmsg =  sk_Msg(09);
119800180206               leavesr;
119900180206             endif;
120000180208
120100180208             RrnLast = S01nrr;
120200180206
120300180206           // - ?Record senza nessun dato?
120400180206           WHEN  S01des = *blank  and  S01de5 = *blank  and
120500180206                 S01ent = *zero   and  S01pep = *blank  and
120600180206                 S01apl = *blank  and  S01ap5 = *blank;
120700180206             // - ?Controllo se è stato tolto un record già inserito?
120800180206             If  S1Hdes <> *blank  and  S1Hde5 <> *blank  and
120900180206                 S1Hent <> *zero   and  S1Hpep <> *blank  and
121000180206                 S1Hapl <> *blank  and  S1Hap5 <> *blank;
121100180206               // - ?Reimpostazione a video dei dati già inseriti?
121200180206               S01des = S1Hdes;
121300180206               S01de5 = S1Hde5;
121400180206               evalR  S01pep = S1Hpep;
121500180206               S01ent = S1Hent;
121600180206               S01apl = S1Hapl;
121700180206               S01ap5 = S1Hap5;
121800180206               S01pke = S1Hpke;
121900180206               S01pwf = S1Hpwf;
122000180206               S01ann = S1Hann;
122100180206               $ErrGenerico = *on;
122200180206               $ErrMessage  = *on;
122300180206               $PosCurDES   = *on;
122400180206               VIDmsg =  sk_Msg(10);
122500180206               leavesr;
122600180206             EndIf;
122700180206
122800180206           // - ?Qualche campo è vuoto?
122900180206           OTHER;
123000180206             $ErrGenerico = *on;
123100180206             $ErrMessage  = *on;
123200180206             $PosCurDES   = *on;
123300180206             VIDmsg = sk_Msg(11);
123400180206             leavesr;
123500180206
123600180206         ENDSL;
123700180206
123800180206       ENDSR;
123900180301
124000180301       //--------------------------------------------------------------
124100180301       //? Controllo corretta sequenza delle Postazioni.                ?
124200180301       //--------------------------------------------------------------
124300180301       BEGSR  sr_CtrlSeqPostaz;
124400180301
124500180301         For  S01nrr = 1  To  c_SflSizMax;
124600180301
124700180301           chain  S01nrr  TB11S01;
124800180301           if  NOT %found( TNTB11D );
124900180301             leave;
125000180301           endif;
125100180301
125200180301           select;
125300180301             // - ?Ovviamente si scarta l'Entrata n° 1?
125400180301             when  S01ent = 1;
125500180301               iter;
125600180301             // - ?Ovviamente si scartano i rec. vuoti del sfl?
125700180301             when  S01ent <= *zero;
125800180301               iter;
125900180301           endsl;
126000180301
126100180301           // - ?Ctrl dei Numeri Entrata usati per Tipo Applicazione?
126200180301           xx = %lookup( S01apl : sk_MTP );
126300180301           // - ?I Tipi Applicazione sono già stati intabellati:?
126400180301           //   ?questo lo trova SICURO!?
126500180301           If  xx > *zero;
126600180301             ds_ENT = sk_ENTxMTP(xx);
126700180301             if  sk_ENT( S01ent - 1 ) = *blank;
126800180301               if  sk_MemoErrENT( S01nrr ) = *off;
126900180301                 sk_MemoErrENT( S01nrr ) = *on;
127000180301                 // - ?Segnalazione errore?
127100180301                 $ErrGenerico = *on;
127200180301                 $ErrMessage  = *on;
127300180301                 $PosCurENT   = *on;
127400180301                 VIDmsg = sk_Msg(16);
127500180301                 // - ?Posizionamento del cursore?
127600180301                 if  $ErrGenerico  or  $ErrMessage;
127700180301                   C1CsrRrn = S01nrr;
127800180301                 endif;
127900180301                 // - ?Attributi di Visualizzazione?
128000180301                 $ProtAnnull = (S01ann <> *blank);
128100180301                 // - ?Aggiornamento SubFile?
128200180301                 update  TB11S01;
128300180301                 leavesr;
128400180301               endif;
128500180301             else;
128600180301               clear  sk_MemoErrENT( S01nrr );
128700180301             endif;
128800180301           EndIf;
128900180301
129000180301         EndFor;
129100180301
129200180301       ENDSR;
129300180205
129400180205       //--------------------------------------------------------------
129500180205       //? Visualizzazione postazioni finora gestite.                   ?
129600180205       //--------------------------------------------------------------
129700180205       BEGSR  sr_GesS05;
129800180205
129900180205         // - ?Inizializzazione videata?
130000180205         if  $InzS05;
130100180205           exsr  sr_InzS05;
130200180205           $InzS05 = *off;
130300180205         endif;
130400180205
130500180205         // - ?(Dis)Abilitazione tasti funzionali?
130600180205         $F6Attivo  = *off;
130700180205         $F8Attivo  = *off;
130800180205         $F9Attivo  = *off;
130900180205         $F12Attivo = *on;
131000180205
131100180205         // - ?Posizionamento cursore?
131200180205         if  C5CsrRrn > *zero;
131300180205           C5RcdNbr = C5CsrRrn;
131400180205         else;
131500180205           C5RcdNbr = 1;
131600180205         endif;
131700180205
131800180206         // - ?Emissione Testata & Piede con tasti funzionali abilitati?
131900180206         write  TB11T01;
132000180205         write  TB11P01;
132100180205
132200180205         // - ?Emissione videata?
132300180205         exfmt  TB11C05;
132400180205
132500180205         clear  VIDmsg;
132600180205         clear  $ErrMessage;
132700180205         clear  $ErrGenerico;
132800180205
132900180205         Select;
133000180205
133100180205           // - ?F3=Fine?
133200180205           When  InfDspF.dsp_aid = c_F03;
133300180205             unlock  TNTBE01L;
133400180205             $Fine = *on;
133500180205             leavesr;
133600180205
133700180205           // - ?F12=Ritorno?
133800180205           When  InfDspF.dsp_aid = c_F12;
133900180205             $Video = 'S1';
134000180205             leavesr;
134100180205
134200180205         EndSl;
134300180205
134400180205       ENDSR;
134500180205
134600180205       //--------------------------------------------------------------
134700180205       //? Preparazione videata con le postazioni finora gestite.       ?
134800180205       //--------------------------------------------------------------
134900180205       BEGSR  sr_InzS05;
135000180205
135100180205         // - ?Pulizia subfile?
135200180205         $SflDsp5N    = *on;
135300180205         $SflDspCtl5N = *on;
135400180205         write  TB11C05;
135500180205         $SflDspCtl5N = *off;
135600180205         $SflEnd5     = *off;
135700180205
135800180205         // - ?Caricamento subfile?
135900180205         //   ?(da S01 a S05)?
136000180205         S05nrr = 1;
136100180205         chain  S05nrr  TB11S01;
136200180205
136300180205         DoW  %found( TNTB11D );
136400180205
136500180205           clear  TB11S05;
136600180205
136700180205           if  S01ann = *blank;
136800180205
136900180205             S05des = S01des;
137000180205             S05pep = S01pep;
137100180205             S05ent = S01ent;
137200180205             S05apl = S01apl;
137300180205             S05ap5 = S01ap5;
137400180205             S05pke = S01pke;
137500180205             S05pwf = S01pwf;
137600180205             //*·S05ann = S01ann;
137700180205
137800180205           endif;
137900180205
138000180205           write  TB11S05;
138100180205
138200180205           S05nrr += 1;
138300180205           chain  S05nrr  TB11S01;
138400180205
138500180205         EndDo;
138600180205
138700180205         // - ?Visualizzazione del SFL (se ci sono dati)?
138800180205         $SflDsp5N = (S05nrr <= *zero);
138900180205
139000180205         // - ?Attivazione del SFLEND?
139100180205         $SflEnd5 = *on;
139200180205
139300180205         // - ?Posizionamento cursore al 1° rcd della 1ª pagina?
139400180205         if  S05nrr > *zero;
139500180205           C5RcdNbr = 1;
139600180205         else;
139700180205           clear  C5RcdNbr;
139800180205         endif;
139900180205
140000180205         C5CsrRrn = C5RcdNbr;
140100180205
140200180205       ENDSR;
140300180206
140400180206       //--------------------------------------------------------------
140500180206       //? Gestione window W01.                                         ?
140600180206       //--------------------------------------------------------------
140700180206       BEGSR  sr_GesW01;
140800180206
140900180206         // - ?Inizializzazione videata?
141000180206         if  $InzW01;
141100180206           exsr  sr_InzW01;
141200180206           $InzW01 = *off;
141300180206         endif;
141400180206
141500180213         // - ?Emissione window?
141600180219         if  $OrdAppl02   or
141700180219             $ErrGenerico;
141800180219           exfmt  TB11W01;
141900180219         endif;
142000180206
142100180206         clear  VIDmsg;
142200180206         clear  $ErrMessage;
142300180206         clear  $ErrGenerico;
142400180206
142500180206         // - ?F12=Ritorno?
142600180206         if  InfDspF.dsp_aid = c_F12;
142700180206           $Video = 'S1';
142800180206           leavesr;
142900180206         endif;
143000180206
143100180213         // - ?Controlli?
143200180206         exsr  sr_CtrW01;
143300180213         if  $ErrGenerico = *on  or
143400180219             InfDspF.dsp_aid <> c_F08;
143500180206           leavesr;
143600180206         endif;
143700180213
143800180213         // - ?Impostazione sequenza di ordinamento in ogni rec del sfl?
143900180213         exsr  sr_Set_Ord_Sfl;
144000180213
144100180213         // - ?Ordinamento del subfile?
144200180208         exsr  sr_SortSfl;
144300180206
144400180213         // - ?Ritorno alla gestione del subfile?
144500180206         $Video  = 'S1';
144600180206
144700180206       ENDSR;
144800180206
144900180206       //--------------------------------------------------------------
145000180206       //? Inizializzazione window W01.                                 ?
145100180206       //--------------------------------------------------------------
145200180206       BEGSR  sr_InzW01;
145300180206
145400180206         clear  TB11W01;
145500180206
145600180206         // - ?Caricamento Applicazioni?
145700180206         W1Capl1 = sk_APL(01);
145800180219         W1Capl2 = sk_APL(02);
145900180219         W1Capl3 = sk_APL(03);
146000180219         W1Capl4 = sk_APL(04);
146100180219         W1Capl5 = sk_APL(05);
146200180219         W1Capl6 = sk_APL(06);
146300180206
146400180206         // - ?Decodifica Applicazioni?
146500180206         For  xx = 1  To  6;
146600180206
146700180206           clear  yy;
146800180206           If  sk_APL(xx) <> *blank;
146900180206
147000180206             yy = %lookup( sk_APL(xx) : sk_MTP );
147100180206             if  yy > *zero;
147200180206
147300180206               select;
147400180206                 when  xx  =  1;
147500180206                   W1Dapl1 = sk_MTPdes(yy);
147600180206                   W1Nseq1 = xx;
147700180206                 when  xx  =  2;
147800180206                   W1Dapl2 = sk_MTPdes(yy);
147900180206                   W1Nseq2 = xx;
148000180206                   $OrdAppl02 = *on;
148100180206                 when  xx  =  3;
148200180206                   W1Dapl3 = sk_MTPdes(yy);
148300180206                   W1Nseq3 = xx;
148400180206                   $OrdAppl03 = *on;
148500180206                 when  xx  =  4;
148600180206                   W1Dapl4 = sk_MTPdes(yy);
148700180206                   W1Nseq4 = xx;
148800180206                   $OrdAppl04 = *on;
148900180206                 when  xx  =  5;
149000180206                   W1Dapl5 = sk_MTPdes(yy);
149100180206                   W1Nseq5 = xx;
149200180206                   $OrdAppl05 = *on;
149300180206                 when  xx  =  6;
149400180206                   W1Dapl6 = sk_MTPdes(yy);
149500180206                   W1Nseq6 = xx;
149600180206                   $OrdAppl06 = *on;
149700180206               endsl;
149800180206
149900180206             endif;
150000180206
150100180206           EndIf;
150200180206
150300180206         EndFor;
150400180206
150500180206       ENDSR;
150600180206
150700180206       //--------------------------------------------------------------
150800180206       //? Controllo dati immessi nella window W01.                     ?
150900180206       //--------------------------------------------------------------
151000180206       BEGSR  sr_CtrW01;
151100180206
151200180206         // - ?Spegnimento indicatori di posizionamento cursore?
151300180219         %subst( IndDspF : 50 ) = *off;
151400180206
151500180206         xx = %lookup( *blank : sk_APL );
151600180206         if  xx = *zero;
151700180206           xx = %elem( sk_APL );
151800180206         endif;
151900180206
152000180206         eval  sk_Ord = *hival;
152100180206         sk_Ord(01) = %editc( W1Nseq1 : 'X' ) + W1Capl1;
152200180206         if  $OrdAppl02;
152300180206           sk_Ord(02) = %editc( W1Nseq2 : 'X' ) + W1Capl2;
152400180206         endif;
152500180206         if  $OrdAppl03;
152600180206           sk_Ord(03) = %editc( W1Nseq3 : 'X' ) + W1Capl3;
152700180206         endif;
152800180206         if  $OrdAppl04;
152900180206           sk_Ord(04) = %editc( W1Nseq4 : 'X' ) + W1Capl4;
153000180206         endif;
153100180206         if  $OrdAppl05;
153200180206           sk_Ord(05) = %editc( W1Nseq5 : 'X' ) + W1Capl5;
153300180206         endif;
153400180206         if  $OrdAppl06;
153500180206           sk_Ord(06) = %editc( W1Nseq6 : 'X' ) + W1Capl6;
153600180206         endif;
153700180206
153800180206       ENDSR;
153900180206
154000180206       //--------------------------------------------------------------
154100180206       //? Inizializzazione window W01.                                 ?
154200180206       //--------------------------------------------------------------
154300180206       BEGSR  sr_InzWxx;
154400180206
154500180206         clear  TB11W01;
154600180206
154700180206         // - ?Caricamento Applicazioni?
154800180206         W1Capl1 = §MFPapl;
154900180219         W1Capl2 = §MFPap2;
155000180219         W1Capl3 = §MFPap3;
155100180219         W1Capl4 = §MFPap4;
155200180219         W1Capl5 = §MFPap5;
155300180219         W1Capl6 = §MFPap6;
155400180206
155500180206         // - ?Decodifica Applicazioni?
155600180206         For  xx = 1  To  6;
155700180206
155800180206           clear  wAPL;
155900180206           select;
156000180206             when  xx = 1;
156100180213               wAPL = §MFPapl;
156200180206             when  xx = 2;
156300180213               wAPL = §MFPap2;
156400180206             when  xx = 3;
156500180213               wAPL = §MFPap3;
156600180206             when  xx = 4;
156700180213               wAPL = §MFPap4;
156800180206             when  xx = 5;
156900180213               wAPL = §MFPap5;
157000180206             when  xx = 6;
157100180213               wAPL = §MFPap6;
157200180206           endsl;
157300180206           clear  yy;
157400180206           If  wAPL <> *blank;
157500180206             yy = %lookup( wAPL : sk_MTP );
157600180206             if  yy <= *zero;
157700180206               select;
157800180206                 when  xx  =  1;
157900180206                   W1Dapl1 = sk_MTPdes(yy);
158000180206                   W1Nseq1 = xx;
158100180206                 when  xx  =  2;
158200180206                   W1Dapl2 = sk_MTPdes(yy);
158300180206                   W1Nseq2 = xx;
158400180206                   $OrdAppl02 = *on;
158500180206                 when  xx  =  3;
158600180206                   W1Dapl3 = sk_MTPdes(yy);
158700180206                   W1Nseq3 = xx;
158800180206                   $OrdAppl03 = *on;
158900180206                 when  xx  =  4;
159000180206                   W1Dapl4 = sk_MTPdes(yy);
159100180206                   W1Nseq4 = xx;
159200180206                   $OrdAppl04 = *on;
159300180206                 when  xx  =  5;
159400180206                   W1Dapl5 = sk_MTPdes(yy);
159500180206                   W1Nseq5 = xx;
159600180206                   $OrdAppl05 = *on;
159700180206                 when  xx  =  6;
159800180206                   W1Dapl6 = sk_MTPdes(yy);
159900180206                   W1Nseq6 = xx;
160000180206                   $OrdAppl06 = *on;
160100180206               endsl;
160200180206             endif;
160300180206           EndIf;
160400180206
160500180206         EndFor;
160600180206
160700180206       ENDSR;
160800180213
160900180213       //--------------------------------------------------------------
161000180213       //? Impostazione sequenza di ordinamento nel subfile.            ?
161100180213       //--------------------------------------------------------------
161200180213       BEGSR  sr_Set_Ord_Sfl;
161300180213
161400180213         sortA  sk_Ord;
161500180213
161600180213         For  S01nrr = 1  To  RrnLast;
161700180213
161800180213            chain  S01nrr  TB11S01;
161900180213
162000180213            If  %found( TNTB11D );
162100180213
162200180213              Select;
162300180213                When  S01apl = *blank;
162400180213                  S1Hseq = *hival;
162500180213                When  S01ann = 'A';
162600180213                  S1Hseq = *hival;
162700180213                Other;
162800180213                  S1Hseq = *hival;
162900180213                  for  xx = 1  To  %elem( sk_Ord );
163000180213                    if  %subst( sk_Ord( xx ) : 2 : 1 ) = S01apl;
163100180213                      S1Hseq = %int( %subst( sk_Ord( xx ) : 1 : 1 ) );
163200180219                      leave;
163300180213                    endif;
163400180213                  endfor;
163500180213              EndSl;
163600180213
163700180213              update  TB11S01;
163800180213
163900180213            EndIf;
164000180213
164100180213         EndFor;
164200180213
164300180213       ENDSR;
164400180206
164500180206       //--------------------------------------------------------------
164600180206       //? Ordinamento SubFile S01.                                     ?
164700180208       //?  This subroutine sorts the subfile records.                 ?
164800180208       //--------------------------------------------------------------
164900180208       BEGSR  sr_SortSfl;
165000180208
165100180213         // - ?Impostazione NRR dell'ultimo record del subfile          ?
165200180213         //   ?(già fatto in sr_CtrS01)                                 ?
165300180208         //*·RrnLast  = S01nrr;
165400180208
165500180208         C1RcdNbr = 1;
165600180208         clear  C1CsrRrn;
165700180208         clear  S01nrr;
165800180208         clear  VIDmsg;
165900180208         $SflNxtChg   = *off;
166000180208         $ErrMessage  = *off;
166100180208         $ErrGenerico = *off;
166200180208         %subst(IndDspF : 50) = *off;
166300180208
166400180208         //?___________________________________________________________?
166500180213         // ?Initialize the key fields to sort on.?
166600180213         // ?There is one extra field not in the subfile -- Selected --?
166700180213         // ?that is added to the record. This field is used to place?
166800180213         // ?selected records in front of nonselected records.?
166900180208         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
167000180208
167100180208         clear  QLGKL;
167200180208         clear  QLGSKL;
167300180208         clear  QLGSCB;
167400180208         clear  QLGSCB00;
167500180208
167600180219         // - ?Ordinamento per Nr. Sequenza in ordinamento (da window) /?
167700180219         //                   ?Tipo Applicazione FV2 /?
167800180219         //                   ?Nr. Sequenza a video /?
167900180219         //                   ?Postazione Entrata /?
168000180219         //                   ?Tipo Applicazione FV5?
168100180219         // - ?5 campi chiave?
168200180219         QLGNBRK  = 5;
168300180219         // - ?1° campo: Numero Sequenza del Tipo Applicazione (SH1seq)?
168400180219         //             ?a posizione   1,  1 byte, num., ascending.?
168500180219         QLGSP    = 1;
168600180219         QLGSS    = %size(S1Hseq);
168700180219         QLGDT    = c_Numero;
168800180219         QLGSO    = c_Ascendente;
168900180219         QLGKL(1) = QLGSKL;
169000180219         // - ?2° campo: Tipo Applicazione (S01apl)?
169100180219         //             ?a posizione  76,  1 byte, char, ascending.?
169200180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
169300180219                    %size(S1Hde5)   + %size(S1Hpep)   +
169400180219                    %size(S1Hent)   + %size(S1Hapl)   +
169500180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
169600180219                    %size(S1Hpke)   + %size(S1Hann)   +
169700180219                    %size(S01opz)   + %size(S01des)   +
169800180219                    %size(S01de5)   + %size(S01pep)   +
169900180219                    %size(S01ent)   + 1;
170000180219         QLGSS    = %size(S01apl);
170100180219         QLGDT    = c_Carattere;
170200180219         QLGSO    = c_Ascendente;
170300180219         QLGKL(2) = QLGSKL;
170400180219         // - ?3° campo: Numero Entrata (S01ent)?
170500180219         //             ?a posizione  74,  2 byte, num., ascending.?
170600180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
170700180219                    %size(S1Hde5)   + %size(S1Hpep)   +
170800180219                    %size(S1Hent)   + %size(S1Hapl)   +
170900180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
171000180219                    %size(S1Hpke)   + %size(S1Hann)   +
171100180219                    %size(S01opz)   + %size(S01des)   +
171200180219                    %size(S01de5)   + %size(S01pep)   + 1;
171300180219         QLGSS    = %size(S01ent);
171400180219         QLGDT    = c_Numero;
171500180219         QLGSO    = c_Ascendente;
171600180219         QLGKL(3) = QLGSKL;
171700180219         // - ?4° campo: Postazione Entrata (S01pep)?
171800180219         //             ?a posizione  64,  1 byte, char, ascending.?
171900180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
172000180219                    %size(S1Hde5)   + %size(S1Hpep)   +
172100180219                    %size(S1Hent)   + %size(S1Hapl)   +
172200180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
172300180219                    %size(S1Hpke)   + %size(S1Hann)   +
172400180219                    %size(S01opz)   + %size(S01des)   +
172500180219                    %size(S01de5)   + 1;
172600180219         QLGSS    = %size(S01pep);
172700180219         QLGDT    = c_Carattere;
172800180219         QLGSO    = c_Ascendente;
172900180219         QLGKL(4) = QLGSKL;
173000180219         // - ?5° campo: Tipo Applicazione (S01ap5)?
173100180219         //             ?a posizione  76,  1 byte, char, ascending.?
173200180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
173300180219                    %size(S1Hde5)   + %size(S1Hpep)   +
173400180219                    %size(S1Hent)   + %size(S1Hapl)   +
173500180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
173600180219                    %size(S1Hpke)   + %size(S1Hann)   +
173700180219                    %size(S01opz)   + %size(S01des)   +
173800180219                    %size(S01de5)   + %size(S01pep)   +
173900180219                    %size(S01ent)   + %size(S01apl) + 1;
174000180219         QLGSS    = %size(S01ap5);
174100180219         QLGDT    = c_Carattere;
174200180219         QLGSO    = c_Ascendente;
174300180219         QLGKL(5) = QLGSKL;
174400180208
174500180219         // - ?Load other sort parameters.?
174600180208         QLGLB   = 80 + 16 * c_MaxKey;
174700180208         QLGRL   = %size(SflRcd) - 1;
174800180208         QLGRT   = 8;
174900180208         QLGOKL  = 80;
175000180208         QLGLKE  = 16;
175100180208         QLGLSS  = 290;
175200180219         // - ?Initialize Sort I/O API fields.?
175300180208         QLGRL00 = QLGRL;
175400180208         QLGRC00 = 1;
175500180208         clear  QUSEI;
175600180208         QUSBPRV = %size(QUSEC);
175700180208
175800180219         // - ?First step - Initialize the sort routine.?
175900180208         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
176000180208                      SizeList : ReturnSize : QUSEC );
176100180208
176200180219         // - ?Next step - Write records to I/O routine.?
176300180208         QLGRT00  = c_Put;
176400180208         For  Nrr = 1  To  RrnLast;
176500180219           chain  Nrr  TB11S01;
176600180219           // - ?Solo le righe con Selected = 'Y' sono riordinate,?
176700180219           //   ?quindi per fare un ordinamento di tutte le righe?
176800180219           //   ?metto 'Y' sempre.?
176900180208           Selected = 'Y';
177000180208           clear  QUSEI;
177100180208           QUSBPRV  = %size(QUSEC);
177200180208           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
177300180208                         SizeList : NotUsed : QUSEC );
177400180208         EndFor;
177500180208
177600180219         // - ?Next step - Signal end of input, clear subfile for reload.?
177700180208         QLGRT00 = c_EndPut;
177800180208         clear  QUSEI;
177900180208         QUSBPRV = %size(QUSEC);
178000180208         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
178100180208                       SizeList : NotUsed : QUSEC );
178200180208
178300180219         // - ?Pulizia subfile?
178400180208         $SflDsp_N    = *on;
178500180208         $SflDspCtl_N = *on;
178600180219         write  TB11C01;
178700180208         $SflDspCtl_N = *off;
178800180208         $SflEnd      = *off;
178900180208
179000180219         // - ?Final step - Write the records back to the subfile.?
179100180208         QLGRT00  = c_Get;
179200180208         For  Nrr = 1  To RrnLast;
179300180208           clear  QUSEI;
179400180208           QUSBPRV = %size(QUSEC);
179500180208           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
179600180208                         QLGRL00  : NotUsed : QUSEC );
179700180219           // - ?Ripristino indicatori utilizzati nel sfl rec?
179800180208           $SflNxtChg = ( S01opz <> *zero );
179900180208           S01nrr = Nrr;
180000180219           write  TB11S01;
180100180208         EndFor;
180200180219
180300180219         // - ?RI-Scrittura eventuali ultimi record vuoti?
180400180219         DoW  S01nrr < c_SflSizMax;
180500180219           S01nrr += 1;
180600180219           exsr  sr_RecVuotoS01;
180700180219         EndDo;
180800180208
180900180208         C1CsrRrn  = 1;
181000180208
181100180219         // - ?Visualizzazione del SFL (se ci sono dati)?
181200180208         $SflDsp_N = ( S01nrr <= *zero );
181300180208
181400180219         // - ?Attivazione del SFLEND?
181500180219         $SflEnd   = *on;
181600180208
181700180219         // - ?(Dis)Abilitazione tasti funzionali?
181800180219         //*·// · ?F8 = Ordinamento x ...?
181900180219         //*·$F8Attivo = ( S01nrr > 1 );
182000180208
182100180208       ENDSR;
182200180205
182300180205       //--------------------------------------------------------------
182400180205       //? Scrittura records nel *file TNTBE00F.                        ?
182500180205       //--------------------------------------------------------------
182600180205       BEGSR  sr_WrtTNTBE;
182700180205
182800180205         if  S01nrr <= *zero;
182900180205           leavesr;
183000180205         endif;
183100180205
183200180205         clear  S01nrr;
183300180205
183400180205         For  S01nrr = 1  To  c_SflSizMax;
183500180205
183600180205           chain  S01nrr  TB11S01;
183700180205           if  Not %found( TNTB11D );
183800180205             leave;
183900180205           endif;
184000180205
184100180205           // - ?Memorizzazione dati in tab. "MSL"?
184200180205           clear  dMSL;
184300180205           §MSLdes = S01des;
184400180205           evalR  §MSLpep = S01pep;
184500180205           §MSLent = S01ent;
184600180205           if  S01pke = 'S';
184700180205             §MSLpke = S01pke;
184800180205           endif;
184900180205           §MSLapl = S01apl;
185000180205           §MSLap5 = S01ap5;
185100180205           §MSLde5 = S01de5;
185200180205           §MSLpwf = S01pwf;
185300180205
185400180206           // - ?Impostazione chiave di accesso al file TNTBE00F?
185500180205           wKe2 = S01nrr;
185600180205           clear  keyTNTBE01;
185700180205           k_TBEcod = c_Tab;
185800180205           k_TBEke1 = V1Cfil;
185900180205           k_TBEke2 = %editc( wKe2 : 'X' );
186000180205
186100180206           // - ?Apertura file tabelle del 1° S.I. (sede)?
186200180205           if  w_SisInf <> 1   or   Not %open(TNTBE01L);
186300180205             w_SisInf = 1;
186400180205             exsr  sr_OpenFileTab;
186500180205           endif;
186600180205
186700180206           // - ?Ciclo di elaborazione per ogni sistema informativo?
186800180205           For  w_SisInf = 1  To  %elem(sk_Libr);
186900180205
187000180206             // - ?Apertura degli archivi?
187100180205             if  w_SisInf > 1;
187200180205               exsr  sr_OpenFileTab;
187300180205             endif;
187400180205
187500180205             chain  %kds( keyTNTBE01 )  TNTBE000;
187600180205
187700180206             // - ?Aggiornamento file TNTBE00F?
187800180313             IF  %found( TNTBE01L );
187900180205
188000180313               // - ?Cancellazione della posizione vuota?
188100180313               //   ?Esempio: PRIMA dell'F8, il 1° record nel subfile?
188200180313               //   ?era vuoto; in tabella risultano già inseriti le?
188300180313               //   ?postazioni con progressivo (KE2) dal 2 al 9.?
188400180313               //   ?DOPO il riordinamento del subfile, il 1° record?
188500180313               //   ?vuoto viene portato in 9ª posizione (risultando?
188600180313               //   ?ora impostati solo i primi 8 record nel subfile).?
188700180313               //   ?In fase di aggiornamento, quest'ultimo record?
188800180313               //   ?VUOTO con progressivo (KE2) 9 - prima esistente,?
188900180313               //   ?NON vuoto - va CANCELLATO.?
189000180313               If  S01des = *blank  and  S01de5 = *blank  and
189100180313                   S01pep = *blank  and  S01ent = *zero   and
189200180313                   S01apl = *blank  and  S01ap5 = *blank  and
189300180313                   S1Hdes = *blank  and  S1Hde5 = *blank  and
189400180313                   S1Hpep = *blank  and  S1Hent = *zero   and
189500180313                   S1Hapl = *blank  and  S1Hap5 = *blank;
189600180313
189700180313                 //_______________
189800180313                 DELETE  TNTBE000;
189900180313                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
190000180313
190100180313               // - ?Aggiornamento della posizione gestita?
190200180313               Else;
190300180313
190400180313                 dMSL_vd = TBEuni;
190500180313
190600180313                 if  TBEatb  <> S01ann  or
190700180313                     dMSL_vd <> dMSL;
190800180313
190900180313                   exsr  sr_RiempiCampi;
191000180313                   //_______________
191100180313                   UPDATE  TNTBE000;
191200180313                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
191300180313
191400180313                 endif;
191500180313
191600180313               EndIf;
191700180205
191800180206             // - ?Scrittura file TNTBE00F?
191900180313             ELSE;
192000180205
192100180205               clear  TNTBE000;
192200180205
192300180205               if  S01des <> *blank  and  S01de5 <> *blank  and
192400180205                   S01pep <> *blank  and  S01ent <> *zero   and
192500180205                   S01apl <> *blank  and  S01ap5 <> *blank;
192600180205
192700180205                 TBEcod = k_TBEcod;
192800180205                 TBEke1 = k_TBEke1;
192900180205                 TBEke2 = k_TBEke2;
193000180205                 exsr  sr_RiempiCampi;
193100180205                 //______________
193200180205                 WRITE  TNTBE000;
193300180205                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
193400180205
193500180205               endif;
193600180205
193700180313             ENDIF;
193800180205
193900180205           EndFor;
194000180205
194100180205         EndFor;
194200180205
194300180205         unlock  TNTBE01L;
194400180205
194500180205       ENDSR;
194600161027
194700161027       //--------------------------------------------------------------
194800180206       //? Apertura del file tabelle nel sistema informativo impostato. ?
194900161027       //--------------------------------------------------------------
195000161027       BEGSR  sr_OpenFileTab;
195100161027
195200180206         // - ?Chiusura (eventuale) file tabelle?
195300161027         if  %open(TNTBE01L);
195400161027           close  TNTBE01L;
195500161027         endif;
195600161027
195700180206         // - ?Apertura file tabelle?
195800161104         ds_Libr  = sk_Libraries(w_iSystem);
195900161104         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
196000161027         open  TNTBE01L;
196100161027
196200161027       ENDSR;
196300161027
196400161027       //--------------------------------------------------------------
196500180206       //? Impostazione dati nel record in aggiornamento.               ?
196600161027       //--------------------------------------------------------------
196700161027       //? N.B.                                                        ?
196800180206       //? L'annullamento  ed  il ripristino  vanno lasciati in         ?
196900180206       //?   trasmissione (vedi flag TBEFTR).                           ?
197000180206       //? L'aggiornamento  e  l'inserimento  NO: vanno registrati      ?
197100180206       //?   subito nello stesso file di entrambi i S.I. - in due       ?
197200180206       //?   cicli diversi - ma NON vanno messi in trasmissione.        ?
197300161027       //--------------------------------------------------------------
197400161027       BEGSR  sr_RiempiCampi;
197500161027
197600180206         // - ?Dati della tabella?
197700161027         TBEuni = dMSL;
197800180205         TBEatb = S01ann;
197900161027
198000180206         // - ?Dati della matrice?
198100180205         TBEapl = TBXapl;
198200180205         //*·TBEftt = TBXftt;
198300161027         TBEftt = 'S';
198400161027         //*·TBEflt = sav_TBEflt;
198500161027         clear  TBEflt;
198600161027
198700180206         // - ?Dati della "trasmissione"?
198800180205         If  S01ann = *blank  and  S01ann = S1Hann;
198900161027           if  w_SisInf = 1;
199000161027             TBEftr = 'T';
199100161027           else;
199200161027             TBEftr = 'R';
199300161027           endif;
199400161027         Else;
199500161027           clear  TBEftr;
199600161027         EndIf;
199700161027         TBEdtr = wDate;
199800161027
199900161027       ENDSR;
200000180205
200100180205       //--------------------------------------------------------------
200200180206       //? Apertura del file tabelle nel sistema informativo impostato. ?
200300180205       //--------------------------------------------------------------
200400180205       BEGSR  sr_RoutEnd;
200500180205
200600180206         // - ?Chiusura pgm?
200700180205         return;
200800180205
200900180205       ENDSR;
201000161027
201100161027       //--------------------------------------------------------------
201200180206       //? Definizione schiere a tempo di compilazione                  ?
201300161027       //--------------------------------------------------------------
201400161027
201500180206** -- ?sk_iSystem / sk_Libraries: ?Sistemi AS/400 & Librerie con entrambi i file?
201600161027SETRAS  GAITRAGRU FILTRAGRU
201700161027AS888   GAITRAGRPSFILTRAGRPF
201800180206** -- ?sk_Msg: ?Messaggi di Errore? --------------------------------------------*
201900180205Inserire Filiale                                                                1
202000180205Filiale inesistente                                                             2
202100180205Filiale annullata                                                               3
202200180205Filiale non valida                                                              4
202300180206Postazione Entrata già inserita nella posizione n°                              5
202400180206N° Postazione già usato per la postazione "                                     6
202500180206Codice tipo applicazione non valido                                             7
202600180206Tipo applicazione non valido per Software VDL                                   8
202700180206Entrata picking ammessa solo per applicazione di FV 2 = "K"                     9
202800180206Non è possibile cancellare un record esistente                                 10
202900180206Inserire tutti i dati                                                          11
203000180206Record già annullato                                                           12
203100180206Impossibile ripristinare un record già valido                                  13
203200180206Opzione errata                                                                 14
203300180206Inserire l'opzione o (in alternativa) l'interrogazione                         15
203400180301AVVISO: esistono Posizioni libere prima di questa                              16
