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
010700180206     d sk_Msg          s             78    dim(15)  ctdata  perrcd( 1)
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
012900180213
013000180213       // - ?Comodo per ordinamento?
013100180213     d sk_Ord          s              2    inz
013200180213     d                                     dim(10)
013300180205
013400180205       //--------------------------------------------------------------
013500180205       //? Definizione aree dati.                                       ?
013600180205       //--------------------------------------------------------------
013700180205
013800180205       // - ?Dati utente?
013900180205     d §AzUte        e ds                  extname(AZUTE00F)
014000180205     d                                     dtaara
014100180205     d §DatiUte      e ds                  extname(dDatiUte)
014200180205     d                                     dtaara
014300180205
014400180205       //--------------------------------------------------------------
014500180205       //? Definizione strutture dati.                                  ?
014600180205       //--------------------------------------------------------------
014700060221
014800180205       // - ?Status ds?
014900180205     d Status         sds
015000180205     d   SDSpgm          *proc
015100180205
015200180205       // - ?InfDS?
015300180205     d InfDspF         ds                  qualified
015400180205     d   dsp_aid             369    369a
015500180205     d   sfl_rrn             376    377i 0
015600180205     d   min_nrr             378    379i 0
015700180205     d   num_rcds            380    381i 0
015800180205
015900180205       // - ?Indicatori su DspF?
016000180205     d IndDspF         ds                  inz
016100180205         // - ?Abilitazione tasti funzionali?
016200180206     d   $F6Attivo                     n   overlay( IndDspF : 06 )
016300180206     d   $F8Attivo                     n   overlay( IndDspF : 08 )
016400180206     d   $F9Attivo                     n   overlay( IndDspF : 09 )
016500180206     d   $F12Attivo                    n   overlay( IndDspF : 12 )
016600180205         // - ?Emissione messaggio di errore?
016700180206     d   $ErrMessage                   n   overlay( IndDspF : 28 )
016800180205         // - ?Indicatori di gestione del subfile?
016900180206     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
017000180206     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
017100180206     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
017200180206     d   $SflEnd                       n   overlay( IndDspF : 33 )
017300180206     d   $SflDsp5N                     n   overlay( IndDspF : 34 )
017400180206     d   $SflDspCtl5N                  n   overlay( IndDspF : 35 )
017500180206     d   $SflEnd5                      n   overlay( IndDspF : 36 )
017600180205         // - ?Indicatori per Attibuti di visualizzazione?
017700180206     d   $ProtAnnull                   n   overlay( IndDspF : 40 )
017800180206     d   $OrdAppl02                    n   overlay( IndDspF : 42 )
017900180206     d   $OrdAppl03                    n   overlay( IndDspF : 43 )
018000180206     d   $OrdAppl04                    n   overlay( IndDspF : 44 )
018100180206     d   $OrdAppl05                    n   overlay( IndDspF : 45 )
018200180206     d   $OrdAppl06                    n   overlay( IndDspF : 46 )
018300180205         // - ?Posizionamento cursore & segnalazione errore?
018400180206     d   $PosCurFIL                    n   overlay( IndDspF : 50 )
018500180206     d   $PosCurOPZ                    n   overlay( IndDspF : 51 )
018600180206     d   $PosCurDES                    n   overlay( IndDspF : 52 )
018700180206     d   $PosCurDE5                    n   overlay( IndDspF : 53 )
018800180206     d   $PosCurPEP                    n   overlay( IndDspF : 54 )
018900180206     d   $PosCurENT                    n   overlay( IndDspF : 55 )
019000180206     d   $PosCurAPL                    n   overlay( IndDspF : 56 )
019100180206     d   $PosCurAP5                    n   overlay( IndDspF : 57 )
019200180206     d   $PosCurPWF                    n   overlay( IndDspF : 58 )
019300180206     d   $PosCurPKE                    n   overlay( IndDspF : 59 )
019400180206     d*//$PosCurANN                    n   overlay( IndDspF : 60 )
019500180206     d   $PosCurApl1                   n   overlay( IndDspF : 61 )
019600180206     d   $PosCurApl2                   n   overlay( IndDspF : 62 )
019700180206     d   $PosCurApl3                   n   overlay( IndDspF : 63 )
019800180206     d   $PosCurApl4                   n   overlay( IndDspF : 64 )
019900180206     d   PosCurApl5                    n   overlay( IndDspF : 65 )
020000180206     d   $PosCurApl6                   n   overlay( IndDspF : 66 )
020100180205         // - ?Riemissione videata?
020200180206     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
020300180205
020400180205       // - ?Parametri ricevuti?
020500180205     d KPJBA         e ds
020600180205
020700180205       // - ?Dati record principale della tabella?
020800180219     d TNTBEds       e ds                  inz  extname( TNTBE00F )
020900180219     d xTNTBEds      e ds                  inz  extname( TNTBE00F )
021000180219     d                                          prefix( TBX : 3 )
021100010308
021200180205       // - ?Tabelle usate:?
021300180205       // · ?"MTP" = VdL-Tipo Applicazione?
021400180205     d dMTP          e ds                  inz
021500180205       // · ?"MSL" = VdL-Layout Postazioni?
021600180205     d dMSL          e ds                  inz
021700180219     d dMSL_vd       e ds                  inz  extname( dMSL )
021800180219     d                                          prefix( W )
021900180206       // · ?"MFP" = Positrova Attivo in Filiale?
022000180206     d dMFP          e ds                  inz
022100180205
022200180205       // - ?Descrizione 150 dell'ORGANIGRAMMA?
022300180206     d Og150         e ds                  inz  qualified
022400180205
022500180205       //--------------------------------------------------------------
022600180205       //? Definizione variabili globali.                               ?
022700180205       //--------------------------------------------------------------
022800060221
022900180205       // - ?Flags booleani?
023000180205     d $Fine           s               n   inz
023100180205     d $InzD01         s               n   inz(*on)
023200180205     d $InzS01         s               n   inz(*on)
023300180205     d $InzS05         s               n   inz(*on)
023400180206     d $InzW01         s               n   inz(*on)
023500180206     d $Ord_AplPrg     s               n   inz
023600180205
023700180205       // - ?Variabili per la gestione del video?
023800180205     d $Video          s              2    inz('D1')
023900180219     d S01nrr          s                   like( C1RcdNbr )  inz
024000180219     d SavS01csr       s                   like( C1RcdNbr )  inz
024100180219     d S05nrr          s                   like( C5RcdNbr )  inz
024200180205
024300180205       // - ?Indici di schiera?
024400180205     d xx              s              3  0 inz
024500180219     d yy              s              3  0 inz
024600180205
024700180205       // - ?Nome esteso Libreria/File del file tabella?
024800180205     d wLibFile        s             21a   inz
024900180205
025000180205       // - ?Campi di comodo?
025100180205     d w_iSystem       s              1  0 inz
025200180205     d w_SisInf        s              3  0 inz
025300180205     d wDate           s              8  0 inz
025400180205     d wKe2            s              3  0 inz
025500180219     d wAPL            s                   like( §MFPapl )   inz
025600180208
025700180208       //--------------------------------------------------------------
025800180213       // - ?PARAMETRI PER ORDINAMENTO SUBFILE?
025900180208       //--------------------------------------------------------------
026000180208
026100180208       //--------------------------------------------------------------
026200180213       // - ?Constants?
026300180208       //--------------------------------------------------------------
026400180213       // - ?MaxKey - Maximum number of key fields allowed by this program?
026500180219     d c_MaxKey        c                   const(5)
026600180213       // - ?Sort order:?
026700180213       //   ?1 = Sort in an ascending sequence?
026800180213       //   ?2 = Sort in a descending sequence?
026900180208     d c_Ascendente    c                   const(1)
027000180208     d c_Discendente   c                   const(2)
027100180213       // - ?Key data type:?
027200180208       //  ? 0 = Signed binary?
027300180208       //  ? 2 = Signed zoned decimal?
027400180208       //  ? 3 = Signed packed decimal?
027500180208       //  ? 6 = Character with no national language sort sequence applied,?
027600180208       //  ?     if specified?
027700180208       //  ? 7 = Unsigned packed decimal?
027800180208       //  ?     All numbers will have the sign forced positive ('F'X).?
027900180208       //  ? 8 = Unsigned zoned decimal?
028000180208       //  ?     All numbers will have the sign forced positive ('F'X).?
028100180208       //  ? 9 = Unsigned binary?
028200180208       //  ?14 = Date in form of DD/MM/YY?
028300180208       //  ?15 = Date in form of DD.MM.YYYY?
028400180208     d c_Numero        c                   const(2)
028500180208     d c_Carattere     c                   const(6)
028600180208     d c_NumIntero     c                   const(8)
028700180208       //
028800180208     d c_Put           c                   const(1)
028900180208     d c_EndPut        c                   const(2)
029000180208     d c_Get           c                   const(3)
029100180208
029200180208       //--------------------------------------------------------------
029300180213       // - ?Data Structures?
029400180213       //   ?SflRcd     - The entire subfile record to pass to the sort?
029500180213       //   ?QLGSCB     - The sort request block for the QLGSORT API?
029600180213       //   ?QLGSCB00   - The sort request block for the QLGSRTIO API?
029700180213       //   ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
029800180213       //   ?QUSEC      - Error structure for the QLGSORT API?
029900180208       //--------------------------------------------------------------
030000180208     d SflRcd          ds                  inz
030100180219     d   S1Hseq                            inz
030200180219     d   S1Hdes                            inz
030300180219     d   S1Hde5                            inz
030400180219     d   S1Hpep                            inz
030500180219     d   S1Hent                            inz
030600180219     d   S1Hapl                            inz
030700180219     d   S1Hap5                            inz
030800180219     d   S1Hpwf                            inz
030900180219     d   S1Hpke                            inz
031000180219     d   S1Hann                            inz
031100180208     d   S01opz                            inz
031200180208     d   S01des                            inz
031300180208     d   S01de5                            inz
031400180208     d   S01pep                            inz
031500180208     d   S01ent                            inz
031600180208     d   S01apl                            inz
031700180208     d   S01ap5                            inz
031800180208     d   S01pwf                            inz
031900180208     d   S01pke                            inz
032000180208     d   S01ann                            inz
032100180208     d   Selected                     1a   inz
032200180208      /copy QSYSINC/QRPGLESRC,QLGSORT
032300180208     d QLGKL                         16    dim(c_MaxKey)
032400180208     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
032500180208     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
032600180208     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
032700180208     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
032800180208      /copy QSYSINC/QRPGLESRC,QLGSRTIO
032900180208      /copy QSYSINC/QRPGLESRC,QUSEC
033000180208
033100180208       //--------------------------------------------------------------
033200180213       // - ?Standalone fields?
033300180213       //   ?Nrr        - Relative record number for subfile?
033400180213       //   ?SizeList   - Size of list?
033500180213       //   ?ReturnSize - Size of list returned by sort API?
033600180208       //--------------------------------------------------------------
033700180208     d NotUsed         s             16a   inz
033800180208     d ReturnSize      s             10i 0 inz
033900180208     d SizeList        s             10i 0 inz
034000180208     d RrnLast         s              5i 0 inz
034100180208     d Nrr             s              5i 0 inz
034200161027
034300161027       //--------------------------------------------------------------
034400180205       //? Definizione prototipi/procedure.                             ?
034500161027       //--------------------------------------------------------------
034600161027
034700180205       // - ?Reperimento NETA sistema AS/400 corrente?
034800180205     d currSysNeta     s              8a
034900161027      /copy gaitrasrc/srcProtoPR,UBRTVNETA
035000180205
035100180205       // - ?Reperimento dati utente?
035200180205     d TIBS34ds      e ds                  inz
035300180205      /copy gaitrasrc/srcProtoPR,TIBS34R
035400180205
035500180205       // - ?Parametri per Ricerca/Controllo tabelle (TNTBE)?
035600180205     d TIBS02ds      e ds                  inz
035700180205     d   T02mod      e                     inz('R')
035800180205     d   T02cod      e                     inz('MSL')
035900180205       // - ?Ricerca/Controllo tabelle (TNTBE)?
036000180205      /copy gaitrasrc/srcProtoPR,TIBS02R
036100180208
036200180208       // -?Ordinamento subfile?
036300180208      /copy gaitrasrc/srcProtoPR,QLGSRTIO
036400180205
036500180205       //--------------------------------------------------------------
036600180205       //? Definizione key-list.                                        ?
036700180205       //--------------------------------------------------------------
036800180205
036900180205       // - ?File TNTBE01L?
037000180205     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
037100180205     d                                     prefix( k_ )   inz
037200180205
037300180205       //--------------------------------------------------------------
037400180205       //? Riepilogo indicatori utilizzati.                             ?
037500180205       //--------------------------------------------------------------
037600180205
037700180205
037800180205       //--------------------------------------------------------------
037900180205       //? M A I N - L I N E                                            ?
038000180205       //--------------------------------------------------------------
038100180205
038200180205     c     *Entry        plist
038300180205     c                   parm                    KPJBA
038400180205
038500180205      /free
038600180205
038700180205       // - ?Operazioni iniziali?
038800180205       exsr sr_RoutInz;
038900180205
039000180205       // - ?Ciclo di gestione del file video?
039100180205       DoW  $Fine = *off;
039200180205
039300180205         select;
039400180205
039500180205           // - ?Richiesta chiave di accesso?
039600180205           when $Video = 'D1';
039700180205             exsr  sr_GesD01;
039800180205
039900180205           // - ?Gestione videata S1 (subfile)?
040000180205           when $Video = 'S1';
040100180205             exsr  sr_GesS01;
040200180205
040300180205           // - ?Visualizzazione videata S5 (subfile vecchio stile)?
040400180205           when $Video = 'S5';
040500180205             exsr  sr_GesS05;
040600180206
040700180206           // - ?Visualizzazione videata W1 (window x ordinamento sfl)?
040800180206           when $Video = 'W1';
040900180206             exsr  sr_GesW01;
041000180205
041100180205           // - ?? ? ??
041200180205           other;
041300180205             $Fine = *on;
041400180205
041500180205         endsl;
041600180205
041700180205       EndDo;
041800180205
041900180205       // - ?Operazioni finali?
042000180205       exsr sr_RoutEnd;
042100180205
042200180205       //--------------------------------------------------------------
042300180205       //? Operazioni iniziali.                                         ?
042400180205       //--------------------------------------------------------------
042500180205       BEGSR  sr_RoutInz;
042600180205
042700180205         // - ?Impostazione chiusura?
042800180205         *inLR = *on;
042900180205
043000180205         // - ?Verifica del sistema AS/400 corrente?
043100180205         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
043200180205           $Fine = *on;
043300180205           leavesr;
043400180205         endif;
043500180205
043600180205         // - ?Impostazione elenco librerie in cui gestire le tabelle?
043700180205         //   ?(a seconda del sistema in cui si stà lavorando)?
043800180205         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
043900180205         if  w_iSystem = *zero;
044000180205           $Fine = *on;
044100180205           leavesr;
044200180205         endif;
044300180205
044400180205         // - ?1ª apertura file TABEL00F del 1° S.I. (sede)?
044500180205         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
044600180205           w_SisInf = 1;
044700180205           exsr  sr_OpenFileTab;
044800180205         endif;
044900180205
045000180205         // - ?Reperimento data odierna?
045100180205         wDate = %int( %subst( %char( %dec( %timestamp() ) )
045200180205                               : 1 : 8 ) );
045300180205
045400180205         // - ?Reperimento dati job?
045500180205         exsr  sr_DatiJob;
045600180205
045700180205         // - ?Impostazione nome programma a video?
045800180205         V1Tpgm = SDSpgm;
045900180205
046000180205         // - ?Aggancio dati generali della tabella in esame?
046100180205         clear  k_TBEcod;
046200180205         k_TBEke1 = *zero;
046300180205         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
046400180205                = c_Tab;
046500180205         clear  k_TBEke2;
046600180205         clear  k_TBElin;
046700180205         k_TBEsif = KNSIF;
046800180206         chain(N)  %kds( keyTNTBE01 )  TNTBE000;
046900180205         if  not  %found(TNTBE01L);
047000180205           clear  k_TBEsif;
047100180206           chain(n)  %kds( keyTNTBE01 )  TNTBE000;
047200180205         endif;
047300180205         if  %found(TNTBE01L);
047400180205           xTNTBEds = TNTBEds;
047500180205         else;
047600180205           clear  xTNTBEds;
047700180205         endif;
047800180205
047900180206         // - ?Caricamento Tipi Applicazione gestiti?
048000180206         clear  sk_MTP;
048100180206         clear  keyTNTBE01;
048200180206         k_TBEcod = 'MTP';
048300180206         setll     %kds( keyTNTBE01 )  TNTBE000;
048400180206         reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
048500180206         DoW  NOT %eof(TNTBE01L);
048600180206           dMTP = TBEuni;
048700180206           if  §MTPswv = 'S';
048800180206             xx += 1;
048900180206             sk_MTP(xx)    = TBEke1;
049000180206             sk_MTPdes(xx) = §MTPdes;
049100180206           endif;
049200180206           reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
049300180206         EndDo;
049400180206
049500180205         // - ?Impostazione iniziale dei flags booleani?
049600180205         reset  $Fine;
049700180205         reset  $InzD01;
049800180205         reset  $InzS01;
049900180205         reset  $InzS05;
050000180205         reset  $Video;
050100180205
050200180205       ENDSR;
050300180205
050400180205       //--------------------------------------------------------------
050500180205       //? Reperimento Dati del job (Utente/Operativi).                 ?
050600180205       //--------------------------------------------------------------
050700180205       BEGSR  sr_DatiJob;
050800180205
050900180205         in(e) §AzUte;
051000180205         if NOT %error;
051100180205           in(e) §DatiUte;
051200180205         endif;
051300180205         if %error or RSut = *blank;
051400180205           tibs34r ( tibs34ds );
051500180205           in §AzUte;
051600180205           in §DatiUte;
051700180205         endif;
051800180205
051900180205       ENDSR;
052000180205
052100180205       //--------------------------------------------------------------
052200180205       //? Gestione videata D01.                                        ?
052300180205       //--------------------------------------------------------------
052400180205       BEGSR  sr_GesD01;
052500180205
052600180205         // - ?Inizializzazione videata?
052700180205         if  $InzD01 = *on;
052800180205           exsr  sr_InzD01;
052900180205           $InzD01 = *off;
053000180205         endif;
053100180205
053200180205         // - ?(Dis)Abilitazione tasti funzionali?
053300180205         $F6Attivo  = *off;
053400180205         $F8Attivo  = *off;
053500180205         $F9Attivo  = *off;
053600180205         $F12Attivo = *off;
053700180205
053800180205         // - ?Emissione Testata & Piede?
053900180205         write  TB11T01;
054000180205         write  TB11P01;
054100180205
054200180205         // - ?Emissione videata?
054300180205         exfmt  TB11D01;
054400180205
054500180205         clear  VIDmsg;
054600180205         reset  $ErrMessage;
054700180205         reset  $ErrGenerico;
054800180205
054900180205         SELECT;
055000180205
055100180205           // - ?F3=Fine?
055200180205           WHEN  InfDspF.dsp_aid = c_F03;
055300180205             $Fine = *on;
055400180205
055500180205           // - ?Invio?
055600180205           OTHER;
055700180205             exsr  sr_CtrD01;
055800180205             if  $ErrGenerico = *on;
055900180205               leavesr;
056000180205             endif;
056100180205             $Video  = 'S1';
056200180205             $InzS01 = *on;
056300180205
056400180205         ENDSL;
056500180205
056600180205       ENDSR;
056700180205
056800180205       //--------------------------------------------------------------
056900180205       //? Inizializzazione videata D01.                                ?
057000180205       //--------------------------------------------------------------
057100180205       BEGSR  sr_InzD01;
057200180205
057300180205         clear  TB11D01;
057400180205
057500180205         V1Cfil = '?';
057600180205
057700180205       ENDSR;
057800180205
057900180205       //--------------------------------------------------------------
058000180205       //? Controllo dati immessi nella videata D01.                    ?
058100180205       //--------------------------------------------------------------
058200180205       BEGSR  sr_CtrD01;
058300180205
058400180205         // - ?Spegnimento indicatori di posizionamento cursore?
058500180205         %subst(IndDspF : 28) = *off;
058600161027
058700180205         Select;
058800180205
058900180205           // - ?Ricerca Filiale (in tab. "MSL")?
059000180205           When  %scan('?' : V1Cfil) > *zero;
059100180205             //*·clear  V1Cfil;
059200180205             clear  V1Dfil;
059300180205             reset  TIBS02ds;
059400180205             //T02mod = 'R';                // ?(già così)?
059500180205             //T02cod = c_Tab;              // ?(già così)?
059600180205             T02sif = kNSif;
059700180205             TNTBE_RicercaControllo ( kpjba : TIBS02ds );
059800180205             if  T02err = *blank;
059900180205               V1Cfil = T02ke1;
060000180205               V1Dfil = %subst( T02uni : 1 : %len(V1Dfil) );
060100180205             endif;
060200180205             $ErrGenerico = *on;
060300180205             $PosCurFIL   = *on;
060400180205             leavesr;
060500180205
060600180205           // - ?Filiale NON inserita?
060700180205           When  V1Cfil = *blank  or  V1Cfil = *zero;
060800180205             $ErrGenerico = *on;
060900180205             $ErrMessage  = *on;
061000180205             $PosCurFIL   = *on;
061100180205             VIDmsg = sk_Msg(01);
061200180205             leavesr;
061300180205
061400180205         EndSl;
061500010601
061600180205         // - ?Controllo Filiale?
061700180205         clear  V1Dfil;
061800180205         chain  ( %int( V1CFIL ) )  AZORG;
061900180205         if  NOT %found(AZORG01L);
062000180205           $ErrGenerico = *on;
062100180205           $ErrMessage  = *on;
062200180205           $PosCurFIL   = *on;
062300180205           VIDmsg = sk_Msg(02);
062400180205           leavesr;
062500180205         endif;
062600180205
062700180205         V1Dfil = ORGdes;
062800180205
062900180205         Select;
063000180205
063100180205           When  ORGfva <> *blank;
063200180205             $ErrGenerico = *on;
063300180205             $ErrMessage  = *on;
063400180205             $PosCurFIL   = *on;
063500180205             VIDmsg = sk_Msg(03);
063600180205             leavesr;
063700180205
063800180205           When  ORGfag <> 'A'  and  ORGfag <> 'F';
063900180205             $ErrGenerico = *on;
064000180205             $ErrMessage  = *on;
064100180205             $PosCurFIL   = *on;
064200180205             VIDmsg = sk_Msg(04);
064300180205             leavesr;
064400180205
064500180205         EndSl;
064600180206
064700180206         // - ?Reperimento tab. "MFP" per la filiale?
064800180206         clear  dMFP;
064900180206         clear  keyTNTBE01;
065000180206         k_TBEcod = 'MFP';
065100180206         k_TBEke1 = V1Cfil;
065200180206         chain(N)  %kds( keyTNTBE01 )  TNTBE000;
065300180206         if  %found(TNTBE01L);
065400180206           dMFP = TBEuni;
065500180206         endif;
065600010604
065700180205       ENDSR;
065800180205
065900180205       //--------------------------------------------------------------
066000180205       //? Gestione SubFile S01.                                        ?
066100180205       //--------------------------------------------------------------
066200180205       BEGSR  sr_GesS01;
066300180205
066400180205         // - ?Inizializzazione subfile?
066500180205         if  $InzS01 = *on;
066600180205           exsr  sr_InzS01;
066700180205           $InzS01 = *off;
066800180219           exsr  sr_CtrS01;
066900180219           clear  VIDmsg;
067000180219           clear  $ErrMessage;
067100180219           clear  $ErrGenerico;
067200180205         endif;
067300180205
067400180205         // - ?(Dis)Abilitazione tasti funzionali?
067500180205         $F6Attivo  = *on;
067600180205         $F8Attivo  = *on;
067700180205         $F9Attivo  = *on;
067800180205         $F12Attivo = *on;
067900180205
068000180205         // - ?Posizionamento cursore?
068100180205         //   ?C1CsrRrn contiene il numero di riga del subfile su cui?
068200180205         //   ?era posizionato il cursore; impostandolo in C1RcdNbr?
068300180205         //   ?si visualizza la pagina che era visualizzata quando?
068400180205         //   ?l'utente ha premuto l'ultimo tasto.?
068500180205         if  C1CsrRrn > *zero;
068600180205           C1RcdNbr = C1CsrRrn;
068700180205         else;
068800180205           C1RcdNbr = 1;
068900180205         endif;
069000180205
069100180205         // - ?Emissione Testata & Piede con tasti funzionali abilitati?
069200180205         write  TB11T01;
069300180205         write  TB11P01;
069400180205
069500180205         // - ?Emissione subfile?
069600180205         exfmt  TB11C01;
069700180205
069800180205         clear  VIDmsg;
069900180205         clear  $ErrMessage;
070000180205         clear  $ErrGenerico;
070100180205
070200180205         Select;
070300180205
070400180205           // - ?F3=Fine?
070500180205           When  InfDspF.dsp_aid = c_F03;
070600180205             unlock  TNTBE01L;
070700180205             $Fine = *on;
070800180205             leavesr;
070900180206
071000180206           // - ?F8=Ordinamento per Applicazione / N° Postazione?
071100180206           When  InfDspF.dsp_aid = c_F08;
071200180206             $InzW01 = *on;
071300180206             $Video  = 'W1';
071400180205
071500180205           // - ?F9=Altra vista?
071600180205           When  InfDspF.dsp_aid = c_F09;
071700180205             $InzS05 = *on;
071800180205             $Video  = 'S5';
071900180205
072000180205           // - ?F12=Ritorno?
072100180205           WHEN  InfDspF.dsp_aid = c_F12;
072200180205             unlock  TNTBE01L;
072300180205             $Video = 'D1';
072400180205             leavesr;
072500180205
072600180205           // - ?Invio / F6=Conferma?
072700180205           OTHER;
072800180205             // - ?Controllo dati?
072900180205             exsr  sr_CtrS01;
073000180205             select;
073100180205               // - ?Rilevato errore?
073200180205               when  $ErrGenerico = *on;
073300180205                 leavesr;
073400180205               // - ?F6=Conferma?
073500180205               when  InfDspF.dsp_aid = c_F06;
073600180205                 exsr  sr_WrtTNTBE;
073700180205                 if  NOT $ErrGenerico;
073800180205                   $Video = 'D1';
073900180205                   reset  $InzD01;
074000180205                   leavesr;
074100180205                 endif;
074200180205             endsl;
074300180205
074400180205         ENDSL;
074500180205
074600180205       ENDSR;
074700180205
074800180205       //--------------------------------------------------------------
074900180205       //? Inizializzazione SubFile S01.                                ?
075000180205       //--------------------------------------------------------------
075100180205       BEGSR  sr_InzS01;
075200180205
075300180205         // - ?1ª apertura file tabelle del 1° S.I. (sede)?
075400180205         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
075500180205           w_SisInf = 1;
075600180205           exsr  sr_OpenFileTab;
075700180205         endif;
075800180205
075900180205         // - ?Pulizia subfile?
076000180205         clear  TB11S01;
076100180205         clear  TB11C01;
076200180205         $SflDsp_N    = *on;
076300180205         $SflDspCtl_N = *on;
076400180205         write  TB11C01;
076500180205         $SflDspCtl_N = *off;
076600180205         $SflEnd      = *off;
076700180205
076800180205         $SflNxtChg   = *off;
076900180205
077000180205         clear  C1RcdNbr;
077100180205         clear  C1CsrRrn;
077200180205         clear  S01nrr;
077300180205         clear  VIDmsg;
077400180205         $ErrMessage  = *off;
077500180205         $ErrGenerico = *off;
077600180205
077700180205         // - ?Spegnimento degli indicatori di errore?
077800180205         %subst( IndDspF : 50 ) = *off;
077900180205
078000180205         // - ?Impostazione dati nel subfile-control?
078100180205         C1Cfil = V1Cfil;
078200180205         C1Dfil = V1Dfil;
078300180205
078400180205         // - ?Ciclo di caricamento (completo) dei dati nel subfile?
078500180205         clear  S01nrr;
078600180205         $SflNxtChg = *off;
078700180205         clear  keyTNTBE01;
078800180205         k_TBEcod = c_Tab;
078900180205         k_TBEke1 = V1Cfil;
079000180205         setll     %kds( keyTNTBE01 )  TNTBE000;
079100180205         reade(N)  %kds( keyTNTBE01 : 2 )  TNTBE000;
079200180205         For  S01nrr = 1  To  c_SflSizMax;
079300180205           if  Not %eof(TNTBE01L);
079400180205             exsr  sr_CaricaS01;
079500180205             reade(N)  %kds( keyTNTBE01 : 2 )  TNTBE000;
079600180205           else;
079700180206             exsr  sr_RecVuotoS01;
079800180205           endif;
079900180205         EndFor;
080000180205
080100180205         // - ?Visualizzazione del SFL (se ci sono dati)?
080200180205         $SflDsp_N = (S01nrr <= *zero);
080300180205
080400180205         // - ?Attivazione del SFLEND?
080500180205         $SflEnd = *on;
080600180205
080700180205         // - ?Posizionamento cursore al 1° rcd della 1ª pagina?
080800180205         if  S01nrr > *zero;
080900180205           C1RcdNbr = 1;
081000180205         else;
081100180205           clear  C1RcdNbr;
081200180205         endif;
081300180205
081400180205         C1CsrRrn = C1RcdNbr;
081500180205
081600180205       ENDSR;
081700180205
081800180205       //--------------------------------------------------------------
081900180205       //? Caricamento singolo rec. di dati nel SubFile (da tab."MSL"). ?
082000180205       //--------------------------------------------------------------
082100180205       BEGSR  sr_CaricaS01;
082200180205
082300180205         dMSL   = TBEuni;
082400180205
082500180205         // - ?Eventuale caricamento dei record vuoti PRIMA di quello?
082600180205         //   ?con i dati in Input?
082700180205         wKe2 = %int( %trim( TBEke2 ) );
082800180205         DoW  S01nrr < wKe2;
082900180219           exsr  sr_RecVuotoS01;
083000180205           S01nrr += 1;
083100180205         EndDo;
083200180205
083300180205         // - ?Impostazione campi?
083400180205         clear  TB11S01;
083500180205
083600180205         // - ?Campi Hidden?
083700180205         S1Hdes = §MSLdes;
083800180205         S1Hde5 = §MSLde5;
083900180205         S1Hpep = §MSLpep;
084000180205         S1Hent = §MSLent;
084100180205         S1Hapl = §MSLapl;
084200180205         S1Hap5 = §MSLap5;
084300180205         S1Hpwf = §MSLpke;
084400180205         S1Hpke = §MSLpwf;
084500180205         S1Hann = TBEatb;
084600180205
084700180205         // - ?Campi I/O?
084800180205         S01des = §MSLdes;
084900180205         S01de5 = §MSLde5;
085000180205         evalR  S01pep = §MSLpep;
085100180205         S01ent = §MSLent;
085200180205         S01apl = §MSLapl;
085300180205         S01ap5 = §MSLap5;
085400180205         S01pke = §MSLpke;
085500180205         S01pwf = §MSLpwf;
085600180205         S01ann = TBEatb;
085700180205
085800180205         // - ?Attributi di Visualizzazione?
085900180205         $ProtAnnull = (S01ann <> *blank);
086000180205
086100180205         S01nrr = wKe2;
086200180205
086300180205         write  TB11S01;
086400180205
086500180205       ENDSR;
086600180205
086700180205       //--------------------------------------------------------------
086800180205       //? Caricamento singolo rec. vuoto nel SubFile.                  ?
086900180205       //--------------------------------------------------------------
087000180206       BEGSR  sr_RecVuotoS01;
087100180205
087200180205         clear  TB11S01;
087300180205
087400180205         write  TB11S01;
087500180205
087600180205       ENDSR;
087700180206
087800180206       //--------------------------------------------------------------
087900180206       //? Controllo dati nel SubFile S01.                              ?
088000180206       //--------------------------------------------------------------
088100180206       BEGSR  sr_CtrS01;
088200180206
088300180206           // - ?Memorizzazione nrr del sfl con la 1ª opzione per?
088400180206           //   ?riposizionarvici il cursore dopo il tasto "Invio"?
088500180206           if (SavS01csr = *zeros   or   SavS01csr <> C1CsrRrn);
088600180206             SavS01csr   = C1CsrRrn;
088700180206           endif;
088800180206
088900180206         // - ?Ciclo di lettura del SubFile?
089000180206         clear  sk_APL;
089100180206         clear  sk_PEP;
089200180206         clear  sk_ENT;
089300180206         clear  sk_ENTxMTP;
089400180206         clear  S01nrr;
089500180208         clear  RrnLast;
089600180206
089700180206         For  S01nrr = 1  To  c_SflSizMax;
089800180206
089900180206           chain  S01nrr  TB11S01;
090000180206           if  NOT %found( TNTB11D );
090100180206             leave;
090200180206           endif;
090300180206
090400180206           // - ?Controllo dati nel singolo record del subfile?
090500180206           clear  VIDmsg;
090600180206           exsr  sr_CtrRecS01;
090700180206
090800180206           // - ?Posizionamento del cursore?
090900180206           //*·$PosCurOPZ = $ErrGenerico;
091000180206           if  $ErrGenerico  or  $ErrMessage;
091100180206             C1CsrRrn = S01nrr;
091200180206           endif;
091300180206
091400180206           // - ?Attributi di Visualizzazione?
091500180206           $ProtAnnull = (S01ann <> *blank);
091600180206
091700180206           // - ?Aggiornamento SubFile?
091800180206           update  TB11S01;
091900180206
092000180206           if  $ErrGenerico  or  $ErrMessage;
092100180206             leave;
092200180206           endif;
092300180206
092400180206         EndFor;
092500180206
092600180206         // - ?Riposizionam. cursore sul record in cui era posizionato?
092700180206         //   ?(se non sono stati rilevati errori)?
092800180206         if  NOT $ErrMessage   and   NOT $ErrGenerico   and
092900180206             SavS01csr > *zero;
093000180206           C1CsrRrn = SavS01csr;
093100180206         endif;
093200180206
093300180206       ENDSR;
093400180206
093500180206       //--------------------------------------------------------------
093600180206       //? Controllo dati nel singolo record del SubFile S01.           ?
093700180206       //--------------------------------------------------------------
093800180206       BEGSR  sr_CtrRecS01;
093900180206
094000180206         // - ?Spegnimento degli indicatori di errore?
094100180206         %subst(IndDspF : 50) = *off;
094200180206
094300180206         // · ?Ricerca sul 1° campo Tipo Applicazione (FV 2)?
094400180206         If  %scan('?' : S01apl) > *zero;
094500180206
094600180206           if  S01opz <> *blank;
094700180206             $ErrGenerico = *on;
094800180206             $ErrMessage  = *on;
094900180206             $PosCurOPZ   = *on;
095000180206             VIDmsg = sk_Msg(15);
095100180206             leavesr;
095200180206           endif;
095300180206
095400180206           clear  TIBS02ds;
095500180206           T02mod = 'R';
095600180206           T02sif = kNSif;
095700180206           T02cod = 'MTP';
095800180206           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
095900180206           $ErrGenerico = *on;
096000180206           $PosCurAPL   = *on;
096100180206           if  T02err = *blank;
096200180206             S01apl = T02ke1;
096300180206           else;
096400180206             $ErrMessage  = *on;
096500180206             VIDmsg = T02msg;
096600180206             leavesr;
096700180206           endif;
096800180206
096900180206         EndIf;
097000180206
097100180206         // · ?Ricerca sul 2° campo Tipo Applicazione (FV 5)?
097200180206         If  %scan('?' : S01ap5) > *zero;
097300180206
097400180206           if  S01opz <> *blank;
097500180206             $ErrGenerico = *on;
097600180206             $ErrMessage  = *on;
097700180206             $PosCurOPZ   = *on;
097800180206             VIDmsg = sk_Msg(15);
097900180206             leavesr;
098000180206           endif;
098100180206
098200180206           clear  TIBS02ds;
098300180206           T02mod = 'R';
098400180206           T02sif = kNSif;
098500180206           T02cod = 'MTP';
098600180206           TNTBE_RicercaControllo ( kpjba : TIBS02ds );
098700180206           $ErrGenerico = *on;
098800180206           $PosCurAP5   = *on;
098900180206           if  T02err = *blank;
099000180206             S01ap5 = T02ke1;
099100180206           else;
099200180206             $ErrMessage  = *on;
099300180206             VIDmsg = T02msg;
099400180206             leavesr;
099500180206           endif;
099600180206
099700180206         EndIf;
099800180206
099900180206         // - ?Controllo dati nel singolo record?
100000180206         SELECT;
100100180206
100200180206           // - ?Opz. 4 = ANNULLAMENTO?
100300180206           WHEN  S01opz = '4';
100400180206             if  S01ann = 'A';
100500180206               $ErrGenerico = *on;
100600180206               $ErrMessage  = *on;
100700180206               $PosCurOPZ   = *on;
100800180206               VIDmsg = sk_Msg(12);
100900180206               leavesr;
101000180206             endif;
101100180206             clear  S01opz;
101200180206             S01ann = 'A';
101300180206             $ProtAnnull = *on;
101400180208             RrnLast = S01nrr;
101500180206
101600180206           // - ?Opz. 5 = RIPRISTINO?
101700180206           WHEN  S01opz = '5';
101800180206             if  S01ann = *blank;
101900180206               $ErrGenerico = *on;
102000180206               $ErrMessage  = *on;
102100180206               $PosCurOPZ   = *on;
102200180206               VIDmsg = sk_Msg(13);
102300180206               leavesr;
102400180206             endif;
102500180206             clear  S01opz;
102600180206             clear  S01ann;
102700180206             $ProtAnnull = *off;
102800180208             RrnLast = S01nrr;
102900180206
103000180206           // - ?Opz. Errata?
103100180206           WHEN  S01opz <> *blank  and  S01opz <> '4'
103200180206                                   and  S01opz <> '5';
103300180206             $ErrGenerico = *on;
103400180206             $ErrMessage  = *on;
103500180206             $PosCurOPZ   = *on;
103600180206             VIDmsg = sk_Msg(14);
103700180206             leavesr;
103800180206
103900180206           // - ?Record con tutti i dati?
104000180206           WHEN  S01des <> *blank  and  S01de5 <> *blank  and
104100180206                 S01ent <> *zero   and  S01pep <> *blank  and
104200180206                 S01apl <> *blank  and  S01ap5 <> *blank;
104300180206
104400180206             // - ?S01pep (se valorizzato) DEVE avere un valore univoco?
104500180206             If  ( S01opz <> '4'  and  S01ann = *blank )  or
104600180206                 ( S01opz =  '5'  and  S01ann = 'A' );
104700180206               xx = %lookup( S01pep : sk_PEP );
104800180206               if  xx > *zero;
104900180206                 $ErrGenerico = *on;
105000180206                 $ErrMessage  = *on;
105100180206                 $PosCurPEP   = *on;
105200180206                 VIDmsg =  %trimR( sk_Msg(05) ) + ' ' +
105300180206                           %trim( %editc( xx : '3' ) );
105400180206                 leavesr;
105500180206               endif;
105600180206               xx = %lookup( *blank : sk_PEP );
105700180206               if  xx > *zero;
105800180206                 sk_PEP(xx) = S01pep;
105900180206               endif;
106000180206             EndIf;
106100180206
106200180206             // - ?S01apl (se inserito) DEVE avere un valore valido,?
106300180206             //   ?esistente in tab. "MTP" (Tipo Applicazione)?
106400180206             If  S01apl <> *blank;
106500180206
106600180206               clear  TIBS02ds;
106700180206               T02mod = 'C';
106800180206               T02sif = kNSif;
106900180206               T02cod = 'MTP';
107000180206               T02ke1 = S01apl;
107100180206               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
107200180206               if  T02err <> *blank;
107300180206                 $ErrGenerico = *on;
107400180206                 $ErrMessage  = *on;
107500180206                 $PosCurAPL   = *on;
107600180206                 VIDmsg =  sk_Msg(07);
107700180206                 leavesr;
107800180206               endif;
107900180206               // - ?Controllo della validità dell'applicazione per il VDL?
108000180206               dMTP = T02uni;
108100180206               if  §MTPswv = 'N';
108200180206                 $ErrGenerico = *on;
108300180206                 $ErrMessage  = *on;
108400180206                 $PosCurAPL   = *on;
108500180206                 VIDmsg =  sk_Msg(08);
108600180206                 leavesr;
108700180206               endif;
108800180206
108900180206               // - ?Intabellamento dei Numeri Postazione usati per?
109000180206               //   ?Tipo Applicazione?
109100180206               xx = %lookup( S01apl : sk_MTP );
109200180206               // - ?I Tipi Applicazione sono già stati intabellati:?
109300180206               //   ?questo lo trova SICURO!?
109400180206               If  xx > *zero;
109500180206                 ds_ENT = sk_ENTxMTP(xx);
109600180206                 if  sk_ENT( S01ent ) <> *blank;
109700180206                   $ErrGenerico = *on;
109800180206                   $ErrMessage  = *on;
109900180206                   $PosCurENT   = *on;
110000180206                   VIDmsg =  %trimR( sk_Msg(06) ) +
110100180206                             %trim( sk_ENT( S01ent ) ) + '"';
110200180206                   leavesr;
110300180206                 endif;
110400180206                 sk_ENT( S01ent ) = S01pep;
110500180206                 sk_ENTxMTP(xx)   = ds_ENT;
110600180206               EndIf;
110700180206
110800180206               // - ?Memorizzazione Applicazioni utilizzate?
110900180206               If  %lookup( S01apl : sk_APL ) = *zero;
111000180206                 xx = %lookup( *blank : sk_APL );
111100180206                 if  xx > *zero;
111200180206                   sk_APL(xx) = S01apl;
111300180206                 endif;
111400180206               EndIf;
111500180206
111600180206             EndIf;
111700180206
111800180206             // - ?S01ap5 (se inserito) DEVE avere un valore valido,?
111900180206             //   ?esistente in tab. "MTP" (Tipo Applicazione)?
112000180206             If  S01ap5 <> *blank;
112100180206
112200180206               clear  TIBS02ds;
112300180206               T02mod = 'C';
112400180206               T02sif = kNSif;
112500180206               T02cod = 'MTP';
112600180206               T02ke1 = S01ap5;
112700180206               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
112800180206               if  T02err <> *blank;
112900180206                 $ErrGenerico = *on;
113000180206                 $ErrMessage  = *on;
113100180206                 $PosCurAP5   = *on;
113200180206                 VIDmsg =  sk_Msg(07);
113300180206                 leavesr;
113400180206               endif;
113500180206               // - ?Controllo della validità dell'applicazione per il VDL?
113600180206               dMTP = T02uni;
113700180206               if  §MTPswv = 'N';
113800180206                 $ErrGenerico = *on;
113900180206                 $ErrMessage  = *on;
114000180206                 $PosCurAP5   = *on;
114100180206                 VIDmsg =  sk_Msg(08);
114200180206                 leavesr;
114300180206               endif;
114400180206
114500180206               // - ?Intabellamento dei Numeri Postazione usati per?
114600180206               //   ?Tipo Applicazione?
114700180206               //   ?(SE quello in FV5 è diverso da quello in FV2)?
114800180206               if  S01ap5 <> S01apl;
114900180206
115000180206                 xx = %lookup( S01ap5 : sk_MTP );
115100180206                 // - ?I Tipi Applicazione sono già stati intabellati:?
115200180206                 //   ?questo lo trova SICURO!?
115300180206                 If  xx > *zero;
115400180206                   ds_ENT = sk_ENTxMTP(xx);
115500180206                   if  sk_ENT( S01ent ) <> *blank;
115600180206                     $ErrGenerico = *on;
115700180206                     $ErrMessage  = *on;
115800180206                     $PosCurENT   = *on;
115900180206                     VIDmsg =  %trimR( sk_Msg(06) ) +
116000180206                               %trim( sk_ENT( S01ent ) ) + '"';
116100180206                     leavesr;
116200180206                   endif;
116300180206                   sk_ENT( S01ent ) = S01pep;
116400180206                   sk_ENTxMTP(xx)   = ds_ENT;
116500180206                 EndIf;
116600180206
116700180206               endif;
116800180206
116900180206               // - ?Memorizzazione Applicazioni utilizzate?
117000180206               If  %lookup( S01ap5 : sk_APL ) = *zero;
117100180206                 xx = %lookup( *blank : sk_APL );
117200180206                 if  xx > *zero;
117300180206                   sk_APL(xx) = S01ap5;
117400180206                 endif;
117500180206               EndIf;
117600180206
117700180206             EndIf;
117800180206
117900180206             // - ?Controllo che il flag "Postazione entrata picking" sia?
118000180206             //   ?impostato "S" SOLO per applicazione di FV 2 = "K"?
118100180206             if  S01pke = 'S'  and  S01apl <> 'K';
118200180206               $ErrGenerico = *on;
118300180206               $ErrMessage  = *on;
118400180206               $PosCurPKE   = *on;
118500180206               VIDmsg =  sk_Msg(09);
118600180206               leavesr;
118700180206             endif;
118800180208
118900180208             RrnLast = S01nrr;
119000180206
119100180206           // - ?Record senza nessun dato?
119200180206           WHEN  S01des = *blank  and  S01de5 = *blank  and
119300180206                 S01ent = *zero   and  S01pep = *blank  and
119400180206                 S01apl = *blank  and  S01ap5 = *blank;
119500180206             // - ?Controllo se è stato tolto un record già inserito?
119600180206             If  S1Hdes <> *blank  and  S1Hde5 <> *blank  and
119700180206                 S1Hent <> *zero   and  S1Hpep <> *blank  and
119800180206                 S1Hapl <> *blank  and  S1Hap5 <> *blank;
119900180206               // - ?Reimpostazione a video dei dati già inseriti?
120000180206               S01des = S1Hdes;
120100180206               S01de5 = S1Hde5;
120200180206               evalR  S01pep = S1Hpep;
120300180206               S01ent = S1Hent;
120400180206               S01apl = S1Hapl;
120500180206               S01ap5 = S1Hap5;
120600180206               S01pke = S1Hpke;
120700180206               S01pwf = S1Hpwf;
120800180206               S01ann = S1Hann;
120900180206               $ErrGenerico = *on;
121000180206               $ErrMessage  = *on;
121100180206               $PosCurDES   = *on;
121200180206               VIDmsg =  sk_Msg(10);
121300180206               leavesr;
121400180206             EndIf;
121500180206
121600180206           // - ?Qualche campo è vuoto?
121700180206           OTHER;
121800180206             $ErrGenerico = *on;
121900180206             $ErrMessage  = *on;
122000180206             $PosCurDES   = *on;
122100180206             VIDmsg = sk_Msg(11);
122200180206             leavesr;
122300180206
122400180206         ENDSL;
122500180206
122600180206       ENDSR;
122700180205
122800180205       //--------------------------------------------------------------
122900180205       //? Visualizzazione postazioni finora gestite.                   ?
123000180205       //--------------------------------------------------------------
123100180205       BEGSR  sr_GesS05;
123200180205
123300180205         // - ?Inizializzazione videata?
123400180205         if  $InzS05;
123500180205           exsr  sr_InzS05;
123600180205           $InzS05 = *off;
123700180205         endif;
123800180205
123900180205         // - ?(Dis)Abilitazione tasti funzionali?
124000180205         $F6Attivo  = *off;
124100180205         $F8Attivo  = *off;
124200180205         $F9Attivo  = *off;
124300180205         $F12Attivo = *on;
124400180205
124500180205         // - ?Posizionamento cursore?
124600180205         if  C5CsrRrn > *zero;
124700180205           C5RcdNbr = C5CsrRrn;
124800180205         else;
124900180205           C5RcdNbr = 1;
125000180205         endif;
125100180205
125200180206         // - ?Emissione Testata & Piede con tasti funzionali abilitati?
125300180206         write  TB11T01;
125400180205         write  TB11P01;
125500180205
125600180205         // - ?Emissione videata?
125700180205         exfmt  TB11C05;
125800180205
125900180205         clear  VIDmsg;
126000180205         clear  $ErrMessage;
126100180205         clear  $ErrGenerico;
126200180205
126300180205         Select;
126400180205
126500180205           // - ?F3=Fine?
126600180205           When  InfDspF.dsp_aid = c_F03;
126700180205             unlock  TNTBE01L;
126800180205             $Fine = *on;
126900180205             leavesr;
127000180205
127100180205           // - ?F12=Ritorno?
127200180205           When  InfDspF.dsp_aid = c_F12;
127300180205             $Video = 'S1';
127400180205             leavesr;
127500180205
127600180205         EndSl;
127700180205
127800180205       ENDSR;
127900180205
128000180205       //--------------------------------------------------------------
128100180205       //? Preparazione videata con le postazioni finora gestite.       ?
128200180205       //--------------------------------------------------------------
128300180205       BEGSR  sr_InzS05;
128400180205
128500180205         // - ?Pulizia subfile?
128600180205         $SflDsp5N    = *on;
128700180205         $SflDspCtl5N = *on;
128800180205         write  TB11C05;
128900180205         $SflDspCtl5N = *off;
129000180205         $SflEnd5     = *off;
129100180205
129200180205         // - ?Caricamento subfile?
129300180205         //   ?(da S01 a S05)?
129400180205         S05nrr = 1;
129500180205         chain  S05nrr  TB11S01;
129600180205
129700180205         DoW  %found( TNTB11D );
129800180205
129900180205           clear  TB11S05;
130000180205
130100180205           if  S01ann = *blank;
130200180205
130300180205             S05des = S01des;
130400180205             S05pep = S01pep;
130500180205             S05ent = S01ent;
130600180205             S05apl = S01apl;
130700180205             S05ap5 = S01ap5;
130800180205             S05pke = S01pke;
130900180205             S05pwf = S01pwf;
131000180205             //*·S05ann = S01ann;
131100180205
131200180205           endif;
131300180205
131400180205           write  TB11S05;
131500180205
131600180205           S05nrr += 1;
131700180205           chain  S05nrr  TB11S01;
131800180205
131900180205         EndDo;
132000180205
132100180205         // - ?Visualizzazione del SFL (se ci sono dati)?
132200180205         $SflDsp5N = (S05nrr <= *zero);
132300180205
132400180205         // - ?Attivazione del SFLEND?
132500180205         $SflEnd5 = *on;
132600180205
132700180205         // - ?Posizionamento cursore al 1° rcd della 1ª pagina?
132800180205         if  S05nrr > *zero;
132900180205           C5RcdNbr = 1;
133000180205         else;
133100180205           clear  C5RcdNbr;
133200180205         endif;
133300180205
133400180205         C5CsrRrn = C5RcdNbr;
133500180205
133600180205       ENDSR;
133700180206
133800180206       //--------------------------------------------------------------
133900180206       //? Gestione window W01.                                         ?
134000180206       //--------------------------------------------------------------
134100180206       BEGSR  sr_GesW01;
134200180206
134300180206         // - ?Inizializzazione videata?
134400180206         if  $InzW01;
134500180206           exsr  sr_InzW01;
134600180206           $InzW01 = *off;
134700180206         endif;
134800180206
134900180213         // - ?Emissione window?
135000180219         if  $OrdAppl02   or
135100180219             $ErrGenerico;
135200180219           exfmt  TB11W01;
135300180219         endif;
135400180206
135500180206         clear  VIDmsg;
135600180206         clear  $ErrMessage;
135700180206         clear  $ErrGenerico;
135800180206
135900180206         // - ?F12=Ritorno?
136000180206         if  InfDspF.dsp_aid = c_F12;
136100180206           $Video = 'S1';
136200180206           leavesr;
136300180206         endif;
136400180206
136500180213         // - ?Controlli?
136600180206         exsr  sr_CtrW01;
136700180213         if  $ErrGenerico = *on  or
136800180219             InfDspF.dsp_aid <> c_F08;
136900180206           leavesr;
137000180206         endif;
137100180213
137200180213         // - ?Impostazione sequenza di ordinamento in ogni rec del sfl?
137300180213         exsr  sr_Set_Ord_Sfl;
137400180213
137500180213         // - ?Ordinamento del subfile?
137600180208         exsr  sr_SortSfl;
137700180206
137800180213         // - ?Ritorno alla gestione del subfile?
137900180206         $Video  = 'S1';
138000180206
138100180206       ENDSR;
138200180206
138300180206       //--------------------------------------------------------------
138400180206       //? Inizializzazione window W01.                                 ?
138500180206       //--------------------------------------------------------------
138600180206       BEGSR  sr_InzW01;
138700180206
138800180206         clear  TB11W01;
138900180206
139000180206         // - ?Caricamento Applicazioni?
139100180206         W1Capl1 = sk_APL(01);
139200180219         W1Capl2 = sk_APL(02);
139300180219         W1Capl3 = sk_APL(03);
139400180219         W1Capl4 = sk_APL(04);
139500180219         W1Capl5 = sk_APL(05);
139600180219         W1Capl6 = sk_APL(06);
139700180206
139800180206         // - ?Decodifica Applicazioni?
139900180206         For  xx = 1  To  6;
140000180206
140100180206           clear  yy;
140200180206           If  sk_APL(xx) <> *blank;
140300180206
140400180206             yy = %lookup( sk_APL(xx) : sk_MTP );
140500180206             if  yy > *zero;
140600180206
140700180206               select;
140800180206                 when  xx  =  1;
140900180206                   W1Dapl1 = sk_MTPdes(yy);
141000180206                   W1Nseq1 = xx;
141100180206                 when  xx  =  2;
141200180206                   W1Dapl2 = sk_MTPdes(yy);
141300180206                   W1Nseq2 = xx;
141400180206                   $OrdAppl02 = *on;
141500180206                 when  xx  =  3;
141600180206                   W1Dapl3 = sk_MTPdes(yy);
141700180206                   W1Nseq3 = xx;
141800180206                   $OrdAppl03 = *on;
141900180206                 when  xx  =  4;
142000180206                   W1Dapl4 = sk_MTPdes(yy);
142100180206                   W1Nseq4 = xx;
142200180206                   $OrdAppl04 = *on;
142300180206                 when  xx  =  5;
142400180206                   W1Dapl5 = sk_MTPdes(yy);
142500180206                   W1Nseq5 = xx;
142600180206                   $OrdAppl05 = *on;
142700180206                 when  xx  =  6;
142800180206                   W1Dapl6 = sk_MTPdes(yy);
142900180206                   W1Nseq6 = xx;
143000180206                   $OrdAppl06 = *on;
143100180206               endsl;
143200180206
143300180206             endif;
143400180206
143500180206           EndIf;
143600180206
143700180206         EndFor;
143800180206
143900180206       ENDSR;
144000180206
144100180206       //--------------------------------------------------------------
144200180206       //? Controllo dati immessi nella window W01.                     ?
144300180206       //--------------------------------------------------------------
144400180206       BEGSR  sr_CtrW01;
144500180206
144600180206         // - ?Spegnimento indicatori di posizionamento cursore?
144700180219         %subst( IndDspF : 50 ) = *off;
144800180206
144900180206         xx = %lookup( *blank : sk_APL );
145000180206         if  xx = *zero;
145100180206           xx = %elem( sk_APL );
145200180206         endif;
145300180206
145400180206         eval  sk_Ord = *hival;
145500180206         sk_Ord(01) = %editc( W1Nseq1 : 'X' ) + W1Capl1;
145600180206         if  $OrdAppl02;
145700180206           sk_Ord(02) = %editc( W1Nseq2 : 'X' ) + W1Capl2;
145800180206         endif;
145900180206         if  $OrdAppl03;
146000180206           sk_Ord(03) = %editc( W1Nseq3 : 'X' ) + W1Capl3;
146100180206         endif;
146200180206         if  $OrdAppl04;
146300180206           sk_Ord(04) = %editc( W1Nseq4 : 'X' ) + W1Capl4;
146400180206         endif;
146500180206         if  $OrdAppl05;
146600180206           sk_Ord(05) = %editc( W1Nseq5 : 'X' ) + W1Capl5;
146700180206         endif;
146800180206         if  $OrdAppl06;
146900180206           sk_Ord(06) = %editc( W1Nseq6 : 'X' ) + W1Capl6;
147000180206         endif;
147100180206
147200180206       ENDSR;
147300180206
147400180206       //--------------------------------------------------------------
147500180206       //? Inizializzazione window W01.                                 ?
147600180206       //--------------------------------------------------------------
147700180206       BEGSR  sr_InzWxx;
147800180206
147900180206         clear  TB11W01;
148000180206
148100180206         // - ?Caricamento Applicazioni?
148200180206         W1Capl1 = §MFPapl;
148300180219         W1Capl2 = §MFPap2;
148400180219         W1Capl3 = §MFPap3;
148500180219         W1Capl4 = §MFPap4;
148600180219         W1Capl5 = §MFPap5;
148700180219         W1Capl6 = §MFPap6;
148800180206
148900180206         // - ?Decodifica Applicazioni?
149000180206         For  xx = 1  To  6;
149100180206
149200180206           clear  wAPL;
149300180206           select;
149400180206             when  xx = 1;
149500180213               wAPL = §MFPapl;
149600180206             when  xx = 2;
149700180213               wAPL = §MFPap2;
149800180206             when  xx = 3;
149900180213               wAPL = §MFPap3;
150000180206             when  xx = 4;
150100180213               wAPL = §MFPap4;
150200180206             when  xx = 5;
150300180213               wAPL = §MFPap5;
150400180206             when  xx = 6;
150500180213               wAPL = §MFPap6;
150600180206           endsl;
150700180206           clear  yy;
150800180206           If  wAPL <> *blank;
150900180206             yy = %lookup( wAPL : sk_MTP );
151000180206             if  yy <= *zero;
151100180206               select;
151200180206                 when  xx  =  1;
151300180206                   W1Dapl1 = sk_MTPdes(yy);
151400180206                   W1Nseq1 = xx;
151500180206                 when  xx  =  2;
151600180206                   W1Dapl2 = sk_MTPdes(yy);
151700180206                   W1Nseq2 = xx;
151800180206                   $OrdAppl02 = *on;
151900180206                 when  xx  =  3;
152000180206                   W1Dapl3 = sk_MTPdes(yy);
152100180206                   W1Nseq3 = xx;
152200180206                   $OrdAppl03 = *on;
152300180206                 when  xx  =  4;
152400180206                   W1Dapl4 = sk_MTPdes(yy);
152500180206                   W1Nseq4 = xx;
152600180206                   $OrdAppl04 = *on;
152700180206                 when  xx  =  5;
152800180206                   W1Dapl5 = sk_MTPdes(yy);
152900180206                   W1Nseq5 = xx;
153000180206                   $OrdAppl05 = *on;
153100180206                 when  xx  =  6;
153200180206                   W1Dapl6 = sk_MTPdes(yy);
153300180206                   W1Nseq6 = xx;
153400180206                   $OrdAppl06 = *on;
153500180206               endsl;
153600180206             endif;
153700180206           EndIf;
153800180206
153900180206         EndFor;
154000180206
154100180206       ENDSR;
154200180213
154300180213       //--------------------------------------------------------------
154400180213       //? Impostazione sequenza di ordinamento nel subfile.            ?
154500180213       //--------------------------------------------------------------
154600180213       BEGSR  sr_Set_Ord_Sfl;
154700180213
154800180213         sortA  sk_Ord;
154900180213
155000180213         For  S01nrr = 1  To  RrnLast;
155100180213
155200180213            chain  S01nrr  TB11S01;
155300180213
155400180213            If  %found( TNTB11D );
155500180213
155600180213              Select;
155700180213                When  S01apl = *blank;
155800180213                  S1Hseq = *hival;
155900180213                When  S01ann = 'A';
156000180213                  S1Hseq = *hival;
156100180213                Other;
156200180213                  S1Hseq = *hival;
156300180213                  for  xx = 1  To  %elem( sk_Ord );
156400180213                    if  %subst( sk_Ord( xx ) : 2 : 1 ) = S01apl;
156500180213                      S1Hseq = %int( %subst( sk_Ord( xx ) : 1 : 1 ) );
156600180219                      leave;
156700180213                    endif;
156800180213                  endfor;
156900180213              EndSl;
157000180213
157100180213              update  TB11S01;
157200180213
157300180213            EndIf;
157400180213
157500180213         EndFor;
157600180213
157700180213       ENDSR;
157800180206
157900180206       //--------------------------------------------------------------
158000180206       //? Ordinamento SubFile S01.                                     ?
158100180208       //?  This subroutine sorts the subfile records.                 ?
158200180208       //--------------------------------------------------------------
158300180208       BEGSR  sr_SortSfl;
158400180208
158500180213         // - ?Impostazione NRR dell'ultimo record del subfile          ?
158600180213         //   ?(già fatto in sr_CtrS01)                                 ?
158700180208         //*·RrnLast  = S01nrr;
158800180208
158900180208         C1RcdNbr = 1;
159000180208         clear  C1CsrRrn;
159100180208         clear  S01nrr;
159200180208         clear  VIDmsg;
159300180208         $SflNxtChg   = *off;
159400180208         $ErrMessage  = *off;
159500180208         $ErrGenerico = *off;
159600180208         %subst(IndDspF : 50) = *off;
159700180208
159800180208         //?___________________________________________________________?
159900180213         // ?Initialize the key fields to sort on.?
160000180213         // ?There is one extra field not in the subfile -- Selected --?
160100180213         // ?that is added to the record. This field is used to place?
160200180213         // ?selected records in front of nonselected records.?
160300180208         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
160400180208
160500180208         clear  QLGKL;
160600180208         clear  QLGSKL;
160700180208         clear  QLGSCB;
160800180208         clear  QLGSCB00;
160900180208
161000180219         // - ?Ordinamento per Nr. Sequenza in ordinamento (da window) /?
161100180219         //                   ?Tipo Applicazione FV2 /?
161200180219         //                   ?Nr. Sequenza a video /?
161300180219         //                   ?Postazione Entrata /?
161400180219         //                   ?Tipo Applicazione FV5?
161500180219         // - ?5 campi chiave?
161600180219         QLGNBRK  = 5;
161700180219         // - ?1° campo: Numero Sequenza del Tipo Applicazione (SH1seq)?
161800180219         //             ?a posizione   1,  1 byte, num., ascending.?
161900180219         QLGSP    = 1;
162000180219         QLGSS    = %size(S1Hseq);
162100180219         QLGDT    = c_Numero;
162200180219         QLGSO    = c_Ascendente;
162300180219         QLGKL(1) = QLGSKL;
162400180219         // - ?2° campo: Tipo Applicazione (S01apl)?
162500180219         //             ?a posizione  76,  1 byte, char, ascending.?
162600180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
162700180219                    %size(S1Hde5)   + %size(S1Hpep)   +
162800180219                    %size(S1Hent)   + %size(S1Hapl)   +
162900180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
163000180219                    %size(S1Hpke)   + %size(S1Hann)   +
163100180219                    %size(S01opz)   + %size(S01des)   +
163200180219                    %size(S01de5)   + %size(S01pep)   +
163300180219                    %size(S01ent)   + 1;
163400180219         QLGSS    = %size(S01apl);
163500180219         QLGDT    = c_Carattere;
163600180219         QLGSO    = c_Ascendente;
163700180219         QLGKL(2) = QLGSKL;
163800180219         // - ?3° campo: Numero Entrata (S01ent)?
163900180219         //             ?a posizione  74,  2 byte, num., ascending.?
164000180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
164100180219                    %size(S1Hde5)   + %size(S1Hpep)   +
164200180219                    %size(S1Hent)   + %size(S1Hapl)   +
164300180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
164400180219                    %size(S1Hpke)   + %size(S1Hann)   +
164500180219                    %size(S01opz)   + %size(S01des)   +
164600180219                    %size(S01de5)   + %size(S01pep)   + 1;
164700180219         QLGSS    = %size(S01ent);
164800180219         QLGDT    = c_Numero;
164900180219         QLGSO    = c_Ascendente;
165000180219         QLGKL(3) = QLGSKL;
165100180219         // - ?4° campo: Postazione Entrata (S01pep)?
165200180219         //             ?a posizione  64,  1 byte, char, ascending.?
165300180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
165400180219                    %size(S1Hde5)   + %size(S1Hpep)   +
165500180219                    %size(S1Hent)   + %size(S1Hapl)   +
165600180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
165700180219                    %size(S1Hpke)   + %size(S1Hann)   +
165800180219                    %size(S01opz)   + %size(S01des)   +
165900180219                    %size(S01de5)   + 1;
166000180219         QLGSS    = %size(S01pep);
166100180219         QLGDT    = c_Carattere;
166200180219         QLGSO    = c_Ascendente;
166300180219         QLGKL(4) = QLGSKL;
166400180219         // - ?5° campo: Tipo Applicazione (S01ap5)?
166500180219         //             ?a posizione  76,  1 byte, char, ascending.?
166600180219         QLGSP    = %size(S1Hseq)   + %size(S1Hdes)   +
166700180219                    %size(S1Hde5)   + %size(S1Hpep)   +
166800180219                    %size(S1Hent)   + %size(S1Hapl)   +
166900180219                    %size(S1Hap5)   + %size(S1Hpwf)   +
167000180219                    %size(S1Hpke)   + %size(S1Hann)   +
167100180219                    %size(S01opz)   + %size(S01des)   +
167200180219                    %size(S01de5)   + %size(S01pep)   +
167300180219                    %size(S01ent)   + %size(S01apl) + 1;
167400180219         QLGSS    = %size(S01ap5);
167500180219         QLGDT    = c_Carattere;
167600180219         QLGSO    = c_Ascendente;
167700180219         QLGKL(5) = QLGSKL;
167800180208
167900180219         // - ?Load other sort parameters.?
168000180208         QLGLB   = 80 + 16 * c_MaxKey;
168100180208         QLGRL   = %size(SflRcd) - 1;
168200180208         QLGRT   = 8;
168300180208         QLGOKL  = 80;
168400180208         QLGLKE  = 16;
168500180208         QLGLSS  = 290;
168600180219         // - ?Initialize Sort I/O API fields.?
168700180208         QLGRL00 = QLGRL;
168800180208         QLGRC00 = 1;
168900180208         clear  QUSEI;
169000180208         QUSBPRV = %size(QUSEC);
169100180208
169200180219         // - ?First step - Initialize the sort routine.?
169300180208         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
169400180208                      SizeList : ReturnSize : QUSEC );
169500180208
169600180219         // - ?Next step - Write records to I/O routine.?
169700180208         QLGRT00  = c_Put;
169800180208         For  Nrr = 1  To  RrnLast;
169900180219           chain  Nrr  TB11S01;
170000180219           // - ?Solo le righe con Selected = 'Y' sono riordinate,?
170100180219           //   ?quindi per fare un ordinamento di tutte le righe?
170200180219           //   ?metto 'Y' sempre.?
170300180208           Selected = 'Y';
170400180208           clear  QUSEI;
170500180208           QUSBPRV  = %size(QUSEC);
170600180208           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
170700180208                         SizeList : NotUsed : QUSEC );
170800180208         EndFor;
170900180208
171000180219         // - ?Next step - Signal end of input, clear subfile for reload.?
171100180208         QLGRT00 = c_EndPut;
171200180208         clear  QUSEI;
171300180208         QUSBPRV = %size(QUSEC);
171400180208         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
171500180208                       SizeList : NotUsed : QUSEC );
171600180208
171700180219         // - ?Pulizia subfile?
171800180208         $SflDsp_N    = *on;
171900180208         $SflDspCtl_N = *on;
172000180219         write  TB11C01;
172100180208         $SflDspCtl_N = *off;
172200180208         $SflEnd      = *off;
172300180208
172400180219         // - ?Final step - Write the records back to the subfile.?
172500180208         QLGRT00  = c_Get;
172600180208         For  Nrr = 1  To RrnLast;
172700180208           clear  QUSEI;
172800180208           QUSBPRV = %size(QUSEC);
172900180208           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
173000180208                         QLGRL00  : NotUsed : QUSEC );
173100180219           // - ?Ripristino indicatori utilizzati nel sfl rec?
173200180208           $SflNxtChg = ( S01opz <> *zero );
173300180208           S01nrr = Nrr;
173400180219           write  TB11S01;
173500180208         EndFor;
173600180219
173700180219         // - ?RI-Scrittura eventuali ultimi record vuoti?
173800180219         DoW  S01nrr < c_SflSizMax;
173900180219           S01nrr += 1;
174000180219           exsr  sr_RecVuotoS01;
174100180219         EndDo;
174200180208
174300180208         C1CsrRrn  = 1;
174400180208
174500180219         // - ?Visualizzazione del SFL (se ci sono dati)?
174600180208         $SflDsp_N = ( S01nrr <= *zero );
174700180208
174800180219         // - ?Attivazione del SFLEND?
174900180219         $SflEnd   = *on;
175000180208
175100180219         // - ?(Dis)Abilitazione tasti funzionali?
175200180219         //*·// · ?F8 = Ordinamento x ...?
175300180219         //*·$F8Attivo = ( S01nrr > 1 );
175400180208
175500180208       ENDSR;
175600180205
175700180205       //--------------------------------------------------------------
175800180205       //? Scrittura records nel *file TNTBE00F.                        ?
175900180205       //--------------------------------------------------------------
176000180205       BEGSR  sr_WrtTNTBE;
176100180205
176200180205         if  S01nrr <= *zero;
176300180205           leavesr;
176400180205         endif;
176500180205
176600180205         clear  S01nrr;
176700180205
176800180205         For  S01nrr = 1  To  c_SflSizMax;
176900180205
177000180205           chain  S01nrr  TB11S01;
177100180205           if  Not %found( TNTB11D );
177200180205             leave;
177300180205           endif;
177400180205
177500180205           // - ?Memorizzazione dati in tab. "MSL"?
177600180205           clear  dMSL;
177700180205           §MSLdes = S01des;
177800180205           evalR  §MSLpep = S01pep;
177900180205           §MSLent = S01ent;
178000180205           if  S01pke = 'S';
178100180205             §MSLpke = S01pke;
178200180205           endif;
178300180205           §MSLapl = S01apl;
178400180205           §MSLap5 = S01ap5;
178500180205           §MSLde5 = S01de5;
178600180205           §MSLpwf = S01pwf;
178700180205
178800180206           // - ?Impostazione chiave di accesso al file TNTBE00F?
178900180205           wKe2 = S01nrr;
179000180205           clear  keyTNTBE01;
179100180205           k_TBEcod = c_Tab;
179200180205           k_TBEke1 = V1Cfil;
179300180205           k_TBEke2 = %editc( wKe2 : 'X' );
179400180205
179500180206           // - ?Apertura file tabelle del 1° S.I. (sede)?
179600180205           if  w_SisInf <> 1   or   Not %open(TNTBE01L);
179700180205             w_SisInf = 1;
179800180205             exsr  sr_OpenFileTab;
179900180205           endif;
180000180205
180100180206           // - ?Ciclo di elaborazione per ogni sistema informativo?
180200180205           For  w_SisInf = 1  To  %elem(sk_Libr);
180300180205
180400180206             // - ?Apertura degli archivi?
180500180205             if  w_SisInf > 1;
180600180205               exsr  sr_OpenFileTab;
180700180205             endif;
180800180205
180900180205             chain  %kds( keyTNTBE01 )  TNTBE000;
181000180205
181100180206             // - ?Aggiornamento file TNTBE00F?
181200180205             If  %found( TNTBE01L );
181300180205
181400180205               dMSL_vd = TBEuni;
181500180205
181600180205               if  TBEatb  <> S01ann  or
181700180205                   dMSL_vd <> dMSL;
181800180205
181900180205                 exsr  sr_RiempiCampi;
182000180205                 //_______________
182100180205                 UPDATE  TNTBE000;
182200180205                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
182300180205
182400180205               endif;
182500180205
182600180206             // - ?Scrittura file TNTBE00F?
182700180205             Else;
182800180205
182900180205               clear  TNTBE000;
183000180205
183100180205               if  S01des <> *blank  and  S01de5 <> *blank  and
183200180205                   S01pep <> *blank  and  S01ent <> *zero   and
183300180205                   S01apl <> *blank  and  S01ap5 <> *blank;
183400180205
183500180205                 TBEcod = k_TBEcod;
183600180205                 TBEke1 = k_TBEke1;
183700180205                 TBEke2 = k_TBEke2;
183800180205                 exsr  sr_RiempiCampi;
183900180205                 //______________
184000180205                 WRITE  TNTBE000;
184100180205                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
184200180205
184300180205               endif;
184400180205
184500180205             EndIf;
184600180205
184700180205           EndFor;
184800180205
184900180205         EndFor;
185000180205
185100180205         unlock  TNTBE01L;
185200180205
185300180205       ENDSR;
185400161027
185500161027       //--------------------------------------------------------------
185600180206       //? Apertura del file tabelle nel sistema informativo impostato. ?
185700161027       //--------------------------------------------------------------
185800161027       BEGSR  sr_OpenFileTab;
185900161027
186000180206         // - ?Chiusura (eventuale) file tabelle?
186100161027         if  %open(TNTBE01L);
186200161027           close  TNTBE01L;
186300161027         endif;
186400161027
186500180206         // - ?Apertura file tabelle?
186600161104         ds_Libr  = sk_Libraries(w_iSystem);
186700161104         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
186800161027         open  TNTBE01L;
186900161027
187000161027       ENDSR;
187100161027
187200161027       //--------------------------------------------------------------
187300180206       //? Impostazione dati nel record in aggiornamento.               ?
187400161027       //--------------------------------------------------------------
187500161027       //? N.B.                                                        ?
187600180206       //? L'annullamento  ed  il ripristino  vanno lasciati in         ?
187700180206       //?   trasmissione (vedi flag TBEFTR).                           ?
187800180206       //? L'aggiornamento  e  l'inserimento  NO: vanno registrati      ?
187900180206       //?   subito nello stesso file di entrambi i S.I. - in due       ?
188000180206       //?   cicli diversi - ma NON vanno messi in trasmissione.        ?
188100161027       //--------------------------------------------------------------
188200161027       BEGSR  sr_RiempiCampi;
188300161027
188400180206         // - ?Dati della tabella?
188500161027         TBEuni = dMSL;
188600180205         TBEatb = S01ann;
188700161027
188800180206         // - ?Dati della matrice?
188900180205         TBEapl = TBXapl;
189000180205         //*·TBEftt = TBXftt;
189100161027         TBEftt = 'S';
189200161027         //*·TBEflt = sav_TBEflt;
189300161027         clear  TBEflt;
189400161027
189500180206         // - ?Dati della "trasmissione"?
189600180205         If  S01ann = *blank  and  S01ann = S1Hann;
189700161027           if  w_SisInf = 1;
189800161027             TBEftr = 'T';
189900161027           else;
190000161027             TBEftr = 'R';
190100161027           endif;
190200161027         Else;
190300161027           clear  TBEftr;
190400161027         EndIf;
190500161027         TBEdtr = wDate;
190600161027
190700161027       ENDSR;
190800180205
190900180205       //--------------------------------------------------------------
191000180206       //? Apertura del file tabelle nel sistema informativo impostato. ?
191100180205       //--------------------------------------------------------------
191200180205       BEGSR  sr_RoutEnd;
191300180205
191400180206         // - ?Chiusura pgm?
191500180205         return;
191600180205
191700180205       ENDSR;
191800161027
191900161027       //--------------------------------------------------------------
192000180206       //? Definizione schiere a tempo di compilazione                  ?
192100161027       //--------------------------------------------------------------
192200161027
192300180206** -- ?sk_iSystem / sk_Libraries: ?Sistemi AS/400 & Librerie con entrambi i file?
192400161027SETRAS  GAITRAGRU FILTRAGRU
192500161027AS888   GAITRAGRPSFILTRAGRPF
192600180206** -- ?sk_Msg: ?Messaggi di Errore? --------------------------------------------*
192700180205Inserire Filiale                                                                1
192800180205Filiale inesistente                                                             2
192900180205Filiale annullata                                                               3
193000180205Filiale non valida                                                              4
193100180206Postazione Entrata già inserita nella posizione n°                              5
193200180206N° Postazione già usato per la postazione "                                     6
193300180206Codice tipo applicazione non valido                                             7
193400180206Tipo applicazione non valido per Software VDL                                   8
193500180206Entrata picking ammessa solo per applicazione di FV 2 = "K"                     9
193600180206Non è possibile cancellare un record esistente                                 10
193700180206Inserire tutti i dati                                                          11
193800180206Record già annullato                                                           12
193900180206Impossibile ripristinare un record già valido                                  13
194000180206Opzione errata                                                                 14
194100180206Inserire l'opzione o (in alternativa) l'interrogazione                         15
