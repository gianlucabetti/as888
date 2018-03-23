000100080206      //--------------------------------------------------------------
000200081009      //?TRMK07R - Normalizzazione  CLIENTI POTENZIALI
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100080208      // - Anagrafica clienti potenziali
001200080619     fTNCPO00F  uf   e             disk    prefix(F_)
001300080619     f                                     rename(tncpo000:tncpofis)
001400080619     fTNCPO01L  uf   e           k disk    infds(cpo01)
001500080208     fTNCPO02L  if   e           k disk    rename(TNCPO000 : TNCPO002)
001600080610     f                                     prefix(R_)
001700140715     fTNCPO11L  Uf A e           k disk
001800130916      // - Anagrafica Commerciali
001900130916     fAZCMM01L  if   e           k disk
002000080208      // - Organigramma filiale/aziendale
002100080206     fAZORG01L  if   e           k disk
002200080208      // - Tabelle
002300080206     fTABEL00F  if   e           k disk
002400080604     fTNTBE01l  if   e           k disk
002500080604
002600080619     ftfntc01l  uf a e           k disk
002700100525     fticpn03l  uf a e           k disk
002800080929     fticpi01l  uf a e           k disk
002900100525     ftiatc04l  uf   e           k disk
003000100525     ftiatl01l  uf   e           k disk
003100080624
003200080624     fcnaco16l  uf   e           k disk
003300080624     fTFaco16l  uf   e           k disk    rename(cnaco000:tfaco000)
003400081112     f                                     extfile(WTFACO) USROPN
003500110803     fTIVIS00F  If   e             disk
003600160901      // - File Variazioni
003700160901     fTNCPVD1L  uf a e           k disk
003800160901     fTNCPVT1L  uf a e           k disk
003900080206
004000080208      // - Video Interrogazione anagrafica clienti potenziali
004100080409     fTRMK07D   cf   e             workstn
004200080410     f                                     sfile(MK07S01 : S01nrr)
004300080208     f                                     indds(IndDspF)
004400080206     f                                     infds(InfDspF)
004500080411     f                                     sfile(MK07S02 : S02nrr)
004600080603     f                                     sfile(MK07S03 : S03nrr)
004700080206
004800080206      //---------------------------------------------------------------
004900080206      //?Definizione costanti.
005000080206      //---------------------------------------------------------------
005100080411      // - Numero di record in ciascuna videata di subfile
005200080414     d c_SflPag        c                   const(17)
005300080411     d W_SflPag        s              3  0 inz
005400080530     d PAgric          s              3  0 inz
005500080530     d totrig          s              5  0 inz
005600080207
005700080207      // - Tasti funzionali a video
005800080207     d c_F01           c                   const(x'31')
005900080207     d c_F02           c                   const(x'32')
006000080207     d c_F03           c                   const(x'33')
006100080207     d c_F05           c                   const(x'35')
006200080207     d c_F06           c                   const(x'36')
006300080207     d c_F07           c                   const(x'37')
006400080207     d c_F08           c                   const(x'38')
006500080207     d c_F09           c                   const(x'39')
006600080207     d c_F10           c                   const(x'3A')
006700080207     d c_F12           c                   const(x'3C')
006800080207     d c_F13           c                   const(x'B1')
006900080207     d c_F14           c                   const(x'B2')
007000080207     d c_F15           c                   const(x'B3')
007100080207     d c_F16           c                   const(x'B4')
007200080207     d c_F17           c                   const(x'B5')
007300080207     d c_F18           c                   const(x'B6')
007400080207     d c_F19           c                   const(x'B7')
007500080207     d c_F20           c                   const(x'B8')
007600080207     d c_F21           c                   const(x'B9')
007700080207     d c_F22           c                   const(x'BA')
007800080207     d c_F23           c                   const(x'BB')
007900080207     d c_F24           c                   const(x'BC')
008000080207     d c_Enter         c                   const(x'F1')
008100080207     d c_RollDown      c                   const(x'F4')
008200080207     d c_RollUp        c                   const(x'F5')
008300080214
008400080214      // - Attributi di visualizzazione
008500080215      //  -  DspAtr() - Normale
008600080214     d C_dspatr_Norm   c                   const(x'20')
008700080215      //  -  DspAtr(RI)
008800080214     d C_dspatr_RI     c                   const(x'21')
008900080215      //  -  DspAtr(ND)
009000080214     d C_dspatr_ND     c                   const(x'27')
009100080215      //  -  DspAtr(BL) / Color(Red)
009200080214     d C_dspatr_BL     c                   const(x'28')
009300080206
009400080206      //---------------------------------------------------------------
009500080206      //?Definizione schiere.
009600080206      //---------------------------------------------------------------
009700080206
009800080206      // - Messaggi di errore
009900100525     d MSG             s             78    dim(43) ctdata perrcd(1)
010000080415     d FILIALI         s             11  0 dim(8)
010100080522     d nrrfil          s              5  0 dim(8)
010200080415     d SEDI            s             11  0 dim(2)
010300080522     d nrrSed          s              5  0 dim(2)
010400080603     d FUSIONE         s             11  0 dim(3)
010500080603     d nrrFus          s              5  0 dim(3)
010600111115     d CatFus          s              1    dim(3)
010700080603     d tipoFus         s              1    dim(3)
010800080609
010900080929     d keysfl3         s              7    dim(40)
011000080929     d key3pot1        s              3    dim(40)
011100080929     d key3pot2        s              3    dim(40)
011200080929     d keynrr          s              5  0 dim(40)
011300080929     d keyrig          s              2  0 dim(40)
011400080929     d keycont         s              3  0 dim(40)
011500081013     d
011600081013     d NTCcod1         s             21    dim(40)
011700081013     d DTimm1          s              4    dim(40)
011800081013     d sottot1         s              4    dim(40)
011900080609     d
012000080929     d NTCcod2         s             21    dim(40)
012100080929     d DTimm2          s              4    dim(40)
012200081013     d sottot2         s              4    dim(40)
012300080929
012400080929     d kifoord         s             28    dim(50)
012500080206
012600080206      //---------------------------------------------------------------
012700080206      //?Definizione aree dati.
012800080206      //---------------------------------------------------------------
012900080206
013000080206      // - Dati utente
013100080206     d §AzUte        e ds                  extname(AZUTE00F)
013200080206     d                                     dtaara
013300080206     d §DatiUte      e ds                  extname(dDatiUte)
013400080206     d                                     dtaara
013500080206
013600080206      //---------------------------------------------------------------
013700080206      //?Definizione strutture dati.
013800080206      //---------------------------------------------------------------
013900080619     D CPO01           DS
014000080619     D  cp1NRR               397    400B 0
014100080206
014200080206      // - Status
014300080206
014400080206      // - InfDS
014500080206     d InfDspF         ds
014600080207     d  dsp_aid              369    369a                                        AID byte
014700080206
014800080206      // - Indicatori su DspF
014900080208     d IndDspF         ds
015000080206        // - Indicatori di gestione del subfile
015100080626     d  Intpotenz                     1n   overlay(IndDspF : 01)
015200081009     d  IndInfo                       1n   overlay(IndDspF : 10)
015300080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
015400080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
015500080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
015600080206     d  SflEnd                        1n   overlay(IndDspF : 33)
015700080206        // - Indicatori di errore
015800080206     d  errMessage                    1n   overlay(IndDspF : 28)
015900080523     d  errSEDE                       1n   overlay(IndDspF : 40)
016000080523     d  errFIL                        1n   overlay(IndDspF : 41)
016100111115     d  errFLS                        1n   overlay(IndDspF : 43)
016200080523     d  errFILIALE                    1n   overlay(IndDspF : 44)
016300080411     d  errRAG                        1n   overlay(IndDspF : 45)
016400080523     d  ProtPOTSED                    1n   overlay(IndDspF : 46)
016500080415     d  ErrPRV                        1n   overlay(IndDspF : 47)
016600080415     d  ErrScelta                     1n   overlay(IndDspF : 48)
016700080523     d  ProtPOTFIL                    1n   overlay(IndDspF : 49)
016800080603     d  errFusione                    1n   overlay(IndDspF : 50)
016900080603     d  ProtPOTDUE                    1n   overlay(IndDspF : 51)
017000080604     d  noScelta1                     1n   overlay(IndDspF : 52)
017100080604     d  noScelta2                     1n   overlay(IndDspF : 53)
017200080605     d  TestataHI                     1n   overlay(IndDspF : 54)
017300080609     d  ERRsfl3_1                     1n   overlay(IndDspF : 55)
017400080609     d  ERRsfl3_2                     1n   overlay(IndDspF : 56)
017500080610     d  errTipoFus1                   1n   overlay(IndDspF : 57)
017600080610     d  errTipoFus2                   1n   overlay(IndDspF : 58)
017700081010     d  InfoScelta1                   1n   overlay(IndDspF : 62)
017800081010     d  InfoScelta2                   1n   overlay(IndDspF : 63)
017900111116     d  RIcodif1                      1n   overlay(IndDspF : 64)
018000111116     d  RIcodif2                      1n   overlay(IndDspF : 65)
018100080609     d  SFLRosso1                     1n   overlay(IndDspF : 95)
018200080609     d  SFLRosso2                     1n   overlay(IndDspF : 96)
018300080606     d  ColorROSSO2                   1n   overlay(IndDspF : 97)
018400080606     d  ColorROSSO1                   1n   overlay(IndDspF : 98)
018500080606     d  errGenerico                   1n   overlay(IndDspF : 99)
018600080206
018700080206      // - Parametri ricevuti
018800080206     d KPJBA         e ds
018900080206
019000080206      // - Reperimento dati utente
019100080206     d TIBS34ds      e ds
019200080206     d dUte01        e ds
019300080206
019400080206      // - Ricerca/Controllo tabelle
019500080206     d TIBS02ds      e ds                  inz
019600110304
019700110304      // - Calcolo categoria potenziale
019800110304     d TRMK05ds      e ds
019900160901
020000160901       // -?Storicizzazione variazioni
020100160901     d TRMK30DS      e ds
020200160901     d TNCPO_30      e ds                  extname(TNCPO00F) inz
020300160901     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
020400160901     d                                     prefix(b_)
020500160901     d TICPI_30      e ds                  extname(TICPI00F) inz
020600111115
020700140918      // -?Tabella CPO - categoria potenziale
020800111115     d dCPO          e ds                  inz
020900080206
021000080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
021100080206     d dLAT          e ds                  inz
021200080206
021300080206      // - Tabella 1L = Categorie Merceologiche
021400080207     d ds1L          e ds                  inz
021500140918
021600080410      // - ds del campo CPORST
021700080410     d DCPO01        e ds                  inz
021800140918      // -?DS per campo CPOTEL di TNCPO00F
021900140918     d dCPO02        e ds                  inz  qualified
022000160901
022100160901      // -?DS per Variazione di tipo F
022200160901     d dCPV_F        e ds                  prefix(f)
022300160901      // -?DS per Variazione di tipo G
022400160901     d dCPV_G        e ds                  prefix(g)
022500080410
022600080604     d ds1T          e ds                  inz
022700080929     d dIFO          e ds                  inz
022800100524     d ticpnds       e ds                  inz  extname(TICPN00F)
022900140918     d TNCPO1ds      e ds                  inz  extname(TNCPO10F)
023000140918     d SavCPO1ds     e ds                  inz  extname(TNCPO10F)
023100140918     d                                     prefix(X_)
023200140918     d TRMK01ds      e ds                  inz
023300080929     D TRMK50DS      E DS
023400080606     D TNTA12DS      E DS
023500140918     d tnta43ds      e ds                  inz
023600080617     D TRUL31DS      E DS
023700140918     d   POG                          3  0 DIM(250) overlay(o31pog)
023800080206
023900080206      //---------------------------------------------------------------
024000080206      //?Definizione variabili globali.
024100080206      //---------------------------------------------------------------
024200080206
024300080206      // - Flags booleani
024400111115     d $Contattato1    s               n   inz(*off)
024500111115     d $Contattato2    s               n   inz(*off)
024600111115     d $EndAtt         s               n   inz(*off)
024700080208     d $Fine           s               n   inz(*off)
024800080208     d $InzD01         s               n   inz(*on)
024900080208     d $InzS01         s               n   inz(*on)
025000080414     d $InzS02         s               n   inz(*on)
025100080603     d $InzS03         s               n   inz(*on)
025200080206     d $ErrGrave       s               n   inz(*off)
025300080207     d $EoF            s               n   inz(*off)
025400080208     d $RecOK          s               n   inz(*off)
025500080206
025600080207      // - Campi associati al video
025700080207     d $Video          s              2    inz('D1')
025800080208     d S01nrr          s              4  0 inz
025900080414     d S02nrr          s              4  0 inz
026000080603     d S03nrr          s              4  0 inz
026100080414     d wPag            s              4  0 inz
026200080414     d wRig            s              3  0 inz
026300080523     d record          s              2  0 inz(7)
026400080414
026500100805     d d11cpo          s                   like(cpocpo)  inz
026600100805     d forzacpo        s                   like(cpocpo)  inz
026700080624     d savcpo          s                   like(cpocpo)  inz
026800080604     d wAbi            s                   like(UTEaut)  inz
026900080609     d Indx            s              3  0 inz
027000080619     d Indx2           s              3  0 inz
027100080609     d xx              s              3  0 inz
027200080929     d yy              s              3  0 inz
027300080415     d ss              s              3  0 inz
027400080415     d ff              s              3  0 inz
027500080526     d contafil        s              3  0
027600080414     d K48A            s             48
027700080526     d Pulizforz       s              1
027800080603     d Pulizpot1       s              1
027900080603     d Pulizpot2       s              1
028000080604     d ricInt          s              1
028100080606     d UpdateSFL       s              1
028200080606     d Primariga       s              1
028300080619     d Wscrivi         s              1
028400081013     d Wscelta         s              1
028500080604     d Wpft            s             12  0
028600080605     d W0060           s              6  0
028700080929     d W007A           s              7
028800080605     d W004a           s              4
028900080929     d Wdes            s             19
029000080929     d WIspp           s              1
029100081009     d WcodCPI         s             21
029200081013     d wspf            s                   like(cpispf)  inz
029300081010     d Wgiorno         s              2  0
029400081010     d WDTunAnno       s              8  0
029500100524     d totnot          s              5  0
029600111116     d codif1          s              1
029700111116     d codif2          s              1
029800120622
029900120622     d DataPrimoConF   s              8  0
030000120622     d DataPrimoConT   s              8  0
030100111115
030200111115     d sav_CPOcpo      s                   like(CPOcpo)
030300111115     d sav_ATCcad      s                   like(ATCcad)
030400111115
030500100525     d NewHIM          s                   like(cpnhim)
030600081010
030700081010     d W1Esinfo        s              1
030800081010     d W2Esinfo        s              1
030900081010     d TieniEsinfo     s              1
031000081010     d FondiEsinfo     s              1
031100081010     d ImpostaT        s              1
031200100525     d TieniconTrCd    s              1
031300100525     d fondiAttB       s              1
031400100525     d fondiAttI       s              1
031500100525     d TieniAtt        s              1
031600090521     d Alfacpo         s             11
031700100419     d Alfaoggi        s              8
031800100525     d W0030           s              3  0
031900100525     d W0080           s              8  0
032000080609     d knk1            s                   like(ntcnk1)  inz
032100080604     d knk2            s                   like(ntcnk2)  inz
032200080604     d ktnt            s                   like(ntctnt)  inz
032300080929     d ktprI           s                   like(cpitpf)  inz
032400080619     d TieniCPO        s                   like(cpocpo)  inz
032500080619     d FondiCPO        s                   like(cpocpo)  inz
032600080619     d FondiRag        s                   like(v031rag) inz
032700080605     d Dataiso         s               d   datfmt(*iso)
032800080605     d Dataymd         s               d   datfmt(*ymd)
032900080605     d Datadmy         s               d   datfmt(*dmy)
033000081010     d DataSYS         s               d   inz(*sys)
033100100524     d OraHMS          s               t   timfmt(*hms)
033200130916     D*
033300081112     d WTFACO          s             21    inz('FILTRAGRU /TFACO16L')
033400130916     d*
033500080415     d                 DS
033600080415     d  Fcpo                          3  0
033700080415     d  Ncpo                          8  0
033800080415     d kcpo                    1     11  0
033900130916      *
034000090521     D TRMK08R         pr                  extpgm('TRMK08R')
034100090521     D  kpjba                              likeds(kpjba)
034200090521     D  ALFACPO                      11
034300080206      //---------------------------------------------------------------
034400080206      //?Definizione procedure usate.
034500080206      //---------------------------------------------------------------
034600080414      /copy gaitrasrc/srcprotopr,tibs02r
034700080414      /copy gaitrasrc/srcprotopr,tibs34r
034800080606      /copy gaitrasrc/srcprotopr,tnta12r
034900080929      /copy gaitrasrc/srcprotopr,trmk50r
035000080617      /copy gaitrasrc/srcprotopr,trul31r
035100100520      /copy gaitrasrc/srcprotopr,trmk01r
035200100520      /copy gaitrasrc/srcprotopr,trmk02r
035300100521      /copy gaitrasrc/srcprotopr,tnta43r
035400110304
035500110304      // - Calcolo categoria potenziale
035600110304     d Trmk05r         pr                  extpgm('TRMK05R')
035700110304     d  kpjba                              likeds(KPJBA)
035800110304     d  trmk05ds                           likeds(TRMK05ds)
035900160901
036000160901       // -?Storicizzazione Variazioni Potenziale
036100160901     d TRMK30R         pr                  extpgm('TRMK30R')
036200160901     d  trmk30ds                           likeds(trmk30ds)
036300160901     d  tncpo_30                           likeds(tncpo_30)
036400160901     d  tncpo1_30                          likeds(tncpo1_30)
036500160901     d  ticpi_30                           likeds(ticpi_30)
036600080206
036700080206      //---------------------------------------------------------------
036800080206      //?Riepilogo indicatori.
036900080206      //---------------------------------------------------------------
037000080207      // - Comuni
037100080207      // 28    : Emissione messaggio di errore a video
037200080207      // - C01/S01
037300080207      // 30    : Condiziona SFLDSP    (*not)
037400080207      // 31    : Condiziona SFLDSPCTL (*not)
037500080207      // 30+31 : Condiziona SFLCLR
037600080207      // 32    : Condiziona SFLNXTCHG in S01
037700080207      // 50    : Errore: Opzione errata
037800080207      // - D01
037900080207      // - Comuni
038000080207      // 99    : Generico di Errore
038100080206      //---------------------------------------------------------------
038200080206
038300080206      //---------------------------------------------------------------
038400080206      //?M A I N - L I N E
038500080206      //---------------------------------------------------------------
038600080206
038700080206     c     *Entry        plist
038800080206     c                   parm                    KPJBA
038900080626     c                   movel     kpjbu         flagintpot        1
039000080626     c* Attivo tasto funzionale di int.potenziali se non vengo dalla int
039100080626     c                   if        flagintpot<>'N'
039200080701     c                   eval      Intpotenz='1'
039300080626     c                   endif
039400080206
039500080206      /free
039600080206
039700080206       // Operazioni iniziali
039800080206       exsr RoutInz;
039900080206
040000080206       // Gestione video
040100080206       DOW $Fine = *off;
040200080206         select;
040300080530
040400080530         // Videata di selezioni
040500080206           when $Video = 'D1';
040600080206             exsr GesD01;
040700080530
040800080530         // SFL di scelta
040900080411           when $Video = 'S2';
041000080411             exsr GesS02;
041100080530
041200080530         // U N I O N E
041300080530           when $Video = 'S1';
041400080530             exsr GesS01;
041500080530
041600080530         // F U S I O N E
041700080603           when $Video = 'S3';
041800080603             exsr GesS03;
041900080206           other;
042000080206             $Fine = *on;
042100080206         endsl;
042200080206       ENDDO;
042300080206
042400080206       // Operazioni finali
042500080206       exsr RoutEnd;
042600080206
042700080206       //--------------------------------------------------------------
042800080206       //?Operazioni iniziali.
042900080206       //--------------------------------------------------------------
043000080206       BEGSR RoutInz;
043100080414       $inzd01=*on;
043200080606       v01cric='F'  ;
043300080206
043400080206         // Impostazione campi "fissi"
043500080208         TBLkut = 1;
043600080208         TBLcod = '1L';
043700080206
043800080206         // Reperimento dati job
043900080206         exsr DatiJob;
044000080206
044100080206         // Verifica errori e autorità profilo
044200080206         clear  wAbi;
044300080206         clear  dLAT;
044400080206         select;
044500080206         // - Se ho errori nei dati utente esco dal pgm
044600080208           when  DUTerr = 'E';
044700080206             $Fine = *on;
044800080206         // - Se non c'è l'abilitazione:
044900080206         //   - se 1° livello, abilitazioni al terminal
045000080206         //   - se 2° livello, abilitazioni al punto operativo
045100080206         //   - se sede è impossibile ma se capita mando a fine il pgm
045200080208           when  UTEaut = *blank;
045300080206             select;
045400080208               when  DUTlpo = '1';
045500080206                 wAbi  = 'TP';
045600080208               when  DUTlpo = '2';
045700080206                 wAbi  = 'PO';
045800080208               when  DUTlpo = 'S';
045900080206                 $Fine = *on;
046000080206             endsl;
046100080206         // - Altrimenti si caricano le abilitazioni del profilo
046200080206           other;
046300080208             dUTE01 = UTEfaf;
046400080208             if  §UTEpot <> *blank;
046500080208               wAbi = §UTEpot;
046600080206             else;
046700080208               wAbi = UTEaut;
046800080206             endif;
046900080206         ENDSL;
047000080206
047100080206         // Controllo se ok l'abilitazione dell'utente
047200080206         reset TIBS02ds;
047300111115         T02Mod = 'C';
047400080208         T02sif = knsif;
047500080208         T02cod = 'LAT';
047600080208         T02ke1 = wAbi;
047700080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
047800080206
047900080208         if  T02err  = *blank;
048000080208           dLAT = T02uni;
048100080206         endif;
048200080206
048300080206         // Errore o utente non abilitato
048400080208         if  T02err <> *blanks   or
048500080208             §LATabi = 'S';
048600080206           $ErrGrave   = *on;
048700080206           errMessage  = *on;
048800080206           errGenerico = *on;
048900080410           V1Dmsg = Msg(01);
049000080206           leavesr;
049100080206         endif;
049200080617
049300080617         clear trul31ds  ;
049400080617         I31abi = wabi    ;
049500080617         I31cdi = DUTdis  ;
049600080617         I31car = DUTare  ;
049700080617         I31cpo = DUTpou  ;
049800080617         TRUL31R   (KPJBA:trul31ds)   ;
049900080617
050000080617         if o31pog=*zeros   ;
050100080617           $ErrGrave   = *on;
050200080617           errMessage  = *on;
050300080617           errGenerico = *on;
050400080617          V1Dmsg = Msg(01);
050500080617           leavesr;
050600080617         endif         ;
050700100520
050800100520          // imposto la data del giorno - 1 anno    ;
050900100520          dataiso=datasys         ;
051000100520          w0080=%dec(dataiso)     ;
051100100520          Alfaoggi=%editc(w0080:'X')  ;
051200100520
051300100520          dataiso=dataiso- %years(1)      ;
051400100520          wgiorno=%subdt(dataiso:*d)  ;
051500100520          wgiorno=wgiorno-1     ;
051600100520          dataiso=dataiso- %days(wgiorno) ;
051700100520          wdtunanno=%dec(dataiso)    ;
051800080929
051900080929       // Carico ds con le INFO ordinate
052000080929          yy= 1    ;
052100080929          setll ('IFO')   TNTBE01l  ;
052200080929          reade ('IFO')   tntbe01l  ;
052300080929
052400080929  1       dow    not %eof(tntbe01l)  ;
052500080929
052600080929  2       if tbeatb=' '                  ;
052700080929          dIFO=tbEuni                ;
052800080929          kifoord(yy) =§ifoifg+%editc(§ifoogi:'X')+%subst(tbeke1:1:3) +
052900081009          (§ifodeb)+§ifoifs   ;
053000080929          yy=yy+1    ;
053100080929          endif     ;
053200080929
053300080929          reade ('IFO')   tntbe01l  ;
053400080929          enddo    ;
053500080929
053600080929          sorta kifoord  ;
053700080929
053800081112          open(e)   TFACO16L              ;
053900081112          if        not %open(tFACO16L)   ;
054000081112           %subst(wtfaco:7:4)='GRPF'      ;
054100081112           OPEN tfaco16l                  ;
054200081112         Endif                            ;
054300081112
054400080206       ENDSR;
054500080206
054600080206       //--------------------------------------------------------------
054700080206       //?Reperimento Dati del job (Utente/Operativi).
054800080206       //--------------------------------------------------------------
054900080206       BEGSR DatiJob;
055000080206
055100080206         in(E) §AzUte;
055200080206         if NOT %error;
055300080206           in(E) §DatiUte;
055400080206         endif;
055500080206         if %error or RSut = *blanks;
055600080206           clear TIBS34ds;
055700080206           tibs34r(tibs34ds);
055800080206           in §AzUte;
055900080206           in §DatiUte;
056000080206         endif;
056100080206
056200080206       ENDSR;
056300080206
056400080206       //--------------------------------------------------------------
056500080206       //?Gestione videata D01
056600080206       //--------------------------------------------------------------
056700080206       BEGSR GesD01;
056800080206
056900080206         // Inizializzazione videata
057000080414
057100080414         // campi da pulire sempre se non c'e' errore
057200080414         if  errGenerico = *off;
057300080414           clear v01dric;
057400080414         endif;
057500080414
057600080414         if  $inzd01 = *on;
057700080411           clear v01dric;
057800080523           v01ptdb='T';
057900111115           clear V01fls;
058000111115           clear V01dfls;
058100080618           v01ela ='T';
058200080411           clear v01fil ;
058300080411           clear v01rag ;
058400080415           clear v01prv ;
058500080414          $Inzd01=*off;
058600080411         endif ;
058700080606
058800080606         // Pulisco sempre
058900080606           clear Sedi   ;
059000080606           clear nrrsed ;
059100080606           clear Filiali;
059200080606           clear nrrfil ;
059300080606           clear Fusione;
059400080606           clear nrrfus ;
059500080606           clear tipofus;
059600111115           clear Catfus ;
059700080206
059800080206         // Emissione testata
059900080411         if  errGenerico = *off;
060000080409           write MK07T01;
060100080411         endif;
060200080206
060300080206         // Emissione videata
060400080409         exfmt MK07D01;
060500080411
060600080206         clear V1Dmsg;
060700080523         clear errFILIALE;
060800111115         clear errFLS;
060900080411         clear errGenerico;
061000080207
061100080206         select;
061200080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
061300080206           when  $ErrGrave = *on;
061400080206             $Fine  = *on;
061500080411
061600080206         // F3=Fine
061700080208           when dsp_aid = c_F03;
061800080409           $Fine = *on;
061900080411
062000080411         // F7=Int.potenziali
062100080411           when dsp_aid = c_F07;
062200080606           EXSR  INTPOTG ;
062300080409            other           ;
062400080411
062500080411             exsr Contrd01 ;
062600080411
062700080414         // Avanzamento se non errori
062800080414
062900080411             if errGenerico=*off;
063000080415
063100080523            //  Visualizzo primo sfl di scelta;
063200080411              $Video = 'S2';
063300080411              $InzS02 = *on;
063400080603
063500080603           // Pulisco qui il posizionamento cursore della 2 videata
063600080603           // perchè in caso di conferma devo riemettere la videata
063700080603           //  in cui si sono effettuare le scelte per unione/fusione
063800080603              clear C02csr;
063900080603              clear pagric;
064000080603             endif  ;
064100080206         endsl;
064200080206
064300080411       // Tipo richiamo
064400080411          if v01cric='F' ;
064500080411          v01dric='F U S I O N E' ;
064600080411          ELSE ;
064700080411          v01dric=' U N I O N E ' ;
064800080411          ENDIF ;
064900080411
065000080206       ENDSR;
065100080606       //--------------------------------------------------------------
065200080606       //?Interrogazione potenziali da prima videata
065300080606       //--------------------------------------------------------------
065400080606       BEGSR    INTPOTG   ;
065500100520
065600100520       clear trmk01ds   ;
065700100520
065800100520           mk01k11='2'   ;
065900100520           kpjbu=trmk01ds;
066000100520           TRMK01R (kpjba) ;
066100100520           trmk01ds=kpjbu  ;
066200100520
066300100520           d11cpo=mk01cpo   ;
066400080606
066500080606           // Selezionato potenziale
066600080606 1         if  d11cpo>0;
066700080606             chain(N)  d11cpo  TNCPO000;
066800080606 2            if %found(tncpo01l) ;
066900080606         // U N I O N E
067000080606 3         if   V01cric='U';
067100080606 4           if cpofsf='S'   ;
067200080606              sedi(1)=cpocpo  ;
067300080606 x4          else    ;
067400080606              filiali(1)=cpocpo  ;
067500080606 4           endif   ;
067600080606           $video='S1'  ;
067700080606           $InzS01 = *on;
067800080606 x3        else   ;
067900080606         // F U S I O N E
068000080606           Fusione(1)=cpocpo  ;
068100080606           tipofus(1)=cpoFSF  ;
068200080606           $video='S3'  ;
068300080606           $InzS03 = *on;
068400080606 3         endif;
068500080606 2         endif;
068600080606 1         endif;
068700080606            ENDSR    ;
068800080411       //--------------------------------------------------------------
068900080411       //?Controllo campi 1 videata
069000080411       //--------------------------------------------------------------
069100080411       BEGSR ContrD01;
069200080415
069300080415         if v01prv<>'  ' ;
069400080415         clear tblkey;
069500080415         tblkey=v01prv;
069600080415
069700080415         chain  (1:'PR':tblkey) tabel00f;
069800080415 2       if  not %found(tabel00f);
069900080415           errPRV=*on;
070000080415           errMessage  = *on;
070100080415           errGenerico = *on;
070200080415           V1Dmsg = Msg(12);
070300080523           leavesr;
070400080415         endif ;
070500080523         endif ;
070600080415
070700111115       //?--> Categoria potenziale <--
070800111116         clear V01dfls;
070900111115       //?Ricerca
071000111115         IF  %scan('?':V01fls) > 0;
071100111115           clear TIBS02ds;
071200111115           T02mod = 'R';
071300111115           T02sif = knsif;
071400111115           T02cod = 'CPO';
071500111115           TNTBE_RicercaControllo (kpjba : TIBS02DS);
071600111115           IF  T02err = *blank;
071700111115             V01fls = T02ke1;
071800111115           ENDIF;
071900111115         ENDIF;
072000080411
072100111115       //?Controllo
072200111115         IF  V01fls <> *blanks;
072300111115           clear dCPO;
072400111115           clear TIBS02ds;
072500111115           T02mod = 'C';
072600111115           T02sif = knsif;
072700111115           T02cod = 'CPO';
072800111116           T02ke1 = V01fls;
072900111115           TNTBE_RicercaControllo (kpjba : TIBS02DS);
073000111115
073100111115           IF  T02err <> *blank;
073200111115             errFLS      = *on;
073300111115             errMessage  = *on;
073400111115             errGenerico = *on;
073500111115             V1Dmsg = msg(07);
073600111115             leavesr;
073700111115           ENDIF;
073800111115           dCPO = T02uni;
073900111115           V01dfls = §CPOdes;
074000111115 1       ENDIF;
074100080415
074200080415         // filiale di appartenenza
074300080414 1       if errGenerico=*off and v01fil>0;
074400080411         chain  v01fil  azorg01l;
074500080414 2       if not %found(azorg01l) ;
074600080523           errFILIALE=*on;
074700080411           errMessage  = *on;
074800080411           errGenerico = *on;
074900080411           V1Dmsg = Msg(8);
075000080414 2       endif;
075100080414 1       endif;
075200080411       ENDSR;
075300080206       //--------------------------------------------------------------
075400080523       //?Gestione videata S01   - UNIONE
075500080206       //--------------------------------------------------------------
075600080409       BEGSR GesS01;
075700080207
075800080207         // Inizializzazione videata
075900080409         if  $InzS01 = *on;
076000080409            exsr InzS01;
076100080409            $InzS01  = *off;
076200080207         endif;
076300080207
076400080207         // Posizionamento cursore
076500080410           C01rcd = 1;
076600080410           sfldsp_n=*off;
076700080207
076800080207         // Emissione Testata e Piede con tasti funzionali abilitati
076900080207         if  errGenerico = *off;
077000080409           write  MK07T01;
077100080409           write  MK07P01;
077200080207         endif;
077300080207
077400080207         // Emissione videata
077500080409         exfmt  MK07C01;
077600080410
077700080207         reset errMessage;
077800080207         reset errGenerico;
077900080207         clear V1Dmsg;
078000080523         clear errSEDE ;
078100080526         clear errFIL  ;
078200080207
078300080523 1       SELECT;
078400080207
078500080207         // - F3=Fine
078600080523 1         WHEN  dsp_aid = c_F03;
078700080409            $Fine = *on;
078800080207
078900080207         // - F12=Ritorno
079000080523 1         WHEN  dsp_aid = c_F12;
079100080606           if nrrsed(1)>0 or  nrrfil(1)>0   ;
079200080523              $Video = 'S2';
079300080606           else  ;
079400080606              $Video = 'D1';
079500080606              $inzd01=*on;
079600080606           endif ;
079700080414
079800080414         // F7=Int.potenziali
079900080523 1         when dsp_aid = c_F07;
080000080603           exsr   INTpotU;
080100080530 2           if  errGenerico = *on;
080200080530           write  MK07T01;
080300080530           write  MK07P01;
080400080530           endif  ;
080500080414
080600080530 x1      // Invio / F6
080700080207           OTHER;
080800080409             exsr ContrS01 ;
080900080523 2           if  errGenerico = *on;
081000080207               leavesr;
081100080523 2           endif;
081200080530
081300080530         // F6=conferma UNIONE
081400080530 1         if   dsp_aid = c_F06;
081500080530           exsr   ConfUNIONE;
081600080530           endif  ;
081700080207
081800080523 1       ENDSL;
081900080207
082000080207       ENDSR;
082100080207
082200080409       //--------------------------------------------------------------
082300080603       //?Interrogazione potenziali per UNIONE
082400080526       //--------------------------------------------------------------
082500080603           Begsr INTPOTU ;
082600080526
082700100520           clear trmk01ds   ;
082800080618           if v01sncpo>0 and v01srag<>*blanks   ;
082900100805           mk01rag=%subst(v01srag:1:8)   ;
083000080618           else     ;
083100080618             if s01fncpo>0 and s01frag<>*blanks   ;
083200100805             mk01rag=%subst(s01frag:1:8)   ;
083300080618             endif    ;
083400080618           endif    ;
083500100520
083600100520             MK01K11='4'  ;
083700100520             kpjbu=trmk01ds;
083800100520             TRMK01R (kpjba) ;
083900100520             trmk01ds=kpjbu  ;
084000100520             D11CPO=Mk01Cpo  ;
084100080526
084200080526           // Selezionato potenziale
084300080526 1         if  d11cpo>0;
084400080530             chain(N)  d11cpo  TNCPO000;
084500080526
084600080526            // Se sede
084700080526 2            if not %found(tncpo01l) or (cpofsf='S' and sedi(1)>0) ;
084800080526               errSEDE =*on;
084900080526               errMessage  = *on;
085000080526               errGenerico = *on;
085100080526               V1Dmsg = Msg(16);
085200080530               %subst(v1dmsg:5:11)=%char(cpocpo) ;
085300080530               %subst(v1dmsg:17:12)=cporag ;
085400080526               leavesr ;
085500080526 2            endif  ;
085600080526
085700080526            // Se sede deve avere partita iva o codice fiscale
085800080526 2            if cpofsf='S' and cpopiv=*blanks and cpocdf=*blanks ;
085900080526               errSEDE =*on;
086000080526               errMessage  = *on;
086100080526               errGenerico = *on;
086200080526               V1Dmsg = Msg(15);
086300080526               leavesr ;
086400080526 2            endif  ;
086500080526
086600080526              // Carico dati sede
086700080526 2            if cpofsf='S' ;
086800080526              kcpo=d11cpo   ;
086900080526              exsr ClearCTL1 ;
087000080526              exsr RiempiCTL1 ;
087100080526              leavesr ;
087200080526 2            endif ;
087300080526
087400080526            // Se Filiale
087500080526 2            if cpofsf='F' and Filiali(7)>0 ;
087600080526               errSEDE =*on;
087700080526               errMessage  = *on;
087800080526               errGenerico = *on;
087900080526               V1Dmsg = Msg(14);
088000080526               leavesr ;
088100080526 2            endif  ;
088200080526
088300080526              // cerco primo record di sfl libero per impostarlo
088400080526              s01nrr=0 ;
088500080526              s01fncpo=99999999 ;
088600080526 2            dow  s01nrr<7 and (s01fncpo<>0 or s01ffcpo<>0) ;
088700080526                s01nrr=s01nrr+1 ;
088800080526                chain  s01nrr mk07s01;
088900080526 2            enddo  ;
089000080526
089100080526 2            if s01fncpo=0 and s01ffcpo=0 ;
089200080526                kcpo=d11cpo;
089300080526                exsr riempiSFL1 ;
089400080530                protpotFIL=*off;
089500080530                update mk07s01  ;
089600080526 2            endif;
089700080526 1          endif;
089800080526            ENDSR;
089900080526       //--------------------------------------------------------------
090000080523       //?controlli videata S01  - UNIONE
090100080409       //--------------------------------------------------------------
090200080409       BEGSR ContrS01;
090300080410
090400080523       // Solo se non protetto
0905000805261      if  ProtpotSED=*off    ;
090600080523
090700080410       // Immettere cod potenziale
0908000805262      if   v01sncpo=0 and v01sfcpo=0  ;
090900080530           exsr clearCTL1  ;
091000080523           errSEDE =*on;
091100080410           errMessage  = *on;
091200080409           errGenerico = *on;
091300080414           V1Dmsg = Msg(03);
0914000805262      endif;
091500080410
091600080523         fcpo=v01Sfcpo      ;
091700080523         ncpo=v01Sncpo      ;
091800080530         chain(N)  Kcpo  TNCPO000;
091900080410
092000080410         // Codice potenziale NON trovato
092100080526 2       if  NOT  %found(TNCPO01L);
092200080523           errSEDE =*on;
092300080410           errMessage  = *on;
092400080410           errGenerico = *on;
092500080414           V1Dmsg = Msg(03);
092600080526 x2    else ;
092700080526           exsr clearCTL1         ;
092800080604           if cpocpo<>savcpo
092900080604              and savcpo>0       ;
093000080526           pulizforz='S'       ;
093100080526           endif               ;
093200080526
093300080604           savcpo =cpocpo      ;
093400080526
093500080526           exsr riempiCTL1         ;
093600080526           clear Pulizforz     ;
093700080526 2     endif ;
093800080523 1     endif ;
093900080410
094000080410 1       if errGenerico=*off;
094100080410
094200080414       // controllo SFL
094300080526       s01nrr=1;
094400080526       clear contafil     ;
094500080526 2     dow s01nrr<=record and errGenerico=*off;
094600080523       clear errFIL   ;
094700080410
094800080526       chain  s01nrr mk07s01;
094900080410
095000080523 3     if s01ffcpo>0 or s01fncpo>0;
095100080523         fcpo=s01ffcpo      ;
095200080523         ncpo=s01fncpo      ;
095300080530         chain(N)  Kcpo  TNCPO000;
095400080410
095500080410         // Codice potenziale NON trovato
095600080410 4       if  NOT  %found(TNCPO01L);
095700080523           errFIL   =*on;
095800080410           errMessage  = *on;
095900080410           errGenerico = *on;
096000080618           V1Dmsg = Msg(27);
096100080526           else ;
096200080526           contafil=contafil+1 ;
096300080410 4     endif;
096400080526
096500080526         // Se non è selezione da sfl del pgm, ricarico i dati
096600080526 4       if filiali(s01nrr)=0 ;
096700080526         ProtpotFIL=*off    ;
096800080526         exsr RiempiSFL1;
096900080526         else    ;
097000080526         ProtpotFIL=*on     ;
097100080526 4       endif          ;
097200080618
097300080618         // controllo se utente abilitato all'aggiornamento
097400080618 4       if Errfil=*off   ;
097500080618           Indx=%lookup(s01ffil:pog)   ;
097600080618 5         if Indx=0    ;
097700080618             errFIL   =*on;
097800080618             errMessage  = *on;
097900080618             errGenerico = *on;
098000080618             V1Dmsg = Msg(26);
098100080618 5         endif        ;
098200080618 4       endif        ;
098300080526
098400080526         // Se la filiale presenta una partita iva o cod fiscale
098500080526         //       diversi --> errore forzabile
098600080526
098700080618 4       if  Errfil=*off and S01forzpiv=' '  ;
098800080526 5       if (s01fpiv<>v01spiv and s01fpiv<>*blanks and v01spiv<>*blanks) or
098900080526            (s01fcdf<>v01scdf and s01fcdf<>*blanks and v01scdf<>*blanks)  ;
099000080526           errFIL   =*on;
099100080526           errMessage  = *on;
099200080526           errGenerico = *on;
099300080526           V1Dmsg = Msg(17);
099400080526           s01forzpiv='S' ;
099500080526 5       endif            ;
099600080526 4       endif            ;
099700080410
099800080526 x3      else  ;
099900080526         exsr ClearSFL1 ;
100000080530         ProtpotFIL=*off    ;
100100080410 3       endif  ;
100200080410
100300080410       update mk07s01;
100400080530       ProtpotFIL=*off    ;
100500080410
100600080526       s01nrr=s01nrr+1;
100700080410 2     enddo;
100800080410
100900080523       // Immettere almeno un figlio
101000080526 2       if errGenerico=*off and contaFil=0;
101100080410          chain  1 mk07s01;
101200080523           errFIL   =*on;
101300080410           errMessage  = *on;
101400080410           errGenerico = *on;
101500080410           V1Dmsg = Msg(06);
101600080410          update mk07s01;
101700080410 2       endif;
101800080410 1     endif;
101900080409
102000080409       ENDSR;
102100080530       //--------------------------------------------------------------
102200080530       //?Conferma unione potenziali
102300080530       //--------------------------------------------------------------
102400080530       BEGSR ConfUNIONE ;
102500080530       // Leggo i potenziali di FILIALE ed aggiorno con PIV e CDF
102600080530       //  della sede
102700080530
102800080530       s01nrr=1;
102900080530 2     dow s01nrr<=record ;
103000080530
103100080530       chain  s01nrr mk07s01;
103200080530
103300080530 3     if s01ffcpo>0 or s01fncpo>0;
103400080530         fcpo=s01ffcpo      ;
103500080530         ncpo=s01fncpo      ;
103600080530         chain  Kcpo  TNCPO000;
103700080530         if %found(tncpo01l)  ;
103800160901         //?Salvo l'immagine Precedente
103900160901           exsr ImmaginePrima;
104000080530         cpopiv=v01spiv;
104100080530         cpocdf=v01scdf;
104200080530         dcpo01=cporst ;
104300080530         §cporicuni='S' ;
104400080530         cporst=dcpo01 ;
104500080530         cpodtr=*date  ;
104600080530         update tncpo000 ;
104700160901         //?Scrivo la variazione fatta sul potenziale
104800160901           exsr ScriviVariazione;
104900080530         endif   ;
105000160901         //?Scrivo una riga per memorizzare il codice potenziale Sede di Unione
105100160901           exsr ScriviSede;
105200080530       endif   ;
105300080530       s01nrr=s01nrr+1;
105400080530
105500080530       enddo   ;
105600080530
105700100520       // Chaino la SEDE per aggiornare flag unione
105800080530
105900080530         fcpo=v01Sfcpo      ;
106000080530         ncpo=v01Sncpo      ;
106100080530         chain  Kcpo  TNCPO000;
106200080530
106300080530         // Codice potenziale NON trovato
106400080530 2       if  %found(TNCPO01L);
106500080530         dcpo01=cporst ;
106600080530         §cporicuni='S' ;
106700080530         cporst=dcpo01 ;
106800080530         update tncpo000 ;
106900080530         endif ;
107000080530
107100080530         // imposto il posizionamento cursore per il sfl che emetto
107200080530         //  aggiornato
107300080606         if nrrSED(1)> 0  or nrrfil(1)>0 ;
107400080606           C02csr=nrrSED(1) ;
107500080606           if c02csr=0 ;
107600080606             C02csr=nrrFIL(1) ;
107700080606           endif     ;
107800080606           pagric=(c02csr/C_sflPag)+1 ;
107900080606           $Video = 'S2';
108000080606           $InzS02 = *on;
108100080606         else           ;
108200080606           $Video = 'D1';
108300080606           $InzD01 = *on;
108400080606         endif    ;
108500080530
108600080530       ENDSR;
108700080409
108800080207       //--------------------------------------------------------------
108900080415       //?Inizializzazione videata S01 per  U N I O N E
109000080207       //--------------------------------------------------------------
109100080409       BEGSR InzS01;
109200080207
109300080207       // Pulizia subfile
109400080207         SflDsp_N    = *on;
109500080207         SflDspCtl_N = *on;
109600080409         write  MK07C01;
109700080207         SflDspCtl_N = *off;
109800080207         SflEnd      = *off;
109900080530
110000080530         clear pagric;
110100080530         clear C01rcd;
110200080409         S01nrr=1 ;
110300080207         clear V1Dmsg;
110400080207         errMessage  = *off;
110500080207         errGenerico = *off;
110600080523         errScelta   = *off;
110700080526         errFil      = *off;
110800080526         errSede     = *off;
110900080409
111000080409       // Carico una pagina vuota
111100080410          clear  MK07S01;
111200080414
111300080414
111400080523          dow   s01nrr<= Record ;
111500080410           write MK07S01;
111600080409           S01nrr += 1;
111700080414          endDO      ;
111800080409
111900080409       // SFL CONTROL
112000080409
112100080523          exsr    ClearCTL1 ;
112200080414
112300080415       // Se passata SEDE imposto
112400080523          if SEDI(1)>0     ;
112500080523            kcpo=sedI(1) ;
112600080530            chain(N) kcpo   tncpo01l;
112700080414            if   %found(tncpo01l) ;
112800080523              exsr RiempiCTL1;
112900080523              eval ProtPOTSED=*on ;
113000080414            endif    ;
113100080414          endif    ;
113200080523
113300080523          FF=1  ;
113400080530          clear errFIL   ;
113500080530          protpotFIL=*on ;
113600080523          dow FILIALI(ff)>0     ;
113700080523            kcpo=filiali(ff) ;
113800080530            chain(N) kcpo   tncpo01l;
113900080523            if   %found(tncpo01l) ;
114000080530              s01nrr=ff  ;
114100080530              chain   s01nrr  mk07s01  ;
114200080523              exsr RiempiSFL1;
114300080530
114400080530              update mk07s01  ;
114500080523            endif    ;
114600080523
114700080523          ff=ff+1  ;
114800080523          enddo    ;
114900080530          protpotFIL=*off;
115000080207
115100080207       ENDSR;
115200080207
115300080410       //--------------------------------------------------------------
115400080523       //?Pulizia dati codice SEDE
115500080410       //--------------------------------------------------------------
115600080523          BEGSR   ClearCTL1  ;
115700080523          clear   v01Sfcpo   ;
115800080523          clear   v01Sncpo   ;
115900080523          protpotSED=*off       ;
116000080523          clear   v01Srag    ;
116100080523          clear   v01Sloc    ;
116200080523          clear   v01Scap    ;
116300080523          clear   v01Sprv    ;
116400080523          clear   v01Sfil    ;
116500080523          clear   v01Spiv    ;
116600080523          clear   v01Scdf    ;
116700080523          clear   v01Sric    ;
116800080523          clear   v01Sdmerc  ;
116900111115
117000111115          clear   v01sfls;
117100111115          clear   v01scad;
117200111115
117300080410          ENDSR;
117400080414       //--------------------------------------------------------------
117500080523       //?carica dati della SEDE
117600080414       //--------------------------------------------------------------
117700080523          BEGSR   RiempiCTL1  ;
117800080414
117900080522       v01Sfcpo=fcpo  ;
118000080522       v01Sncpo=ncpo  ;
118100080522       v01srag=cporag;
118200080522       v01sloc=cpocit;
118300080522       v01scap=cpocap;
118400080523       if cponaz=*blanks  ;
118500080522       v01sprv=cpoprv;
118600080523       else  ;
118700080523       v01sprv=cponaz;
118800080523       endif ;
118900111115
119000111115       //?Categoria potenziale
119100111115         V01sfls = CPOfls;
119200111115
119300111115       //?Prossima attività
119400111115         sav_CPOcpo = CPOcpo;
119500111115         exsr RIC_proATT;
119600111115         V01scad = sav_ATCcad;
119700080522
119800080522         v01sfil=cpoflt;
119900080522         v01spiv=cpopiv;
120000080522         v01scdf=cpocdf;
120100080523         clear tblkey  ;
120200080523         tblkey=%editc(cposct:'X') ;
120300080523         chain  (1:'1L':tblkey) tabel00f;
120400080523 1       if %found(tabel00f);
120500080522         v01sdmerc=tbluni;
120600080522         else ;
120700080522         v01sdmerc=*all'?' ;
120800080523 1       endif  ;
120900080522
121000080414         dcpo01=cporst;
121100080523 1     select   ;
121200080522         when §cporicfus='S' and §cporicuni='S';
121300080522           v01sric='F/U'     ;
121400080522         when §cporicfus='S' ;
121500080618           v01sric=' Fu'     ;
121600080522         when §cporicuni='S' ;
121700080618           v01sric=' Un'     ;
121800080522         other  ;
121900080522           v01sric='   ';
122000080523 1     endsl;
122100080526
122200080526       // Se devo pulire le forzature nel sfl eseguo
122300080526       if Pulizforz='S'  ;
122400080526       s01nrr=1   ;
122500080526         dow s01nrr<=record  ;
122600080526         chain   s01nrr   mk07s01  ;
122700080526         clear s01forzpiv ;
122800080526
122900080526         if filiali(s01nrr)>0 ;
123000080526         ProtpotFil=*on   ;
123100080526         else  ;
123200080526         ProtpotFil=*off  ;
123300080526         endif ;
123400080526         update mk07s01   ;
123500080526
123600080526         s01nrr=s01nrr+1     ;
123700080526         enddo   ;
123800080526
123900080526         ProtpotFil=*off  ;
124000080526       endif     ;
124100080414
124200100520         ENDSR;
124300111115
124400111115       //--------------------------------------------------------------
124500111115       //?Ricerca la prossima attività da eseguire sul potenziale
124600111115       //--------------------------------------------------------------
124700111115         BEGSR  RIC_proATT;
124800111115
124900111115           $EndAtt = *off;
125000111115           clear sav_ATCcad;
125100111115           exec sql
125200111115            declare ATT cursor for select ATCcad from TIATC00F
125300111115            where ATCcpo = :sav_CPOcpo and atcdco = 0
125400111115            order by ATCdad, ATChda;
125500111115           exec sql
125600111115            open ATT;
125700111115           IF sqlcode < 0;
125800111115             $EndAtt = *on;
125900111115           ENDIF;
126000111115           DOW not $EndAtt;
126100111115             exec sql
126200111115             fetch next from ATT into :ATCcad;
126300111115             IF sqlcod = 100 or sqlcod < 0;
126400111115               $EndAtt = *on;
126500111115               leave;
126600111115             ENDIF;
126700111115             sav_ATCcad = ATCcad;
126800111115             leave;
126900111115           ENDDO;
127000111115           exec sql close ATT;
127100111115
127200111115         ENDSR;
127300080526       //--------------------------------------------------------------
127400080526       //?Pulizia riga filiale per unione
127500080526       //--------------------------------------------------------------
127600080526          BEGSR   ClearSFL1  ;
127700080526          clear s01ffcpo  ;
127800080526          clear s01fncpo  ;
127900080526          clear s01frag   ;
128000080526          clear s01floc   ;
128100080526          clear s01fcap   ;
128200080526          clear s01fprv   ;
128300080526          clear s01ffil   ;
128400080526          clear s01fpiv   ;
128500080526          clear s01fcdf   ;
128600080526          clear s01fric   ;
128700080526          clear s01fdmerc ;
128800080526          clear s01forzpiv;
128900111115
129000111115          clear s01fls;
129100111115          clear s01cad;
129200111115
129300080526          ENDSR     ;
129400080523       //--------------------------------------------------------------
129500080523       //?carica dati delle FILIALI
129600080523       //--------------------------------------------------------------
129700080523          BEGSR   RiempiSFL1  ;
129800080523
129900080523       s01ffcpo=fcpo  ;
130000080523       s01fncpo=ncpo  ;
130100080523       s01frag=cporag;
130200080523       s01floc=cpocit;
130300080523       s01fcap=cpocap;
130400080523       if cponaz=*blanks  ;
130500080523       s01fprv=cpoprv;
130600080523       else  ;
130700080523       s01fprv=cponaz;
130800080523       endif ;
130900080523
131000111115
131100111115       //?Categoria potenziale
131200111115         S01fls = CPOfls;
131300111115
131400111115       //?Prossima attività
131500111115         sav_CPOcpo = CPOcpo;
131600111115         exsr RIC_proATT;
131700111115         S01cad = sav_ATCcad;
131800080523
131900080523         s01ffil=cpoflt;
132000080523         s01fpiv=cpopiv;
132100080523         s01fcdf=cpocdf;
132200080523         clear tblkey  ;
132300080523         tblkey=%editc(cposct:'X') ;
132400080523         chain  (1:'1L':tblkey) tabel00f;
132500080523 6       if %found(tabel00f);
132600080523         s01fdmerc=tbluni;
132700080523         else ;
132800080523         s01fdmerc=*all'?' ;
132900080523         endif  ;
133000080523
133100080523         dcpo01=cporst;
133200080523       select   ;
133300080523         when §cporicfus='S' and §cporicuni='S';
133400080523           s01fric='F/U'     ;
133500080523         when §cporicfus='S' ;
133600080618           s01fric=' Fu'     ;
133700080523         when §cporicuni='S' ;
133800080618           s01fric=' Un'     ;
133900080523         other  ;
134000080523           s01fric='   ';
134100080523       endsl;
134200080523
134300080523          ENDSR;
134400080411       //--------------------------------------------------------------
134500080523       //?Gestione videata S02   - SCelta potenziali
134600080411       //--------------------------------------------------------------
134700080411       BEGSR GesS02;
134800080411
134900080411         // Inizializzazione videata
135000080411         if  $InzS02 = *on;
135100080411            exsr InzS02;
135200080411             $InzS02  = *off;
135300080411         endif;
135400080411
135500080411         // Posizionamento cursore
135600080411         if  C02csr  > *zero;
135700080411           C02rcd = C02csr;
135800080411         else;
135900080411
136000080411         // Se non è stato caricato nulla si riemette la videata D01
136100080411           C02rcd = 1;
136200080411           $Video = 'D1';
136300080411           errMessage  = *on;
136400080411           errGenerico = *on;
136500080411           errRAG      = *on;
136600080414           V1Dmsg = Msg(09);
136700080411           leavesr;
136800080411         endif;
136900080411
137000080411
137100080411         // Posizionamento cursore
137200080411           sfldsp_n=*off;
137300080411
137400080411         // Emissione Testata e Piede con tasti funzionali abilitati
137500080604         if  errGenerico = *off or ricInt ='S'  ;
137600080414           write  MK07T01;
137700080411           write  MK07P02;
137800080604           clear  ricInt   ;
137900080411         endif;
138000080411
138100080411         // Emissione videata
138200080411         exfmt  MK07C02;
138300080411
138400080411         reset errMessage;
138500080411         reset errGenerico;
138600080411         clear V1Dmsg;
138700080523         clear errScelta ;
138800080411
138900080411         SELECT;
139000080411
139100080411         // - F3=Fine
139200080411           WHEN  dsp_aid = c_F03;
139300080411            $Fine = *on;
139400080411
139500080411         // - F12=Ritorno
139600080411           WHEN  dsp_aid = c_F12;
139700080411           $Video = 'D1';
139800080411
139900080414         // - Roll-Up
140000080414           WHEN  dsp_aid = c_RollUp;
140100080414           // - Verifica se raggiunto il limite di records registrabili
140200080414           //   nel subfile (9999)
140300080414             if  S02nrr = *hival;
140400080414               errMessage  = *on;
140500080414               errGenerico = *on;
140600080414               V1Dmsg = Msg(10);
140700080414             else;
140800080414               exsr RollUpS02;
140900080414             endif;
141000080414
141100080411         // Invio
141200080411           OTHER;
141300080523             exsr ContrS02  ;
141400080411             if  errGenerico = *on;
141500080411               leavesr;
141600080411             endif;
141700080411
141800080411         ENDSL;
141900080411
142000080411       ENDSR;
142100080411       //--------------------------------------------------------------
142200080414       //?Inizializzazione sfl 2
142300080411       //--------------------------------------------------------------
142400080411       BEGSR InzS02;
142500080411
142600080411       // Pulizia subfile
142700080411         SflDsp_N    = *on;
142800080411         SflDspCtl_N = *on;
142900080411         write  MK07C02;
143000080411         SflDspCtl_N = *off;
143100080411         SflEnd      = *off;
143200080411
143300080530         clear W_SflPag;
143400080414         $EoF = *off;
143500080411         clear C02rcd;
143600080414         clear S02nrr;
143700080411         clear V1Dmsg;
143800080411         errMessage  = *off;
143900080411         errGenerico = *off;
144000080415         errscelta=*off  ;
144100080604         clear savcpo    ;
144200080411
144300080411       // Carico pagina
144400080414          K48A=V01RAG;
144500080414          setll  K48A    TNCPO02l;
144600080411          exsr sr_ReadRec;
144700080411
144800080530       //  se vengo da videata di selezioni carico una pagina
144900080530       //    altrimenti il numero delle pagine dove è presente il record
145000080530       //    di sede
145100080530         if c02csr=0           ;
145200080530         totrig=C_SflPAg       ;
145300080530         else                  ;
145400080530         totrig=c_SflPag*pagric  ;
145500080530         endif                 ;
145600080530
145700080411         dow  $EoF   = *off     and
145800080530              S02nrr < totrig  ;
145900080411           exsr RollUpS02;
146000080411         enddo;
146100080411
146200080411       // -> forza l'impaginazione sul 1° rec. del subfile
146300080530       //    se vengo dalla videata di selezioni
146400080530
146500080411         if S02nrr > *zero;
146600080530           if c02csr=0 ;
146700080411           C02rcd  = 1;
146800080411           C02csr  = 1;
146900080530           else       ;
147000110831
147100110831           // Se c02csr > del numero di record, imposto il num max
147200110831             if c02csr>s02nrr ;
147300110831              c02csr=s02nrr ;
147400110831             endif  ;
147500080530           C02rcd  = c02csr;
147600080530           endif      ;
147700080530
147800080411         else;
147900080411           clear C02rcd;
148000110831           clear C02csr;
148100080411         endif;
148200080411
148300080411       ENDSR;
148400080411       //--------------------------------------------------------------
148500080411       //?Lettura x caricamento SFL
148600080411       //--------------------------------------------------------------
148700080411       BEGSR sr_ReadRec;
148800080411
148900080411         $RecOK  = *off;
149000080411
149100080411         DOW  $EoF   = *off   and
149200080411              $RecOK = *off;
149300080411
149400080411           // (se non rilevato stop record elaborabili in selezione)
149500080411             if  $EoF = *off;
149600080411               read  TNCPO002;
149700080411               $EoF  = (%eof(TNCPO02L));
149800080411             endif;
149900080411
150000080411           // Selezione record in base alle parzializzazioni
150100080411           if  $EoF  = *off;
150200080411             exsr sr_SelRec;
150300080411           endif;
150400080411
150500080411         ENDDO;
150600080411
150700080411       ENDSR;
150800080411       //--------------------------------------------------------------
150900080411       //?Selezione record in base alle parzializzazioni
151000080411       //--------------------------------------------------------------
151100080411       BEGSR sr_SelRec;
151200080411
151300080411         $RecOK  = *off;
151400080411
151500080411         // Parzializzazione per ragione sociale
151600080411         if  V01rag <> *blank;
151700080411           xx    = %checkr(' ' : V01rag);
151800080610           if   %subst(V01rag : 1 : xx) <> %subst(r_CPOrag : 1 : xx);
151900080411               $EoF = *on;
152000080411             leavesr;
152100080411           endif;
152200080411         endif;
152300080411
152400080411         select;
152500080415         // Parz. per Provincia
152600080610         when  v01prv<>'  ' and v01prv<>r_cpoprv;
152700080415             leavesr;
152800080411
152900080411           // Parzializzazione per filiale di appartenenza
153000080414           when  V01fil <> *zero    and
153100080610                 V01fil <> r_cpOflt;
153200080411             leavesr;
153300080411
153400080415           // parz. per potenziale FILIALE
153500080610           when  V01ptDB ='F' and  r_cpofsf='S'   ;
153600080411             leavesr;
153700080415           // parz. per Potenziale SEDE
153800080610           when  V01ptDB ='S' and r_cpofsf='F'   ;
153900080411             leavesr;
154000080411           ENDSL;
154100080411
154200111115       //?Parzializzazione per categoria potenziale
154300111115         IF  V01fls <> *blanks and V01fls <> r_CPOfls;
154400111115           leavesr;
154500111115         ENDIF;
154600080603
154700081009         // parz per Potenziali normalizzati
154800080603         if  V01ela <>'T' ;
154900080610         dcpo01=r_cporst ;
155000080603
155100080603         if   v01cric='U'  ;
155200080603           if  V01ela = 'S' and §cporicuni='S';
155300080603             leavesr;
155400080603           endif;
155500081009           if  V01ela = 'N' and §cporicuni=' ';
155600080603             leavesr;
155700080603           endif;
155800080603         endif;
155900080415
156000080603         if   v01cric='F'  ;
156100080603           if  V01ela = 'S' and §cporicfus='S';
156200080603             leavesr;
156300080603           endif;
156400081009           if  V01ela = 'N' and §cporicfus=' ';
156500080603             leavesr;
156600080603           endif;
156700080603         endif;
156800080411         endif;
156900080411
157000080411         // => record OK!
157100080411         $RecOK  = *on;
157200080411
157300080411       ENDSR;
157400080411       //--------------------------------------------------------------
157500080411       //?Caricamento singola pagina S02
157600080411       //--------------------------------------------------------------
157700080411       BEGSR RollUpS02;
157800080411
157900080411         S02nrr    = (W_SflPag * C_SflPag);
158000080411         W_SflPag += 1;
158100080411         SflNxtChg = *off;
158200080411
158300080411         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
158400080523 1       DOW  $EoF   = *off                   and
158500080411              S02nrr < (W_SflPag * C_SflPag)  and
158600080411              S02nrr < *hival;
158700080411
158800080411         // - Caricamento dati nel record del subfile
158900080414         clear s02sce  ;
159000080610         s02cpo=r_cpocpo;
159100080610         s02rag=r_cporag;
159200080610 2       if r_cpofsf='S' ;
159300080523          s02deb='Sd';
159400080411         ELSE         ;
159500080523          s02deb='Fi';
159600080523 2       endif;
159700080415
159800080610         s02loc=r_cpocit;
159900080610         %subst(s02loc:21:3)=' '+r_cpoprv;
160000080610         s02fil=r_cpoflt;
160100080610         s02piv=r_cpopiv;
160200080610         s02cdf=r_cpocdf;
160300080610         dcpo01=r_cporst;
160400111115
160500111115       //?Categoria potenziale
160600111115         S02fls = r_CPOfls;
160700111115
160800111115       //?Prossima attività
160900111115         sav_CPOcpo = r_CPOcpo;
161000111115         exsr RIC_proATT;
161100111115         S02cad = sav_ATCcad;
161200111115
161300080523 2     select   ;
161400080522         when §cporicfus='S' and §cporicuni='S';
161500080522           s02ric='F/U'     ;
161600080522         when §cporicfus='S' ;
161700080618           s02ric=' Fu'     ;
161800080522         when §cporicuni='S' ;
161900080618           s02ric=' Un'     ;
162000080522         other  ;
162100080522           s02ric='   ';
162200080523 2     endSL;
162300080415
162400080411
162500080411           S02nrr += 1;
162600080411           write MK07S02;
162700080411
162800080411         // - Lettura record successivo nell'archivio
162900080411           exsr sr_ReadRec;
163000080411
163100080523 1       ENDDO;
163200080411
163300080411         // Attivazione (eventuale) del SFLEND
163400080411         SflEnd   = ($EoF = *on);
163500080411
163600080411         // Posizionamento cursore al 1° rcd della pagina
163700080523 1       if  S02nrr > *zero;
163800080414           wPag   = S02nrr / C_SflPag;
163900080414           wRig   = S02nrr - (C_SflPag * wPag);
164000080411           C02rcd = wPag * C_SflPag;
164100080523 2         if  wRig > *zeros;
164200080415             C02rcd = C02rcd + 1;
164300080411           else;
164400080411             C02rcd = C02rcd - C_SflPag + 1;
164500080523 2         endif;
164600080523 x1      else;
164700080411           clear  C02rcd;
164800080523 1       endif;
164900080411
165000080411         C02csr = C02rcd;
165100080411
165200080411       ENDSR;
165300080414       //--------------------------------------------------------------
165400080414       //?controllo scelta SFL 2
165500080414       //--------------------------------------------------------------
165600080523       BEGSR contrS02 ;
165700080414
165800080523       clear ErrScelta ;
165900080523       clear nrrSED  ;
166000080415       clear SEDI ;
166100080415       clear SS   ;
166200080603
166300080522       clear nrrFIL    ;
166400080415       clear filiali;
166500080415       clear FF     ;
166600080603
166700080603       clear Fusione;
166800080603       clear nrrFus    ;
166900111115       clear CatFus  ;
167000080603       clear TipoFus  ;
167100080604       clear RicINt   ;
167200080625       clear savcpo    ;
167300080415
167400080414       readc mk07s02;
167500080526 1     DOW  NOT  %eof(TRMK07D);
167600080414
167700080415       // 5 - Visualizza
167800080526 2     if s02sce='5' ;
167900100520         clear trmk01ds;
168000100520          Mk01K10='N'   ;
168100100805          mk01cpo=s02cpo  ;
168200100520         trmk02r ( kpjba : trmk01ds );
168300080415         clear s02sce;
168400080523         c02csr=s02nrr  ;
168500080604         ricInt ='S'     ;
168600080617         errGenerico = *on;
168700080526 2     endif ;
168800080414
168900080603       // U N I O N E
169000080603 1a    if   V01cric='U';
169100080603
169200080603       // Scelta SEDE
169300080523
169400080526 2     if   s02sce='1' ;
169500080523
169600080523       // impossibile  unire una sede senza partita iva e cod fiscale
169700080526 3     if s02deb='Sd'    ;
169800080526 4     if s02piv=*blanks and s02cdf=*blanks ;
169900080523         errscelta=*on   ;
170000080523         SflNxtChg = *on ;
170100080523         errMessage  = *on;
170200080523         errGenerico = *on;
170300080523         C02csr = s02nrr    ;
170400080523         V1Dmsg = Msg(15);
170500080523         update mk07s02;
170600080523
170700080523         errscelta=*off  ;
170800080523         SflNxtChg = *off ;
170900080523         leavesr;
171000080526 4     endif  ;
171100080523
171200080415       ss=ss+1 ;
171300080526 4       if  ss<=2;
171400080522         sedi(ss)=s02cpo;
171500080523         nrrsed(ss)=s02nrr;
171600080526 4       endif;
171700080526 3     endif;
171800080415
171900080526 3     if s02deb='Fi'    ;
172000080618       // Errore se non abilitato al potenziale m
172100080618       Indx=%lookup(s02fil:pog)  ;
172200080618 4     if Indx=0;
172300080618         errscelta=*on   ;
172400080618         SflNxtChg = *on ;
172500080618         errMessage  = *on;
172600080618         errGenerico = *on;
172700080618         C02csr = s02nrr    ;
172800080618         V1Dmsg = Msg(26);
172900080618         update mk07s02;
173000080618
173100080618         errscelta=*off  ;
173200080618         SflNxtChg = *off ;
173300080618         leavesr;
173400080618 4      endif   ;
173500080618
173600080526       ff=ff+1 ;
173700080526 4       if  ff<=8;
173800080415         filiali(ff)=s02cpo;
173900080523         nrrfil(ff) =s02nrr;
174000080526 4       endif;
174100080526 3     endif;
174200080526 2     endif;
174300080603 1a    endif;
174400080603
174500080603       // F U S I O N E
174600080603 1a    if   V01cric='F';
174700080603 2     if   s02sce='1' ;
174800100520
174900100419
175000100419        ss=ss+1 ;
175100080603 3       if  ss<=3;
175200080603         Fusione(ss)=s02cpo;
175300080603         nrrfus(ss)=s02nrr;
175400080603         tipofus(ss)=s02deb;
175500111115       //?Categoria potenziale
175600111115         CatFus(ss)= S02fls;
175700100520
175800080603 3       endif;
175900080603 2     endif;
176000080603 1a    endif;
176100080603
176200080415
176300080526 2     if s02sce<>' ';
176400080415         SflNxtChg = *on ;
176500080415         else ;
176600080415         SflNxtChg = *off;
176700080526 2     endif;
176800080415
176900080414       update mk07s02;
177000080414
177100080414       readc mk07s02;
177200080526 1     enddo;
177300080415
177400080603       // U N I O N E
177500080603 1a    if   V01cric='U';
177600080603
177700080415       // Se più di una sede --> errore
177800080526 1     if sedi(2)>0   ;
177900080522         chain  nrrsed(2) mk07s02;
178000080603         V1Dmsg = Msg(13);
178100080603         C02csr = nrrsed(2) ;
178200080603         exsr ERRSFL2  ;
178300080523         leavesr;
178400080526 1     endif ;
178500080522
178600080526 1     if filiali(8)>0   ;
178700080522         chain  nrrfil(8)  mk07s02;
178800080523         C02csr = nrrfil(ff);
178900080415         V1Dmsg = Msg(14);
179000080603         exsr ERRSFL2  ;
179100080523         leavesr;
179200080526 1     endif ;
179300080523
179400080610       // Se non selezionato nè una sede nè una filiale errore bloccante
179500080617 1     if sedi(1)=0 and filiali(1)=0 and ricint=' ';
179600080610         chain  c02csr mk07s02;
179700080523         V1Dmsg = Msg(02);
179800080603         exsr ERRSFL2  ;
179900080523         leavesr;
180000080526 1     endif ;
180100080603 1a    endif ;
180200080603
180300080603       // F U S I O N E
180400080603 1a    if   V01cric='F';
180500080603
180600080603       // Se più di due potenziali da fondere --> errore
180700080603 1     if Fusione(3)>0   ;
180800080603         chain  nrrFus(3) mk07s02;
180900080603         C02csr = nrrFus(3) ;
181000080603         V1Dmsg = Msg(18);
181100080603         exsr ERRSFL2  ;
181200080603         leavesr;
181300080603 1     endif ;
181400080603
181500111115       //?Impossibile fondere 2 potenziali in cat. Eliminabile
181600111115 1     IF  CatFus(1) = 'E' and Catfus(2) = 'E' ;
181700080603         chain  nrrFus(2) mk07s02;
181800080603         C02csr = nrrFus(2) ;
181900080603         V1Dmsg = Msg(05);
182000080603         exsr ERRSFL2  ;
182100080603         leavesr;
182200080603 1     endif   ;
182300080603
182400080603       // Impossibile fondere 2 sedi
182500080603 1     if tipoFus(1)='S' and tipofus(2)='S' ;
182600080603         chain  nrrFus(2) mk07s02;
182700080603         C02csr = nrrFus(2) ;
182800080603         V1Dmsg = Msg(04);
182900080603         exsr ERRSFL2  ;
183000080603         leavesr;
183100080603 1     endif   ;
183200080603
183300080617       // Se non selezionato nessun codice --> errore
183400080617 1     if fusione(1)=0 and ricint=' ';
183500080610         chain  c02csr   mk07s02;
183600080603         V1Dmsg = Msg(02);
183700080603         exsr ERRSFL2  ;
183800080603         leavesr;
183900080603 1     endif   ;
184000080603 1a    endif   ;
184100080603
184200080617       // Se non devo riemettere la videata
184300080617 0     if ErrGenerico=*off   ;
184400080617
184500080526 1     if v01cric='U';
184600080415         $video='S1'  ;
184700080415         $InzS01 = *on;
184800080526 1     endif;
184900080603 1     if v01cric='F';
185000080603         $video='S3'  ;
185100080603         $InzS03 = *on;
185200080603 1     endif;
185300080617 0     endif;
185400080523
185500080414       ENDSR;
185600080603       //--------------------------------------------------------------
185700080603       //?Aggiornamento sfl per errore
185800080603       //--------------------------------------------------------------
185900080603       BEGSR ErrSFL2 ;
186000080603         errscelta=*on   ;
186100080603         SflNxtChg = *on ;
186200080603         errMessage  = *on;
186300080603         errGenerico = *on;
186400080603         update mk07s02;
186500080603
186600080603         errscelta=*off  ;
186700080603         SflNxtChg = *off ;
186800080603       ENDSR;
186900080411
187000080603       //--------------------------------------------------------------
187100080603       //?Gestione videata S03   - FUSIONE
187200080603       //--------------------------------------------------------------
187300080603       BEGSR GesS03;
187400080603
187500080603         // Inizializzazione videata
187600080603         if  $InzS03 = *on;
187700080624            clear forzacpo;
187800080603            exsr InzS03;
187900080603            $InzS03  = *off;
188000080609
188100080609         // Posizionamento cursore
188200080609           C03rcd = 1;
188300080609           C03csr = 1;
188400080603         endif;
188500080603
188600080603         // Emissione Testata e Piede con tasti funzionali abilitati
188700080603         if  errGenerico = *off;
188800080603           write  MK07T01;
188900080603           write  MK07P03;
189000080609          if c03csr>0 ;
189100080609          c03rcd=c03csr   ;
189200080609          endif;
189300080609         endif;
189400080603
189500080603         // Emissione videata
189600080603         exfmt  MK07C03;
189700080603
189800080603         reset errMessage;
189900080603         reset errGenerico;
190000080603         clear V1Dmsg;
190100080603         clear errFusione  ;
190200080618         clear errTipofus1 ;
190300080618         clear errTipofus2 ;
190400080603
190500080603 1       SELECT;
190600080603
190700080603         // - F3=Fine
190800080603 1         WHEN  dsp_aid = c_F03;
190900080603            $Fine = *on;
191000080603
191100080603         // - F12=Ritorno
191200080603 1         WHEN  dsp_aid = c_F12;
191300080617           // da sflcontrol vado a videata precednete
191400080617 1        if SflDSp_N=*on   ;
191500080617
191600080617             if nrrfus(1)>0 ;
191700080617                $Video = 'S2';
191800080617             else  ;
191900080617                $Video = 'D1';
192000080617                $inzd01=*on;
192100080617             endif ;
192200080617
192300080617          else  ;
192400080617           // da sfl vado a sflcontrol
192500080617 1        SflDSp_N=*on   ;
192600080617          $InzS03  = *on ;
192700080620          unlock tncpo01l ;
192800080620          unlock tncpo00F ;
192900080617          endif ;
193000080603
193100080603         // F7=Int.potenziali
193200080603 1         when dsp_aid = c_F07;
193300080604             exsr IntPotF   ;
193400080603 2           if  errGenerico = *on;
193500080603           write  MK07T01;
193600080603           write  MK07P03;
193700080603           endif  ;
193800080603
193900080603 x1      // Invio / F6
194000080603           OTHER;
194100080603             exsr ContrS03 ;
194200080603 2           if  errGenerico = *on;
194300080625 1            if SflDSp_N=*on   ;
194400080625               unlock tncpo00f ;
194500080625               unlock tncpo01l ;
194600080625              endif            ;
194700080603               leavesr;
194800080603 2           endif;
194900080603
195000080603         // F6=conferma FUSIONE
195100080603 1         if   dsp_aid = c_F06;
195200080619           exsr   ConfFUSIONE;
195300080603           endif  ;
195400080603
195500080603 1       ENDSL;
195600080603
195700080603       ENDSR;
195800080603       //--------------------------------------------------------------
195900080603       //?Inizializzazione videata S03 per  F U S I O N E
196000080603       //--------------------------------------------------------------
196100080603       BEGSR InzS03;
196200080603
196300080603       // Pulizia subfile
196400080603         SflDsp_N    = *on;
196500080603         SflDspCtl_N = *on;
196600080603         write  MK07C03;
196700080603         SflDspCtl_N = *off;
196800080603         SflEnd      = *off;
196900080603
197000080603         clear pagric;
197100080603         clear C03rcd;
197200080603         S03nrr=1 ;
197300080603         clear V1Dmsg;
197400080603         errMessage  = *off;
197500080603         errGenerico = *off;
197600080603         errScelta   = *off;
197700080603
197800080603       // SFL CONTROL
197900080603
198000080603          pulizpot1='S'  ;
198100080603          pulizpot2='S'  ;
198200080604          clear   v032tipf   ;
198300080604          clear   v032dtip    ;
198400080610          clear   v031tipf   ;
198500080610          clear   v031dtip    ;
198600080606          ColorRosso1=*off          ;
198700080606          ColorRosso2=*off          ;
198800080603          exsr    ClearCTL3 ;
198900080603
199000080603          SS=1  ;
199100080603          dow Fusione(ss)>0     ;
199200080603            kcpo=Fusione(ss) ;
199300080603            chain(N) kcpo   tncpo01l;
199400080603            if   %found(tncpo01l) ;
199500080603              s03nrr=ss  ;
199600080603              if ss=1    ;
199700080603              pulizpot1='C'  ;
199800080603              else       ;
199900080603              pulizpot2='C'  ;
200000080603              protpotdue=*on   ;
200100100525              endif      ;
200200080603              exsr RiempiCTL3;
200300080603            endif    ;
200400080603
200500080603          ss=ss+1  ;
200600080603          enddo    ;
200700080610          // Se ho selezionato solo un codice, mi posiziono sul secondo
200800080610          if fusione(2)=0  ;
200900080610          Errfusione=*on  ;
201000080610          endif   ;
201100080604
201200080603         SflDsp_N    = *on ;
201300080610         // Se SAVCPO impostato, vengo da manutenzione , per cui
201400080610         //  ricarico anche secondo potenziale
201500080610         If Fusione(2)=0 and  savcpo>0 ;
201600080625            kcpo=savcpo      ;
201700080625            chain(N) kcpo   tncpo01l;
201800080625            if   %found(tncpo01l) ;
201900080610            pulizpot2='C'  ;
202000080610            exsr RiempiCTL3;
202100080610            endif   ;
202200080625         endif   ;
202300100525
202400080603       ENDSR;
202500080603       //--------------------------------------------------------------
202600080603       //?Pulizia dati potenziali per fusione
202700080603       //--------------------------------------------------------------
202800080603          BEGSR   ClearCTL3  ;
202900080603          if pulizpot1='S'  ;
203000080603          clear   v031fcpo   ;
203100080603          clear   v031ncpo   ;
203200080603          clear   v031tipf   ;
203300080603          clear   v031dtip    ;
203400080603          clear   v031rag    ;
203500080603          clear   v031loc    ;
203600080603          clear   v031cap    ;
203700080603          clear   v031ind    ;
203800080603          clear   v031fil    ;
203900080603          clear   v031piv    ;
204000080603          clear   v031cdf    ;
204100080603          clear   v031cmerc  ;
204200080603          clear   v031dmerc  ;
204300080904          clear   c031cmuti  ;
204400080603          clear   v031cmm    ;
204500080603          clear   v031dcmm   ;
204600080603          clear   pulizpot1  ;
204700080603          clear   v031fsf    ;
204800111115          clear v031fls;
204900111115          clear v031cad;
205000111116          clear   codif1     ;
205100111116          RIcodif1=*off      ;
205200111115          $Contattato1 = *off;
205300080603          endif  ;
205400080603
205500080604          if pulizpot2='S'   ;
205600080603          clear   v032fcpo   ;
205700080603          clear   v032ncpo   ;
205800080604
205900080603          protpotdue=*off       ;
206000080603          clear   v032rag    ;
206100080603          clear   v032loc    ;
206200080603          clear   v032cap    ;
206300080603          clear   v032ind    ;
206400080603          clear   v032fil    ;
206500080603          clear   v032piv    ;
206600080603          clear   v032cdf    ;
206700080603          clear   v032cmerc  ;
206800080603          clear   v032dmerc  ;
206900080904          clear   c032cmuti  ;
207000080603          clear   v032cmm    ;
207100080603          clear   v032dcmm   ;
207200080603          clear   pulizpot2  ;
207300080603          clear   v032fsf    ;
207400111115          clear v032fls;
207500111115          clear v032cad;
207600111116          clear   codif2     ;
207700111116          RIcodif2=*off      ;
207800111115          $Contattato2 = *off;
207900080603          endif  ;
208000080603          ENDSR;
208100080603
208200080603       //--------------------------------------------------------------
208300080603       //?carica dati potenziale UNO del SFL3 - Fusione
208400080603       //--------------------------------------------------------------
208500080603          BEGSR   RiempiCTL3  ;
208600100525
208700100525        setll    kcpo   cnaco16l;
208800080603
208900080603       if pulizpot1='C' ;
209000111116
209100111116         if %equal(cnaco16l)  ;
209200111116           Codif1='S'  ;
209300111116           RICodif1=*on  ;
209400111116         else       ;
209500111116           Codif1='N'  ;
209600111116         endif       ;
209700100525
209800080603       v031fcpo=fcpo  ;
209900080603       v031ncpo=ncpo  ;
210000080603       v031rag=cporag;
210100080603       v031ind=cpovia;
210200080603       v031loc=cpocit;
210300080603       v031cap=cpocap;
210400080603       if cponaz=*blanks  ;
210500080603       %subst(v031loc:27:3)=' '+cpoprv;
210600080603       else  ;
210700080603       %subst(v031loc:26:4)=' '+cponaz;
210800080603       endif ;
210900080603
211000111115       //?Categoria potenziale
211100111116        clear V031fls;
211200111116        clear trmk05ds;
211300111116        IMK05cpo = CPOcpo;
211400111116        trmk05r (kpjba : TRMK05DS);
211500111116        IF  OMK05err = *blanks;
211600111116          V031fls = OMK05cat;
211700111116        ENDIF;
211800111115
211900111115       //?Prossima attività
212000111115         sav_CPOcpo = CPOcpo;
212100111115         exsr RIC_proATT;
212200111115         V031cad = sav_ATCcad;
212300080604
212400111115       //?Viste le nuove modifiche imposto 'F' se cat. eliminabile
212500111115         IF  V031fls = 'E';
212600080604           v031tipf='F'     ;
212700080610           v031dtip='FONDERE'  ;
212800111115         ENDIF;
212900111115
213000111115       //?Controllo se potenziale mai contattato
213100120914         dCPO01 = CPOrst;
213200120914         IF  §CPOdtpcon <> *blanks and §CPOdtpcon <> *zeros;
213300111115           $Contattato1 = *on;
213400111115         ENDIF;
213500080603
213600080603         v031fil=cpoflt;
213700080603         v031piv=cpopiv;
213800080603         v031cdf=cpocdf;
213900080603 2       if cpofsf='S' ;
214000080603          v031fsf='Sd';
214100080603         ELSE         ;
214200080603          v031fsf='Fi';
214300080603 2       endif;
214400080603
214500080603         v031cmerc=cposct;
214600080603         clear tblkey  ;
214700080603         tblkey=%editc(cposct:'X') ;
214800080603         chain  (1:'1L':tblkey) tabel00f;
214900080603 1       if %found(tabel00f);
215000080603         v031dmerc=tbluni;
215100080904         ds1l=     tbluni;
215200080904         c031cmuti=§1luti ;
215300080603         else ;
215400080603         v031dmerc=*all'?' ;
215500080904         c031cmuti='N'    ;
215600080603 1       endif  ;
215700080603
215800080603         v031cmm=cpocmm;
215900080604         if cpocmm> 0  ;
216000130916           chain  (CPOcmm)  AZCMM000;
216100130916 1         if %found(AZCMM01L);
216200130916             v031dcmm=CMMdes;
216300080604           else ;
216400130916             v031dcmm=*all'?' ;
216500080604 1         endif  ;
216600080604 1       endif  ;
216700080603
216800080603          clear   pulizpot1  ;
216900080603       endif     ;
217000080603
217100080603
217200080603       if pulizpot2='C' ;
217300111116
217400111116         if %equal(cnaco16l)  ;
217500111116           Codif2='S'  ;
217600111116           RICodif2=*on  ;
217700111116         else       ;
217800111116           Codif2='N'  ;
217900111116         endif       ;
218000100525
218100080603       v032fcpo=fcpo  ;
218200080603       v032ncpo=ncpo  ;
218300080603       v032rag=cporag;
218400080603       v032ind=cpovia;
218500080603       v032loc=cpocit;
218600080603       v032cap=cpocap;
218700080603       if cponaz=*blanks  ;
218800080603       %subst(v032loc:27:3)=' '+cpoprv;
218900080603       else  ;
219000080603       %subst(v032loc:26:4)=' '+cponaz;
219100080603       endif ;
219200111115
219300111115       //?Categoria potenziale
219400111116        clear V032fls;
219500111116        clear trmk05ds;
219600111116        IMK05cpo = CPOcpo;
219700111116        trmk05r (kpjba : TRMK05DS);
219800111116        IF  OMK05err = *blanks;
219900111116          V032fls = OMK05cat;
220000111116        ENDIF;
220100111115
220200111115       //?Prossima attività
220300111115         sav_CPOcpo = CPOcpo;
220400111115         exsr RIC_proATT;
220500111115         V032cad = sav_ATCcad;
220600111115
220700111115       //?Viste le nuove modifiche imposto 'F' se cat. eliminabile
220800111116         IF  V032fls = 'E';
220900080604           v032tipf='F'     ;
221000080610           v032dtip='FONDERE'  ;
221100080604           v031tipf='T'     ;
221200080610           v031dtip='TENERE '  ;
221300111115         ENDIF;
221400111115
221500111115       //?Controllo se potenziale mai contattato
221600120914         dCPO01 = CPOrst;
221700120914         IF  §CPOdtpcon <> *blanks and §CPOdtpcon <> *zeros;
221800111115           $Contattato2 = *on;
221900111115         ENDIF;
222000080603
222100080603         v032fil=cpoflt;
222200080603         v032piv=cpopiv;
222300080603         v032cdf=cpocdf;
222400080603 2       if cpofsf='S' ;
222500080603          v032fsf='Sd';
222600080603         ELSE         ;
222700080603          v032fsf='Fi';
222800080603 2       endif;
222900080603
223000080603         v032cmerc=cposct;
223100080603         clear tblkey  ;
223200080603         tblkey=%editc(cposct:'X') ;
223300080603         chain  (1:'1L':tblkey) tabel00f;
223400080603 1       if %found(tabel00f);
223500080603         v032dmerc=tbluni;
223600080904         ds1l=tbluni     ;
223700080904         c032cmuti=§1luti;
223800080603         else ;
223900080603         v032dmerc=*all'?' ;
224000080904         c032cmuti='N'     ;
224100080603 1       endif  ;
224200080603
224300080603         v032cmm=cpocmm;
224400080604         if cpocmm>0   ;
224500130916           chain  (CPOcmm)  AZCMM000;
224600130916 1         if %found(AZCMM01L);
224700130916             v032dcmm=CMMdes;
224800080604           else ;
224900130916             v032dcmm=*all'?' ;
225000080603 1       endif  ;
225100080604 1       endif  ;
225200100525         // Se presenti entrambi i potenziali, uno codificato e uno no,
225300100525         // devo TENERE il codificato
225400111116         if codif1='S' and codif2='N'  ;
225500100525           v032tipf='F'     ;
225600100525           v032dtip='FONDERE'  ;
225700100525           v031tipf='T'     ;
225800100525           v031dtip='TENERE '  ;
225900100525         endif   ;
226000111116         if codif1='N' and codif2='S'  ;
226100100525           v031tipf='F'     ;
226200100525           v031dtip='FONDERE'  ;
226300100525           v032tipf='T'     ;
226400100525           v032dtip='TENERE '  ;
226500100525         endif   ;
226600080603
226700080603          clear   pulizpot2  ;
226800080603       endif     ;
226900080603          ENDSR;
227000080603       //--------------------------------------------------------------
227100080603       //?Interrogazione potenziali per FUSIONE
227200080603       //--------------------------------------------------------------
227300080603           Begsr INTPOTF ;
227400080603
227500100805           clear trmk01ds   ;
227600080610           // se presente uno dei 2 imposto la ragione sociale
227700100805           mk01rag=%subst(v031rag:1:8)   ;
227800080610
227900100520           mk01k11='4'  ;
228000100520           kpjbu=trmK01ds;
228100100520           TRMK01R (kpjba) ;
228200100520           trmk01ds=kpjbu  ;
228300100520           d11cpo=mk01cpo   ;
228400080603
228500080603           // Selezionato potenziale
228600080603 1         if  d11cpo>0;
228700080603             chain(N)  d11cpo  TNCPO000;
228800080603 2            if not %found(tncpo01l) or  fusione(2)>0 or
228900080603               v032ncpo>0  ;
229000080603               errSEDE =*on;
229100080603               errMessage  = *on;
229200080603               errGenerico = *on;
229300080603               errFusione  = *on;
229400080603               V1Dmsg = Msg(19);
229500080603               %subst(v1dmsg:5:11)=%char(cpocpo) ;
229600080603               %subst(v1dmsg:17:12)=cporag ;
229700080603               leavesr ;
229800080603 2            endif  ;
229900080603
230000080603              // Carico dati
230100080603              kcpo=d11cpo   ;
230200080603              pulizpot2='S';
230300080603              exsr ClearCTL3 ;
230400080604              if kcpo<>savcpo;
230500080604               clear   v032tipf   ;
230600080604               clear   v032dtip    ;
230700080604               savcpo=kcpo    ;
230800080604              endif          ;
230900080603              pulizpot2='C';
231000080603              exsr RiempiCTL3 ;
231100080603            endif ;
231200080603
231300080603          ENDSR;
231400080603
231500080603       //--------------------------------------------------------------
231600080603       //?Carico i contatti nel sfl per fusione
231700080603       //--------------------------------------------------------------
231800080603          BEGSR   Carcontatti;
231900080606          s03nrr=1         ;
232000080606          s03tipo='C'      ;
232100080606          clear primariga  ;
232200080609          clear keysfl3    ;
232300080617          clear key3pot1   ;
232400080617          clear key3pot2   ;
232500080609          clear keynrr     ;
232600080609          clear keyrig     ;
232700080609          clear keycont    ;
232800080609          clear XX         ;
232900080604
233000080604          // Leggo tabella contatti e uno per uno vedo se c'e'
233100080604          // scrivo solo se trovato uno dei 2
233200080604          setll (1:'1T') tabel00f  ;
233300080604          reade (1:'1T') tabel00f  ;
233400080604
233500080604  1       dow    not %eof(tabel00f)  ;
233600080604          ds1t=tbluni                ;
233700080604
233800080604  2       if tblflg=' ' and §1trich='C'  ;
233900080604          ktnt=tblkey ;
234000080604
234100080604          // contatto potenziale 1
234200080604          EXSR LeggiNTC1                                  ;
234300080604
234400080604          // contatto potenziale 2
234500080604          EXSR LeggiNTC2                                  ;
234600080604
234700080604          ff=1    ;
234800080604  3       dow ntccod1(ff)<>*blanks or ntccod2(ff)<>*blanks  ;
234900080606
235000080606          // emetto riga descrittiva se caricato almeno un dato
235100080606          if primariga=' '   ;
235200090716          s03dec='-- R U B R I C A --' ;
235300081009          exsr  RigaDESCR  ;
235400080606          Primariga='S' ;
235500080606          endif         ;
235600080604
235700080604          s03dec=ktnt+'-'+§1tdes ;
235800080929          clear s03tpr           ;
235900080929          s03tpr=ktnt            ;
236000080606          s03righe=§1trig          ;
236100080620          clear s031nk2  ;
236200080620          clear s032nk2  ;
236300080604
236400080606          EXSR SCriviSFL3         ;
236500080604
236600080604          ff=ff+1    ;
236700080604  3       enddo     ;
236800080604
236900080604  2       endif               ;
237000080604          reade (1:'1T') tabel00f  ;
237100080604  1       enddo     ;
237200080604
237300080603          ENDSR;
237400080606       //--------------------------------------------------------------
237500080606       //?emetto prima riga descrittiva
237600080606       //--------------------------------------------------------------
237700080606          BEGSR RigaDESCR ;
237800080606          // Riga descrittiva
237900081009          noscelta1=*on   ;
238000081009          noscelta2=*on   ;
238100080606          in52=noscelta1  ;
238200080606          in53=noscelta2  ;
238300080606          Testatahi=*on   ;
238400080606          clear s03righe  ;
238500080606          clear s031sce   ;
238600080606          clear s032sce   ;
238700081010          Infoscelta1=*off;
238800081010          Infoscelta2=*off;
238900081010
239000081010          // Riga per info con campo scelta attivo
239100081010          if IndInfo=*on  ;
239200081010
239300081010           if ImpostaT='1'     ;
239400081010             s031sce='T'    ;
239500081010             SFLRosso1=*on   ;
239600081010           endif           ;
239700081010           if ImpostaT='2'     ;
239800081010             s032sce='T'    ;
239900081010             SFLRosso2=*on   ;
240000081010           endif           ;
240100081010           if w1EsInfo='N' ;
240200081010             s031sce='N'   ;
240300081010             Infoscelta1=*on;
240400081010           endif          ;
240500081010           if w1Esinfo<>'S' and w2EsInfo='S'  ;
240600081010             s031sce='N'   ;
240700081010             Infoscelta1=*on;
240800081010           endif   ;
240900081010           if w2EsInfo='N' ;
241000081010             s032sce='N'   ;
241100081010             Infoscelta2=*on;
241200081010           endif          ;
241300081010           if w2Esinfo<>'S' and w1EsInfo='S'  ;
241400081010             s032sce='N'   ;
241500081010             Infoscelta2=*on;
241600081010           endif   ;
241700081010
241800081010           // Per le infocomm nei campi in52 e in53 ci metto l'altro indicat
241900081010          in52=infoscelta1  ;
242000081010          in53=infoscelta2  ;
242100081010         endif          ;
242200081010
242300080606          clear s031imm   ;
242400081010          clear s032imm   ;
242500081010          if s03nrr=1 ;
242600081010            s031imm='MM/AA' ;
242700081010            s032imm='MM/AA' ;
242800081010          endif   ;
242900080606          clear s031cod   ;
243000080606          clear s032cod   ;
243100080606          clear s031nk2   ;
243200080606          clear s032nk2   ;
243300080610          clear s03forza  ;
243400080606          write   mk07s03     ;
243500080606          Testatahi=*off  ;
243600081010          Infoscelta1=*off;
243700081010          Infoscelta2=*off;
243800081010          ENDSR  ;
243900080604       //--------------------------------------------------------------
244000080604       //?Carico le info commerciali per fusione
244100080604       //--------------------------------------------------------------
244200080604          BEGSR   InfoComm   ;
244300081009          IndInfo=*on      ;
244400081009
244500080606          s03tipo='I'      ;
244600080606          clear primariga  ;
244700081010
244800081010          // Controllo di chi devo tenere le info comm   ;
244900081010          // vince il potenziale da Tenere se:
245000081010          //  - ha dati e sono nuovi
245100081010          //  - ha dati e sono vecchi ed il pot da Fondere ha dati
245200081010          //    vecchi o non ne ha
245300081010          // vince il potenziale da Fondere se:
245400081010          //  - ha dati e sono nuovi ed  il pot da Tenere ha dati
245500081010          //    vecchi o non ne ha
245600081010
245700081010          fcpo=v031fcpo      ;
245800081010          ncpo=v031ncpo      ;
245900081010          w1EsInfo='N'       ;
246000081010          setll     kcpo ticpi01l             ;
246100081010  1       if %equal(ticpi01l)  ;
246200081010           w1EsInfo='O'      ;
246300081010
246400081010          reade     kcpo ticpi01l             ;
246500081010  2       dow not %eof(ticpi01l)  and w1EsInfo<>'S' ;
246600081010  3       if cpitpf<>'10 ' and cpitpf<>'20 ' and cpitpf<>'30 ' and
246700081010             cpitpf<>'40 ' and cpitpf<>'OUT'  ;
246800081010             w1EsInfo='S'      ;
246900081010          else;
247000081010             reade     kcpo ticpi01l             ;
247100081010  3       endif;
247200081010  2       enddo     ;
247300081010  1       endif     ;
247400081010
247500081010          fcpo=v032fcpo      ;
247600081010          ncpo=v032ncpo      ;
247700081010          w2EsInfo='N'       ;
247800081010          setll     kcpo ticpi01l             ;
247900081010  1       if %equal(ticpi01l)  ;
248000081010           w2EsInfo='O'      ;
248100081010
248200081010          reade     kcpo ticpi01l             ;
248300081010  2       dow not %eof(ticpi01l)  and w2EsInfo<>'S' ;
248400081010  3       if cpitpf<>'10 ' and cpitpf<>'20 ' and cpitpf<>'30 ' and
248500081010             cpitpf<>'40 ' and cpitpf<>'OUT'  ;
248600081010             w2EsInfo='S'      ;
248700081010          else;
248800081010             reade     kcpo ticpi01l             ;
248900081010  3       endif;
249000081010  2       enddo     ;
249100081010  1       endif;
249200081010
249300081010          if v031tipf='T'     ;
249400081010          TieniEsInfo=w1EsInfo;
249500081010          FondiesInfo=w2EsInfo;
249600081010          else                ;
249700081010          TieniEsInfo=w2EsInfo;
249800081010          FondiesInfo=w1EsInfo;
249900081010          endif               ;
250000081010
250100081010          clear ImpostaT   ;
250200081010          ImpostaT='T'   ;
250300081010
250400081010          if   (TieniEsInfo='N' and FondiEsinfo<>'N')  or
250500081010               (TieniEsInfo='O' and FondiEsinfo='S')  ;
250600081010            ImpostaT='F'   ;
250700081010          endif            ;
250800081010
250900081010          if    TieniEsInfo='N' and FondiEsinfo='N'   ;
251000081010            ImpostaT='N'   ;
251100081010          else  ;
251200081010              select   ;
251300081010              when ImpostaT='T' and v031tipf='T' ;
251400081010            ImpostaT='1'   ;
251500081010              when ImpostaT='T' and v032tipf='T' ;
251600081010            ImpostaT='2'   ;
251700081010              when ImpostaT='F' and v031tipf='F' ;
251800081010            ImpostaT='1'   ;
251900081010              when ImpostaT='F' and v032tipf='F' ;
252000081010            ImpostaT='2'   ;
252100081010              endsl ;
252200081010          endif            ;
252300080604
252400081010          // se non ho info  --> non carico nulla
252500081010          if ImpostaT<>'N'   ;
252600081010
252700080604          // Leggo tabella DELLE INFO e uno per uno vedo se c'e'
252800080604          // scrivo solo se trovato uno dei 2
252900080929          yy=  1   ;
253000080929          dow   yy<=50   ;
253100080929  1       if    kifoord(yy) <>*blanks   ;
253200080929          ktprI=%subst(kifoord(yy):6:3) ;
253300080929          wdes =%subst(kifoord(yy):9:19) ;
253400080929          wIspp =%subst(kifoord(yy):28:1) ;
253500080929          // chain tabella per la decodifica
253600080604
253700080604          // contatto potenziale 1
253800080604          EXSR LeggiCPI1                                  ;
253900080604
254000080604          // contatto potenziale 2
254100080604          EXSR LeggiCPI2                                  ;
254200080604
254300080604          ff=1    ;
254400080929  3       dow ff<=40 and ntccod1(ff)<>*blanks or ntccod2(ff)<>*blanks  ;
254500080606
254600080606          // Emetto riga descrittiva se caricato almeno un dato
254700080606          if primariga=' '   ;
254800080610          s03dec='-INFO  COMMERCIALI-' ;
254900080606          if s03nrr>1   ;
255000080606          s03nrr=s03nrr+1  ;
255100080606          endif         ;
255200080606
255300080606          exsr  RigaDESCR  ;
255400080606          Primariga='S' ;
255500080606          endif         ;
255600080604
255700080929          clear s03tpr   ;
255800080929          s03tpr=ktprI  ;
255900141002
256000141002       //?Se INFO relativa al NON affidato a BRT metto n.a. davanti
256100141002          IF  S03tpr = 'NRF' or S03tpr = 'NRO' or S03tpr = 'NER' or
256200141002              S03tpr = 'NAE' or S03tpr = 'NTR';
256300141002            S03dec = 'N.A. ' + wdes;
256400141002          ELSE;
256500080929          s03dec=wdes    ;
256600141002          ENDIF;
256700141002
256800080929          if wIspp  =' '     ;
256900080606            s03righe=1               ;
257000080620            clear s031nk2    ;
257100080620            clear s032nk2    ;
257200080606          else   ;
257300080606            s03righe=90              ;
257400081013            s031nk2=sottot1(ff)       ;
257500081013            s032nk2=sottot2(ff)  ;
257600080606          endif  ;
257700080604
257800080606          EXSR  SCriviSFL3   ;
257900080604
258000080604          ff=ff+1    ;
258100080604  3       enddo     ;
258200080929          endif     ;
258300080929
258400080929          yy=yy+1    ;
258500080604  1       enddo     ;
258600081010
258700081010          endif     ;
258800080604
258900081009          IndInfo=*off     ;
259000080604          ENDSR;
259100140923
259200140923       //--------------------------------------------------------------
259300140923       //?Caricamento Dettaglio Prodotti per FUSIONE.                  ?
259400140923       //--------------------------------------------------------------
259500140923       BEGSR  sr_DettProd;
259600140923
259700140923         IndInfo = *on;
259800140923         clear  PrimaRiga;
259900140923         S03tipo = 'P';
260000140923
260100140923         Ktnt = 'DP';
260200140923         clear  Knk2;
260300140923         clear  NTCcod1;
260400140923         clear  NTCcod2;
260500140923         clear  DtImm1;
260600140923         clear  DtImm2;
260700140923
260800140923         chain  ( 1 : '1T' : Ktnt )  TABEL;
260900140923         if  %found(TABEL00F);
261000140923           ds1T = TBLuni;
261100140923         else;
261200140923           clear  ds1T;
261300140923           §1Tdes = *all'? ';
261400140923         endif;
261500140923
261600140923         // -?Reperimento Dettaglio Prodotti del 1° Potenziale.?
261700140923         clear  FF;
261800140923         Knk1 = %editc( V031fcpo : 'X' ) +
261900140923                %editc( V031ncpo : 'X' );
262000140923         setll     ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
262100140923         reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
262200140923
262300140923         DoW  Not  %eof(TFNTC01L);
262400140923
262500140923           FF += 1;
262600140923           DataDMY = %date( NTCntr : *YMD );
262700140923           w004a   = %subst( %editc( %dec( DataDMY ) : 'X' ) : 3 : 4 );
262800140923           DtImm1(FF)  = w004a;
262900140923           NTCcod1(FF) = NTCrnt;
263000140923
263100140923           reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
263200140923
263300140923         EndDo;
263400140923
263500140923         // -?Reperimento Dettaglio Prodotti del 2° Potenziale.?
263600140923         clear  FF;
263700140923         Knk1 = %editc( V032fcpo : 'X' ) +
263800140923                %editc( V032ncpo : 'X' );
263900140923         setll     ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
264000140923         reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
264100140923
264200140923         DoW  Not  %eof(TFNTC01L);
264300140923
264400140923           FF += 1;
264500140923           DataDMY = %date( NTCntr : *YMD );
264600140923           w004a   = %subst( %editc( %dec( DataDMY ) : 'X' ) : 3 : 4 );
264700140923           DtImm2(FF)  = w004a;
264800140923           NTCcod2(FF) = NTCrnt;
264900140923
265000140923           reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
265100140923
265200140923         EndDo;
265300140925
265400140925
265500140925         // -?Controllo di chi devo tenere le info comm?
265600140925         //  ?Vince il potenziale da Tenere se:?
265700140925         //  ?- ha dati?
265800140925         //  ?Vince il potenziale da Fondere se:?
265900140925         //  ?- ha dati ed il potenziale da Tenere NON ha dati?
266000140925         w1EsInfo = 'N';
266100140925         w2EsInfo = 'N';
266200140925         ImpostaT = 'T';
266300140925         Select;
266400140925           When  V031tipf = 'T'  and  NTCcod1(1) <> *blank;
266500140925             ImpostaT = '1';
266600140925             w1EsInfo = 'T';
266700140925           When  V032tipf = 'T'  and  NTCcod2(1) <> *blank;
266800140925             ImpostaT = '2';
266900140925             w2EsInfo = 'T';
267000140925           When  V031tipf = 'T'  and  NTCcod1(1) =  *blank
267100140925                                 and  NTCcod2(1) <> *blank;
267200140925             ImpostaT = '2';
267300140925             w2EsInfo = 'T';
267400140925           When  V032tipf = 'T'  and  NTCcod1(1) <> *blank
267500140925                                 and  NTCcod2(1) =  *blank;
267600140925             ImpostaT = '1';
267700140925             w1EsInfo = 'T';
267800140925         EndSl;
267900140925
268000140923
268100140923         // -?Scrittura record nel subfile?
268200140923         FF = 1;
268300140923         DoW  NTCcod1(FF) <> *blank  or  NTCcod2(FF) <> *blank;
268400140923
268500140923           // -?Emissione della riga descrittiva se caricato almeno un dato?
268600140923           If  PrimaRiga = *blank;
268700140923
268800140925             S03dec    = '-DESCRIZIONE PROD.-';
268900140923             if  S03nrr > 1;
269000140923               S03nrr += 1;
269100140923             endif;
269200140923             exsr  RigaDESCR;
269300140923             Primariga = 'S';
269400140923
269500140923           EndIf;
269600140923
269700140923           // -?Emissione del Dettaglio Prodotti dei 2 potenziali?
269800140923           S03dec = Ktnt + '-' + §1Tdes;
269900140923           S03tpr = Ktnt;
270000140923           S03righe = §1Trig;
270100140923           clear  S031nk2;
270200140923           clear  S032nk2;
270300140923
270400140923           exsr  ScriviSFL3;
270500140923
270600140923           FF += 1;
270700140923
270800140923         EndDo;
270900140923
271000140923         IndInfo = *off;
271100140923
271200140923       ENDSR;
271300140923
271400080606       //--------------------------------------------------------------
271500080606       //?Scrittura righe di dettaglio sfl3
271600080606       //--------------------------------------------------------------
271700080606         BEGSR ScriviSFL3   ;
271800081009
271900081009         // Memorizzo in skiera i codici con più di una riga ;
272000081009 1       if s03righe>1   ;
272100081009
272200081009 2       if   s031nk2<>*blanks   ;
272300081009           w007a=s03tpr +s031nk2 ;
272400081009           indx=%lookup(w007a:keysfl3)  ;
272500081009 3         if indx=0  ;
272600081009            xx=xx+1   ;
272700081009            keysfl3(xx)=w007a ;
272800081009            keyrig (xx)=s03righe   ;
272900081009 3         endif   ;
273000081009
273100081009            // Memorizzo solo una volta il codice
273200081009           indx=%lookup(s03tpr :key3pot1)  ;
273300081009 3         if indx=0   ;
273400081009              key3pot1(xx)=s03tpr              ;
273500081009              keynrr(xx) =s03nrr+1   ;
273600081009              // Se presenti per entrami lo memorizzo subito per
273700081009              //  avere il codice nella stessa posizione di skiera
273800081009              if s032nk2<>*blanks   ;
273900081009               key3pot2(xx)=s03tpr              ;
274000081009              endif                 ;
274100081009 3         endif   ;
274200081009 2       endif   ;
274300081009
274400081009 2       if   s032nk2<>*blanks   ;
274500081009           w007a=s03tpr+s032nk2 ;
274600081009           indx=%lookup(w007a:keysfl3)  ;
274700081009 3         if indx=0   ;
274800081009            xx=xx+1   ;
274900081009            keysfl3(xx)=w007a ;
275000081009            keyrig (xx)=s03righe   ;
275100081009 3         endif   ;
275200081009            // Memorizzo solo una volta il codice
275300081009           indx=%lookup(s03tpr :key3pot2)  ;
275400081009 3         if indx=0   ;
275500081009            key3pot2(XX)=s03tpr              ;
275600081009            keynrr(xx) =s03nrr+1   ;
275700081009 3         endif   ;
275800081009 2       endif   ;
275900081009
276000081009 2       if   s031nk2=*blanks and s032nk2=*blanks  ;
276100081009           w007a=s03tpr+'    '  ;
276200081009           indx=%lookup(w007a:keysfl3)  ;
276300081009 3         if indx=0   ;
276400081009            xx=xx+1   ;
276500081009            keysfl3(xx)=w007a ;
276600081009            keyrig (xx)=s03righe   ;
276700081009            keynrr(xx) =s03nrr+1   ;
276800081009              if ntccod1(ff)<>*blanks    ;
276900081009               key3pot1(xx)=s03tpr                     ;
277000081009              endif  ;
277100081009              if ntccod2(ff)<>*blanks    ;
277200081009               key3pot2(xx)=s03tpr                     ;
277300081009              endif  ;
277400081009 3         endif   ;
277500081009 2         endif   ;
277600081009 1       endif   ;
277700081009
277800081009         clear s031sce ;
277900081009         clear s032sce ;
278000081009
278100081009         if ntccod1(ff)<>*blanks            ;
278200081009         s031cod=ntccod1(ff)                 ;
278300081009         s031imm=%subst(dtimm1(ff):1:2)+'/' +%subst(dtimm1(ff):3:2)  ;
278400081009         in52='0'             ;
278500081009         noscelta1=*off  ;
278600081009         // Se scelto primo pote imposto la "T"
278700081009           if v031tipf='T'   ;
278800081009           s031sce='T'  ;
278900081009           SFLRosso1=*on ;
279000081009           endif    ;
279100081009         else   ;
279200081009         clear s031cod    ;
279300081009         clear s031imm    ;
279400081009         in52='1'             ;
279500081009         noscelta1=*on   ;
279600081009         clear s031nk2   ;
279700081009         endif   ;
279800081009
279900081009         if ntccod2(ff)<>*blanks            ;
280000081009         s032cod=ntccod2(ff)                 ;
280100081009         s032imm=%subst(dtimm2(ff):1:2)+'/' +%subst(dtimm2(ff):3:2)  ;
280200081009         in53='0'             ;
280300081009         noscelta2=*off  ;
280400081009
280500081009         // Se scelto 2° pot. o non presente riga per pot1 imposto la "T"
280600081009           if v032tipf='T' or (IndInfo=*off and s03righe=1 and noscelta1=*on);
280700081009           s032sce='T'  ;
280800081009           SFLRosso2=*on ;
280900081009           endif    ;
281000081009
281100081009           // Se più righe e 1°pot vuoto, vedo se devo impostare la "T"
281200081009           //   sul secondo potenziale
281300081009             if INdInfo=*off and v031tipf='T' and s03righe>1 and noscelta1=*on;
281400081009               Indx=%lookup(s03tpr :key3pot1)         ;
281500081009               if Indx=0   ;
281600081009                s032sce='T'  ;
281700081009                SFLRosso2=*on ;
281800081009               endif       ;
281900081009             endif   ;
282000081009
282100081009         else   ;
282200081009         clear s032cod    ;
282300081009         clear s032imm    ;
282400081009         in53='1'             ;
282500081009         noscelta2=*on   ;
282600081009         clear s032nk2   ;
282700081009
282800081009         // Se scelto 2° pot. e non presente riga per pot2 imposto la "T"
282900081009         //   nel 1° potenziale
283000081009           if IndInfo=*off and v032tipf='T' and s03righe=1 and noscelta1=*off;
283100081009           s031sce='T'  ;
283200081009           SFLRosso1=*on ;
283300081009           endif    ;
283400081009
283500081009           // Se più righe e 2°pot vuoto, vedo se devo impostare la "T"
283600081009           //   sul 1° potenziale
283700081009             if IndINfo=*off and v032tipf='T' and s03righe>1 and noscelta1=*off;
283800081009               Indx=%lookup(s03tpr:key3pot2)         ;
283900081009               if Indx=0   ;
284000081009                s031sce='T'  ;
284100081009                SFLRosso1=*on ;
284200081009               endif       ;
284300081009             endif   ;
284400081009
284500081009         endif   ;
284600081009
284700081009         clear s03forza  ;
284800081009
284900081009         s03nrr=s03nrr+1     ;
285000081009          // posizionamento cursore sulla prima riga
285100140923          if (s03tipo='C' and s03nrr=2) ;
285200081009            if noscelta1=*off    ;
285300081009            errsfl3_1=*on    ;
285400081009            else     ;
285500081009            errsfl3_2=*on    ;
285600081009            endif    ;
285700081009          endif    ;
285800081009
285900081009          // Per info proteggo sempre il campo di scelta
286000081009          if IndInfo          ;
286100081009          clear s031sce ;
286200081009          clear s032sce ;
286300081010          SFLRosso1=*off;
286400081010          SFLRosso2=*off;
286500081010            if ImpostaT='1';
286600081010             SFLRosso1=*on ;
286700081010            endif               ;
286800081010            if ImpostaT='2';
286900081010             SFLRosso2=*on ;
287000081010            endif               ;
287100081010          endif               ;
287200081009
287300081009          write mk07s03       ;
287400081009          errsfl3_1=*off   ;
287500081009          errsfl3_2=*off   ;
287600081009          SFLRosso1=*off   ;
287700081009          SFLRosso2=*off   ;
287800080606          ENDSR               ;
287900080604       //--------------------------------------------------------------
288000080604       //?Leggi contatti per potenziale 1 - Fusione
288100080604       //--------------------------------------------------------------
288200080604          BEGSR  LeggiNTC1                                ;
288300080604          clear ntccod1  ;
288400080605          clear DtImm1   ;
288500080604          clear FF       ;
288600080619          clear knk2     ;
288700080604
288800080604          knk1=%editc(v031fcpo:'X')+%editc(v031ncpo:'X')  ;
288900080619          setll    ('P':knk1:knk2:ktnt) tfntc01l             ;
289000080619          READE(N) ('P':knk1:knk2:ktnt) tfntc01l             ;
289100080604          dow not %eof(tfntc01l)  ;
289200080604          ff=ff+1              ;
289300080605          dataymd=%date(ntcntr:*ymd)    ;
289400080605          datadmy=dataymd ;
289500080605          w0060=%dec(datadmy)  ;
289600080605          w004a     =%subst(%editc(w0060:'X'):3:4) ;
289700080610          dtimm1(ff)=w004a                    ;
289800080605
289900080604          ntccod1(ff)=ntcrnt       ;
290000080619          READE(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
290100080604          enddo                ;
290200080604
290300080604          ENDSR                ;
290400080604       //--------------------------------------------------------------
290500080604       //?Leggi contatti per potenziale 2 - Fusione
290600080604       //--------------------------------------------------------------
290700080604          BEGSR  LeggiNTC2                                ;
290800080604          clear ntccod2  ;
290900080605          clear DtImm2   ;
291000080604          clear FF       ;
291100080604
291200080604          knk1=%editc(v032fcpo:'X')+%editc(v032ncpo:'X')  ;
291300080619          setll    ('P':knk1:knk2:ktnt) tfntc01l             ;
291400080604
291500080619          READE(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
291600080604          dow not %eof(tfntc01l)  ;
291700080604          ff=ff+1              ;
291800080605          dataymd=%date(ntcntr:*ymd)    ;
291900080605          datadmy=dataymd ;
292000080605          w0060=%dec(datadmy)  ;
292100080605          w004a     =%subst(%editc(w0060:'X'):3:4) ;
292200080610          dtimm2(ff)=     w004a                    ;
292300080605
292400080604          ntccod2(ff)=ntcrnt       ;
292500080619          reade(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
292600080604          enddo                ;
292700080604          ENDSR                ;
292800080604       //--------------------------------------------------------------
292900080604       //?Leggo info comm per potenziale 1 - Fusione
293000080604       //--------------------------------------------------------------
293100080604          BEGSR  LeggiCPI1                                ;
293200080604          clear ntccod1  ;
293300081013          clear sottot1  ;
293400080605          clear DtImm1   ;
293500080604          clear FF       ;
293600080604
293700080929          fcpo=v031fcpo      ;
293800080929          ncpo=v031ncpo      ;
293900080929          setll    (kcpo:ktprI) ticpi01l             ;
294000080929          READE(n) (kcpo:ktprI) ticpi01l             ;
294100081009
294200080929  1       dow not %eof(ticpi01l)  ;
294300081009  2       if cpiatb=' '        ;
294400081009
294500081009          EXSR ElaboraCPI      ;
294600081009
294700081009          ff=ff+1              ;
294800081009          dtimm1(ff)= w004a        ;
294900081009          ntccod1(ff)=wcodcpi      ;
295000081013          sottot1(ff)=wspf         ;
295100081009
295200081009  2       endif;
295300080604
295400080929          READE(n) (kcpo:ktprI) ticpi01l             ;
295500081009  1       enddo                ;
295600080604
295700080604          ENDSR                ;
295800080604       //--------------------------------------------------------------
295900080604       //?Leggo info comm per potenziale 2 - Fusione
296000080604       //--------------------------------------------------------------
296100080604          BEGSR  LeggiCPI2                                ;
296200080604          clear ntccod2  ;
296300081013          clear sottot2  ;
296400080605          clear DtImm2   ;
296500080604          clear FF       ;
296600080604
296700080929          fcpo=v032fcpo      ;
296800080929          ncpo=v032ncpo      ;
296900080929          setll    (kcpo:ktprI) ticpi01l             ;
297000080929          READE(n) (kcpo:ktprI) ticpi01l             ;
297100081009  1       dow not %eof(ticpi01l)  ;
297200081009  2       if cpiatb=' '        ;
297300081009
297400081009          EXSR ElaboraCPI      ;
297500081009
297600081009          ff=ff+1              ;
297700081009          dtimm2(ff)= w004a        ;
297800081009          ntccod2(ff)=wcodcpi      ;
297900081013          sottot2(ff)=wspf         ;
298000081009
298100081009  2       endif;
298200080605
298300080929          READE(n) (kcpo:ktprI) ticpi01l             ;
298400081009  1       enddo                ;
298500080604
298600080604          ENDSR                ;
298700081009       //--------------------------------------------------------------
298800081009       //?elaborazione per visualizzare    info comm
298900081009       //--------------------------------------------------------------
299000081009          BEGSR   ElaboraCPI   ;
299100081009
299200081009          dataiso=%date(cpidim)   ;
299300081009          datadmy=dataiso ;
299400081009          w0060=%dec(datadmy)  ;
299500081009          w004a     =%subst(%editc(w0060:'X'):3:4) ;
299600081009
299700081009          clear difo     ;
299800081009          clear wcodcpi  ;
299900081013          wspf=cpispf    ;
300000081009          // Se ha il sottotipo vedo se elencati o a scelta con '?'
300100081009  2       if cpispf<>*blanks   ;
300200081009            tbeke1=cpitpf        ;
300300081009            chain    ('IFO':tbeke1) tntbe01l             ;
300400081009  3         if %found(tntbe01l)   ;
300500081009            difo=tbeuni    ;
300600081009  3         endif          ;
300700081009
300800081009            // Decodifico il sottotipo
300900081009            tbeke2=cpiSPF        ;
301000081009            chain    ('IFS':tbeke1:TBEKE2) tntbe01l             ;
301100081009  3         IF %FOUND(TNTBE01L)  ;
301200081009  4            if §ifoifv='?'       ;
301300081009                 wcodcpi    =TBEUNI   ;
301400081009               else    ;
301500081009                 wcodcpi    =%subst(TBEUNI:1:15)   ;
301600081009  4            endif   ;
301700081009
301800081009  x3        ELSE    ;
301900081009             wcodcpi    =cpispf       ;
302000081009  3         endif   ;
302100081009  2         endif   ;
302200081009
302300081009  1       if cpispf=*blanks or §ifoifv='S'    ;
302400081009
302500081009  2         if cpifsn='N'        ;
302600081009            wcodcpi    =%trim(wcodcpi)+' NO'  ;
302700081009  2         endif               ;
302800081009  2         if cpifsn='S'        ;
302900081009            wcodcpi    =%trim(wcodcpi)+' SI'  ;
303000081009  2         endif               ;
303100081009
303200081009   2        if cpival>0  and cpitva='%  '    ;
303300081009            wcodcpi    =%trim(wcodcpi)+' '+%char(cpival) +'%' ;
303400081009            wcodcpi    =%trim(wcodcpi)  ;
303500081009   2        endif  ;
303600081009   2        if cpival>0  and cpitva='KG '    ;
303700081009            wcodcpi    =%trim(wcodcpi)+' KG'+%char(cpival) ;
303800081009            wcodcpi    =%trim(wcodcpi)  ;
303900081009   2        endif  ;
304000141002
304100141002   2        IF  CPIval > 0 and CPItva = *blanks;
304200141002              wcodcpi = %trim(wcodcpi)+ %char(cpival);
304300141002   2        endif  ;
304400081009
304500081009   2        if cpipft>0          ;
304600081009            wpft=cpipft   ;
304700081009            wcodcpi    =%trim(wcodcpi)+' Fat'+%char(wpft) ;
304800081009            wcodcpi    =%trim(wcodcpi)  ;
304900081009   2        endif;
305000081009
305100081009   2        if cpisna>0 ;
305200081009            wcodcpi    =%trim(wcodcpi)+' Spe'+%char(cpisna) ;
305300081009            wcodcpi    =%trim(wcodcpi)  ;
305400081009   2        endif;
305500081009
305600081009   1      endif;
305700081009          ENDSR;
305800080604       //--------------------------------------------------------------
305900080603       //?controlla SFL3  - Fusione
306000080603       //--------------------------------------------------------------
306100080606       BEGSR   contrS03    ;
306200080604
306300080617       // controllo nel sfl control solo fin quando non ho ancora
306400080609       //  caricato il sfl
306500080609
306600080609 1        if SflDSp_N=*on   ;
306700080609
306800080609       // Il secondo codice potenziale deve essere selezionato per la
306900080604       //  fusione
307000080604  1       if v032fcpo=0    and v032ncpo=0         ;
307100080604           errFusione  = *on;
307200080604           errMessage  = *on;
307300080604           errGenerico = *on;
307400080618           V1Dmsg = Msg(27);
307500080604           leavesr;
307600080604  1       endif    ;
307700080604
307800080604       // Non può essere lo stesso codice
307900080604  1       if v031fcpo=v032fcpo and v031ncpo=v032ncpo  ;
308000080604           errFusione  = *on;
308100080604           errMessage  = *on;
308200080604           errGenerico = *on;
308300080604           V1Dmsg = Msg(20);
308400080604           leavesr;
308500080604  1        endif    ;
308600080604
308700080604       // controllo il secondo codice e carico i dati
308800080604  1    if    fusione(2)=0    ;
308900080604
309000080604         fcpo=v032fcpo      ;
309100080604         ncpo=v032ncpo      ;
309200080604         chain(N)  Kcpo  TNCPO000;
309300080604
309400080604         // Codice potenziale NON trovato
309500080604 2       if  NOT  %found(TNCPO01L);
309600100420          if v032rag<>*blanks  ;
309700100420            pulizpot2='S';
309800100420            exsr ClearCTL3 ;
309900100420          endif   ;
310000100420           errFusione  = *on;
310100080604           errMessage  = *on;
310200080604           errGenerico = *on;
310300080618           V1Dmsg = Msg(27);
310400080604           leavesr;
310500080604  2      endif          ;
310600100420
310700080604         pulizpot2='S';
310800080604         exsr ClearCTL3 ;
310900080604         if kcpo<>savcpo;
311000080604          clear   v032tipf   ;
311100080604          clear   v032dtip    ;
311200080604         savcpo=kcpo    ;
311300080604         endif          ;
311400080604
311500080604         pulizpot2='C';
311600080604         exsr RiempiCTL3 ;
311700080604  1    endif  ;
311800100420
311900080603       // Non posso fondere 2 sedi
312000080606  1       if v031fsf='Sd' and v032fsf='Sd' ;
312100080603           errFusione  = *on;
312200080603           errMessage  = *on;
312300080603           errGenerico = *on;
312400080603           V1Dmsg = Msg(04);
312500080603           leavesr;
312600080604  1       endif    ;
312700080603
312800111115       //?Non posso fondere se 2 potenziali in cat. eliminabile
312900111115  1     IF  V031fls = 'E' and V032fls = 'E';
313000080603           errFusione  = *on;
313100080603           errMessage  = *on;
313200080603           errGenerico = *on;
313300080603           V1Dmsg = Msg(05);
313400080603           leavesr;
313500080604  1       endif    ;
313600080604
313700080604        // Decodifico il flag Tenere/Fondere  ;
313800080604        clear v031dtip   ;
313900080604        clear v032dtip   ;
314000080610
314100080610        // Richiesta la manutenzione
314200100520        clear trmk01ds  ;
314300080610        Indx=1              ;
314400080610          dow indx<=2        ;
314500080610          clear kcpo           ;
314600080610
314700080610          if  Indx=1 and v031tipf='M'   ;
314800080610          fcpo=v031fcpo  ;
314900080610          ncpo=v031ncpo  ;
315000080618          clear v031tipf   ;
315100080610          endif    ;
315200080610          if  Indx=2 and v032tipf='M'   ;
315300080610          fcpo=v032fcpo  ;
315400080610          ncpo=v032ncpo  ;
315500080618          clear v032tipf   ;
315600080610          endif    ;
315700080610
315800080610          if kcpo>0       ;
315900100520            clear trmk01ds  ;
316000100520            mk01k10='R'   ;
316100100520            mk01cpo=kcpo  ;
316200100520
316300100520            kpjbu =  trmk01ds;
316400100520            trmk02r ( kpjba : trmk01ds );
316500080618
316600080618            // se restituisce errore, lo visualizzo
316700100805              if mk01m10<>*blanks   ;
316800080618                if Indx=1   ;
316900080618                errtipoFus1 = *on;
317000080618                else    ;
317100080618                errtipoFus2 = *on;
317200080618                endif   ;
317300080618                errMessage  = *on;
317400080618                errGenerico = *on;
317500080618                V1Dmsg = Msg(26);
317600080618              leavesr;
317700080618              else   ;
317800080618               $InzS03  = *on ;
317900080618              endif  ;
318000080610          endif   ;
318100080610
318200080610          Indx=indx+1 ;
318300080610          enddo   ;
318400080610
318500080610          // Se manutenzione riemetto videata per ricarica i dati
318600080610          if $inzs03=*on  ;
318700080610          leavesr  ;
318800080610          endif    ;
318900080604
319000080604          if v031tipf='T'   ;
319100080610          v031dtip='TENERE '  ;
319200080604          endif   ;
319300080604          if v031tipf='F'   ;
319400080610          v031dtip='FONDERE'  ;
319500080604          endif   ;
319600080604
319700080604          if v032tipf='T'   ;
319800080610          v032dtip='TENERE '  ;
319900080604          endif   ;
320000080604          if v032tipf='F'   ;
320100080610          v032dtip='FONDERE'  ;
320200080604          endif   ;
320300080604
320400080604          if (v031tipf='T' and v032tipf='T') or
320500080604            (v031tipf='F' and v032tipf='F')  or
320600080618             v031tipf=' ';
320700080610           errtipoFus1 = *on;
320800080604           errMessage  = *on;
320900080604           errGenerico = *on;
321000080604           V1Dmsg = Msg(21);
321100080604           leavesr;
321200080604          endif ;
321300080618           if  v032tipf=' ';
321400080618           errtipoFus2 = *on;
321500080618           errMessage  = *on;
321600080618           errGenerico = *on;
321700080618           V1Dmsg = Msg(21);
321800080618           leavesr;
321900080618          endif ;
322000080610
322100111115       //?Non posso tenere un potenziale in cat. eliminabile
322200111115        if v031tipf='T' and v031fls = 'E' ;
322300080610           errtipoFus1 = *on;
322400080610           errMessage  = *on;
322500080610           errGenerico = *on;
322600080610           V1Dmsg = Msg(25);
322700080610           leavesr;
322800080610        endif   ;
322900111115        if v032tipf='T' and v032fls = 'E' ;
323000080610           errtipoFus2 = *on;
323100080610           errMessage  = *on;
323200080610           errGenerico = *on;
323300080610           V1Dmsg = Msg(25);
323400080610           leavesr;
323500080610        endif   ;
323600100525
323700100525        //  se un potenziale codificato e uno no, devo TENERE per forza
323800100525        //     quello codificato
323900111116        if codif1='S' and codif2='N'  and v031tipf='F' ;
324000100525           errtipoFus1 = *on;
324100100525           errMessage  = *on;
324200100525           errGenerico = *on;
324300100525           V1Dmsg = Msg(43);
324400100525           leavesr;
324500100525        endif   ;
324600111116        if codif1='N' and codif2='S'  and v032tipf='F' ;
324700100525           errtipoFus1 = *on;
324800100525           errMessage  = *on;
324900100525           errGenerico = *on;
325000100525           V1Dmsg = Msg(43);
325100100525           leavesr;
325200100525        endif   ;
325300080619
325400080619        // Uno dei 2 potenziali deve essere in gestione all'utente
325500080619        Indx =%lookup(v031fil:pog)   ;
325600080619        Indx2=%lookup(v032fil:pog)   ;
325700080619         if Indx=0 and Indx2=0  ;
325800080619           errtipoFus1 = *on;
325900080619           errMessage  = *on;
326000080619           errGenerico = *on;
326100080619           V1Dmsg = Msg(29);
326200080619           leavesr;
326300080619          endif       ;
326400080619
326500111115       // Se quello che fondo non è mio
326600111115       //?il potenziale deve essere un mai contattato
326700111115       // Senza anagrafiche  SEMPRE
326800111115        if v031tipf='F' and Indx=0 and $Contattato1;
326900081010             fcpo=v031fcpo    ;
327000081010             ncpo=v031ncpo    ;
327100081010             setll kcpo  cnaco16l  ;
327200081010             setll kcpo  tfaco16l  ;
327300081010             if %equal(cnaco16l) or %equal(tfaco16l) ;
327400081010             errtipoFus1 = *on;
327500081010             errMessage  = *on;
327600081010             errGenerico = *on;
327700100524             V1Dmsg = Msg(42);
327800081010             leavesr;
327900081010          endif    ;
328000080619        endif       ;
328100081010
328200111115        if v032tipf='F' and Indx2=0 and $Contattato2;
328300081010             fcpo=v032fcpo    ;
328400081010             ncpo=v032ncpo    ;
328500081010             setll kcpo  cnaco16l  ;
328600081010             setll kcpo  tfaco16l  ;
328700081010             if %equal(cnaco16l) or %equal(tfaco16l) ;
328800081010             errtipoFus2 = *on;
328900081010             errMessage  = *on;
329000081010             errGenerico = *on;
329100100524             V1Dmsg = Msg(42);
329200081010             leavesr;
329300081010           endif       ;
329400081010        endif       ;
329500080904
329600081010        //  Se lo posso manutenzionare, il potenziale da Tenere non deve avere
329700081010        //  un settore merceologico NON CLASSIFICATO
329800080904        if v031tipf='T'  and Indx>0  and c031cmuti='N'   ;
329900080904           errtipoFus1 = *on;
330000080904           errMessage  = *on;
330100080904           errGenerico = *on;
330200080904           V1Dmsg = Msg(35);
330300080904           leavesr;
330400080904        endif       ;
330500080904        if v032tipf='T'  and Indx2>0  and c032cmuti='N'   ;
330600080904           errtipoFus2 = *on;
330700080904           errMessage  = *on;
330800080904           errGenerico = *on;
330900080904           V1Dmsg = Msg(35);
331000080904           leavesr;
331100080904        endif       ;
331200080619
331300080619       // Memorizzo il codice da tenere ;
331400080619       if v031tipf='T'  ;
331500080619        fcpo=v031fcpo    ;
331600080619        ncpo=v031ncpo    ;
331700080619       else   ;
331800080619        fcpo=v032fcpo    ;
331900080619        ncpo=v032ncpo    ;
332000080619       endif   ;
332100080619
332200080619        Tienicpo=kcpo  ;
332300080619
332400080619       // Memorizzo il codice da fondere ;
332500080619       if v031tipf='T'  ;
332600080619        fcpo=v032fcpo    ;
332700080619        ncpo=v032ncpo    ;
332800080619        Fondirag=v032rag  ;
332900080619       else   ;
333000080619        fcpo=v031fcpo    ;
333100080619        ncpo=v031ncpo    ;
333200080619        Fondirag=v031rag  ;
333300080619       endif   ;
333400080619
333500080619        Fondicpo=kcpo  ;
333600100521
333700100525       // Il potenziale da Fondere non deve avere una trattativa aperta non fittizia
333800100521
333900100521           clear tnta43ds  ;
334000100525           ita43cpo=fondicpo   ;
334100100521
334200100521           TNTA43R (TNTA43DS)  ;
334300100521
334400100521           if ota43nrv>0 or ota43err<>' '   ;
334500100521           if v031tipf='F'  ;
334600100521            errtipoFus1 = *on;
334700100521           ELSE   ;
334800100521            errtipoFus2 = *on;
334900100521           ENDIF   ;
335000100521           errMessage  = *on;
335100100521           errGenerico = *on;
335200100521           V1Dmsg = Msg(39);
335300100521
335400100521           leavesr;
335500100521           endif   ;
335600080624
335700080624        // Se quello che fondo ha anagrafiche o visite, avviso
335800100521  2     if Fondicpo<>forzacpo;
335900080624        forzacpo=fondicpo   ;
336000100521        clear  v1dmsg   ;
336100080624        setll fondicpo  cnaco16l  ;
336200100521
336300100805        clear tnta43ds  ;
336400100805        ita43cpo=Fondicpo   ;
336500100521
336600100805        TNTA43R (TNTA43DS)  ;
336700100805   3    if %equal(cnaco16l) or ota43nrv>0       ;
336800100805   4    select   ;
336900100521        when %equal(cnaco16l) and ota43nrv>0       ;
337000100521           V1Dmsg = Msg(41);
337100100521        when %equal(cnaco16l) ;
337200100521           V1Dmsg = Msg(32);
337300100521        when ota43nrv>0       ;
337400100521           V1Dmsg = Msg(40);
337500100805   4    endsl                 ;
337600100805   3    endif  ;
337700100521
337800100521
337900100521   3   if v1Dmsg<>*blanks    ;
338000100521   4   if v031tipf='F'  ;
338100080624           errtipoFus1 = *on;
338200080624       else  ;
338300080624           errtipoFus2 = *on;
338400100521   4   endif ;
338500080624           errMessage  = *on;
338600080624           errGenerico = *on;
338700080624           leavesr;
338800100521   3    endif   ;
338900100521
339000100521   2    endif   ;
339100100521
339200080604
339300080619        // Alloco i 2 record potenziali--> nessuno li deve aggiornare
339400120622       clear dCPO01;
339500080624       chain(n)  Fondicpo  TNCPO000;
339600080619       chain(e)  cp1nrr    TNCPO00F;
339700080619 2     if not %found(tncpo00F) or %error;
339800080619         if v032tipf='F'   ;
339900080619         errFusione = *on;
340000080619         else   ;
340100080619         errtipoFus1 = *on;
340200080619         endif  ;
340300080619
340400080619         errMessage  = *on;
340500080619         errGenerico = *on;
340600080619         if %error    ;
340700080619         V1Dmsg = Msg(30);
340800080619         else   ;
340900080619         V1Dmsg = Msg(31);
341000080619         endif   ;
341100080619       leavesr  ;
341200080619       endif    ;
341300120622
341400120622       //?Salvo la data primo contatto del Potenziale
341500120622       dCPO01 = CPOrst;
341600120702       IF  §CPOdtpcon > *zeros;
341700120622         DataPrimoConF = %dec(§CPOdtpcon:8:0);
341800120622       ELSE;
341900120622         clear DataPrimoConF;
342000120622       ENDIF;
342100080619
342200120622       clear dCPO01;
342300080619       chain(e)  Tienicpo  TNCPO000;
342400080619 2     if not %found(tncpo01l) or %error;
342500080619         if v032tipf='F'   ;
342600080619         errFusione = *on;
342700080619         else   ;
342800080619         errtipoFus1 = *on;
342900080619         endif  ;
343000080619
343100080619         errMessage  = *on;
343200080619         errGenerico = *on;
343300080619         if %error    ;
343400080619         V1Dmsg = Msg(30);
343500080619         else   ;
343600080619         V1Dmsg = Msg(31);
343700080619         endif   ;
343800080619       leavesr  ;
343900080619       endif   ;
344000120622
344100120622       //?Salvo la data primo contatto del Potenziale
344200120622       dCPO01 = CPOrst;
344300120702       IF  §CPOdtpcon > *zeros;
344400120622         DataPrimoConT = %dec(§CPOdtpcon:8:0);
344500120622       ELSE;
344600120622         clear DataPrimoConT;
344700120622       ENDIF;
344800080604
344900080619        // Se non ci sono errori carico i dati nel sfl
345000080619
345100080610        // C O N T A T T I
345200080604          exsr Carcontatti  ;
345300080610        // INFO COMM
345400080604          exsr Infocomm     ;
345500140923        // -?Dettaglio Prodotti?
345600140923          exsr  sr_DettProd;
345700140923
345800080610       // se non ho caricato nessuna informazione, emetto riga descrittiva
345900080610          if s03nrr=1     ;
346000081114          clear s03tipo   ;
346100080610          s03dec='Dati  non  presenti' ;
346200081009          exsr  RigaDESCR  ;
346300080610          endif   ;
346400080610
346500081009          SflDsp_N    = *off;
346600080606
346700080606 2        if v031tipf='T'   ;
346800080606          ColorRosso1=*on           ;
346900080606          else      ;
347000080606          ColorRosso2=*on           ;
347100080606 2        endif     ;
347200080609
347300080606 x2       else      ;
347400080606
347500080606        // Controlla scelte SFL   ;
347600080606         Exsr Contrsfl3           ;
347700080606 1       endif    ;
347800080604
347900080606       ENDSR;
348000100521       //--------------------------------------------------------------
348100100521       //?controlla scelte SFL 3
348200100521       //--------------------------------------------------------------
348300100521       BEGSR contrSFL3  ;
348400080606       clear ricint  ;
348500081013       clear wscelta ;
348600081010       s03nrr=1 ;
348700080609       clear keycont  ;
348800080606
348900081013       // Prima controllo la V= Visualizzazione estesa
349000080606        chain  s03nrr mk07s03  ;
349100080606 1      dow   %found            ;
349200080606
349300080606        clear updatesfl  ;
349400080606 2      if s031sce='V'        ;
349500080606
349600080606        // Visulizza info commerciali    ;
349700080609 3       if s03tipo='I'       ;
349800080929         CLEAR     TRMK50DS   ;
349900080929         fcpo=v031fcpo      ;
350000080929         ncpo=v031ncpo      ;
350100080929         i50cpo=kcpo ;
350200080929         i50mod='I'   ;
350300080929         i50rag=v031rag   ;
350400080929         i50pgm='TRMK07R' ;
350500080606
350600080929         TRMK50R  (kpjba:trmk50ds) ;
350700080609 3       endif    ;
350800080606
350900080606        // Visulizza contatti            ;
351000080609 3       if s03tipo='C'       ;
351100080606         clear tnta12ds   ;
351200080606         ta12ric='V'   ;
351300080606         ta12apl='P'   ;
351400080606         fcpo=v031fcpo  ;
351500080606         ncpo=v031ncpo  ;
351600080606         ta12pot=kcpo ;
351700080606         ta12rag=v031rag  ;
351800080606         TNTA12R  (kpjba:TNTA12ds);
351900080609 3       endif    ;
352000140923
352100140923        // -?Visualizzazione Dettaglio Prodotti?
352200140923         if  S03tipo = 'P';
352300140923           errMessage  = *on;
352400140923           errGenerico = *on;
352500140923           V1Dmsg = Msg(37);
352600140923           exsr UpdateSFL3;
352700140923           leavesr;
352800140923         endif;
352900080606
353000080606         clear    s031sce  ;
353100080606         Ricint='S'   ;
353200080606         Updatesfl='S'   ;
353300080606 2      endif             ;
353400080606
353500080606
353600080606 2      if s032sce='V' ;
353700080606
353800080606        // Visulizza info commerciali    ;
353900080609 3       if s03tipo='I'       ;
354000080929         CLEAR     TRMK50DS   ;
354100080929         fcpo=v032fcpo      ;
354200080929         ncpo=v032ncpo      ;
354300080929         i50cpo=kcpo ;
354400080929         i50mod='I'   ;
354500080929         i50rag=v031rag   ;
354600080929         i50pgm='TRMK07R' ;
354700080606
354800080929         TRMK50R  (kpjba:trmk50ds) ;
354900080609 3       endif   ;
355000080606
355100080606        // Visulizza contatti            ;
355200080609 3       if s03tipo='C'       ;
355300080606         clear tnta12ds   ;
355400080606         ta12ric='V'   ;
355500080606         ta12apl='P'   ;
355600080606         fcpo=v032fcpo  ;
355700080606         ncpo=v032ncpo  ;
355800080606         ta12pot=kcpo ;
355900080606         ta12rag=v032rag  ;
356000080606         TNTA12R  (kpjba:TNTA12ds);
356100080609 3       endif    ;
356200140923
356300140923        // -?Visualizzazione Dettaglio Prodotti?
356400140923         if  S03tipo = 'P';
356500140923           errMessage  = *on;
356600140923           errGenerico = *on;
356700140923           V1Dmsg = Msg(37);
356800140923           exsr UpdateSFL3;
356900140923           leavesr;
357000140923         endif;
357100080606
357200080606         clear    s032sce  ;
357300080606         Ricint='S'   ;
357400080929         Updatesfl='S'   ;
357500080606 2      endif             ;
357600080606
357700080606 2      if Updatesfl='S'             ;
357800080609        exsr UPDATESFL3  ;
357900080606 2      endif           ;
358000080606
358100080606        s03nrr=s03nrr+1 ;
358200080606        chain  s03nrr mk07s03  ;
358300080606 1      enddo    ;
358400080606
358500080606        // SE non ho richiesto la visulizzazione estesa
358600080606        //  eseguo i controlli di scelta
358700080606 1      if ricint=' '  ;
358800081010        s03nrr=1 ;
358900080606
359000080606        chain  s03nrr mk07s03  ;
359100080606 2      dow   %found            ;
359200080609
359300140923        // -?C O N T A T T I?
359400140923 2A     IF  S03TIPO = 'C';
359500081010
359600081010        // Se più di una riga ammessa             ;
359700080609 3      if s03righe>1   ;
359800080609
359900080609        // con sottotipo
360000080609 4      if s031nk2<>*blanks and s031sce='T';
360100080929        w007a=s03tpr+s031nk2 ;
360200080929        xx  =%lookup(w007a:keysfl3) ;
360300080609 5      if keycont(xx)=0;
360400080609           keycont(xx)=90 ;
360500080609 x5     else    ;
360600080609          errMessage  = *on;
360700080609          errGenerico = *on;
360800080609          V1Dmsg = Msg(24);
360900080609          %subst(v1dmsg:44:4)=%subst(s031cod:1:4)  ;
361000080609          %subst(v1dmsg:54:24)=s03dec               ;
361100080609 5      endif   ;
361200080609 4      endif   ;
361300080609 4      if s032nk2<>*blanks and s032sce='T';
361400080929        w007a=s03tpr+s032nk2 ;
361500080929        xx  =%lookup(w007a:keysfl3) ;
361600080609 5      if keycont(xx)=0;
361700080609           keycont(xx)=90 ;
361800080609 x5     else    ;
361900080609          errMessage  = *on;
362000080609          errGenerico = *on;
362100080609          V1Dmsg = Msg(24);
362200080609          %subst(v1dmsg:44:4)=%subst(s032cod:1:4)  ;
362300080609          %subst(v1dmsg:54:24)=s03dec               ;
362400080609 5      endif   ;
362500080609 4      endif   ;
362600080609
362700080617 4      if s031nk2= *blanks and s032nk2=*blanks ;
362800080929        w007a=s03tpr+'    '  ;
362900080929        xx  =%lookup(w007a:keysfl3) ;
363000080609 5      if xx>0    ;
363100080609
363200080609 6        if   s031sce='T'   ;
363300080609          keycont(xx)=keycont(xx)+1;
363400080609 6        endif   ;
363500080609
363600080609 6        if   s032sce='T'   ;
363700080609          keycont(xx)=keycont(xx)+1;
363800080609 6        endif                    ;
363900080609 5      endif     ;
364000080609
364100080609 5      if keycont(xx) > keyrig(xx)  ;
364200080609        errMessage  = *on;
364300080609        errGenerico = *on;
364400080609        V1Dmsg = Msg(22);
364500080609        %subst(v1dmsg:32:2)=%editc(s03righe:'X') ;
364600080609        %subst(v1dmsg:35:5)='righe'              ;
364700140923 6      select;
364800140923 6        when  S03tipo = 'C';
364900140923            %subst(v1dmsg:45:19)='la RUBRICA         ' ;
365000140923 6        when  S03tipo = 'I';
365100140923            %subst(v1dmsg:45:19)='la Info Commerciale' ;
365200140923 6        when  S03tipo = 'P';
365300140923            %subst(v1dmsg:45:19)='il Dettaglio Prod. ' ;
365400140923 6      endsl    ;
365500080609        v1dmsg =%trim(v1dmsg)+' ' +s03dec    ;
365600080609 5      endif     ;
365700080617
365800080617 x4     else      ;
365900080929           xx  =%lookup(s03tpr  :key3pot1) ;
366000080617            if xx=0    ;
366100080929             xx  =%lookup(s03tpr:key3pot2) ;
366200080617            endif      ;
366300080617
366400080617 5      if xx>0    ;
366500080617 6        if   s031sce='T'   ;
366600080617          keycont(xx)=keycont(xx)+1;
366700080617 6        endif   ;
366800080617
366900080617 6        if   s032sce='T'   ;
367000080617          keycont(xx)=keycont(xx)+1;
367100080617 6        endif                    ;
367200080617 5      endif                    ;
367300080617
367400080609 4      endif     ;
367500080609 3      endif     ;
367600080609
367700080609 3      if (s03righe=1 and s031sce='T' and s032sce='T' );
367800080609        errMessage  = *on;
367900080609        errGenerico = *on;
368000080609        V1Dmsg = Msg(22);
368100080609        %subst(v1dmsg:32:2)=%editc(s03righe:'X') ;
368200080609        %subst(v1dmsg:35:5)='riga '              ;
368300140923        select;
368400140923 4        when  S03tipo = 'C';
368500140923            %subst(v1dmsg:45:19)='la RUBRICA         ' ;
368600140923 4        when  S03tipo = 'I';
368700140923            %subst(v1dmsg:45:19)='la Info Commerciale' ;
368800140923 4        when  S03tipo = 'P';
368900140923            %subst(v1dmsg:45:19)='il Dettaglio Prod. ' ;
369000140923 4      endsl    ;
369100080609        v1dmsg =%trim(v1dmsg)+' '+s03dec    ;
369200080609 3      endif    ;
369300080609
369400080609 3      if s03righe=1 and s031sce=' ' and s032sce=' ' and s03forza=' ';
369500080609        errMessage  = *on;
369600080609        errGenerico = *on;
369700080609        V1Dmsg = Msg(23);
369800140923        select;
369900140923 4        when  S03tipo = 'C';
370000140923            %subst(v1dmsg:34:11)='la Rubrica '  ;
370100140923 4        when  S03tipo = 'I';
370200140923            %subst(v1dmsg:34:11)='la InfoComm' ;
370300140923 4        when  S03tipo = 'P';
370400140923            %subst(V1Dmsg:34:11)='il Dett.Pr.' ;
370500140923 4      endsl;
370600080609        %subst(v1dmsg:46:19)=s03dec   ;
370700080609 3      endif    ;
370800081013 2A     endif           ;
370900081013
371000140923        // -?I N F O   C O M M   e   D E T T A G L I O   P R O D .?
371100081013        //   Visualizzo tutto in giallo a seconda di dove è la T
371200140923 2A     IF  S03TIPO = 'I'  or  S03tipo = 'P';
371300081013        indInfo=*on      ;
371400081013
371500081013        // Riga di testa: controllo se c'e' la scelta
371600081013 3      if s031cod=*blanks and s032cod=*blanks;
371700081013
371800081013        // Se entrambe vuote o entrambe piene errore
371900081013 4         if (s031sce<>'T'  and s032sce<>'T') or
372000081013 4            (s031sce= 'T'  and s032sce= 'T') ;
372100081013            errMessage  = *on;
372200081013            errGenerico = *on;
372300081013            V1Dmsg = Msg(36);
372400081013 x4        else;
372500081013             // Memorizzo cosa scelto per evidenziarlo in giallo
372600081013 5           if s031sce='T' ;
372700081013              wscelta='1'    ;
372800081013             else;
372900081013              wscelta='2'    ;
373000081013 5           endif;
373100081013 4         endif    ;
373200081013 3         endif    ;
373300081013 2a        endif    ;
373400080609
373500080609        // Aggiorno SFL
373600080609        exsr UpdateSFL3  ;
373700080609
373800080609 3      if ErrGenerico=*on            ;
373900080609         leavesr;
374000080609 3      endif           ;
374100080609
374200080606        s03nrr=s03nrr+1 ;
374300080606        chain  s03nrr mk07s03  ;
374400080606 2      enddo    ;
374500080609
374600140923        // -?PER CONTATTI: controllo eventuali righe >1 senza scelta?
374700080609        xx=1   ;
374800081013 2      dow keysfl3(xx)<>*blanks  ;
374900080609
375000080609 3      if keynrr(XX) >0    ;
375100080609        s03nrr=keynrr(xx)  ;
375200080609        chain s03nrr   mk07s03   ;
375300080609
375400140923 4      if s03tipo='C' and  s03forza=' ' and keycont(xx)=0  ;
375500080609          errMessage  = *on;
375600080609          errGenerico = *on;
375700080609          V1Dmsg = Msg(23);
375800140923          select;
375900140923 5          when  S03tipo = 'C';
376000140923              %subst(v1dmsg:34:11)='la Rubrica ';
376100140923 5          when  S03tipo = 'I';
376200140923              %subst(v1dmsg:34:11)='la InfoComm';
376300140923 5          when  S03tipo = 'P';
376400140923              %subst(V1Dmsg:34:11)='il Dett.Pr.';
376500140923 5        endsl;
376600080609          %subst(v1dmsg:46:19)=s03dec   ;
376700080609
376800080609        // Aggiorno SFL
376900080609        exsr UpdateSFL3  ;
377000080609
377100080609 5      if ErrGenerico=*on            ;
377200080609         leavesr;
377300080609 5      endif           ;
377400080609 4      endif           ;
377500080609 3      endif           ;
377600080609
377700080609        xx=xx+1   ;
377800081013 2      enddo    ;
377900080609 1      endif    ;
378000080606
378100080606        ENDSR;
378200080609       //--------------------------------------------------------------
378300080609       //?Aggiorno SFL3
378400081013       //--------------------------------------------------------------
378500080609        BEGSR   UpdateSFL3  ;
378600080609
378700140923        //if s03tipo='I' and s031cod=*blanks and s032cod=*blanks ;
378800140923        if  (S03tipo = 'I'      or   S03tipo = 'P')  and
378900140923             S031cod = *blanks  and  S032cod = *blanks ;
379000081013          Testatahi=*on   ;
379100081013          Infoscelta1=in52  ;
379200081013          Infoscelta2=in53  ;
379300081013        else     ;
379400081013          noscelta1=in52  ;
379500081013          noscelta2=in53  ;
379600081013        endif    ;
379700081013
379800140923        if s03tipo='C' and noscelta1=*on and noscelta2=*on  ;
379900081013          Testatahi=*on   ;
380000081013        endif    ;
380100081013
380200081013        if (s03tipo='C' and s031sce='T')  or
380300140923           (S03tipo='P' and wScelta='1')  or
380400081013           (s03tipo='I' and wscelta='1')   ;
380500140923          sflrosso1=*on   ;
380600140923          clear s03forza  ;
380700081013          if Testatahi  =*off  ;
380800081013            s031sce='T'     ;
380900081013          endif             ;
381000080609        else  ;
381100140923          sflrosso1=*off  ;
381200081013          if Testatahi  =*off  ;
381300081013            clear s031sce   ;
381400081013          endif             ;
381500080609        endif ;
381600080609
381700081013        if (s03tipo='C' and s032sce='T')  or
381800140923           (S03tipo='P' and wScelta='2')  or
381900081013           (s03tipo='I' and wscelta='2')   ;
382000140923          sflrosso2=*on   ;
382100140923          clear s03forza  ;
382200081013          if Testatahi  =*off  ;
382300081013            s032sce='T'     ;
382400081013          endif             ;
382500080609        else  ;
382600140923          sflrosso2=*off  ;
382700081013          if Testatahi  =*off  ;
382800081013            clear s032sce   ;
382900081013          endif             ;
383000080609        endif ;
383100080609
383200081013
383300080609        // solo se c'e' errore
383400080609          if ErrGenerico=*on   ;
383500080609           c03rcd=s03nrr  ;
383600080609
383700080609            if noscelta1=*off  ;
383800080609            errsfl3_1=*on     ;
383900080609            else    ;
384000080609            errsfl3_2=*on     ;
384100080609            endif             ;
384200080609          endif             ;
384300080609
384400080609        update mk07s03  ;
384500080609
384600080609        Testatahi=*off  ;
384700081013        IndInfo  =*off  ;
384800080609        errSFL3_1= *off;
384900080609        errSFL3_2= *off;
385000080609        sflrosso1= *off;
385100080609        sflrosso2= *off;
385200081013        noscelta1= *off;
385300081013        noscelta2= *off;
385400081013        Infoscelta1= *off;
385500081013        Infoscelta2= *off;
385600080619       ENDSR  ;
385700080619       //--------------------------------------------------------------
385800080619       //?Conferma FUSIONE
385900080619       //--------------------------------------------------------------
386000080619       BEGSR ConfFUSIONE  ;
386100080619       clear Wscrivi   ;
386200100525       clear TieniconTrCd  ;
386300100525       clear FondiAttB     ;
386400100525       clear FondiAttI     ;
386500100525       clear TieniAtt      ;
386600100525
386700100525       // Controllo se potenziale  che  TENGO ha una trattativa
386800100525           clear tnta43ds  ;
386900100525           ita43cpo=Tienicpo   ;
387000100525
387100100525           TNTA43R (TNTA43DS)  ;
387200100525           if ota43nrv>0 or ota43err<>' '   ;
387300100525             TieniconTrCD='S'   ;
387400100525
387500100525           else   ;
387600100525        setll   TieniCPO  cnaco16l;
387700100525         if %equal(cnaco16l)  ;
387800100525           TieniconTrCd='S'    ;
387900100525         endif  ;
388000100525         endif  ;
388100080619
388200080619        clear knk1  ;
388300080619        knk1 =%editc(TieniCPO:'X') ;
388400160901
388500160901      //?Cerco i dati del codice che tengo per memorizzare
388600160901      //?l'immagine precedente
388700160901        chain(n) (TieniCPO:'SPT') TICPI01L;
388800160901      //?Salvo l'immagine Precedente
388900160901        exsr ImmaginePrimaF;
389000080619
389100140923        // -?Lettura SFL per aggiornare CONTATTI, INFO e DETT.PROD.?
389200080619
389300080619        // 1° giro: deleto i record del pot che tengo, se manca la T
389400081013        s03nrr=1 ;
389500080619        chain  s03nrr mk07s03  ;
389600080619 1      dow   %found            ;
389700081013
389800081013        // Escludo i record di testata
389900081013 1a     if s031cod<>*blanks or s032cod<>*blanks  ;
390000080619
390100080620 2      if (v031tipf='T' and s031sce<>'T'and in52='0')  or
390200080620           (v032tipf='T' and s032sce<>'T'and in53='0')   ;
390300080619
390400080929         ktnt=%subst(s03tpr:1:2)  ;
390500080929         ktprI=%subst(s03tpr:1:3)  ;
390600080619         clear knk2  ;
390700080619 3       if v031tipf='T' and s031nk2<>*blanks    ;
390800080619         knk2=s031nk2  ;
390900080619 3       endif   ;
391000080619 3       if v032tipf='T' and s032nk2<>*blanks    ;
391100080619         knk2=s032nk2  ;
391200080619 3       endif   ;
391300080619
391400080619        // 1 riga
391500080619 3        if s03righe=1 or (s03tipo='I' and knk2<>*blanks) ;
391600140923 4         if  S03tipo = 'C'  or  S03tipo = 'P';
391700080619             delete ('P':knk1:knk2:ktnt) TFNTC01l    ;
391800080619             else   ;
391900080929             delete (TIENIcpo:ktprI:knk2) TiCPI01l    ;
392000080619 4           endif  ;
392100080619 x3       else    ;
392200080619
392300140923 4        if  S03tipo = 'C'  or  S03tipo = 'P';
392400080619        // loop per + righe e cerco quello uguale  SOLO PER CONTATTI
392500140923        //    e per DETTAGLIO PRODOTTI (anche se - PER ORA - è
392600140923        //    previsto un solo record)
392700080619           setll  ('P':knk1:knk2:ktnt) TFNTC01l    ;
392800080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
392900080619 5         dow not %eof (tfntc01l)    ;
393000080619 6          if (v031tipf='T' and %subst(ntcrnt:1:21) =s031cod )  or
393100080619               (v032tipf='T' and %subst(ntcrnt:1:21) =s032cod )  ;
393200080619               delete tfntc   ;
393300080619 6          endif    ;
393400080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
393500080619 5         enddo   ;
393600080619 4         endif  ;
393700080619
393800080619 3      endif   ;
393900080619 2      endif   ;
394000081013 1a     endif   ;
394100080619
394200080619        s03nrr=s03nrr+1  ;
394300080619        chain  s03nrr mk07s03  ;
394400080619        enddo  ;
394500080619
394600080619        // 2° giro: Aggiungo quello dell'altro potenziale
394700081013        s03nrr=1 ;
394800080619        chain  s03nrr mk07s03  ;
394900080619 1      dow   %found            ;
395000080619
395100081013
395200081013        // Escludo i record di testata
395300081013 1a     if s031cod<>*blanks or s032cod<>*blanks  ;
395400080620 2      if (v031tipf='T' and s032sce='T'and in53='0')  or
395500080620           (v032tipf='T' and s031sce='T'and in52='0')   ;
395600080619
395700080929         ktnt=%subst(s03tpr:1:2)  ;
395800080929         ktprI=%subst(s03tpr:1:3)  ;
395900080619         clear knk2  ;
396000080619 3       if v031tipf='T' and s032nk2<>*blanks    ;
396100080619         knk2=s032nk2  ;
396200080619 3       endif   ;
396300080619 3       if v032tipf='T' and s031nk2<>*blanks    ;
396400080619         knk2=s031nk2  ;
396500080619 3       endif   ;
396600080619
396700080619        // 1 riga
396800080619 3        if s03righe=1 or (s03tipo='I' and knk2<>*blanks) ;
396900140923 4          if  S03tipo = 'C'  or  S03tipo = 'P';
397000080619              knk1 =%editc(FondiCPO:'X') ;
397100080619              chain  ('P':knk1:knk2:ktnt) TFNTC01l    ;
397200080619              ntcnk1=%editc(Tienicpo:'X')   ;
397300080619              update tfntc ;
397400080619 x4          else    ;
397500080929              chain  (FondiCPO:ktprI:knk2) TiCPI01l    ;
397600080929              cpicpo=Tienicpo        ;
397700080929              update ticpi000 ;
397800080619 4           endif   ;
397900080619 x3       else    ;
398000080619
398100140923 4        if  S03tipo = 'C'  or  S03tipo = 'P';
398200080619        // loop per + righe e cerco quello uguale  SOLO PER CONTATTI
398300140923        //    e per DETTAGLIO PRODOTTI (anche se - PER ORA - è
398400140923        //    previsto un solo record)
398500080619           knk1 =%editc(FondiCPO:'X') ;
398600080619           setll  ('P':knk1:knk2:ktnt) TFNTC01l    ;
398700080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
398800080619 5         dow not %eof (tfntc01l)    ;
398900080619 6          if (v031tipf='T' and %subst(ntcrnt:1:21) =s032cod )  or
399000080619               (v032tipf='T' and %subst(ntcrnt:1:21) =s031cod )  ;
399100080619               ntcnk1=%editc(Tienicpo:'X')   ;
399200080619               update tfntc   ;
399300080619 6          endif    ;
399400080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
399500080619 5         enddo   ;
399600080619 4         endif  ;
399700080619
399800080619 3      endif   ;
399900080619 2      endif   ;
400000081013 1a     endif   ;
400100080619
400200080619        s03nrr=s03nrr+1  ;
400300080619        chain  s03nrr mk07s03  ;
400400080619 1      enddo  ;
400500160901
400600100525
400700100525        // 3° giro : cerco CONTATTI BATCH su pot FUSO/ da Tenere
400800100525
400900100525        setll (fondicpo: 0:0:0) tiatc04l     ;
401000100525        reade (fondicpo: 0:0:0) tiatc04l     ;
401100100525        dow   not %eof(tiatc04l)   ;
401200100525
401300100525        if atcpri<>'BATCH'    ;
401400100525        w0030= %int(%subst(%editc(atccmm:'X') :1:3))  ;
401500100525        Indx =%lookup(w0030:pog)   ;
401600100525        endif   ;
401700100525
401800100525        if atcpri='BATCH' or Indx>0 ;
401900100525        // se potenziale che tengo ha trattativa o ksc e attitivà mia o batch, deleto subito
402000100525          if TieniconTrCd='S'   ;
402100100525            exsr CanceAtt   ;
402200100525          else   ;
402300100525          // Memorizzo che c'e'
402400100525            if atcpri='BATCH' ;
402500100525             FondiAttB='S'   ;
402600100525             else  ;
402700100525             FondiAttI='S'   ;
402800100525            endif  ;
402900100525          endif  ;
403000100525        else   ;
403100100525          if fondiAttI=' '   ;
403200100525            FondiAttI='N'   ;
403300100525          endif  ;
403400100525        endif  ;
403500100525
403600100525        reade (fondicpo: 0:0:0) tiatc04l     ;
403700100525        enddo   ;
403800100525
403900100525        // Se esiste una BATCH  o mia e Tieni non ha trattativa ;
404000100525        //  verifico cosa devo cancellare
404100100525        if TieniconTrCd=' ' and (FondiattI='S' or FondiAttB='S');
404200100525        setll (Tienicpo: 0:0:0) tiatc04l     ;
404300100525        reade (Tienicpo: 0:0:0) tiatc04l     ;
404400100525        dow   not %eof(tiatc04l)   ;
404500100525
404600100525        if atcpri= 'BATCH' and FondiAttI<>' '  ;
404700100525            exsr CanceAtt   ;
404800100525            else   ;
404900100525            TieniAtt ='S'   ;
405000100525        endif   ;
405100100525
405200100525        reade (Tienicpo: 0:0:0) tiatc04l     ;
405300100525        enddo  ;
405400100525        endif   ;
405500100525
405600100525        // Se quello che tengo ha attività e il Fondi ne ha una BATCH --> la deleto
405700100525        if TieniAtt='S' and fondiAttB='S'    ;
405800100525        setll (fondicpo: 0:0:0) tiatc04l     ;
405900100525        reade (fondicpo: 0:0:0) tiatc04l     ;
406000100525        dow   not %eof(tiatc04l)   ;
406100100525        if atcpri='BATCH'   ;
406200100525            exsr CanceAtt   ;
406300100525        endif   ;
406400100525
406500100525        reade (fondicpo: 0:0:0) tiatc04l     ;
406600100525        enddo   ;
406700100525        endif   ;
406800100525
406900100525        //  Aggiorno TUTTE LE ATTIVITA' RIMASTE
407000100525
407100111013        exec sql  update tiatc00f set atccpo=:tienicpo, atctas = ' '
407200111013                  where atccpo=:fondicpo;
407300100521
407400100525        // 5° giro : copio tutte le note interne di quello che fondo
407500100525        //          in quello da tenere DEL NUOVO FILE  NOTE
407600100521          exsr NewNote   ;
407700080619
407800100525        // 6° giro: DELETO i record rimasti di quello da fondere
407900080619        knk1 =%editc(FondiCPO:'X') ;
408000080619        clear knk2  ;
408100080619        setll  ('P':knk1) TFNTC01l    ;
408200080619        reade  ('P':knk1) TFNTC01l    ;
408300080619        dow not %eof (tfntc01l)    ;
408400080619          delete tfntc ;
408500080619          reade  ('P':knk1) TFNTC01l    ;
408600080619        enddo    ;
408700080929        setll  (FondiCPO) TiCPI01l    ;
408800080929        reade  (fondiCPO) TiCPI01l    ;
408900080929        dow not %eof (ticpi01l)    ;
409000080929          delete ticpi000 ;
409100080929        reade  (fondiCPO) TiCPI01l    ;
409200080619        enddo    ;
409300160901        setll (FondiCPO) TNCPVT1L;
409400160901        reade (FondiCPO) TNCPVT1L;
409500160901        DOW  not %eof (TNCPVT1L);
409600160901          delete TNCPVT00;
409700160901          reade (FondiCPO) TNCPVT1L;
409800160901        ENDDO;
409900160901        setll (FondiCPO) TNCPVD1L;
410000160901        reade (FondiCPO) TNCPVD1L;
410100160901        DOW  not %eof (TNCPVD1L);
410200160901          delete TNCPVD00;
410300160901          reade (FondiCPO) TNCPVD1L;
410400160901        ENDDO;
410500121018
410600121018       //?8° giro aggiorno potenziale sul file storico CRM del potenziale FUSO
410700121018       //?        imposto il potenziale DA TENERE
410800130215        exec sql
410900130215          UPDATE TICRM00F set CRMcpo = :Tienicpo
411000130215          WHERE  CRMcpo = :Fondicpo;
411100080619
411200100525        // 9° giro : aggiorno cod potenziale su eventuali ANAGRAFICHE
411300080624        //           che hanno il codice vecchio
411400080624        setll    fondicpo cnaco16l  ;
411500080624        reade(E) fondicpo cnaco16l  ;
411600080624        dow not %eof(cnaco16l) and not %error  ;
411700080624        clear acodtr             ;
411800080624        clear acoftr             ;
411900080624        Dataiso =%date(*date:*iso)  ;
412000080624        Dataymd =Dataiso;
412100080624        acoduv=%dec(Dataymd);
412200080624        acolib=tienicpo          ;
412300080624        update cnaco000          ;
412400080624
412500080624        reade(E) fondicpo cnaco16l  ;
412600080624        enddo                    ;
412700080624
412800080624        setll    fondicpo tfaco16l  ;
412900080624        reade(E) fondicpo tfaco16l  ;
413000080624        dow not %eof(tfaco16l) and not %error  ;
413100080624        clear acodtr             ;
413200080624        clear acoftr             ;
413300080624        Dataiso =%date(*date:*iso)  ;
413400080624        Dataymd =Dataiso;
413500080624        acoduv=%dec(Dataymd);
413600080624        acolib=tienicpo          ;
413700080624        update tfaco000          ;
413800080624
413900080624        reade(E) fondicpo tfaco16l  ;
414000080624        enddo                    ;
414100100525
414200100525        //10° giro : aggiorno TRATTATIVE --> TIVIS
414300100525        exec sql  update tivis00f set viscpo=:Tienicpo where viscpo=:Fondicpo;
414400140715
414500140715        //?11° giro: aggiorno ESTENSIONE anagrafica POTENZIALE?
414600140715
414700140918        // -?Salvataggio e Cancellazione dati potenziale da Fondere?
414800140918        clear  SavCPO1ds;
414900140715        chain  FondiCPO  TNCPO100;
415000140715        if  %found(TNCPO11L);
415100140918          SavCPO1ds = TNCPO1ds;
415200140715          DELETE  TNCPO100;
415300140715        endif;
415400140918
415500140918        // -?Aggiornamento dati potenziale da Tenere?
415600140715        chain  TieniCPO  TNCPO100;
415700140918        if  NOT %found(TNCPO11L);
415800140918          clear  TNCPO100;
415900140918          CPO1cpo = TieniCPO;
416000140918        endif;
416100140918
416200140918        // -?Telefono?
416300140715        Select;
416400140715          When  %found(TNCPO11L)  and  CPO1tel = *blanks
416500140918                                  and  X_CPO1tel <> *blanks;
416600140918            CPO1tel = X_CPO1tel;
416700140918          When  NOT %found(TNCPO11L)  and  X_CPO1tel <> *blanks;
416800140918            CPO1tel = X_CPO1tel;
416900140715        EndSl;
417000140918
417100140918        // -?Fax?
417200140918
417300140918        // -?Data Ultima Conferma INFO Commerciali?
417400140919        if  (V031tipF = 'T'  and  wScelta = '2')  or
417500140919            (V032tipF = 'T'  and  wScelta = '1');
417600140919          CPO1dUCifo = X_CPO1dUCifo;
417700140919        endif;
417800140918
417900140918        // -?CRIF Number?
418000140918        if  X_CPO1crifN  <> *blank  and
418100140918            (CPO1crifN   =  *blank  or
418200140918             F_CPOfsf    =  'S');
418300140918          CPO1crifN  = X_CPO1crifN;
418400140918        endif;
418500140918
418600140918        // -?CRIF Number Casa Madre?
418700140918        if  X_CPO1crifNM <> *blank  and
418800140918            (CPO1crifNM  =  *blank  or
418900140918             F_CPOfsf    =  'S');
419000140918          CPO1crifNM = X_CPO1crifNM;
419100140918        endif;
419200140918
419300140918        // -?Flag Operativi?
419400140918
419500140918        // -?Aggiornamento rec.?
419600140918        Select;
419700140918          When  %found(TNCPO11L);
419800140918            UPDATE  TNCPO100;
419900140918          When  NOT %found(TNCPO11L)  and
420000140918               (CPO1tel    <> *blank  or
420100140918                CPO1dUCIfO <> *zero   or
420200140918                CPO1crifN  <> *blank  or
420300140918                CPO1crifNM <> *blank);
420400140918            WRITE  TNCPO100;
420500140918          Other;
420600140918            UNLOCK  TNCPO11L;
420700140918        EndSl;
420800080624
420900140715        //?12° giro: aggiorno anagrafica POTENZIALE?
421000080619
421100140918        // -?Anno Inizio Attività (EX TELEFONO - vedi dCPO02)?
421200140918        dCPO02 = CPOtel;
421300140918        if  %subst( F_CPOtel : 1 : %size(dCPO02.§CPOannoIA) )
421400140918                               >  *zero  and
421500140918            (dCPO02.§CPOannoIA <= *zero  or
421600140918             F_CPOfsf          =  'S');
421700140918          dCPO02.§CPOannoIA = %subst( F_CPOtel : 1 :
421800140918                                    %size(dCPO02.§CPOannoIA) );
421900140918          CPOtel = dCPO02;
422000140918        endif;
422100140918
422200140918        // -?piv e cdf?
422300080619        if cpopiv=*blanks and cpocdf=*blanks   ;
422400080619        cpopiv=F_cpopiv     ;
422500080619        cpocdf=F_cpocdf     ;
422600080619        endif               ;
422700080624
422800080624
422900140918        // -?cod commerciale?
423000080624        if cpocmm=0   and f_cpocmm>0       ;
423100080624        cpocmm=F_cpocmm     ;
423200080624        endif               ;
423300080619
423400140918        // -?dati D&B   tengo quelli che li ha o il pot SEDE se entrambi li hanno?
423500140918        //  ?data primo contatto, tengo la data più vecchia tra i 2 potenziali
423600090520        dcpo01=cporst       ;
423700090520
423800090520        if F_cpoftaz>0;
423900090520        if cpoftaz=0  or  f_cpofsf='S'   ;
424000080619        cpoftaz=F_cpoftaz   ;
424100080619        cpoafaz=F_cpoafaz   ;
424200080619        cpoffaz=F_cpoffaz   ;
424300080619        cpopexp = F_cpopexp ;
424400090520          // Memorizzo anche il fatturato presunto
424500090520          if %subst(f_cporst:24:11)>*zeros   ;
424600090520            §cposptp=%int(%subst(f_cporst:24:11)) ;
424700090520          endif   ;
424800080619        endif               ;
424900090520        endif               ;
425000120622
425100120622        //?data primo contatto
425200120622        SELECT;
425300120622          WHEN  (DataPrimoConT > 0 and
425400120622                 DataPrimoConT < DataPrimoConF) or
425500120622                 DataPrimoConF = 0;
425600120622            §CPOdtpcon = %editc(DataPrimoConT:'X');
425700120622          WHEN  (DataPrimoConF > 0 and
425800120622                 DataPrimoConF < DataPrimoConT) or
425900120622                 DataPrimoConT = 0;
426000120622            §CPOdtpcon = %editc(DataPrimoConF:'X');
426100120622        ENDSL;
426200140918
426300140918        // -?Data Ultimo Aggiornamento INFO Commerciali?
426400140919        if  (V031tipF = 'T'  and  wScelta = '2')  or
426500140919            (V032tipF = 'T'  and  wScelta = '1');
426600140919          CPO1dUCifo = X_CPO1dUCifo;
426700140919          §CPOifoDUl = %subst( F_CPOrst : 5 : %size(§CPOifoDUl) );
426800140919        endif;
426900140918
427000140918        // -?Flag Problemi Finanziari (1° byte di CPOCLT)?
427100140918        if  %subst( F_CPOclt : 1 : 1 ) <> *blank  and
427200140918            ( %subst( CPOclt : 1 : 1 ) =  *blank  or
427300140918              F_CPOfsf               =  'S' );
427400140918          %subst( CPOclt : 1 : 1 ) = %subst( F_CPOclt : 1 : 1 );
427500140918        endif;
427600080619
427700140918        // -?Sede/Filiale?
427800080619        if cpofsf='F' and F_cpofsf='S'  ;
427900080619        cpofsf='S'          ;
428000080619        endif               ;
428100080619
428200140918        // -?codice  duns?
428300080619        if cpoduns=*blanks or (F_cpoduns<>*blanks and F_cpofsf='S') ;
428400080619        cpoduns=F_cpoduns   ;
428500080619        endif               ;
428600110304
428700110304       //?Ricalcolo la categoria del potenziale
428800110304        clear trmk05ds;
428900110304        IMK05cpo = CPOcpo;
429000110304        trmk05r (kpjba : TRMK05DS);
429100110304        IF  OMK05err = *blanks;
429200110304          CPOfls = OMK05cat;
429300110304        ENDIF;
429400080619
429500080619        cpodtr=*date        ;
429600080619        §cporicfus='S'      ;
429700080619        cporst=dcpo01       ;
429800080619        update tncpo000     ;
429900080619
430000080619        delete tncpofis     ;
430100090520
430200140918        // -?Aggiorno potenziale per i dati contenuti in CPORST?
430300090521        alfacpo=%editc(cpocpo:'X')      ;
430400090521        callp   TRMK08R   (kpjba:alfacpo)  ;
430500090521
430600160901      //?Cerco i dati del codice che tengo per memorizzare
430700160902      //?l'immagine dopo
430800160901        chain(n) (TieniCPO:'SPT') TICPI01L;
430900160902        chain(n) (TieniCPO) TNCPO01L;
431000160901      //?Scrivo la variazione
431100160901        exsr ScriviVariazioneF;
431200160901
431300160901      //?Scrivo una riga per memorizzare il codice potenziale Fuso
431400160901        exsr ScriviFuso;
431500080619
431600140918         // -?Imposto il posizionamento cursore per il sfl che emetto?
431700140918         //  ?aggiornato?
431800080619         if nrrFUS(1)> 0 ;
431900080619           C02csr=nrrFUS(1) ;
432000080619           pagric=(c02csr/C_sflPag)+1 ;
432100080619           $Video = 'S2';
432200080619           $InzS02 = *on;
432300080619         else           ;
432400080619           $Video = 'D1';
432500080619           $InzD01 = *on;
432600080619         endif    ;
432700080619
432800080619       ENDSR  ;
432900100525       //--------------------------------------------------------------
433000100525       //?cancello attività della trascodifica
433100100525       //--------------------------------------------------------------
433200100525       BEGSR CanceAtt   ;
433300100525
433400100525        // cancello  eventuale legame
433500100525          delete (atcatn:atcatnp) tiatl01l   ;
433600100525
433700100525        // cancello  eventuali note
433800100525
433900100525         cpncpo = atccpo ;
434000100525         cpntat = atctat ;
434100100525         cpnatn = atcatn ;
434200100525         cpnatnp= 0 ;
434300100525         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
434400100525         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
434500100525         dow not %eof(ticpn03l);
434600100525             delete ticpn000;
434700100525             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
434800100525         enddo ;
434900100525
435000100525       delete   tiatc000   ;
435100100525
435200100525       ENDSR  ;
435300100521       //--------------------------------------------------------------
435400100521       //?Scrivo le note interne nuovo file sl pot che tengo
435500100521       //--------------------------------------------------------------
435600100521       BEGSR Newnote;
435700100521
435800100524        exec sql declare A1 cursor for
435900100524                 select  * from ticpn04l where cpncpo= :Fondicpo
436000100524                 order by cpncpo, cpndim, cpnhim   ;
436100100524        exec sql open  A1;
436200100524        exec sql fetch next from A1 into :ticpnds ;
436300100524
436400100524        dow  sqlcod = 0;
436500100524
436600100524        Totnot=10 ;
436700100524
436800100524        // Verifico se c'e' già un nota con data e ora per il potenziale da tenere
436900100524
437000100524       dow Totnot>0  and cpnpno=1 ;
437100100524       exec sql  select count(cpncpo) into :Totnot from ticpn04l where
437200100524                 cpncpo= :Tienicpo  and cpndim= :cpndim and cpnhim= :cpnhim  ;
437300100524
437400100524        if  totnot>0   ;
437500100524         OraHMS=%time(cpnhim)  ;
437600100524         OraHMS=oraHMS- %minutes(10) ;
437700100524         cpnhim=%dec(OraHMS)  ;
437800100524        else  ;
437900100524         newhim=cpnhim    ;
438000100524        endif  ;
438100100524        enddo  ;
438200100524
438300100524        cpnhim= newhim     ;
438400100525        cpncpo= Tienicpo   ;
438500100524        write ticpn000  ;
438600100524
438700100525        exec sql fetch next from A1 into :ticpnds ;
438800100524        enddo    ;
438900100524
439000100524        exec sql close A1;
439100100524
439200100524        exec sql delete from ticpn04l where cpncpo= :Fondicpo    ;
439300100524
439400100521       ENDSR   ;
439500160901
439600160901       //--------------------------------------------------------------
439700160901       //?Scrivo l'immagine Precedente anagrafica Potenziale
439800160901       //--------------------------------------------------------------
439900160901       BEGSR ImmaginePrima;
440000160901
440100160901         clear TRMK30DS;
440200160901         tncpo1_30 = TNCPO1ds;
440300160901         IMK30immag = 'P';
440400160901         IMK30tvp = 'B';
440500160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
440600160901
440700160901       ENDSR;
440800160901
440900160901       //--------------------------------------------------------------
441000160901       //?Scrivo Storico Variazioni.
441100160901       //--------------------------------------------------------------
441200160901       BEGSR ScriviVariazione;
441300160901
441400160901         clear TRMK30DS;
441500160901         tncpo1_30 = TNCPO1ds;
441600160901         IMK30pru = knmus;
441700160901         IMK30noj = knmeb;
441800160901         IMK30pgm = 'TRMK07R';
441900160901         IMK30immag = 'D';
442000160901         IMK30cta = 'M';
442100160901         IMK30tvp = 'B';
442200160901
442300160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
442400160901
442500160901       ENDSR;
442600160901
442700160901       //--------------------------------------------------------------
442800160901       //?Scrivo Riga per Memorizzare il codice Sede di Unione.
442900160901       //--------------------------------------------------------------
443000160901       BEGSR ScriviSede;
443100160901
443200160901         clear TNCPVT00;
443300160901         clear TNCPVD00;
443400160901
443500160901         clear dCPV_G;
443600160901         gCPOcpo = V01sfcpo * 100000000 + V01sncpo;
443700160901
443800160901         CPVcpo = CPOcpo;
443900160901         CPVflo = 'G';
444000160901         CPVcta = 'M';
444100160901         CPVpgm = 'TRMK07R';
444200160901         CPVdav = %dec(%date());
444300160901         CPVorv = %dec(%time());
444400160901         CPVorv += 1;
444500160901         CPVpru = knmus;
444600160901         CPVnoj = knmeb;
444700160901         write TNCPVT00;
444800160901
444900160901         CPVdopo = dCPV_G;
445000160901         CPVtvp = 'G';
445100160901         write TNCPVD00;
445200160901
445300160901       ENDSR;
445400160901
445500160901       //--------------------------------------------------------------
445600160901       //?Scrivo l'immagine Precedente anagrafica Potenziale Fusione
445700160901       //--------------------------------------------------------------
445800160901       BEGSR ImmaginePrimaF;
445900160901
446000160901         clear TRMK30DS;
446100160901         tncpo1_30 = TNCPO1ds;
446200160901         IMK30immag = 'P';
446300160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
446400160901
446500160901       ENDSR;
446600160901
446700160901       //--------------------------------------------------------------
446800160901       //?Scrivo Storico Variazioni Fusione
446900160901       //--------------------------------------------------------------
447000160901       BEGSR ScriviVariazioneF;
447100160901
447200160901         clear TRMK30DS;
447300160901         tncpo1_30 = TNCPO1ds;
447400160901         IMK30pru = knmus;
447500160901         IMK30noj = knmeb;
447600160901         IMK30pgm = 'TRMK07R';
447700160901         IMK30immag = 'D';
447800160901         IMK30cta = 'M';
447900160901
448000160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
448100160901
448200160901       ENDSR;
448300160901
448400160901       //--------------------------------------------------------------
448500160901       //?Scrivo Riga per Memorizzare il codice Fuso.
448600160901       //--------------------------------------------------------------
448700160901       BEGSR ScriviFuso;
448800160901
448900160901         clear TNCPVT00;
449000160901         clear TNCPVD00;
449100160901
449200160901         clear dCPV_F;
449300160901         fCPOcpo = FondiCPO;
449400160901
449500160901         CPVcpo = CPOcpo;
449600160901         CPVflo = 'F';
449700160901         CPVcta = 'M';
449800160901         CPVpgm = 'TRMK07R';
449900160901         CPVdav = %dec(%date());
450000160901         CPVorv = %dec(%time());
450100160901         CPVorv += 1;
450200160901         CPVpru = knmus;
450300160901         CPVnoj = knmeb;
450400160901         write TNCPVT00;
450500160901
450600160901         CPVdopo = dCPV_F;
450700160901         CPVtvp = 'F';
450800160901         write TNCPVD00;
450900160901
451000160901       ENDSR;
451100160901
451200080206       //--------------------------------------------------------------
451300080206       //?Operazioni finali.
451400080206       //--------------------------------------------------------------
451500080206       BEGSR RoutEnd;
451600080410
451700080206         *inLR = *on;
451800080206         return;
451900080206
452000080206       ENDSR;
452100080206
452200080206      /end-free
452300080206
452400080206       //--------------------------------------------------------------
452500080206       //?Schiere a tempo di compilazione.
452600080206       //--------------------------------------------------------------
452700080206
452800080410** - MSG ------------------------------------------------------------------ -*
452900080411Utente non abilitato alla gestione dei potenziali!!!                           01
453000080523Effettuare almeno una scelta                                                   02
453100080618Codice potenziale SEDE  errato o non selezionato                               03
453200080603Impossibile fare FUSIONE di 2 potenziali SEDE                                  04
453300111115Impossibile fare FUSIONE di 2 potenziali in categoria "Eliminabile"            05
453400080523Immettere almeno un codice potenziale FILIALE                                  06
453500111115Categoria potenziale errata                                                    07
453600080411Filiale appartenenza errata                                                    08
453700080414Non è stato trovato nessun potenziale per le selezioni immesse                 09
453800080415Caricamento dati non completabile: selezionati troppi dati!                    10
453900140923Se immessa la provincia, immettere anche la ragione sociale                    11 LIBERO
454000080415Provincia errata                                                               12
454100080415Selezionare soltanto una SEDE                                                  13
454200080523Impossibile selezionare più di 7 FILIALI per l'Unione                          14
454300080526Impossibile Unione di una SEDE senza partiva iva e codice fiscale              15
454400140923Pot.xxxxxxxxxxx-yyyyyyyyyyyy non selezionabile:già presente la SEDE x l'Unione 16
454500080526Filiale con partita IVA e/o cod.Fiscale diversi dalla SEDE-Enter Forza         17
454600080603Per la FUSIONE selezionare soltanto 2 potenziali !                             18
454700140923Pot.xxxxxxxxxxx-yyyyyyyyyyyy non selezionabile:già due codici per la Fusione   19
454800080604E' stato selezionato lo stesso codice potenziale!!                             20
454900080609Selezionare il potenziale da TENERE e quello da FONDERE!!!                     21
455000080609Impossibile selezionare più di xx xxxxx per                                    22
455100080617Nessuna selezione effettuata per YYYYYYYYYYY xxxxxxxxxxxxxxxxxxx               23
455200080609Impossibile selezionare 2 volte il codice "xxxx" per xxxxxx                    24
455300111115Il potenziale in cat. "Eliminabile" deve essere "F U S O"  !!                  25
455400080618Utente non abilitato all'aggiornamento del potenziale                          26
455500080618Codice potenziale  errato o non selezionato                                    27
455600140923Utente non abilitato a FONDERE il potenz.:presenti AZIONI nell'anno            28 LIBERO
455700081010Utente non abilitato all'aggiornamento dei 2 potenz.:FUSIONE NON EFFETTUABILE  29
455800080619Potenziale allocato da altro utente:controllare e riprovare!                   30
455900081009Potenziale non più presente in anagrafica: già normalizzato!!??!!              31
456000100521Presenti ANAGRAFICHE sul potenziale da Fondere: saranno aggiornate             32
456100140923                                                                                  LIBERO
456200140923                                                                                  LIBERO
456300081013Categ.Merceologica del potenziale da TENERE errata: correggerla con "M-Manut." 35
456400100419Scegliere le Info Commerciali da TENERE impostando una sola "T"                36
456500140923Dettaglio Prodotti NON visualizzabile                                          37
456600140923                                                                                  LIBERO
456700100525Fusione NON EFFETTUABILE:trattativa aperta per il potenziale da Fondere        39
456800100521Presenti TRATTATIVE sul potenziale da Fondere: saranno aggiornate              40
456900100521Presenti Anagrafiche e Trattative sul potenziale da Fondere:saranno aggiornate 41
457000100524Utente non abilitato a FONDERE il potenz.:presenti ANAGRAFICHE                 42
457100100525Il potenziale codificato è da TENERE se l'altro non è codificato               43
