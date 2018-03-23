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
028600180219     d wsna            s              9  0
028700080605     d W0060           s              6  0
028800080929     d W007A           s              7
028900080605     d W004a           s              4
029000080929     d Wdes            s             19
029100080929     d WIspp           s              1
029200081009     d WcodCPI         s             21
029300081013     d wspf            s                   like(cpispf)  inz
029400081010     d Wgiorno         s              2  0
029500081010     d WDTunAnno       s              8  0
029600100524     d totnot          s              5  0
029700111116     d codif1          s              1
029800111116     d codif2          s              1
029900120622
030000120622     d DataPrimoConF   s              8  0
030100120622     d DataPrimoConT   s              8  0
030200111115
030300111115     d sav_CPOcpo      s                   like(CPOcpo)
030400111115     d sav_ATCcad      s                   like(ATCcad)
030500111115
030600100525     d NewHIM          s                   like(cpnhim)
030700081010
030800081010     d W1Esinfo        s              1
030900081010     d W2Esinfo        s              1
031000081010     d TieniEsinfo     s              1
031100081010     d FondiEsinfo     s              1
031200081010     d ImpostaT        s              1
031300100525     d TieniconTrCd    s              1
031400100525     d fondiAttB       s              1
031500100525     d fondiAttI       s              1
031600100525     d TieniAtt        s              1
031700090521     d Alfacpo         s             11
031800100419     d Alfaoggi        s              8
031900100525     d W0030           s              3  0
032000100525     d W0080           s              8  0
032100080609     d knk1            s                   like(ntcnk1)  inz
032200080604     d knk2            s                   like(ntcnk2)  inz
032300080604     d ktnt            s                   like(ntctnt)  inz
032400080929     d ktprI           s                   like(cpitpf)  inz
032500080619     d TieniCPO        s                   like(cpocpo)  inz
032600080619     d FondiCPO        s                   like(cpocpo)  inz
032700080619     d FondiRag        s                   like(v031rag) inz
032800080605     d Dataiso         s               d   datfmt(*iso)
032900080605     d Dataymd         s               d   datfmt(*ymd)
033000080605     d Datadmy         s               d   datfmt(*dmy)
033100081010     d DataSYS         s               d   inz(*sys)
033200100524     d OraHMS          s               t   timfmt(*hms)
033300130916     D*
033400081112     d WTFACO          s             21    inz('FILTRAGRU /TFACO16L')
033500130916     d*
033600080415     d                 DS
033700080415     d  Fcpo                          3  0
033800080415     d  Ncpo                          8  0
033900080415     d kcpo                    1     11  0
034000130916      *
034100090521     D TRMK08R         pr                  extpgm('TRMK08R')
034200090521     D  kpjba                              likeds(kpjba)
034300090521     D  ALFACPO                      11
034400080206      //---------------------------------------------------------------
034500080206      //?Definizione procedure usate.
034600080206      //---------------------------------------------------------------
034700080414      /copy gaitrasrc/srcprotopr,tibs02r
034800080414      /copy gaitrasrc/srcprotopr,tibs34r
034900080606      /copy gaitrasrc/srcprotopr,tnta12r
035000080929      /copy gaitrasrc/srcprotopr,trmk50r
035100080617      /copy gaitrasrc/srcprotopr,trul31r
035200100520      /copy gaitrasrc/srcprotopr,trmk01r
035300100520      /copy gaitrasrc/srcprotopr,trmk02r
035400100521      /copy gaitrasrc/srcprotopr,tnta43r
035500110304
035600110304      // - Calcolo categoria potenziale
035700110304     d Trmk05r         pr                  extpgm('TRMK05R')
035800110304     d  kpjba                              likeds(KPJBA)
035900110304     d  trmk05ds                           likeds(TRMK05ds)
036000160901
036100160901       // -?Storicizzazione Variazioni Potenziale
036200160901     d TRMK30R         pr                  extpgm('TRMK30R')
036300160901     d  trmk30ds                           likeds(trmk30ds)
036400160901     d  tncpo_30                           likeds(tncpo_30)
036500160901     d  tncpo1_30                          likeds(tncpo1_30)
036600160901     d  ticpi_30                           likeds(ticpi_30)
036700080206
036800080206      //---------------------------------------------------------------
036900080206      //?Riepilogo indicatori.
037000080206      //---------------------------------------------------------------
037100080207      // - Comuni
037200080207      // 28    : Emissione messaggio di errore a video
037300080207      // - C01/S01
037400080207      // 30    : Condiziona SFLDSP    (*not)
037500080207      // 31    : Condiziona SFLDSPCTL (*not)
037600080207      // 30+31 : Condiziona SFLCLR
037700080207      // 32    : Condiziona SFLNXTCHG in S01
037800080207      // 50    : Errore: Opzione errata
037900080207      // - D01
038000080207      // - Comuni
038100080207      // 99    : Generico di Errore
038200080206      //---------------------------------------------------------------
038300080206
038400080206      //---------------------------------------------------------------
038500080206      //?M A I N - L I N E
038600080206      //---------------------------------------------------------------
038700080206
038800080206     c     *Entry        plist
038900080206     c                   parm                    KPJBA
039000080626     c                   movel     kpjbu         flagintpot        1
039100080626     c* Attivo tasto funzionale di int.potenziali se non vengo dalla int
039200080626     c                   if        flagintpot<>'N'
039300080701     c                   eval      Intpotenz='1'
039400080626     c                   endif
039500080206
039600080206      /free
039700080206
039800080206       // Operazioni iniziali
039900080206       exsr RoutInz;
040000080206
040100080206       // Gestione video
040200080206       DOW $Fine = *off;
040300080206         select;
040400080530
040500080530         // Videata di selezioni
040600080206           when $Video = 'D1';
040700080206             exsr GesD01;
040800080530
040900080530         // SFL di scelta
041000080411           when $Video = 'S2';
041100080411             exsr GesS02;
041200080530
041300080530         // U N I O N E
041400080530           when $Video = 'S1';
041500080530             exsr GesS01;
041600080530
041700080530         // F U S I O N E
041800080603           when $Video = 'S3';
041900080603             exsr GesS03;
042000080206           other;
042100080206             $Fine = *on;
042200080206         endsl;
042300080206       ENDDO;
042400080206
042500080206       // Operazioni finali
042600080206       exsr RoutEnd;
042700080206
042800080206       //--------------------------------------------------------------
042900080206       //?Operazioni iniziali.
043000080206       //--------------------------------------------------------------
043100080206       BEGSR RoutInz;
043200080414       $inzd01=*on;
043300080606       v01cric='F'  ;
043400080206
043500080206         // Impostazione campi "fissi"
043600080208         TBLkut = 1;
043700080208         TBLcod = '1L';
043800080206
043900080206         // Reperimento dati job
044000080206         exsr DatiJob;
044100080206
044200080206         // Verifica errori e autorità profilo
044300080206         clear  wAbi;
044400080206         clear  dLAT;
044500080206         select;
044600080206         // - Se ho errori nei dati utente esco dal pgm
044700080208           when  DUTerr = 'E';
044800080206             $Fine = *on;
044900080206         // - Se non c'è l'abilitazione:
045000080206         //   - se 1° livello, abilitazioni al terminal
045100080206         //   - se 2° livello, abilitazioni al punto operativo
045200080206         //   - se sede è impossibile ma se capita mando a fine il pgm
045300080208           when  UTEaut = *blank;
045400080206             select;
045500080208               when  DUTlpo = '1';
045600080206                 wAbi  = 'TP';
045700080208               when  DUTlpo = '2';
045800080206                 wAbi  = 'PO';
045900080208               when  DUTlpo = 'S';
046000080206                 $Fine = *on;
046100080206             endsl;
046200080206         // - Altrimenti si caricano le abilitazioni del profilo
046300080206           other;
046400080208             dUTE01 = UTEfaf;
046500080208             if  §UTEpot <> *blank;
046600080208               wAbi = §UTEpot;
046700080206             else;
046800080208               wAbi = UTEaut;
046900080206             endif;
047000080206         ENDSL;
047100080206
047200080206         // Controllo se ok l'abilitazione dell'utente
047300080206         reset TIBS02ds;
047400111115         T02Mod = 'C';
047500080208         T02sif = knsif;
047600080208         T02cod = 'LAT';
047700080208         T02ke1 = wAbi;
047800080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
047900080206
048000080208         if  T02err  = *blank;
048100080208           dLAT = T02uni;
048200080206         endif;
048300080206
048400080206         // Errore o utente non abilitato
048500080208         if  T02err <> *blanks   or
048600080208             §LATabi = 'S';
048700080206           $ErrGrave   = *on;
048800080206           errMessage  = *on;
048900080206           errGenerico = *on;
049000080410           V1Dmsg = Msg(01);
049100080206           leavesr;
049200080206         endif;
049300080617
049400080617         clear trul31ds  ;
049500080617         I31abi = wabi    ;
049600080617         I31cdi = DUTdis  ;
049700080617         I31car = DUTare  ;
049800080617         I31cpo = DUTpou  ;
049900080617         TRUL31R   (KPJBA:trul31ds)   ;
050000080617
050100080617         if o31pog=*zeros   ;
050200080617           $ErrGrave   = *on;
050300080617           errMessage  = *on;
050400080617           errGenerico = *on;
050500080617          V1Dmsg = Msg(01);
050600080617           leavesr;
050700080617         endif         ;
050800100520
050900100520          // imposto la data del giorno - 1 anno    ;
051000100520          dataiso=datasys         ;
051100100520          w0080=%dec(dataiso)     ;
051200100520          Alfaoggi=%editc(w0080:'X')  ;
051300100520
051400100520          dataiso=dataiso- %years(1)      ;
051500100520          wgiorno=%subdt(dataiso:*d)  ;
051600100520          wgiorno=wgiorno-1     ;
051700100520          dataiso=dataiso- %days(wgiorno) ;
051800100520          wdtunanno=%dec(dataiso)    ;
051900080929
052000080929       // Carico ds con le INFO ordinate
052100080929          yy= 1    ;
052200080929          setll ('IFO')   TNTBE01l  ;
052300080929          reade ('IFO')   tntbe01l  ;
052400080929
052500080929  1       dow    not %eof(tntbe01l)  ;
052600080929
052700080929  2       if tbeatb=' '                  ;
052800080929          dIFO=tbEuni                ;
052900080929          kifoord(yy) =§ifoifg+%editc(§ifoogi:'X')+%subst(tbeke1:1:3) +
053000081009          (§ifodeb)+§ifoifs   ;
053100080929          yy=yy+1    ;
053200080929          endif     ;
053300080929
053400080929          reade ('IFO')   tntbe01l  ;
053500080929          enddo    ;
053600080929
053700080929          sorta kifoord  ;
053800080929
053900081112          open(e)   TFACO16L              ;
054000081112          if        not %open(tFACO16L)   ;
054100081112           %subst(wtfaco:7:4)='GRPF'      ;
054200081112           OPEN tfaco16l                  ;
054300081112         Endif                            ;
054400081112
054500080206       ENDSR;
054600080206
054700080206       //--------------------------------------------------------------
054800080206       //?Reperimento Dati del job (Utente/Operativi).
054900080206       //--------------------------------------------------------------
055000080206       BEGSR DatiJob;
055100080206
055200080206         in(E) §AzUte;
055300080206         if NOT %error;
055400080206           in(E) §DatiUte;
055500080206         endif;
055600080206         if %error or RSut = *blanks;
055700080206           clear TIBS34ds;
055800080206           tibs34r(tibs34ds);
055900080206           in §AzUte;
056000080206           in §DatiUte;
056100080206         endif;
056200080206
056300080206       ENDSR;
056400080206
056500080206       //--------------------------------------------------------------
056600080206       //?Gestione videata D01
056700080206       //--------------------------------------------------------------
056800080206       BEGSR GesD01;
056900080206
057000080206         // Inizializzazione videata
057100080414
057200080414         // campi da pulire sempre se non c'e' errore
057300080414         if  errGenerico = *off;
057400080414           clear v01dric;
057500080414         endif;
057600080414
057700080414         if  $inzd01 = *on;
057800080411           clear v01dric;
057900080523           v01ptdb='T';
058000111115           clear V01fls;
058100111115           clear V01dfls;
058200080618           v01ela ='T';
058300080411           clear v01fil ;
058400080411           clear v01rag ;
058500080415           clear v01prv ;
058600080414          $Inzd01=*off;
058700080411         endif ;
058800080606
058900080606         // Pulisco sempre
059000080606           clear Sedi   ;
059100080606           clear nrrsed ;
059200080606           clear Filiali;
059300080606           clear nrrfil ;
059400080606           clear Fusione;
059500080606           clear nrrfus ;
059600080606           clear tipofus;
059700111115           clear Catfus ;
059800080206
059900080206         // Emissione testata
060000080411         if  errGenerico = *off;
060100080409           write MK07T01;
060200080411         endif;
060300080206
060400080206         // Emissione videata
060500080409         exfmt MK07D01;
060600080411
060700080206         clear V1Dmsg;
060800080523         clear errFILIALE;
060900111115         clear errFLS;
061000080411         clear errGenerico;
061100080207
061200080206         select;
061300080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
061400080206           when  $ErrGrave = *on;
061500080206             $Fine  = *on;
061600080411
061700080206         // F3=Fine
061800080208           when dsp_aid = c_F03;
061900080409           $Fine = *on;
062000080411
062100080411         // F7=Int.potenziali
062200080411           when dsp_aid = c_F07;
062300080606           EXSR  INTPOTG ;
062400080409            other           ;
062500080411
062600080411             exsr Contrd01 ;
062700080411
062800080414         // Avanzamento se non errori
062900080414
063000080411             if errGenerico=*off;
063100080415
063200080523            //  Visualizzo primo sfl di scelta;
063300080411              $Video = 'S2';
063400080411              $InzS02 = *on;
063500080603
063600080603           // Pulisco qui il posizionamento cursore della 2 videata
063700080603           // perchè in caso di conferma devo riemettere la videata
063800080603           //  in cui si sono effettuare le scelte per unione/fusione
063900080603              clear C02csr;
064000080603              clear pagric;
064100080603             endif  ;
064200080206         endsl;
064300080206
064400080411       // Tipo richiamo
064500080411          if v01cric='F' ;
064600080411          v01dric='F U S I O N E' ;
064700080411          ELSE ;
064800080411          v01dric=' U N I O N E ' ;
064900080411          ENDIF ;
065000080411
065100080206       ENDSR;
065200080606       //--------------------------------------------------------------
065300080606       //?Interrogazione potenziali da prima videata
065400080606       //--------------------------------------------------------------
065500080606       BEGSR    INTPOTG   ;
065600100520
065700100520       clear trmk01ds   ;
065800100520
065900100520           mk01k11='2'   ;
066000100520           kpjbu=trmk01ds;
066100100520           TRMK01R (kpjba) ;
066200100520           trmk01ds=kpjbu  ;
066300100520
066400100520           d11cpo=mk01cpo   ;
066500080606
066600080606           // Selezionato potenziale
066700080606 1         if  d11cpo>0;
066800080606             chain(N)  d11cpo  TNCPO000;
066900080606 2            if %found(tncpo01l) ;
067000080606         // U N I O N E
067100080606 3         if   V01cric='U';
067200080606 4           if cpofsf='S'   ;
067300080606              sedi(1)=cpocpo  ;
067400080606 x4          else    ;
067500080606              filiali(1)=cpocpo  ;
067600080606 4           endif   ;
067700080606           $video='S1'  ;
067800080606           $InzS01 = *on;
067900080606 x3        else   ;
068000080606         // F U S I O N E
068100080606           Fusione(1)=cpocpo  ;
068200080606           tipofus(1)=cpoFSF  ;
068300080606           $video='S3'  ;
068400080606           $InzS03 = *on;
068500080606 3         endif;
068600080606 2         endif;
068700080606 1         endif;
068800080606            ENDSR    ;
068900080411       //--------------------------------------------------------------
069000080411       //?Controllo campi 1 videata
069100080411       //--------------------------------------------------------------
069200080411       BEGSR ContrD01;
069300080415
069400080415         if v01prv<>'  ' ;
069500080415         clear tblkey;
069600080415         tblkey=v01prv;
069700080415
069800080415         chain  (1:'PR':tblkey) tabel00f;
069900080415 2       if  not %found(tabel00f);
070000080415           errPRV=*on;
070100080415           errMessage  = *on;
070200080415           errGenerico = *on;
070300080415           V1Dmsg = Msg(12);
070400080523           leavesr;
070500080415         endif ;
070600080523         endif ;
070700080415
070800111115       //?--> Categoria potenziale <--
070900111116         clear V01dfls;
071000111115       //?Ricerca
071100111115         IF  %scan('?':V01fls) > 0;
071200111115           clear TIBS02ds;
071300111115           T02mod = 'R';
071400111115           T02sif = knsif;
071500111115           T02cod = 'CPO';
071600111115           TNTBE_RicercaControllo (kpjba : TIBS02DS);
071700111115           IF  T02err = *blank;
071800111115             V01fls = T02ke1;
071900111115           ENDIF;
072000111115         ENDIF;
072100080411
072200111115       //?Controllo
072300111115         IF  V01fls <> *blanks;
072400111115           clear dCPO;
072500111115           clear TIBS02ds;
072600111115           T02mod = 'C';
072700111115           T02sif = knsif;
072800111115           T02cod = 'CPO';
072900111116           T02ke1 = V01fls;
073000111115           TNTBE_RicercaControllo (kpjba : TIBS02DS);
073100111115
073200111115           IF  T02err <> *blank;
073300111115             errFLS      = *on;
073400111115             errMessage  = *on;
073500111115             errGenerico = *on;
073600111115             V1Dmsg = msg(07);
073700111115             leavesr;
073800111115           ENDIF;
073900111115           dCPO = T02uni;
074000111115           V01dfls = §CPOdes;
074100111115 1       ENDIF;
074200080415
074300080415         // filiale di appartenenza
074400080414 1       if errGenerico=*off and v01fil>0;
074500080411         chain  v01fil  azorg01l;
074600080414 2       if not %found(azorg01l) ;
074700080523           errFILIALE=*on;
074800080411           errMessage  = *on;
074900080411           errGenerico = *on;
075000080411           V1Dmsg = Msg(8);
075100080414 2       endif;
075200080414 1       endif;
075300080411       ENDSR;
075400080206       //--------------------------------------------------------------
075500080523       //?Gestione videata S01   - UNIONE
075600080206       //--------------------------------------------------------------
075700080409       BEGSR GesS01;
075800080207
075900080207         // Inizializzazione videata
076000080409         if  $InzS01 = *on;
076100080409            exsr InzS01;
076200080409            $InzS01  = *off;
076300080207         endif;
076400080207
076500080207         // Posizionamento cursore
076600080410           C01rcd = 1;
076700080410           sfldsp_n=*off;
076800080207
076900080207         // Emissione Testata e Piede con tasti funzionali abilitati
077000080207         if  errGenerico = *off;
077100080409           write  MK07T01;
077200080409           write  MK07P01;
077300080207         endif;
077400080207
077500080207         // Emissione videata
077600080409         exfmt  MK07C01;
077700080410
077800080207         reset errMessage;
077900080207         reset errGenerico;
078000080207         clear V1Dmsg;
078100080523         clear errSEDE ;
078200080526         clear errFIL  ;
078300080207
078400080523 1       SELECT;
078500080207
078600080207         // - F3=Fine
078700080523 1         WHEN  dsp_aid = c_F03;
078800080409            $Fine = *on;
078900080207
079000080207         // - F12=Ritorno
079100080523 1         WHEN  dsp_aid = c_F12;
079200080606           if nrrsed(1)>0 or  nrrfil(1)>0   ;
079300080523              $Video = 'S2';
079400080606           else  ;
079500080606              $Video = 'D1';
079600080606              $inzd01=*on;
079700080606           endif ;
079800080414
079900080414         // F7=Int.potenziali
080000080523 1         when dsp_aid = c_F07;
080100080603           exsr   INTpotU;
080200080530 2           if  errGenerico = *on;
080300080530           write  MK07T01;
080400080530           write  MK07P01;
080500080530           endif  ;
080600080414
080700080530 x1      // Invio / F6
080800080207           OTHER;
080900080409             exsr ContrS01 ;
081000080523 2           if  errGenerico = *on;
081100080207               leavesr;
081200080523 2           endif;
081300080530
081400080530         // F6=conferma UNIONE
081500080530 1         if   dsp_aid = c_F06;
081600080530           exsr   ConfUNIONE;
081700080530           endif  ;
081800080207
081900080523 1       ENDSL;
082000080207
082100080207       ENDSR;
082200080207
082300080409       //--------------------------------------------------------------
082400080603       //?Interrogazione potenziali per UNIONE
082500080526       //--------------------------------------------------------------
082600080603           Begsr INTPOTU ;
082700080526
082800100520           clear trmk01ds   ;
082900080618           if v01sncpo>0 and v01srag<>*blanks   ;
083000100805           mk01rag=%subst(v01srag:1:8)   ;
083100080618           else     ;
083200080618             if s01fncpo>0 and s01frag<>*blanks   ;
083300100805             mk01rag=%subst(s01frag:1:8)   ;
083400080618             endif    ;
083500080618           endif    ;
083600100520
083700100520             MK01K11='4'  ;
083800100520             kpjbu=trmk01ds;
083900100520             TRMK01R (kpjba) ;
084000100520             trmk01ds=kpjbu  ;
084100100520             D11CPO=Mk01Cpo  ;
084200080526
084300080526           // Selezionato potenziale
084400080526 1         if  d11cpo>0;
084500080530             chain(N)  d11cpo  TNCPO000;
084600080526
084700080526            // Se sede
084800080526 2            if not %found(tncpo01l) or (cpofsf='S' and sedi(1)>0) ;
084900080526               errSEDE =*on;
085000080526               errMessage  = *on;
085100080526               errGenerico = *on;
085200080526               V1Dmsg = Msg(16);
085300080530               %subst(v1dmsg:5:11)=%char(cpocpo) ;
085400080530               %subst(v1dmsg:17:12)=cporag ;
085500080526               leavesr ;
085600080526 2            endif  ;
085700080526
085800080526            // Se sede deve avere partita iva o codice fiscale
085900080526 2            if cpofsf='S' and cpopiv=*blanks and cpocdf=*blanks ;
086000080526               errSEDE =*on;
086100080526               errMessage  = *on;
086200080526               errGenerico = *on;
086300080526               V1Dmsg = Msg(15);
086400080526               leavesr ;
086500080526 2            endif  ;
086600080526
086700080526              // Carico dati sede
086800080526 2            if cpofsf='S' ;
086900080526              kcpo=d11cpo   ;
087000080526              exsr ClearCTL1 ;
087100080526              exsr RiempiCTL1 ;
087200080526              leavesr ;
087300080526 2            endif ;
087400080526
087500080526            // Se Filiale
087600080526 2            if cpofsf='F' and Filiali(7)>0 ;
087700080526               errSEDE =*on;
087800080526               errMessage  = *on;
087900080526               errGenerico = *on;
088000080526               V1Dmsg = Msg(14);
088100080526               leavesr ;
088200080526 2            endif  ;
088300080526
088400080526              // cerco primo record di sfl libero per impostarlo
088500080526              s01nrr=0 ;
088600080526              s01fncpo=99999999 ;
088700080526 2            dow  s01nrr<7 and (s01fncpo<>0 or s01ffcpo<>0) ;
088800080526                s01nrr=s01nrr+1 ;
088900080526                chain  s01nrr mk07s01;
089000080526 2            enddo  ;
089100080526
089200080526 2            if s01fncpo=0 and s01ffcpo=0 ;
089300080526                kcpo=d11cpo;
089400080526                exsr riempiSFL1 ;
089500080530                protpotFIL=*off;
089600080530                update mk07s01  ;
089700080526 2            endif;
089800080526 1          endif;
089900080526            ENDSR;
090000080526       //--------------------------------------------------------------
090100080523       //?controlli videata S01  - UNIONE
090200080409       //--------------------------------------------------------------
090300080409       BEGSR ContrS01;
090400080410
090500080523       // Solo se non protetto
0906000805261      if  ProtpotSED=*off    ;
090700080523
090800080410       // Immettere cod potenziale
0909000805262      if   v01sncpo=0 and v01sfcpo=0  ;
091000080530           exsr clearCTL1  ;
091100080523           errSEDE =*on;
091200080410           errMessage  = *on;
091300080409           errGenerico = *on;
091400080414           V1Dmsg = Msg(03);
0915000805262      endif;
091600080410
091700080523         fcpo=v01Sfcpo      ;
091800080523         ncpo=v01Sncpo      ;
091900080530         chain(N)  Kcpo  TNCPO000;
092000080410
092100080410         // Codice potenziale NON trovato
092200080526 2       if  NOT  %found(TNCPO01L);
092300080523           errSEDE =*on;
092400080410           errMessage  = *on;
092500080410           errGenerico = *on;
092600080414           V1Dmsg = Msg(03);
092700080526 x2    else ;
092800080526           exsr clearCTL1         ;
092900080604           if cpocpo<>savcpo
093000080604              and savcpo>0       ;
093100080526           pulizforz='S'       ;
093200080526           endif               ;
093300080526
093400080604           savcpo =cpocpo      ;
093500080526
093600080526           exsr riempiCTL1         ;
093700080526           clear Pulizforz     ;
093800080526 2     endif ;
093900080523 1     endif ;
094000080410
094100080410 1       if errGenerico=*off;
094200080410
094300080414       // controllo SFL
094400080526       s01nrr=1;
094500080526       clear contafil     ;
094600080526 2     dow s01nrr<=record and errGenerico=*off;
094700080523       clear errFIL   ;
094800080410
094900080526       chain  s01nrr mk07s01;
095000080410
095100080523 3     if s01ffcpo>0 or s01fncpo>0;
095200080523         fcpo=s01ffcpo      ;
095300080523         ncpo=s01fncpo      ;
095400080530         chain(N)  Kcpo  TNCPO000;
095500080410
095600080410         // Codice potenziale NON trovato
095700080410 4       if  NOT  %found(TNCPO01L);
095800080523           errFIL   =*on;
095900080410           errMessage  = *on;
096000080410           errGenerico = *on;
096100080618           V1Dmsg = Msg(27);
096200080526           else ;
096300080526           contafil=contafil+1 ;
096400080410 4     endif;
096500080526
096600080526         // Se non è selezione da sfl del pgm, ricarico i dati
096700080526 4       if filiali(s01nrr)=0 ;
096800080526         ProtpotFIL=*off    ;
096900080526         exsr RiempiSFL1;
097000080526         else    ;
097100080526         ProtpotFIL=*on     ;
097200080526 4       endif          ;
097300080618
097400080618         // controllo se utente abilitato all'aggiornamento
097500080618 4       if Errfil=*off   ;
097600080618           Indx=%lookup(s01ffil:pog)   ;
097700080618 5         if Indx=0    ;
097800080618             errFIL   =*on;
097900080618             errMessage  = *on;
098000080618             errGenerico = *on;
098100080618             V1Dmsg = Msg(26);
098200080618 5         endif        ;
098300080618 4       endif        ;
098400080526
098500080526         // Se la filiale presenta una partita iva o cod fiscale
098600080526         //       diversi --> errore forzabile
098700080526
098800080618 4       if  Errfil=*off and S01forzpiv=' '  ;
098900080526 5       if (s01fpiv<>v01spiv and s01fpiv<>*blanks and v01spiv<>*blanks) or
099000080526            (s01fcdf<>v01scdf and s01fcdf<>*blanks and v01scdf<>*blanks)  ;
099100080526           errFIL   =*on;
099200080526           errMessage  = *on;
099300080526           errGenerico = *on;
099400080526           V1Dmsg = Msg(17);
099500080526           s01forzpiv='S' ;
099600080526 5       endif            ;
099700080526 4       endif            ;
099800080410
099900080526 x3      else  ;
100000080526         exsr ClearSFL1 ;
100100080530         ProtpotFIL=*off    ;
100200080410 3       endif  ;
100300080410
100400080410       update mk07s01;
100500080530       ProtpotFIL=*off    ;
100600080410
100700080526       s01nrr=s01nrr+1;
100800080410 2     enddo;
100900080410
101000080523       // Immettere almeno un figlio
101100080526 2       if errGenerico=*off and contaFil=0;
101200080410          chain  1 mk07s01;
101300080523           errFIL   =*on;
101400080410           errMessage  = *on;
101500080410           errGenerico = *on;
101600080410           V1Dmsg = Msg(06);
101700080410          update mk07s01;
101800080410 2       endif;
101900080410 1     endif;
102000080409
102100080409       ENDSR;
102200080530       //--------------------------------------------------------------
102300080530       //?Conferma unione potenziali
102400080530       //--------------------------------------------------------------
102500080530       BEGSR ConfUNIONE ;
102600080530       // Leggo i potenziali di FILIALE ed aggiorno con PIV e CDF
102700080530       //  della sede
102800080530
102900080530       s01nrr=1;
103000080530 2     dow s01nrr<=record ;
103100080530
103200080530       chain  s01nrr mk07s01;
103300080530
103400080530 3     if s01ffcpo>0 or s01fncpo>0;
103500080530         fcpo=s01ffcpo      ;
103600080530         ncpo=s01fncpo      ;
103700080530         chain  Kcpo  TNCPO000;
103800080530         if %found(tncpo01l)  ;
103900160901         //?Salvo l'immagine Precedente
104000160901           exsr ImmaginePrima;
104100080530         cpopiv=v01spiv;
104200080530         cpocdf=v01scdf;
104300080530         dcpo01=cporst ;
104400080530         §cporicuni='S' ;
104500080530         cporst=dcpo01 ;
104600080530         cpodtr=*date  ;
104700080530         update tncpo000 ;
104800160901         //?Scrivo la variazione fatta sul potenziale
104900160901           exsr ScriviVariazione;
105000080530         endif   ;
105100160901         //?Scrivo una riga per memorizzare il codice potenziale Sede di Unione
105200160901           exsr ScriviSede;
105300080530       endif   ;
105400080530       s01nrr=s01nrr+1;
105500080530
105600080530       enddo   ;
105700080530
105800100520       // Chaino la SEDE per aggiornare flag unione
105900080530
106000080530         fcpo=v01Sfcpo      ;
106100080530         ncpo=v01Sncpo      ;
106200080530         chain  Kcpo  TNCPO000;
106300080530
106400080530         // Codice potenziale NON trovato
106500080530 2       if  %found(TNCPO01L);
106600080530         dcpo01=cporst ;
106700080530         §cporicuni='S' ;
106800080530         cporst=dcpo01 ;
106900080530         update tncpo000 ;
107000080530         endif ;
107100080530
107200080530         // imposto il posizionamento cursore per il sfl che emetto
107300080530         //  aggiornato
107400080606         if nrrSED(1)> 0  or nrrfil(1)>0 ;
107500080606           C02csr=nrrSED(1) ;
107600080606           if c02csr=0 ;
107700080606             C02csr=nrrFIL(1) ;
107800080606           endif     ;
107900080606           pagric=(c02csr/C_sflPag)+1 ;
108000080606           $Video = 'S2';
108100080606           $InzS02 = *on;
108200080606         else           ;
108300080606           $Video = 'D1';
108400080606           $InzD01 = *on;
108500080606         endif    ;
108600080530
108700080530       ENDSR;
108800080409
108900080207       //--------------------------------------------------------------
109000080415       //?Inizializzazione videata S01 per  U N I O N E
109100080207       //--------------------------------------------------------------
109200080409       BEGSR InzS01;
109300080207
109400080207       // Pulizia subfile
109500080207         SflDsp_N    = *on;
109600080207         SflDspCtl_N = *on;
109700080409         write  MK07C01;
109800080207         SflDspCtl_N = *off;
109900080207         SflEnd      = *off;
110000080530
110100080530         clear pagric;
110200080530         clear C01rcd;
110300080409         S01nrr=1 ;
110400080207         clear V1Dmsg;
110500080207         errMessage  = *off;
110600080207         errGenerico = *off;
110700080523         errScelta   = *off;
110800080526         errFil      = *off;
110900080526         errSede     = *off;
111000080409
111100080409       // Carico una pagina vuota
111200080410          clear  MK07S01;
111300080414
111400080414
111500080523          dow   s01nrr<= Record ;
111600080410           write MK07S01;
111700080409           S01nrr += 1;
111800080414          endDO      ;
111900080409
112000080409       // SFL CONTROL
112100080409
112200080523          exsr    ClearCTL1 ;
112300080414
112400080415       // Se passata SEDE imposto
112500080523          if SEDI(1)>0     ;
112600080523            kcpo=sedI(1) ;
112700080530            chain(N) kcpo   tncpo01l;
112800080414            if   %found(tncpo01l) ;
112900080523              exsr RiempiCTL1;
113000080523              eval ProtPOTSED=*on ;
113100080414            endif    ;
113200080414          endif    ;
113300080523
113400080523          FF=1  ;
113500080530          clear errFIL   ;
113600080530          protpotFIL=*on ;
113700080523          dow FILIALI(ff)>0     ;
113800080523            kcpo=filiali(ff) ;
113900080530            chain(N) kcpo   tncpo01l;
114000080523            if   %found(tncpo01l) ;
114100080530              s01nrr=ff  ;
114200080530              chain   s01nrr  mk07s01  ;
114300080523              exsr RiempiSFL1;
114400080530
114500080530              update mk07s01  ;
114600080523            endif    ;
114700080523
114800080523          ff=ff+1  ;
114900080523          enddo    ;
115000080530          protpotFIL=*off;
115100080207
115200080207       ENDSR;
115300080207
115400080410       //--------------------------------------------------------------
115500080523       //?Pulizia dati codice SEDE
115600080410       //--------------------------------------------------------------
115700080523          BEGSR   ClearCTL1  ;
115800080523          clear   v01Sfcpo   ;
115900080523          clear   v01Sncpo   ;
116000080523          protpotSED=*off       ;
116100080523          clear   v01Srag    ;
116200080523          clear   v01Sloc    ;
116300080523          clear   v01Scap    ;
116400080523          clear   v01Sprv    ;
116500080523          clear   v01Sfil    ;
116600080523          clear   v01Spiv    ;
116700080523          clear   v01Scdf    ;
116800080523          clear   v01Sric    ;
116900080523          clear   v01Sdmerc  ;
117000111115
117100111115          clear   v01sfls;
117200111115          clear   v01scad;
117300111115
117400080410          ENDSR;
117500080414       //--------------------------------------------------------------
117600080523       //?carica dati della SEDE
117700080414       //--------------------------------------------------------------
117800080523          BEGSR   RiempiCTL1  ;
117900080414
118000080522       v01Sfcpo=fcpo  ;
118100080522       v01Sncpo=ncpo  ;
118200080522       v01srag=cporag;
118300080522       v01sloc=cpocit;
118400080522       v01scap=cpocap;
118500080523       if cponaz=*blanks  ;
118600080522       v01sprv=cpoprv;
118700080523       else  ;
118800080523       v01sprv=cponaz;
118900080523       endif ;
119000111115
119100111115       //?Categoria potenziale
119200111115         V01sfls = CPOfls;
119300111115
119400111115       //?Prossima attività
119500111115         sav_CPOcpo = CPOcpo;
119600111115         exsr RIC_proATT;
119700111115         V01scad = sav_ATCcad;
119800080522
119900080522         v01sfil=cpoflt;
120000080522         v01spiv=cpopiv;
120100080522         v01scdf=cpocdf;
120200080523         clear tblkey  ;
120300080523         tblkey=%editc(cposct:'X') ;
120400080523         chain  (1:'1L':tblkey) tabel00f;
120500080523 1       if %found(tabel00f);
120600080522         v01sdmerc=tbluni;
120700080522         else ;
120800080522         v01sdmerc=*all'?' ;
120900080523 1       endif  ;
121000080522
121100080414         dcpo01=cporst;
121200080523 1     select   ;
121300080522         when §cporicfus='S' and §cporicuni='S';
121400080522           v01sric='F/U'     ;
121500080522         when §cporicfus='S' ;
121600080618           v01sric=' Fu'     ;
121700080522         when §cporicuni='S' ;
121800080618           v01sric=' Un'     ;
121900080522         other  ;
122000080522           v01sric='   ';
122100080523 1     endsl;
122200080526
122300080526       // Se devo pulire le forzature nel sfl eseguo
122400080526       if Pulizforz='S'  ;
122500080526       s01nrr=1   ;
122600080526         dow s01nrr<=record  ;
122700080526         chain   s01nrr   mk07s01  ;
122800080526         clear s01forzpiv ;
122900080526
123000080526         if filiali(s01nrr)>0 ;
123100080526         ProtpotFil=*on   ;
123200080526         else  ;
123300080526         ProtpotFil=*off  ;
123400080526         endif ;
123500080526         update mk07s01   ;
123600080526
123700080526         s01nrr=s01nrr+1     ;
123800080526         enddo   ;
123900080526
124000080526         ProtpotFil=*off  ;
124100080526       endif     ;
124200080414
124300100520         ENDSR;
124400111115
124500111115       //--------------------------------------------------------------
124600111115       //?Ricerca la prossima attività da eseguire sul potenziale
124700111115       //--------------------------------------------------------------
124800111115         BEGSR  RIC_proATT;
124900111115
125000111115           $EndAtt = *off;
125100111115           clear sav_ATCcad;
125200111115           exec sql
125300111115            declare ATT cursor for select ATCcad from TIATC00F
125400111115            where ATCcpo = :sav_CPOcpo and atcdco = 0
125500111115            order by ATCdad, ATChda;
125600111115           exec sql
125700111115            open ATT;
125800111115           IF sqlcode < 0;
125900111115             $EndAtt = *on;
126000111115           ENDIF;
126100111115           DOW not $EndAtt;
126200111115             exec sql
126300111115             fetch next from ATT into :ATCcad;
126400111115             IF sqlcod = 100 or sqlcod < 0;
126500111115               $EndAtt = *on;
126600111115               leave;
126700111115             ENDIF;
126800111115             sav_ATCcad = ATCcad;
126900111115             leave;
127000111115           ENDDO;
127100111115           exec sql close ATT;
127200111115
127300111115         ENDSR;
127400080526       //--------------------------------------------------------------
127500080526       //?Pulizia riga filiale per unione
127600080526       //--------------------------------------------------------------
127700080526          BEGSR   ClearSFL1  ;
127800080526          clear s01ffcpo  ;
127900080526          clear s01fncpo  ;
128000080526          clear s01frag   ;
128100080526          clear s01floc   ;
128200080526          clear s01fcap   ;
128300080526          clear s01fprv   ;
128400080526          clear s01ffil   ;
128500080526          clear s01fpiv   ;
128600080526          clear s01fcdf   ;
128700080526          clear s01fric   ;
128800080526          clear s01fdmerc ;
128900080526          clear s01forzpiv;
129000111115
129100111115          clear s01fls;
129200111115          clear s01cad;
129300111115
129400080526          ENDSR     ;
129500080523       //--------------------------------------------------------------
129600080523       //?carica dati delle FILIALI
129700080523       //--------------------------------------------------------------
129800080523          BEGSR   RiempiSFL1  ;
129900080523
130000080523       s01ffcpo=fcpo  ;
130100080523       s01fncpo=ncpo  ;
130200080523       s01frag=cporag;
130300080523       s01floc=cpocit;
130400080523       s01fcap=cpocap;
130500080523       if cponaz=*blanks  ;
130600080523       s01fprv=cpoprv;
130700080523       else  ;
130800080523       s01fprv=cponaz;
130900080523       endif ;
131000080523
131100111115
131200111115       //?Categoria potenziale
131300111115         S01fls = CPOfls;
131400111115
131500111115       //?Prossima attività
131600111115         sav_CPOcpo = CPOcpo;
131700111115         exsr RIC_proATT;
131800111115         S01cad = sav_ATCcad;
131900080523
132000080523         s01ffil=cpoflt;
132100080523         s01fpiv=cpopiv;
132200080523         s01fcdf=cpocdf;
132300080523         clear tblkey  ;
132400080523         tblkey=%editc(cposct:'X') ;
132500080523         chain  (1:'1L':tblkey) tabel00f;
132600080523 6       if %found(tabel00f);
132700080523         s01fdmerc=tbluni;
132800080523         else ;
132900080523         s01fdmerc=*all'?' ;
133000080523         endif  ;
133100080523
133200080523         dcpo01=cporst;
133300080523       select   ;
133400080523         when §cporicfus='S' and §cporicuni='S';
133500080523           s01fric='F/U'     ;
133600080523         when §cporicfus='S' ;
133700080618           s01fric=' Fu'     ;
133800080523         when §cporicuni='S' ;
133900080618           s01fric=' Un'     ;
134000080523         other  ;
134100080523           s01fric='   ';
134200080523       endsl;
134300080523
134400080523          ENDSR;
134500080411       //--------------------------------------------------------------
134600080523       //?Gestione videata S02   - SCelta potenziali
134700080411       //--------------------------------------------------------------
134800080411       BEGSR GesS02;
134900080411
135000080411         // Inizializzazione videata
135100080411         if  $InzS02 = *on;
135200080411            exsr InzS02;
135300080411             $InzS02  = *off;
135400080411         endif;
135500080411
135600080411         // Posizionamento cursore
135700080411         if  C02csr  > *zero;
135800080411           C02rcd = C02csr;
135900080411         else;
136000080411
136100080411         // Se non è stato caricato nulla si riemette la videata D01
136200080411           C02rcd = 1;
136300080411           $Video = 'D1';
136400080411           errMessage  = *on;
136500080411           errGenerico = *on;
136600080411           errRAG      = *on;
136700080414           V1Dmsg = Msg(09);
136800080411           leavesr;
136900080411         endif;
137000080411
137100080411
137200080411         // Posizionamento cursore
137300080411           sfldsp_n=*off;
137400080411
137500080411         // Emissione Testata e Piede con tasti funzionali abilitati
137600080604         if  errGenerico = *off or ricInt ='S'  ;
137700080414           write  MK07T01;
137800080411           write  MK07P02;
137900080604           clear  ricInt   ;
138000080411         endif;
138100080411
138200080411         // Emissione videata
138300080411         exfmt  MK07C02;
138400080411
138500080411         reset errMessage;
138600080411         reset errGenerico;
138700080411         clear V1Dmsg;
138800080523         clear errScelta ;
138900080411
139000080411         SELECT;
139100080411
139200080411         // - F3=Fine
139300080411           WHEN  dsp_aid = c_F03;
139400080411            $Fine = *on;
139500080411
139600080411         // - F12=Ritorno
139700080411           WHEN  dsp_aid = c_F12;
139800080411           $Video = 'D1';
139900080411
140000080414         // - Roll-Up
140100080414           WHEN  dsp_aid = c_RollUp;
140200080414           // - Verifica se raggiunto il limite di records registrabili
140300080414           //   nel subfile (9999)
140400080414             if  S02nrr = *hival;
140500080414               errMessage  = *on;
140600080414               errGenerico = *on;
140700080414               V1Dmsg = Msg(10);
140800080414             else;
140900080414               exsr RollUpS02;
141000080414             endif;
141100080414
141200080411         // Invio
141300080411           OTHER;
141400080523             exsr ContrS02  ;
141500080411             if  errGenerico = *on;
141600080411               leavesr;
141700080411             endif;
141800080411
141900080411         ENDSL;
142000080411
142100080411       ENDSR;
142200080411       //--------------------------------------------------------------
142300080414       //?Inizializzazione sfl 2
142400080411       //--------------------------------------------------------------
142500080411       BEGSR InzS02;
142600080411
142700080411       // Pulizia subfile
142800080411         SflDsp_N    = *on;
142900080411         SflDspCtl_N = *on;
143000080411         write  MK07C02;
143100080411         SflDspCtl_N = *off;
143200080411         SflEnd      = *off;
143300080411
143400080530         clear W_SflPag;
143500080414         $EoF = *off;
143600080411         clear C02rcd;
143700080414         clear S02nrr;
143800080411         clear V1Dmsg;
143900080411         errMessage  = *off;
144000080411         errGenerico = *off;
144100080415         errscelta=*off  ;
144200080604         clear savcpo    ;
144300080411
144400080411       // Carico pagina
144500080414          K48A=V01RAG;
144600080414          setll  K48A    TNCPO02l;
144700080411          exsr sr_ReadRec;
144800080411
144900080530       //  se vengo da videata di selezioni carico una pagina
145000080530       //    altrimenti il numero delle pagine dove è presente il record
145100080530       //    di sede
145200080530         if c02csr=0           ;
145300080530         totrig=C_SflPAg       ;
145400080530         else                  ;
145500080530         totrig=c_SflPag*pagric  ;
145600080530         endif                 ;
145700080530
145800080411         dow  $EoF   = *off     and
145900080530              S02nrr < totrig  ;
146000080411           exsr RollUpS02;
146100080411         enddo;
146200080411
146300080411       // -> forza l'impaginazione sul 1° rec. del subfile
146400080530       //    se vengo dalla videata di selezioni
146500080530
146600080411         if S02nrr > *zero;
146700080530           if c02csr=0 ;
146800080411           C02rcd  = 1;
146900080411           C02csr  = 1;
147000080530           else       ;
147100110831
147200110831           // Se c02csr > del numero di record, imposto il num max
147300110831             if c02csr>s02nrr ;
147400110831              c02csr=s02nrr ;
147500110831             endif  ;
147600080530           C02rcd  = c02csr;
147700080530           endif      ;
147800080530
147900080411         else;
148000080411           clear C02rcd;
148100110831           clear C02csr;
148200080411         endif;
148300080411
148400080411       ENDSR;
148500080411       //--------------------------------------------------------------
148600080411       //?Lettura x caricamento SFL
148700080411       //--------------------------------------------------------------
148800080411       BEGSR sr_ReadRec;
148900080411
149000080411         $RecOK  = *off;
149100080411
149200080411         DOW  $EoF   = *off   and
149300080411              $RecOK = *off;
149400080411
149500080411           // (se non rilevato stop record elaborabili in selezione)
149600080411             if  $EoF = *off;
149700080411               read  TNCPO002;
149800080411               $EoF  = (%eof(TNCPO02L));
149900080411             endif;
150000080411
150100080411           // Selezione record in base alle parzializzazioni
150200080411           if  $EoF  = *off;
150300080411             exsr sr_SelRec;
150400080411           endif;
150500080411
150600080411         ENDDO;
150700080411
150800080411       ENDSR;
150900080411       //--------------------------------------------------------------
151000080411       //?Selezione record in base alle parzializzazioni
151100080411       //--------------------------------------------------------------
151200080411       BEGSR sr_SelRec;
151300080411
151400080411         $RecOK  = *off;
151500080411
151600080411         // Parzializzazione per ragione sociale
151700080411         if  V01rag <> *blank;
151800080411           xx    = %checkr(' ' : V01rag);
151900080610           if   %subst(V01rag : 1 : xx) <> %subst(r_CPOrag : 1 : xx);
152000080411               $EoF = *on;
152100080411             leavesr;
152200080411           endif;
152300080411         endif;
152400080411
152500080411         select;
152600080415         // Parz. per Provincia
152700080610         when  v01prv<>'  ' and v01prv<>r_cpoprv;
152800080415             leavesr;
152900080411
153000080411           // Parzializzazione per filiale di appartenenza
153100080414           when  V01fil <> *zero    and
153200080610                 V01fil <> r_cpOflt;
153300080411             leavesr;
153400080411
153500080415           // parz. per potenziale FILIALE
153600080610           when  V01ptDB ='F' and  r_cpofsf='S'   ;
153700080411             leavesr;
153800080415           // parz. per Potenziale SEDE
153900080610           when  V01ptDB ='S' and r_cpofsf='F'   ;
154000080411             leavesr;
154100080411           ENDSL;
154200080411
154300111115       //?Parzializzazione per categoria potenziale
154400111115         IF  V01fls <> *blanks and V01fls <> r_CPOfls;
154500111115           leavesr;
154600111115         ENDIF;
154700080603
154800081009         // parz per Potenziali normalizzati
154900080603         if  V01ela <>'T' ;
155000080610         dcpo01=r_cporst ;
155100080603
155200080603         if   v01cric='U'  ;
155300080603           if  V01ela = 'S' and §cporicuni='S';
155400080603             leavesr;
155500080603           endif;
155600081009           if  V01ela = 'N' and §cporicuni=' ';
155700080603             leavesr;
155800080603           endif;
155900080603         endif;
156000080415
156100080603         if   v01cric='F'  ;
156200080603           if  V01ela = 'S' and §cporicfus='S';
156300080603             leavesr;
156400080603           endif;
156500081009           if  V01ela = 'N' and §cporicfus=' ';
156600080603             leavesr;
156700080603           endif;
156800080603         endif;
156900080411         endif;
157000080411
157100080411         // => record OK!
157200080411         $RecOK  = *on;
157300080411
157400080411       ENDSR;
157500080411       //--------------------------------------------------------------
157600080411       //?Caricamento singola pagina S02
157700080411       //--------------------------------------------------------------
157800080411       BEGSR RollUpS02;
157900080411
158000080411         S02nrr    = (W_SflPag * C_SflPag);
158100080411         W_SflPag += 1;
158200080411         SflNxtChg = *off;
158300080411
158400080411         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
158500080523 1       DOW  $EoF   = *off                   and
158600080411              S02nrr < (W_SflPag * C_SflPag)  and
158700080411              S02nrr < *hival;
158800080411
158900080411         // - Caricamento dati nel record del subfile
159000080414         clear s02sce  ;
159100080610         s02cpo=r_cpocpo;
159200080610         s02rag=r_cporag;
159300080610 2       if r_cpofsf='S' ;
159400080523          s02deb='Sd';
159500080411         ELSE         ;
159600080523          s02deb='Fi';
159700080523 2       endif;
159800080415
159900080610         s02loc=r_cpocit;
160000080610         %subst(s02loc:21:3)=' '+r_cpoprv;
160100080610         s02fil=r_cpoflt;
160200080610         s02piv=r_cpopiv;
160300080610         s02cdf=r_cpocdf;
160400080610         dcpo01=r_cporst;
160500111115
160600111115       //?Categoria potenziale
160700111115         S02fls = r_CPOfls;
160800111115
160900111115       //?Prossima attività
161000111115         sav_CPOcpo = r_CPOcpo;
161100111115         exsr RIC_proATT;
161200111115         S02cad = sav_ATCcad;
161300111115
161400080523 2     select   ;
161500080522         when §cporicfus='S' and §cporicuni='S';
161600080522           s02ric='F/U'     ;
161700080522         when §cporicfus='S' ;
161800080618           s02ric=' Fu'     ;
161900080522         when §cporicuni='S' ;
162000080618           s02ric=' Un'     ;
162100080522         other  ;
162200080522           s02ric='   ';
162300080523 2     endSL;
162400080415
162500080411
162600080411           S02nrr += 1;
162700080411           write MK07S02;
162800080411
162900080411         // - Lettura record successivo nell'archivio
163000080411           exsr sr_ReadRec;
163100080411
163200080523 1       ENDDO;
163300080411
163400080411         // Attivazione (eventuale) del SFLEND
163500080411         SflEnd   = ($EoF = *on);
163600080411
163700080411         // Posizionamento cursore al 1° rcd della pagina
163800080523 1       if  S02nrr > *zero;
163900080414           wPag   = S02nrr / C_SflPag;
164000080414           wRig   = S02nrr - (C_SflPag * wPag);
164100080411           C02rcd = wPag * C_SflPag;
164200080523 2         if  wRig > *zeros;
164300080415             C02rcd = C02rcd + 1;
164400080411           else;
164500080411             C02rcd = C02rcd - C_SflPag + 1;
164600080523 2         endif;
164700080523 x1      else;
164800080411           clear  C02rcd;
164900080523 1       endif;
165000080411
165100080411         C02csr = C02rcd;
165200080411
165300080411       ENDSR;
165400080414       //--------------------------------------------------------------
165500080414       //?controllo scelta SFL 2
165600080414       //--------------------------------------------------------------
165700080523       BEGSR contrS02 ;
165800080414
165900080523       clear ErrScelta ;
166000080523       clear nrrSED  ;
166100080415       clear SEDI ;
166200080415       clear SS   ;
166300080603
166400080522       clear nrrFIL    ;
166500080415       clear filiali;
166600080415       clear FF     ;
166700080603
166800080603       clear Fusione;
166900080603       clear nrrFus    ;
167000111115       clear CatFus  ;
167100080603       clear TipoFus  ;
167200080604       clear RicINt   ;
167300080625       clear savcpo    ;
167400080415
167500080414       readc mk07s02;
167600080526 1     DOW  NOT  %eof(TRMK07D);
167700080414
167800080415       // 5 - Visualizza
167900080526 2     if s02sce='5' ;
168000100520         clear trmk01ds;
168100100520          Mk01K10='N'   ;
168200100805          mk01cpo=s02cpo  ;
168300100520         trmk02r ( kpjba : trmk01ds );
168400080415         clear s02sce;
168500080523         c02csr=s02nrr  ;
168600080604         ricInt ='S'     ;
168700080617         errGenerico = *on;
168800080526 2     endif ;
168900080414
169000080603       // U N I O N E
169100080603 1a    if   V01cric='U';
169200080603
169300080603       // Scelta SEDE
169400080523
169500080526 2     if   s02sce='1' ;
169600080523
169700080523       // impossibile  unire una sede senza partita iva e cod fiscale
169800080526 3     if s02deb='Sd'    ;
169900080526 4     if s02piv=*blanks and s02cdf=*blanks ;
170000080523         errscelta=*on   ;
170100080523         SflNxtChg = *on ;
170200080523         errMessage  = *on;
170300080523         errGenerico = *on;
170400080523         C02csr = s02nrr    ;
170500080523         V1Dmsg = Msg(15);
170600080523         update mk07s02;
170700080523
170800080523         errscelta=*off  ;
170900080523         SflNxtChg = *off ;
171000080523         leavesr;
171100080526 4     endif  ;
171200080523
171300080415       ss=ss+1 ;
171400080526 4       if  ss<=2;
171500080522         sedi(ss)=s02cpo;
171600080523         nrrsed(ss)=s02nrr;
171700080526 4       endif;
171800080526 3     endif;
171900080415
172000080526 3     if s02deb='Fi'    ;
172100080618       // Errore se non abilitato al potenziale m
172200080618       Indx=%lookup(s02fil:pog)  ;
172300080618 4     if Indx=0;
172400080618         errscelta=*on   ;
172500080618         SflNxtChg = *on ;
172600080618         errMessage  = *on;
172700080618         errGenerico = *on;
172800080618         C02csr = s02nrr    ;
172900080618         V1Dmsg = Msg(26);
173000080618         update mk07s02;
173100080618
173200080618         errscelta=*off  ;
173300080618         SflNxtChg = *off ;
173400080618         leavesr;
173500080618 4      endif   ;
173600080618
173700080526       ff=ff+1 ;
173800080526 4       if  ff<=8;
173900080415         filiali(ff)=s02cpo;
174000080523         nrrfil(ff) =s02nrr;
174100080526 4       endif;
174200080526 3     endif;
174300080526 2     endif;
174400080603 1a    endif;
174500080603
174600080603       // F U S I O N E
174700080603 1a    if   V01cric='F';
174800080603 2     if   s02sce='1' ;
174900100520
175000100419
175100100419        ss=ss+1 ;
175200080603 3       if  ss<=3;
175300080603         Fusione(ss)=s02cpo;
175400080603         nrrfus(ss)=s02nrr;
175500080603         tipofus(ss)=s02deb;
175600111115       //?Categoria potenziale
175700111115         CatFus(ss)= S02fls;
175800100520
175900080603 3       endif;
176000080603 2     endif;
176100080603 1a    endif;
176200080603
176300080415
176400080526 2     if s02sce<>' ';
176500080415         SflNxtChg = *on ;
176600080415         else ;
176700080415         SflNxtChg = *off;
176800080526 2     endif;
176900080415
177000080414       update mk07s02;
177100080414
177200080414       readc mk07s02;
177300080526 1     enddo;
177400080415
177500080603       // U N I O N E
177600080603 1a    if   V01cric='U';
177700080603
177800080415       // Se più di una sede --> errore
177900080526 1     if sedi(2)>0   ;
178000080522         chain  nrrsed(2) mk07s02;
178100080603         V1Dmsg = Msg(13);
178200080603         C02csr = nrrsed(2) ;
178300080603         exsr ERRSFL2  ;
178400080523         leavesr;
178500080526 1     endif ;
178600080522
178700080526 1     if filiali(8)>0   ;
178800080522         chain  nrrfil(8)  mk07s02;
178900080523         C02csr = nrrfil(ff);
179000080415         V1Dmsg = Msg(14);
179100080603         exsr ERRSFL2  ;
179200080523         leavesr;
179300080526 1     endif ;
179400080523
179500080610       // Se non selezionato nè una sede nè una filiale errore bloccante
179600080617 1     if sedi(1)=0 and filiali(1)=0 and ricint=' ';
179700080610         chain  c02csr mk07s02;
179800080523         V1Dmsg = Msg(02);
179900080603         exsr ERRSFL2  ;
180000080523         leavesr;
180100080526 1     endif ;
180200080603 1a    endif ;
180300080603
180400080603       // F U S I O N E
180500080603 1a    if   V01cric='F';
180600080603
180700080603       // Se più di due potenziali da fondere --> errore
180800080603 1     if Fusione(3)>0   ;
180900080603         chain  nrrFus(3) mk07s02;
181000080603         C02csr = nrrFus(3) ;
181100080603         V1Dmsg = Msg(18);
181200080603         exsr ERRSFL2  ;
181300080603         leavesr;
181400080603 1     endif ;
181500080603
181600111115       //?Impossibile fondere 2 potenziali in cat. Eliminabile
181700111115 1     IF  CatFus(1) = 'E' and Catfus(2) = 'E' ;
181800080603         chain  nrrFus(2) mk07s02;
181900080603         C02csr = nrrFus(2) ;
182000080603         V1Dmsg = Msg(05);
182100080603         exsr ERRSFL2  ;
182200080603         leavesr;
182300080603 1     endif   ;
182400080603
182500080603       // Impossibile fondere 2 sedi
182600080603 1     if tipoFus(1)='S' and tipofus(2)='S' ;
182700080603         chain  nrrFus(2) mk07s02;
182800080603         C02csr = nrrFus(2) ;
182900080603         V1Dmsg = Msg(04);
183000080603         exsr ERRSFL2  ;
183100080603         leavesr;
183200080603 1     endif   ;
183300080603
183400080617       // Se non selezionato nessun codice --> errore
183500080617 1     if fusione(1)=0 and ricint=' ';
183600080610         chain  c02csr   mk07s02;
183700080603         V1Dmsg = Msg(02);
183800080603         exsr ERRSFL2  ;
183900080603         leavesr;
184000080603 1     endif   ;
184100080603 1a    endif   ;
184200080603
184300080617       // Se non devo riemettere la videata
184400080617 0     if ErrGenerico=*off   ;
184500080617
184600080526 1     if v01cric='U';
184700080415         $video='S1'  ;
184800080415         $InzS01 = *on;
184900080526 1     endif;
185000080603 1     if v01cric='F';
185100080603         $video='S3'  ;
185200080603         $InzS03 = *on;
185300080603 1     endif;
185400080617 0     endif;
185500080523
185600080414       ENDSR;
185700080603       //--------------------------------------------------------------
185800080603       //?Aggiornamento sfl per errore
185900080603       //--------------------------------------------------------------
186000080603       BEGSR ErrSFL2 ;
186100080603         errscelta=*on   ;
186200080603         SflNxtChg = *on ;
186300080603         errMessage  = *on;
186400080603         errGenerico = *on;
186500080603         update mk07s02;
186600080603
186700080603         errscelta=*off  ;
186800080603         SflNxtChg = *off ;
186900080603       ENDSR;
187000080411
187100080603       //--------------------------------------------------------------
187200080603       //?Gestione videata S03   - FUSIONE
187300080603       //--------------------------------------------------------------
187400080603       BEGSR GesS03;
187500080603
187600080603         // Inizializzazione videata
187700080603         if  $InzS03 = *on;
187800080624            clear forzacpo;
187900080603            exsr InzS03;
188000080603            $InzS03  = *off;
188100080609
188200080609         // Posizionamento cursore
188300080609           C03rcd = 1;
188400080609           C03csr = 1;
188500080603         endif;
188600080603
188700080603         // Emissione Testata e Piede con tasti funzionali abilitati
188800080603         if  errGenerico = *off;
188900080603           write  MK07T01;
189000080603           write  MK07P03;
189100080609          if c03csr>0 ;
189200080609          c03rcd=c03csr   ;
189300080609          endif;
189400080609         endif;
189500080603
189600080603         // Emissione videata
189700080603         exfmt  MK07C03;
189800080603
189900080603         reset errMessage;
190000080603         reset errGenerico;
190100080603         clear V1Dmsg;
190200080603         clear errFusione  ;
190300080618         clear errTipofus1 ;
190400080618         clear errTipofus2 ;
190500080603
190600080603 1       SELECT;
190700080603
190800080603         // - F3=Fine
190900080603 1         WHEN  dsp_aid = c_F03;
191000080603            $Fine = *on;
191100080603
191200080603         // - F12=Ritorno
191300080603 1         WHEN  dsp_aid = c_F12;
191400080617           // da sflcontrol vado a videata precednete
191500080617 1        if SflDSp_N=*on   ;
191600080617
191700080617             if nrrfus(1)>0 ;
191800080617                $Video = 'S2';
191900080617             else  ;
192000080617                $Video = 'D1';
192100080617                $inzd01=*on;
192200080617             endif ;
192300080617
192400080617          else  ;
192500080617           // da sfl vado a sflcontrol
192600080617 1        SflDSp_N=*on   ;
192700080617          $InzS03  = *on ;
192800080620          unlock tncpo01l ;
192900080620          unlock tncpo00F ;
193000080617          endif ;
193100080603
193200080603         // F7=Int.potenziali
193300080603 1         when dsp_aid = c_F07;
193400080604             exsr IntPotF   ;
193500080603 2           if  errGenerico = *on;
193600080603           write  MK07T01;
193700080603           write  MK07P03;
193800080603           endif  ;
193900080603
194000080603 x1      // Invio / F6
194100080603           OTHER;
194200080603             exsr ContrS03 ;
194300080603 2           if  errGenerico = *on;
194400080625 1            if SflDSp_N=*on   ;
194500080625               unlock tncpo00f ;
194600080625               unlock tncpo01l ;
194700080625              endif            ;
194800080603               leavesr;
194900080603 2           endif;
195000080603
195100080603         // F6=conferma FUSIONE
195200080603 1         if   dsp_aid = c_F06;
195300080619           exsr   ConfFUSIONE;
195400080603           endif  ;
195500080603
195600080603 1       ENDSL;
195700080603
195800080603       ENDSR;
195900080603       //--------------------------------------------------------------
196000080603       //?Inizializzazione videata S03 per  F U S I O N E
196100080603       //--------------------------------------------------------------
196200080603       BEGSR InzS03;
196300080603
196400080603       // Pulizia subfile
196500080603         SflDsp_N    = *on;
196600080603         SflDspCtl_N = *on;
196700080603         write  MK07C03;
196800080603         SflDspCtl_N = *off;
196900080603         SflEnd      = *off;
197000080603
197100080603         clear pagric;
197200080603         clear C03rcd;
197300080603         S03nrr=1 ;
197400080603         clear V1Dmsg;
197500080603         errMessage  = *off;
197600080603         errGenerico = *off;
197700080603         errScelta   = *off;
197800080603
197900080603       // SFL CONTROL
198000080603
198100080603          pulizpot1='S'  ;
198200080603          pulizpot2='S'  ;
198300080604          clear   v032tipf   ;
198400080604          clear   v032dtip    ;
198500080610          clear   v031tipf   ;
198600080610          clear   v031dtip    ;
198700080606          ColorRosso1=*off          ;
198800080606          ColorRosso2=*off          ;
198900080603          exsr    ClearCTL3 ;
199000080603
199100080603          SS=1  ;
199200080603          dow Fusione(ss)>0     ;
199300080603            kcpo=Fusione(ss) ;
199400080603            chain(N) kcpo   tncpo01l;
199500080603            if   %found(tncpo01l) ;
199600080603              s03nrr=ss  ;
199700080603              if ss=1    ;
199800080603              pulizpot1='C'  ;
199900080603              else       ;
200000080603              pulizpot2='C'  ;
200100080603              protpotdue=*on   ;
200200100525              endif      ;
200300080603              exsr RiempiCTL3;
200400080603            endif    ;
200500080603
200600080603          ss=ss+1  ;
200700080603          enddo    ;
200800080610          // Se ho selezionato solo un codice, mi posiziono sul secondo
200900080610          if fusione(2)=0  ;
201000080610          Errfusione=*on  ;
201100080610          endif   ;
201200080604
201300080603         SflDsp_N    = *on ;
201400080610         // Se SAVCPO impostato, vengo da manutenzione , per cui
201500080610         //  ricarico anche secondo potenziale
201600080610         If Fusione(2)=0 and  savcpo>0 ;
201700080625            kcpo=savcpo      ;
201800080625            chain(N) kcpo   tncpo01l;
201900080625            if   %found(tncpo01l) ;
202000080610            pulizpot2='C'  ;
202100080610            exsr RiempiCTL3;
202200080610            endif   ;
202300080625         endif   ;
202400100525
202500080603       ENDSR;
202600080603       //--------------------------------------------------------------
202700080603       //?Pulizia dati potenziali per fusione
202800080603       //--------------------------------------------------------------
202900080603          BEGSR   ClearCTL3  ;
203000080603          if pulizpot1='S'  ;
203100080603          clear   v031fcpo   ;
203200080603          clear   v031ncpo   ;
203300080603          clear   v031tipf   ;
203400080603          clear   v031dtip    ;
203500080603          clear   v031rag    ;
203600080603          clear   v031loc    ;
203700080603          clear   v031cap    ;
203800080603          clear   v031ind    ;
203900080603          clear   v031fil    ;
204000080603          clear   v031piv    ;
204100080603          clear   v031cdf    ;
204200080603          clear   v031cmerc  ;
204300080603          clear   v031dmerc  ;
204400080904          clear   c031cmuti  ;
204500080603          clear   v031cmm    ;
204600080603          clear   v031dcmm   ;
204700080603          clear   pulizpot1  ;
204800080603          clear   v031fsf    ;
204900111115          clear v031fls;
205000111115          clear v031cad;
205100111116          clear   codif1     ;
205200111116          RIcodif1=*off      ;
205300111115          $Contattato1 = *off;
205400080603          endif  ;
205500080603
205600080604          if pulizpot2='S'   ;
205700080603          clear   v032fcpo   ;
205800080603          clear   v032ncpo   ;
205900080604
206000080603          protpotdue=*off       ;
206100080603          clear   v032rag    ;
206200080603          clear   v032loc    ;
206300080603          clear   v032cap    ;
206400080603          clear   v032ind    ;
206500080603          clear   v032fil    ;
206600080603          clear   v032piv    ;
206700080603          clear   v032cdf    ;
206800080603          clear   v032cmerc  ;
206900080603          clear   v032dmerc  ;
207000080904          clear   c032cmuti  ;
207100080603          clear   v032cmm    ;
207200080603          clear   v032dcmm   ;
207300080603          clear   pulizpot2  ;
207400080603          clear   v032fsf    ;
207500111115          clear v032fls;
207600111115          clear v032cad;
207700111116          clear   codif2     ;
207800111116          RIcodif2=*off      ;
207900111115          $Contattato2 = *off;
208000080603          endif  ;
208100080603          ENDSR;
208200080603
208300080603       //--------------------------------------------------------------
208400080603       //?carica dati potenziale UNO del SFL3 - Fusione
208500080603       //--------------------------------------------------------------
208600080603          BEGSR   RiempiCTL3  ;
208700100525
208800100525        setll    kcpo   cnaco16l;
208900080603
209000080603       if pulizpot1='C' ;
209100111116
209200111116         if %equal(cnaco16l)  ;
209300111116           Codif1='S'  ;
209400111116           RICodif1=*on  ;
209500111116         else       ;
209600111116           Codif1='N'  ;
209700111116         endif       ;
209800100525
209900080603       v031fcpo=fcpo  ;
210000080603       v031ncpo=ncpo  ;
210100080603       v031rag=cporag;
210200080603       v031ind=cpovia;
210300080603       v031loc=cpocit;
210400080603       v031cap=cpocap;
210500080603       if cponaz=*blanks  ;
210600080603       %subst(v031loc:27:3)=' '+cpoprv;
210700080603       else  ;
210800080603       %subst(v031loc:26:4)=' '+cponaz;
210900080603       endif ;
211000080603
211100111115       //?Categoria potenziale
211200111116        clear V031fls;
211300111116        clear trmk05ds;
211400111116        IMK05cpo = CPOcpo;
211500111116        trmk05r (kpjba : TRMK05DS);
211600111116        IF  OMK05err = *blanks;
211700111116          V031fls = OMK05cat;
211800111116        ENDIF;
211900111115
212000111115       //?Prossima attività
212100111115         sav_CPOcpo = CPOcpo;
212200111115         exsr RIC_proATT;
212300111115         V031cad = sav_ATCcad;
212400080604
212500111115       //?Viste le nuove modifiche imposto 'F' se cat. eliminabile
212600111115         IF  V031fls = 'E';
212700080604           v031tipf='F'     ;
212800080610           v031dtip='FONDERE'  ;
212900111115         ENDIF;
213000111115
213100111115       //?Controllo se potenziale mai contattato
213200120914         dCPO01 = CPOrst;
213300120914         IF  §CPOdtpcon <> *blanks and §CPOdtpcon <> *zeros;
213400111115           $Contattato1 = *on;
213500111115         ENDIF;
213600080603
213700080603         v031fil=cpoflt;
213800080603         v031piv=cpopiv;
213900080603         v031cdf=cpocdf;
214000080603 2       if cpofsf='S' ;
214100080603          v031fsf='Sd';
214200080603         ELSE         ;
214300080603          v031fsf='Fi';
214400080603 2       endif;
214500080603
214600080603         v031cmerc=cposct;
214700080603         clear tblkey  ;
214800080603         tblkey=%editc(cposct:'X') ;
214900080603         chain  (1:'1L':tblkey) tabel00f;
215000080603 1       if %found(tabel00f);
215100080603         v031dmerc=tbluni;
215200080904         ds1l=     tbluni;
215300080904         c031cmuti=§1luti ;
215400080603         else ;
215500080603         v031dmerc=*all'?' ;
215600080904         c031cmuti='N'    ;
215700080603 1       endif  ;
215800080603
215900080603         v031cmm=cpocmm;
216000080604         if cpocmm> 0  ;
216100130916           chain  (CPOcmm)  AZCMM000;
216200130916 1         if %found(AZCMM01L);
216300130916             v031dcmm=CMMdes;
216400080604           else ;
216500130916             v031dcmm=*all'?' ;
216600080604 1         endif  ;
216700080604 1       endif  ;
216800080603
216900080603          clear   pulizpot1  ;
217000080603       endif     ;
217100080603
217200080603
217300080603       if pulizpot2='C' ;
217400111116
217500111116         if %equal(cnaco16l)  ;
217600111116           Codif2='S'  ;
217700111116           RICodif2=*on  ;
217800111116         else       ;
217900111116           Codif2='N'  ;
218000111116         endif       ;
218100100525
218200080603       v032fcpo=fcpo  ;
218300080603       v032ncpo=ncpo  ;
218400080603       v032rag=cporag;
218500080603       v032ind=cpovia;
218600080603       v032loc=cpocit;
218700080603       v032cap=cpocap;
218800080603       if cponaz=*blanks  ;
218900080603       %subst(v032loc:27:3)=' '+cpoprv;
219000080603       else  ;
219100080603       %subst(v032loc:26:4)=' '+cponaz;
219200080603       endif ;
219300111115
219400111115       //?Categoria potenziale
219500111116        clear V032fls;
219600111116        clear trmk05ds;
219700111116        IMK05cpo = CPOcpo;
219800111116        trmk05r (kpjba : TRMK05DS);
219900111116        IF  OMK05err = *blanks;
220000111116          V032fls = OMK05cat;
220100111116        ENDIF;
220200111115
220300111115       //?Prossima attività
220400111115         sav_CPOcpo = CPOcpo;
220500111115         exsr RIC_proATT;
220600111115         V032cad = sav_ATCcad;
220700111115
220800111115       //?Viste le nuove modifiche imposto 'F' se cat. eliminabile
220900111116         IF  V032fls = 'E';
221000080604           v032tipf='F'     ;
221100080610           v032dtip='FONDERE'  ;
221200080604           v031tipf='T'     ;
221300080610           v031dtip='TENERE '  ;
221400111115         ENDIF;
221500111115
221600111115       //?Controllo se potenziale mai contattato
221700120914         dCPO01 = CPOrst;
221800120914         IF  §CPOdtpcon <> *blanks and §CPOdtpcon <> *zeros;
221900111115           $Contattato2 = *on;
222000111115         ENDIF;
222100080603
222200080603         v032fil=cpoflt;
222300080603         v032piv=cpopiv;
222400080603         v032cdf=cpocdf;
222500080603 2       if cpofsf='S' ;
222600080603          v032fsf='Sd';
222700080603         ELSE         ;
222800080603          v032fsf='Fi';
222900080603 2       endif;
223000080603
223100080603         v032cmerc=cposct;
223200080603         clear tblkey  ;
223300080603         tblkey=%editc(cposct:'X') ;
223400080603         chain  (1:'1L':tblkey) tabel00f;
223500080603 1       if %found(tabel00f);
223600080603         v032dmerc=tbluni;
223700080904         ds1l=tbluni     ;
223800080904         c032cmuti=§1luti;
223900080603         else ;
224000080603         v032dmerc=*all'?' ;
224100080904         c032cmuti='N'     ;
224200080603 1       endif  ;
224300080603
224400080603         v032cmm=cpocmm;
224500080604         if cpocmm>0   ;
224600130916           chain  (CPOcmm)  AZCMM000;
224700130916 1         if %found(AZCMM01L);
224800130916             v032dcmm=CMMdes;
224900080604           else ;
225000130916             v032dcmm=*all'?' ;
225100080603 1       endif  ;
225200080604 1       endif  ;
225300100525         // Se presenti entrambi i potenziali, uno codificato e uno no,
225400100525         // devo TENERE il codificato
225500111116         if codif1='S' and codif2='N'  ;
225600100525           v032tipf='F'     ;
225700100525           v032dtip='FONDERE'  ;
225800100525           v031tipf='T'     ;
225900100525           v031dtip='TENERE '  ;
226000100525         endif   ;
226100111116         if codif1='N' and codif2='S'  ;
226200100525           v031tipf='F'     ;
226300100525           v031dtip='FONDERE'  ;
226400100525           v032tipf='T'     ;
226500100525           v032dtip='TENERE '  ;
226600100525         endif   ;
226700080603
226800080603          clear   pulizpot2  ;
226900080603       endif     ;
227000080603          ENDSR;
227100080603       //--------------------------------------------------------------
227200080603       //?Interrogazione potenziali per FUSIONE
227300080603       //--------------------------------------------------------------
227400080603           Begsr INTPOTF ;
227500080603
227600100805           clear trmk01ds   ;
227700080610           // se presente uno dei 2 imposto la ragione sociale
227800100805           mk01rag=%subst(v031rag:1:8)   ;
227900080610
228000100520           mk01k11='4'  ;
228100100520           kpjbu=trmK01ds;
228200100520           TRMK01R (kpjba) ;
228300100520           trmk01ds=kpjbu  ;
228400100520           d11cpo=mk01cpo   ;
228500080603
228600080603           // Selezionato potenziale
228700080603 1         if  d11cpo>0;
228800080603             chain(N)  d11cpo  TNCPO000;
228900080603 2            if not %found(tncpo01l) or  fusione(2)>0 or
229000080603               v032ncpo>0  ;
229100080603               errSEDE =*on;
229200080603               errMessage  = *on;
229300080603               errGenerico = *on;
229400080603               errFusione  = *on;
229500080603               V1Dmsg = Msg(19);
229600080603               %subst(v1dmsg:5:11)=%char(cpocpo) ;
229700080603               %subst(v1dmsg:17:12)=cporag ;
229800080603               leavesr ;
229900080603 2            endif  ;
230000080603
230100080603              // Carico dati
230200080603              kcpo=d11cpo   ;
230300080603              pulizpot2='S';
230400080603              exsr ClearCTL3 ;
230500080604              if kcpo<>savcpo;
230600080604               clear   v032tipf   ;
230700080604               clear   v032dtip    ;
230800080604               savcpo=kcpo    ;
230900080604              endif          ;
231000080603              pulizpot2='C';
231100080603              exsr RiempiCTL3 ;
231200080603            endif ;
231300080603
231400080603          ENDSR;
231500080603
231600080603       //--------------------------------------------------------------
231700080603       //?Carico i contatti nel sfl per fusione
231800080603       //--------------------------------------------------------------
231900080603          BEGSR   Carcontatti;
232000080606          s03nrr=1         ;
232100080606          s03tipo='C'      ;
232200080606          clear primariga  ;
232300080609          clear keysfl3    ;
232400080617          clear key3pot1   ;
232500080617          clear key3pot2   ;
232600080609          clear keynrr     ;
232700080609          clear keyrig     ;
232800080609          clear keycont    ;
232900080609          clear XX         ;
233000080604
233100080604          // Leggo tabella contatti e uno per uno vedo se c'e'
233200080604          // scrivo solo se trovato uno dei 2
233300080604          setll (1:'1T') tabel00f  ;
233400080604          reade (1:'1T') tabel00f  ;
233500080604
233600080604  1       dow    not %eof(tabel00f)  ;
233700080604          ds1t=tbluni                ;
233800080604
233900080604  2       if tblflg=' ' and §1trich='C'  ;
234000080604          ktnt=tblkey ;
234100080604
234200080604          // contatto potenziale 1
234300080604          EXSR LeggiNTC1                                  ;
234400080604
234500080604          // contatto potenziale 2
234600080604          EXSR LeggiNTC2                                  ;
234700080604
234800080604          ff=1    ;
234900080604  3       dow ntccod1(ff)<>*blanks or ntccod2(ff)<>*blanks  ;
235000080606
235100080606          // emetto riga descrittiva se caricato almeno un dato
235200080606          if primariga=' '   ;
235300090716          s03dec='-- R U B R I C A --' ;
235400081009          exsr  RigaDESCR  ;
235500080606          Primariga='S' ;
235600080606          endif         ;
235700080604
235800080604          s03dec=ktnt+'-'+§1tdes ;
235900080929          clear s03tpr           ;
236000080929          s03tpr=ktnt            ;
236100080606          s03righe=§1trig          ;
236200080620          clear s031nk2  ;
236300080620          clear s032nk2  ;
236400080604
236500080606          EXSR SCriviSFL3         ;
236600080604
236700080604          ff=ff+1    ;
236800080604  3       enddo     ;
236900080604
237000080604  2       endif               ;
237100080604          reade (1:'1T') tabel00f  ;
237200080604  1       enddo     ;
237300080604
237400080603          ENDSR;
237500080606       //--------------------------------------------------------------
237600080606       //?emetto prima riga descrittiva
237700080606       //--------------------------------------------------------------
237800080606          BEGSR RigaDESCR ;
237900080606          // Riga descrittiva
238000081009          noscelta1=*on   ;
238100081009          noscelta2=*on   ;
238200080606          in52=noscelta1  ;
238300080606          in53=noscelta2  ;
238400080606          Testatahi=*on   ;
238500080606          clear s03righe  ;
238600080606          clear s031sce   ;
238700080606          clear s032sce   ;
238800081010          Infoscelta1=*off;
238900081010          Infoscelta2=*off;
239000081010
239100081010          // Riga per info con campo scelta attivo
239200081010          if IndInfo=*on  ;
239300081010
239400081010           if ImpostaT='1'     ;
239500081010             s031sce='T'    ;
239600081010             SFLRosso1=*on   ;
239700081010           endif           ;
239800081010           if ImpostaT='2'     ;
239900081010             s032sce='T'    ;
240000081010             SFLRosso2=*on   ;
240100081010           endif           ;
240200081010           if w1EsInfo='N' ;
240300081010             s031sce='N'   ;
240400081010             Infoscelta1=*on;
240500081010           endif          ;
240600081010           if w1Esinfo<>'S' and w2EsInfo='S'  ;
240700081010             s031sce='N'   ;
240800081010             Infoscelta1=*on;
240900081010           endif   ;
241000081010           if w2EsInfo='N' ;
241100081010             s032sce='N'   ;
241200081010             Infoscelta2=*on;
241300081010           endif          ;
241400081010           if w2Esinfo<>'S' and w1EsInfo='S'  ;
241500081010             s032sce='N'   ;
241600081010             Infoscelta2=*on;
241700081010           endif   ;
241800081010
241900081010           // Per le infocomm nei campi in52 e in53 ci metto l'altro indicat
242000081010          in52=infoscelta1  ;
242100081010          in53=infoscelta2  ;
242200081010         endif          ;
242300081010
242400080606          clear s031imm   ;
242500081010          clear s032imm   ;
242600081010          if s03nrr=1 ;
242700081010            s031imm='MM/AA' ;
242800081010            s032imm='MM/AA' ;
242900081010          endif   ;
243000080606          clear s031cod   ;
243100080606          clear s032cod   ;
243200080606          clear s031nk2   ;
243300080606          clear s032nk2   ;
243400080610          clear s03forza  ;
243500080606          write   mk07s03     ;
243600080606          Testatahi=*off  ;
243700081010          Infoscelta1=*off;
243800081010          Infoscelta2=*off;
243900081010          ENDSR  ;
244000080604       //--------------------------------------------------------------
244100080604       //?Carico le info commerciali per fusione
244200080604       //--------------------------------------------------------------
244300080604          BEGSR   InfoComm   ;
244400081009          IndInfo=*on      ;
244500081009
244600080606          s03tipo='I'      ;
244700080606          clear primariga  ;
244800081010
244900081010          // Controllo di chi devo tenere le info comm   ;
245000081010          // vince il potenziale da Tenere se:
245100081010          //  - ha dati e sono nuovi
245200081010          //  - ha dati e sono vecchi ed il pot da Fondere ha dati
245300081010          //    vecchi o non ne ha
245400081010          // vince il potenziale da Fondere se:
245500081010          //  - ha dati e sono nuovi ed  il pot da Tenere ha dati
245600081010          //    vecchi o non ne ha
245700081010
245800081010          fcpo=v031fcpo      ;
245900081010          ncpo=v031ncpo      ;
246000081010          w1EsInfo='N'       ;
246100081010          setll     kcpo ticpi01l             ;
246200081010  1       if %equal(ticpi01l)  ;
246300081010           w1EsInfo='O'      ;
246400081010
246500081010          reade     kcpo ticpi01l             ;
246600081010  2       dow not %eof(ticpi01l)  and w1EsInfo<>'S' ;
246700081010  3       if cpitpf<>'10 ' and cpitpf<>'20 ' and cpitpf<>'30 ' and
246800081010             cpitpf<>'40 ' and cpitpf<>'OUT'  ;
246900081010             w1EsInfo='S'      ;
247000081010          else;
247100081010             reade     kcpo ticpi01l             ;
247200081010  3       endif;
247300081010  2       enddo     ;
247400081010  1       endif     ;
247500081010
247600081010          fcpo=v032fcpo      ;
247700081010          ncpo=v032ncpo      ;
247800081010          w2EsInfo='N'       ;
247900081010          setll     kcpo ticpi01l             ;
248000081010  1       if %equal(ticpi01l)  ;
248100081010           w2EsInfo='O'      ;
248200081010
248300081010          reade     kcpo ticpi01l             ;
248400081010  2       dow not %eof(ticpi01l)  and w2EsInfo<>'S' ;
248500081010  3       if cpitpf<>'10 ' and cpitpf<>'20 ' and cpitpf<>'30 ' and
248600081010             cpitpf<>'40 ' and cpitpf<>'OUT'  ;
248700081010             w2EsInfo='S'      ;
248800081010          else;
248900081010             reade     kcpo ticpi01l             ;
249000081010  3       endif;
249100081010  2       enddo     ;
249200081010  1       endif;
249300081010
249400081010          if v031tipf='T'     ;
249500081010          TieniEsInfo=w1EsInfo;
249600081010          FondiesInfo=w2EsInfo;
249700081010          else                ;
249800081010          TieniEsInfo=w2EsInfo;
249900081010          FondiesInfo=w1EsInfo;
250000081010          endif               ;
250100081010
250200081010          clear ImpostaT   ;
250300081010          ImpostaT='T'   ;
250400081010
250500081010          if   (TieniEsInfo='N' and FondiEsinfo<>'N')  or
250600081010               (TieniEsInfo='O' and FondiEsinfo='S')  ;
250700081010            ImpostaT='F'   ;
250800081010          endif            ;
250900081010
251000081010          if    TieniEsInfo='N' and FondiEsinfo='N'   ;
251100081010            ImpostaT='N'   ;
251200081010          else  ;
251300081010              select   ;
251400081010              when ImpostaT='T' and v031tipf='T' ;
251500081010            ImpostaT='1'   ;
251600081010              when ImpostaT='T' and v032tipf='T' ;
251700081010            ImpostaT='2'   ;
251800081010              when ImpostaT='F' and v031tipf='F' ;
251900081010            ImpostaT='1'   ;
252000081010              when ImpostaT='F' and v032tipf='F' ;
252100081010            ImpostaT='2'   ;
252200081010              endsl ;
252300081010          endif            ;
252400080604
252500081010          // se non ho info  --> non carico nulla
252600081010          if ImpostaT<>'N'   ;
252700081010
252800080604          // Leggo tabella DELLE INFO e uno per uno vedo se c'e'
252900080604          // scrivo solo se trovato uno dei 2
253000080929          yy=  1   ;
253100080929          dow   yy<=50   ;
253200080929  1       if    kifoord(yy) <>*blanks   ;
253300080929          ktprI=%subst(kifoord(yy):6:3) ;
253400080929          wdes =%subst(kifoord(yy):9:19) ;
253500080929          wIspp =%subst(kifoord(yy):28:1) ;
253600080929          // chain tabella per la decodifica
253700080604
253800080604          // contatto potenziale 1
253900080604          EXSR LeggiCPI1                                  ;
254000080604
254100080604          // contatto potenziale 2
254200080604          EXSR LeggiCPI2                                  ;
254300080604
254400080604          ff=1    ;
254500080929  3       dow ff<=40 and ntccod1(ff)<>*blanks or ntccod2(ff)<>*blanks  ;
254600080606
254700080606          // Emetto riga descrittiva se caricato almeno un dato
254800080606          if primariga=' '   ;
254900080610          s03dec='-INFO  COMMERCIALI-' ;
255000080606          if s03nrr>1   ;
255100080606          s03nrr=s03nrr+1  ;
255200080606          endif         ;
255300080606
255400080606          exsr  RigaDESCR  ;
255500080606          Primariga='S' ;
255600080606          endif         ;
255700080604
255800080929          clear s03tpr   ;
255900080929          s03tpr=ktprI  ;
256000141002
256100141002       //?Se INFO relativa al NON affidato a BRT metto n.a. davanti
256200141002          IF  S03tpr = 'NRF' or S03tpr = 'NRO' or S03tpr = 'NER' or
256300141002              S03tpr = 'NAE' or S03tpr = 'NTR';
256400141002            S03dec = 'N.A. ' + wdes;
256500141002          ELSE;
256600080929          s03dec=wdes    ;
256700141002          ENDIF;
256800180219
256900180219          //se INFO Spese totali e n. spedizioni (SPT)
257000180219          //modifico la descrizione
257100180219          IF  ktpri = 'SPT' and ff = 1;
257200180219            S03dec = 'Spese Totali';
257300180219          ENDIF;
257400180219          IF  ktpri = 'SPT' and ff = 2;
257500180219            S03dec = 'Spedizioni Totali';
257600180219          ENDIF;
257700141002
257800080929          if wIspp  =' '     ;
257900080606            s03righe=1               ;
258000080620            clear s031nk2    ;
258100080620            clear s032nk2    ;
258200080606          else   ;
258300080606            s03righe=90              ;
258400081013            s031nk2=sottot1(ff)       ;
258500081013            s032nk2=sottot2(ff)  ;
258600080606          endif  ;
258700080604
258800080606          EXSR  SCriviSFL3   ;
258900080604
259000080604          ff=ff+1    ;
259100080604  3       enddo     ;
259200080929          endif     ;
259300080929
259400080929          yy=yy+1    ;
259500080604  1       enddo     ;
259600081010
259700081010          endif     ;
259800080604
259900081009          IndInfo=*off     ;
260000080604          ENDSR;
260100140923
260200140923       //--------------------------------------------------------------
260300140923       //?Caricamento Dettaglio Prodotti per FUSIONE.                  ?
260400140923       //--------------------------------------------------------------
260500140923       BEGSR  sr_DettProd;
260600140923
260700140923         IndInfo = *on;
260800140923         clear  PrimaRiga;
260900140923         S03tipo = 'P';
261000140923
261100140923         Ktnt = 'DP';
261200140923         clear  Knk2;
261300140923         clear  NTCcod1;
261400140923         clear  NTCcod2;
261500140923         clear  DtImm1;
261600140923         clear  DtImm2;
261700140923
261800140923         chain  ( 1 : '1T' : Ktnt )  TABEL;
261900140923         if  %found(TABEL00F);
262000140923           ds1T = TBLuni;
262100140923         else;
262200140923           clear  ds1T;
262300140923           §1Tdes = *all'? ';
262400140923         endif;
262500140923
262600140923         // -?Reperimento Dettaglio Prodotti del 1° Potenziale.?
262700140923         clear  FF;
262800140923         Knk1 = %editc( V031fcpo : 'X' ) +
262900140923                %editc( V031ncpo : 'X' );
263000140923         setll     ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
263100140923         reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
263200140923
263300140923         DoW  Not  %eof(TFNTC01L);
263400140923
263500140923           FF += 1;
263600140923           DataDMY = %date( NTCntr : *YMD );
263700140923           w004a   = %subst( %editc( %dec( DataDMY ) : 'X' ) : 3 : 4 );
263800140923           DtImm1(FF)  = w004a;
263900140923           NTCcod1(FF) = NTCrnt;
264000140923
264100140923           reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
264200140923
264300140923         EndDo;
264400140923
264500140923         // -?Reperimento Dettaglio Prodotti del 2° Potenziale.?
264600140923         clear  FF;
264700140923         Knk1 = %editc( V032fcpo : 'X' ) +
264800140923                %editc( V032ncpo : 'X' );
264900140923         setll     ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
265000140923         reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
265100140923
265200140923         DoW  Not  %eof(TFNTC01L);
265300140923
265400140923           FF += 1;
265500140923           DataDMY = %date( NTCntr : *YMD );
265600140923           w004a   = %subst( %editc( %dec( DataDMY ) : 'X' ) : 3 : 4 );
265700140923           DtImm2(FF)  = w004a;
265800140923           NTCcod2(FF) = NTCrnt;
265900140923
266000140923           reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
266100140923
266200140923         EndDo;
266300140925
266400140925
266500140925         // -?Controllo di chi devo tenere le info comm?
266600140925         //  ?Vince il potenziale da Tenere se:?
266700140925         //  ?- ha dati?
266800140925         //  ?Vince il potenziale da Fondere se:?
266900140925         //  ?- ha dati ed il potenziale da Tenere NON ha dati?
267000140925         w1EsInfo = 'N';
267100140925         w2EsInfo = 'N';
267200140925         ImpostaT = 'T';
267300140925         Select;
267400140925           When  V031tipf = 'T'  and  NTCcod1(1) <> *blank;
267500140925             ImpostaT = '1';
267600140925             w1EsInfo = 'T';
267700140925           When  V032tipf = 'T'  and  NTCcod2(1) <> *blank;
267800140925             ImpostaT = '2';
267900140925             w2EsInfo = 'T';
268000140925           When  V031tipf = 'T'  and  NTCcod1(1) =  *blank
268100140925                                 and  NTCcod2(1) <> *blank;
268200140925             ImpostaT = '2';
268300140925             w2EsInfo = 'T';
268400140925           When  V032tipf = 'T'  and  NTCcod1(1) <> *blank
268500140925                                 and  NTCcod2(1) =  *blank;
268600140925             ImpostaT = '1';
268700140925             w1EsInfo = 'T';
268800140925         EndSl;
268900140925
269000140923
269100140923         // -?Scrittura record nel subfile?
269200140923         FF = 1;
269300140923         DoW  NTCcod1(FF) <> *blank  or  NTCcod2(FF) <> *blank;
269400140923
269500140923           // -?Emissione della riga descrittiva se caricato almeno un dato?
269600140923           If  PrimaRiga = *blank;
269700140923
269800140925             S03dec    = '-DESCRIZIONE PROD.-';
269900140923             if  S03nrr > 1;
270000140923               S03nrr += 1;
270100140923             endif;
270200140923             exsr  RigaDESCR;
270300140923             Primariga = 'S';
270400140923
270500140923           EndIf;
270600140923
270700140923           // -?Emissione del Dettaglio Prodotti dei 2 potenziali?
270800140923           S03dec = Ktnt + '-' + §1Tdes;
270900140923           S03tpr = Ktnt;
271000140923           S03righe = §1Trig;
271100140923           clear  S031nk2;
271200140923           clear  S032nk2;
271300140923
271400140923           exsr  ScriviSFL3;
271500140923
271600140923           FF += 1;
271700140923
271800140923         EndDo;
271900140923
272000140923         IndInfo = *off;
272100140923
272200140923       ENDSR;
272300140923
272400080606       //--------------------------------------------------------------
272500080606       //?Scrittura righe di dettaglio sfl3
272600080606       //--------------------------------------------------------------
272700080606         BEGSR ScriviSFL3   ;
272800081009
272900081009         // Memorizzo in skiera i codici con più di una riga ;
273000081009 1       if s03righe>1   ;
273100081009
273200081009 2       if   s031nk2<>*blanks   ;
273300081009           w007a=s03tpr +s031nk2 ;
273400081009           indx=%lookup(w007a:keysfl3)  ;
273500081009 3         if indx=0  ;
273600081009            xx=xx+1   ;
273700081009            keysfl3(xx)=w007a ;
273800081009            keyrig (xx)=s03righe   ;
273900081009 3         endif   ;
274000081009
274100081009            // Memorizzo solo una volta il codice
274200081009           indx=%lookup(s03tpr :key3pot1)  ;
274300081009 3         if indx=0   ;
274400081009              key3pot1(xx)=s03tpr              ;
274500081009              keynrr(xx) =s03nrr+1   ;
274600081009              // Se presenti per entrami lo memorizzo subito per
274700081009              //  avere il codice nella stessa posizione di skiera
274800081009              if s032nk2<>*blanks   ;
274900081009               key3pot2(xx)=s03tpr              ;
275000081009              endif                 ;
275100081009 3         endif   ;
275200081009 2       endif   ;
275300081009
275400081009 2       if   s032nk2<>*blanks   ;
275500081009           w007a=s03tpr+s032nk2 ;
275600081009           indx=%lookup(w007a:keysfl3)  ;
275700081009 3         if indx=0   ;
275800081009            xx=xx+1   ;
275900081009            keysfl3(xx)=w007a ;
276000081009            keyrig (xx)=s03righe   ;
276100081009 3         endif   ;
276200081009            // Memorizzo solo una volta il codice
276300081009           indx=%lookup(s03tpr :key3pot2)  ;
276400081009 3         if indx=0   ;
276500081009            key3pot2(XX)=s03tpr              ;
276600081009            keynrr(xx) =s03nrr+1   ;
276700081009 3         endif   ;
276800081009 2       endif   ;
276900081009
277000081009 2       if   s031nk2=*blanks and s032nk2=*blanks  ;
277100081009           w007a=s03tpr+'    '  ;
277200081009           indx=%lookup(w007a:keysfl3)  ;
277300081009 3         if indx=0   ;
277400081009            xx=xx+1   ;
277500081009            keysfl3(xx)=w007a ;
277600081009            keyrig (xx)=s03righe   ;
277700081009            keynrr(xx) =s03nrr+1   ;
277800081009              if ntccod1(ff)<>*blanks    ;
277900081009               key3pot1(xx)=s03tpr                     ;
278000081009              endif  ;
278100081009              if ntccod2(ff)<>*blanks    ;
278200081009               key3pot2(xx)=s03tpr                     ;
278300081009              endif  ;
278400081009 3         endif   ;
278500081009 2         endif   ;
278600081009 1       endif   ;
278700081009
278800081009         clear s031sce ;
278900081009         clear s032sce ;
279000081009
279100081009         if ntccod1(ff)<>*blanks            ;
279200081009         s031cod=ntccod1(ff)                 ;
279300081009         s031imm=%subst(dtimm1(ff):1:2)+'/' +%subst(dtimm1(ff):3:2)  ;
279400081009         in52='0'             ;
279500081009         noscelta1=*off  ;
279600081009         // Se scelto primo pote imposto la "T"
279700081009           if v031tipf='T'   ;
279800081009           s031sce='T'  ;
279900081009           SFLRosso1=*on ;
280000081009           endif    ;
280100081009         else   ;
280200081009         clear s031cod    ;
280300081009         clear s031imm    ;
280400081009         in52='1'             ;
280500081009         noscelta1=*on   ;
280600081009         clear s031nk2   ;
280700081009         endif   ;
280800081009
280900081009         if ntccod2(ff)<>*blanks            ;
281000081009         s032cod=ntccod2(ff)                 ;
281100081009         s032imm=%subst(dtimm2(ff):1:2)+'/' +%subst(dtimm2(ff):3:2)  ;
281200081009         in53='0'             ;
281300081009         noscelta2=*off  ;
281400081009
281500081009         // Se scelto 2° pot. o non presente riga per pot1 imposto la "T"
281600081009           if v032tipf='T' or (IndInfo=*off and s03righe=1 and noscelta1=*on);
281700081009           s032sce='T'  ;
281800081009           SFLRosso2=*on ;
281900081009           endif    ;
282000081009
282100081009           // Se più righe e 1°pot vuoto, vedo se devo impostare la "T"
282200081009           //   sul secondo potenziale
282300081009             if INdInfo=*off and v031tipf='T' and s03righe>1 and noscelta1=*on;
282400081009               Indx=%lookup(s03tpr :key3pot1)         ;
282500081009               if Indx=0   ;
282600081009                s032sce='T'  ;
282700081009                SFLRosso2=*on ;
282800081009               endif       ;
282900081009             endif   ;
283000081009
283100081009         else   ;
283200081009         clear s032cod    ;
283300081009         clear s032imm    ;
283400081009         in53='1'             ;
283500081009         noscelta2=*on   ;
283600081009         clear s032nk2   ;
283700081009
283800081009         // Se scelto 2° pot. e non presente riga per pot2 imposto la "T"
283900081009         //   nel 1° potenziale
284000081009           if IndInfo=*off and v032tipf='T' and s03righe=1 and noscelta1=*off;
284100081009           s031sce='T'  ;
284200081009           SFLRosso1=*on ;
284300081009           endif    ;
284400081009
284500081009           // Se più righe e 2°pot vuoto, vedo se devo impostare la "T"
284600081009           //   sul 1° potenziale
284700081009             if IndINfo=*off and v032tipf='T' and s03righe>1 and noscelta1=*off;
284800081009               Indx=%lookup(s03tpr:key3pot2)         ;
284900081009               if Indx=0   ;
285000081009                s031sce='T'  ;
285100081009                SFLRosso1=*on ;
285200081009               endif       ;
285300081009             endif   ;
285400081009
285500081009         endif   ;
285600081009
285700081009         clear s03forza  ;
285800081009
285900081009         s03nrr=s03nrr+1     ;
286000081009          // posizionamento cursore sulla prima riga
286100140923          if (s03tipo='C' and s03nrr=2) ;
286200081009            if noscelta1=*off    ;
286300081009            errsfl3_1=*on    ;
286400081009            else     ;
286500081009            errsfl3_2=*on    ;
286600081009            endif    ;
286700081009          endif    ;
286800081009
286900081009          // Per info proteggo sempre il campo di scelta
287000081009          if IndInfo          ;
287100081009          clear s031sce ;
287200081009          clear s032sce ;
287300081010          SFLRosso1=*off;
287400081010          SFLRosso2=*off;
287500081010            if ImpostaT='1';
287600081010             SFLRosso1=*on ;
287700081010            endif               ;
287800081010            if ImpostaT='2';
287900081010             SFLRosso2=*on ;
288000081010            endif               ;
288100081010          endif               ;
288200081009
288300081009          write mk07s03       ;
288400081009          errsfl3_1=*off   ;
288500081009          errsfl3_2=*off   ;
288600081009          SFLRosso1=*off   ;
288700081009          SFLRosso2=*off   ;
288800080606          ENDSR               ;
288900080604       //--------------------------------------------------------------
289000080604       //?Leggi contatti per potenziale 1 - Fusione
289100080604       //--------------------------------------------------------------
289200080604          BEGSR  LeggiNTC1                                ;
289300080604          clear ntccod1  ;
289400080605          clear DtImm1   ;
289500080604          clear FF       ;
289600080619          clear knk2     ;
289700080604
289800080604          knk1=%editc(v031fcpo:'X')+%editc(v031ncpo:'X')  ;
289900080619          setll    ('P':knk1:knk2:ktnt) tfntc01l             ;
290000080619          READE(N) ('P':knk1:knk2:ktnt) tfntc01l             ;
290100080604          dow not %eof(tfntc01l)  ;
290200080604          ff=ff+1              ;
290300080605          dataymd=%date(ntcntr:*ymd)    ;
290400080605          datadmy=dataymd ;
290500080605          w0060=%dec(datadmy)  ;
290600080605          w004a     =%subst(%editc(w0060:'X'):3:4) ;
290700080610          dtimm1(ff)=w004a                    ;
290800080605
290900080604          ntccod1(ff)=ntcrnt       ;
291000080619          READE(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
291100080604          enddo                ;
291200080604
291300080604          ENDSR                ;
291400080604       //--------------------------------------------------------------
291500080604       //?Leggi contatti per potenziale 2 - Fusione
291600080604       //--------------------------------------------------------------
291700080604          BEGSR  LeggiNTC2                                ;
291800080604          clear ntccod2  ;
291900080605          clear DtImm2   ;
292000080604          clear FF       ;
292100080604
292200080604          knk1=%editc(v032fcpo:'X')+%editc(v032ncpo:'X')  ;
292300080619          setll    ('P':knk1:knk2:ktnt) tfntc01l             ;
292400080604
292500080619          READE(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
292600080604          dow not %eof(tfntc01l)  ;
292700080604          ff=ff+1              ;
292800080605          dataymd=%date(ntcntr:*ymd)    ;
292900080605          datadmy=dataymd ;
293000080605          w0060=%dec(datadmy)  ;
293100080605          w004a     =%subst(%editc(w0060:'X'):3:4) ;
293200080610          dtimm2(ff)=     w004a                    ;
293300080605
293400080604          ntccod2(ff)=ntcrnt       ;
293500080619          reade(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
293600080604          enddo                ;
293700080604          ENDSR                ;
293800080604       //--------------------------------------------------------------
293900080604       //?Leggo info comm per potenziale 1 - Fusione
294000080604       //--------------------------------------------------------------
294100080604          BEGSR  LeggiCPI1                                ;
294200080604          clear ntccod1  ;
294300081013          clear sottot1  ;
294400080605          clear DtImm1   ;
294500080604          clear FF       ;
294600080604
294700080929          fcpo=v031fcpo      ;
294800080929          ncpo=v031ncpo      ;
294900080929          setll    (kcpo:ktprI) ticpi01l             ;
295000080929          READE(n) (kcpo:ktprI) ticpi01l             ;
295100081009
295200080929  1       dow not %eof(ticpi01l)  ;
295300081009  2       if cpiatb=' '        ;
295400081009
295500081009          EXSR ElaboraCPI      ;
295600081009
295700081009          ff=ff+1              ;
295800081009          dtimm1(ff)= w004a        ;
295900081009          ntccod1(ff)=wcodcpi      ;
296000081013          sottot1(ff)=wspf         ;
296100180219
296200180219          //se INFO Spese totali e n. spedizioni (SPT)
296300180219          //devo scrivere due righe nel subfile
296400180219            IF  ktpri = 'SPT' and ff = 1;
296500180219              ntccod1(ff) = 'Fatturato ' + %trim(%editc(wpft:'2'));
296600180219              ff += 1;
296700180219              dtimm1(ff) = w004a;
296800180219              ntccod1(ff) = 'Spedizioni ' + %trim(%editc(wsna:'4'));
296900180219             sottot1(ff) = wspf;
297000180219            ENDIF;
297100081009
297200081009  2       endif;
297300080604
297400080929          READE(n) (kcpo:ktprI) ticpi01l             ;
297500081009  1       enddo                ;
297600080604
297700080604          ENDSR                ;
297800080604       //--------------------------------------------------------------
297900080604       //?Leggo info comm per potenziale 2 - Fusione
298000080604       //--------------------------------------------------------------
298100080604          BEGSR  LeggiCPI2                                ;
298200080604          clear ntccod2  ;
298300081013          clear sottot2  ;
298400080605          clear DtImm2   ;
298500080604          clear FF       ;
298600080604
298700080929          fcpo=v032fcpo      ;
298800080929          ncpo=v032ncpo      ;
298900080929          setll    (kcpo:ktprI) ticpi01l             ;
299000080929          READE(n) (kcpo:ktprI) ticpi01l             ;
299100081009  1       dow not %eof(ticpi01l)  ;
299200081009  2       if cpiatb=' '        ;
299300081009
299400081009          EXSR ElaboraCPI      ;
299500081009
299600081009          ff=ff+1              ;
299700081009          dtimm2(ff)= w004a        ;
299800081009          ntccod2(ff)=wcodcpi      ;
299900081013          sottot2(ff)=wspf         ;
300000180219
300100180219          //se INFO Spese totali e n. spedizioni (SPT)
300200180219          //devo scrivere due righe nel subfile
300300180219            IF  ktpri = 'SPT' and ff = 1;
300400180219              ntccod2(ff) = 'Fatturato ' + %trim(%editc(wpft:'2'));
300500180219              ff += 1;
300600180219              dtimm2(ff) = w004a;
300700180219              ntccod2(ff) = 'Spedizioni ' + %trim(%editc(wsna:'4'));
300800180219             sottot2(ff) = wspf;
300900180219            ENDIF;
301000081009
301100081009  2       endif;
301200080605
301300080929          READE(n) (kcpo:ktprI) ticpi01l             ;
301400081009  1       enddo                ;
301500080604
301600080604          ENDSR                ;
301700081009       //--------------------------------------------------------------
301800081009       //?elaborazione per visualizzare    info comm
301900081009       //--------------------------------------------------------------
302000081009          BEGSR   ElaboraCPI   ;
302100081009
302200081009          dataiso=%date(cpidim)   ;
302300081009          datadmy=dataiso ;
302400081009          w0060=%dec(datadmy)  ;
302500081009          w004a     =%subst(%editc(w0060:'X'):3:4) ;
302600081009
302700081009          clear difo     ;
302800081009          clear wcodcpi  ;
302900081013          wspf=cpispf    ;
303000081009          // Se ha il sottotipo vedo se elencati o a scelta con '?'
303100081009  2       if cpispf<>*blanks   ;
303200081009            tbeke1=cpitpf        ;
303300081009            chain    ('IFO':tbeke1) tntbe01l             ;
303400081009  3         if %found(tntbe01l)   ;
303500081009            difo=tbeuni    ;
303600081009  3         endif          ;
303700081009
303800081009            // Decodifico il sottotipo
303900081009            tbeke2=cpiSPF        ;
304000081009            chain    ('IFS':tbeke1:TBEKE2) tntbe01l             ;
304100081009  3         IF %FOUND(TNTBE01L)  ;
304200081009  4            if §ifoifv='?'       ;
304300081009                 wcodcpi    =TBEUNI   ;
304400081009               else    ;
304500081009                 wcodcpi    =%subst(TBEUNI:1:15)   ;
304600081009  4            endif   ;
304700081009
304800081009  x3        ELSE    ;
304900081009             wcodcpi    =cpispf       ;
305000081009  3         endif   ;
305100081009  2         endif   ;
305200081009
305300081009  1       if cpispf=*blanks or §ifoifv='S'    ;
305400081009
305500081009  2         if cpifsn='N'        ;
305600081009            wcodcpi    =%trim(wcodcpi)+' NO'  ;
305700081009  2         endif               ;
305800081009  2         if cpifsn='S'        ;
305900081009            wcodcpi    =%trim(wcodcpi)+' SI'  ;
306000081009  2         endif               ;
306100081009
306200081009   2        if cpival>0  and cpitva='%  '    ;
306300081009            wcodcpi    =%trim(wcodcpi)+' '+%char(cpival) +'%' ;
306400081009            wcodcpi    =%trim(wcodcpi)  ;
306500081009   2        endif  ;
306600081009   2        if cpival>0  and cpitva='KG '    ;
306700081009            wcodcpi    =%trim(wcodcpi)+' KG'+%char(cpival) ;
306800081009            wcodcpi    =%trim(wcodcpi)  ;
306900081009   2        endif  ;
307000141002
307100141002   2        IF  CPIval > 0 and CPItva = *blanks;
307200141002              wcodcpi = %trim(wcodcpi)+ %char(cpival);
307300141002   2        endif  ;
307400081009
307500081009   2        if cpipft>0          ;
307600081009            wpft=cpipft   ;
307700081009            wcodcpi    =%trim(wcodcpi)+' Fat'+%char(wpft) ;
307800081009            wcodcpi    =%trim(wcodcpi)  ;
307900081009   2        endif;
308000081009
308100081009   2        if cpisna>0 ;
308200180219            wsna = CPIsna;
308300081009            wcodcpi    =%trim(wcodcpi)+' Spe'+%char(cpisna) ;
308400081009            wcodcpi    =%trim(wcodcpi)  ;
308500081009   2        endif;
308600081009
308700081009   1      endif;
308800081009          ENDSR;
308900080604       //--------------------------------------------------------------
309000080603       //?controlla SFL3  - Fusione
309100080603       //--------------------------------------------------------------
309200080606       BEGSR   contrS03    ;
309300080604
309400080617       // controllo nel sfl control solo fin quando non ho ancora
309500080609       //  caricato il sfl
309600080609
309700080609 1        if SflDSp_N=*on   ;
309800080609
309900080609       // Il secondo codice potenziale deve essere selezionato per la
310000080604       //  fusione
310100080604  1       if v032fcpo=0    and v032ncpo=0         ;
310200080604           errFusione  = *on;
310300080604           errMessage  = *on;
310400080604           errGenerico = *on;
310500080618           V1Dmsg = Msg(27);
310600080604           leavesr;
310700080604  1       endif    ;
310800080604
310900080604       // Non può essere lo stesso codice
311000080604  1       if v031fcpo=v032fcpo and v031ncpo=v032ncpo  ;
311100080604           errFusione  = *on;
311200080604           errMessage  = *on;
311300080604           errGenerico = *on;
311400080604           V1Dmsg = Msg(20);
311500080604           leavesr;
311600080604  1        endif    ;
311700080604
311800080604       // controllo il secondo codice e carico i dati
311900080604  1    if    fusione(2)=0    ;
312000080604
312100080604         fcpo=v032fcpo      ;
312200080604         ncpo=v032ncpo      ;
312300080604         chain(N)  Kcpo  TNCPO000;
312400080604
312500080604         // Codice potenziale NON trovato
312600080604 2       if  NOT  %found(TNCPO01L);
312700100420          if v032rag<>*blanks  ;
312800100420            pulizpot2='S';
312900100420            exsr ClearCTL3 ;
313000100420          endif   ;
313100100420           errFusione  = *on;
313200080604           errMessage  = *on;
313300080604           errGenerico = *on;
313400080618           V1Dmsg = Msg(27);
313500080604           leavesr;
313600080604  2      endif          ;
313700100420
313800080604         pulizpot2='S';
313900080604         exsr ClearCTL3 ;
314000080604         if kcpo<>savcpo;
314100080604          clear   v032tipf   ;
314200080604          clear   v032dtip    ;
314300080604         savcpo=kcpo    ;
314400080604         endif          ;
314500080604
314600080604         pulizpot2='C';
314700080604         exsr RiempiCTL3 ;
314800080604  1    endif  ;
314900100420
315000080603       // Non posso fondere 2 sedi
315100080606  1       if v031fsf='Sd' and v032fsf='Sd' ;
315200080603           errFusione  = *on;
315300080603           errMessage  = *on;
315400080603           errGenerico = *on;
315500080603           V1Dmsg = Msg(04);
315600080603           leavesr;
315700080604  1       endif    ;
315800080603
315900111115       //?Non posso fondere se 2 potenziali in cat. eliminabile
316000111115  1     IF  V031fls = 'E' and V032fls = 'E';
316100080603           errFusione  = *on;
316200080603           errMessage  = *on;
316300080603           errGenerico = *on;
316400080603           V1Dmsg = Msg(05);
316500080603           leavesr;
316600080604  1       endif    ;
316700080604
316800080604        // Decodifico il flag Tenere/Fondere  ;
316900080604        clear v031dtip   ;
317000080604        clear v032dtip   ;
317100080610
317200080610        // Richiesta la manutenzione
317300100520        clear trmk01ds  ;
317400080610        Indx=1              ;
317500080610          dow indx<=2        ;
317600080610          clear kcpo           ;
317700080610
317800080610          if  Indx=1 and v031tipf='M'   ;
317900080610          fcpo=v031fcpo  ;
318000080610          ncpo=v031ncpo  ;
318100080618          clear v031tipf   ;
318200080610          endif    ;
318300080610          if  Indx=2 and v032tipf='M'   ;
318400080610          fcpo=v032fcpo  ;
318500080610          ncpo=v032ncpo  ;
318600080618          clear v032tipf   ;
318700080610          endif    ;
318800080610
318900080610          if kcpo>0       ;
319000100520            clear trmk01ds  ;
319100100520            mk01k10='R'   ;
319200100520            mk01cpo=kcpo  ;
319300100520
319400100520            kpjbu =  trmk01ds;
319500100520            trmk02r ( kpjba : trmk01ds );
319600080618
319700080618            // se restituisce errore, lo visualizzo
319800100805              if mk01m10<>*blanks   ;
319900080618                if Indx=1   ;
320000080618                errtipoFus1 = *on;
320100080618                else    ;
320200080618                errtipoFus2 = *on;
320300080618                endif   ;
320400080618                errMessage  = *on;
320500080618                errGenerico = *on;
320600080618                V1Dmsg = Msg(26);
320700080618              leavesr;
320800080618              else   ;
320900080618               $InzS03  = *on ;
321000080618              endif  ;
321100080610          endif   ;
321200080610
321300080610          Indx=indx+1 ;
321400080610          enddo   ;
321500080610
321600080610          // Se manutenzione riemetto videata per ricarica i dati
321700080610          if $inzs03=*on  ;
321800080610          leavesr  ;
321900080610          endif    ;
322000080604
322100080604          if v031tipf='T'   ;
322200080610          v031dtip='TENERE '  ;
322300080604          endif   ;
322400080604          if v031tipf='F'   ;
322500080610          v031dtip='FONDERE'  ;
322600080604          endif   ;
322700080604
322800080604          if v032tipf='T'   ;
322900080610          v032dtip='TENERE '  ;
323000080604          endif   ;
323100080604          if v032tipf='F'   ;
323200080610          v032dtip='FONDERE'  ;
323300080604          endif   ;
323400080604
323500080604          if (v031tipf='T' and v032tipf='T') or
323600080604            (v031tipf='F' and v032tipf='F')  or
323700080618             v031tipf=' ';
323800080610           errtipoFus1 = *on;
323900080604           errMessage  = *on;
324000080604           errGenerico = *on;
324100080604           V1Dmsg = Msg(21);
324200080604           leavesr;
324300080604          endif ;
324400080618           if  v032tipf=' ';
324500080618           errtipoFus2 = *on;
324600080618           errMessage  = *on;
324700080618           errGenerico = *on;
324800080618           V1Dmsg = Msg(21);
324900080618           leavesr;
325000080618          endif ;
325100080610
325200111115       //?Non posso tenere un potenziale in cat. eliminabile
325300111115        if v031tipf='T' and v031fls = 'E' ;
325400080610           errtipoFus1 = *on;
325500080610           errMessage  = *on;
325600080610           errGenerico = *on;
325700080610           V1Dmsg = Msg(25);
325800080610           leavesr;
325900080610        endif   ;
326000111115        if v032tipf='T' and v032fls = 'E' ;
326100080610           errtipoFus2 = *on;
326200080610           errMessage  = *on;
326300080610           errGenerico = *on;
326400080610           V1Dmsg = Msg(25);
326500080610           leavesr;
326600080610        endif   ;
326700100525
326800100525        //  se un potenziale codificato e uno no, devo TENERE per forza
326900100525        //     quello codificato
327000111116        if codif1='S' and codif2='N'  and v031tipf='F' ;
327100100525           errtipoFus1 = *on;
327200100525           errMessage  = *on;
327300100525           errGenerico = *on;
327400100525           V1Dmsg = Msg(43);
327500100525           leavesr;
327600100525        endif   ;
327700111116        if codif1='N' and codif2='S'  and v032tipf='F' ;
327800100525           errtipoFus1 = *on;
327900100525           errMessage  = *on;
328000100525           errGenerico = *on;
328100100525           V1Dmsg = Msg(43);
328200100525           leavesr;
328300100525        endif   ;
328400080619
328500080619        // Uno dei 2 potenziali deve essere in gestione all'utente
328600080619        Indx =%lookup(v031fil:pog)   ;
328700080619        Indx2=%lookup(v032fil:pog)   ;
328800080619         if Indx=0 and Indx2=0  ;
328900080619           errtipoFus1 = *on;
329000080619           errMessage  = *on;
329100080619           errGenerico = *on;
329200080619           V1Dmsg = Msg(29);
329300080619           leavesr;
329400080619          endif       ;
329500080619
329600111115       // Se quello che fondo non è mio
329700111115       //?il potenziale deve essere un mai contattato
329800111115       // Senza anagrafiche  SEMPRE
329900111115        if v031tipf='F' and Indx=0 and $Contattato1;
330000081010             fcpo=v031fcpo    ;
330100081010             ncpo=v031ncpo    ;
330200081010             setll kcpo  cnaco16l  ;
330300081010             setll kcpo  tfaco16l  ;
330400081010             if %equal(cnaco16l) or %equal(tfaco16l) ;
330500081010             errtipoFus1 = *on;
330600081010             errMessage  = *on;
330700081010             errGenerico = *on;
330800100524             V1Dmsg = Msg(42);
330900081010             leavesr;
331000081010          endif    ;
331100080619        endif       ;
331200081010
331300111115        if v032tipf='F' and Indx2=0 and $Contattato2;
331400081010             fcpo=v032fcpo    ;
331500081010             ncpo=v032ncpo    ;
331600081010             setll kcpo  cnaco16l  ;
331700081010             setll kcpo  tfaco16l  ;
331800081010             if %equal(cnaco16l) or %equal(tfaco16l) ;
331900081010             errtipoFus2 = *on;
332000081010             errMessage  = *on;
332100081010             errGenerico = *on;
332200100524             V1Dmsg = Msg(42);
332300081010             leavesr;
332400081010           endif       ;
332500081010        endif       ;
332600080904
332700081010        //  Se lo posso manutenzionare, il potenziale da Tenere non deve avere
332800081010        //  un settore merceologico NON CLASSIFICATO
332900080904        if v031tipf='T'  and Indx>0  and c031cmuti='N'   ;
333000080904           errtipoFus1 = *on;
333100080904           errMessage  = *on;
333200080904           errGenerico = *on;
333300080904           V1Dmsg = Msg(35);
333400080904           leavesr;
333500080904        endif       ;
333600080904        if v032tipf='T'  and Indx2>0  and c032cmuti='N'   ;
333700080904           errtipoFus2 = *on;
333800080904           errMessage  = *on;
333900080904           errGenerico = *on;
334000080904           V1Dmsg = Msg(35);
334100080904           leavesr;
334200080904        endif       ;
334300080619
334400080619       // Memorizzo il codice da tenere ;
334500080619       if v031tipf='T'  ;
334600080619        fcpo=v031fcpo    ;
334700080619        ncpo=v031ncpo    ;
334800080619       else   ;
334900080619        fcpo=v032fcpo    ;
335000080619        ncpo=v032ncpo    ;
335100080619       endif   ;
335200080619
335300080619        Tienicpo=kcpo  ;
335400080619
335500080619       // Memorizzo il codice da fondere ;
335600080619       if v031tipf='T'  ;
335700080619        fcpo=v032fcpo    ;
335800080619        ncpo=v032ncpo    ;
335900080619        Fondirag=v032rag  ;
336000080619       else   ;
336100080619        fcpo=v031fcpo    ;
336200080619        ncpo=v031ncpo    ;
336300080619        Fondirag=v031rag  ;
336400080619       endif   ;
336500080619
336600080619        Fondicpo=kcpo  ;
336700100521
336800100525       // Il potenziale da Fondere non deve avere una trattativa aperta non fittizia
336900100521
337000100521           clear tnta43ds  ;
337100100525           ita43cpo=fondicpo   ;
337200100521
337300100521           TNTA43R (TNTA43DS)  ;
337400100521
337500100521           if ota43nrv>0 or ota43err<>' '   ;
337600100521           if v031tipf='F'  ;
337700100521            errtipoFus1 = *on;
337800100521           ELSE   ;
337900100521            errtipoFus2 = *on;
338000100521           ENDIF   ;
338100100521           errMessage  = *on;
338200100521           errGenerico = *on;
338300100521           V1Dmsg = Msg(39);
338400100521
338500100521           leavesr;
338600100521           endif   ;
338700080624
338800080624        // Se quello che fondo ha anagrafiche o visite, avviso
338900100521  2     if Fondicpo<>forzacpo;
339000080624        forzacpo=fondicpo   ;
339100100521        clear  v1dmsg   ;
339200080624        setll fondicpo  cnaco16l  ;
339300100521
339400100805        clear tnta43ds  ;
339500100805        ita43cpo=Fondicpo   ;
339600100521
339700100805        TNTA43R (TNTA43DS)  ;
339800100805   3    if %equal(cnaco16l) or ota43nrv>0       ;
339900100805   4    select   ;
340000100521        when %equal(cnaco16l) and ota43nrv>0       ;
340100100521           V1Dmsg = Msg(41);
340200100521        when %equal(cnaco16l) ;
340300100521           V1Dmsg = Msg(32);
340400100521        when ota43nrv>0       ;
340500100521           V1Dmsg = Msg(40);
340600100805   4    endsl                 ;
340700100805   3    endif  ;
340800100521
340900100521
341000100521   3   if v1Dmsg<>*blanks    ;
341100100521   4   if v031tipf='F'  ;
341200080624           errtipoFus1 = *on;
341300080624       else  ;
341400080624           errtipoFus2 = *on;
341500100521   4   endif ;
341600080624           errMessage  = *on;
341700080624           errGenerico = *on;
341800080624           leavesr;
341900100521   3    endif   ;
342000100521
342100100521   2    endif   ;
342200100521
342300080604
342400080619        // Alloco i 2 record potenziali--> nessuno li deve aggiornare
342500120622       clear dCPO01;
342600080624       chain(n)  Fondicpo  TNCPO000;
342700080619       chain(e)  cp1nrr    TNCPO00F;
342800080619 2     if not %found(tncpo00F) or %error;
342900080619         if v032tipf='F'   ;
343000080619         errFusione = *on;
343100080619         else   ;
343200080619         errtipoFus1 = *on;
343300080619         endif  ;
343400080619
343500080619         errMessage  = *on;
343600080619         errGenerico = *on;
343700080619         if %error    ;
343800080619         V1Dmsg = Msg(30);
343900080619         else   ;
344000080619         V1Dmsg = Msg(31);
344100080619         endif   ;
344200080619       leavesr  ;
344300080619       endif    ;
344400120622
344500120622       //?Salvo la data primo contatto del Potenziale
344600120622       dCPO01 = CPOrst;
344700120702       IF  §CPOdtpcon > *zeros;
344800120622         DataPrimoConF = %dec(§CPOdtpcon:8:0);
344900120622       ELSE;
345000120622         clear DataPrimoConF;
345100120622       ENDIF;
345200080619
345300120622       clear dCPO01;
345400080619       chain(e)  Tienicpo  TNCPO000;
345500080619 2     if not %found(tncpo01l) or %error;
345600080619         if v032tipf='F'   ;
345700080619         errFusione = *on;
345800080619         else   ;
345900080619         errtipoFus1 = *on;
346000080619         endif  ;
346100080619
346200080619         errMessage  = *on;
346300080619         errGenerico = *on;
346400080619         if %error    ;
346500080619         V1Dmsg = Msg(30);
346600080619         else   ;
346700080619         V1Dmsg = Msg(31);
346800080619         endif   ;
346900080619       leavesr  ;
347000080619       endif   ;
347100120622
347200120622       //?Salvo la data primo contatto del Potenziale
347300120622       dCPO01 = CPOrst;
347400120702       IF  §CPOdtpcon > *zeros;
347500120622         DataPrimoConT = %dec(§CPOdtpcon:8:0);
347600120622       ELSE;
347700120622         clear DataPrimoConT;
347800120622       ENDIF;
347900080604
348000080619        // Se non ci sono errori carico i dati nel sfl
348100080619
348200080610        // C O N T A T T I
348300080604          exsr Carcontatti  ;
348400080610        // INFO COMM
348500080604          exsr Infocomm     ;
348600140923        // -?Dettaglio Prodotti?
348700140923          exsr  sr_DettProd;
348800140923
348900080610       // se non ho caricato nessuna informazione, emetto riga descrittiva
349000080610          if s03nrr=1     ;
349100081114          clear s03tipo   ;
349200080610          s03dec='Dati  non  presenti' ;
349300081009          exsr  RigaDESCR  ;
349400080610          endif   ;
349500080610
349600081009          SflDsp_N    = *off;
349700080606
349800080606 2        if v031tipf='T'   ;
349900080606          ColorRosso1=*on           ;
350000080606          else      ;
350100080606          ColorRosso2=*on           ;
350200080606 2        endif     ;
350300080609
350400080606 x2       else      ;
350500080606
350600080606        // Controlla scelte SFL   ;
350700080606         Exsr Contrsfl3           ;
350800080606 1       endif    ;
350900080604
351000080606       ENDSR;
351100100521       //--------------------------------------------------------------
351200100521       //?controlla scelte SFL 3
351300100521       //--------------------------------------------------------------
351400100521       BEGSR contrSFL3  ;
351500080606       clear ricint  ;
351600081013       clear wscelta ;
351700081010       s03nrr=1 ;
351800080609       clear keycont  ;
351900080606
352000081013       // Prima controllo la V= Visualizzazione estesa
352100080606        chain  s03nrr mk07s03  ;
352200080606 1      dow   %found            ;
352300080606
352400080606        clear updatesfl  ;
352500080606 2      if s031sce='V'        ;
352600080606
352700080606        // Visulizza info commerciali    ;
352800080609 3       if s03tipo='I'       ;
352900080929         CLEAR     TRMK50DS   ;
353000080929         fcpo=v031fcpo      ;
353100080929         ncpo=v031ncpo      ;
353200080929         i50cpo=kcpo ;
353300080929         i50mod='I'   ;
353400080929         i50rag=v031rag   ;
353500080929         i50pgm='TRMK07R' ;
353600080606
353700080929         TRMK50R  (kpjba:trmk50ds) ;
353800080609 3       endif    ;
353900080606
354000080606        // Visulizza contatti            ;
354100080609 3       if s03tipo='C'       ;
354200080606         clear tnta12ds   ;
354300080606         ta12ric='V'   ;
354400080606         ta12apl='P'   ;
354500080606         fcpo=v031fcpo  ;
354600080606         ncpo=v031ncpo  ;
354700080606         ta12pot=kcpo ;
354800080606         ta12rag=v031rag  ;
354900080606         TNTA12R  (kpjba:TNTA12ds);
355000080609 3       endif    ;
355100140923
355200140923        // -?Visualizzazione Dettaglio Prodotti?
355300140923         if  S03tipo = 'P';
355400140923           errMessage  = *on;
355500140923           errGenerico = *on;
355600140923           V1Dmsg = Msg(37);
355700140923           exsr UpdateSFL3;
355800140923           leavesr;
355900140923         endif;
356000080606
356100080606         clear    s031sce  ;
356200080606         Ricint='S'   ;
356300080606         Updatesfl='S'   ;
356400080606 2      endif             ;
356500080606
356600080606
356700080606 2      if s032sce='V' ;
356800080606
356900080606        // Visulizza info commerciali    ;
357000080609 3       if s03tipo='I'       ;
357100080929         CLEAR     TRMK50DS   ;
357200080929         fcpo=v032fcpo      ;
357300080929         ncpo=v032ncpo      ;
357400080929         i50cpo=kcpo ;
357500080929         i50mod='I'   ;
357600080929         i50rag=v031rag   ;
357700080929         i50pgm='TRMK07R' ;
357800080606
357900080929         TRMK50R  (kpjba:trmk50ds) ;
358000080609 3       endif   ;
358100080606
358200080606        // Visulizza contatti            ;
358300080609 3       if s03tipo='C'       ;
358400080606         clear tnta12ds   ;
358500080606         ta12ric='V'   ;
358600080606         ta12apl='P'   ;
358700080606         fcpo=v032fcpo  ;
358800080606         ncpo=v032ncpo  ;
358900080606         ta12pot=kcpo ;
359000080606         ta12rag=v032rag  ;
359100080606         TNTA12R  (kpjba:TNTA12ds);
359200080609 3       endif    ;
359300140923
359400140923        // -?Visualizzazione Dettaglio Prodotti?
359500140923         if  S03tipo = 'P';
359600140923           errMessage  = *on;
359700140923           errGenerico = *on;
359800140923           V1Dmsg = Msg(37);
359900140923           exsr UpdateSFL3;
360000140923           leavesr;
360100140923         endif;
360200080606
360300080606         clear    s032sce  ;
360400080606         Ricint='S'   ;
360500080929         Updatesfl='S'   ;
360600080606 2      endif             ;
360700080606
360800080606 2      if Updatesfl='S'             ;
360900080609        exsr UPDATESFL3  ;
361000080606 2      endif           ;
361100080606
361200080606        s03nrr=s03nrr+1 ;
361300080606        chain  s03nrr mk07s03  ;
361400080606 1      enddo    ;
361500080606
361600080606        // SE non ho richiesto la visulizzazione estesa
361700080606        //  eseguo i controlli di scelta
361800080606 1      if ricint=' '  ;
361900081010        s03nrr=1 ;
362000080606
362100080606        chain  s03nrr mk07s03  ;
362200080606 2      dow   %found            ;
362300080609
362400140923        // -?C O N T A T T I?
362500140923 2A     IF  S03TIPO = 'C';
362600081010
362700081010        // Se più di una riga ammessa             ;
362800080609 3      if s03righe>1   ;
362900080609
363000080609        // con sottotipo
363100080609 4      if s031nk2<>*blanks and s031sce='T';
363200080929        w007a=s03tpr+s031nk2 ;
363300080929        xx  =%lookup(w007a:keysfl3) ;
363400080609 5      if keycont(xx)=0;
363500080609           keycont(xx)=90 ;
363600080609 x5     else    ;
363700080609          errMessage  = *on;
363800080609          errGenerico = *on;
363900080609          V1Dmsg = Msg(24);
364000080609          %subst(v1dmsg:44:4)=%subst(s031cod:1:4)  ;
364100080609          %subst(v1dmsg:54:24)=s03dec               ;
364200080609 5      endif   ;
364300080609 4      endif   ;
364400080609 4      if s032nk2<>*blanks and s032sce='T';
364500080929        w007a=s03tpr+s032nk2 ;
364600080929        xx  =%lookup(w007a:keysfl3) ;
364700080609 5      if keycont(xx)=0;
364800080609           keycont(xx)=90 ;
364900080609 x5     else    ;
365000080609          errMessage  = *on;
365100080609          errGenerico = *on;
365200080609          V1Dmsg = Msg(24);
365300080609          %subst(v1dmsg:44:4)=%subst(s032cod:1:4)  ;
365400080609          %subst(v1dmsg:54:24)=s03dec               ;
365500080609 5      endif   ;
365600080609 4      endif   ;
365700080609
365800080617 4      if s031nk2= *blanks and s032nk2=*blanks ;
365900080929        w007a=s03tpr+'    '  ;
366000080929        xx  =%lookup(w007a:keysfl3) ;
366100080609 5      if xx>0    ;
366200080609
366300080609 6        if   s031sce='T'   ;
366400080609          keycont(xx)=keycont(xx)+1;
366500080609 6        endif   ;
366600080609
366700080609 6        if   s032sce='T'   ;
366800080609          keycont(xx)=keycont(xx)+1;
366900080609 6        endif                    ;
367000080609 5      endif     ;
367100080609
367200080609 5      if keycont(xx) > keyrig(xx)  ;
367300080609        errMessage  = *on;
367400080609        errGenerico = *on;
367500080609        V1Dmsg = Msg(22);
367600080609        %subst(v1dmsg:32:2)=%editc(s03righe:'X') ;
367700080609        %subst(v1dmsg:35:5)='righe'              ;
367800140923 6      select;
367900140923 6        when  S03tipo = 'C';
368000140923            %subst(v1dmsg:45:19)='la RUBRICA         ' ;
368100140923 6        when  S03tipo = 'I';
368200140923            %subst(v1dmsg:45:19)='la Info Commerciale' ;
368300140923 6        when  S03tipo = 'P';
368400140923            %subst(v1dmsg:45:19)='il Dettaglio Prod. ' ;
368500140923 6      endsl    ;
368600080609        v1dmsg =%trim(v1dmsg)+' ' +s03dec    ;
368700080609 5      endif     ;
368800080617
368900080617 x4     else      ;
369000080929           xx  =%lookup(s03tpr  :key3pot1) ;
369100080617            if xx=0    ;
369200080929             xx  =%lookup(s03tpr:key3pot2) ;
369300080617            endif      ;
369400080617
369500080617 5      if xx>0    ;
369600080617 6        if   s031sce='T'   ;
369700080617          keycont(xx)=keycont(xx)+1;
369800080617 6        endif   ;
369900080617
370000080617 6        if   s032sce='T'   ;
370100080617          keycont(xx)=keycont(xx)+1;
370200080617 6        endif                    ;
370300080617 5      endif                    ;
370400080617
370500080609 4      endif     ;
370600080609 3      endif     ;
370700080609
370800080609 3      if (s03righe=1 and s031sce='T' and s032sce='T' );
370900080609        errMessage  = *on;
371000080609        errGenerico = *on;
371100080609        V1Dmsg = Msg(22);
371200080609        %subst(v1dmsg:32:2)=%editc(s03righe:'X') ;
371300080609        %subst(v1dmsg:35:5)='riga '              ;
371400140923        select;
371500140923 4        when  S03tipo = 'C';
371600140923            %subst(v1dmsg:45:19)='la RUBRICA         ' ;
371700140923 4        when  S03tipo = 'I';
371800140923            %subst(v1dmsg:45:19)='la Info Commerciale' ;
371900140923 4        when  S03tipo = 'P';
372000140923            %subst(v1dmsg:45:19)='il Dettaglio Prod. ' ;
372100140923 4      endsl    ;
372200080609        v1dmsg =%trim(v1dmsg)+' '+s03dec    ;
372300080609 3      endif    ;
372400080609
372500080609 3      if s03righe=1 and s031sce=' ' and s032sce=' ' and s03forza=' ';
372600080609        errMessage  = *on;
372700080609        errGenerico = *on;
372800080609        V1Dmsg = Msg(23);
372900140923        select;
373000140923 4        when  S03tipo = 'C';
373100140923            %subst(v1dmsg:34:11)='la Rubrica '  ;
373200140923 4        when  S03tipo = 'I';
373300140923            %subst(v1dmsg:34:11)='la InfoComm' ;
373400140923 4        when  S03tipo = 'P';
373500140923            %subst(V1Dmsg:34:11)='il Dett.Pr.' ;
373600140923 4      endsl;
373700080609        %subst(v1dmsg:46:19)=s03dec   ;
373800080609 3      endif    ;
373900081013 2A     endif           ;
374000081013
374100140923        // -?I N F O   C O M M   e   D E T T A G L I O   P R O D .?
374200081013        //   Visualizzo tutto in giallo a seconda di dove è la T
374300140923 2A     IF  S03TIPO = 'I'  or  S03tipo = 'P';
374400081013        indInfo=*on      ;
374500081013
374600081013        // Riga di testa: controllo se c'e' la scelta
374700081013 3      if s031cod=*blanks and s032cod=*blanks;
374800081013
374900081013        // Se entrambe vuote o entrambe piene errore
375000081013 4         if (s031sce<>'T'  and s032sce<>'T') or
375100081013 4            (s031sce= 'T'  and s032sce= 'T') ;
375200081013            errMessage  = *on;
375300081013            errGenerico = *on;
375400081013            V1Dmsg = Msg(36);
375500081013 x4        else;
375600081013             // Memorizzo cosa scelto per evidenziarlo in giallo
375700081013 5           if s031sce='T' ;
375800081013              wscelta='1'    ;
375900081013             else;
376000081013              wscelta='2'    ;
376100081013 5           endif;
376200081013 4         endif    ;
376300081013 3         endif    ;
376400081013 2a        endif    ;
376500080609
376600080609        // Aggiorno SFL
376700080609        exsr UpdateSFL3  ;
376800080609
376900080609 3      if ErrGenerico=*on            ;
377000080609         leavesr;
377100080609 3      endif           ;
377200080609
377300080606        s03nrr=s03nrr+1 ;
377400080606        chain  s03nrr mk07s03  ;
377500080606 2      enddo    ;
377600080609
377700140923        // -?PER CONTATTI: controllo eventuali righe >1 senza scelta?
377800080609        xx=1   ;
377900081013 2      dow keysfl3(xx)<>*blanks  ;
378000080609
378100080609 3      if keynrr(XX) >0    ;
378200080609        s03nrr=keynrr(xx)  ;
378300080609        chain s03nrr   mk07s03   ;
378400080609
378500140923 4      if s03tipo='C' and  s03forza=' ' and keycont(xx)=0  ;
378600080609          errMessage  = *on;
378700080609          errGenerico = *on;
378800080609          V1Dmsg = Msg(23);
378900140923          select;
379000140923 5          when  S03tipo = 'C';
379100140923              %subst(v1dmsg:34:11)='la Rubrica ';
379200140923 5          when  S03tipo = 'I';
379300140923              %subst(v1dmsg:34:11)='la InfoComm';
379400140923 5          when  S03tipo = 'P';
379500140923              %subst(V1Dmsg:34:11)='il Dett.Pr.';
379600140923 5        endsl;
379700080609          %subst(v1dmsg:46:19)=s03dec   ;
379800080609
379900080609        // Aggiorno SFL
380000080609        exsr UpdateSFL3  ;
380100080609
380200080609 5      if ErrGenerico=*on            ;
380300080609         leavesr;
380400080609 5      endif           ;
380500080609 4      endif           ;
380600080609 3      endif           ;
380700080609
380800080609        xx=xx+1   ;
380900081013 2      enddo    ;
381000080609 1      endif    ;
381100080606
381200080606        ENDSR;
381300080609       //--------------------------------------------------------------
381400080609       //?Aggiorno SFL3
381500081013       //--------------------------------------------------------------
381600080609        BEGSR   UpdateSFL3  ;
381700080609
381800140923        //if s03tipo='I' and s031cod=*blanks and s032cod=*blanks ;
381900140923        if  (S03tipo = 'I'      or   S03tipo = 'P')  and
382000140923             S031cod = *blanks  and  S032cod = *blanks ;
382100081013          Testatahi=*on   ;
382200081013          Infoscelta1=in52  ;
382300081013          Infoscelta2=in53  ;
382400081013        else     ;
382500081013          noscelta1=in52  ;
382600081013          noscelta2=in53  ;
382700081013        endif    ;
382800081013
382900140923        if s03tipo='C' and noscelta1=*on and noscelta2=*on  ;
383000081013          Testatahi=*on   ;
383100081013        endif    ;
383200081013
383300081013        if (s03tipo='C' and s031sce='T')  or
383400140923           (S03tipo='P' and wScelta='1')  or
383500081013           (s03tipo='I' and wscelta='1')   ;
383600140923          sflrosso1=*on   ;
383700140923          clear s03forza  ;
383800081013          if Testatahi  =*off  ;
383900081013            s031sce='T'     ;
384000081013          endif             ;
384100080609        else  ;
384200140923          sflrosso1=*off  ;
384300081013          if Testatahi  =*off  ;
384400081013            clear s031sce   ;
384500081013          endif             ;
384600080609        endif ;
384700080609
384800081013        if (s03tipo='C' and s032sce='T')  or
384900140923           (S03tipo='P' and wScelta='2')  or
385000081013           (s03tipo='I' and wscelta='2')   ;
385100140923          sflrosso2=*on   ;
385200140923          clear s03forza  ;
385300081013          if Testatahi  =*off  ;
385400081013            s032sce='T'     ;
385500081013          endif             ;
385600080609        else  ;
385700140923          sflrosso2=*off  ;
385800081013          if Testatahi  =*off  ;
385900081013            clear s032sce   ;
386000081013          endif             ;
386100080609        endif ;
386200080609
386300081013
386400080609        // solo se c'e' errore
386500080609          if ErrGenerico=*on   ;
386600080609           c03rcd=s03nrr  ;
386700080609
386800080609            if noscelta1=*off  ;
386900080609            errsfl3_1=*on     ;
387000080609            else    ;
387100080609            errsfl3_2=*on     ;
387200080609            endif             ;
387300080609          endif             ;
387400080609
387500080609        update mk07s03  ;
387600080609
387700080609        Testatahi=*off  ;
387800081013        IndInfo  =*off  ;
387900080609        errSFL3_1= *off;
388000080609        errSFL3_2= *off;
388100080609        sflrosso1= *off;
388200080609        sflrosso2= *off;
388300081013        noscelta1= *off;
388400081013        noscelta2= *off;
388500081013        Infoscelta1= *off;
388600081013        Infoscelta2= *off;
388700080619       ENDSR  ;
388800080619       //--------------------------------------------------------------
388900080619       //?Conferma FUSIONE
389000080619       //--------------------------------------------------------------
389100080619       BEGSR ConfFUSIONE  ;
389200080619       clear Wscrivi   ;
389300100525       clear TieniconTrCd  ;
389400100525       clear FondiAttB     ;
389500100525       clear FondiAttI     ;
389600100525       clear TieniAtt      ;
389700100525
389800100525       // Controllo se potenziale  che  TENGO ha una trattativa
389900100525           clear tnta43ds  ;
390000100525           ita43cpo=Tienicpo   ;
390100100525
390200100525           TNTA43R (TNTA43DS)  ;
390300100525           if ota43nrv>0 or ota43err<>' '   ;
390400100525             TieniconTrCD='S'   ;
390500100525
390600100525           else   ;
390700100525        setll   TieniCPO  cnaco16l;
390800100525         if %equal(cnaco16l)  ;
390900100525           TieniconTrCd='S'    ;
391000100525         endif  ;
391100100525         endif  ;
391200080619
391300080619        clear knk1  ;
391400080619        knk1 =%editc(TieniCPO:'X') ;
391500160901
391600160901      //?Cerco i dati del codice che tengo per memorizzare
391700160901      //?l'immagine precedente
391800160901        chain(n) (TieniCPO:'SPT') TICPI01L;
391900160901      //?Salvo l'immagine Precedente
392000160901        exsr ImmaginePrimaF;
392100080619
392200140923        // -?Lettura SFL per aggiornare CONTATTI, INFO e DETT.PROD.?
392300080619
392400080619        // 1° giro: deleto i record del pot che tengo, se manca la T
392500081013        s03nrr=1 ;
392600080619        chain  s03nrr mk07s03  ;
392700080619 1      dow   %found            ;
392800081013
392900081013        // Escludo i record di testata
393000081013 1a     if s031cod<>*blanks or s032cod<>*blanks  ;
393100080619
393200080620 2      if (v031tipf='T' and s031sce<>'T'and in52='0')  or
393300080620           (v032tipf='T' and s032sce<>'T'and in53='0')   ;
393400080619
393500080929         ktnt=%subst(s03tpr:1:2)  ;
393600080929         ktprI=%subst(s03tpr:1:3)  ;
393700080619         clear knk2  ;
393800080619 3       if v031tipf='T' and s031nk2<>*blanks    ;
393900080619         knk2=s031nk2  ;
394000080619 3       endif   ;
394100080619 3       if v032tipf='T' and s032nk2<>*blanks    ;
394200080619         knk2=s032nk2  ;
394300080619 3       endif   ;
394400080619
394500080619        // 1 riga
394600080619 3        if s03righe=1 or (s03tipo='I' and knk2<>*blanks) ;
394700140923 4         if  S03tipo = 'C'  or  S03tipo = 'P';
394800080619             delete ('P':knk1:knk2:ktnt) TFNTC01l    ;
394900080619             else   ;
395000080929             delete (TIENIcpo:ktprI:knk2) TiCPI01l    ;
395100080619 4           endif  ;
395200080619 x3       else    ;
395300080619
395400140923 4        if  S03tipo = 'C'  or  S03tipo = 'P';
395500080619        // loop per + righe e cerco quello uguale  SOLO PER CONTATTI
395600140923        //    e per DETTAGLIO PRODOTTI (anche se - PER ORA - è
395700140923        //    previsto un solo record)
395800080619           setll  ('P':knk1:knk2:ktnt) TFNTC01l    ;
395900080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
396000080619 5         dow not %eof (tfntc01l)    ;
396100080619 6          if (v031tipf='T' and %subst(ntcrnt:1:21) =s031cod )  or
396200080619               (v032tipf='T' and %subst(ntcrnt:1:21) =s032cod )  ;
396300080619               delete tfntc   ;
396400080619 6          endif    ;
396500080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
396600080619 5         enddo   ;
396700080619 4         endif  ;
396800080619
396900080619 3      endif   ;
397000080619 2      endif   ;
397100081013 1a     endif   ;
397200080619
397300080619        s03nrr=s03nrr+1  ;
397400080619        chain  s03nrr mk07s03  ;
397500080619        enddo  ;
397600080619
397700080619        // 2° giro: Aggiungo quello dell'altro potenziale
397800081013        s03nrr=1 ;
397900080619        chain  s03nrr mk07s03  ;
398000080619 1      dow   %found            ;
398100080619
398200081013
398300081013        // Escludo i record di testata
398400081013 1a     if s031cod<>*blanks or s032cod<>*blanks  ;
398500080620 2      if (v031tipf='T' and s032sce='T'and in53='0')  or
398600080620           (v032tipf='T' and s031sce='T'and in52='0')   ;
398700080619
398800080929         ktnt=%subst(s03tpr:1:2)  ;
398900080929         ktprI=%subst(s03tpr:1:3)  ;
399000080619         clear knk2  ;
399100080619 3       if v031tipf='T' and s032nk2<>*blanks    ;
399200080619         knk2=s032nk2  ;
399300080619 3       endif   ;
399400080619 3       if v032tipf='T' and s031nk2<>*blanks    ;
399500080619         knk2=s031nk2  ;
399600080619 3       endif   ;
399700080619
399800080619        // 1 riga
399900080619 3        if s03righe=1 or (s03tipo='I' and knk2<>*blanks) ;
400000140923 4          if  S03tipo = 'C'  or  S03tipo = 'P';
400100080619              knk1 =%editc(FondiCPO:'X') ;
400200080619              chain  ('P':knk1:knk2:ktnt) TFNTC01l    ;
400300080619              ntcnk1=%editc(Tienicpo:'X')   ;
400400080619              update tfntc ;
400500080619 x4          else    ;
400600080929              chain  (FondiCPO:ktprI:knk2) TiCPI01l    ;
400700080929              cpicpo=Tienicpo        ;
400800080929              update ticpi000 ;
400900080619 4           endif   ;
401000080619 x3       else    ;
401100080619
401200140923 4        if  S03tipo = 'C'  or  S03tipo = 'P';
401300080619        // loop per + righe e cerco quello uguale  SOLO PER CONTATTI
401400140923        //    e per DETTAGLIO PRODOTTI (anche se - PER ORA - è
401500140923        //    previsto un solo record)
401600080619           knk1 =%editc(FondiCPO:'X') ;
401700080619           setll  ('P':knk1:knk2:ktnt) TFNTC01l    ;
401800080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
401900080619 5         dow not %eof (tfntc01l)    ;
402000080619 6          if (v031tipf='T' and %subst(ntcrnt:1:21) =s032cod )  or
402100080619               (v032tipf='T' and %subst(ntcrnt:1:21) =s031cod )  ;
402200080619               ntcnk1=%editc(Tienicpo:'X')   ;
402300080619               update tfntc   ;
402400080619 6          endif    ;
402500080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
402600080619 5         enddo   ;
402700080619 4         endif  ;
402800080619
402900080619 3      endif   ;
403000080619 2      endif   ;
403100081013 1a     endif   ;
403200080619
403300080619        s03nrr=s03nrr+1  ;
403400080619        chain  s03nrr mk07s03  ;
403500080619 1      enddo  ;
403600160901
403700100525
403800100525        // 3° giro : cerco CONTATTI BATCH su pot FUSO/ da Tenere
403900100525
404000100525        setll (fondicpo: 0:0:0) tiatc04l     ;
404100100525        reade (fondicpo: 0:0:0) tiatc04l     ;
404200100525        dow   not %eof(tiatc04l)   ;
404300100525
404400100525        if atcpri<>'BATCH'    ;
404500100525        w0030= %int(%subst(%editc(atccmm:'X') :1:3))  ;
404600100525        Indx =%lookup(w0030:pog)   ;
404700100525        endif   ;
404800100525
404900100525        if atcpri='BATCH' or Indx>0 ;
405000100525        // se potenziale che tengo ha trattativa o ksc e attitivà mia o batch, deleto subito
405100100525          if TieniconTrCd='S'   ;
405200100525            exsr CanceAtt   ;
405300100525          else   ;
405400100525          // Memorizzo che c'e'
405500100525            if atcpri='BATCH' ;
405600100525             FondiAttB='S'   ;
405700100525             else  ;
405800100525             FondiAttI='S'   ;
405900100525            endif  ;
406000100525          endif  ;
406100100525        else   ;
406200100525          if fondiAttI=' '   ;
406300100525            FondiAttI='N'   ;
406400100525          endif  ;
406500100525        endif  ;
406600100525
406700100525        reade (fondicpo: 0:0:0) tiatc04l     ;
406800100525        enddo   ;
406900100525
407000100525        // Se esiste una BATCH  o mia e Tieni non ha trattativa ;
407100100525        //  verifico cosa devo cancellare
407200100525        if TieniconTrCd=' ' and (FondiattI='S' or FondiAttB='S');
407300100525        setll (Tienicpo: 0:0:0) tiatc04l     ;
407400100525        reade (Tienicpo: 0:0:0) tiatc04l     ;
407500100525        dow   not %eof(tiatc04l)   ;
407600100525
407700100525        if atcpri= 'BATCH' and FondiAttI<>' '  ;
407800100525            exsr CanceAtt   ;
407900100525            else   ;
408000100525            TieniAtt ='S'   ;
408100100525        endif   ;
408200100525
408300100525        reade (Tienicpo: 0:0:0) tiatc04l     ;
408400100525        enddo  ;
408500100525        endif   ;
408600100525
408700100525        // Se quello che tengo ha attività e il Fondi ne ha una BATCH --> la deleto
408800100525        if TieniAtt='S' and fondiAttB='S'    ;
408900100525        setll (fondicpo: 0:0:0) tiatc04l     ;
409000100525        reade (fondicpo: 0:0:0) tiatc04l     ;
409100100525        dow   not %eof(tiatc04l)   ;
409200100525        if atcpri='BATCH'   ;
409300100525            exsr CanceAtt   ;
409400100525        endif   ;
409500100525
409600100525        reade (fondicpo: 0:0:0) tiatc04l     ;
409700100525        enddo   ;
409800100525        endif   ;
409900100525
410000100525        //  Aggiorno TUTTE LE ATTIVITA' RIMASTE
410100100525
410200111013        exec sql  update tiatc00f set atccpo=:tienicpo, atctas = ' '
410300111013                  where atccpo=:fondicpo;
410400100521
410500100525        // 5° giro : copio tutte le note interne di quello che fondo
410600100525        //          in quello da tenere DEL NUOVO FILE  NOTE
410700100521          exsr NewNote   ;
410800080619
410900100525        // 6° giro: DELETO i record rimasti di quello da fondere
411000080619        knk1 =%editc(FondiCPO:'X') ;
411100080619        clear knk2  ;
411200080619        setll  ('P':knk1) TFNTC01l    ;
411300080619        reade  ('P':knk1) TFNTC01l    ;
411400080619        dow not %eof (tfntc01l)    ;
411500080619          delete tfntc ;
411600080619          reade  ('P':knk1) TFNTC01l    ;
411700080619        enddo    ;
411800080929        setll  (FondiCPO) TiCPI01l    ;
411900080929        reade  (fondiCPO) TiCPI01l    ;
412000080929        dow not %eof (ticpi01l)    ;
412100080929          delete ticpi000 ;
412200080929        reade  (fondiCPO) TiCPI01l    ;
412300080619        enddo    ;
412400160901        setll (FondiCPO) TNCPVT1L;
412500160901        reade (FondiCPO) TNCPVT1L;
412600160901        DOW  not %eof (TNCPVT1L);
412700160901          delete TNCPVT00;
412800160901          reade (FondiCPO) TNCPVT1L;
412900160901        ENDDO;
413000160901        setll (FondiCPO) TNCPVD1L;
413100160901        reade (FondiCPO) TNCPVD1L;
413200160901        DOW  not %eof (TNCPVD1L);
413300160901          delete TNCPVD00;
413400160901          reade (FondiCPO) TNCPVD1L;
413500160901        ENDDO;
413600121018
413700121018       //?8° giro aggiorno potenziale sul file storico CRM del potenziale FUSO
413800121018       //?        imposto il potenziale DA TENERE
413900130215        exec sql
414000130215          UPDATE TICRM00F set CRMcpo = :Tienicpo
414100130215          WHERE  CRMcpo = :Fondicpo;
414200080619
414300100525        // 9° giro : aggiorno cod potenziale su eventuali ANAGRAFICHE
414400080624        //           che hanno il codice vecchio
414500080624        setll    fondicpo cnaco16l  ;
414600080624        reade(E) fondicpo cnaco16l  ;
414700080624        dow not %eof(cnaco16l) and not %error  ;
414800080624        clear acodtr             ;
414900080624        clear acoftr             ;
415000080624        Dataiso =%date(*date:*iso)  ;
415100080624        Dataymd =Dataiso;
415200080624        acoduv=%dec(Dataymd);
415300080624        acolib=tienicpo          ;
415400080624        update cnaco000          ;
415500080624
415600080624        reade(E) fondicpo cnaco16l  ;
415700080624        enddo                    ;
415800080624
415900080624        setll    fondicpo tfaco16l  ;
416000080624        reade(E) fondicpo tfaco16l  ;
416100080624        dow not %eof(tfaco16l) and not %error  ;
416200080624        clear acodtr             ;
416300080624        clear acoftr             ;
416400080624        Dataiso =%date(*date:*iso)  ;
416500080624        Dataymd =Dataiso;
416600080624        acoduv=%dec(Dataymd);
416700080624        acolib=tienicpo          ;
416800080624        update tfaco000          ;
416900080624
417000080624        reade(E) fondicpo tfaco16l  ;
417100080624        enddo                    ;
417200100525
417300100525        //10° giro : aggiorno TRATTATIVE --> TIVIS
417400100525        exec sql  update tivis00f set viscpo=:Tienicpo where viscpo=:Fondicpo;
417500140715
417600140715        //?11° giro: aggiorno ESTENSIONE anagrafica POTENZIALE?
417700140715
417800140918        // -?Salvataggio e Cancellazione dati potenziale da Fondere?
417900140918        clear  SavCPO1ds;
418000140715        chain  FondiCPO  TNCPO100;
418100140715        if  %found(TNCPO11L);
418200140918          SavCPO1ds = TNCPO1ds;
418300140715          DELETE  TNCPO100;
418400140715        endif;
418500140918
418600140918        // -?Aggiornamento dati potenziale da Tenere?
418700140715        chain  TieniCPO  TNCPO100;
418800140918        if  NOT %found(TNCPO11L);
418900140918          clear  TNCPO100;
419000140918          CPO1cpo = TieniCPO;
419100140918        endif;
419200140918
419300140918        // -?Telefono?
419400140715        Select;
419500140715          When  %found(TNCPO11L)  and  CPO1tel = *blanks
419600140918                                  and  X_CPO1tel <> *blanks;
419700140918            CPO1tel = X_CPO1tel;
419800140918          When  NOT %found(TNCPO11L)  and  X_CPO1tel <> *blanks;
419900140918            CPO1tel = X_CPO1tel;
420000140715        EndSl;
420100140918
420200140918        // -?Fax?
420300140918
420400140918        // -?Data Ultima Conferma INFO Commerciali?
420500140919        if  (V031tipF = 'T'  and  wScelta = '2')  or
420600140919            (V032tipF = 'T'  and  wScelta = '1');
420700140919          CPO1dUCifo = X_CPO1dUCifo;
420800140919        endif;
420900140918
421000140918        // -?CRIF Number?
421100140918        if  X_CPO1crifN  <> *blank  and
421200140918            (CPO1crifN   =  *blank  or
421300140918             F_CPOfsf    =  'S');
421400140918          CPO1crifN  = X_CPO1crifN;
421500140918        endif;
421600140918
421700140918        // -?CRIF Number Casa Madre?
421800140918        if  X_CPO1crifNM <> *blank  and
421900140918            (CPO1crifNM  =  *blank  or
422000140918             F_CPOfsf    =  'S');
422100140918          CPO1crifNM = X_CPO1crifNM;
422200140918        endif;
422300140918
422400140918        // -?Flag Operativi?
422500140918
422600140918        // -?Aggiornamento rec.?
422700140918        Select;
422800140918          When  %found(TNCPO11L);
422900140918            UPDATE  TNCPO100;
423000140918          When  NOT %found(TNCPO11L)  and
423100140918               (CPO1tel    <> *blank  or
423200140918                CPO1dUCIfO <> *zero   or
423300140918                CPO1crifN  <> *blank  or
423400140918                CPO1crifNM <> *blank);
423500140918            WRITE  TNCPO100;
423600140918          Other;
423700140918            UNLOCK  TNCPO11L;
423800140918        EndSl;
423900080624
424000140715        //?12° giro: aggiorno anagrafica POTENZIALE?
424100080619
424200140918        // -?Anno Inizio Attività (EX TELEFONO - vedi dCPO02)?
424300140918        dCPO02 = CPOtel;
424400140918        if  %subst( F_CPOtel : 1 : %size(dCPO02.§CPOannoIA) )
424500140918                               >  *zero  and
424600140918            (dCPO02.§CPOannoIA <= *zero  or
424700140918             F_CPOfsf          =  'S');
424800140918          dCPO02.§CPOannoIA = %subst( F_CPOtel : 1 :
424900140918                                    %size(dCPO02.§CPOannoIA) );
425000140918          CPOtel = dCPO02;
425100140918        endif;
425200140918
425300140918        // -?piv e cdf?
425400080619        if cpopiv=*blanks and cpocdf=*blanks   ;
425500080619        cpopiv=F_cpopiv     ;
425600080619        cpocdf=F_cpocdf     ;
425700080619        endif               ;
425800080624
425900080624
426000140918        // -?cod commerciale?
426100080624        if cpocmm=0   and f_cpocmm>0       ;
426200080624        cpocmm=F_cpocmm     ;
426300080624        endif               ;
426400080619
426500140918        // -?dati D&B   tengo quelli che li ha o il pot SEDE se entrambi li hanno?
426600140918        //  ?data primo contatto, tengo la data più vecchia tra i 2 potenziali
426700090520        dcpo01=cporst       ;
426800090520
426900090520        if F_cpoftaz>0;
427000090520        if cpoftaz=0  or  f_cpofsf='S'   ;
427100080619        cpoftaz=F_cpoftaz   ;
427200080619        cpoafaz=F_cpoafaz   ;
427300080619        cpoffaz=F_cpoffaz   ;
427400080619        cpopexp = F_cpopexp ;
427500090520          // Memorizzo anche il fatturato presunto
427600090520          if %subst(f_cporst:24:11)>*zeros   ;
427700090520            §cposptp=%int(%subst(f_cporst:24:11)) ;
427800090520          endif   ;
427900080619        endif               ;
428000090520        endif               ;
428100120622
428200120622        //?data primo contatto
428300120622        SELECT;
428400120622          WHEN  (DataPrimoConT > 0 and
428500120622                 DataPrimoConT < DataPrimoConF) or
428600120622                 DataPrimoConF = 0;
428700120622            §CPOdtpcon = %editc(DataPrimoConT:'X');
428800120622          WHEN  (DataPrimoConF > 0 and
428900120622                 DataPrimoConF < DataPrimoConT) or
429000120622                 DataPrimoConT = 0;
429100120622            §CPOdtpcon = %editc(DataPrimoConF:'X');
429200120622        ENDSL;
429300140918
429400140918        // -?Data Ultimo Aggiornamento INFO Commerciali?
429500140919        if  (V031tipF = 'T'  and  wScelta = '2')  or
429600140919            (V032tipF = 'T'  and  wScelta = '1');
429700140919          CPO1dUCifo = X_CPO1dUCifo;
429800140919          §CPOifoDUl = %subst( F_CPOrst : 5 : %size(§CPOifoDUl) );
429900140919        endif;
430000140918
430100140918        // -?Flag Problemi Finanziari (1° byte di CPOCLT)?
430200140918        if  %subst( F_CPOclt : 1 : 1 ) <> *blank  and
430300140918            ( %subst( CPOclt : 1 : 1 ) =  *blank  or
430400140918              F_CPOfsf               =  'S' );
430500140918          %subst( CPOclt : 1 : 1 ) = %subst( F_CPOclt : 1 : 1 );
430600140918        endif;
430700080619
430800140918        // -?Sede/Filiale?
430900080619        if cpofsf='F' and F_cpofsf='S'  ;
431000080619        cpofsf='S'          ;
431100080619        endif               ;
431200080619
431300140918        // -?codice  duns?
431400080619        if cpoduns=*blanks or (F_cpoduns<>*blanks and F_cpofsf='S') ;
431500080619        cpoduns=F_cpoduns   ;
431600080619        endif               ;
431700110304
431800110304       //?Ricalcolo la categoria del potenziale
431900110304        clear trmk05ds;
432000110304        IMK05cpo = CPOcpo;
432100110304        trmk05r (kpjba : TRMK05DS);
432200110304        IF  OMK05err = *blanks;
432300110304          CPOfls = OMK05cat;
432400110304        ENDIF;
432500080619
432600080619        cpodtr=*date        ;
432700080619        §cporicfus='S'      ;
432800080619        cporst=dcpo01       ;
432900080619        update tncpo000     ;
433000080619
433100080619        delete tncpofis     ;
433200090520
433300140918        // -?Aggiorno potenziale per i dati contenuti in CPORST?
433400090521        alfacpo=%editc(cpocpo:'X')      ;
433500090521        callp   TRMK08R   (kpjba:alfacpo)  ;
433600090521
433700160901      //?Cerco i dati del codice che tengo per memorizzare
433800160902      //?l'immagine dopo
433900160901        chain(n) (TieniCPO:'SPT') TICPI01L;
434000160902        chain(n) (TieniCPO) TNCPO01L;
434100160901      //?Scrivo la variazione
434200160901        exsr ScriviVariazioneF;
434300160901
434400160901      //?Scrivo una riga per memorizzare il codice potenziale Fuso
434500160901        exsr ScriviFuso;
434600080619
434700140918         // -?Imposto il posizionamento cursore per il sfl che emetto?
434800140918         //  ?aggiornato?
434900080619         if nrrFUS(1)> 0 ;
435000080619           C02csr=nrrFUS(1) ;
435100080619           pagric=(c02csr/C_sflPag)+1 ;
435200080619           $Video = 'S2';
435300080619           $InzS02 = *on;
435400080619         else           ;
435500080619           $Video = 'D1';
435600080619           $InzD01 = *on;
435700080619         endif    ;
435800080619
435900080619       ENDSR  ;
436000100525       //--------------------------------------------------------------
436100100525       //?cancello attività della trascodifica
436200100525       //--------------------------------------------------------------
436300100525       BEGSR CanceAtt   ;
436400100525
436500100525        // cancello  eventuale legame
436600100525          delete (atcatn:atcatnp) tiatl01l   ;
436700100525
436800100525        // cancello  eventuali note
436900100525
437000100525         cpncpo = atccpo ;
437100100525         cpntat = atctat ;
437200100525         cpnatn = atcatn ;
437300100525         cpnatnp= 0 ;
437400100525         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
437500100525         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
437600100525         dow not %eof(ticpn03l);
437700100525             delete ticpn000;
437800100525             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
437900100525         enddo ;
438000100525
438100100525       delete   tiatc000   ;
438200100525
438300100525       ENDSR  ;
438400100521       //--------------------------------------------------------------
438500100521       //?Scrivo le note interne nuovo file sl pot che tengo
438600100521       //--------------------------------------------------------------
438700100521       BEGSR Newnote;
438800100521
438900100524        exec sql declare A1 cursor for
439000100524                 select  * from ticpn04l where cpncpo= :Fondicpo
439100100524                 order by cpncpo, cpndim, cpnhim   ;
439200100524        exec sql open  A1;
439300100524        exec sql fetch next from A1 into :ticpnds ;
439400100524
439500100524        dow  sqlcod = 0;
439600100524
439700100524        Totnot=10 ;
439800100524
439900100524        // Verifico se c'e' già un nota con data e ora per il potenziale da tenere
440000100524
440100100524       dow Totnot>0  and cpnpno=1 ;
440200100524       exec sql  select count(cpncpo) into :Totnot from ticpn04l where
440300100524                 cpncpo= :Tienicpo  and cpndim= :cpndim and cpnhim= :cpnhim  ;
440400100524
440500100524        if  totnot>0   ;
440600100524         OraHMS=%time(cpnhim)  ;
440700100524         OraHMS=oraHMS- %minutes(10) ;
440800100524         cpnhim=%dec(OraHMS)  ;
440900100524        else  ;
441000100524         newhim=cpnhim    ;
441100100524        endif  ;
441200100524        enddo  ;
441300100524
441400100524        cpnhim= newhim     ;
441500100525        cpncpo= Tienicpo   ;
441600100524        write ticpn000  ;
441700100524
441800100525        exec sql fetch next from A1 into :ticpnds ;
441900100524        enddo    ;
442000100524
442100100524        exec sql close A1;
442200100524
442300100524        exec sql delete from ticpn04l where cpncpo= :Fondicpo    ;
442400100524
442500100521       ENDSR   ;
442600160901
442700160901       //--------------------------------------------------------------
442800160901       //?Scrivo l'immagine Precedente anagrafica Potenziale
442900160901       //--------------------------------------------------------------
443000160901       BEGSR ImmaginePrima;
443100160901
443200160901         clear TRMK30DS;
443300160901         tncpo1_30 = TNCPO1ds;
443400160901         IMK30immag = 'P';
443500160901         IMK30tvp = 'B';
443600160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
443700160901
443800160901       ENDSR;
443900160901
444000160901       //--------------------------------------------------------------
444100160901       //?Scrivo Storico Variazioni.
444200160901       //--------------------------------------------------------------
444300160901       BEGSR ScriviVariazione;
444400160901
444500160901         clear TRMK30DS;
444600160901         tncpo1_30 = TNCPO1ds;
444700160901         IMK30pru = knmus;
444800160901         IMK30noj = knmeb;
444900160901         IMK30pgm = 'TRMK07R';
445000160901         IMK30immag = 'D';
445100160901         IMK30cta = 'M';
445200160901         IMK30tvp = 'B';
445300160901
445400160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
445500160901
445600160901       ENDSR;
445700160901
445800160901       //--------------------------------------------------------------
445900160901       //?Scrivo Riga per Memorizzare il codice Sede di Unione.
446000160901       //--------------------------------------------------------------
446100160901       BEGSR ScriviSede;
446200160901
446300160901         clear TNCPVT00;
446400160901         clear TNCPVD00;
446500160901
446600160901         clear dCPV_G;
446700160901         gCPOcpo = V01sfcpo * 100000000 + V01sncpo;
446800160901
446900160901         CPVcpo = CPOcpo;
447000160901         CPVflo = 'G';
447100160901         CPVcta = 'M';
447200160901         CPVpgm = 'TRMK07R';
447300160901         CPVdav = %dec(%date());
447400160901         CPVorv = %dec(%time());
447500160901         CPVorv += 1;
447600160901         CPVpru = knmus;
447700160901         CPVnoj = knmeb;
447800160901         write TNCPVT00;
447900160901
448000160901         CPVdopo = dCPV_G;
448100160901         CPVtvp = 'G';
448200160901         write TNCPVD00;
448300160901
448400160901       ENDSR;
448500160901
448600160901       //--------------------------------------------------------------
448700160901       //?Scrivo l'immagine Precedente anagrafica Potenziale Fusione
448800160901       //--------------------------------------------------------------
448900160901       BEGSR ImmaginePrimaF;
449000160901
449100160901         clear TRMK30DS;
449200160901         tncpo1_30 = TNCPO1ds;
449300160901         IMK30immag = 'P';
449400160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
449500160901
449600160901       ENDSR;
449700160901
449800160901       //--------------------------------------------------------------
449900160901       //?Scrivo Storico Variazioni Fusione
450000160901       //--------------------------------------------------------------
450100160901       BEGSR ScriviVariazioneF;
450200160901
450300160901         clear TRMK30DS;
450400160901         tncpo1_30 = TNCPO1ds;
450500160901         IMK30pru = knmus;
450600160901         IMK30noj = knmeb;
450700160901         IMK30pgm = 'TRMK07R';
450800160901         IMK30immag = 'D';
450900160901         IMK30cta = 'M';
451000160901
451100160901         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
451200160901
451300160901       ENDSR;
451400160901
451500160901       //--------------------------------------------------------------
451600160901       //?Scrivo Riga per Memorizzare il codice Fuso.
451700160901       //--------------------------------------------------------------
451800160901       BEGSR ScriviFuso;
451900160901
452000160901         clear TNCPVT00;
452100160901         clear TNCPVD00;
452200160901
452300160901         clear dCPV_F;
452400160901         fCPOcpo = FondiCPO;
452500160901
452600160901         CPVcpo = CPOcpo;
452700160901         CPVflo = 'F';
452800160901         CPVcta = 'M';
452900160901         CPVpgm = 'TRMK07R';
453000160901         CPVdav = %dec(%date());
453100160901         CPVorv = %dec(%time());
453200160901         CPVorv += 1;
453300160901         CPVpru = knmus;
453400160901         CPVnoj = knmeb;
453500160901         write TNCPVT00;
453600160901
453700160901         CPVdopo = dCPV_F;
453800160901         CPVtvp = 'F';
453900160901         write TNCPVD00;
454000160901
454100160901       ENDSR;
454200160901
454300080206       //--------------------------------------------------------------
454400080206       //?Operazioni finali.
454500080206       //--------------------------------------------------------------
454600080206       BEGSR RoutEnd;
454700080410
454800080206         *inLR = *on;
454900080206         return;
455000080206
455100080206       ENDSR;
455200080206
455300080206      /end-free
455400080206
455500080206       //--------------------------------------------------------------
455600080206       //?Schiere a tempo di compilazione.
455700080206       //--------------------------------------------------------------
455800080206
455900080410** - MSG ------------------------------------------------------------------ -*
456000080411Utente non abilitato alla gestione dei potenziali!!!                           01
456100080523Effettuare almeno una scelta                                                   02
456200080618Codice potenziale SEDE  errato o non selezionato                               03
456300080603Impossibile fare FUSIONE di 2 potenziali SEDE                                  04
456400111115Impossibile fare FUSIONE di 2 potenziali in categoria "Eliminabile"            05
456500080523Immettere almeno un codice potenziale FILIALE                                  06
456600111115Categoria potenziale errata                                                    07
456700080411Filiale appartenenza errata                                                    08
456800080414Non è stato trovato nessun potenziale per le selezioni immesse                 09
456900080415Caricamento dati non completabile: selezionati troppi dati!                    10
457000140923Se immessa la provincia, immettere anche la ragione sociale                    11 LIBERO
457100080415Provincia errata                                                               12
457200080415Selezionare soltanto una SEDE                                                  13
457300080523Impossibile selezionare più di 7 FILIALI per l'Unione                          14
457400080526Impossibile Unione di una SEDE senza partiva iva e codice fiscale              15
457500140923Pot.xxxxxxxxxxx-yyyyyyyyyyyy non selezionabile:già presente la SEDE x l'Unione 16
457600080526Filiale con partita IVA e/o cod.Fiscale diversi dalla SEDE-Enter Forza         17
457700080603Per la FUSIONE selezionare soltanto 2 potenziali !                             18
457800140923Pot.xxxxxxxxxxx-yyyyyyyyyyyy non selezionabile:già due codici per la Fusione   19
457900080604E' stato selezionato lo stesso codice potenziale!!                             20
458000080609Selezionare il potenziale da TENERE e quello da FONDERE!!!                     21
458100080609Impossibile selezionare più di xx xxxxx per                                    22
458200080617Nessuna selezione effettuata per YYYYYYYYYYY xxxxxxxxxxxxxxxxxxx               23
458300080609Impossibile selezionare 2 volte il codice "xxxx" per xxxxxx                    24
458400111115Il potenziale in cat. "Eliminabile" deve essere "F U S O"  !!                  25
458500080618Utente non abilitato all'aggiornamento del potenziale                          26
458600080618Codice potenziale  errato o non selezionato                                    27
458700140923Utente non abilitato a FONDERE il potenz.:presenti AZIONI nell'anno            28 LIBERO
458800081010Utente non abilitato all'aggiornamento dei 2 potenz.:FUSIONE NON EFFETTUABILE  29
458900080619Potenziale allocato da altro utente:controllare e riprovare!                   30
459000081009Potenziale non più presente in anagrafica: già normalizzato!!??!!              31
459100100521Presenti ANAGRAFICHE sul potenziale da Fondere: saranno aggiornate             32
459200140923                                                                                  LIBERO
459300140923                                                                                  LIBERO
459400081013Categ.Merceologica del potenziale da TENERE errata: correggerla con "M-Manut." 35
459500100419Scegliere le Info Commerciali da TENERE impostando una sola "T"                36
459600140923Dettaglio Prodotti NON visualizzabile                                          37
459700140923                                                                                  LIBERO
459800100525Fusione NON EFFETTUABILE:trattativa aperta per il potenziale da Fondere        39
459900100521Presenti TRATTATIVE sul potenziale da Fondere: saranno aggiornate              40
460000100521Presenti Anagrafiche e Trattative sul potenziale da Fondere:saranno aggiornate 41
460100100524Utente non abilitato a FONDERE il potenz.:presenti ANAGRAFICHE                 42
460200100525Il potenziale codificato è da TENERE se l'altro non è codificato               43
