000100140918     /*END
000200080206      //--------------------------------------------------------------
000300081009      //?TRMK07R - Normalizzazione  CLIENTI POTENZIALI
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200080208      // - Anagrafica clienti potenziali
001300080619     fTNCPO00F  uf   e             disk    prefix(F_)
001400080619     f                                     rename(tncpo000:tncpofis)
001500080619     fTNCPO01L  uf   e           k disk    infds(cpo01)
001600080208     fTNCPO02L  if   e           k disk    rename(TNCPO000 : TNCPO002)
001700080610     f                                     prefix(R_)
001800140715     fTNCPO11L  Uf A e           k disk
001900121018      // - Storico CRM
002000130916     f*//TICRM00F  if   e             disk
002100130916      // - Anagrafica Commerciali
002200130916     fAZCMM01L  if   e           k disk
002300080208      // - Organigramma filiale/aziendale
002400080206     fAZORG01L  if   e           k disk
002500080208      // - Tabelle
002600080206     fTABEL00F  if   e           k disk
002700080604     fTNTBE01l  if   e           k disk
002800080604
002900080619     ftfntc01l  uf a e           k disk
003000100525     fticpn03l  uf a e           k disk
003100080929     fticpi01l  uf a e           k disk
003200100525     ftiatc04l  uf   e           k disk
003300100525     ftiatl01l  uf   e           k disk
003400080624
003500080624     fcnaco16l  uf   e           k disk
003600080624     fTFaco16l  uf   e           k disk    rename(cnaco000:tfaco000)
003700081112     f                                     extfile(WTFACO) USROPN
003800110803     fTIVIS00F  If   e             disk
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
014300140923     d*// Psds           sds
014400140923     d*//   SDSpgm          *proc
014500080206
014600080206      // - InfDS
014700080206     d InfDspF         ds
014800080207     d  dsp_aid              369    369a                                        AID byte
014900130916     d*//sfl_rrn              376    377i 0                                      Subfile rrn
015000130916     d*//min_nrr              378    379i 0                                      Subfile min rrn
015100130916     d*//num_rcds             380    381i 0                                      Subfile num rcds
015200080206
015300080206      // - Indicatori su DspF
015400080208     d IndDspF         ds
015500080206        // - Indicatori di gestione del subfile
015600080626     d  Intpotenz                     1n   overlay(IndDspF : 01)
015700081009     d  IndInfo                       1n   overlay(IndDspF : 10)
015800080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
015900080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
016000080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016100080206     d  SflEnd                        1n   overlay(IndDspF : 33)
016200080206        // - Indicatori di errore
016300080206     d  errMessage                    1n   overlay(IndDspF : 28)
016400080523     d  errSEDE                       1n   overlay(IndDspF : 40)
016500080523     d  errFIL                        1n   overlay(IndDspF : 41)
016600111115     d  errFLS                        1n   overlay(IndDspF : 43)
016700080523     d  errFILIALE                    1n   overlay(IndDspF : 44)
016800080411     d  errRAG                        1n   overlay(IndDspF : 45)
016900080523     d  ProtPOTSED                    1n   overlay(IndDspF : 46)
017000080415     d  ErrPRV                        1n   overlay(IndDspF : 47)
017100080415     d  ErrScelta                     1n   overlay(IndDspF : 48)
017200080523     d  ProtPOTFIL                    1n   overlay(IndDspF : 49)
017300080603     d  errFusione                    1n   overlay(IndDspF : 50)
017400080603     d  ProtPOTDUE                    1n   overlay(IndDspF : 51)
017500080604     d  noScelta1                     1n   overlay(IndDspF : 52)
017600080604     d  noScelta2                     1n   overlay(IndDspF : 53)
017700080605     d  TestataHI                     1n   overlay(IndDspF : 54)
017800080609     d  ERRsfl3_1                     1n   overlay(IndDspF : 55)
017900080609     d  ERRsfl3_2                     1n   overlay(IndDspF : 56)
018000080610     d  errTipoFus1                   1n   overlay(IndDspF : 57)
018100080610     d  errTipoFus2                   1n   overlay(IndDspF : 58)
018200081010     d  InfoScelta1                   1n   overlay(IndDspF : 62)
018300081010     d  InfoScelta2                   1n   overlay(IndDspF : 63)
018400111116     d  RIcodif1                      1n   overlay(IndDspF : 64)
018500111116     d  RIcodif2                      1n   overlay(IndDspF : 65)
018600080609     d  SFLRosso1                     1n   overlay(IndDspF : 95)
018700080609     d  SFLRosso2                     1n   overlay(IndDspF : 96)
018800080606     d  ColorROSSO2                   1n   overlay(IndDspF : 97)
018900080606     d  ColorROSSO1                   1n   overlay(IndDspF : 98)
019000080606     d  errGenerico                   1n   overlay(IndDspF : 99)
019100080206
019200080206      // - Parametri ricevuti
019300080206     d KPJBA         e ds
019400080206
019500080206      // - Reperimento dati utente
019600080206     d TIBS34ds      e ds
019700080206     d dUte01        e ds
019800080206
019900080206      // - Ricerca/Controllo tabelle
020000080206     d TIBS02ds      e ds                  inz
020100110304
020200110304      // - Calcolo categoria potenziale
020300110304     d TRMK05ds      e ds
020400111115
020500140918      // -?Tabella CPO - categoria potenziale
020600111115     d dCPO          e ds                  inz
020700080206
020800080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
020900080206     d dLAT          e ds                  inz
021000080206
021100080206      // - Tabella 1L = Categorie Merceologiche
021200080207     d ds1L          e ds                  inz
021300140918
021400080410      // - ds del campo CPORST
021500080410     d DCPO01        e ds                  inz
021600140918      // -?DS per campo CPOTEL di TNCPO00F
021700140918     d dCPO02        e ds                  inz  qualified
021800080410
021900080604     d ds1T          e ds                  inz
022000080929     d dIFO          e ds                  inz
022100100524     d ticpnds       e ds                  inz  extname(TICPN00F)
022200140918     d*// savCPOds      e ds                  inz  extname(TNCPO00F)
022300140918     d*//                                     prefix(F_)
022400140918     d TNCPO1ds      e ds                  inz  extname(TNCPO10F)
022500140918     d SavCPO1ds     e ds                  inz  extname(TNCPO10F)
022600140918     d                                     prefix(X_)
022700140918     d TRMK01ds      e ds                  inz
022800080929     D TRMK50DS      E DS
022900080606     D TNTA12DS      E DS
023000140918     d tnta43ds      e ds                  inz
023100080617     D TRUL31DS      E DS
023200140918     d   POG                          3  0 DIM(250) overlay(o31pog)
023300080206
023400080206      //---------------------------------------------------------------
023500080206      //?Definizione variabili globali.
023600080206      //---------------------------------------------------------------
023700080206
023800080206      // - Flags booleani
023900111115     d $Contattato1    s               n   inz(*off)
024000111115     d $Contattato2    s               n   inz(*off)
024100111115     d $EndAtt         s               n   inz(*off)
024200080208     d $Fine           s               n   inz(*off)
024300080208     d $InzD01         s               n   inz(*on)
024400080208     d $InzS01         s               n   inz(*on)
024500080414     d $InzS02         s               n   inz(*on)
024600080603     d $InzS03         s               n   inz(*on)
024700080206     d $ErrGrave       s               n   inz(*off)
024800080207     d $EoF            s               n   inz(*off)
024900080208     d $RecOK          s               n   inz(*off)
025000080206
025100080207      // - Campi associati al video
025200080207     d $Video          s              2    inz('D1')
025300080208     d S01nrr          s              4  0 inz
025400080414     d S02nrr          s              4  0 inz
025500080603     d S03nrr          s              4  0 inz
025600080414     d wPag            s              4  0 inz
025700080414     d wRig            s              3  0 inz
025800080523     d record          s              2  0 inz(7)
025900080414
026000080206
026100100805     d d11cpo          s                   like(cpocpo)  inz
026200100805     d forzacpo        s                   like(cpocpo)  inz
026300080624     d savcpo          s                   like(cpocpo)  inz
026400080604     d wAbi            s                   like(UTEaut)  inz
026500080609     d Indx            s              3  0 inz
026600080619     d Indx2           s              3  0 inz
026700080609     d xx              s              3  0 inz
026800080929     d yy              s              3  0 inz
026900080415     d ss              s              3  0 inz
027000080415     d ff              s              3  0 inz
027100080526     d contafil        s              3  0
027200080414     d K48A            s             48
027300080526     d Pulizforz       s              1
027400080603     d Pulizpot1       s              1
027500080603     d Pulizpot2       s              1
027600080604     d ricInt          s              1
027700080606     d UpdateSFL       s              1
027800080606     d Primariga       s              1
027900080619     d Wscrivi         s              1
028000081013     d Wscelta         s              1
028100080604     d Wpft            s             12  0
028200080605     d W0060           s              6  0
028300080929     d W007A           s              7
028400080605     d W004a           s              4
028500080929     d Wdes            s             19
028600080929     d WIspp           s              1
028700081009     d WcodCPI         s             21
028800081013     d wspf            s                   like(cpispf)  inz
028900081010     d Wgiorno         s              2  0
029000081010     d WDTunAnno       s              8  0
029100100524     d totnot          s              5  0
029200111116     d codif1          s              1
029300111116     d codif2          s              1
029400120622
029500120622     d DataPrimoConF   s              8  0
029600120622     d DataPrimoConT   s              8  0
029700111115
029800111115     d sav_CPOcpo      s                   like(CPOcpo)
029900111115     d sav_ATCcad      s                   like(ATCcad)
030000111115
030100100525     d NewHIM          s                   like(cpnhim)
030200081010
030300081010     d W1Esinfo        s              1
030400081010     d W2Esinfo        s              1
030500081010     d TieniEsinfo     s              1
030600081010     d FondiEsinfo     s              1
030700081010     d ImpostaT        s              1
030800100525     d TieniconTrCd    s              1
030900100525     d fondiAttB       s              1
031000100525     d fondiAttI       s              1
031100100525     d TieniAtt        s              1
031200090521     d Alfacpo         s             11
031300100419     d Alfaoggi        s              8
031400100525     d W0030           s              3  0
031500100525     d W0080           s              8  0
031600080609     d knk1            s                   like(ntcnk1)  inz
031700080604     d knk2            s                   like(ntcnk2)  inz
031800080604     d ktnt            s                   like(ntctnt)  inz
031900080929     d ktprI           s                   like(cpitpf)  inz
032000080619     d TieniCPO        s                   like(cpocpo)  inz
032100080619     d FondiCPO        s                   like(cpocpo)  inz
032200080619     d FondiRag        s                   like(v031rag) inz
032300080605     d Dataiso         s               d   datfmt(*iso)
032400080605     d Dataymd         s               d   datfmt(*ymd)
032500080605     d Datadmy         s               d   datfmt(*dmy)
032600081010     d DataSYS         s               d   inz(*sys)
032700100524     d OraHMS          s               t   timfmt(*hms)
032800130916     D*
032900081112     d WTFACO          s             21    inz('FILTRAGRU /TFACO16L')
033000130916     d*
033100080415     d                 DS
033200080415     d  Fcpo                          3  0
033300080415     d  Ncpo                          8  0
033400080415     d kcpo                    1     11  0
033500130916      *
033600090521     D TRMK08R         pr                  extpgm('TRMK08R')
033700090521     D  kpjba                              likeds(kpjba)
033800090521     D  ALFACPO                      11
033900080206      //---------------------------------------------------------------
034000080206      //?Definizione procedure usate.
034100080206      //---------------------------------------------------------------
034200080414      /copy gaitrasrc/srcprotopr,tibs02r
034300080414      /copy gaitrasrc/srcprotopr,tibs34r
034400080606      /copy gaitrasrc/srcprotopr,tnta12r
034500080929      /copy gaitrasrc/srcprotopr,trmk50r
034600140923      *///copy gaitrasrc/srcprotopr,trmk_opm
034700080617      /copy gaitrasrc/srcprotopr,trul31r
034800100520      /copy gaitrasrc/srcprotopr,trmk01r
034900100520      /copy gaitrasrc/srcprotopr,trmk02r
035000100521      /copy gaitrasrc/srcprotopr,tnta43r
035100110304
035200110304      // - Calcolo categoria potenziale
035300110304     d Trmk05r         pr                  extpgm('TRMK05R')
035400110304     d  kpjba                              likeds(KPJBA)
035500110304     d  trmk05ds                           likeds(TRMK05ds)
035600080206
035700080206      //---------------------------------------------------------------
035800080206      //?Riepilogo indicatori.
035900080206      //---------------------------------------------------------------
036000080207      // - Comuni
036100080207      // 28    : Emissione messaggio di errore a video
036200080207      // - C01/S01
036300080207      // 30    : Condiziona SFLDSP    (*not)
036400080207      // 31    : Condiziona SFLDSPCTL (*not)
036500080207      // 30+31 : Condiziona SFLCLR
036600080207      // 32    : Condiziona SFLNXTCHG in S01
036700080207      // 50    : Errore: Opzione errata
036800080207      // - D01
036900080207      // - Comuni
037000080207      // 99    : Generico di Errore
037100080206      //---------------------------------------------------------------
037200080206
037300080206      //---------------------------------------------------------------
037400080206      //?M A I N - L I N E
037500080206      //---------------------------------------------------------------
037600080206
037700080206     c     *Entry        plist
037800080206     c                   parm                    KPJBA
037900080626     c                   movel     kpjbu         flagintpot        1
038000080626     c* Attivo tasto funzionale di int.potenziali se non vengo dalla int
038100080626     c                   if        flagintpot<>'N'
038200080701     c                   eval      Intpotenz='1'
038300080626     c                   endif
038400080206
038500080206      /free
038600080206
038700080206       // Operazioni iniziali
038800080206       exsr RoutInz;
038900080206
039000080206       // Gestione video
039100080206       DOW $Fine = *off;
039200080206         select;
039300080530
039400080530         // Videata di selezioni
039500080206           when $Video = 'D1';
039600080206             exsr GesD01;
039700080530
039800080530         // SFL di scelta
039900080411           when $Video = 'S2';
040000080411             exsr GesS02;
040100080530
040200080530         // U N I O N E
040300080530           when $Video = 'S1';
040400080530             exsr GesS01;
040500080530
040600080530         // F U S I O N E
040700080603           when $Video = 'S3';
040800080603             exsr GesS03;
040900080206           other;
041000080206             $Fine = *on;
041100080206         endsl;
041200080206       ENDDO;
041300080206
041400080206       // Operazioni finali
041500080206       exsr RoutEnd;
041600080206
041700080206       //--------------------------------------------------------------
041800080206       //?Operazioni iniziali.
041900080206       //--------------------------------------------------------------
042000080206       BEGSR RoutInz;
042100080414       $inzd01=*on;
042200080606       v01cric='F'  ;
042300080206
042400080206         // Impostazione campi "fissi"
042500080208         TBLkut = 1;
042600080208         TBLcod = '1L';
042700080206
042800080206         // Reperimento dati job
042900080206         exsr DatiJob;
043000080206
043100080206         // Verifica errori e autorità profilo
043200080206         clear  wAbi;
043300080206         clear  dLAT;
043400080206         select;
043500080206         // - Se ho errori nei dati utente esco dal pgm
043600080208           when  DUTerr = 'E';
043700080206             $Fine = *on;
043800080206         // - Se non c'è l'abilitazione:
043900080206         //   - se 1° livello, abilitazioni al terminal
044000080206         //   - se 2° livello, abilitazioni al punto operativo
044100080206         //   - se sede è impossibile ma se capita mando a fine il pgm
044200080208           when  UTEaut = *blank;
044300080206             select;
044400080208               when  DUTlpo = '1';
044500080206                 wAbi  = 'TP';
044600080208               when  DUTlpo = '2';
044700080206                 wAbi  = 'PO';
044800080208               when  DUTlpo = 'S';
044900080206                 $Fine = *on;
045000080206             endsl;
045100080206         // - Altrimenti si caricano le abilitazioni del profilo
045200080206           other;
045300080208             dUTE01 = UTEfaf;
045400080208             if  §UTEpot <> *blank;
045500080208               wAbi = §UTEpot;
045600080206             else;
045700080208               wAbi = UTEaut;
045800080206             endif;
045900080206         ENDSL;
046000080206
046100080206         // Controllo se ok l'abilitazione dell'utente
046200080206         reset TIBS02ds;
046300111115         T02Mod = 'C';
046400080208         T02sif = knsif;
046500080208         T02cod = 'LAT';
046600080208         T02ke1 = wAbi;
046700080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
046800080206
046900080208         if  T02err  = *blank;
047000080208           dLAT = T02uni;
047100080206         endif;
047200080206
047300080206         // Errore o utente non abilitato
047400080208         if  T02err <> *blanks   or
047500080208             §LATabi = 'S';
047600080206           $ErrGrave   = *on;
047700080206           errMessage  = *on;
047800080206           errGenerico = *on;
047900080410           V1Dmsg = Msg(01);
048000080206           leavesr;
048100080206         endif;
048200080617
048300080617         clear trul31ds  ;
048400080617         I31abi = wabi    ;
048500080617         I31cdi = DUTdis  ;
048600080617         I31car = DUTare  ;
048700080617         I31cpo = DUTpou  ;
048800080617         TRUL31R   (KPJBA:trul31ds)   ;
048900080617
049000080617         if o31pog=*zeros   ;
049100080617           $ErrGrave   = *on;
049200080617           errMessage  = *on;
049300080617           errGenerico = *on;
049400080617          V1Dmsg = Msg(01);
049500080617           leavesr;
049600080617         endif         ;
049700100520
049800100520          // imposto la data del giorno - 1 anno    ;
049900100520          dataiso=datasys         ;
050000100520          w0080=%dec(dataiso)     ;
050100100520          Alfaoggi=%editc(w0080:'X')  ;
050200100520
050300100520          dataiso=dataiso- %years(1)      ;
050400100520          wgiorno=%subdt(dataiso:*d)  ;
050500100520          wgiorno=wgiorno-1     ;
050600100520          dataiso=dataiso- %days(wgiorno) ;
050700100520          wdtunanno=%dec(dataiso)    ;
050800080929
050900080929       // Carico ds con le INFO ordinate
051000080929          yy= 1    ;
051100080929          setll ('IFO')   TNTBE01l  ;
051200080929          reade ('IFO')   tntbe01l  ;
051300080929
051400080929  1       dow    not %eof(tntbe01l)  ;
051500080929
051600080929  2       if tbeatb=' '                  ;
051700080929          dIFO=tbEuni                ;
051800080929          kifoord(yy) =§ifoifg+%editc(§ifoogi:'X')+%subst(tbeke1:1:3) +
051900081009          (§ifodeb)+§ifoifs   ;
052000080929          yy=yy+1    ;
052100080929          endif     ;
052200080929
052300080929          reade ('IFO')   tntbe01l  ;
052400080929          enddo    ;
052500080929
052600080929          sorta kifoord  ;
052700080929
052800081112          open(e)   TFACO16L              ;
052900081112          if        not %open(tFACO16L)   ;
053000081112           %subst(wtfaco:7:4)='GRPF'      ;
053100081112           OPEN tfaco16l                  ;
053200081112         Endif                            ;
053300081112
053400080206       ENDSR;
053500080206
053600080206       //--------------------------------------------------------------
053700080206       //?Reperimento Dati del job (Utente/Operativi).
053800080206       //--------------------------------------------------------------
053900080206       BEGSR DatiJob;
054000080206
054100080206         in(E) §AzUte;
054200080206         if NOT %error;
054300080206           in(E) §DatiUte;
054400080206         endif;
054500080206         if %error or RSut = *blanks;
054600080206           clear TIBS34ds;
054700080206           tibs34r(tibs34ds);
054800080206           in §AzUte;
054900080206           in §DatiUte;
055000080206         endif;
055100080206
055200080206       ENDSR;
055300080206
055400080206       //--------------------------------------------------------------
055500080206       //?Gestione videata D01
055600080206       //--------------------------------------------------------------
055700080206       BEGSR GesD01;
055800080206
055900080206         // Inizializzazione videata
056000080414
056100080414         // campi da pulire sempre se non c'e' errore
056200080414         if  errGenerico = *off;
056300080414           clear v01dric;
056400080414         endif;
056500080414
056600080414         if  $inzd01 = *on;
056700080411           clear v01dric;
056800080523           v01ptdb='T';
056900111115           clear V01fls;
057000111115           clear V01dfls;
057100080618           v01ela ='T';
057200080411           clear v01fil ;
057300080411           clear v01rag ;
057400080415           clear v01prv ;
057500080414          $Inzd01=*off;
057600080411         endif ;
057700080606
057800080606         // Pulisco sempre
057900080606           clear Sedi   ;
058000080606           clear nrrsed ;
058100080606           clear Filiali;
058200080606           clear nrrfil ;
058300080606           clear Fusione;
058400080606           clear nrrfus ;
058500080606           clear tipofus;
058600111115           clear Catfus ;
058700080206
058800080206         // Emissione testata
058900080411         if  errGenerico = *off;
059000080409           write MK07T01;
059100080411         endif;
059200080206
059300080206         // Emissione videata
059400080409         exfmt MK07D01;
059500080411
059600080206         clear V1Dmsg;
059700080523         clear errFILIALE;
059800111115         clear errFLS;
059900080411         clear errGenerico;
060000080207
060100080206         select;
060200080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
060300080206           when  $ErrGrave = *on;
060400080206             $Fine  = *on;
060500080411
060600080206         // F3=Fine
060700080208           when dsp_aid = c_F03;
060800080409           $Fine = *on;
060900080411
061000080411         // F7=Int.potenziali
061100080411           when dsp_aid = c_F07;
061200080606           EXSR  INTPOTG ;
061300080409            other           ;
061400080411
061500080411             exsr Contrd01 ;
061600080411
061700080414         // Avanzamento se non errori
061800080414
061900080411             if errGenerico=*off;
062000080415
062100080523            //  Visualizzo primo sfl di scelta;
062200080411              $Video = 'S2';
062300080411              $InzS02 = *on;
062400080603
062500080603           // Pulisco qui il posizionamento cursore della 2 videata
062600080603           // perchè in caso di conferma devo riemettere la videata
062700080603           //  in cui si sono effettuare le scelte per unione/fusione
062800080603              clear C02csr;
062900080603              clear pagric;
063000080603             endif  ;
063100080206         endsl;
063200080206
063300080411       // Tipo richiamo
063400080411          if v01cric='F' ;
063500080411          v01dric='F U S I O N E' ;
063600080411          ELSE ;
063700080411          v01dric=' U N I O N E ' ;
063800080411          ENDIF ;
063900080411
064000080206       ENDSR;
064100080606       //--------------------------------------------------------------
064200080606       //?Interrogazione potenziali da prima videata
064300080606       //--------------------------------------------------------------
064400080606       BEGSR    INTPOTG   ;
064500100520
064600100520       clear trmk01ds   ;
064700100520
064800100520           mk01k11='2'   ;
064900100520           kpjbu=trmk01ds;
065000100520           TRMK01R (kpjba) ;
065100100520           trmk01ds=kpjbu  ;
065200100520
065300100520           d11cpo=mk01cpo   ;
065400080606
065500080606           // Selezionato potenziale
065600080606 1         if  d11cpo>0;
065700080606             chain(N)  d11cpo  TNCPO000;
065800080606 2            if %found(tncpo01l) ;
065900080606         // U N I O N E
066000080606 3         if   V01cric='U';
066100080606 4           if cpofsf='S'   ;
066200080606              sedi(1)=cpocpo  ;
066300080606 x4          else    ;
066400080606              filiali(1)=cpocpo  ;
066500080606 4           endif   ;
066600080606           $video='S1'  ;
066700080606           $InzS01 = *on;
066800080606 x3        else   ;
066900080606         // F U S I O N E
067000080606           Fusione(1)=cpocpo  ;
067100080606           tipofus(1)=cpoFSF  ;
067200080606           $video='S3'  ;
067300080606           $InzS03 = *on;
067400080606 3         endif;
067500080606 2         endif;
067600080606 1         endif;
067700080606            ENDSR    ;
067800080411       //--------------------------------------------------------------
067900080411       //?Controllo campi 1 videata
068000080411       //--------------------------------------------------------------
068100080411       BEGSR ContrD01;
068200080415
068300080415         if v01prv<>'  ' ;
068400080415         clear tblkey;
068500080415         tblkey=v01prv;
068600080415
068700080415         chain  (1:'PR':tblkey) tabel00f;
068800080415 2       if  not %found(tabel00f);
068900080415           errPRV=*on;
069000080415           errMessage  = *on;
069100080415           errGenerico = *on;
069200080415           V1Dmsg = Msg(12);
069300080523           leavesr;
069400080415         endif ;
069500080523         endif ;
069600080415
069700111115       //?--> Categoria potenziale <--
069800111116         clear V01dfls;
069900111115       //?Ricerca
070000111115         IF  %scan('?':V01fls) > 0;
070100111115           clear TIBS02ds;
070200111115           T02mod = 'R';
070300111115           T02sif = knsif;
070400111115           T02cod = 'CPO';
070500111115           TNTBE_RicercaControllo (kpjba : TIBS02DS);
070600111115           IF  T02err = *blank;
070700111115             V01fls = T02ke1;
070800111115           ENDIF;
070900111115         ENDIF;
071000080411
071100111115       //?Controllo
071200111115         IF  V01fls <> *blanks;
071300111115           clear dCPO;
071400111115           clear TIBS02ds;
071500111115           T02mod = 'C';
071600111115           T02sif = knsif;
071700111115           T02cod = 'CPO';
071800111116           T02ke1 = V01fls;
071900111115           TNTBE_RicercaControllo (kpjba : TIBS02DS);
072000111115
072100111115           IF  T02err <> *blank;
072200111115             errFLS      = *on;
072300111115             errMessage  = *on;
072400111115             errGenerico = *on;
072500111115             V1Dmsg = msg(07);
072600111115             leavesr;
072700111115           ENDIF;
072800111115           dCPO = T02uni;
072900111115           V01dfls = §CPOdes;
073000111115 1       ENDIF;
073100080415
073200080415         // filiale di appartenenza
073300080414 1       if errGenerico=*off and v01fil>0;
073400080411         chain  v01fil  azorg01l;
073500080414 2       if not %found(azorg01l) ;
073600080523           errFILIALE=*on;
073700080411           errMessage  = *on;
073800080411           errGenerico = *on;
073900080411           V1Dmsg = Msg(8);
074000080414 2       endif;
074100080414 1       endif;
074200080411       ENDSR;
074300080206       //--------------------------------------------------------------
074400080523       //?Gestione videata S01   - UNIONE
074500080206       //--------------------------------------------------------------
074600080409       BEGSR GesS01;
074700080207
074800080207         // Inizializzazione videata
074900080409         if  $InzS01 = *on;
075000080409            exsr InzS01;
075100080409            $InzS01  = *off;
075200080207         endif;
075300080207
075400080207         // Posizionamento cursore
075500080410           C01rcd = 1;
075600080410           sfldsp_n=*off;
075700080207
075800080207         // Emissione Testata e Piede con tasti funzionali abilitati
075900080207         if  errGenerico = *off;
076000080409           write  MK07T01;
076100080409           write  MK07P01;
076200080207         endif;
076300080207
076400080207         // Emissione videata
076500080409         exfmt  MK07C01;
076600080410
076700080207         reset errMessage;
076800080207         reset errGenerico;
076900080207         clear V1Dmsg;
077000080523         clear errSEDE ;
077100080526         clear errFIL  ;
077200080207
077300080523 1       SELECT;
077400080207
077500080207         // - F3=Fine
077600080523 1         WHEN  dsp_aid = c_F03;
077700080409            $Fine = *on;
077800080207
077900080207         // - F12=Ritorno
078000080523 1         WHEN  dsp_aid = c_F12;
078100080606           if nrrsed(1)>0 or  nrrfil(1)>0   ;
078200080523              $Video = 'S2';
078300080606           else  ;
078400080606              $Video = 'D1';
078500080606              $inzd01=*on;
078600080606           endif ;
078700080414
078800080414         // F7=Int.potenziali
078900080523 1         when dsp_aid = c_F07;
079000080603           exsr   INTpotU;
079100080530 2           if  errGenerico = *on;
079200080530           write  MK07T01;
079300080530           write  MK07P01;
079400080530           endif  ;
079500080414
079600080530 x1      // Invio / F6
079700080207           OTHER;
079800080409             exsr ContrS01 ;
079900080523 2           if  errGenerico = *on;
080000080207               leavesr;
080100080523 2           endif;
080200080530
080300080530         // F6=conferma UNIONE
080400080530 1         if   dsp_aid = c_F06;
080500080530           exsr   ConfUNIONE;
080600080530           endif  ;
080700080207
080800080523 1       ENDSL;
080900080207
081000080207       ENDSR;
081100080207
081200080409       //--------------------------------------------------------------
081300080603       //?Interrogazione potenziali per UNIONE
081400080526       //--------------------------------------------------------------
081500080603           Begsr INTPOTU ;
081600080526
081700100520           clear trmk01ds   ;
081800080618           if v01sncpo>0 and v01srag<>*blanks   ;
081900100805           mk01rag=%subst(v01srag:1:8)   ;
082000080618           else     ;
082100080618             if s01fncpo>0 and s01frag<>*blanks   ;
082200100805             mk01rag=%subst(s01frag:1:8)   ;
082300080618             endif    ;
082400080618           endif    ;
082500100520
082600100520             MK01K11='4'  ;
082700100520             kpjbu=trmk01ds;
082800100520             TRMK01R (kpjba) ;
082900100520             trmk01ds=kpjbu  ;
083000100520             D11CPO=Mk01Cpo  ;
083100080526
083200080526           // Selezionato potenziale
083300080526 1         if  d11cpo>0;
083400080530             chain(N)  d11cpo  TNCPO000;
083500080526
083600080526            // Se sede
083700080526 2            if not %found(tncpo01l) or (cpofsf='S' and sedi(1)>0) ;
083800080526               errSEDE =*on;
083900080526               errMessage  = *on;
084000080526               errGenerico = *on;
084100080526               V1Dmsg = Msg(16);
084200080530               %subst(v1dmsg:5:11)=%char(cpocpo) ;
084300080530               %subst(v1dmsg:17:12)=cporag ;
084400080526               leavesr ;
084500080526 2            endif  ;
084600080526
084700080526            // Se sede deve avere partita iva o codice fiscale
084800080526 2            if cpofsf='S' and cpopiv=*blanks and cpocdf=*blanks ;
084900080526               errSEDE =*on;
085000080526               errMessage  = *on;
085100080526               errGenerico = *on;
085200080526               V1Dmsg = Msg(15);
085300080526               leavesr ;
085400080526 2            endif  ;
085500080526
085600080526              // Carico dati sede
085700080526 2            if cpofsf='S' ;
085800080526              kcpo=d11cpo   ;
085900080526              exsr ClearCTL1 ;
086000080526              exsr RiempiCTL1 ;
086100080526              leavesr ;
086200080526 2            endif ;
086300080526
086400080526            // Se Filiale
086500080526 2            if cpofsf='F' and Filiali(7)>0 ;
086600080526               errSEDE =*on;
086700080526               errMessage  = *on;
086800080526               errGenerico = *on;
086900080526               V1Dmsg = Msg(14);
087000080526               leavesr ;
087100080526 2            endif  ;
087200080526
087300080526              // cerco primo record di sfl libero per impostarlo
087400080526              s01nrr=0 ;
087500080526              s01fncpo=99999999 ;
087600080526 2            dow  s01nrr<7 and (s01fncpo<>0 or s01ffcpo<>0) ;
087700080526                s01nrr=s01nrr+1 ;
087800080526                chain  s01nrr mk07s01;
087900080526 2            enddo  ;
088000080526
088100080526 2            if s01fncpo=0 and s01ffcpo=0 ;
088200080526                kcpo=d11cpo;
088300080526                exsr riempiSFL1 ;
088400080530                protpotFIL=*off;
088500080530                update mk07s01  ;
088600080526 2            endif;
088700080526 1          endif;
088800080526            ENDSR;
088900080526       //--------------------------------------------------------------
089000080523       //?controlli videata S01  - UNIONE
089100080409       //--------------------------------------------------------------
089200080409       BEGSR ContrS01;
089300080410
089400080523       // Solo se non protetto
0895000805261      if  ProtpotSED=*off    ;
089600080523
089700080410       // Immettere cod potenziale
0898000805262      if   v01sncpo=0 and v01sfcpo=0  ;
089900080530           exsr clearCTL1  ;
090000080523           errSEDE =*on;
090100080410           errMessage  = *on;
090200080409           errGenerico = *on;
090300080414           V1Dmsg = Msg(03);
0904000805262      endif;
090500080410
090600080523         fcpo=v01Sfcpo      ;
090700080523         ncpo=v01Sncpo      ;
090800080530         chain(N)  Kcpo  TNCPO000;
090900080410
091000080410         // Codice potenziale NON trovato
091100080526 2       if  NOT  %found(TNCPO01L);
091200080523           errSEDE =*on;
091300080410           errMessage  = *on;
091400080410           errGenerico = *on;
091500080414           V1Dmsg = Msg(03);
091600080526 x2    else ;
091700080526           exsr clearCTL1         ;
091800080604           if cpocpo<>savcpo
091900080604              and savcpo>0       ;
092000080526           pulizforz='S'       ;
092100080526           endif               ;
092200080526
092300080604           savcpo =cpocpo      ;
092400080526
092500080526           exsr riempiCTL1         ;
092600080526           clear Pulizforz     ;
092700080526 2     endif ;
092800080523 1     endif ;
092900080410
093000080410 1       if errGenerico=*off;
093100080410
093200080414       // controllo SFL
093300080526       s01nrr=1;
093400080526       clear contafil     ;
093500080526 2     dow s01nrr<=record and errGenerico=*off;
093600080523       clear errFIL   ;
093700080410
093800080526       chain  s01nrr mk07s01;
093900080410
094000080523 3     if s01ffcpo>0 or s01fncpo>0;
094100080523         fcpo=s01ffcpo      ;
094200080523         ncpo=s01fncpo      ;
094300080530         chain(N)  Kcpo  TNCPO000;
094400080410
094500080410         // Codice potenziale NON trovato
094600080410 4       if  NOT  %found(TNCPO01L);
094700080523           errFIL   =*on;
094800080410           errMessage  = *on;
094900080410           errGenerico = *on;
095000080618           V1Dmsg = Msg(27);
095100080526           else ;
095200080526           contafil=contafil+1 ;
095300080410 4     endif;
095400080526
095500080526         // Se non è selezione da sfl del pgm, ricarico i dati
095600080526 4       if filiali(s01nrr)=0 ;
095700080526         ProtpotFIL=*off    ;
095800080526         exsr RiempiSFL1;
095900080526         else    ;
096000080526         ProtpotFIL=*on     ;
096100080526 4       endif          ;
096200080618
096300080618         // controllo se utente abilitato all'aggiornamento
096400080618 4       if Errfil=*off   ;
096500080618           Indx=%lookup(s01ffil:pog)   ;
096600080618 5         if Indx=0    ;
096700080618             errFIL   =*on;
096800080618             errMessage  = *on;
096900080618             errGenerico = *on;
097000080618             V1Dmsg = Msg(26);
097100080618 5         endif        ;
097200080618 4       endif        ;
097300080526
097400080526         // Se la filiale presenta una partita iva o cod fiscale
097500080526         //       diversi --> errore forzabile
097600080526
097700080618 4       if  Errfil=*off and S01forzpiv=' '  ;
097800080526 5       if (s01fpiv<>v01spiv and s01fpiv<>*blanks and v01spiv<>*blanks) or
097900080526            (s01fcdf<>v01scdf and s01fcdf<>*blanks and v01scdf<>*blanks)  ;
098000080526           errFIL   =*on;
098100080526           errMessage  = *on;
098200080526           errGenerico = *on;
098300080526           V1Dmsg = Msg(17);
098400080526           s01forzpiv='S' ;
098500080526 5       endif            ;
098600080526 4       endif            ;
098700080410
098800080526 x3      else  ;
098900080526         exsr ClearSFL1 ;
099000080530         ProtpotFIL=*off    ;
099100080410 3       endif  ;
099200080410
099300080410       update mk07s01;
099400080530       ProtpotFIL=*off    ;
099500080410
099600080526       s01nrr=s01nrr+1;
099700080410 2     enddo;
099800080410
099900080523       // Immettere almeno un figlio
100000080526 2       if errGenerico=*off and contaFil=0;
100100080410          chain  1 mk07s01;
100200080523           errFIL   =*on;
100300080410           errMessage  = *on;
100400080410           errGenerico = *on;
100500080410           V1Dmsg = Msg(06);
100600080410          update mk07s01;
100700080410 2       endif;
100800080410 1     endif;
100900080409
101000080409       ENDSR;
101100080530       //--------------------------------------------------------------
101200080530       //?Conferma unione potenziali
101300080530       //--------------------------------------------------------------
101400080530       BEGSR ConfUNIONE ;
101500080530       // Leggo i potenziali di FILIALE ed aggiorno con PIV e CDF
101600080530       //  della sede
101700080530
101800080530       s01nrr=1;
101900080530 2     dow s01nrr<=record ;
102000080530
102100080530       chain  s01nrr mk07s01;
102200080530
102300080530 3     if s01ffcpo>0 or s01fncpo>0;
102400080530         fcpo=s01ffcpo      ;
102500080530         ncpo=s01fncpo      ;
102600080530         chain  Kcpo  TNCPO000;
102700080530         if %found(tncpo01l)  ;
102800080530         cpopiv=v01spiv;
102900080530         cpocdf=v01scdf;
103000080530         dcpo01=cporst ;
103100080530         §cporicuni='S' ;
103200080530         cporst=dcpo01 ;
103300080530         cpodtr=*date  ;
103400080530         update tncpo000 ;
103500080530         endif   ;
103600080530       endif   ;
103700080530       s01nrr=s01nrr+1;
103800080530
103900080530       enddo   ;
104000080530
104100100520       // Chaino la SEDE per aggiornare flag unione
104200080530
104300080530         fcpo=v01Sfcpo      ;
104400080530         ncpo=v01Sncpo      ;
104500080530         chain  Kcpo  TNCPO000;
104600080530
104700080530         // Codice potenziale NON trovato
104800080530 2       if  %found(TNCPO01L);
104900080530         dcpo01=cporst ;
105000080530         §cporicuni='S' ;
105100080530         cporst=dcpo01 ;
105200080530         update tncpo000 ;
105300080530         endif ;
105400080530
105500080530         // imposto il posizionamento cursore per il sfl che emetto
105600080530         //  aggiornato
105700080606         if nrrSED(1)> 0  or nrrfil(1)>0 ;
105800080606           C02csr=nrrSED(1) ;
105900080606           if c02csr=0 ;
106000080606             C02csr=nrrFIL(1) ;
106100080606           endif     ;
106200080606           pagric=(c02csr/C_sflPag)+1 ;
106300080606           $Video = 'S2';
106400080606           $InzS02 = *on;
106500080606         else           ;
106600080606           $Video = 'D1';
106700080606           $InzD01 = *on;
106800080606         endif    ;
106900080530
107000080530       ENDSR;
107100080409
107200080207       //--------------------------------------------------------------
107300080415       //?Inizializzazione videata S01 per  U N I O N E
107400080207       //--------------------------------------------------------------
107500080409       BEGSR InzS01;
107600080207
107700080207       // Pulizia subfile
107800080207         SflDsp_N    = *on;
107900080207         SflDspCtl_N = *on;
108000080409         write  MK07C01;
108100080207         SflDspCtl_N = *off;
108200080207         SflEnd      = *off;
108300080530
108400080530         clear pagric;
108500080530         clear C01rcd;
108600080409         S01nrr=1 ;
108700080207         clear V1Dmsg;
108800080207         errMessage  = *off;
108900080207         errGenerico = *off;
109000080523         errScelta   = *off;
109100080526         errFil      = *off;
109200080526         errSede     = *off;
109300080409
109400080409       // Carico una pagina vuota
109500080410          clear  MK07S01;
109600080414
109700080414
109800080523          dow   s01nrr<= Record ;
109900080410           write MK07S01;
110000080409           S01nrr += 1;
110100080414          endDO      ;
110200080409
110300080409       // SFL CONTROL
110400080409
110500080523          exsr    ClearCTL1 ;
110600080414
110700080415       // Se passata SEDE imposto
110800080523          if SEDI(1)>0     ;
110900080523            kcpo=sedI(1) ;
111000080530            chain(N) kcpo   tncpo01l;
111100080414            if   %found(tncpo01l) ;
111200080523              exsr RiempiCTL1;
111300080523              eval ProtPOTSED=*on ;
111400080414            endif    ;
111500080414          endif    ;
111600080523
111700080523          FF=1  ;
111800080530          clear errFIL   ;
111900080530          protpotFIL=*on ;
112000080523          dow FILIALI(ff)>0     ;
112100080523            kcpo=filiali(ff) ;
112200080530            chain(N) kcpo   tncpo01l;
112300080523            if   %found(tncpo01l) ;
112400080530              s01nrr=ff  ;
112500080530              chain   s01nrr  mk07s01  ;
112600080523              exsr RiempiSFL1;
112700080530
112800080530              update mk07s01  ;
112900080523            endif    ;
113000080523
113100080523          ff=ff+1  ;
113200080523          enddo    ;
113300080530          protpotFIL=*off;
113400080207
113500080207       ENDSR;
113600080207
113700080410       //--------------------------------------------------------------
113800080523       //?Pulizia dati codice SEDE
113900080410       //--------------------------------------------------------------
114000080523          BEGSR   ClearCTL1  ;
114100080523          clear   v01Sfcpo   ;
114200080523          clear   v01Sncpo   ;
114300080523          protpotSED=*off       ;
114400080523          clear   v01Srag    ;
114500080523          clear   v01Sloc    ;
114600080523          clear   v01Scap    ;
114700080523          clear   v01Sprv    ;
114800080523          clear   v01Sfil    ;
114900080523          clear   v01Spiv    ;
115000080523          clear   v01Scdf    ;
115100080523          clear   v01Sric    ;
115200080523          clear   v01Sdmerc  ;
115300111115
115400111115          clear   v01sfls;
115500111115          clear   v01scad;
115600111115
115700080410          ENDSR;
115800080414       //--------------------------------------------------------------
115900080523       //?carica dati della SEDE
116000080414       //--------------------------------------------------------------
116100080523          BEGSR   RiempiCTL1  ;
116200080414
116300080522       v01Sfcpo=fcpo  ;
116400080522       v01Sncpo=ncpo  ;
116500080522       v01srag=cporag;
116600080522       v01sloc=cpocit;
116700080522       v01scap=cpocap;
116800080523       if cponaz=*blanks  ;
116900080522       v01sprv=cpoprv;
117000080523       else  ;
117100080523       v01sprv=cponaz;
117200080523       endif ;
117300111115
117400111115       //?Categoria potenziale
117500111115         V01sfls = CPOfls;
117600111115
117700111115       //?Prossima attività
117800111115         sav_CPOcpo = CPOcpo;
117900111115         exsr RIC_proATT;
118000111115         V01scad = sav_ATCcad;
118100080522
118200080522         v01sfil=cpoflt;
118300080522         v01spiv=cpopiv;
118400080522         v01scdf=cpocdf;
118500080523         clear tblkey  ;
118600080523         tblkey=%editc(cposct:'X') ;
118700080523         chain  (1:'1L':tblkey) tabel00f;
118800080523 1       if %found(tabel00f);
118900080522         v01sdmerc=tbluni;
119000080522         else ;
119100080522         v01sdmerc=*all'?' ;
119200080523 1       endif  ;
119300080522
119400080414         dcpo01=cporst;
119500080523 1     select   ;
119600080522         when §cporicfus='S' and §cporicuni='S';
119700080522           v01sric='F/U'     ;
119800080522         when §cporicfus='S' ;
119900080618           v01sric=' Fu'     ;
120000080522         when §cporicuni='S' ;
120100080618           v01sric=' Un'     ;
120200080522         other  ;
120300080522           v01sric='   ';
120400080523 1     endsl;
120500080526
120600080526       // Se devo pulire le forzature nel sfl eseguo
120700080526       if Pulizforz='S'  ;
120800080526       s01nrr=1   ;
120900080526         dow s01nrr<=record  ;
121000080526         chain   s01nrr   mk07s01  ;
121100080526         clear s01forzpiv ;
121200080526
121300080526         if filiali(s01nrr)>0 ;
121400080526         ProtpotFil=*on   ;
121500080526         else  ;
121600080526         ProtpotFil=*off  ;
121700080526         endif ;
121800080526         update mk07s01   ;
121900080526
122000080526         s01nrr=s01nrr+1     ;
122100080526         enddo   ;
122200080526
122300080526         ProtpotFil=*off  ;
122400080526       endif     ;
122500080414
122600100520         ENDSR;
122700111115
122800111115       //--------------------------------------------------------------
122900111115       //?Ricerca la prossima attività da eseguire sul potenziale
123000111115       //--------------------------------------------------------------
123100111115         BEGSR  RIC_proATT;
123200111115
123300111115           $EndAtt = *off;
123400111115           clear sav_ATCcad;
123500111115           exec sql
123600111115            declare ATT cursor for select ATCcad from TIATC00F
123700111115            where ATCcpo = :sav_CPOcpo and atcdco = 0
123800111115            order by ATCdad, ATChda;
123900111115           exec sql
124000111115            open ATT;
124100111115           IF sqlcode < 0;
124200111115             $EndAtt = *on;
124300111115           ENDIF;
124400111115           DOW not $EndAtt;
124500111115             exec sql
124600111115             fetch next from ATT into :ATCcad;
124700111115             IF sqlcod = 100 or sqlcod < 0;
124800111115               $EndAtt = *on;
124900111115               leave;
125000111115             ENDIF;
125100111115             sav_ATCcad = ATCcad;
125200111115             leave;
125300111115           ENDDO;
125400111115           exec sql close ATT;
125500111115
125600111115         ENDSR;
125700080526       //--------------------------------------------------------------
125800080526       //?Pulizia riga filiale per unione
125900080526       //--------------------------------------------------------------
126000080526          BEGSR   ClearSFL1  ;
126100080526          clear s01ffcpo  ;
126200080526          clear s01fncpo  ;
126300080526          clear s01frag   ;
126400080526          clear s01floc   ;
126500080526          clear s01fcap   ;
126600080526          clear s01fprv   ;
126700080526          clear s01ffil   ;
126800080526          clear s01fpiv   ;
126900080526          clear s01fcdf   ;
127000080526          clear s01fric   ;
127100080526          clear s01fdmerc ;
127200080526          clear s01forzpiv;
127300111115
127400111115          clear s01fls;
127500111115          clear s01cad;
127600111115
127700080526          ENDSR     ;
127800080523       //--------------------------------------------------------------
127900080523       //?carica dati delle FILIALI
128000080523       //--------------------------------------------------------------
128100080523          BEGSR   RiempiSFL1  ;
128200080523
128300080523       s01ffcpo=fcpo  ;
128400080523       s01fncpo=ncpo  ;
128500080523       s01frag=cporag;
128600080523       s01floc=cpocit;
128700080523       s01fcap=cpocap;
128800080523       if cponaz=*blanks  ;
128900080523       s01fprv=cpoprv;
129000080523       else  ;
129100080523       s01fprv=cponaz;
129200080523       endif ;
129300080523
129400111115
129500111115       //?Categoria potenziale
129600111115         S01fls = CPOfls;
129700111115
129800111115       //?Prossima attività
129900111115         sav_CPOcpo = CPOcpo;
130000111115         exsr RIC_proATT;
130100111115         S01cad = sav_ATCcad;
130200080523
130300080523         s01ffil=cpoflt;
130400080523         s01fpiv=cpopiv;
130500080523         s01fcdf=cpocdf;
130600080523         clear tblkey  ;
130700080523         tblkey=%editc(cposct:'X') ;
130800080523         chain  (1:'1L':tblkey) tabel00f;
130900080523 6       if %found(tabel00f);
131000080523         s01fdmerc=tbluni;
131100080523         else ;
131200080523         s01fdmerc=*all'?' ;
131300080523         endif  ;
131400080523
131500080523         dcpo01=cporst;
131600080523       select   ;
131700080523         when §cporicfus='S' and §cporicuni='S';
131800080523           s01fric='F/U'     ;
131900080523         when §cporicfus='S' ;
132000080618           s01fric=' Fu'     ;
132100080523         when §cporicuni='S' ;
132200080618           s01fric=' Un'     ;
132300080523         other  ;
132400080523           s01fric='   ';
132500080523       endsl;
132600080523
132700080523          ENDSR;
132800080411       //--------------------------------------------------------------
132900080523       //?Gestione videata S02   - SCelta potenziali
133000080411       //--------------------------------------------------------------
133100080411       BEGSR GesS02;
133200080411
133300080411         // Inizializzazione videata
133400080411         if  $InzS02 = *on;
133500080411            exsr InzS02;
133600080411             $InzS02  = *off;
133700080411         endif;
133800080411
133900080411         // Posizionamento cursore
134000080411         if  C02csr  > *zero;
134100080411           C02rcd = C02csr;
134200080411         else;
134300080411
134400080411         // Se non è stato caricato nulla si riemette la videata D01
134500080411           C02rcd = 1;
134600080411           $Video = 'D1';
134700080411           errMessage  = *on;
134800080411           errGenerico = *on;
134900080411           errRAG      = *on;
135000080414           V1Dmsg = Msg(09);
135100080411           leavesr;
135200080411         endif;
135300080411
135400080411
135500080411         // Posizionamento cursore
135600080411           sfldsp_n=*off;
135700080411
135800080411         // Emissione Testata e Piede con tasti funzionali abilitati
135900080604         if  errGenerico = *off or ricInt ='S'  ;
136000080414           write  MK07T01;
136100080411           write  MK07P02;
136200080604           clear  ricInt   ;
136300080411         endif;
136400080411
136500080411         // Emissione videata
136600080411         exfmt  MK07C02;
136700080411
136800080411         reset errMessage;
136900080411         reset errGenerico;
137000080411         clear V1Dmsg;
137100080523         clear errScelta ;
137200080411
137300080411         SELECT;
137400080411
137500080411         // - F3=Fine
137600080411           WHEN  dsp_aid = c_F03;
137700080411            $Fine = *on;
137800080411
137900080411         // - F12=Ritorno
138000080411           WHEN  dsp_aid = c_F12;
138100080411           $Video = 'D1';
138200080411
138300080414         // - Roll-Up
138400080414           WHEN  dsp_aid = c_RollUp;
138500080414           // - Verifica se raggiunto il limite di records registrabili
138600080414           //   nel subfile (9999)
138700080414             if  S02nrr = *hival;
138800080414               errMessage  = *on;
138900080414               errGenerico = *on;
139000080414               V1Dmsg = Msg(10);
139100080414             else;
139200080414               exsr RollUpS02;
139300080414             endif;
139400080414
139500080411         // Invio
139600080411           OTHER;
139700080523             exsr ContrS02  ;
139800080411             if  errGenerico = *on;
139900080411               leavesr;
140000080411             endif;
140100080411
140200080411         ENDSL;
140300080411
140400080411       ENDSR;
140500080411       //--------------------------------------------------------------
140600080414       //?Inizializzazione sfl 2
140700080411       //--------------------------------------------------------------
140800080411       BEGSR InzS02;
140900080411
141000080411       // Pulizia subfile
141100080411         SflDsp_N    = *on;
141200080411         SflDspCtl_N = *on;
141300080411         write  MK07C02;
141400080411         SflDspCtl_N = *off;
141500080411         SflEnd      = *off;
141600080411
141700080530         clear W_SflPag;
141800080414         $EoF = *off;
141900080411         clear C02rcd;
142000080414         clear S02nrr;
142100080411         clear V1Dmsg;
142200080411         errMessage  = *off;
142300080411         errGenerico = *off;
142400080415         errscelta=*off  ;
142500080604         clear savcpo    ;
142600080411
142700080411       // Carico pagina
142800080414          K48A=V01RAG;
142900080414          setll  K48A    TNCPO02l;
143000080411          exsr sr_ReadRec;
143100080411
143200080530       //  se vengo da videata di selezioni carico una pagina
143300080530       //    altrimenti il numero delle pagine dove è presente il record
143400080530       //    di sede
143500080530         if c02csr=0           ;
143600080530         totrig=C_SflPAg       ;
143700080530         else                  ;
143800080530         totrig=c_SflPag*pagric  ;
143900080530         endif                 ;
144000080530
144100080411         dow  $EoF   = *off     and
144200080530              S02nrr < totrig  ;
144300080411           exsr RollUpS02;
144400080411         enddo;
144500080411
144600080411       // -> forza l'impaginazione sul 1° rec. del subfile
144700080530       //    se vengo dalla videata di selezioni
144800080530
144900080411         if S02nrr > *zero;
145000080530           if c02csr=0 ;
145100080411           C02rcd  = 1;
145200080411           C02csr  = 1;
145300080530           else       ;
145400110831
145500110831           // Se c02csr > del numero di record, imposto il num max
145600110831             if c02csr>s02nrr ;
145700110831              c02csr=s02nrr ;
145800110831             endif  ;
145900080530           C02rcd  = c02csr;
146000080530           endif      ;
146100080530
146200080411         else;
146300080411           clear C02rcd;
146400110831           clear C02csr;
146500080411         endif;
146600080411
146700080411       ENDSR;
146800080411       //--------------------------------------------------------------
146900080411       //?Lettura x caricamento SFL
147000080411       //--------------------------------------------------------------
147100080411       BEGSR sr_ReadRec;
147200080411
147300080411         $RecOK  = *off;
147400080411
147500080411         DOW  $EoF   = *off   and
147600080411              $RecOK = *off;
147700080411
147800080411           // (se non rilevato stop record elaborabili in selezione)
147900080411             if  $EoF = *off;
148000080411               read  TNCPO002;
148100080411               $EoF  = (%eof(TNCPO02L));
148200080411             endif;
148300080411
148400080411           // Selezione record in base alle parzializzazioni
148500080411           if  $EoF  = *off;
148600080411             exsr sr_SelRec;
148700080411           endif;
148800080411
148900080411         ENDDO;
149000080411
149100080411       ENDSR;
149200080411       //--------------------------------------------------------------
149300080411       //?Selezione record in base alle parzializzazioni
149400080411       //--------------------------------------------------------------
149500080411       BEGSR sr_SelRec;
149600080411
149700080411         $RecOK  = *off;
149800080411
149900080411         // Parzializzazione per ragione sociale
150000080411         if  V01rag <> *blank;
150100080411         //xx    = %checkr(' ' : %subst(V01rag : 1 : 5));
150200080411           xx    = %checkr(' ' : V01rag);
150300080610           if   %subst(V01rag : 1 : xx) <> %subst(r_CPOrag : 1 : xx);
150400080411               $EoF = *on;
150500080411             leavesr;
150600080411           endif;
150700080411         endif;
150800080411
150900080411         select;
151000080415         // Parz. per Provincia
151100080610         when  v01prv<>'  ' and v01prv<>r_cpoprv;
151200080415             leavesr;
151300080411
151400080411           // Parzializzazione per filiale di appartenenza
151500080414           when  V01fil <> *zero    and
151600080610                 V01fil <> r_cpOflt;
151700080411             leavesr;
151800080411
151900080415           // parz. per potenziale FILIALE
152000080610           when  V01ptDB ='F' and  r_cpofsf='S'   ;
152100080411             leavesr;
152200080415           // parz. per Potenziale SEDE
152300080610           when  V01ptDB ='S' and r_cpofsf='F'   ;
152400080411             leavesr;
152500080411           ENDSL;
152600080411
152700111115       //?Parzializzazione per categoria potenziale
152800111115         IF  V01fls <> *blanks and V01fls <> r_CPOfls;
152900111115           leavesr;
153000111115         ENDIF;
153100080603
153200081009         // parz per Potenziali normalizzati
153300080603         if  V01ela <>'T' ;
153400080610         dcpo01=r_cporst ;
153500080603
153600080603         if   v01cric='U'  ;
153700080603           if  V01ela = 'S' and §cporicuni='S';
153800080603             leavesr;
153900080603           endif;
154000081009           if  V01ela = 'N' and §cporicuni=' ';
154100080603             leavesr;
154200080603           endif;
154300080603         endif;
154400080415
154500080603         if   v01cric='F'  ;
154600080603           if  V01ela = 'S' and §cporicfus='S';
154700080603             leavesr;
154800080603           endif;
154900081009           if  V01ela = 'N' and §cporicfus=' ';
155000080603             leavesr;
155100080603           endif;
155200080603         endif;
155300080411         endif;
155400080411
155500080411         // => record OK!
155600080411         $RecOK  = *on;
155700080411
155800080411       ENDSR;
155900080411       //--------------------------------------------------------------
156000080411       //?Caricamento singola pagina S02
156100080411       //--------------------------------------------------------------
156200080411       BEGSR RollUpS02;
156300080411
156400080411         S02nrr    = (W_SflPag * C_SflPag);
156500080411         W_SflPag += 1;
156600080411         SflNxtChg = *off;
156700080411
156800080411         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
156900080523 1       DOW  $EoF   = *off                   and
157000080411              S02nrr < (W_SflPag * C_SflPag)  and
157100080411              S02nrr < *hival;
157200080411
157300080411         // - Caricamento dati nel record del subfile
157400080414         clear s02sce  ;
157500080610         s02cpo=r_cpocpo;
157600080610         s02rag=r_cporag;
157700080610 2       if r_cpofsf='S' ;
157800080523          s02deb='Sd';
157900080411         ELSE         ;
158000080523          s02deb='Fi';
158100080523 2       endif;
158200080415
158300080610         s02loc=r_cpocit;
158400080610         %subst(s02loc:21:3)=' '+r_cpoprv;
158500080610         s02fil=r_cpoflt;
158600080610         s02piv=r_cpopiv;
158700080610         s02cdf=r_cpocdf;
158800080610         dcpo01=r_cporst;
158900111115
159000111115       //?Categoria potenziale
159100111115         S02fls = r_CPOfls;
159200111115
159300111115       //?Prossima attività
159400111115         sav_CPOcpo = r_CPOcpo;
159500111115         exsr RIC_proATT;
159600111115         S02cad = sav_ATCcad;
159700111115
159800080523 2     select   ;
159900080522         when §cporicfus='S' and §cporicuni='S';
160000080522           s02ric='F/U'     ;
160100080522         when §cporicfus='S' ;
160200080618           s02ric=' Fu'     ;
160300080522         when §cporicuni='S' ;
160400080618           s02ric=' Un'     ;
160500080522         other  ;
160600080522           s02ric='   ';
160700080523 2     endSL;
160800080415
160900080411
161000080411           S02nrr += 1;
161100080411           write MK07S02;
161200080411
161300080411         // - Lettura record successivo nell'archivio
161400080411           exsr sr_ReadRec;
161500080411
161600080523 1       ENDDO;
161700080411
161800080411         // Attivazione (eventuale) del SFLEND
161900080411         SflEnd   = ($EoF = *on);
162000080411
162100080411         // Posizionamento cursore al 1° rcd della pagina
162200080523 1       if  S02nrr > *zero;
162300080414           wPag   = S02nrr / C_SflPag;
162400080414           wRig   = S02nrr - (C_SflPag * wPag);
162500080411           C02rcd = wPag * C_SflPag;
162600080523 2         if  wRig > *zeros;
162700080415             C02rcd = C02rcd + 1;
162800080411           else;
162900080411             C02rcd = C02rcd - C_SflPag + 1;
163000080523 2         endif;
163100080523 x1      else;
163200080411           clear  C02rcd;
163300080523 1       endif;
163400080411
163500080411         C02csr = C02rcd;
163600080411
163700080411       ENDSR;
163800080414       //--------------------------------------------------------------
163900080414       //?controllo scelta SFL 2
164000080414       //--------------------------------------------------------------
164100080523       BEGSR contrS02 ;
164200080414
164300080523       clear ErrScelta ;
164400080523       clear nrrSED  ;
164500080415       clear SEDI ;
164600080415       clear SS   ;
164700080603
164800080522       clear nrrFIL    ;
164900080415       clear filiali;
165000080415       clear FF     ;
165100080603
165200080603       clear Fusione;
165300080603       clear nrrFus    ;
165400111115       clear CatFus  ;
165500080603       clear TipoFus  ;
165600080604       clear RicINt   ;
165700080625       clear savcpo    ;
165800080415
165900080414       readc mk07s02;
166000080526 1     DOW  NOT  %eof(TRMK07D);
166100080414
166200080415       // 5 - Visualizza
166300080526 2     if s02sce='5' ;
166400100520         clear trmk01ds;
166500100520          Mk01K10='N'   ;
166600100805          mk01cpo=s02cpo  ;
166700100520         trmk02r ( kpjba : trmk01ds );
166800080415         clear s02sce;
166900080523         c02csr=s02nrr  ;
167000080604         ricInt ='S'     ;
167100080617         errGenerico = *on;
167200080526 2     endif ;
167300080414
167400080603       // U N I O N E
167500080603 1a    if   V01cric='U';
167600080603
167700080603       // Scelta SEDE
167800080523
167900080526 2     if   s02sce='1' ;
168000080523
168100080523       // impossibile  unire una sede senza partita iva e cod fiscale
168200080526 3     if s02deb='Sd'    ;
168300080526 4     if s02piv=*blanks and s02cdf=*blanks ;
168400080523         errscelta=*on   ;
168500080523         SflNxtChg = *on ;
168600080523         errMessage  = *on;
168700080523         errGenerico = *on;
168800080523         C02csr = s02nrr    ;
168900080523         V1Dmsg = Msg(15);
169000080523         update mk07s02;
169100080523
169200080523         errscelta=*off  ;
169300080523         SflNxtChg = *off ;
169400080523         leavesr;
169500080526 4     endif  ;
169600080523
169700080415       ss=ss+1 ;
169800080526 4       if  ss<=2;
169900080522         sedi(ss)=s02cpo;
170000080523         nrrsed(ss)=s02nrr;
170100080526 4       endif;
170200080526 3     endif;
170300080415
170400080526 3     if s02deb='Fi'    ;
170500080618       // Errore se non abilitato al potenziale m
170600080618       Indx=%lookup(s02fil:pog)  ;
170700080618 4     if Indx=0;
170800080618         errscelta=*on   ;
170900080618         SflNxtChg = *on ;
171000080618         errMessage  = *on;
171100080618         errGenerico = *on;
171200080618         C02csr = s02nrr    ;
171300080618         V1Dmsg = Msg(26);
171400080618         update mk07s02;
171500080618
171600080618         errscelta=*off  ;
171700080618         SflNxtChg = *off ;
171800080618         leavesr;
171900080618 4      endif   ;
172000080618
172100080526       ff=ff+1 ;
172200080526 4       if  ff<=8;
172300080415         filiali(ff)=s02cpo;
172400080523         nrrfil(ff) =s02nrr;
172500080526 4       endif;
172600080526 3     endif;
172700080526 2     endif;
172800080603 1a    endif;
172900080603
173000080603       // F U S I O N E
173100080603 1a    if   V01cric='F';
173200080603 2     if   s02sce='1' ;
173300100520
173400100419
173500100419        ss=ss+1 ;
173600080603 3       if  ss<=3;
173700080603         Fusione(ss)=s02cpo;
173800080603         nrrfus(ss)=s02nrr;
173900080603         tipofus(ss)=s02deb;
174000111115       //?Categoria potenziale
174100111115         CatFus(ss)= S02fls;
174200100520
174300080603 3       endif;
174400080603 2     endif;
174500080603 1a    endif;
174600080603
174700080415
174800080526 2     if s02sce<>' ';
174900080415         SflNxtChg = *on ;
175000080415         else ;
175100080415         SflNxtChg = *off;
175200080526 2     endif;
175300080415
175400080414       update mk07s02;
175500080414
175600080414       readc mk07s02;
175700080526 1     enddo;
175800080415
175900080603       // U N I O N E
176000080603 1a    if   V01cric='U';
176100080603
176200080415       // Se più di una sede --> errore
176300080526 1     if sedi(2)>0   ;
176400080522         chain  nrrsed(2) mk07s02;
176500080603         V1Dmsg = Msg(13);
176600080603         C02csr = nrrsed(2) ;
176700080603         exsr ERRSFL2  ;
176800080523         leavesr;
176900080526 1     endif ;
177000080522
177100080526 1     if filiali(8)>0   ;
177200080522         chain  nrrfil(8)  mk07s02;
177300080523         C02csr = nrrfil(ff);
177400080415         V1Dmsg = Msg(14);
177500080603         exsr ERRSFL2  ;
177600080523         leavesr;
177700080526 1     endif ;
177800080523
177900080610       // Se non selezionato nè una sede nè una filiale errore bloccante
178000080617 1     if sedi(1)=0 and filiali(1)=0 and ricint=' ';
178100080610         chain  c02csr mk07s02;
178200080523         V1Dmsg = Msg(02);
178300080603         exsr ERRSFL2  ;
178400080523         leavesr;
178500080526 1     endif ;
178600080603 1a    endif ;
178700080603
178800080603       // F U S I O N E
178900080603 1a    if   V01cric='F';
179000080603
179100080603       // Se più di due potenziali da fondere --> errore
179200080603 1     if Fusione(3)>0   ;
179300080603         chain  nrrFus(3) mk07s02;
179400080603         C02csr = nrrFus(3) ;
179500080603         V1Dmsg = Msg(18);
179600080603         exsr ERRSFL2  ;
179700080603         leavesr;
179800080603 1     endif ;
179900080603
180000111115       //?Impossibile fondere 2 potenziali in cat. Eliminabile
180100111115 1     IF  CatFus(1) = 'E' and Catfus(2) = 'E' ;
180200080603         chain  nrrFus(2) mk07s02;
180300080603         C02csr = nrrFus(2) ;
180400080603         V1Dmsg = Msg(05);
180500080603         exsr ERRSFL2  ;
180600080603         leavesr;
180700080603 1     endif   ;
180800080603
180900080603       // Impossibile fondere 2 sedi
181000080603 1     if tipoFus(1)='S' and tipofus(2)='S' ;
181100080603         chain  nrrFus(2) mk07s02;
181200080603         C02csr = nrrFus(2) ;
181300080603         V1Dmsg = Msg(04);
181400080603         exsr ERRSFL2  ;
181500080603         leavesr;
181600080603 1     endif   ;
181700080603
181800080617       // Se non selezionato nessun codice --> errore
181900080617 1     if fusione(1)=0 and ricint=' ';
182000080610         chain  c02csr   mk07s02;
182100080603         V1Dmsg = Msg(02);
182200080603         exsr ERRSFL2  ;
182300080603         leavesr;
182400080603 1     endif   ;
182500080603 1a    endif   ;
182600080603
182700080617       // Se non devo riemettere la videata
182800080617 0     if ErrGenerico=*off   ;
182900080617
183000080526 1     if v01cric='U';
183100080415         $video='S1'  ;
183200080415         $InzS01 = *on;
183300080526 1     endif;
183400080603 1     if v01cric='F';
183500080603         $video='S3'  ;
183600080603         $InzS03 = *on;
183700080603 1     endif;
183800080617 0     endif;
183900080523
184000080414       ENDSR;
184100080603       //--------------------------------------------------------------
184200080603       //?Aggiornamento sfl per errore
184300080603       //--------------------------------------------------------------
184400080603       BEGSR ErrSFL2 ;
184500080603         errscelta=*on   ;
184600080603         SflNxtChg = *on ;
184700080603         errMessage  = *on;
184800080603         errGenerico = *on;
184900080603         update mk07s02;
185000080603
185100080603         errscelta=*off  ;
185200080603         SflNxtChg = *off ;
185300080603       ENDSR;
185400080411
185500080603       //--------------------------------------------------------------
185600080603       //?Gestione videata S03   - FUSIONE
185700080603       //--------------------------------------------------------------
185800080603       BEGSR GesS03;
185900080603
186000080603         // Inizializzazione videata
186100080603         if  $InzS03 = *on;
186200080624            clear forzacpo;
186300080603            exsr InzS03;
186400080603            $InzS03  = *off;
186500080609
186600080609         // Posizionamento cursore
186700080609           C03rcd = 1;
186800080609           C03csr = 1;
186900080603         endif;
187000080603
187100080603         // Emissione Testata e Piede con tasti funzionali abilitati
187200080603         if  errGenerico = *off;
187300080603           write  MK07T01;
187400080603           write  MK07P03;
187500080609          if c03csr>0 ;
187600080609          c03rcd=c03csr   ;
187700080609          endif;
187800080609         endif;
187900080603
188000080603         // Emissione videata
188100080603         exfmt  MK07C03;
188200080603
188300080603         reset errMessage;
188400080603         reset errGenerico;
188500080603         clear V1Dmsg;
188600080603         clear errFusione  ;
188700080618         clear errTipofus1 ;
188800080618         clear errTipofus2 ;
188900080603
189000080603 1       SELECT;
189100080603
189200080603         // - F3=Fine
189300080603 1         WHEN  dsp_aid = c_F03;
189400080603            $Fine = *on;
189500080603
189600080603         // - F12=Ritorno
189700080603 1         WHEN  dsp_aid = c_F12;
189800080617           // da sflcontrol vado a videata precednete
189900080617 1        if SflDSp_N=*on   ;
190000080617
190100080617             if nrrfus(1)>0 ;
190200080617                $Video = 'S2';
190300080617             else  ;
190400080617                $Video = 'D1';
190500080617                $inzd01=*on;
190600080617             endif ;
190700080617
190800080617          else  ;
190900080617           // da sfl vado a sflcontrol
191000080617 1        SflDSp_N=*on   ;
191100080617          $InzS03  = *on ;
191200080620          unlock tncpo01l ;
191300080620          unlock tncpo00F ;
191400080617          endif ;
191500080603
191600080603         // F7=Int.potenziali
191700080603 1         when dsp_aid = c_F07;
191800080604             exsr IntPotF   ;
191900080603 2           if  errGenerico = *on;
192000080603           write  MK07T01;
192100080603           write  MK07P03;
192200080603           endif  ;
192300080603
192400080603 x1      // Invio / F6
192500080603           OTHER;
192600080603             exsr ContrS03 ;
192700080603 2           if  errGenerico = *on;
192800080625 1            if SflDSp_N=*on   ;
192900080625               unlock tncpo00f ;
193000080625               unlock tncpo01l ;
193100080625              endif            ;
193200080603               leavesr;
193300080603 2           endif;
193400080603
193500080603         // F6=conferma FUSIONE
193600080603 1         if   dsp_aid = c_F06;
193700080619           exsr   ConfFUSIONE;
193800080603           endif  ;
193900080603
194000080603 1       ENDSL;
194100080603
194200080603       ENDSR;
194300080603       //--------------------------------------------------------------
194400080603       //?Inizializzazione videata S03 per  F U S I O N E
194500080603       //--------------------------------------------------------------
194600080603       BEGSR InzS03;
194700080603
194800080603       // Pulizia subfile
194900080603         SflDsp_N    = *on;
195000080603         SflDspCtl_N = *on;
195100080603         write  MK07C03;
195200080603         SflDspCtl_N = *off;
195300080603         SflEnd      = *off;
195400080603
195500080603         clear pagric;
195600080603         clear C03rcd;
195700080603         S03nrr=1 ;
195800080603         clear V1Dmsg;
195900080603         errMessage  = *off;
196000080603         errGenerico = *off;
196100080603         errScelta   = *off;
196200080603
196300080603       // SFL CONTROL
196400080603
196500080603          pulizpot1='S'  ;
196600080603          pulizpot2='S'  ;
196700080604          clear   v032tipf   ;
196800080604          clear   v032dtip    ;
196900080610          clear   v031tipf   ;
197000080610          clear   v031dtip    ;
197100080606          ColorRosso1=*off          ;
197200080606          ColorRosso2=*off          ;
197300080603          exsr    ClearCTL3 ;
197400080603
197500080603          SS=1  ;
197600080603          dow Fusione(ss)>0     ;
197700080603            kcpo=Fusione(ss) ;
197800080603            chain(N) kcpo   tncpo01l;
197900080603            if   %found(tncpo01l) ;
198000080603              s03nrr=ss  ;
198100080603              if ss=1    ;
198200080603              pulizpot1='C'  ;
198300080603              else       ;
198400080603              pulizpot2='C'  ;
198500080603              protpotdue=*on   ;
198600100525              endif      ;
198700080603              exsr RiempiCTL3;
198800080603            endif    ;
198900080603
199000080603          ss=ss+1  ;
199100080603          enddo    ;
199200080610          // Se ho selezionato solo un codice, mi posiziono sul secondo
199300080610          if fusione(2)=0  ;
199400080610          Errfusione=*on  ;
199500080610          endif   ;
199600080604
199700080603         SflDsp_N    = *on ;
199800080610         // Se SAVCPO impostato, vengo da manutenzione , per cui
199900080610         //  ricarico anche secondo potenziale
200000080610         If Fusione(2)=0 and  savcpo>0 ;
200100080625            kcpo=savcpo      ;
200200080625            chain(N) kcpo   tncpo01l;
200300080625            if   %found(tncpo01l) ;
200400080610            pulizpot2='C'  ;
200500080610            exsr RiempiCTL3;
200600080610            endif   ;
200700080625         endif   ;
200800100525
200900080603       ENDSR;
201000080603       //--------------------------------------------------------------
201100080603       //?Pulizia dati potenziali per fusione
201200080603       //--------------------------------------------------------------
201300080603          BEGSR   ClearCTL3  ;
201400080603          if pulizpot1='S'  ;
201500080603          clear   v031fcpo   ;
201600080603          clear   v031ncpo   ;
201700080603          clear   v031tipf   ;
201800080603          clear   v031dtip    ;
201900080603          clear   v031rag    ;
202000080603          clear   v031loc    ;
202100080603          clear   v031cap    ;
202200080603          clear   v031ind    ;
202300080603          clear   v031fil    ;
202400080603          clear   v031piv    ;
202500080603          clear   v031cdf    ;
202600080603          clear   v031cmerc  ;
202700080603          clear   v031dmerc  ;
202800080904          clear   c031cmuti  ;
202900080603          clear   v031cmm    ;
203000080603          clear   v031dcmm   ;
203100080603          clear   pulizpot1  ;
203200080603          clear   v031fsf    ;
203300111115          clear v031fls;
203400111115          clear v031cad;
203500111116          clear   codif1     ;
203600111116          RIcodif1=*off      ;
203700111115          $Contattato1 = *off;
203800080603          endif  ;
203900080603
204000080604          if pulizpot2='S'   ;
204100080603          clear   v032fcpo   ;
204200080603          clear   v032ncpo   ;
204300080604
204400080603          protpotdue=*off       ;
204500080603          clear   v032rag    ;
204600080603          clear   v032loc    ;
204700080603          clear   v032cap    ;
204800080603          clear   v032ind    ;
204900080603          clear   v032fil    ;
205000080603          clear   v032piv    ;
205100080603          clear   v032cdf    ;
205200080603          clear   v032cmerc  ;
205300080603          clear   v032dmerc  ;
205400080904          clear   c032cmuti  ;
205500080603          clear   v032cmm    ;
205600080603          clear   v032dcmm   ;
205700080603          clear   pulizpot2  ;
205800080603          clear   v032fsf    ;
205900111115          clear v032fls;
206000111115          clear v032cad;
206100111116          clear   codif2     ;
206200111116          RIcodif2=*off      ;
206300111115          $Contattato2 = *off;
206400080603          endif  ;
206500080603          ENDSR;
206600080603
206700080603       //--------------------------------------------------------------
206800080603       //?carica dati potenziale UNO del SFL3 - Fusione
206900080603       //--------------------------------------------------------------
207000080603          BEGSR   RiempiCTL3  ;
207100100525
207200100525        setll    kcpo   cnaco16l;
207300080603
207400080603       if pulizpot1='C' ;
207500111116
207600111116         if %equal(cnaco16l)  ;
207700111116           Codif1='S'  ;
207800111116           RICodif1=*on  ;
207900111116         else       ;
208000111116           Codif1='N'  ;
208100111116         endif       ;
208200100525
208300080603       v031fcpo=fcpo  ;
208400080603       v031ncpo=ncpo  ;
208500080603       v031rag=cporag;
208600080603       v031ind=cpovia;
208700080603       v031loc=cpocit;
208800080603       v031cap=cpocap;
208900080603       if cponaz=*blanks  ;
209000080603       %subst(v031loc:27:3)=' '+cpoprv;
209100080603       else  ;
209200080603       %subst(v031loc:26:4)=' '+cponaz;
209300080603       endif ;
209400080603
209500111115       //?Categoria potenziale
209600111116        clear V031fls;
209700111116        clear trmk05ds;
209800111116        IMK05cpo = CPOcpo;
209900111116        trmk05r (kpjba : TRMK05DS);
210000111116        IF  OMK05err = *blanks;
210100111116          V031fls = OMK05cat;
210200111116        ENDIF;
210300111115
210400111115       //?Prossima attività
210500111115         sav_CPOcpo = CPOcpo;
210600111115         exsr RIC_proATT;
210700111115         V031cad = sav_ATCcad;
210800080604
210900111115       //?Viste le nuove modifiche imposto 'F' se cat. eliminabile
211000111115         IF  V031fls = 'E';
211100080604           v031tipf='F'     ;
211200080610           v031dtip='FONDERE'  ;
211300111115         ENDIF;
211400111115
211500111115       //?Controllo se potenziale mai contattato
211600120914         dCPO01 = CPOrst;
211700120914         IF  §CPOdtpcon <> *blanks and §CPOdtpcon <> *zeros;
211800111115           $Contattato1 = *on;
211900111115         ENDIF;
212000080603
212100080603         v031fil=cpoflt;
212200080603         v031piv=cpopiv;
212300080603         v031cdf=cpocdf;
212400080603 2       if cpofsf='S' ;
212500080603          v031fsf='Sd';
212600080603         ELSE         ;
212700080603          v031fsf='Fi';
212800080603 2       endif;
212900080603
213000080603         v031cmerc=cposct;
213100080603         clear tblkey  ;
213200080603         tblkey=%editc(cposct:'X') ;
213300080603         chain  (1:'1L':tblkey) tabel00f;
213400080603 1       if %found(tabel00f);
213500080603         v031dmerc=tbluni;
213600080904         ds1l=     tbluni;
213700080904         c031cmuti=§1luti ;
213800080603         else ;
213900080603         v031dmerc=*all'?' ;
214000080904         c031cmuti='N'    ;
214100080603 1       endif  ;
214200080603
214300080603         v031cmm=cpocmm;
214400080604         if cpocmm> 0  ;
214500130916           chain  (CPOcmm)  AZCMM000;
214600130916 1         if %found(AZCMM01L);
214700130916             v031dcmm=CMMdes;
214800080604           else ;
214900130916             v031dcmm=*all'?' ;
215000080604 1         endif  ;
215100080604 1       endif  ;
215200080603
215300080603          clear   pulizpot1  ;
215400080603       endif     ;
215500080603
215600080603
215700080603       if pulizpot2='C' ;
215800111116
215900111116         if %equal(cnaco16l)  ;
216000111116           Codif2='S'  ;
216100111116           RICodif2=*on  ;
216200111116         else       ;
216300111116           Codif2='N'  ;
216400111116         endif       ;
216500100525
216600080603       v032fcpo=fcpo  ;
216700080603       v032ncpo=ncpo  ;
216800080603       v032rag=cporag;
216900080603       v032ind=cpovia;
217000080603       v032loc=cpocit;
217100080603       v032cap=cpocap;
217200080603       if cponaz=*blanks  ;
217300080603       %subst(v032loc:27:3)=' '+cpoprv;
217400080603       else  ;
217500080603       %subst(v032loc:26:4)=' '+cponaz;
217600080603       endif ;
217700111115
217800111115       //?Categoria potenziale
217900111116        clear V032fls;
218000111116        clear trmk05ds;
218100111116        IMK05cpo = CPOcpo;
218200111116        trmk05r (kpjba : TRMK05DS);
218300111116        IF  OMK05err = *blanks;
218400111116          V032fls = OMK05cat;
218500111116        ENDIF;
218600111115
218700111115       //?Prossima attività
218800111115         sav_CPOcpo = CPOcpo;
218900111115         exsr RIC_proATT;
219000111115         V032cad = sav_ATCcad;
219100111115
219200111115       //?Viste le nuove modifiche imposto 'F' se cat. eliminabile
219300111116         IF  V032fls = 'E';
219400080604           v032tipf='F'     ;
219500080610           v032dtip='FONDERE'  ;
219600080604           v031tipf='T'     ;
219700080610           v031dtip='TENERE '  ;
219800111115         ENDIF;
219900111115
220000111115       //?Controllo se potenziale mai contattato
220100120914         dCPO01 = CPOrst;
220200120914         IF  §CPOdtpcon <> *blanks and §CPOdtpcon <> *zeros;
220300111115           $Contattato2 = *on;
220400111115         ENDIF;
220500080603
220600080603         v032fil=cpoflt;
220700080603         v032piv=cpopiv;
220800080603         v032cdf=cpocdf;
220900080603 2       if cpofsf='S' ;
221000080603          v032fsf='Sd';
221100080603         ELSE         ;
221200080603          v032fsf='Fi';
221300080603 2       endif;
221400080603
221500080603         v032cmerc=cposct;
221600080603         clear tblkey  ;
221700080603         tblkey=%editc(cposct:'X') ;
221800080603         chain  (1:'1L':tblkey) tabel00f;
221900080603 1       if %found(tabel00f);
222000080603         v032dmerc=tbluni;
222100080904         ds1l=tbluni     ;
222200080904         c032cmuti=§1luti;
222300080603         else ;
222400080603         v032dmerc=*all'?' ;
222500080904         c032cmuti='N'     ;
222600080603 1       endif  ;
222700080603
222800080603         v032cmm=cpocmm;
222900080604         if cpocmm>0   ;
223000130916           chain  (CPOcmm)  AZCMM000;
223100130916 1         if %found(AZCMM01L);
223200130916             v032dcmm=CMMdes;
223300080604           else ;
223400130916             v032dcmm=*all'?' ;
223500080603 1       endif  ;
223600080604 1       endif  ;
223700100525         // Se presenti entrambi i potenziali, uno codificato e uno no,
223800100525         // devo TENERE il codificato
223900111116         if codif1='S' and codif2='N'  ;
224000100525           v032tipf='F'     ;
224100100525           v032dtip='FONDERE'  ;
224200100525           v031tipf='T'     ;
224300100525           v031dtip='TENERE '  ;
224400100525         endif   ;
224500111116         if codif1='N' and codif2='S'  ;
224600100525           v031tipf='F'     ;
224700100525           v031dtip='FONDERE'  ;
224800100525           v032tipf='T'     ;
224900100525           v032dtip='TENERE '  ;
225000100525         endif   ;
225100080603
225200080603          clear   pulizpot2  ;
225300080603       endif     ;
225400080603          ENDSR;
225500080603       //--------------------------------------------------------------
225600080603       //?Interrogazione potenziali per FUSIONE
225700080603       //--------------------------------------------------------------
225800080603           Begsr INTPOTF ;
225900080603
226000100805           clear trmk01ds   ;
226100080610           // se presente uno dei 2 imposto la ragione sociale
226200100805           mk01rag=%subst(v031rag:1:8)   ;
226300080610
226400100520           mk01k11='4'  ;
226500100520           kpjbu=trmK01ds;
226600100520           TRMK01R (kpjba) ;
226700100520           trmk01ds=kpjbu  ;
226800100520           d11cpo=mk01cpo   ;
226900080603
227000080603           // Selezionato potenziale
227100080603 1         if  d11cpo>0;
227200080603             chain(N)  d11cpo  TNCPO000;
227300080603 2            if not %found(tncpo01l) or  fusione(2)>0 or
227400080603               v032ncpo>0  ;
227500080603               errSEDE =*on;
227600080603               errMessage  = *on;
227700080603               errGenerico = *on;
227800080603               errFusione  = *on;
227900080603               V1Dmsg = Msg(19);
228000080603               %subst(v1dmsg:5:11)=%char(cpocpo) ;
228100080603               %subst(v1dmsg:17:12)=cporag ;
228200080603               leavesr ;
228300080603 2            endif  ;
228400080603
228500080603              // Carico dati
228600080603              kcpo=d11cpo   ;
228700080603              pulizpot2='S';
228800080603              exsr ClearCTL3 ;
228900080604              if kcpo<>savcpo;
229000080604               clear   v032tipf   ;
229100080604               clear   v032dtip    ;
229200080604               savcpo=kcpo    ;
229300080604              endif          ;
229400080603              pulizpot2='C';
229500080603              exsr RiempiCTL3 ;
229600080603            endif ;
229700080603
229800080603          ENDSR;
229900080603
230000080603       //--------------------------------------------------------------
230100080603       //?Carico i contatti nel sfl per fusione
230200080603       //--------------------------------------------------------------
230300080603          BEGSR   Carcontatti;
230400080606          s03nrr=1         ;
230500080606          s03tipo='C'      ;
230600080606          clear primariga  ;
230700080609          clear keysfl3    ;
230800080617          clear key3pot1   ;
230900080617          clear key3pot2   ;
231000080609          clear keynrr     ;
231100080609          clear keyrig     ;
231200080609          clear keycont    ;
231300080609          clear XX         ;
231400080604
231500080604          // Leggo tabella contatti e uno per uno vedo se c'e'
231600080604          // scrivo solo se trovato uno dei 2
231700080604          setll (1:'1T') tabel00f  ;
231800080604          reade (1:'1T') tabel00f  ;
231900080604
232000080604  1       dow    not %eof(tabel00f)  ;
232100080604          ds1t=tbluni                ;
232200080604
232300080604  2       if tblflg=' ' and §1trich='C'  ;
232400080604          ktnt=tblkey ;
232500080604
232600080604          // contatto potenziale 1
232700080604          EXSR LeggiNTC1                                  ;
232800080604
232900080604          // contatto potenziale 2
233000080604          EXSR LeggiNTC2                                  ;
233100080604
233200080604          ff=1    ;
233300080604  3       dow ntccod1(ff)<>*blanks or ntccod2(ff)<>*blanks  ;
233400080606
233500080606          // emetto riga descrittiva se caricato almeno un dato
233600080606          if primariga=' '   ;
233700090716          s03dec='-- R U B R I C A --' ;
233800081009          exsr  RigaDESCR  ;
233900080606          Primariga='S' ;
234000080606          endif         ;
234100080604
234200080604          s03dec=ktnt+'-'+§1tdes ;
234300080929          clear s03tpr           ;
234400080929          s03tpr=ktnt            ;
234500080606          s03righe=§1trig          ;
234600080620          clear s031nk2  ;
234700080620          clear s032nk2  ;
234800080604
234900080606          EXSR SCriviSFL3         ;
235000080604
235100080604          ff=ff+1    ;
235200080604  3       enddo     ;
235300080604
235400080604  2       endif               ;
235500080604          reade (1:'1T') tabel00f  ;
235600080604  1       enddo     ;
235700080604
235800080603          ENDSR;
235900080606       //--------------------------------------------------------------
236000080606       //?emetto prima riga descrittiva
236100080606       //--------------------------------------------------------------
236200080606          BEGSR RigaDESCR ;
236300080606          // Riga descrittiva
236400081009          noscelta1=*on   ;
236500081009          noscelta2=*on   ;
236600080606          in52=noscelta1  ;
236700080606          in53=noscelta2  ;
236800080606          Testatahi=*on   ;
236900080606          clear s03righe  ;
237000080606          clear s031sce   ;
237100080606          clear s032sce   ;
237200081010          Infoscelta1=*off;
237300081010          Infoscelta2=*off;
237400081010
237500081010          // Riga per info con campo scelta attivo
237600081010          if IndInfo=*on  ;
237700081010
237800081010           if ImpostaT='1'     ;
237900081010             s031sce='T'    ;
238000081010             SFLRosso1=*on   ;
238100081010           endif           ;
238200081010           if ImpostaT='2'     ;
238300081010             s032sce='T'    ;
238400081010             SFLRosso2=*on   ;
238500081010           endif           ;
238600081010           if w1EsInfo='N' ;
238700081010             s031sce='N'   ;
238800081010             Infoscelta1=*on;
238900081010           endif          ;
239000081010           if w1Esinfo<>'S' and w2EsInfo='S'  ;
239100081010             s031sce='N'   ;
239200081010             Infoscelta1=*on;
239300081010           endif   ;
239400081010           if w2EsInfo='N' ;
239500081010             s032sce='N'   ;
239600081010             Infoscelta2=*on;
239700081010           endif          ;
239800081010           if w2Esinfo<>'S' and w1EsInfo='S'  ;
239900081010             s032sce='N'   ;
240000081010             Infoscelta2=*on;
240100081010           endif   ;
240200081010
240300081010           // Per le infocomm nei campi in52 e in53 ci metto l'altro indicat
240400081010          in52=infoscelta1  ;
240500081010          in53=infoscelta2  ;
240600081010         endif          ;
240700081010
240800080606          clear s031imm   ;
240900081010          clear s032imm   ;
241000081010          if s03nrr=1 ;
241100081010            s031imm='MM/AA' ;
241200081010            s032imm='MM/AA' ;
241300081010          endif   ;
241400080606          clear s031cod   ;
241500080606          clear s032cod   ;
241600080606          clear s031nk2   ;
241700080606          clear s032nk2   ;
241800080610          clear s03forza  ;
241900080606          write   mk07s03     ;
242000080606          Testatahi=*off  ;
242100081010          Infoscelta1=*off;
242200081010          Infoscelta2=*off;
242300081010          ENDSR  ;
242400080604       //--------------------------------------------------------------
242500080604       //?Carico le info commerciali per fusione
242600080604       //--------------------------------------------------------------
242700080604          BEGSR   InfoComm   ;
242800081009          IndInfo=*on      ;
242900081009
243000080606          s03tipo='I'      ;
243100080606          clear primariga  ;
243200081010
243300081010          // Controllo di chi devo tenere le info comm   ;
243400081010          // vince il potenziale da Tenere se:
243500081010          //  - ha dati e sono nuovi
243600081010          //  - ha dati e sono vecchi ed il pot da Fondere ha dati
243700081010          //    vecchi o non ne ha
243800081010          // vince il potenziale da Fondere se:
243900081010          //  - ha dati e sono nuovi ed  il pot da Tenere ha dati
244000081010          //    vecchi o non ne ha
244100081010
244200081010          fcpo=v031fcpo      ;
244300081010          ncpo=v031ncpo      ;
244400081010          w1EsInfo='N'       ;
244500081010          setll     kcpo ticpi01l             ;
244600081010  1       if %equal(ticpi01l)  ;
244700081010           w1EsInfo='O'      ;
244800081010
244900081010          reade     kcpo ticpi01l             ;
245000081010  2       dow not %eof(ticpi01l)  and w1EsInfo<>'S' ;
245100081010  3       if cpitpf<>'10 ' and cpitpf<>'20 ' and cpitpf<>'30 ' and
245200081010             cpitpf<>'40 ' and cpitpf<>'OUT'  ;
245300081010             w1EsInfo='S'      ;
245400081010          else;
245500081010             reade     kcpo ticpi01l             ;
245600081010  3       endif;
245700081010  2       enddo     ;
245800081010  1       endif     ;
245900081010
246000081010          fcpo=v032fcpo      ;
246100081010          ncpo=v032ncpo      ;
246200081010          w2EsInfo='N'       ;
246300081010          setll     kcpo ticpi01l             ;
246400081010  1       if %equal(ticpi01l)  ;
246500081010           w2EsInfo='O'      ;
246600081010
246700081010          reade     kcpo ticpi01l             ;
246800081010  2       dow not %eof(ticpi01l)  and w2EsInfo<>'S' ;
246900081010  3       if cpitpf<>'10 ' and cpitpf<>'20 ' and cpitpf<>'30 ' and
247000081010             cpitpf<>'40 ' and cpitpf<>'OUT'  ;
247100081010             w2EsInfo='S'      ;
247200081010          else;
247300081010             reade     kcpo ticpi01l             ;
247400081010  3       endif;
247500081010  2       enddo     ;
247600081010  1       endif;
247700081010
247800081010          if v031tipf='T'     ;
247900081010          TieniEsInfo=w1EsInfo;
248000081010          FondiesInfo=w2EsInfo;
248100081010          else                ;
248200081010          TieniEsInfo=w2EsInfo;
248300081010          FondiesInfo=w1EsInfo;
248400081010          endif               ;
248500081010
248600081010          clear ImpostaT   ;
248700081010          ImpostaT='T'   ;
248800081010
248900081010          if   (TieniEsInfo='N' and FondiEsinfo<>'N')  or
249000081010               (TieniEsInfo='O' and FondiEsinfo='S')  ;
249100081010            ImpostaT='F'   ;
249200081010          endif            ;
249300081010
249400081010          if    TieniEsInfo='N' and FondiEsinfo='N'   ;
249500081010            ImpostaT='N'   ;
249600081010          else  ;
249700081010              select   ;
249800081010              when ImpostaT='T' and v031tipf='T' ;
249900081010            ImpostaT='1'   ;
250000081010              when ImpostaT='T' and v032tipf='T' ;
250100081010            ImpostaT='2'   ;
250200081010              when ImpostaT='F' and v031tipf='F' ;
250300081010            ImpostaT='1'   ;
250400081010              when ImpostaT='F' and v032tipf='F' ;
250500081010            ImpostaT='2'   ;
250600081010              endsl ;
250700081010          endif            ;
250800080604
250900081010          // se non ho info  --> non carico nulla
251000081010          if ImpostaT<>'N'   ;
251100081010
251200080604          // Leggo tabella DELLE INFO e uno per uno vedo se c'e'
251300080604          // scrivo solo se trovato uno dei 2
251400080929          yy=  1   ;
251500080929          dow   yy<=50   ;
251600080929  1       if    kifoord(yy) <>*blanks   ;
251700080929          ktprI=%subst(kifoord(yy):6:3) ;
251800080929          wdes =%subst(kifoord(yy):9:19) ;
251900080929          wIspp =%subst(kifoord(yy):28:1) ;
252000080929          // chain tabella per la decodifica
252100080604
252200080604          // contatto potenziale 1
252300080604          EXSR LeggiCPI1                                  ;
252400080604
252500080604          // contatto potenziale 2
252600080604          EXSR LeggiCPI2                                  ;
252700080604
252800080604          ff=1    ;
252900080929  3       dow ff<=40 and ntccod1(ff)<>*blanks or ntccod2(ff)<>*blanks  ;
253000080606
253100080606          // Emetto riga descrittiva se caricato almeno un dato
253200080606          if primariga=' '   ;
253300080610          s03dec='-INFO  COMMERCIALI-' ;
253400080606          if s03nrr>1   ;
253500080606          s03nrr=s03nrr+1  ;
253600080606          endif         ;
253700080606
253800080606          exsr  RigaDESCR  ;
253900080606          Primariga='S' ;
254000080606          endif         ;
254100080604
254200080929          clear s03tpr   ;
254300080929          s03tpr=ktprI  ;
254400141002
254500141002       //?Se INFO relativa al NON affidato a BRT metto n.a. davanti
254600141002          IF  S03tpr = 'NRF' or S03tpr = 'NRO' or S03tpr = 'NER' or
254700141002              S03tpr = 'NAE' or S03tpr = 'NTR';
254800141002            S03dec = 'N.A. ' + wdes;
254900141002          ELSE;
255000080929          s03dec=wdes    ;
255100141002          ENDIF;
255200141002
255300080929          if wIspp  =' '     ;
255400080606            s03righe=1               ;
255500080620            clear s031nk2    ;
255600080620            clear s032nk2    ;
255700080606          else   ;
255800080606            s03righe=90              ;
255900081013            s031nk2=sottot1(ff)       ;
256000081013            s032nk2=sottot2(ff)  ;
256100080606          endif  ;
256200080604
256300080606          EXSR  SCriviSFL3   ;
256400080604
256500080604          ff=ff+1    ;
256600080604  3       enddo     ;
256700080929          endif     ;
256800080929
256900080929          yy=yy+1    ;
257000080604  1       enddo     ;
257100081010
257200081010          endif     ;
257300080604
257400081009          IndInfo=*off     ;
257500080604          ENDSR;
257600140923
257700140923       //--------------------------------------------------------------
257800140923       //?Caricamento Dettaglio Prodotti per FUSIONE.                  ?
257900140923       //--------------------------------------------------------------
258000140923       BEGSR  sr_DettProd;
258100140923
258200140923         IndInfo = *on;
258300140923         clear  PrimaRiga;
258400140923         S03tipo = 'P';
258500140923
258600140923         Ktnt = 'DP';
258700140923         clear  Knk2;
258800140923         clear  NTCcod1;
258900140923         clear  NTCcod2;
259000140923         clear  DtImm1;
259100140923         clear  DtImm2;
259200140923
259300140923         chain  ( 1 : '1T' : Ktnt )  TABEL;
259400140923         if  %found(TABEL00F);
259500140923           ds1T = TBLuni;
259600140923         else;
259700140923           clear  ds1T;
259800140923           §1Tdes = *all'? ';
259900140923         endif;
260000140923
260100140923         // -?Reperimento Dettaglio Prodotti del 1° Potenziale.?
260200140923         clear  FF;
260300140923         Knk1 = %editc( V031fcpo : 'X' ) +
260400140923                %editc( V031ncpo : 'X' );
260500140923         setll     ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
260600140923         reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
260700140923
260800140923         DoW  Not  %eof(TFNTC01L);
260900140923
261000140923           FF += 1;
261100140923           DataDMY = %date( NTCntr : *YMD );
261200140923           w004a   = %subst( %editc( %dec( DataDMY ) : 'X' ) : 3 : 4 );
261300140923           DtImm1(FF)  = w004a;
261400140923           NTCcod1(FF) = NTCrnt;
261500140923
261600140923           reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
261700140923
261800140923         EndDo;
261900140923
262000140923         // -?Reperimento Dettaglio Prodotti del 2° Potenziale.?
262100140923         clear  FF;
262200140923         Knk1 = %editc( V032fcpo : 'X' ) +
262300140923                %editc( V032ncpo : 'X' );
262400140923         setll     ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
262500140923         reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
262600140923
262700140923         DoW  Not  %eof(TFNTC01L);
262800140923
262900140923           FF += 1;
263000140923           DataDMY = %date( NTCntr : *YMD );
263100140923           w004a   = %subst( %editc( %dec( DataDMY ) : 'X' ) : 3 : 4 );
263200140923           DtImm2(FF)  = w004a;
263300140923           NTCcod2(FF) = NTCrnt;
263400140923
263500140923           reade(N)  ( 'P' : Knk1 : Knk2 : Ktnt )  TFNTC;
263600140923
263700140923         EndDo;
263800140925
263900140925
264000140925         // -?Controllo di chi devo tenere le info comm?
264100140925         //  ?Vince il potenziale da Tenere se:?
264200140925         //  ?- ha dati?
264300140925         //  ?Vince il potenziale da Fondere se:?
264400140925         //  ?- ha dati ed il potenziale da Tenere NON ha dati?
264500140925         w1EsInfo = 'N';
264600140925         w2EsInfo = 'N';
264700140925         ImpostaT = 'T';
264800140925         Select;
264900140925           When  V031tipf = 'T'  and  NTCcod1(1) <> *blank;
265000140925             ImpostaT = '1';
265100140925             w1EsInfo = 'T';
265200140925           When  V032tipf = 'T'  and  NTCcod2(1) <> *blank;
265300140925             ImpostaT = '2';
265400140925             w2EsInfo = 'T';
265500140925           When  V031tipf = 'T'  and  NTCcod1(1) =  *blank
265600140925                                 and  NTCcod2(1) <> *blank;
265700140925             ImpostaT = '2';
265800140925             w2EsInfo = 'T';
265900140925           When  V032tipf = 'T'  and  NTCcod1(1) <> *blank
266000140925                                 and  NTCcod2(1) =  *blank;
266100140925             ImpostaT = '1';
266200140925             w1EsInfo = 'T';
266300140925         EndSl;
266400140925
266500140923
266600140923         // -?Scrittura record nel subfile?
266700140923         FF = 1;
266800140923         DoW  NTCcod1(FF) <> *blank  or  NTCcod2(FF) <> *blank;
266900140923
267000140923           // -?Emissione della riga descrittiva se caricato almeno un dato?
267100140923           If  PrimaRiga = *blank;
267200140923
267300140925             S03dec    = '-DESCRIZIONE PROD.-';
267400140923             if  S03nrr > 1;
267500140923               S03nrr += 1;
267600140923             endif;
267700140923             exsr  RigaDESCR;
267800140923             Primariga = 'S';
267900140923
268000140923           EndIf;
268100140923
268200140923           // -?Emissione del Dettaglio Prodotti dei 2 potenziali?
268300140923           S03dec = Ktnt + '-' + §1Tdes;
268400140923           S03tpr = Ktnt;
268500140923           S03righe = §1Trig;
268600140923           clear  S031nk2;
268700140923           clear  S032nk2;
268800140923
268900140923           exsr  ScriviSFL3;
269000140923
269100140923           FF += 1;
269200140923
269300140923         EndDo;
269400140923
269500140923         IndInfo = *off;
269600140923
269700140923       ENDSR;
269800140923
269900080606       //--------------------------------------------------------------
270000080606       //?Scrittura righe di dettaglio sfl3
270100080606       //--------------------------------------------------------------
270200080606         BEGSR ScriviSFL3   ;
270300081009
270400081009         // Memorizzo in skiera i codici con più di una riga ;
270500081009 1       if s03righe>1   ;
270600081009
270700081009 2       if   s031nk2<>*blanks   ;
270800081009           w007a=s03tpr +s031nk2 ;
270900081009           indx=%lookup(w007a:keysfl3)  ;
271000081009 3         if indx=0  ;
271100081009            xx=xx+1   ;
271200081009            keysfl3(xx)=w007a ;
271300081009            keyrig (xx)=s03righe   ;
271400081009 3         endif   ;
271500081009
271600081009            // Memorizzo solo una volta il codice
271700081009           indx=%lookup(s03tpr :key3pot1)  ;
271800081009 3         if indx=0   ;
271900081009              key3pot1(xx)=s03tpr              ;
272000081009              keynrr(xx) =s03nrr+1   ;
272100081009              // Se presenti per entrami lo memorizzo subito per
272200081009              //  avere il codice nella stessa posizione di skiera
272300081009              if s032nk2<>*blanks   ;
272400081009               key3pot2(xx)=s03tpr              ;
272500081009              endif                 ;
272600081009 3         endif   ;
272700081009 2       endif   ;
272800081009
272900081009 2       if   s032nk2<>*blanks   ;
273000081009           w007a=s03tpr+s032nk2 ;
273100081009           indx=%lookup(w007a:keysfl3)  ;
273200081009 3         if indx=0   ;
273300081009            xx=xx+1   ;
273400081009            keysfl3(xx)=w007a ;
273500081009            keyrig (xx)=s03righe   ;
273600081009 3         endif   ;
273700081009            // Memorizzo solo una volta il codice
273800081009           indx=%lookup(s03tpr :key3pot2)  ;
273900081009 3         if indx=0   ;
274000081009            key3pot2(XX)=s03tpr              ;
274100081009            keynrr(xx) =s03nrr+1   ;
274200081009 3         endif   ;
274300081009 2       endif   ;
274400081009
274500081009 2       if   s031nk2=*blanks and s032nk2=*blanks  ;
274600081009           w007a=s03tpr+'    '  ;
274700081009           indx=%lookup(w007a:keysfl3)  ;
274800081009 3         if indx=0   ;
274900081009            xx=xx+1   ;
275000081009            keysfl3(xx)=w007a ;
275100081009            keyrig (xx)=s03righe   ;
275200081009            keynrr(xx) =s03nrr+1   ;
275300081009              if ntccod1(ff)<>*blanks    ;
275400081009               key3pot1(xx)=s03tpr                     ;
275500081009              endif  ;
275600081009              if ntccod2(ff)<>*blanks    ;
275700081009               key3pot2(xx)=s03tpr                     ;
275800081009              endif  ;
275900081009 3         endif   ;
276000081009 2         endif   ;
276100081009 1       endif   ;
276200081009
276300081009         clear s031sce ;
276400081009         clear s032sce ;
276500081009
276600081009         if ntccod1(ff)<>*blanks            ;
276700081009         s031cod=ntccod1(ff)                 ;
276800081009         s031imm=%subst(dtimm1(ff):1:2)+'/' +%subst(dtimm1(ff):3:2)  ;
276900081009         in52='0'             ;
277000081009         noscelta1=*off  ;
277100081009         // Se scelto primo pote imposto la "T"
277200081009           if v031tipf='T'   ;
277300081009           s031sce='T'  ;
277400081009           SFLRosso1=*on ;
277500081009           endif    ;
277600081009         else   ;
277700081009         clear s031cod    ;
277800081009         clear s031imm    ;
277900081009         in52='1'             ;
278000081009         noscelta1=*on   ;
278100081009         clear s031nk2   ;
278200081009         endif   ;
278300081009
278400081009         if ntccod2(ff)<>*blanks            ;
278500081009         s032cod=ntccod2(ff)                 ;
278600081009         s032imm=%subst(dtimm2(ff):1:2)+'/' +%subst(dtimm2(ff):3:2)  ;
278700081009         in53='0'             ;
278800081009         noscelta2=*off  ;
278900081009
279000081009         // Se scelto 2° pot. o non presente riga per pot1 imposto la "T"
279100081009           if v032tipf='T' or (IndInfo=*off and s03righe=1 and noscelta1=*on);
279200081009           s032sce='T'  ;
279300081009           SFLRosso2=*on ;
279400081009           endif    ;
279500081009
279600081009           // Se più righe e 1°pot vuoto, vedo se devo impostare la "T"
279700081009           //   sul secondo potenziale
279800081009             if INdInfo=*off and v031tipf='T' and s03righe>1 and noscelta1=*on;
279900081009               Indx=%lookup(s03tpr :key3pot1)         ;
280000081009               if Indx=0   ;
280100081009                s032sce='T'  ;
280200081009                SFLRosso2=*on ;
280300081009               endif       ;
280400081009             endif   ;
280500081009
280600081009         else   ;
280700081009         clear s032cod    ;
280800081009         clear s032imm    ;
280900081009         in53='1'             ;
281000081009         noscelta2=*on   ;
281100081009         clear s032nk2   ;
281200081009
281300081009         // Se scelto 2° pot. e non presente riga per pot2 imposto la "T"
281400081009         //   nel 1° potenziale
281500081009           if IndInfo=*off and v032tipf='T' and s03righe=1 and noscelta1=*off;
281600081009           s031sce='T'  ;
281700081009           SFLRosso1=*on ;
281800081009           endif    ;
281900081009
282000081009           // Se più righe e 2°pot vuoto, vedo se devo impostare la "T"
282100081009           //   sul 1° potenziale
282200081009             if IndINfo=*off and v032tipf='T' and s03righe>1 and noscelta1=*off;
282300081009               Indx=%lookup(s03tpr:key3pot2)         ;
282400081009               if Indx=0   ;
282500081009                s031sce='T'  ;
282600081009                SFLRosso1=*on ;
282700081009               endif       ;
282800081009             endif   ;
282900081009
283000081009         endif   ;
283100081009
283200081009         clear s03forza  ;
283300081009
283400081009         s03nrr=s03nrr+1     ;
283500081009          // posizionamento cursore sulla prima riga
283600140923          if (s03tipo='C' and s03nrr=2) ;
283700081009            if noscelta1=*off    ;
283800081009            errsfl3_1=*on    ;
283900081009            else     ;
284000081009            errsfl3_2=*on    ;
284100081009            endif    ;
284200081009          endif    ;
284300081009
284400081009          // Per info proteggo sempre il campo di scelta
284500081009          if IndInfo          ;
284600081009          clear s031sce ;
284700081009          clear s032sce ;
284800081010          SFLRosso1=*off;
284900081010          SFLRosso2=*off;
285000081010            if ImpostaT='1';
285100081010             SFLRosso1=*on ;
285200081010            endif               ;
285300081010            if ImpostaT='2';
285400081010             SFLRosso2=*on ;
285500081010            endif               ;
285600081010          endif               ;
285700081009
285800081009          write mk07s03       ;
285900081009          errsfl3_1=*off   ;
286000081009          errsfl3_2=*off   ;
286100081009          SFLRosso1=*off   ;
286200081009          SFLRosso2=*off   ;
286300080606          ENDSR               ;
286400080604       //--------------------------------------------------------------
286500080604       //?Leggi contatti per potenziale 1 - Fusione
286600080604       //--------------------------------------------------------------
286700080604          BEGSR  LeggiNTC1                                ;
286800080604          clear ntccod1  ;
286900080605          clear DtImm1   ;
287000080604          clear FF       ;
287100080619          clear knk2     ;
287200080604
287300080604          knk1=%editc(v031fcpo:'X')+%editc(v031ncpo:'X')  ;
287400080619          setll    ('P':knk1:knk2:ktnt) tfntc01l             ;
287500080619          READE(N) ('P':knk1:knk2:ktnt) tfntc01l             ;
287600080604          dow not %eof(tfntc01l)  ;
287700080604          ff=ff+1              ;
287800080605          dataymd=%date(ntcntr:*ymd)    ;
287900080605          datadmy=dataymd ;
288000080605          w0060=%dec(datadmy)  ;
288100080605          w004a     =%subst(%editc(w0060:'X'):3:4) ;
288200080610          dtimm1(ff)=w004a                    ;
288300080605
288400080604          ntccod1(ff)=ntcrnt       ;
288500080619          READE(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
288600080604          enddo                ;
288700080604
288800080604          ENDSR                ;
288900080604       //--------------------------------------------------------------
289000080604       //?Leggi contatti per potenziale 2 - Fusione
289100080604       //--------------------------------------------------------------
289200080604          BEGSR  LeggiNTC2                                ;
289300080604          clear ntccod2  ;
289400080605          clear DtImm2   ;
289500080604          clear FF       ;
289600080604
289700080604          knk1=%editc(v032fcpo:'X')+%editc(v032ncpo:'X')  ;
289800080619          setll    ('P':knk1:knk2:ktnt) tfntc01l             ;
289900080604
290000080619          READE(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
290100080604          dow not %eof(tfntc01l)  ;
290200080604          ff=ff+1              ;
290300080605          dataymd=%date(ntcntr:*ymd)    ;
290400080605          datadmy=dataymd ;
290500080605          w0060=%dec(datadmy)  ;
290600080605          w004a     =%subst(%editc(w0060:'X'):3:4) ;
290700080610          dtimm2(ff)=     w004a                    ;
290800080605
290900080604          ntccod2(ff)=ntcrnt       ;
291000080619          reade(n) ('P':knk1:knk2:ktnt) tfntc01l             ;
291100080604          enddo                ;
291200080604          ENDSR                ;
291300080604       //--------------------------------------------------------------
291400080604       //?Leggo info comm per potenziale 1 - Fusione
291500080604       //--------------------------------------------------------------
291600080604          BEGSR  LeggiCPI1                                ;
291700080604          clear ntccod1  ;
291800081013          clear sottot1  ;
291900080605          clear DtImm1   ;
292000080604          clear FF       ;
292100080604
292200080929          fcpo=v031fcpo      ;
292300080929          ncpo=v031ncpo      ;
292400080929          setll    (kcpo:ktprI) ticpi01l             ;
292500080929          READE(n) (kcpo:ktprI) ticpi01l             ;
292600081009
292700080929  1       dow not %eof(ticpi01l)  ;
292800081009  2       if cpiatb=' '        ;
292900081009
293000081009          EXSR ElaboraCPI      ;
293100081009
293200081009          ff=ff+1              ;
293300081009          dtimm1(ff)= w004a        ;
293400081009          ntccod1(ff)=wcodcpi      ;
293500081013          sottot1(ff)=wspf         ;
293600081009
293700081009  2       endif;
293800080604
293900080929          READE(n) (kcpo:ktprI) ticpi01l             ;
294000081009  1       enddo                ;
294100080604
294200080604          ENDSR                ;
294300080604       //--------------------------------------------------------------
294400080604       //?Leggo info comm per potenziale 2 - Fusione
294500080604       //--------------------------------------------------------------
294600080604          BEGSR  LeggiCPI2                                ;
294700080604          clear ntccod2  ;
294800081013          clear sottot2  ;
294900080605          clear DtImm2   ;
295000080604          clear FF       ;
295100080604
295200080929          fcpo=v032fcpo      ;
295300080929          ncpo=v032ncpo      ;
295400080929          setll    (kcpo:ktprI) ticpi01l             ;
295500080929          READE(n) (kcpo:ktprI) ticpi01l             ;
295600081009  1       dow not %eof(ticpi01l)  ;
295700081009  2       if cpiatb=' '        ;
295800081009
295900081009          EXSR ElaboraCPI      ;
296000081009
296100081009          ff=ff+1              ;
296200081009          dtimm2(ff)= w004a        ;
296300081009          ntccod2(ff)=wcodcpi      ;
296400081013          sottot2(ff)=wspf         ;
296500081009
296600081009  2       endif;
296700080605
296800080929          READE(n) (kcpo:ktprI) ticpi01l             ;
296900081009  1       enddo                ;
297000080604
297100080604          ENDSR                ;
297200081009       //--------------------------------------------------------------
297300081009       //?elaborazione per visualizzare    info comm
297400081009       //--------------------------------------------------------------
297500081009          BEGSR   ElaboraCPI   ;
297600081009
297700081009          dataiso=%date(cpidim)   ;
297800081009          datadmy=dataiso ;
297900081009          w0060=%dec(datadmy)  ;
298000081009          w004a     =%subst(%editc(w0060:'X'):3:4) ;
298100081009
298200081009          clear difo     ;
298300081009          clear wcodcpi  ;
298400081013          wspf=cpispf    ;
298500081009          // Se ha il sottotipo vedo se elencati o a scelta con '?'
298600081009  2       if cpispf<>*blanks   ;
298700081009            tbeke1=cpitpf        ;
298800081009            chain    ('IFO':tbeke1) tntbe01l             ;
298900081009  3         if %found(tntbe01l)   ;
299000081009            difo=tbeuni    ;
299100081009  3         endif          ;
299200081009
299300081009            // Decodifico il sottotipo
299400081009            tbeke2=cpiSPF        ;
299500081009            chain    ('IFS':tbeke1:TBEKE2) tntbe01l             ;
299600081009  3         IF %FOUND(TNTBE01L)  ;
299700081009  4            if §ifoifv='?'       ;
299800081009                 wcodcpi    =TBEUNI   ;
299900081009               else    ;
300000081009                 wcodcpi    =%subst(TBEUNI:1:15)   ;
300100081009  4            endif   ;
300200081009
300300081009  x3        ELSE    ;
300400081009             wcodcpi    =cpispf       ;
300500081009  3         endif   ;
300600081009  2         endif   ;
300700081009
300800081009  1       if cpispf=*blanks or §ifoifv='S'    ;
300900081009
301000081009  2         if cpifsn='N'        ;
301100081009            wcodcpi    =%trim(wcodcpi)+' NO'  ;
301200081009  2         endif               ;
301300081009  2         if cpifsn='S'        ;
301400081009            wcodcpi    =%trim(wcodcpi)+' SI'  ;
301500081009  2         endif               ;
301600081009
301700081009   2        if cpival>0  and cpitva='%  '    ;
301800081009            wcodcpi    =%trim(wcodcpi)+' '+%char(cpival) +'%' ;
301900081009            wcodcpi    =%trim(wcodcpi)  ;
302000081009   2        endif  ;
302100081009   2        if cpival>0  and cpitva='KG '    ;
302200081009            wcodcpi    =%trim(wcodcpi)+' KG'+%char(cpival) ;
302300081009            wcodcpi    =%trim(wcodcpi)  ;
302400081009   2        endif  ;
302500141002
302600141002   2        IF  CPIval > 0 and CPItva = *blanks;
302700141002              wcodcpi = %trim(wcodcpi)+ %char(cpival);
302800141002   2        endif  ;
302900081009
303000081009   2        if cpipft>0          ;
303100081009            wpft=cpipft   ;
303200081009            wcodcpi    =%trim(wcodcpi)+' Fat'+%char(wpft) ;
303300081009            wcodcpi    =%trim(wcodcpi)  ;
303400081009   2        endif;
303500081009
303600081009   2        if cpisna>0 ;
303700081009            wcodcpi    =%trim(wcodcpi)+' Spe'+%char(cpisna) ;
303800081009            wcodcpi    =%trim(wcodcpi)  ;
303900081009   2        endif;
304000081009
304100081009   1      endif;
304200081009          ENDSR;
304300080604       //--------------------------------------------------------------
304400080603       //?controlla SFL3  - Fusione
304500080603       //--------------------------------------------------------------
304600080606       BEGSR   contrS03    ;
304700080604
304800080617       // controllo nel sfl control solo fin quando non ho ancora
304900080609       //  caricato il sfl
305000080609
305100080609 1        if SflDSp_N=*on   ;
305200080609
305300080609       // Il secondo codice potenziale deve essere selezionato per la
305400080604       //  fusione
305500080604  1       if v032fcpo=0    and v032ncpo=0         ;
305600080604           errFusione  = *on;
305700080604           errMessage  = *on;
305800080604           errGenerico = *on;
305900080618           V1Dmsg = Msg(27);
306000080604           leavesr;
306100080604  1       endif    ;
306200080604
306300080604       // Non può essere lo stesso codice
306400080604  1       if v031fcpo=v032fcpo and v031ncpo=v032ncpo  ;
306500080604           errFusione  = *on;
306600080604           errMessage  = *on;
306700080604           errGenerico = *on;
306800080604           V1Dmsg = Msg(20);
306900080604           leavesr;
307000080604  1        endif    ;
307100080604
307200080604       // controllo il secondo codice e carico i dati
307300080604  1    if    fusione(2)=0    ;
307400080604
307500080604         fcpo=v032fcpo      ;
307600080604         ncpo=v032ncpo      ;
307700080604         chain(N)  Kcpo  TNCPO000;
307800080604
307900080604         // Codice potenziale NON trovato
308000080604 2       if  NOT  %found(TNCPO01L);
308100100420          if v032rag<>*blanks  ;
308200100420            pulizpot2='S';
308300100420            exsr ClearCTL3 ;
308400100420          endif   ;
308500100420           errFusione  = *on;
308600080604           errMessage  = *on;
308700080604           errGenerico = *on;
308800080618           V1Dmsg = Msg(27);
308900080604           leavesr;
309000080604  2      endif          ;
309100100420
309200080604         pulizpot2='S';
309300080604         exsr ClearCTL3 ;
309400080604         if kcpo<>savcpo;
309500080604          clear   v032tipf   ;
309600080604          clear   v032dtip    ;
309700080604         savcpo=kcpo    ;
309800080604         endif          ;
309900080604
310000080604         pulizpot2='C';
310100080604         exsr RiempiCTL3 ;
310200080604  1    endif  ;
310300100420
310400080603       // Non posso fondere 2 sedi
310500080606  1       if v031fsf='Sd' and v032fsf='Sd' ;
310600080603           errFusione  = *on;
310700080603           errMessage  = *on;
310800080603           errGenerico = *on;
310900080603           V1Dmsg = Msg(04);
311000080603           leavesr;
311100080604  1       endif    ;
311200080603
311300111115       //?Non posso fondere se 2 potenziali in cat. eliminabile
311400111115  1     IF  V031fls = 'E' and V032fls = 'E';
311500080603           errFusione  = *on;
311600080603           errMessage  = *on;
311700080603           errGenerico = *on;
311800080603           V1Dmsg = Msg(05);
311900080603           leavesr;
312000080604  1       endif    ;
312100080604
312200080604        // Decodifico il flag Tenere/Fondere  ;
312300080604        clear v031dtip   ;
312400080604        clear v032dtip   ;
312500080610
312600080610        // Richiesta la manutenzione
312700100520        clear trmk01ds  ;
312800080610        Indx=1              ;
312900080610          dow indx<=2        ;
313000080610          clear kcpo           ;
313100080610
313200080610          if  Indx=1 and v031tipf='M'   ;
313300080610          fcpo=v031fcpo  ;
313400080610          ncpo=v031ncpo  ;
313500080618          clear v031tipf   ;
313600080610          endif    ;
313700080610          if  Indx=2 and v032tipf='M'   ;
313800080610          fcpo=v032fcpo  ;
313900080610          ncpo=v032ncpo  ;
314000080618          clear v032tipf   ;
314100080610          endif    ;
314200080610
314300080610          if kcpo>0       ;
314400100520            clear trmk01ds  ;
314500100520            mk01k10='R'   ;
314600100520            mk01cpo=kcpo  ;
314700100520
314800100520            kpjbu =  trmk01ds;
314900100520            trmk02r ( kpjba : trmk01ds );
315000080618
315100080618            // se restituisce errore, lo visualizzo
315200100805              if mk01m10<>*blanks   ;
315300080618                if Indx=1   ;
315400080618                errtipoFus1 = *on;
315500080618                else    ;
315600080618                errtipoFus2 = *on;
315700080618                endif   ;
315800080618                errMessage  = *on;
315900080618                errGenerico = *on;
316000080618                V1Dmsg = Msg(26);
316100080618              leavesr;
316200080618              else   ;
316300080618               $InzS03  = *on ;
316400080618              endif  ;
316500080610          endif   ;
316600080610
316700080610          Indx=indx+1 ;
316800080610          enddo   ;
316900080610
317000080610          // Se manutenzione riemetto videata per ricarica i dati
317100080610          if $inzs03=*on  ;
317200080610          leavesr  ;
317300080610          endif    ;
317400080604
317500080604          if v031tipf='T'   ;
317600080610          v031dtip='TENERE '  ;
317700080604          endif   ;
317800080604          if v031tipf='F'   ;
317900080610          v031dtip='FONDERE'  ;
318000080604          endif   ;
318100080604
318200080604          if v032tipf='T'   ;
318300080610          v032dtip='TENERE '  ;
318400080604          endif   ;
318500080604          if v032tipf='F'   ;
318600080610          v032dtip='FONDERE'  ;
318700080604          endif   ;
318800080604
318900080604          if (v031tipf='T' and v032tipf='T') or
319000080604            (v031tipf='F' and v032tipf='F')  or
319100080618             v031tipf=' ';
319200080610           errtipoFus1 = *on;
319300080604           errMessage  = *on;
319400080604           errGenerico = *on;
319500080604           V1Dmsg = Msg(21);
319600080604           leavesr;
319700080604          endif ;
319800080618           if  v032tipf=' ';
319900080618           errtipoFus2 = *on;
320000080618           errMessage  = *on;
320100080618           errGenerico = *on;
320200080618           V1Dmsg = Msg(21);
320300080618           leavesr;
320400080618          endif ;
320500080610
320600111115       //?Non posso tenere un potenziale in cat. eliminabile
320700111115        if v031tipf='T' and v031fls = 'E' ;
320800080610           errtipoFus1 = *on;
320900080610           errMessage  = *on;
321000080610           errGenerico = *on;
321100080610           V1Dmsg = Msg(25);
321200080610           leavesr;
321300080610        endif   ;
321400111115        if v032tipf='T' and v032fls = 'E' ;
321500080610           errtipoFus2 = *on;
321600080610           errMessage  = *on;
321700080610           errGenerico = *on;
321800080610           V1Dmsg = Msg(25);
321900080610           leavesr;
322000080610        endif   ;
322100100525
322200100525        //  se un potenziale codificato e uno no, devo TENERE per forza
322300100525        //     quello codificato
322400111116        if codif1='S' and codif2='N'  and v031tipf='F' ;
322500100525           errtipoFus1 = *on;
322600100525           errMessage  = *on;
322700100525           errGenerico = *on;
322800100525           V1Dmsg = Msg(43);
322900100525           leavesr;
323000100525        endif   ;
323100111116        if codif1='N' and codif2='S'  and v032tipf='F' ;
323200100525           errtipoFus1 = *on;
323300100525           errMessage  = *on;
323400100525           errGenerico = *on;
323500100525           V1Dmsg = Msg(43);
323600100525           leavesr;
323700100525        endif   ;
323800080619
323900080619        // Uno dei 2 potenziali deve essere in gestione all'utente
324000080619        Indx =%lookup(v031fil:pog)   ;
324100080619        Indx2=%lookup(v032fil:pog)   ;
324200080619         if Indx=0 and Indx2=0  ;
324300080619           errtipoFus1 = *on;
324400080619           errMessage  = *on;
324500080619           errGenerico = *on;
324600080619           V1Dmsg = Msg(29);
324700080619           leavesr;
324800080619          endif       ;
324900080619
325000111115       // Se quello che fondo non è mio
325100111115       //?il potenziale deve essere un mai contattato
325200111115       // Senza anagrafiche  SEMPRE
325300111115        if v031tipf='F' and Indx=0 and $Contattato1;
325400081010             fcpo=v031fcpo    ;
325500081010             ncpo=v031ncpo    ;
325600081010             setll kcpo  cnaco16l  ;
325700081010             setll kcpo  tfaco16l  ;
325800081010             if %equal(cnaco16l) or %equal(tfaco16l) ;
325900081010             errtipoFus1 = *on;
326000081010             errMessage  = *on;
326100081010             errGenerico = *on;
326200100524             V1Dmsg = Msg(42);
326300081010             leavesr;
326400081010          endif    ;
326500080619        endif       ;
326600081010
326700111115        if v032tipf='F' and Indx2=0 and $Contattato2;
326800081010             fcpo=v032fcpo    ;
326900081010             ncpo=v032ncpo    ;
327000081010             setll kcpo  cnaco16l  ;
327100081010             setll kcpo  tfaco16l  ;
327200081010             if %equal(cnaco16l) or %equal(tfaco16l) ;
327300081010             errtipoFus2 = *on;
327400081010             errMessage  = *on;
327500081010             errGenerico = *on;
327600100524             V1Dmsg = Msg(42);
327700081010             leavesr;
327800081010           endif       ;
327900081010        endif       ;
328000080904
328100081010        //  Se lo posso manutenzionare, il potenziale da Tenere non deve avere
328200081010        //  un settore merceologico NON CLASSIFICATO
328300080904        if v031tipf='T'  and Indx>0  and c031cmuti='N'   ;
328400080904           errtipoFus1 = *on;
328500080904           errMessage  = *on;
328600080904           errGenerico = *on;
328700080904           V1Dmsg = Msg(35);
328800080904           leavesr;
328900080904        endif       ;
329000080904        if v032tipf='T'  and Indx2>0  and c032cmuti='N'   ;
329100080904           errtipoFus2 = *on;
329200080904           errMessage  = *on;
329300080904           errGenerico = *on;
329400080904           V1Dmsg = Msg(35);
329500080904           leavesr;
329600080904        endif       ;
329700080619
329800080619       // Memorizzo il codice da tenere ;
329900080619       if v031tipf='T'  ;
330000080619        fcpo=v031fcpo    ;
330100080619        ncpo=v031ncpo    ;
330200080619       else   ;
330300080619        fcpo=v032fcpo    ;
330400080619        ncpo=v032ncpo    ;
330500080619       endif   ;
330600080619
330700080619        Tienicpo=kcpo  ;
330800080619
330900080619       // Memorizzo il codice da fondere ;
331000080619       if v031tipf='T'  ;
331100080619        fcpo=v032fcpo    ;
331200080619        ncpo=v032ncpo    ;
331300080619        Fondirag=v032rag  ;
331400080619       else   ;
331500080619        fcpo=v031fcpo    ;
331600080619        ncpo=v031ncpo    ;
331700080619        Fondirag=v031rag  ;
331800080619       endif   ;
331900080619
332000080619        Fondicpo=kcpo  ;
332100100521
332200100525       // Il potenziale da Fondere non deve avere una trattativa aperta non fittizia
332300100521
332400100521           clear tnta43ds  ;
332500100525           ita43cpo=fondicpo   ;
332600100521
332700100521           TNTA43R (TNTA43DS)  ;
332800100521
332900100521           if ota43nrv>0 or ota43err<>' '   ;
333000100521           if v031tipf='F'  ;
333100100521            errtipoFus1 = *on;
333200100521           ELSE   ;
333300100521            errtipoFus2 = *on;
333400100521           ENDIF   ;
333500100521           errMessage  = *on;
333600100521           errGenerico = *on;
333700100521           V1Dmsg = Msg(39);
333800100521
333900100521           leavesr;
334000100521           endif   ;
334100080624
334200080624        // Se quello che fondo ha anagrafiche o visite, avviso
334300100521  2     if Fondicpo<>forzacpo;
334400080624        forzacpo=fondicpo   ;
334500100521        clear  v1dmsg   ;
334600080624        setll fondicpo  cnaco16l  ;
334700100521
334800100805        clear tnta43ds  ;
334900100805        ita43cpo=Fondicpo   ;
335000100521
335100100805        TNTA43R (TNTA43DS)  ;
335200100805   3    if %equal(cnaco16l) or ota43nrv>0       ;
335300100805   4    select   ;
335400100521        when %equal(cnaco16l) and ota43nrv>0       ;
335500100521           V1Dmsg = Msg(41);
335600100521        when %equal(cnaco16l) ;
335700100521           V1Dmsg = Msg(32);
335800100521        when ota43nrv>0       ;
335900100521           V1Dmsg = Msg(40);
336000100805   4    endsl                 ;
336100100805   3    endif  ;
336200100521
336300100521
336400100521   3   if v1Dmsg<>*blanks    ;
336500100521   4   if v031tipf='F'  ;
336600080624           errtipoFus1 = *on;
336700080624       else  ;
336800080624           errtipoFus2 = *on;
336900100521   4   endif ;
337000080624           errMessage  = *on;
337100080624           errGenerico = *on;
337200080624           leavesr;
337300100521   3    endif   ;
337400100521
337500100521   2    endif   ;
337600100521
337700080604
337800080619        // Alloco i 2 record potenziali--> nessuno li deve aggiornare
337900120622       clear dCPO01;
338000080624       chain(n)  Fondicpo  TNCPO000;
338100080619       chain(e)  cp1nrr    TNCPO00F;
338200080619 2     if not %found(tncpo00F) or %error;
338300080619         if v032tipf='F'   ;
338400080619         errFusione = *on;
338500080619         else   ;
338600080619         errtipoFus1 = *on;
338700080619         endif  ;
338800080619
338900080619         errMessage  = *on;
339000080619         errGenerico = *on;
339100080619         if %error    ;
339200080619         V1Dmsg = Msg(30);
339300080619         else   ;
339400080619         V1Dmsg = Msg(31);
339500080619         endif   ;
339600080619       leavesr  ;
339700080619       endif    ;
339800120622
339900120622       //?Salvo la data primo contatto del Potenziale
340000120622       dCPO01 = CPOrst;
340100120702       IF  §CPOdtpcon > *zeros;
340200120622         DataPrimoConF = %dec(§CPOdtpcon:8:0);
340300120622       ELSE;
340400120622         clear DataPrimoConF;
340500120622       ENDIF;
340600080619
340700120622       clear dCPO01;
340800080619       chain(e)  Tienicpo  TNCPO000;
340900080619 2     if not %found(tncpo01l) or %error;
341000080619         if v032tipf='F'   ;
341100080619         errFusione = *on;
341200080619         else   ;
341300080619         errtipoFus1 = *on;
341400080619         endif  ;
341500080619
341600080619         errMessage  = *on;
341700080619         errGenerico = *on;
341800080619         if %error    ;
341900080619         V1Dmsg = Msg(30);
342000080619         else   ;
342100080619         V1Dmsg = Msg(31);
342200080619         endif   ;
342300080619       leavesr  ;
342400080619       endif   ;
342500120622
342600120622       //?Salvo la data primo contatto del Potenziale
342700120622       dCPO01 = CPOrst;
342800120702       IF  §CPOdtpcon > *zeros;
342900120622         DataPrimoConT = %dec(§CPOdtpcon:8:0);
343000120622       ELSE;
343100120622         clear DataPrimoConT;
343200120622       ENDIF;
343300080604
343400080619        // Se non ci sono errori carico i dati nel sfl
343500080619
343600080610        // C O N T A T T I
343700080604          exsr Carcontatti  ;
343800080610        // INFO COMM
343900080604          exsr Infocomm     ;
344000140923        // -?Dettaglio Prodotti?
344100140923          exsr  sr_DettProd;
344200140923
344300080610       // se non ho caricato nessuna informazione, emetto riga descrittiva
344400080610          if s03nrr=1     ;
344500081114          clear s03tipo   ;
344600080610          s03dec='Dati  non  presenti' ;
344700081009          exsr  RigaDESCR  ;
344800080610          endif   ;
344900080610
345000081009          SflDsp_N    = *off;
345100080606
345200080606 2        if v031tipf='T'   ;
345300080606          ColorRosso1=*on           ;
345400080606          else      ;
345500080606          ColorRosso2=*on           ;
345600080606 2        endif     ;
345700080609
345800080606 x2       else      ;
345900080606
346000080606        // Controlla scelte SFL   ;
346100080606         Exsr Contrsfl3           ;
346200080606 1       endif    ;
346300080604
346400080606       ENDSR;
346500100521       //--------------------------------------------------------------
346600100521       //?controlla scelte SFL 3
346700100521       //--------------------------------------------------------------
346800100521       BEGSR contrSFL3  ;
346900080606       clear ricint  ;
347000081013       clear wscelta ;
347100081010       s03nrr=1 ;
347200080609       clear keycont  ;
347300080606
347400081013       // Prima controllo la V= Visualizzazione estesa
347500080606        chain  s03nrr mk07s03  ;
347600080606 1      dow   %found            ;
347700080606
347800080606        clear updatesfl  ;
347900080606 2      if s031sce='V'        ;
348000080606
348100080606        // Visulizza info commerciali    ;
348200080609 3       if s03tipo='I'       ;
348300080929         CLEAR     TRMK50DS   ;
348400080929         fcpo=v031fcpo      ;
348500080929         ncpo=v031ncpo      ;
348600080929         i50cpo=kcpo ;
348700080929         i50mod='I'   ;
348800080929         i50rag=v031rag   ;
348900080929         i50pgm='TRMK07R' ;
349000080606
349100080929         TRMK50R  (kpjba:trmk50ds) ;
349200080609 3       endif    ;
349300080606
349400080606        // Visulizza contatti            ;
349500080609 3       if s03tipo='C'       ;
349600080606         clear tnta12ds   ;
349700080606         ta12ric='V'   ;
349800080606         ta12apl='P'   ;
349900080606         fcpo=v031fcpo  ;
350000080606         ncpo=v031ncpo  ;
350100080606         ta12pot=kcpo ;
350200080606         ta12rag=v031rag  ;
350300080606         TNTA12R  (kpjba:TNTA12ds);
350400080609 3       endif    ;
350500140923
350600140923        // -?Visualizzazione Dettaglio Prodotti?
350700140923         if  S03tipo = 'P';
350800140923           errMessage  = *on;
350900140923           errGenerico = *on;
351000140923           V1Dmsg = Msg(37);
351100140923           exsr UpdateSFL3;
351200140923           leavesr;
351300140923         endif;
351400080606
351500080606         clear    s031sce  ;
351600080606         Ricint='S'   ;
351700080606         Updatesfl='S'   ;
351800080606 2      endif             ;
351900080606
352000080606
352100080606 2      if s032sce='V' ;
352200080606
352300080606        // Visulizza info commerciali    ;
352400080609 3       if s03tipo='I'       ;
352500080929         CLEAR     TRMK50DS   ;
352600080929         fcpo=v032fcpo      ;
352700080929         ncpo=v032ncpo      ;
352800080929         i50cpo=kcpo ;
352900080929         i50mod='I'   ;
353000080929         i50rag=v031rag   ;
353100080929         i50pgm='TRMK07R' ;
353200080606
353300080929         TRMK50R  (kpjba:trmk50ds) ;
353400080609 3       endif   ;
353500080606
353600080606        // Visulizza contatti            ;
353700080609 3       if s03tipo='C'       ;
353800080606         clear tnta12ds   ;
353900080606         ta12ric='V'   ;
354000080606         ta12apl='P'   ;
354100080606         fcpo=v032fcpo  ;
354200080606         ncpo=v032ncpo  ;
354300080606         ta12pot=kcpo ;
354400080606         ta12rag=v032rag  ;
354500080606         TNTA12R  (kpjba:TNTA12ds);
354600080609 3       endif    ;
354700140923
354800140923        // -?Visualizzazione Dettaglio Prodotti?
354900140923         if  S03tipo = 'P';
355000140923           errMessage  = *on;
355100140923           errGenerico = *on;
355200140923           V1Dmsg = Msg(37);
355300140923           exsr UpdateSFL3;
355400140923           leavesr;
355500140923         endif;
355600080606
355700080606         clear    s032sce  ;
355800080606         Ricint='S'   ;
355900080929         Updatesfl='S'   ;
356000080606 2      endif             ;
356100080606
356200080606 2      if Updatesfl='S'             ;
356300080609        exsr UPDATESFL3  ;
356400080606 2      endif           ;
356500080606
356600080606        s03nrr=s03nrr+1 ;
356700080606        chain  s03nrr mk07s03  ;
356800080606 1      enddo    ;
356900080606
357000080606        // SE non ho richiesto la visulizzazione estesa
357100080606        //  eseguo i controlli di scelta
357200080606 1      if ricint=' '  ;
357300081010        s03nrr=1 ;
357400080606
357500080606        chain  s03nrr mk07s03  ;
357600080606 2      dow   %found            ;
357700080609
357800140923        // -?C O N T A T T I?
357900140923 2A     IF  S03TIPO = 'C';
358000081010
358100081010        // Se più di una riga ammessa             ;
358200080609 3      if s03righe>1   ;
358300080609
358400080609        // con sottotipo
358500080609 4      if s031nk2<>*blanks and s031sce='T';
358600080929        w007a=s03tpr+s031nk2 ;
358700080929        xx  =%lookup(w007a:keysfl3) ;
358800080609 5      if keycont(xx)=0;
358900080609           keycont(xx)=90 ;
359000080609 x5     else    ;
359100080609          errMessage  = *on;
359200080609          errGenerico = *on;
359300080609          V1Dmsg = Msg(24);
359400080609          %subst(v1dmsg:44:4)=%subst(s031cod:1:4)  ;
359500080609          %subst(v1dmsg:54:24)=s03dec               ;
359600080609 5      endif   ;
359700080609 4      endif   ;
359800080609 4      if s032nk2<>*blanks and s032sce='T';
359900080929        w007a=s03tpr+s032nk2 ;
360000080929        xx  =%lookup(w007a:keysfl3) ;
360100080609 5      if keycont(xx)=0;
360200080609           keycont(xx)=90 ;
360300080609 x5     else    ;
360400080609          errMessage  = *on;
360500080609          errGenerico = *on;
360600080609          V1Dmsg = Msg(24);
360700080609          %subst(v1dmsg:44:4)=%subst(s032cod:1:4)  ;
360800080609          %subst(v1dmsg:54:24)=s03dec               ;
360900080609 5      endif   ;
361000080609 4      endif   ;
361100080609
361200080617 4      if s031nk2= *blanks and s032nk2=*blanks ;
361300080929        w007a=s03tpr+'    '  ;
361400080929        xx  =%lookup(w007a:keysfl3) ;
361500080609 5      if xx>0    ;
361600080609
361700080609 6        if   s031sce='T'   ;
361800080609          keycont(xx)=keycont(xx)+1;
361900080609 6        endif   ;
362000080609
362100080609 6        if   s032sce='T'   ;
362200080609          keycont(xx)=keycont(xx)+1;
362300080609 6        endif                    ;
362400080609 5      endif     ;
362500080609
362600080609 5      if keycont(xx) > keyrig(xx)  ;
362700080609        errMessage  = *on;
362800080609        errGenerico = *on;
362900080609        V1Dmsg = Msg(22);
363000080609        %subst(v1dmsg:32:2)=%editc(s03righe:'X') ;
363100080609        %subst(v1dmsg:35:5)='righe'              ;
363200140923 6      select;
363300140923 6        when  S03tipo = 'C';
363400140923            %subst(v1dmsg:45:19)='la RUBRICA         ' ;
363500140923 6        when  S03tipo = 'I';
363600140923            %subst(v1dmsg:45:19)='la Info Commerciale' ;
363700140923 6        when  S03tipo = 'P';
363800140923            %subst(v1dmsg:45:19)='il Dettaglio Prod. ' ;
363900140923 6      endsl    ;
364000080609        v1dmsg =%trim(v1dmsg)+' ' +s03dec    ;
364100080609 5      endif     ;
364200080617
364300080617 x4     else      ;
364400080929           xx  =%lookup(s03tpr  :key3pot1) ;
364500080617            if xx=0    ;
364600080929             xx  =%lookup(s03tpr:key3pot2) ;
364700080617            endif      ;
364800080617
364900080617 5      if xx>0    ;
365000080617 6        if   s031sce='T'   ;
365100080617          keycont(xx)=keycont(xx)+1;
365200080617 6        endif   ;
365300080617
365400080617 6        if   s032sce='T'   ;
365500080617          keycont(xx)=keycont(xx)+1;
365600080617 6        endif                    ;
365700080617 5      endif                    ;
365800080617
365900080609 4      endif     ;
366000080609 3      endif     ;
366100080609
366200080609 3      if (s03righe=1 and s031sce='T' and s032sce='T' );
366300080609        errMessage  = *on;
366400080609        errGenerico = *on;
366500080609        V1Dmsg = Msg(22);
366600080609        %subst(v1dmsg:32:2)=%editc(s03righe:'X') ;
366700080609        %subst(v1dmsg:35:5)='riga '              ;
366800140923        select;
366900140923 4        when  S03tipo = 'C';
367000140923            %subst(v1dmsg:45:19)='la RUBRICA         ' ;
367100140923 4        when  S03tipo = 'I';
367200140923            %subst(v1dmsg:45:19)='la Info Commerciale' ;
367300140923 4        when  S03tipo = 'P';
367400140923            %subst(v1dmsg:45:19)='il Dettaglio Prod. ' ;
367500140923 4      endsl    ;
367600080609        v1dmsg =%trim(v1dmsg)+' '+s03dec    ;
367700080609 3      endif    ;
367800080609
367900080609 3      if s03righe=1 and s031sce=' ' and s032sce=' ' and s03forza=' ';
368000080609        errMessage  = *on;
368100080609        errGenerico = *on;
368200080609        V1Dmsg = Msg(23);
368300140923        select;
368400140923 4        when  S03tipo = 'C';
368500140923            %subst(v1dmsg:34:11)='la Rubrica '  ;
368600140923 4        when  S03tipo = 'I';
368700140923            %subst(v1dmsg:34:11)='la InfoComm' ;
368800140923 4        when  S03tipo = 'P';
368900140923            %subst(V1Dmsg:34:11)='il Dett.Pr.' ;
369000140923 4      endsl;
369100080609        %subst(v1dmsg:46:19)=s03dec   ;
369200080609 3      endif    ;
369300081013 2A     endif           ;
369400081013
369500140923        // -?I N F O   C O M M   e   D E T T A G L I O   P R O D .?
369600081013        //   Visualizzo tutto in giallo a seconda di dove è la T
369700140923 2A     IF  S03TIPO = 'I'  or  S03tipo = 'P';
369800081013        indInfo=*on      ;
369900081013
370000081013        // Riga di testa: controllo se c'e' la scelta
370100081013 3      if s031cod=*blanks and s032cod=*blanks;
370200081013
370300081013        // Se entrambe vuote o entrambe piene errore
370400081013 4         if (s031sce<>'T'  and s032sce<>'T') or
370500081013 4            (s031sce= 'T'  and s032sce= 'T') ;
370600081013            errMessage  = *on;
370700081013            errGenerico = *on;
370800081013            V1Dmsg = Msg(36);
370900081013 x4        else;
371000081013             // Memorizzo cosa scelto per evidenziarlo in giallo
371100081013 5           if s031sce='T' ;
371200081013              wscelta='1'    ;
371300081013             else;
371400081013              wscelta='2'    ;
371500081013 5           endif;
371600081013 4         endif    ;
371700081013 3         endif    ;
371800081013 2a        endif    ;
371900080609
372000080609        // Aggiorno SFL
372100080609        exsr UpdateSFL3  ;
372200080609
372300080609 3      if ErrGenerico=*on            ;
372400080609         leavesr;
372500080609 3      endif           ;
372600080609
372700080606        s03nrr=s03nrr+1 ;
372800080606        chain  s03nrr mk07s03  ;
372900080606 2      enddo    ;
373000080609
373100140923        // -?PER CONTATTI: controllo eventuali righe >1 senza scelta?
373200080609        xx=1   ;
373300081013 2      dow keysfl3(xx)<>*blanks  ;
373400080609
373500080609 3      if keynrr(XX) >0    ;
373600080609        s03nrr=keynrr(xx)  ;
373700080609        chain s03nrr   mk07s03   ;
373800080609
373900140923 4      if s03tipo='C' and  s03forza=' ' and keycont(xx)=0  ;
374000080609          errMessage  = *on;
374100080609          errGenerico = *on;
374200080609          V1Dmsg = Msg(23);
374300140923          select;
374400140923 5          when  S03tipo = 'C';
374500140923              %subst(v1dmsg:34:11)='la Rubrica ';
374600140923 5          when  S03tipo = 'I';
374700140923              %subst(v1dmsg:34:11)='la InfoComm';
374800140923 5          when  S03tipo = 'P';
374900140923              %subst(V1Dmsg:34:11)='il Dett.Pr.';
375000140923 5        endsl;
375100080609          %subst(v1dmsg:46:19)=s03dec   ;
375200080609
375300080609        // Aggiorno SFL
375400080609        exsr UpdateSFL3  ;
375500080609
375600080609 5      if ErrGenerico=*on            ;
375700080609         leavesr;
375800080609 5      endif           ;
375900080609 4      endif           ;
376000080609 3      endif           ;
376100080609
376200080609        xx=xx+1   ;
376300081013 2      enddo    ;
376400080609 1      endif    ;
376500080606
376600080606        ENDSR;
376700080609       //--------------------------------------------------------------
376800080609       //?Aggiorno SFL3
376900081013       //--------------------------------------------------------------
377000080609        BEGSR   UpdateSFL3  ;
377100080609
377200140923        //if s03tipo='I' and s031cod=*blanks and s032cod=*blanks ;
377300140923        if  (S03tipo = 'I'      or   S03tipo = 'P')  and
377400140923             S031cod = *blanks  and  S032cod = *blanks ;
377500081013          Testatahi=*on   ;
377600081013          Infoscelta1=in52  ;
377700081013          Infoscelta2=in53  ;
377800081013        else     ;
377900081013          noscelta1=in52  ;
378000081013          noscelta2=in53  ;
378100081013        endif    ;
378200081013
378300140923        if s03tipo='C' and noscelta1=*on and noscelta2=*on  ;
378400081013          Testatahi=*on   ;
378500081013        endif    ;
378600081013
378700081013        if (s03tipo='C' and s031sce='T')  or
378800140923           (S03tipo='P' and wScelta='1')  or
378900081013           (s03tipo='I' and wscelta='1')   ;
379000140923          sflrosso1=*on   ;
379100140923          clear s03forza  ;
379200081013          if Testatahi  =*off  ;
379300081013            s031sce='T'     ;
379400081013          endif             ;
379500080609        else  ;
379600140923          sflrosso1=*off  ;
379700081013          if Testatahi  =*off  ;
379800081013            clear s031sce   ;
379900081013          endif             ;
380000080609        endif ;
380100080609
380200081013        if (s03tipo='C' and s032sce='T')  or
380300140923           (S03tipo='P' and wScelta='2')  or
380400081013           (s03tipo='I' and wscelta='2')   ;
380500140923          sflrosso2=*on   ;
380600140923          clear s03forza  ;
380700081013          if Testatahi  =*off  ;
380800081013            s032sce='T'     ;
380900081013          endif             ;
381000080609        else  ;
381100140923          sflrosso2=*off  ;
381200081013          if Testatahi  =*off  ;
381300081013            clear s032sce   ;
381400081013          endif             ;
381500080609        endif ;
381600080609
381700081013
381800080609        // solo se c'e' errore
381900080609          if ErrGenerico=*on   ;
382000080609           c03rcd=s03nrr  ;
382100080609
382200080609            if noscelta1=*off  ;
382300080609            errsfl3_1=*on     ;
382400080609            else    ;
382500080609            errsfl3_2=*on     ;
382600080609            endif             ;
382700080609          endif             ;
382800080609
382900080609        update mk07s03  ;
383000080609
383100080609        Testatahi=*off  ;
383200081013        IndInfo  =*off  ;
383300080609        errSFL3_1= *off;
383400080609        errSFL3_2= *off;
383500080609        sflrosso1= *off;
383600080609        sflrosso2= *off;
383700081013        noscelta1= *off;
383800081013        noscelta2= *off;
383900081013        Infoscelta1= *off;
384000081013        Infoscelta2= *off;
384100080619       ENDSR  ;
384200080619       //--------------------------------------------------------------
384300080619       //?Conferma FUSIONE
384400080619       //--------------------------------------------------------------
384500080619       BEGSR ConfFUSIONE  ;
384600080619       clear Wscrivi   ;
384700100525       clear TieniconTrCd  ;
384800100525       clear FondiAttB     ;
384900100525       clear FondiAttI     ;
385000100525       clear TieniAtt      ;
385100100525
385200100525       // Controllo se potenziale  che  TENGO ha una trattativa
385300100525           clear tnta43ds  ;
385400100525           ita43cpo=Tienicpo   ;
385500100525
385600100525           TNTA43R (TNTA43DS)  ;
385700100525           if ota43nrv>0 or ota43err<>' '   ;
385800100525             TieniconTrCD='S'   ;
385900100525
386000100525           else   ;
386100100525        setll   TieniCPO  cnaco16l;
386200100525         if %equal(cnaco16l)  ;
386300100525           TieniconTrCd='S'    ;
386400100525         endif  ;
386500100525         endif  ;
386600080619
386700080619        clear knk1  ;
386800080619        knk1 =%editc(TieniCPO:'X') ;
386900080619
387000140923        // -?Lettura SFL per aggiornare CONTATTI, INFO e DETT.PROD.?
387100080619
387200080619        // 1° giro: deleto i record del pot che tengo, se manca la T
387300081013        s03nrr=1 ;
387400080619        chain  s03nrr mk07s03  ;
387500080619 1      dow   %found            ;
387600081013
387700081013        // Escludo i record di testata
387800081013 1a     if s031cod<>*blanks or s032cod<>*blanks  ;
387900080619
388000080620 2      if (v031tipf='T' and s031sce<>'T'and in52='0')  or
388100080620           (v032tipf='T' and s032sce<>'T'and in53='0')   ;
388200080619
388300080929         ktnt=%subst(s03tpr:1:2)  ;
388400080929         ktprI=%subst(s03tpr:1:3)  ;
388500080619         clear knk2  ;
388600080619 3       if v031tipf='T' and s031nk2<>*blanks    ;
388700080619         knk2=s031nk2  ;
388800080619 3       endif   ;
388900080619 3       if v032tipf='T' and s032nk2<>*blanks    ;
389000080619         knk2=s032nk2  ;
389100080619 3       endif   ;
389200080619
389300080619        // 1 riga
389400080619 3        if s03righe=1 or (s03tipo='I' and knk2<>*blanks) ;
389500140923 4         if  S03tipo = 'C'  or  S03tipo = 'P';
389600080619             delete ('P':knk1:knk2:ktnt) TFNTC01l    ;
389700080619             else   ;
389800080929             delete (TIENIcpo:ktprI:knk2) TiCPI01l    ;
389900080619 4           endif  ;
390000080619 x3       else    ;
390100080619
390200140923 4        if  S03tipo = 'C'  or  S03tipo = 'P';
390300080619        // loop per + righe e cerco quello uguale  SOLO PER CONTATTI
390400140923        //    e per DETTAGLIO PRODOTTI (anche se - PER ORA - è
390500140923        //    previsto un solo record)
390600080619           setll  ('P':knk1:knk2:ktnt) TFNTC01l    ;
390700080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
390800080619 5         dow not %eof (tfntc01l)    ;
390900080619 6          if (v031tipf='T' and %subst(ntcrnt:1:21) =s031cod )  or
391000080619               (v032tipf='T' and %subst(ntcrnt:1:21) =s032cod )  ;
391100080619               delete tfntc   ;
391200080619 6          endif    ;
391300080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
391400080619 5         enddo   ;
391500080619 4         endif  ;
391600080619
391700080619 3      endif   ;
391800080619 2      endif   ;
391900081013 1a     endif   ;
392000080619
392100080619        s03nrr=s03nrr+1  ;
392200080619        chain  s03nrr mk07s03  ;
392300080619        enddo  ;
392400080619
392500080619        // 2° giro: Aggiungo quello dell'altro potenziale
392600081013        s03nrr=1 ;
392700080619        chain  s03nrr mk07s03  ;
392800080619 1      dow   %found            ;
392900080619
393000081013
393100081013        // Escludo i record di testata
393200081013 1a     if s031cod<>*blanks or s032cod<>*blanks  ;
393300080620 2      if (v031tipf='T' and s032sce='T'and in53='0')  or
393400080620           (v032tipf='T' and s031sce='T'and in52='0')   ;
393500080619
393600080929         ktnt=%subst(s03tpr:1:2)  ;
393700080929         ktprI=%subst(s03tpr:1:3)  ;
393800080619         clear knk2  ;
393900080619 3       if v031tipf='T' and s032nk2<>*blanks    ;
394000080619         knk2=s032nk2  ;
394100080619 3       endif   ;
394200080619 3       if v032tipf='T' and s031nk2<>*blanks    ;
394300080619         knk2=s031nk2  ;
394400080619 3       endif   ;
394500080619
394600080619        // 1 riga
394700080619 3        if s03righe=1 or (s03tipo='I' and knk2<>*blanks) ;
394800140923 4          if  S03tipo = 'C'  or  S03tipo = 'P';
394900080619              knk1 =%editc(FondiCPO:'X') ;
395000080619              chain  ('P':knk1:knk2:ktnt) TFNTC01l    ;
395100080619              ntcnk1=%editc(Tienicpo:'X')   ;
395200080619              update tfntc ;
395300080619 x4          else    ;
395400080929              chain  (FondiCPO:ktprI:knk2) TiCPI01l    ;
395500080929              cpicpo=Tienicpo        ;
395600080929              update ticpi000 ;
395700080619 4           endif   ;
395800080619 x3       else    ;
395900080619
396000140923 4        if  S03tipo = 'C'  or  S03tipo = 'P';
396100080619        // loop per + righe e cerco quello uguale  SOLO PER CONTATTI
396200140923        //    e per DETTAGLIO PRODOTTI (anche se - PER ORA - è
396300140923        //    previsto un solo record)
396400080619           knk1 =%editc(FondiCPO:'X') ;
396500080619           setll  ('P':knk1:knk2:ktnt) TFNTC01l    ;
396600080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
396700080619 5         dow not %eof (tfntc01l)    ;
396800080619 6          if (v031tipf='T' and %subst(ntcrnt:1:21) =s032cod )  or
396900080619               (v032tipf='T' and %subst(ntcrnt:1:21) =s031cod )  ;
397000080619               ntcnk1=%editc(Tienicpo:'X')   ;
397100080619               update tfntc   ;
397200080619 6          endif    ;
397300080619           reade  ('P':knk1:knk2:ktnt) TFNTC01l    ;
397400080619 5         enddo   ;
397500080619 4         endif  ;
397600080619
397700080619 3      endif   ;
397800080619 2      endif   ;
397900081013 1a     endif   ;
398000080619
398100080619        s03nrr=s03nrr+1  ;
398200080619        chain  s03nrr mk07s03  ;
398300080619 1      enddo  ;
398400100525
398500100525        // 3° giro : cerco CONTATTI BATCH su pot FUSO/ da Tenere
398600100525
398700100525        setll (fondicpo: 0:0:0) tiatc04l     ;
398800100525        reade (fondicpo: 0:0:0) tiatc04l     ;
398900100525        dow   not %eof(tiatc04l)   ;
399000100525
399100100525        if atcpri<>'BATCH'    ;
399200100525        w0030= %int(%subst(%editc(atccmm:'X') :1:3))  ;
399300100525        Indx =%lookup(w0030:pog)   ;
399400100525        endif   ;
399500100525
399600100525        if atcpri='BATCH' or Indx>0 ;
399700100525        // se potenziale che tengo ha trattativa o ksc e attitivà mia o batch, deleto subito
399800100525          if TieniconTrCd='S'   ;
399900100525            exsr CanceAtt   ;
400000100525          else   ;
400100100525          // Memorizzo che c'e'
400200100525            if atcpri='BATCH' ;
400300100525             FondiAttB='S'   ;
400400100525             else  ;
400500100525             FondiAttI='S'   ;
400600100525            endif  ;
400700100525          endif  ;
400800100525        else   ;
400900100525          if fondiAttI=' '   ;
401000100525            FondiAttI='N'   ;
401100100525          endif  ;
401200100525        endif  ;
401300100525
401400100525        reade (fondicpo: 0:0:0) tiatc04l     ;
401500100525        enddo   ;
401600100525
401700100525        // Se esiste una BATCH  o mia e Tieni non ha trattativa ;
401800100525        //  verifico cosa devo cancellare
401900100525        if TieniconTrCd=' ' and (FondiattI='S' or FondiAttB='S');
402000100525        setll (Tienicpo: 0:0:0) tiatc04l     ;
402100100525        reade (Tienicpo: 0:0:0) tiatc04l     ;
402200100525        dow   not %eof(tiatc04l)   ;
402300100525
402400100525        if atcpri= 'BATCH' and FondiAttI<>' '  ;
402500100525            exsr CanceAtt   ;
402600100525            else   ;
402700100525            TieniAtt ='S'   ;
402800100525        endif   ;
402900100525
403000100525        reade (Tienicpo: 0:0:0) tiatc04l     ;
403100100525        enddo  ;
403200100525        endif   ;
403300100525
403400100525        // Se quello che tengo ha attività e il Fondi ne ha una BATCH --> la deleto
403500100525        if TieniAtt='S' and fondiAttB='S'    ;
403600100525        setll (fondicpo: 0:0:0) tiatc04l     ;
403700100525        reade (fondicpo: 0:0:0) tiatc04l     ;
403800100525        dow   not %eof(tiatc04l)   ;
403900100525        if atcpri='BATCH'   ;
404000100525            exsr CanceAtt   ;
404100100525        endif   ;
404200100525
404300100525        reade (fondicpo: 0:0:0) tiatc04l     ;
404400100525        enddo   ;
404500100525        endif   ;
404600100525
404700100525        //  Aggiorno TUTTE LE ATTIVITA' RIMASTE
404800100525
404900111013        exec sql  update tiatc00f set atccpo=:tienicpo, atctas = ' '
405000111013                  where atccpo=:fondicpo;
405100100521
405200100525        // 5° giro : copio tutte le note interne di quello che fondo
405300100525        //          in quello da tenere DEL NUOVO FILE  NOTE
405400100521          exsr NewNote   ;
405500080619
405600100525        // 6° giro: DELETO i record rimasti di quello da fondere
405700080619        knk1 =%editc(FondiCPO:'X') ;
405800080619        clear knk2  ;
405900080619        setll  ('P':knk1) TFNTC01l    ;
406000080619        reade  ('P':knk1) TFNTC01l    ;
406100080619        dow not %eof (tfntc01l)    ;
406200080619          delete tfntc ;
406300080619          reade  ('P':knk1) TFNTC01l    ;
406400080619        enddo    ;
406500080929        setll  (FondiCPO) TiCPI01l    ;
406600080929        reade  (fondiCPO) TiCPI01l    ;
406700080929        dow not %eof (ticpi01l)    ;
406800080929          delete ticpi000 ;
406900080929        reade  (fondiCPO) TiCPI01l    ;
407000080619        enddo    ;
407100121018
407200121018       //?8° giro aggiorno potenziale sul file storico CRM del potenziale FUSO
407300121018       //?        imposto il potenziale DA TENERE
407400130215        exec sql
407500130215          UPDATE TICRM00F set CRMcpo = :Tienicpo
407600130215          WHERE  CRMcpo = :Fondicpo;
407700130215
407800130215        //setll (fondicpo) TICRM01L;
407900130215        //reade (fondicpo) TICRM01L;
408000130215        //DOW  not %eof(TICRM01L);
408100130215        //  CRMcpo = tienicpo;
408200130215        //  UPDATE  TICRM000;
408300130215        //  reade (fondicpo) TICRM01L;
408400130215        //ENDDO;
408500080619
408600100525        // 9° giro : aggiorno cod potenziale su eventuali ANAGRAFICHE
408700080624        //           che hanno il codice vecchio
408800080624        setll    fondicpo cnaco16l  ;
408900080624        reade(E) fondicpo cnaco16l  ;
409000080624        dow not %eof(cnaco16l) and not %error  ;
409100080624        clear acodtr             ;
409200080624        clear acoftr             ;
409300080624        Dataiso =%date(*date:*iso)  ;
409400080624        Dataymd =Dataiso;
409500080624        acoduv=%dec(Dataymd);
409600080624        acolib=tienicpo          ;
409700080624        update cnaco000          ;
409800080624
409900080624        reade(E) fondicpo cnaco16l  ;
410000080624        enddo                    ;
410100080624
410200080624        setll    fondicpo tfaco16l  ;
410300080624        reade(E) fondicpo tfaco16l  ;
410400080624        dow not %eof(tfaco16l) and not %error  ;
410500080624        clear acodtr             ;
410600080624        clear acoftr             ;
410700080624        Dataiso =%date(*date:*iso)  ;
410800080624        Dataymd =Dataiso;
410900080624        acoduv=%dec(Dataymd);
411000080624        acolib=tienicpo          ;
411100080624        update tfaco000          ;
411200080624
411300080624        reade(E) fondicpo tfaco16l  ;
411400080624        enddo                    ;
411500100525
411600100525        //10° giro : aggiorno TRATTATIVE --> TIVIS
411700100525        exec sql  update tivis00f set viscpo=:Tienicpo where viscpo=:Fondicpo;
411800140715
411900140715        //?11° giro: aggiorno ESTENSIONE anagrafica POTENZIALE?
412000140715
412100140918        // -?Salvataggio e Cancellazione dati potenziale da Fondere?
412200140918        clear  SavCPO1ds;
412300140715        chain  FondiCPO  TNCPO100;
412400140715        if  %found(TNCPO11L);
412500140918          SavCPO1ds = TNCPO1ds;
412600140715          DELETE  TNCPO100;
412700140715        endif;
412800140918
412900140918        // -?Aggiornamento dati potenziale da Tenere?
413000140715        chain  TieniCPO  TNCPO100;
413100140918        if  NOT %found(TNCPO11L);
413200140918          clear  TNCPO100;
413300140918          CPO1cpo = TieniCPO;
413400140918        endif;
413500140918
413600140918        // -?Telefono?
413700140715        Select;
413800140715          When  %found(TNCPO11L)  and  CPO1tel = *blanks
413900140918                                  and  X_CPO1tel <> *blanks;
414000140918            CPO1tel = X_CPO1tel;
414100140918          When  NOT %found(TNCPO11L)  and  X_CPO1tel <> *blanks;
414200140918            CPO1tel = X_CPO1tel;
414300140715        EndSl;
414400140918
414500140918        // -?Fax?
414600140918
414700140918        // -?Data Ultima Conferma INFO Commerciali?
414800140919        if  (V031tipF = 'T'  and  wScelta = '2')  or
414900140919            (V032tipF = 'T'  and  wScelta = '1');
415000140919          CPO1dUCifo = X_CPO1dUCifo;
415100140919        endif;
415200140918
415300140918        // -?CRIF Number?
415400140918        if  X_CPO1crifN  <> *blank  and
415500140918            (CPO1crifN   =  *blank  or
415600140918             F_CPOfsf    =  'S');
415700140918          CPO1crifN  = X_CPO1crifN;
415800140918        endif;
415900140918
416000140918        // -?CRIF Number Casa Madre?
416100140918        if  X_CPO1crifNM <> *blank  and
416200140918            (CPO1crifNM  =  *blank  or
416300140918             F_CPOfsf    =  'S');
416400140918          CPO1crifNM = X_CPO1crifNM;
416500140918        endif;
416600140918
416700140918        // -?Flag Operativi?
416800140918
416900140918        // -?Aggiornamento rec.?
417000140918        Select;
417100140918          When  %found(TNCPO11L);
417200140918            UPDATE  TNCPO100;
417300140918          When  NOT %found(TNCPO11L)  and
417400140918               (CPO1tel    <> *blank  or
417500140918                CPO1dUCIfO <> *zero   or
417600140918                CPO1crifN  <> *blank  or
417700140918                CPO1crifNM <> *blank);
417800140918            WRITE  TNCPO100;
417900140918          Other;
418000140918            UNLOCK  TNCPO11L;
418100140918        EndSl;
418200080624
418300140715        //?12° giro: aggiorno anagrafica POTENZIALE?
418400080619
418500140918        // -?Anno Inizio Attività (EX TELEFONO - vedi dCPO02)?
418600140918        dCPO02 = CPOtel;
418700140918        if  %subst( F_CPOtel : 1 : %size(dCPO02.§CPOannoIA) )
418800140918                               >  *zero  and
418900140918            (dCPO02.§CPOannoIA <= *zero  or
419000140918             F_CPOfsf          =  'S');
419100140918          dCPO02.§CPOannoIA = %subst( F_CPOtel : 1 :
419200140918                                    %size(dCPO02.§CPOannoIA) );
419300140918          CPOtel = dCPO02;
419400140918        endif;
419500140918
419600140918        // -?piv e cdf?
419700080619        if cpopiv=*blanks and cpocdf=*blanks   ;
419800080619        cpopiv=F_cpopiv     ;
419900080619        cpocdf=F_cpocdf     ;
420000080619        endif               ;
420100080624
420200080624
420300140918        // -?cod commerciale?
420400080624        if cpocmm=0   and f_cpocmm>0       ;
420500080624        cpocmm=F_cpocmm     ;
420600080624        endif               ;
420700080619
420800140918        // -?dati D&B   tengo quelli che li ha o il pot SEDE se entrambi li hanno?
420900140918        //  ?data primo contatto, tengo la data più vecchia tra i 2 potenziali
421000090520        dcpo01=cporst       ;
421100090520
421200090520        if F_cpoftaz>0;
421300090520        if cpoftaz=0  or  f_cpofsf='S'   ;
421400080619        cpoftaz=F_cpoftaz   ;
421500080619        cpoafaz=F_cpoafaz   ;
421600080619        cpoffaz=F_cpoffaz   ;
421700080619        cpopexp = F_cpopexp ;
421800090520          // Memorizzo anche il fatturato presunto
421900090520          if %subst(f_cporst:24:11)>*zeros   ;
422000090520            §cposptp=%int(%subst(f_cporst:24:11)) ;
422100090520          endif   ;
422200080619        endif               ;
422300090520        endif               ;
422400120622
422500120622        //?data primo contatto
422600120622        SELECT;
422700120622          WHEN  (DataPrimoConT > 0 and
422800120622                 DataPrimoConT < DataPrimoConF) or
422900120622                 DataPrimoConF = 0;
423000120622            §CPOdtpcon = %editc(DataPrimoConT:'X');
423100120622          WHEN  (DataPrimoConF > 0 and
423200120622                 DataPrimoConF < DataPrimoConT) or
423300120622                 DataPrimoConT = 0;
423400120622            §CPOdtpcon = %editc(DataPrimoConF:'X');
423500120622        ENDSL;
423600140918
423700140918        // -?Data Ultimo Aggiornamento INFO Commerciali?
423800140919        if  (V031tipF = 'T'  and  wScelta = '2')  or
423900140919            (V032tipF = 'T'  and  wScelta = '1');
424000140919          CPO1dUCifo = X_CPO1dUCifo;
424100140919          §CPOifoDUl = %subst( F_CPOrst : 5 : %size(§CPOifoDUl) );
424200140919        endif;
424300140918
424400140918        // -?Flag Problemi Finanziari (1° byte di CPOCLT)?
424500140918        if  %subst( F_CPOclt : 1 : 1 ) <> *blank  and
424600140918            ( %subst( CPOclt : 1 : 1 ) =  *blank  or
424700140918              F_CPOfsf               =  'S' );
424800140918          %subst( CPOclt : 1 : 1 ) = %subst( F_CPOclt : 1 : 1 );
424900140918        endif;
425000080619
425100140918        // -?Sede/Filiale?
425200080619        if cpofsf='F' and F_cpofsf='S'  ;
425300080619        cpofsf='S'          ;
425400080619        endif               ;
425500080619
425600140918        // -?codice  duns?
425700080619        if cpoduns=*blanks or (F_cpoduns<>*blanks and F_cpofsf='S') ;
425800080619        cpoduns=F_cpoduns   ;
425900080619        endif               ;
426000110304
426100110304       //?Ricalcolo la categoria del potenziale
426200110304        clear trmk05ds;
426300110304        IMK05cpo = CPOcpo;
426400110304        trmk05r (kpjba : TRMK05DS);
426500110304        IF  OMK05err = *blanks;
426600110304          CPOfls = OMK05cat;
426700110304        ENDIF;
426800080619
426900080619        cpodtr=*date        ;
427000080619        §cporicfus='S'      ;
427100080619        cporst=dcpo01       ;
427200080619        update tncpo000     ;
427300080619
427400080619        delete tncpofis     ;
427500090520
427600140918        // -?Aggiorno potenziale per i dati contenuti in CPORST?
427700090521        alfacpo=%editc(cpocpo:'X')      ;
427800090521        callp   TRMK08R   (kpjba:alfacpo)  ;
427900090521
428000080619
428100140918         // -?Imposto il posizionamento cursore per il sfl che emetto?
428200140918         //  ?aggiornato?
428300080619         if nrrFUS(1)> 0 ;
428400080619           C02csr=nrrFUS(1) ;
428500080619           pagric=(c02csr/C_sflPag)+1 ;
428600080619           $Video = 'S2';
428700080619           $InzS02 = *on;
428800080619         else           ;
428900080619           $Video = 'D1';
429000080619           $InzD01 = *on;
429100080619         endif    ;
429200080619
429300080619       ENDSR  ;
429400100525       //--------------------------------------------------------------
429500100525       //?cancello attività della trascodifica
429600100525       //--------------------------------------------------------------
429700100525       BEGSR CanceAtt   ;
429800100525
429900100525        // cancello  eventuale legame
430000100525          delete (atcatn:atcatnp) tiatl01l   ;
430100100525
430200100525        // cancello  eventuali note
430300100525
430400100525         cpncpo = atccpo ;
430500100525         cpntat = atctat ;
430600100525         cpnatn = atcatn ;
430700100525         cpnatnp= 0 ;
430800100525         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
430900100525         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
431000100525         dow not %eof(ticpn03l);
431100100525             delete ticpn000;
431200100525             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
431300100525         enddo ;
431400100525
431500100525       delete   tiatc000   ;
431600100525
431700100525       ENDSR  ;
431800100521       //--------------------------------------------------------------
431900100521       //?Scrivo le note interne nuovo file sl pot che tengo
432000100521       //--------------------------------------------------------------
432100100521       BEGSR Newnote;
432200100521
432300100524        exec sql declare A1 cursor for
432400100524                 select  * from ticpn04l where cpncpo= :Fondicpo
432500100524                 order by cpncpo, cpndim, cpnhim   ;
432600100524        exec sql open  A1;
432700100524        exec sql fetch next from A1 into :ticpnds ;
432800100524
432900100524        dow  sqlcod = 0;
433000100524
433100100524        Totnot=10 ;
433200100524
433300100524        // Verifico se c'e' già un nota con data e ora per il potenziale da tenere
433400100524
433500100524       dow Totnot>0  and cpnpno=1 ;
433600100524       exec sql  select count(cpncpo) into :Totnot from ticpn04l where
433700100524                 cpncpo= :Tienicpo  and cpndim= :cpndim and cpnhim= :cpnhim  ;
433800100524
433900100524        if  totnot>0   ;
434000100524         OraHMS=%time(cpnhim)  ;
434100100524         OraHMS=oraHMS- %minutes(10) ;
434200100524         cpnhim=%dec(OraHMS)  ;
434300100524        else  ;
434400100524         newhim=cpnhim    ;
434500100524        endif  ;
434600100524        enddo  ;
434700100524
434800100524        cpnhim= newhim     ;
434900100525        cpncpo= Tienicpo   ;
435000100524        write ticpn000  ;
435100100524
435200100525        exec sql fetch next from A1 into :ticpnds ;
435300100524        enddo    ;
435400100524
435500100524        exec sql close A1;
435600100524
435700100524        exec sql delete from ticpn04l where cpncpo= :Fondicpo    ;
435800100524
435900100521       ENDSR   ;
436000080206       //--------------------------------------------------------------
436100080206       //?Operazioni finali.
436200080206       //--------------------------------------------------------------
436300080206       BEGSR RoutEnd;
436400080410
436500080206         *inLR = *on;
436600080206         return;
436700080206
436800080206       ENDSR;
436900080206
437000080206      /end-free
437100080206
437200080206       //--------------------------------------------------------------
437300080206       //?Schiere a tempo di compilazione.
437400080206       //--------------------------------------------------------------
437500080206
437600080410** - MSG ------------------------------------------------------------------ -*
437700080411Utente non abilitato alla gestione dei potenziali!!!                           01
437800080523Effettuare almeno una scelta                                                   02
437900080618Codice potenziale SEDE  errato o non selezionato                               03
438000080603Impossibile fare FUSIONE di 2 potenziali SEDE                                  04
438100111115Impossibile fare FUSIONE di 2 potenziali in categoria "Eliminabile"            05
438200080523Immettere almeno un codice potenziale FILIALE                                  06
438300111115Categoria potenziale errata                                                    07
438400080411Filiale appartenenza errata                                                    08
438500080414Non è stato trovato nessun potenziale per le selezioni immesse                 09
438600080415Caricamento dati non completabile: selezionati troppi dati!                    10
438700140923Se immessa la provincia, immettere anche la ragione sociale                    11 LIBERO
438800080415Provincia errata                                                               12
438900080415Selezionare soltanto una SEDE                                                  13
439000080523Impossibile selezionare più di 7 FILIALI per l'Unione                          14
439100080526Impossibile Unione di una SEDE senza partiva iva e codice fiscale              15
439200140923Pot.xxxxxxxxxxx-yyyyyyyyyyyy non selezionabile:già presente la SEDE x l'Unione 16
439300080526Filiale con partita IVA e/o cod.Fiscale diversi dalla SEDE-Enter Forza         17
439400080603Per la FUSIONE selezionare soltanto 2 potenziali !                             18
439500140923Pot.xxxxxxxxxxx-yyyyyyyyyyyy non selezionabile:già due codici per la Fusione   19
439600080604E' stato selezionato lo stesso codice potenziale!!                             20
439700080609Selezionare il potenziale da TENERE e quello da FONDERE!!!                     21
439800080609Impossibile selezionare più di xx xxxxx per                                    22
439900080617Nessuna selezione effettuata per YYYYYYYYYYY xxxxxxxxxxxxxxxxxxx               23
440000080609Impossibile selezionare 2 volte il codice "xxxx" per xxxxxx                    24
440100111115Il potenziale in cat. "Eliminabile" deve essere "F U S O"  !!                  25
440200080618Utente non abilitato all'aggiornamento del potenziale                          26
440300080618Codice potenziale  errato o non selezionato                                    27
440400140923Utente non abilitato a FONDERE il potenz.:presenti AZIONI nell'anno            28 LIBERO
440500081010Utente non abilitato all'aggiornamento dei 2 potenz.:FUSIONE NON EFFETTUABILE  29
440600080619Potenziale allocato da altro utente:controllare e riprovare!                   30
440700081009Potenziale non più presente in anagrafica: già normalizzato!!??!!              31
440800100521Presenti ANAGRAFICHE sul potenziale da Fondere: saranno aggiornate             32
440900140923                                                                                  LIBERO
441000140923                                                                                  LIBERO
441100081013Categ.Merceologica del potenziale da TENERE errata: correggerla con "M-Manut." 35
441200100419Scegliere le Info Commerciali da TENERE impostando una sola "T"                36
441300140923Dettaglio Prodotti NON visualizzabile                                          37
441400140923                                                                                  LIBERO
441500100525Fusione NON EFFETTUABILE:trattativa aperta per il potenziale da Fondere        39
441600100521Presenti TRATTATIVE sul potenziale da Fondere: saranno aggiornate              40
441700100521Presenti Anagrafiche e Trattative sul potenziale da Fondere:saranno aggiornate 41
441800100524Utente non abilitato a FONDERE il potenz.:presenti ANAGRAFICHE                 42
441900100525Il potenziale codificato è da TENERE se l'altro non è codificato               43
