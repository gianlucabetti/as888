000100080206      //--------------------------------------------------------------
000200091026      //?TRMK01R - INTERROGAZIONE ANAGRAFICA CLIENTI POTENZIALI
000300080206      //--------------------------------------------------------------
000400080207      //? A T T E N Z I O N E ! ! ! ?
000500080206      // Il pgm controlla il codice dell'azione x stabilire se
000600080206      //   gestione o interrogazione potenziali
000700080206      //--------------------------------------------------------------
000800080206
000900080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000100409     h dftactgrp(*no) actgrp(*caller)
001100080206
001200080206      //---------------------------------------------------------------
001300080206      //?Dichiarazione file.
001400080206      //---------------------------------------------------------------
001500110310
001600110310      // - File Info Commerciali
001700110310     fTICPI01L  if   e           k disk
001800110601
001900110601      // - Trattative
002000110803     fTIVIS01L  if   e           k disk
002100050704
002200080208      // - Anagrafica clienti potenziali
002300080206     fTNCPO01L  if   e           k disk
002400080208     fTNCPO02L  if   e           k disk    rename(TNCPO000 : TNCPO002)
002500080208     fTNCPO04L  if   e           k disk    rename(TNCPO000 : TNCPO004)
002600110308
002700080208      // - Organigramma filiale/aziendale
002800080206     fAZORG01L  if   e           k disk
002900110308
003000080208      // - Tabelle
003100080206     fTABEL00F  if   e           k disk
003200110308     fTNTBE01L  if   e           k disk
003300080206
003400110308      // - Anagrafica clienti
003500081017     fcnaco00f  if   e           k disk
003600081021     fcnind00f  if   e           k disk
003700081021     fcnclp00f  if   e           k disk
003800110308
003900080208      // - Video Interrogazione anagrafica clienti potenziali
004000091026     fTRMK01D   cf   e             workstn
004100091026     f                                     sfile(MK01S01 : S01nrr)
004200080208     f                                     indds(IndDspF)
004300080206     f                                     infds(InfDspF)
004400080206
004500080206      //---------------------------------------------------------------
004600080206      //?Definizione costanti.
004700080206      //---------------------------------------------------------------
004800050519
004900080207      // - Numero di record in ciascuna videata di subfile
005000080207     d c_SflPag        c                   const(18)
005100080207
005200080207      // - Tasti funzionali a video
005300080207     d c_F01           c                   const(x'31')
005400080207     d c_F02           c                   const(x'32')
005500080207     d c_F03           c                   const(x'33')
005600091026     d c_F04           c                   const(x'34')
005700080207     d c_F05           c                   const(x'35')
005800080207     d c_F06           c                   const(x'36')
005900080207     d c_F07           c                   const(x'37')
006000080207     d c_F08           c                   const(x'38')
006100080207     d c_F09           c                   const(x'39')
006200080207     d c_F10           c                   const(x'3A')
006300091026     d c_F11           c                   const(x'3B')
006400080207     d c_F12           c                   const(x'3C')
006500080207     d c_F13           c                   const(x'B1')
006600080207     d c_F14           c                   const(x'B2')
006700080207     d c_F15           c                   const(x'B3')
006800080207     d c_F16           c                   const(x'B4')
006900080207     d c_F17           c                   const(x'B5')
007000080207     d c_F18           c                   const(x'B6')
007100080207     d c_F19           c                   const(x'B7')
007200080207     d c_F20           c                   const(x'B8')
007300080207     d c_F21           c                   const(x'B9')
007400080207     d c_F22           c                   const(x'BA')
007500080207     d c_F23           c                   const(x'BB')
007600080207     d c_F24           c                   const(x'BC')
007700080207     d c_Enter         c                   const(x'F1')
007800080207     d c_RollDown      c                   const(x'F4')
007900080207     d c_RollUp        c                   const(x'F5')
008000080214
008100080214      // - Attributi di visualizzazione
008200080215      //  -  DspAtr() - Normale
008300080214     d C_dspatr_Norm   c                   const(x'20')
008400080215      //  -  DspAtr(RI)
008500080214     d C_dspatr_RI     c                   const(x'21')
008600081021     d C_dspatr_HI     c                   const(x'22')
008700080215      //  -  DspAtr(ND)
008800080214     d C_dspatr_ND     c                   const(x'27')
008900080215      //  -  DspAtr(BL) / Color(Red)
009000080214     d C_dspatr_BL     c                   const(x'28')
009100080206
009200080206      //---------------------------------------------------------------
009300080206      //?Definizione schiere.
009400080206      //---------------------------------------------------------------
009500080206
009600080206      // - Messaggi di errore
009700140910     d $Msg            s             78    dim(34) ctdata perrcd(1)
009800110308
009900081017     d potsede         s             11  0 dim(500)
010000110308
010100080207      // - Filiali
010200080207     d C_Max_Fil       c                   const(350)
010300080207     d $Fil            s                   like(CPOflt)
010400080208     d                                     dim(C_Max_Fil)  inz
010500080207     d $Car            s                   like(ORGcar)
010600080207     d                                     dim(C_Max_Fil)  inz
010700110309
010800110309      // - Sk appoggio per filiali di un'area richiesta
010900110309     d $Fil1           s                   like(CPOflt)
011000110309     d                                     dim(C_Max_Fil)  inz
011100110308
011200110308      // - Categoria merceologica da tabella 1L
011300110308     d $1l             s              5    dim(50)
011400110308
011500110308      // - Importanza clienti da tabella IC
011600110308     d $Ic             s              1    dim(10)
011700110308
011800110407      // - Categoria potenziale da tabella CPO
011900110407     d $Cpo            s              1    dim(10)
012000110407      // - Categoria potenziale da tabella CPO da proporre
012100110407     d $CpoI           s              1    dim(5)
012200110407
012300110308      // - Concorrenti possibili da tabella IFS
012400110308     d $Ifs            s              4    dim(30)
012500110322
012600131122      // - Concorrenti possibili da tabella IFS logistica
012700131122     d $IfsLOG         s              4    dim(30)
012800140910
012900140910      // - Paesi Estero possibili da tabella IFS
013000140910     d $IfsPAESI       s              4    dim(30)
013100131122
013200110407      // - Sk x importanza clienti richiesti nella selezione
013300110322     d $cic            s              1    dim(5)
013400110407
013500110407      // - Sk x categoria potenziali richiesti nella selezione
013600110407     d $fls            s              1    dim(5)
013700110308
013800080206
013900080206      //---------------------------------------------------------------
014000080206      //?Definizione aree dati.
014100080206      //---------------------------------------------------------------
014200080206
014300080206      // - Dati utente
014400080206     d §AzUte        e ds                  extname(AZUTE00F)
014500080206     d                                     dtaara
014600080206     d §DatiUte      e ds                  extname(dDatiUte)
014700080206     d                                     dtaara
014800080206
014900080206      //---------------------------------------------------------------
015000080206      //?Definizione strutture dati.
015100080206      //---------------------------------------------------------------
015200080206
015300080206      // - Status
015400080206     d Psds           sds
015500080206     d   SDSpgm          *proc
015600080206
015700080206      // - InfDS
015800080206     d InfDspF         ds
015900080207     d  dsp_aid              369    369a                                        AID byte
016000080206
016100080206      // - Indicatori su DspF
016200080208     d IndDspF         ds
016300080206        // - Abilitazione tasti funzionali
016400080206     d  F10Attivo                     1n   overlay(IndDspF : 10)
016500131125        // - Reverse immage  per comando altre parzializzazioni
016600131125     d  altreparz                     1n   overlay(IndDspF : 11)
016700080206        // - Condizionamento visualizzazioni
016800080206     d  GESclipot                     1n   overlay(IndDspF : 25)
016900081020     d  pgmrichia                     1n   overlay(IndDspF : 26)
017000080206        // - Indicatori di gestione del subfile
017100080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
017200080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
017300080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
017400080206     d  SflEnd                        1n   overlay(IndDspF : 33)
017500080206        // - Indicatori di errore
017600080206     d  errMessage                    1n   overlay(IndDspF : 28)
017700080206     d  PosCurOpz                     1n   overlay(IndDspF : 50)
017800080206     d  PosCurCP1                     1n   overlay(IndDspF : 51)
017900080215     d  PosCurOSF                     1n   overlay(IndDspF : 52)
018000110308     d  PosCurCic1                    1n   overlay(IndDspF : 53)
018100110407     d  PosCurFls1                    1n   overlay(IndDspF : 54)
018200110308     d  PosCurCic2                    1n   overlay(IndDspF : 55)
018300080215     d  PosCurPRV                     1n   overlay(IndDspF : 56)
018400080215     d  PosCurCAP                     1n   overlay(IndDspF : 57)
018500110308     d  PosCurCic3                    1n   overlay(IndDspF : 58)
018600110308     d  PosCurCic4                    1n   overlay(IndDspF : 59)
018700110308     d  PosCurCic5                    1n   overlay(IndDspF : 60)
018800110308     d  PosCurFlt                     1n   overlay(IndDspF : 61)
018900110308     d  PosCurSCT1                    1n   overlay(IndDspF : 62)
019000080215     d  PosCurCAR                     1n   overlay(IndDspF : 63)
019100140910     d  PosCurPexpa                   1n   overlay(IndDspF : 64)
019200110308     d  PosCurSct2                    1n   overlay(IndDspF : 65)
019300140910     d  PosCurAnnoa                   1n   overlay(IndDspF : 66)
019400110310     d  PosCurSct3                    1n   overlay(IndDspF : 67)
019500140910     d  PosCurPexpda                  1n   overlay(IndDspF : 68)
019600140910     d  PosCurAnnoda                  1n   overlay(IndDspF : 69)
019700110310     d  PosCurPkgda                   1n   overlay(IndDspF : 70)
019800110310     d  PosCurPkga                    1n   overlay(IndDspF : 71)
019900110310     d  PosCurConc1                   1n   overlay(IndDspF : 72)
020000110310     d  PosCurConc2                   1n   overlay(IndDspF : 73)
020100110310     d  PosCurConc3                   1n   overlay(IndDspF : 74)
020200110407     d  PosCurFls2                    1n   overlay(IndDspF : 75)
020300110407     d  PosCurFls3                    1n   overlay(IndDspF : 76)
020400110407     d  PosCurFls4                    1n   overlay(IndDspF : 77)
020500110407     d  PosCurFls5                    1n   overlay(IndDspF : 78)
020600131122     d  PosCurInt                     1n   overlay(IndDspF : 79)
020700131122     d  PosCurDil1                    1n   overlay(IndDspF : 80)
020800131122     d  PosCurDil2                    1n   overlay(IndDspF : 81)
020900131122     d  PosCurlos                     1n   overlay(IndDspF : 82)
021000131122     d  PosCurConcl1                  1n   overlay(IndDspF : 83)
021100131122     d  PosCurConcl2                  1n   overlay(IndDspF : 84)
021200131122     d  PosCurConcl3                  1n   overlay(IndDspF : 85)
021300140910     d  PosCurPaesi1                  1n   overlay(IndDspF : 86)
021400140910     d  PosCurPaesi2                  1n   overlay(IndDspF : 87)
021500140910     d  PosCurPaesi3                  1n   overlay(IndDspF : 88)
021600140910     d  PosCurNrfda                   1n   overlay(IndDspF : 89)
021700140910     d  PosCurNrfa                    1n   overlay(IndDspF : 90)
021800140910     d  PosCurNroda                   1n   overlay(IndDspF : 91)
021900140910     d  PosCurNroa                    1n   overlay(IndDspF : 92)
022000140910     d  PosCurNerda                   1n   overlay(IndDspF : 93)
022100140910     d  PosCurNera                    1n   overlay(IndDspF : 94)
022200140910     d  PosCurNaeda                   1n   overlay(IndDspF : 95)
022300140910     d  PosCurNaea                    1n   overlay(IndDspF : 96)
022400140910     d  PosCurNtrda                   1n   overlay(IndDspF : 97)
022500140910     d  PosCurNtra                    1n   overlay(IndDspF : 98)
022600140910     d  ErrGenerico                   1n   overlay(IndDspF : 99)
022700080206
022800080206     d WindDspF        ds                  inz
022900080207     d   WindDspFa             1     49    inz(*zero)
023000080207     d   WindDspFb            50     99    inz(*zero)
023100080206
023200131122     d wlbdat          ds                  inz
023300131122     d  g02dat                 1      8  0
023400131122     d  g02inv                 9     16  0
023500131122     d  g02err                17     17
023600131122     d  g02tgi                18     22  0
023700131122
023800080206      // "Unificazione" codice cliente potenziale a video
023900110308     d V01cpo          ds                  inz
024000110308     d   V01cp1                            inz
024100110308     d   V01cp2                            inz
024200110308
024300110308      // Campo unico per categoria merceologica
024400110308     d V01sct          ds                  inz
024500110308     d   V01sct1                           inz
024600110308     d   V01sct2                           inz
024700110308     d   V01sct3                           inz
024800110308
024900110308      // Campo unico per Importanza clienti
025000110308     d V01cic          ds                  inz
025100110308     d   V01cic1                           inz
025200110308     d   V01cic2                           inz
025300110308     d   V01cic3                           inz
025400110308     d   V01cic4                           inz
025500110308     d   V01cic5                           inz
025600110308
025700110407      // Campo unico per Categoria potenziale
025800110407     d V01fls          ds                  inz
025900110407     d   V01fls1                           inz
026000110407     d   V01fls2                           inz
026100110407     d   V01fls3                           inz
026200110407     d   V01fls4                           inz
026300110407     d   V01fls5                           inz
026400140910
026500140910      // Campo unico % Fatturato Export
026600140910     d V01pexp         ds                  inz
026700140910     d   V01pexpda                         inz
026800140910     d   V01pexpa                          inz
026900140910
027000140910      // Campo unico Anno Inizio Attività
027100140910     d V01anno         ds                  inz
027200140910     d   V01annoda                         inz
027300140910     d   V01annoa                          inz
027400140910
027500140910      // Campo unico per Paesi Estero
027600140910     d V02paesi        ds                  inz
027700140910     d   V02paesi1                         inz
027800140910     d   V02paesi2                         inz
027900140910     d   V02paesi3                         inz
028000110407
028100110308      // Campo unico per Concorrenti
028200140910     d V02conc         ds                  inz
028300140910     d   V02conc1                          inz
028400140910     d   V02conc2                          inz
028500140910     d   V02conc3                          inz
028600131122
028700131122      // Campo unico per Concorrenti Logistica
028800131125     d V02conl         ds                  inz
028900131125     d   V02concl1                         inz
029000131125     d   V02concl2                         inz
029100131125     d   V02concl3                         inz
029200080206
029300080206      // - Parametri ricevuti
029400080206     d KPJBA         e ds
029500091026     d TRMK01ds      e ds                  inz
029600080206
029700080206      // - Reperimento dati utente
029800080206     d TIBS34ds      e ds
029900081020     d TRUL31DS      e ds
030000081020     d POG                    10    759    DIM(250)
030100080206
030200080206      // - Ricerca/Controllo tabelle
030300080206     d TIBS02ds      e ds                  inz
030400120914
030500120914      // - File attività
030600120914     dTIATCds        e ds                  extname(TIATC00F)
030700110309
030800110309      // - File potenziali
030900110309     d TNCPOds       e ds                  extname(TNCPO00F)
031000110309
031100110309      // - File trattative
031200110309     d TIVIS00F      e ds                  extname(TIVIS00F)
031300110309
031400090701      // - controllo abilitazioni
031500090701     d TNTAA1DS      e ds                  inz
031600110509
031700110509      // - Ricerca tabella CPO
031800110509     d TNTB87ds      e ds                  inz
031900080206
032000080206      // - Tabella 1L = Categorie Merceologiche
032100080207     d ds1L          e ds                  inz
032200110308
032300080208      // - Tabella 05 = Codici Area
032400080208     d ds05          e ds                  inz
032500081020     d
032600110407
032700110407      // - Tabella CPO = Categoria potenziale
032800110407     d dCPO          e ds                  inz
032900110407
033000110308      // - Dati in CPORST
033100081020     d dcpo01        e ds                  inz
033200140910
033300140910      // - Dati in CPOTEL
033400140910     d dcpo02        e ds                  inz
033500080206
033600080206      // - Controllo congruenza CAP e provincia
033700080206     d FNLV13ds      e ds
033800080206     d   I13af0      e                     inz('S')
033900080206     d   I13cng      e                     inz('S')
034000080206     d TISI95ds      e ds                  inz
034100080206     d   I95tcn      e                     inz('3')
034200080206
034300081020     d TRMK50ds      e ds                  inz
034400100408
034500100408      // - apertura attività
034600100408     d Trmk19ds      e ds
034700080206
034800080206      //---------------------------------------------------------------
034900080206      //?Definizione variabili globali.
035000080206      //---------------------------------------------------------------
035100080206
035200080206      // - Flags booleani
035300110310     d $Conc           s               n   inz(*off)
035400131122     d $Conclog        s               n   inz(*off)
035500110309     d $EndAtt         s               n   inz(*off)
035600110309     d $EndTtr         s               n   inz(*off)
035700080208     d $Fine           s               n   inz(*off)
035800080208     d $InzD01         s               n   inz(*on)
035900080208     d $InzS01         s               n   inz(*on)
036000131122     d $InzD02         s               n   inz(*on)
036100080206     d $ErrGrave       s               n   inz(*off)
036200080207     d $EoF            s               n   inz(*off)
036300080208     d $RecOK          s               n   inz(*off)
036400140910     d $Paesi          s               n   inz(*off)
036500080206
036600080206      // - Indici di schiera
036700080208     d xx              s              3  0 inz
036800081017     d yy              s              3  0 inz
036900080206
037000080207      // - Campi associati al video
037100080207     d $Video          s              2    inz('D1')
037200080208     d S01nrr          s              4  0 inz
037300080207     d W_SflPag        s              3  0 inz
037400080207     d wPag            s              4  0 inz
037500080207     d wRig            s              3  0 inz
037600110309
037700110309       // - Stringa SQL da eseguire
037800110309     d wSQL            s           2048    Varying        inz
037900080206
038000080206      // - Campi di comodo
038100080207     d w030a           s             30    inz
038200131122     d wdil1           s              8  0 inz
038300131122     d wdil2           s              8  0 inz
038400080207     d $Ord            s              3  0 inz
038500080626     d savkpjbu        s                   like(kpjbu)
038600131122     d w_dalogistica   s              1
038700100202     d w_damenu        s              1
038800100408     d w_daatti        s              1
038900100423     d DataSYS         s               d   inz(*sys)
039000100423     d Dataiso         s               d   datfmt(*iso)
039100131125     d data_eur        s               d   Datfmt(*eur)
039200100423     d Alfaoggi        s              8
039300131125     d $logistica      s              3
039400100423     d W0080           s              8  0
039500110308     d C_tab           s                   like(TBEcod)
039600110309     d C_ke1           s                   like(TBEke1)
039700110310     d wval            s                   like(CPIval)
039800110310     d wpft            s                   like(CPIpft)
039900110308
040000110308     d idTabella       s              2
040100110308     d Ordinamento     s              1
040200110308     d idElemento      s              8
040300110308     d TastoUscita     s              1
040400110601
040500110601     d wLib            s             21    inz
040600110601     d wTtr            s              4  0 inz
040700080206
040800080206      //---------------------------------------------------------------
040900080206      //?Definizione procedure usate.
041000080206      //---------------------------------------------------------------
041100080208
041200080208      // - Controllo cap/località
041300080208     d fnlv13r         pr                  extpgm('FNLV13R')
041400080208     d  kpjba                              likeds(kpjba)    const
041500080208     d  fnlv13ds                           likeds(FNLV13ds)
041600080208     d  tisi95ds                           likeds(TISI95ds)
041700080206
041800110509      // - Gestione/Interrogazione potenziale
041900100408     d TRMK02R         pr                  extpgm('TRMK02R')
042000100408     d  kpjba                              likeds(kpjba)
042100100408     d  trmk01ds                           likeds(trmk01ds)
042200100408     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
042300110509
042400110509
042500110509      // - interrogazione tabella CPO
042600110509     d TNTB87R         pr                  extpgm('TNTB87R')
042700110509     d  tntb87ds                           likeds(TNTB87ds)
042800100408
042900080626      //---------------------------------------------------------------
043000080626      //?prototipi
043100080626      //---------------------------------------------------------------
043200080626      /copy gaitrasrc/srcprotopr,tibs02r
043300080626      /copy gaitrasrc/srcprotopr,tibs34r
043400080626      /copy gaitrasrc/srcprotopr,trmk07r
043500081020      /copy gaitrasrc/srcprotopr,trmk50r
043600081022      /copy gaitrasrc/srcprotopr,trmk51r
043700110308      /copy gaitrasrc/srcprotopr,trul19r
043800081020      /copy gaitrasrc/srcprotopr,trul31r
043900080626      /copy gaitrasrc/srcprotopr,x§taber
044000081017      /copy gaitrasrc/srcprotopr,xalfa3br
044100131125      /copy gaitrasrc/srcprotopr,xsrda8
044200090703      /copy gaitrasrc/srcprotopr,TNTAA1C
044300080206      //---------------------------------------------------------------
044400080206      //?Definizione key-list.
044500080206      //---------------------------------------------------------------
044600080206
044700080206      //---------------------------------------------------------------
044800080206      //?M A I N - L I N E
044900080206      //---------------------------------------------------------------
045000080206
045100080206     c     *Entry        plist
045200080206     c                   parm                    KPJBA
045300100408     c                   parm                    trmk19ds
045400131125
045500131125      * Verifico se sono stato richiamato da Menu e da una LOGISTICA
045600131125      * i primi 3 caratteri della KPJBU sono uguali a LOG
045700131125     c                   Movel     KPJBU         $LOGISTICA
045800131125     c                   If        $logistica = 'LOG'
045900131125     c                   Clear                   KPJBU
046000131125     c                   Endif
046100080206
046200080206      /free
046300080206
046400080206       // Operazioni iniziali
046500080206       exsr RoutInz;
046600110309
046700110309       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
046800080206
046900080206       // Gestione video
047000080206       DOW $Fine = *off;
047100080206         select;
047200080206           when $Video = 'D1';
047300080206             exsr GesD01;
047400131122           when $Video = 'D2';
047500131122             exsr GesD02;
047600080206           when $Video = 'S1';
047700080206             exsr GesS01;
047800080206           when $Video = 'WW';
047900080206             exsr sr_1_cpo;
048000080206           other;
048100080206             $Fine = *on;
048200080206         endsl;
048300080206       ENDDO;
048400080206
048500080206       // Operazioni finali
048600080206       exsr RoutEnd;
048700080206
048800080206       //--------------------------------------------------------------
048900080206       //?Operazioni iniziali.
049000080206       //--------------------------------------------------------------
049100080206       BEGSR RoutInz;
049200080206
049300080208         // Ricezione parametri
049400091027         TRMK01ds = kpjbu;
049500091028         clear  Mk01APP;
049600091028         Mk01f03 = *off;
049700091028         Mk01f06 = *off;
049800091028         Mk01f12 = *off;
049900091028         Mk01ent = *off;
050000100202
050100100202       // Se la kpjbu è vuota vuol dire che sono da menù
050200100202        IF  kpjbu = *blanks;
050300100202          eval w_damenu = 'S';
050400100202        ENDIF;
050500100408       // Se ho più di un parametro devo ricordarmi di passare anche il trmk19ds
050600100408        IF  %parms > 1   ;
050700100408          eval w_daAtti='S';
050800100408        ENDIF;
050900080206
051000080206       // Se il pgm. NON è richiamato: azzera i campi numerici
051100091028         if  Mk01k11 = *blank;
051200091028           clear  Mk01cpo;
051300091028           clear  Mk01nrv;
051400091028           clear  Mk01ksc;
051500080206         endif;
051600080206
051700080206         // Impostazione campi "fissi"
051800080208         T01pgm = SDSpgm;
051900080208         TBLkut = 1;
052000080206
052100080206         // Reperimento dati job
052200080206         exsr DatiJob;
052300080206
052400080206         // Controllo se ok l'abilitazione dell'utente
052500090701         reset tntaa1ds;
052600090701         itaa1caut='§utepot' ;
052700090701         clear kpjbu   ;
052800090701         kpjbu=tntaa1ds ;
052900090703         TNTAA1C     (kpjba);
053000090702         TNTAA1DS=KPJBU   ;
053100080206
053200131127         if  otaa1fabi='N'and $logistica = *blank ;
053300080206           $ErrGrave   = *on;
053400090702         else ;
053500131125         // in caso di richiami dalla logistica imposto la filiale a 046 e autorizzazione AZ
053600131125         If  $logistica = 'LOG' ;
053700131125             eval dutpou = 046 ;
053800131125             eval otaa1cabi = 'AZ' ;
053900131125         Endif ;
054000090701
054100081020         clear trul31ds  ;
054200090701         i31abi=otaa1cabi;
054300081020         I31cdi = DUTdis   ;
054400081020         I31car = DUTare   ;
054500081020         I31cpo = DUTpou   ;
054600081020         callp TRUL31R (kpjba:trul31ds)   ;
054700081020         if o31pog<=*zeros  ;
054800081020           $ErrGrave   = *on;
054900081020         endif;
055000090702         endif;
055100081020
055200080207
055300080207         // Intabellamento filiali e relative aree
055400080207         setll *loval AZORG;
055500080207         read  AZORG;
055600080208         dow  NOT  %eof(AZORG01L);
055700080208           if  ORGfva = *blank;
055800080207             xx += 1;
055900080208             $Fil(xx) = ORGfil;
056000080208             $Car(xx) = ORGcar;
056100080207           endif;
056200080207           read  AZORG;
056300080207         enddo;
056400110308
056500110308         //?Carico sk Categoria merceologica
056600110308         clear xx;
056700110308         TBLcod = '1L';
056800110308         setll (TBLkut:TBLcod) TABEL00F;
056900110308         reade (TBLkut:TBLcod) TABEL00F;
057000110308
057100110308         DOW not %eof(TABEL00F);
057200110308           IF  TBLflg = *blanks;
057300110308             xx += 1;
057400110309             $1l(xx) = %subst(TBLkey:1:5);
057500110308           ENDIF;
057600110308           reade (TBLkut:TBLcod) TABEL00F;
057700110308         ENDDO;
057800110308
057900110308         //?Carico sk Importanza Clienti
058000110308         clear xx;
058100110309         TBLcod = 'IC';
058200110308         setll (TBLkut:TBLcod) TABEL00F;
058300110308         reade (TBLkut:TBLcod) TABEL00F;
058400110308
058500110308         DOW not %eof(TABEL00F);
058600110308           IF  TBLflg = *blanks;
058700110308             xx += 1;
058800110309             $ic(xx) = %subst(TBLkey:1:1);
058900110308           ENDIF;
059000110308           reade (TBLkut:TBLcod) TABEL00F;
059100110308         ENDDO;
059200110407
059300110407         //?Carico sk Categoria potenziali
059400110407         clear xx;
059500110407         clear yy;
059600110407         C_Tab = 'CPO';
059700110407         setll (C_tab) TNTBE01L;
059800110407         reade (C_tab) TNTBE01L;
059900110407
060000110407         DOW not %eof(TNTBE01L);
060100110407           IF  TBEatb = *blanks;
060200110407             dCPO = TBEuni;
060300110407             xx += 1;
060400110407             $Cpo(xx) = %subst(TBEke1:1:1);
060500110407             IF  §CPOint = 'S';
060600110407               yy += 1;
060700110407               $CpoI(yy) = $Cpo(xx);
060800110407             ENDIF;
060900110407           ENDIF;
061000110407           reade (C_tab) TNTBE01L;
061100110407         ENDDO;
061200110308
061300110308         //?Carico sk Concorrenti
061400110308         clear xx;
061500110308         C_Tab = 'IFS';
061600110309         C_ke1 = '10';
061700110309         setll (C_tab:C_ke1) TNTBE01L;
061800110309         reade (C_tab:C_ke1) TNTBE01L;
061900110308
062000110308         DOW not %eof(TNTBE01L);
062100110308           IF  TBEatb = *blanks;
062200110308             xx += 1;
062300110308             $Ifs(xx) = %subst(TBEke2:1:4);
062400110308           ENDIF;
062500110309           reade (C_tab:C_ke1) TNTBE01L;
062600110308         ENDDO;
062700131122
062800131122         //?Carico sk Concorrenti LOGISTICA
062900131122         clear xx;
063000131122         C_Tab = 'IFS';
063100131122         C_ke1 = '40';
063200131122         setll (C_tab:C_ke1) TNTBE01L;
063300131122         reade (C_tab:C_ke1) TNTBE01L;
063400131122
063500131122         DOW not %eof(TNTBE01L);
063600131122           IF  TBEatb = *blanks;
063700131122             xx += 1;
063800131122             $IfsLOG(xx) = %subst(TBEke2:1:4);
063900131122           ENDIF;
064000131122           reade (C_tab:C_ke1) TNTBE01L;
064100131122         ENDDO;
064200140910
064300140910         //?Carico sk Paesi Estero
064400140910         clear xx;
064500140910         C_Tab = 'IFS';
064600140910         C_ke1 = '50';
064700140910         setll (C_tab:C_ke1) TNTBE01L;
064800140910         reade (C_tab:C_ke1) TNTBE01L;
064900140910
065000140910         DOW not %eof(TNTBE01L);
065100140910           IF  TBEatb = *blanks;
065200140910             xx += 1;
065300140910             $IfsPAESI(xx) = %subst(TBEke2:1:4);
065400140910           ENDIF;
065500140910           reade (C_tab:C_ke1) TNTBE01L;
065600140910         ENDDO;
065700100423
065800100423          dataiso=datasys         ;
065900100423          w0080=%dec(dataiso)     ;
066000100423          Alfaoggi=%editc(w0080:'X')  ;
066100080206
066200080206       ENDSR;
066300080206
066400080206       //--------------------------------------------------------------
066500080206       //?Reperimento Dati del job (Utente/Operativi).
066600080206       //--------------------------------------------------------------
066700080206       BEGSR DatiJob;
066800080206
066900080206         in(E) §AzUte;
067000080206         if NOT %error;
067100080206           in(E) §DatiUte;
067200080206         endif;
067300080206         if %error or RSut = *blanks;
067400080206           clear TIBS34ds;
067500080206           tibs34r(tibs34ds);
067600080206           in §AzUte;
067700080206           in §DatiUte;
067800080206         endif;
067900080206
068000080206       ENDSR;
068100080206
068200080206       //--------------------------------------------------------------
068300080206       //?Gestione videata D01
068400080206       //--------------------------------------------------------------
068500080206       BEGSR GesD01;
068600080206
068700080206         // Inizializzazione videata
068800080206         if $InzD01 = *on;
068900080206           exsr InzD01;
069000080206           $InzD01 = *off;
069100080214         else;
069200080225           if  errGenerico = *off;
069300110308             clear V01cpo;
069400080225           endif;
069500080206         endif;
069600080206
069700080206         // Emissione testata
069800091027           write MK01T01;
069900090702
070000090702         // Se errore grave, ovvero utente non abilitato, emetto msg ed esco dal pgm
070100090702         if $ErrGrave=*on    ;
070200090702           errMessage  = *on;
070300090702           errGenerico = *on;
070400090702           V1Dmsg = $Msg(01);
070500090702         endif   ;
070600080206
070700080206         // Emissione videata
070800091027         exfmt MK01D01;
070900080206         errMessage   = *off;
071000080206         errGenerico  = *off;
071100080206         clear V1Dmsg;
071200080207
071300080206         select;
071400080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
071500080206           when  $ErrGrave = *on;
071600091028             Mk01f03 = *on;
071700080206             $Fine  = *on;
071800110308
071900080206         // F3=Fine
072000080208           when dsp_aid = c_F03;
072100080206             exsr F03D01;
072200110308
072300131122         // F5=Altre Prazializzazioni
072400131122           when dsp_aid = c_F05;
072500131122             exsr CtrD01;
072600131122             if errGenerico;
072700131122               leavesr;
072800131122             endif;
072900131122             exsr F05D01;
073000131122
073100080206         // F10=Immissione
073200080207           when dsp_aid = c_F10;
073300080206             exsr F10D01;
073400110308
073500131122         // F11=Pulizia
073600131122           when dsp_aid = c_F11;
073700131122             exsr F11D01;
073800131122
073900081021         // F14=normalizzazione
074000081021           when dsp_aid = c_F14;
074100081021             exsr F14S01;
074200081021
074300080206         // Enter
074400080206           other;
074500080206             exsr CtrD01;
074600080206             if errGenerico;
074700080206               leavesr;
074800080206             endif;
074900110308
075000080208             if  $Ord = 1;
075100080208               $Video = 'WW';
075200080208             else;
075300080208               $Video = 'S1';
075400080208             endif;
075500110308
075600080206             $InzS01 = *on;
075700080206         endsl;
075800080206
075900080206       ENDSR;
076000080206
076100080206       //--------------------------------------------------------------
076200080206       //?Inizializzazione videata D01
076300080206       //--------------------------------------------------------------
076400080206       BEGSR InzD01;
076500080206
076600091027         clear MK01D01;
076700080206
076800080206         // Si è in "Gestione clienti potenziali" SE
076900091030         // - pgm. NON richiamato da altro pgm.  & azione MK02
077000080206         // (altrimenti si è in "Visualizzazione clienti potenziali")
077100091030         GESclipot = (Mk01k11 = *blank  and  kcdaz = 'MK02');
077200080206
077300080206         // Abilitazione tasto funzionale F10=Immissione
077400091028         F10Attivo = (Mk01imm = 'S'   or   GesCliPot = *on);
077500081020
077600091028         PGMrichia=Mk01k11<>' '    ;
077700080208
077800080206         // Se richiamato e passata, imposto la ricerca alfabetica
077900080206         select;
078000091028           when  Mk01k11 =  *blank;
078100091028             clear Mk01rag;
078200091028           when  Mk01k11 <> *blank   and
078300091028                 Mk01k11 <> '2';
078400110308             V01rag = Mk01rag;
078500080206         endsl;
078600080208
078700110520         //?Se da menù imposto ordinamento per ragione sociale
078800110520         IF  w_damenu <> *blanks;
078900110520           V01osf  = 'R';
079000110520         //?Se da F7 imposto ordinamento per importanza cliente
079100110520         ELSE;
079200110520           V01osf  = 'I';
079300110520         ENDIF;
079400131126         //?Se RICHIAMATO DALLA LOGISTICA  importanza cliente
079500131126         If $logistica = 'LOG';
079600131126           V01osf  = 'I';
079700131126         ENDIF;
079800110407
079900110407         //?Imposto le categorie potenziale da selezionare di default
080000110601         //?Mai codificato - Perso  (recupero da tab. CPO)
080100131127         //?Solo se non è da menù
080200131127         IF  w_damenu = *blanks ;
080300110518           V01fls1 = $CpoI(1);
080400110518           V01fls2 = $CpoI(2);
080500110518           V01fls3 = $CpoI(3);
080600110518           V01fls4 = $CpoI(4);
080700110518           V01fls5 = $CpoI(5);
080800110518         ENDIF;
080900080206
081000080206       ENDSR;
081100080206
081200131125       //--------------------------------------------------------------
081300131125       //?Gestione videata D02
081400131125       //--------------------------------------------------------------
081500131125       BEGSR GesD02;
081600131125
081700131125         // Inizializzazione videata
081800131125         if $InzD02 = *on;
081900131125           exsr InzD02;
082000131125           $InzD02 = *off;
082100131125         endif;
082200131125
082300131125         // Emissione testata
082400131125           write MK01T01;
082500131125
082600131125         // Emissione videata
082700131125         exfmt MK01D02;
082800131125         errMessage   = *off;
082900131125         errGenerico  = *off;
083000131125         clear V2Dmsg;
083100131125
083200131125         select;
083300131125         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
083400131125           when  $ErrGrave = *on;
083500131125             Mk01f03 = *on;
083600131125             $Fine  = *on;
083700131125
083800131127         // Enter
083900131127           other;
084000131127             exsr CtrD02;
084100131127             if errGenerico;
084200131127               leavesr;
084300131127             endif;
084400131127
084500131127             $Video = 'D1';
084600131125             $InzS01 = *on;
084700131127
084800131125         endsl;
084900131125
085000131125       ENDSR;
085100131125
085200131125       //--------------------------------------------------------------
085300131125       //?Inizializzazione videata D02
085400131125       //--------------------------------------------------------------
085500131125       BEGSR InzD02;
085600131125
085700131125         clear MK01D02;
085800131127         clear wdil1;
085900131127         clear wdil2;
086000131125
086100131125       ENDSR;
086200131125
086300080206       //--------------------------------------------------------------
086400080206       //?Gestione tasto funzionale F3 da videata D01
086500080206       //?F3=Fine
086600080206       //--------------------------------------------------------------
086700080206       BEGSR F03D01;
086800080206
086900080207         // Chiusura del programma
087000080206         $Fine = *on;
087100080206
087200080206         // Restituzione indicazione del tasto funzionale premuto
087300091028         Mk01f03 = *on;
087400080206
087500080206       ENDSR;
087600080206
087700131122       //--------------------------------------------------------------
087800131122       //?Gestione tasto funzionale F5 da videata D01
087900131122       //?F5=Altre parzializzazioni
088000131122       //--------------------------------------------------------------
088100131122       BEGSR F05D01;
088200131122
088300131122       //?Videata sucessiva
088400131125       //?Se l'infdicatore di altre parzializzazioni è spento pulisco il viedo altrimenti no
088500131125       If altreparz = *off ;
088600131122         $InzD02 = *on;
088700131125       ENDIF ;
088800131122
088900131125         $Video = 'D2';
089000131125
089100131122       ENDSR;
089200131122
089300080206       //--------------------------------------------------------------
089400080206       //?Gestione tasto funzionale F10 da videata D01
089500080206       //?F10=Immissione
089600080206       //--------------------------------------------------------------
089700080206       BEGSR F10D01;
089800080206
089900091028         Mk01k10 = 'I';
090000100202         kpjbu  = w_damenu;
090100091027         trmk02r(kpjba : trmk01ds);
090200080206
090300080206         select;
090400080206         // Se premuto F12 => si riemette la videata di selezione
090500091028           when  Mk01f03 = *on  or  Mk01f12 = *on;
090600080206             leavesr;
090700080206         // Se premuto F6 (quindi immesso il potenziale) => si emette
090800080206         //   il subfile in ordine di ragione sociale
090900091028           when  Mk01f06 = *on;
091000110309             V01rag   = Mk01rag;
091100080206         endsl;
091200080206
091300080206         // Se restituito errore => lo si visualizza
091400091028         if  Mk01err = 'E';
091500080206           errMessage  = *on;
091600080206           errGenerico = *on;
091700091028           V1Dmsg = Mk01m10;
091800110309           V01cpo = %editc(Mk01cpo : 'X');
091900080206           leavesr;
092000080206         endif;
092100080206
092200080206       ENDSR;
092300080206
092400131122       //--------------------------------------------------------------
092500131122       //?Gestione tasto funzionale F11 da videata D01
092600131122       //?F11=Pulizia
092700131122       //--------------------------------------------------------------
092800131122       BEGSR F11D01;
092900131122
093000131122       //?Pulizia delle videate 1 e 2
093100131122         $Video = 'D1';
093200131127         exsr Inzd01;
093300131127         exsr InzD02;
093400131122
093500131125         altreparz = *off;
093600131122
093700131122       ENDSR;
093800131125
093900080206       //--------------------------------------------------------------
094000080206       //?Controllo videata D01
094100080206       //--------------------------------------------------------------
094200080206       BEGSR CtrD01;
094300080206
094400080206         WindDspF = IndDspF;
094500080206         reset WindDspFb;
094600080206         IndDspF  = WindDspF;
094700110309
094800110309       //?Ordinamento Elenco
094900110309         IF  V01osf <> 'R'   and
095000110309             V01osf <> 'I';
095100110309           ErrMessage  = *on;
095200110309           ErrGenerico = *on;
095300110309           PosCurOSF   = *on;
095400110309           V1Dmsg = $Msg(07);
095500110309           leavesr;
095600110309         ENDIF;
095700080206
095800080206         SELECT;
095900110309
096000110309         //?Se inserita ragione sociale no ordinamento per importanza cliente
096100110601         //?per ora lo tolgo
096200110309           WHEN  V01rag <> *blanks and V01osf <> 'R';
096300110520             //ErrMessage  = *on;
096400110520             //ErrGenerico = *on;
096500110520             //PosCurOsf   = *on;
096600110520             //V1Dmsg      = $Msg(18);
096700110520             //leavesr;
096800110308
096900110308         //?Se inseriti dati info commerciali obbligo 1 dato delle parzializzazioni
097000140910          //WHEN   (V01treda > *zeros or V01trea > *zeros or
097100140910          //        V01pkgda > *zeros or V01pkga > *zeros or
097200140910          //        V01she <> *blanks or V01conc <> *blanks or
097300140910          //        V01sbx <> *blanks)  and
097400140910          //        V01fls = *blanks and V01pst = *blanks and
097500140910          //        V01rag = *blanks and V01loc = *blanks and
097600140910          //        V01prv = *blanks and V01cap = *blanks and
097700140910          //        V01flt = *zeros  and V01car = *blanks and
097800140910          //        V01piv = *blanks and V01cdf = *blanks and
097900140910          //        V01sct = *blanks and V01cic = *blanks;
098000140910          //  ErrMessage  = *on;
098100140910          //  ErrGenerico = *on;
098200140910          //  V1Dmsg      = $Msg(04);
098300140910          //  leavesr;
098400110601
098500110601         //?Se inserita selezione per trattativa obbligo 1 dato delle parzializzazioni
098600110601           WHEN   V01ttr <> *blanks and
098700110601                   V01fls = *blanks and V01pst = *blanks and
098800110601                   V01rag = *blanks and V01loc = *blanks and
098900110601                   V01prv = *blanks and V01cap = *blanks and
099000110601                   V01flt = *zeros  and V01car = *blanks and
099100110601                   V01piv = *blanks and V01cdf = *blanks and
099200140910                   V01sct = *blanks and V01cic = *blanks and
099300140910                   V01pexp = *zeros and V01anno = *zeros;
099400110601             ErrMessage  = *on;
099500110601             ErrGenerico = *on;
099600110616             V1Dmsg      = $Msg(12);
099700110601             leavesr;
099800110601         ENDSL;
099900110308
100000110308       //?Importanza Cliente
100100110308         IF  V01cic1 <> *blanks;
100200110308           IF    %scan('?' : V01cic1) > 0;
100300110308             ErrGenerico = *on;
100400110308             PosCurCic1  = *on;
100500110308             idTabella = 'IC';
100600110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
100700110308                            tastoUscita);
100800110309             V01cic1 = idElemento;
100900110308           ENDIF;
101000110308           IF  %lookup(V01cic1 : $Ic) = 0;
101100110308             ErrMessage  = *on;
101200110308             ErrGenerico = *on;
101300110308             PosCurCic1  = *on;
101400110309             V1Dmsg = $Msg(17);
101500110308             leavesr;
101600110308           ENDIF;
101700110308         ENDIF;
101800110308         IF  V01cic2 <> *blanks;
101900110308           IF    %scan('?' : V01cic2) > 0;
102000110308             ErrGenerico = *on;
102100110308             PosCurCic2  = *on;
102200110308             idTabella = 'IC';
102300110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
102400110308                            tastoUscita);
102500110308             V01cic2 = idElemento;
102600110308           ENDIF;
102700110308           IF  %lookup(V01cic2 : $Ic) = 0;
102800110308             ErrMessage  = *on;
102900110308             ErrGenerico = *on;
103000110308             PosCurCic2  = *on;
103100110309             V1Dmsg = $Msg(17);
103200110308             leavesr;
103300110308           ENDIF;
103400110308         ENDIF;
103500110308         IF  V01cic3 <> *blanks;
103600110308           IF    %scan('?' : V01cic3) > 0;
103700110308             ErrGenerico = *on;
103800110308             PosCurCic3  = *on;
103900110308             idTabella = 'IC';
104000110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
104100110308                            tastoUscita);
104200110308             V01cic3 = idElemento;
104300110308           ENDIF;
104400110308           IF  %lookup(V01cic3 : $Ic) = 0;
104500110308             ErrMessage  = *on;
104600110308             ErrGenerico = *on;
104700110308             PosCurCic3  = *on;
104800110309             V1Dmsg = $Msg(17);
104900110308             leavesr;
105000110308           ENDIF;
105100110308         ENDIF;
105200110308         IF  V01cic4 <> *blanks;
105300110308           IF    %scan('?' : V01cic4) > 0;
105400110308             ErrGenerico = *on;
105500110308             PosCurCic4  = *on;
105600110308             idTabella = 'IC';
105700110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
105800110308                            tastoUscita);
105900110308             V01cic4 = idElemento;
106000110308           ENDIF;
106100110308           IF  %lookup(V01cic4 : $Ic) = 0;
106200110308             ErrMessage  = *on;
106300110308             ErrGenerico = *on;
106400110308             PosCurCic4  = *on;
106500110309             V1Dmsg = $Msg(17);
106600110308             leavesr;
106700110308           ENDIF;
106800110308         ENDIF;
106900110308         IF  V01cic5 <> *blanks;
107000110308           IF    %scan('?' : V01cic5) > 0;
107100110308             ErrGenerico = *on;
107200110308             PosCurCic5  = *on;
107300110308             idTabella = 'IC';
107400110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
107500110308                            tastoUscita);
107600110308             V01cic5 = idElemento;
107700110308           ENDIF;
107800110308           IF  %lookup(V01cic5 : $Ic) = 0;
107900110308             ErrMessage  = *on;
108000110308             ErrGenerico = *on;
108100110308             PosCurCic5  = *on;
108200110309             V1Dmsg = $Msg(17);
108300110308             leavesr;
108400110308           ENDIF;
108500110308         ENDIF;
108600110322         IF  V01cic <> *blanks;
108700110322           clear $cic;
108800110322           $cic(01) = V01cic1;
108900110322           $cic(02) = V01cic2;
109000110322           $cic(03) = V01cic3;
109100110322           $cic(04) = V01cic4;
109200110322           $cic(05) = V01cic5;
109300110322         ENDIF;
109400110308
109500110308         //?Categoria potenziale
109600110407         IF  V01fls1 <> *blanks;
109700110407           IF  %scan('?' : V01fls1) > 0;
109800110308             ErrGenerico = *on;
109900110407             PosCurFls1  = *on;
110000110509             clear TNTB87ds;
110100110509             clear V01fls1;
110200110509             tntb87r  (tntb87ds);
110300110509             IF  TB87fxx = *blanks and TB87err = *off;
110400110509               V01fls1 = TB87cod;
110500110308             ENDIF;
110600110308           ENDIF;
110700110407           IF  %lookup(V01fls1 : $Cpo) = 0;
110800110314             ErrMessage  = *on;
110900110314             ErrGenerico = *on;
111000110407             PosCurFls1  = *on;
111100110314             V1Dmsg = $Msg(24);
111200110314             leavesr;
111300110308           ENDIF;
111400110308         ENDIF;
111500110407         IF  V01fls2 <> *blanks;
111600110407           IF  %scan('?' : V01fls2) > 0;
111700110407             ErrGenerico = *on;
111800110407             PosCurFls2  = *on;
111900110509             clear TNTB87ds;
112000110509             clear V01fls2;
112100110509             tntb87r  (tntb87ds);
112200110509             IF  TB87fxx = *blanks and TB87err = *off;
112300110509               V01fls2 = TB87cod;
112400110509             ENDIF;
112500110407           ENDIF;
112600110407           IF  %lookup(V01fls2 : $Cpo) = 0;
112700110407             ErrMessage  = *on;
112800110407             ErrGenerico = *on;
112900110407             PosCurFls2  = *on;
113000110407             V1Dmsg = $Msg(24);
113100110407             leavesr;
113200110407           ENDIF;
113300110407         ENDIF;
113400110407         IF  V01fls3 <> *blanks;
113500110407           IF  %scan('?' : V01fls3) > 0;
113600110407             ErrGenerico = *on;
113700110407             PosCurFls3  = *on;
113800110509             clear TNTB87ds;
113900110509             clear V01fls3;
114000110509             tntb87r  (tntb87ds);
114100110509             IF  TB87fxx = *blanks and TB87err = *off;
114200110509               V01fls3 = TB87cod;
114300110509             ENDIF;
114400110407           ENDIF;
114500110407           IF  %lookup(V01fls3 : $Cpo) = 0;
114600110407             ErrMessage  = *on;
114700110407             ErrGenerico = *on;
114800110407             PosCurFls3  = *on;
114900110407             V1Dmsg = $Msg(24);
115000110407             leavesr;
115100110407           ENDIF;
115200110407         ENDIF;
115300110407         IF  V01fls4 <> *blanks;
115400110407           IF  %scan('?' : V01fls4) > 0;
115500110407             ErrGenerico = *on;
115600110407             PosCurFls4  = *on;
115700110509             clear TNTB87ds;
115800110509             clear V01fls4;
115900110509             tntb87r  (tntb87ds);
116000110509             IF  TB87fxx = *blanks and TB87err = *off;
116100110509               V01fls4 = TB87cod;
116200110509             ENDIF;
116300110407           ENDIF;
116400110407           IF  %lookup(V01fls4 : $Cpo) = 0;
116500110407             ErrMessage  = *on;
116600110407             ErrGenerico = *on;
116700110407             PosCurFls4  = *on;
116800110407             V1Dmsg = $Msg(24);
116900110407             leavesr;
117000110407           ENDIF;
117100110407         ENDIF;
117200110407         IF  V01fls5 <> *blanks;
117300110407           IF  %scan('?' : V01fls5) > 0;
117400110407             ErrGenerico = *on;
117500110407             PosCurFls5  = *on;
117600110509             clear TNTB87ds;
117700110509             clear V01fls5;
117800110509             tntb87r  (tntb87ds);
117900110509             IF  TB87fxx = *blanks and TB87err = *off;
118000110509               V01fls5 = TB87cod;
118100110509             ENDIF;
118200110407           ENDIF;
118300110407           IF  %lookup(V01fls5 : $Cpo) = 0;
118400110407             ErrMessage  = *on;
118500110407             ErrGenerico = *on;
118600110407             PosCurFls5  = *on;
118700110407             V1Dmsg = $Msg(24);
118800110407             leavesr;
118900110407           ENDIF;
119000110407         ENDIF;
119100110407         IF  V01fls <> *blanks;
119200110407           clear $fls;
119300110407           $fls(01) = V01fls1;
119400110407           $fls(02) = V01fls2;
119500110407           $fls(03) = V01fls3;
119600110407           $fls(04) = V01fls4;
119700110407           $fls(05) = V01fls5;
119800110407         ENDIF;
119900110308
120000110308         // Se inserito il cap o la località è obbligatoria la provincia
120100110308           IF    (V01cap <> *blank  or  V01loc <> *blanks)  and
120200110308                  V01prv  = *blank;
120300110308             errMessage  = *on;
120400110308             errGenerico = *on;
120500110308             PosCurPRV   = *on;
120600110308             V1Dmsg      = $Msg(03);
120700110308             leavesr;
120800110308           ENDIF;
120900080206
121000080206         // Se inserito cap e provincia controllo se sono congruenti
121100110308           if  V01cap <> *blank;
121200080206             reset  fnlv13ds;
121300080206             reset  tisi95ds;
121400110309             I95cap = V01cap;
121500110309             I95prv = V01prv;
121600080206             fnlv13r(kpjba : fnlv13ds : tisi95ds);
121700080206             if  O13err <> *blank;
121800080206               errMessage  = *on;
121900080206               errGenerico = *on;
122000080206               select;
122100080206                 when  O13err = '4';
122200080206                   PosCurPRV   = *on;
122300080206                 when  O13err = '5';
122400080206                   PosCurCAP   = *on;
122500080206               endsl;
122600080206               V1Dmsg = O13msg;
122700080206               leavesr;
122800080206             endif;
122900080206           endif;
123000110308
123100110308         //?Filiale appartenenza
123200110308         IF  V01flt > 0;
123300110308           chain V01flt AZORG01L;
123400110308           IF  not %found(AZORG01L) or ORGfva <> *blanks;
123500110308             ErrMessage  = *on;
123600110308             ErrGenerico = *on;
123700110308             PosCurFlt   = *on;
123800110308             V1Dmsg      = $Msg(16);
123900110308             leavesr;
124000110308           ENDIF;
124100110308         ENDIF;
124200110308
124300110308         // Ricerca/Controllo/Decodifica codice area
124400110308         clear  V01carD;
124500110308         TBLcod = '05';
124600110308         TBLkey = V01car;
124700110308         if  V01car = *zero;
124800110308           clear  V01car;
124900110308         endif;
125000110308         SELECT;
125100110308         // - Ricerca
125200110308           WHEN  %scan('?' : V01car) > *zero;
125300110308             errGenerico = *on;
125400110308             PosCurCAR  = *on;
125500110308             clear w030a;
125600110308             x§taber(TBLkut : TBLcod : TBLkey : w030a);
125700110308         // - - F3 invece di scelta codice
125800110308             if  TBLkey = *blank;
125900110308               clear  V01car;
126000110308         // - - effettuata scelta
126100110308             else;
126200110308               V01car   = TBLkey;
126300110309               V01carD  = %subst(w030a : 1 : %len(V01carD));
126400110308             endif;
126500110308             leavesr;
126600110308         // - Controllo validità del codice area
126700110308           WHEN  V01car <> *blank;
126800110308             clear  ds05;
126900110308             chain (TBLkut : TBLcod : TBLkey) TABEL;
127000110308             if  NOT  %found(TABEL00F)
127100110308                 or   TBLflg <> *blank;
127200110308               errMessage  = *on;
127300110308               errGenerico = *on;
127400110308               PosCurCAR   = *on;
127500110308               V1Dmsg = $Msg(06);
127600110308               leavesr;
127700110308             else;
127800110308               ds05    = TBLuni;
127900110308               V01carD = §05des;
128000110308             endif;
128100110308         ENDSL;
128200050519
128300110308       //?Categoria merceologica
128400110615         IF  V01sct1 <> *blanks;
128500110308           IF    %scan('?' : V01sct1) > 0;
128600110308             ErrGenerico = *on;
128700110308             PosCurSct1  = *on;
128800110309             idTabella = '1L';
128900110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
129000110308                            tastoUscita);
129100110308             V01sct1 = idElemento;
129200110308           ENDIF;
129300110615           IF  V01sct1 <> *zeros;
129400110615             IF  %lookup(V01sct1 : $1l) = 0;
129500110615               ErrMessage  = *on;
129600110615               ErrGenerico = *on;
129700110615               PosCurSct1  = *on;
129800110615               V1Dmsg = $Msg(05);
129900110615               leavesr;
130000110615             ENDIF;
130100110308           ENDIF;
130200110308         ENDIF;
130300110308         IF  V01sct2 <> *blanks;
130400110308           IF    %scan('?' : V01sct2) > 0;
130500110308             ErrGenerico = *on;
130600110308             PosCurSct2  = *on;
130700110309             idTabella = '1L';
130800110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
130900110308                            tastoUscita);
131000110308             V01sct2 = idElemento;
131100110308           ENDIF;
131200110615           IF  V01sct2 <> *zeros;
131300110615             IF  %lookup(V01sct2 : $1l) = 0;
131400110615               ErrMessage  = *on;
131500110615               ErrGenerico = *on;
131600110615               PosCurSct2  = *on;
131700110615               V1Dmsg = $Msg(05);
131800110615               leavesr;
131900110615             ENDIF;
132000110308           ENDIF;
132100110308         ENDIF;
132200110308         IF  V01sct3 <> *blanks;
132300110308           IF    %scan('?' : V01sct3) > 0;
132400110308             ErrGenerico = *on;
132500110308             PosCurSct3  = *on;
132600110309             idTabella = '1L';
132700110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
132800110308                            tastoUscita);
132900110308             V01sct3 = idElemento;
133000110308           ENDIF;
133100110615           IF  V01sct3 <> *zeros;
133200110615             IF  %lookup(V01sct3 : $1l) = 0;
133300110615               ErrMessage  = *on;
133400110615               ErrGenerico = *on;
133500110615               PosCurSct3  = *on;
133600110615               V1Dmsg = $Msg(05);
133700110615               leavesr;
133800110615             ENDIF;
133900110308           ENDIF;
134000110308         ENDIF;
134100140910
134200140910       //?% Fatturato EXPORT
134300140910         IF  V01pexpda <> 0 or V01pexpa <> 0;
134400140910         //?Obbligatori tutti e 2 se immesso uno dei due
134500140910           IF  V01pexpda <> 0  and  V01pexpa = 0;
134600140910             ErrMessage  = *on;
134700140910             ErrGenerico = *on;
134800140910             PosCurPexpa = *on;
134900140910             V1Dmsg      = $Msg(19);
135000140910             leavesr;
135100140910           ENDIF;
135200140910           IF  V01pexpda = 0  and  V01pexpa <> 0;
135300140910             ErrMessage  = *on;
135400140910             ErrGenerico = *on;
135500140910             PosCurPexpda= *on;
135600140910             V1Dmsg      = $Msg(19);
135700140910             leavesr;
135800140910           ENDIF;
135900140910         //?Controllo congruenza tra "DA" e "A"
136000140910           IF  V01pexpda > V01pexpa;
136100140910             ErrMessage  = *on;
136200140910             ErrGenerico = *on;
136300140910             PosCurPexpda= *on;
136400140910             V1Dmsg      = $Msg(20);
136500140910             leavesr;
136600140910           ENDIF;
136700140910         ENDIF;
136800140910
136900140910       //?Anno Inizio Attività
137000140910         IF  V01annoda <> 0 or V01annoa <> 0;
137100140910         //?Obbligatori tutti e 2 se immesso uno dei due
137200140910           IF  V01annoda <> 0  and  V01annoa = 0;
137300140910             ErrMessage  = *on;
137400140910             ErrGenerico = *on;
137500140910             PosCurAnnoa = *on;
137600140910             V1Dmsg      = $Msg(13);
137700140910             leavesr;
137800140910           ENDIF;
137900140910           IF  V01annoda = 0  and  V01annoa <> 0;
138000140910             ErrMessage  = *on;
138100140910             ErrGenerico = *on;
138200140910             PosCurAnnoda= *on;
138300140910             V1Dmsg      = $Msg(13);
138400140910             leavesr;
138500140910           ENDIF;
138600140910         //?Controllo congruenza tra "DA" e "A"
138700140910           IF  V01annoda > V01annoa;
138800140910             ErrMessage  = *on;
138900140910             ErrGenerico = *on;
139000140910             PosCurAnnoda= *on;
139100140910             V1Dmsg      = $Msg(14);
139200140910             leavesr;
139300140910           ENDIF;
139400140910         ENDIF;
139500080213
139600080206         // Imposta il tipo selezioni da eseguire:
139700110310
139800080207         clear $Ord;
139900080207         select;
140000080207           // 1 = Singolo codice cliente potenziale
140100110308           when  V01cpo <> *zero;
140200080206             $Ord   = 1;
140300110520
140400110520           // 2 = per Ragione Sociale se richiesto ordinamento x Ragione sociale
140500110520           when  V01osf = 'R';
140600110520             $Ord   = 2;
140700110309
140800110309           // 3 = per Categoria merceologica
140900110309           when  V01sct  > *zero;
141000110309             $Ord   = 3;
141100110309
141200110309           // 4 = per Provincia
141300110309           when  V01prv <> *blank;
141400110309             $Ord   = 4;
141500110309
141600110309           // 5 = per Codice fiscale
141700110309           when  V01cdf <> *blank;
141800110309             $Ord   = 5;
141900110309
142000110309           // 6 = per Partita iva
142100110309           when  V01piv <> *blank;
142200110309             $Ord   = 6;
142300110309
142400110309           // 7 = per Importanza cliente
142500110309           when  V01cic <> *blank;
142600110309             $Ord   = 7;
142700110309
142800110309           // 8 = per Filiale o Area
142900110309           when  V01flt > *zeros or V01car <> *blanks;
143000110309             $Ord   = 8;
143100110520
143200110520           // 9 = per Categoria Potenziale
143300110520           when  V01fls <> *blank;
143400110520             $Ord   = 9;
143500110309
143600110309           // 10 = per Potenziali mai contattati
143700110310           //      imposto $ord solo per proforma
143800110309           when  V01pst <> *blank;
143900110309             $Ord   = 10;
144000140910
144100140910           // 11 = per % Fatturato Export
144200140910           when  V01pexp > *zeros;
144300140910             $Ord   = 11;
144400140910
144500140910           // 12 = per Anno Inizio Attività
144600140910           when  V01anno > *zeros;
144700140910             $Ord   = 12;
144800110520
144900080206         endsl;
145000110518
145100110601       //?Se richiesto codice potenziale
145200110518       //?finisco qua i controlli
145300140910        // IF  V01cpo > *zeros;
145400140910        //   leavesr;
145500140910        // ENDIF;
145600110310
145700080206
145800080206       ENDSR;
145900131125
146000131125       //--------------------------------------------------------------
146100131125       //?Controllo videata D02
146200131125       //--------------------------------------------------------------
146300131125       BEGSR CtrD02;
146400131125
146500131125         WindDspF = IndDspF;
146600131125         reset WindDspFb;
146700131125         IndDspF  = WindDspF;
146800140910
146900140910       //?Controllo parzializzazioni INFO
147000140910       //?No affido a BRT Italia Parcel
147100140910         IF  V02nrfda <> 0 or V02nrfa <> 0;
147200140910         //?Obbligatori tutti e 2 se immesso uno dei due
147300140910           IF  V02nrfda <> 0  and  V02nrfa = 0;
147400140910             ErrMessage  = *on;
147500140910             ErrGenerico = *on;
147600140910             PosCurNrfa  = *on;
147700140910             V2Dmsg      = $Msg(32);
147800140910             leavesr;
147900140910           ENDIF;
148000140910           IF  V02nrfda = 0  and  V02nrfa <> 0;
148100140910             ErrMessage  = *on;
148200140910             ErrGenerico = *on;
148300140910             PosCurNrfda = *on;
148400140910             V2Dmsg      = $Msg(32);
148500140910             leavesr;
148600140910           ENDIF;
148700140910         //?Controllo congruenza tra "DA" e "A"
148800140910           IF  V02nrfda > V02nrfa;
148900140910             ErrMessage  = *on;
149000140910             ErrGenerico = *on;
149100140910             PosCurNrfda = *on;
149200140910             V2Dmsg      = $Msg(33);
149300140910             leavesr;
149400140910           ENDIF;
149500140910         ENDIF;
149600140910
149700140910       //?No affido a BRT Italia Messaggeria
149800140910         IF  V02nroda <> 0 or V02nroa <> 0;
149900140910         //?Obbligatori tutti e 2 se immesso uno dei due
150000140910           IF  V02nroda <> 0  and  V02nroa = 0;
150100140910             ErrMessage  = *on;
150200140910             ErrGenerico = *on;
150300140910             PosCurNroa  = *on;
150400140910             V2Dmsg      = $Msg(32);
150500140910             leavesr;
150600140910           ENDIF;
150700140910           IF  V02nroda = 0  and  V02nroa <> 0;
150800140910             ErrMessage  = *on;
150900140910             ErrGenerico = *on;
151000140910             PosCurNroda = *on;
151100140910             V2Dmsg      = $Msg(32);
151200140910             leavesr;
151300140910           ENDIF;
151400140910         //?Controllo congruenza tra "DA" e "A"
151500140910           IF  V02nroda > V02nroa;
151600140910             ErrMessage  = *on;
151700140910             ErrGenerico = *on;
151800140910             PosCurNroda = *on;
151900140910             V2Dmsg      = $Msg(33);
152000140910             leavesr;
152100140910           ENDIF;
152200140910         ENDIF;
152300140910
152400140910       //?No affido a BRT Estero Via Aerea
152500140910         IF  V02nerda <> 0 or V02nera <> 0;
152600140910         //?Obbligatori tutti e 2 se immesso uno dei due
152700140910           IF  V02nerda <> 0  and  V02nera = 0;
152800140910             ErrMessage  = *on;
152900140910             ErrGenerico = *on;
153000140910             PosCurNera  = *on;
153100140910             V2Dmsg      = $Msg(32);
153200140910             leavesr;
153300140910           ENDIF;
153400140910           IF  V02nerda = 0  and  V02nera <> 0;
153500140910             ErrMessage  = *on;
153600140910             ErrGenerico = *on;
153700140910             PosCurNerda = *on;
153800140910             V2Dmsg      = $Msg(32);
153900140910             leavesr;
154000140910           ENDIF;
154100140910         //?Controllo congruenza tra "DA" e "A"
154200140910           IF  V02nerda > V02nera;
154300140910             ErrMessage  = *on;
154400140910             ErrGenerico = *on;
154500140910             PosCurNerda = *on;
154600140910             V2Dmsg      = $Msg(33);
154700140910             leavesr;
154800140910           ENDIF;
154900140910         ENDIF;
155000140910
155100140910       //?No affido a BRT Estero Serv. Camionistico
155200140910         IF  V02naeda <> 0 or V02naea <> 0;
155300140910         //?Obbligatori tutti e 2 se immesso uno dei due
155400140910           IF  V02naeda <> 0  and  V02naea = 0;
155500140910             ErrMessage  = *on;
155600140910             ErrGenerico = *on;
155700140910             PosCurNaea  = *on;
155800140910             V2Dmsg      = $Msg(32);
155900140910             leavesr;
156000140910           ENDIF;
156100140910           IF  V02naeda = 0  and  V02naea <> 0;
156200140910             ErrMessage  = *on;
156300140910             ErrGenerico = *on;
156400140910             PosCurNaeda = *on;
156500140910             V2Dmsg      = $Msg(32);
156600140910             leavesr;
156700140910           ENDIF;
156800140910         //?Controllo congruenza tra "DA" e "A"
156900140910           IF  V02naeda > V02naea;
157000140910             ErrMessage  = *on;
157100140910             ErrGenerico = *on;
157200140910             PosCurNaeda = *on;
157300140910             V2Dmsg      = $Msg(33);
157400140910             leavesr;
157500140910           ENDIF;
157600140910         ENDIF;
157700140910
157800140910       //?No affido a BRT Altro (es. Completi, ADR)
157900140910         IF  V02ntrda <> 0 or V02ntra <> 0;
158000140910         //?Obbligatori tutti e 2 se immesso uno dei due
158100140910           IF  V02ntrda <> 0  and  V02ntra = 0;
158200140910             ErrMessage  = *on;
158300140910             ErrGenerico = *on;
158400140910             PosCurNtra  = *on;
158500140910             V2Dmsg      = $Msg(32);
158600140910             leavesr;
158700140910           ENDIF;
158800140910           IF  V02ntrda = 0  and  V02ntra <> 0;
158900140910             ErrMessage  = *on;
159000140910             ErrGenerico = *on;
159100140910             PosCurNtrda = *on;
159200140910             V2Dmsg      = $Msg(32);
159300140910             leavesr;
159400140910           ENDIF;
159500140910         //?Controllo congruenza tra "DA" e "A"
159600140910           IF  V02ntrda > V02ntra;
159700140910             ErrMessage  = *on;
159800140910             ErrGenerico = *on;
159900140910             PosCurNtrda = *on;
160000140910             V2Dmsg      = $Msg(33);
160100140910             leavesr;
160200140910           ENDIF;
160300140910         ENDIF;
160400140910
160500140910         //?Paesi Estero 1
160600140910         IF  V02paesi1 <> *blanks;
160700140910           IF    %scan('?' : V02paesi1) > 0;
160800140910             ErrGenerico  = *on;
160900140910             PosCurPaesi1 = *on;
161000140910             clear TIBS02ds;
161100140910             T02mod = 'R';
161200140910             T02cod = 'IFS';
161300140910             T02ke1 = '50';
161400140910             T02sif = KNSIF;
161500140910             TNTBE_RicercaControllo  (kpjba : tibs02ds);
161600140910             IF  T02err  = *blanks;
161700140910               V02paesi1 = t02ke2;
161800140910             ENDIF;
161900140910           ENDIF;
162000140910           IF  %lookup(V02paesi1 : $IfsPAESI) = 0;
162100140910             ErrMessage   = *on;
162200140910             ErrGenerico  = *on;
162300140910             PosCurPaesi1 = *on;
162400140910             V2Dmsg = $Msg(34);
162500140910             leavesr;
162600140910           ENDIF;
162700140910         ENDIF;
162800140910         //?Paesi Estero 2
162900140910         IF  V02paesi2 <> *blanks;
163000140910           IF    %scan('?' : V02paesi2) > 0;
163100140910             ErrGenerico  = *on;
163200140910             PosCurPaesi2 = *on;
163300140910             clear TIBS02ds;
163400140910             T02mod = 'R';
163500140910             T02cod = 'IFS';
163600140910             T02ke1 = '50';
163700140910             T02sif = KNSIF;
163800140910             TNTBE_RicercaControllo  (kpjba : tibs02ds);
163900140910             IF  T02err  = *blanks;
164000140910               V02paesi2 = t02ke2;
164100140910             ENDIF;
164200140910           ENDIF;
164300140910           IF  %lookup(V02paesi2 : $IfsPAESI) = 0;
164400140910             ErrMessage   = *on;
164500140910             ErrGenerico  = *on;
164600140910             PosCurPaesi2 = *on;
164700140910             V2Dmsg = $Msg(34);
164800140910             leavesr;
164900140910           ENDIF;
165000140910         ENDIF;
165100140910         //?Paesi Estero 3
165200140910         IF  V02paesi3 <> *blanks;
165300140910           IF    %scan('?' : V02paesi3) > 0;
165400140910             ErrGenerico  = *on;
165500140910             PosCurPaesi3 = *on;
165600140910             clear TIBS02ds;
165700140910             T02mod = 'R';
165800140910             T02cod = 'IFS';
165900140910             T02ke1 = '50';
166000140910             T02sif = KNSIF;
166100140910             TNTBE_RicercaControllo  (kpjba : tibs02ds);
166200140910             IF  T02err = *blanks;
166300140910               V02paesi3 = t02ke2;
166400140910             ENDIF;
166500140910           ENDIF;
166600140910           IF  %lookup(V02paesi3 : $IfsPAESI) = 0;
166700140910             ErrMessage   = *on;
166800140910             ErrGenerico  = *on;
166900140910             PosCurPaesi3 = *on;
167000140910             V2Dmsg = $Msg(34);
167100140910             leavesr;
167200140910           ENDIF;
167300140910         ENDIF;
167400140910
167500140910       //?Peso medio a Spedizione
167600140910         IF  V02pkgda <> 0 or V02pkga <> 0;
167700140910         //?Obbligatori tutti e 2 se immesso uno dei due
167800140910           IF  V02pkgda <> 0  and  V02pkga = 0;
167900140910             ErrMessage  = *on;
168000140910             ErrGenerico = *on;
168100140910             PosCurPkga  = *on;
168200140910             V2Dmsg      = $Msg(21);
168300140910             leavesr;
168400140910           ENDIF;
168500140910           IF  V02pkgda = 0  and  V02pkga <> 0;
168600140910             ErrMessage  = *on;
168700140910             ErrGenerico = *on;
168800140910             PosCurPkgda = *on;
168900140910             V2Dmsg      = $Msg(21);
169000140910             leavesr;
169100140910           ENDIF;
169200140910         //?Controllo congruenza tra "DA" e "A"
169300140910           IF  V02pkgda > V02pkga;
169400140910             ErrMessage  = *on;
169500140910             ErrGenerico = *on;
169600140910             PosCurPkgda = *on;
169700140910             V2Dmsg      = $Msg(22);
169800140910             leavesr;
169900140910           ENDIF;
170000140910         ENDIF;
170100140910
170200140910         //?Concorrente 1
170300140910         IF  V02conc1 <> *blanks;
170400140910           IF    %scan('?' : V02conc1) > 0;
170500140910             ErrGenerico = *on;
170600140910             PosCurConc1  = *on;
170700140910             clear TIBS02ds;
170800140910             T02mod = 'R';
170900140910             T02cod = 'IFS';
171000140910             T02ke1 = '10';
171100140910             T02sif = KNSIF;
171200140910             TNTBE_RicercaControllo  (kpjba : tibs02ds);
171300140910             IF  T02err = *blanks;
171400140910               V02conc1 = t02ke2;
171500140910             ENDIF;
171600140910           ENDIF;
171700140910           IF  %lookup(V02conc1 : $Ifs) = 0;
171800140910             ErrMessage  = *on;
171900140910             ErrGenerico = *on;
172000140910             PosCurConc1 = *on;
172100140910             V2Dmsg = $Msg(23);
172200140910             leavesr;
172300140910           ENDIF;
172400140910         ENDIF;
172500140910         //?Concorrente 2
172600140910         IF  V02conc2 <> *blanks;
172700140910           IF    %scan('?' : V02conc2) > 0;
172800140910             ErrGenerico = *on;
172900140910             PosCurConc2  = *on;
173000140910             clear TIBS02ds;
173100140910             T02mod = 'R';
173200140910             T02cod = 'IFS';
173300140910             T02ke1 = '10';
173400140910             T02sif = KNSIF;
173500140910             TNTBE_RicercaControllo  (kpjba : tibs02ds);
173600140910             IF  T02err = *blanks;
173700140910               V02conc2 = t02ke2;
173800140910             ENDIF;
173900140910           ENDIF;
174000140910           IF  %lookup(V02conc2 : $Ifs) = 0;
174100140910             ErrMessage  = *on;
174200140910             ErrGenerico = *on;
174300140910             PosCurConc2 = *on;
174400140910             V2Dmsg = $Msg(23);
174500140910             leavesr;
174600140910           ENDIF;
174700140910         ENDIF;
174800140910         //?Concorrente 3
174900140910         IF  V02conc3 <> *blanks;
175000140910           IF    %scan('?' : V02conc3) > 0;
175100140910             ErrGenerico = *on;
175200140910             PosCurConc3  = *on;
175300140910             clear TIBS02ds;
175400140910             T02mod = 'R';
175500140910             T02cod = 'IFS';
175600140910             T02ke1 = '10';
175700140910             T02sif = KNSIF;
175800140910             TNTBE_RicercaControllo  (kpjba : tibs02ds);
175900140910             IF  T02err = *blanks;
176000140910               V02conc3 = t02ke2;
176100140910             ENDIF;
176200140910           ENDIF;
176300140910           IF  %lookup(V02conc3 : $Ifs) = 0;
176400140910             ErrMessage  = *on;
176500140910             ErrGenerico = *on;
176600140910             PosCurConc3 = *on;
176700140910             V2Dmsg = $Msg(23);
176800140910             leavesr;
176900140910           ENDIF;
177000140910         ENDIF;
177100131125
177200131125       //?Interessati alla Logistica
177300131125       //?Se inserite le data di immissione interessamento alla logistica inserire anche S
177400131125         IF  V02int = ' ' and
177500131125             (V02dil1 > 0 or V02dil2 > 0);
177600131125           ErrMessage  = *on;
177700131125           ErrGenerico = *on;
177800131125           PosCurINT   = *on;
177900131125           V2Dmsg = $Msg(25);
178000131125           leavesr;
178100131125         ENDIF;
178200131125
178300131125       //?Se inserite "S" potenziali interessati alla LOGISTICA inserire anche le date
178400131125       //IF  V02int = 'S' and
178500131125       //    (V02dil1 = 0 and  V02dil2 = 0);
178600131125       //  ErrMessage  = *on;
178700131125       //  ErrGenerico = *on;
178800131125       //  PosCurDIL1  = *on;
178900131125       //  V2Dmsg = $Msg(26);
179000131125       //  leavesr;
179100131125       //ENDIF;
179200131125
179300131125       //?Controllo la correttezza delle date immissione interesse logistica
179400131125           IF v02dil1 > 0 ;
179500131125             clear wlbdat;
179600131125             G02dat = V02dil1;
179700131125             xsrda8(wlbdat);
179800131125             IF  G02err = '1';
179900131125               ErrMessage  = *on;
180000131125               ErrGenerico = *on;
180100131125               PosCurDil1  = *on;
180200131125               V2Dmsg      = $Msg(27);
180300131125               leavesr;
180400131125             ENDIF;
180500131125
180600131125             V02dil1  = G02dat;
180700131125             wdil1    = G02inv;
180800131125
180900131125           ENDIF;
181000131125
181100131125           IF v02dil2 > 0 ;
181200131125             clear wlbdat;
181300131125             G02dat = V02dil2;
181400131125             xsrda8(wlbdat);
181500131125             IF  G02err = '1';
181600131125               ErrMessage  = *on;
181700131125               ErrGenerico = *on;
181800131125               PosCurDil2  = *on;
181900131125               V2Dmsg      = $Msg(27);
182000131125               leavesr;
182100131125             ENDIF;
182200131125
182300131125             V02dil2  = G02dat;
182400131125             wdil2    = G02inv;
182500131125
182600131125           ENDIF;
182700131125
182800131125         //?Se impostata data "dal" e non "al"
182900131125         //?imposto "al" = data del giorno
183000131125           IF  V02dil1  > 0 and V02dil2  = 0;
183100131125             wdil2     = %dec(%date());
183200131125             data_eur  = %date(wdil2:*iso);
183300131125             V02dil2   = %dec(data_eur);
183400131125           ENDIF;
183500131125         //?"DAL" congruente con "AL"
183600131125           IF  wdil1 > 0  and  wdil2 = 0;
183700131125             ErrMessage  = *on;
183800131125             ErrGenerico = *on;
183900131125             PosCurDil1  = *on;
184000131125             V2Dmsg      = $Msg(28);
184100131125             leavesr;
184200131125           ENDIF;
184300131125           IF  wdil1  > 0  and  wdil2  > 0  and  wdil1  > wdil2 ;
184400131125             ErrMessage  = *on;
184500131125             ErrGenerico = *on;
184600131125             PosCurDil2  = *on;
184700131125             V2Dmsg      = $Msg(28);
184800131125             leavesr;
184900131125           ENDIF;
185000131125
185100131125       //?Se inserite "S" potenziali interessati alla LOGISTICA inserire anche se in outsourcing
185200131126       //IF  V02int = 'S' and
185300131126       //     V02LOS = *blank;
185400131126       //  ErrMessage  = *on;
185500131126       //  ErrGenerico = *on;
185600131126       //  PosCurLOS   = *on;
185700131126       //  V2Dmsg = $Msg(29);
185800131126       //  leavesr;
185900131126       //ENDIF;
186000131125
186100131127       //?Se inserite Concorrenti ci deve essere  anche se logistica in outsourcing = SI
186200131125         IF  V02conl <> *blanks  and
186300131127              V02LOS <> 'S' ;
186400131125           ErrMessage  = *on;
186500131125           ErrGenerico = *on;
186600131125           PosCurLOS   = *on;
186700131125           V2Dmsg = $Msg(30);
186800131125           leavesr;
186900131125         ENDIF;
187000131125
187100131125         //?Concorrente 1
187200131125         IF  V02concl1 <> *blanks;
187300131125           IF    %scan('?' : V02concl1) > 0;
187400131125             ErrGenerico = *on;
187500131125             PosCurConcl1  = *on;
187600131125             clear TIBS02ds;
187700131125             T02mod = 'R';
187800131125             T02cod = 'IFS';
187900131125             T02ke1 = '40';
188000131125             T02sif = KNSIF;
188100131125             TNTBE_RicercaControllo  (kpjba : tibs02ds);
188200131125             IF  T02err = *blanks;
188300131125               V02concl1 = t02ke2;
188400131125             ENDIF;
188500131125           ENDIF;
188600131125           IF  %lookup(V02concl1 : $IfsLOG) = 0;
188700131125             ErrMessage  = *on;
188800131125             ErrGenerico = *on;
188900131125             PosCurConcl1 = *on;
189000131125             V2Dmsg = $Msg(31);
189100131125             leavesr;
189200131125           ENDIF;
189300131125         ENDIF;
189400131125         //?Concorrente 2
189500131125         IF  V02concl2 <> *blanks;
189600131125           IF    %scan('?' : V02concl2) > 0;
189700131125             ErrGenerico = *on;
189800131125             PosCurConcl2  = *on;
189900131125             clear TIBS02ds;
190000131125             T02mod = 'R';
190100131125             T02cod = 'IFS';
190200131125             T02ke1 = '40';
190300131125             T02sif = KNSIF;
190400131125             TNTBE_RicercaControllo  (kpjba : tibs02ds);
190500131125             IF  T02err = *blanks;
190600131125               V02concl2 = t02ke2;
190700131125             ENDIF;
190800131125           ENDIF;
190900131125           IF  %lookup(V02concl2 : $IfsLOG) = 0;
191000131125             ErrMessage  = *on;
191100131125             ErrGenerico = *on;
191200131125             PosCurConcl2 = *on;
191300131125             V2Dmsg = $Msg(31);
191400131125             leavesr;
191500131125           ENDIF;
191600131125         ENDIF;
191700131125         //?Concorrente 3
191800131125         IF  V02concl3 <> *blanks;
191900131125           IF    %scan('?' : V02concl3) > 0;
192000131125             ErrGenerico = *on;
192100131125             PosCurConcl3  = *on;
192200131125             clear TIBS02ds;
192300131125             T02mod = 'R';
192400131125             T02cod = 'IFS';
192500131125             T02ke1 = '40';
192600131125             T02sif = KNSIF;
192700131125             TNTBE_RicercaControllo  (kpjba : tibs02ds);
192800131125             IF  T02err = *blanks;
192900131125               V02concl3 = t02ke2;
193000131125             ENDIF;
193100131125           ENDIF;
193200131125           IF  %lookup(V02concl3 : $IfsLOG) = 0;
193300131125             ErrMessage  = *on;
193400131125             ErrGenerico = *on;
193500131125             PosCurConcl3 = *on;
193600131125             V2Dmsg = $Msg(31);
193700131125             leavesr;
193800131125           ENDIF;
193900131125         ENDIF;
194000131125
194100131125         //?Se inserite delle selezioni metto in reverse immage F05
194200131125            altreparz = *off;
194300140910         IF V02INT <> ' 'or V02LOS <> ' ' or V02Conl <> *blanks  or
194400140910            V02nrfda <> *zeros or V02nroda <> *zeros or
194500140910            V02nerda <> *zeros or V02naeda <> *zeros or
194600140910            V02ntrda <> *zeros or V02paesi <> *blanks or
194700140910            V02conc <> *blanks or V02pkgda <> *zeros or
194800140910            V02she <> ' ' or V02secm <> ' ' or V02sbx <> ' ';
194900131125            altreparz = *on;
195000131125         ENDIF;
195100131125
195200131125       ENDSR;
195300080206
195400080206       //--------------------------------------------------------------
195500080206       //?Gestione selezione di un singolo codice cliente potenziale
195600080206       //--------------------------------------------------------------
195700080206       BEGSR sr_1_cpo;
195800080206
195900110308         CPOcpo = %int(V01cpo);
196000080208         chain  CPOcpo  TNCPO000;
196100080206
196200080206         // Codice potenziale NON trovato
196300080206         if  NOT  %found(TNCPO01L);
196400080213           $Video      = 'D1';
196500080206           errMessage  = *on;
196600080206           errGenerico = *on;
196700080206           PosCurCP1   = *on;
196800080208           V1Dmsg = $Msg(08);
196900080206           leavesr;
197000080206         endif;
197100080206
197200080206         // Programma richiamato
197300091028         if  Mk01k11 = '2'or Mk01k11='4';
197400080208           exsr Return_Dati;
197500080206           $Fine    = *on;
197600080206           leavesr;
197700080206         endif;
197800080206
197900080206         // Pgm. richiamato per interrogazione potenziali
198000080206         exsr Call_TRMK10;
198100080206
198200080206       ENDSR;
198300080206
198400080206       //--------------------------------------------------------------
198500080206       //?Gestione videata S01
198600080206       //--------------------------------------------------------------
198700080206       BEGSR GesS01;
198800080207
198900080207         // Inizializzazione videata
199000080207         if  $InzS01 = *on;
199100080207            exsr InzS01;
199200080207            $InzS01  = *off;
199300080207         endif;
199400080207
199500080207         // Posizionamento cursore
199600080207         if  C01csr  > *zero;
199700080207           C01rcd = C01csr;
199800080207         else;
199900080207         // Se non è stato caricato nulla si riemette la videata D01
200000080207           C01rcd = 1;
200100080207           $Video = 'D1';
200200080207           errMessage  = *on;
200300080207           errGenerico = *on;
200400080208           V1Dmsg = $Msg(09);
200500110309
200600110309         // Chiudo il cursore
200700110309         exec sql
200800110309           close CPO;
200900080207           leavesr;
201000080207         endif;
201100080207
201200080207         // Emissione Testata e Piede con tasti funzionali abilitati
201300091027           write  MK01T01;
201400091027           write  MK01P01;
201500080207
201600080207         // Emissione videata
201700091027         exfmt  MK01C01;
201800080207         reset errMessage;
201900080207         reset errGenerico;
202000080207         clear V1Dmsg;
202100080207
202200080207         SELECT;
202300080207
202400080207         // - F3=Fine
202500080207           WHEN  dsp_aid = c_F03;
202600080207             exsr F03D01;
202700080207
202800080207         // - F12=Ritorno
202900080207           WHEN  dsp_aid = c_F12;
203000080207             exsr F12S01;
203100080626
203200080626         // - F14=riconciliazione potenziali - solo in gestione
203300080626           WHEN  dsp_aid = c_F14;
203400080626             exsr F14S01;
203500080207
203600080207         // - Roll-Up
203700080207           WHEN  dsp_aid = c_RollUp;
203800080208           // - Verifica se raggiunto il limite di records registrabili
203900080208           //   nel subfile (9999)
204000080208             if  S01nrr = *hival;
204100080208               errMessage  = *on;
204200080208               errGenerico = *on;
204300080208               V1Dmsg = $Msg(10);
204400080208             else;
204500080208               exsr RollUpS01;
204600080208             endif;
204700080207
204800080207         // Invio
204900080207           OTHER;
205000080207             exsr OpzS01;
205100080207             if  errGenerico = *on;
205200080207               leavesr;
205300080207             endif;
205400080207
205500080207         ENDSL;
205600080207
205700080207       ENDSR;
205800080207
205900080207       //--------------------------------------------------------------
206000080207       //?Inizializzazione videata S01
206100080207       //--------------------------------------------------------------
206200080207       BEGSR InzS01;
206300080207
206400080207       // Pulizia subfile
206500080207         SflDsp_N    = *on;
206600080207         SflDspCtl_N = *on;
206700091027         write  MK01C01;
206800080207         SflDspCtl_N = *off;
206900080207         SflEnd      = *off;
207000080207
207100080207         clear W_SflPag;
207200080207         clear C01rcd;
207300080207         clear C01csr;
207400080207         clear S01nrr;
207500080207         clear V1Dmsg;
207600080207         errMessage  = *off;
207700080207         errGenerico = *off;
207800080207         WindDspF    = IndDspF;
207900080207         reset WindDspFb;
208000080207         IndDspF     = WindDspF;
208100080207
208200081017
208300080207       // Caricamento dei dati da presentare nel sfl
208400080207         $EoF = *off;
208500080213         select;
208600110309
208700080207         // - per Ragione sociale
208800110520           when  $Ord = 2 and V01osf = 'R';
208900110309             setll  V01rag  TNCPO002;
209000110309
209100110309         // - per Provincia in ordine di ragione sociale
209200110309           when  $Ord = 4 and V01osf = 'R';
209300110309             setll (V01prv : V01rag) TNCPO004;
209400110309
209500110309         // - per tutto il resto
209600110309           other;
209700110309             exsr PrepSql;
209800110309
209900080213         endsl;
210000080213
210100080213         exsr sr_ReadRec;
210200080207
210300080207         dow  $EoF   = *off     and
210400080207              S01nrr < C_SflPag;
210500080207           exsr RollUpS01;
210600080207         enddo;
210700080207
210800080207       // Impaginazione subfile
210900080207       // -> forza l'impaginazione sul 1° rec. del subfile
211000080207         if S01nrr > *zero;
211100080207           C01rcd  = 1;
211200080207           C01csr  = 1;
211300080207         else;
211400080207           clear C01rcd;
211500080207         endif;
211600080207
211700080207       ENDSR;
211800110309       //--------------------------------------------------------------
211900110309       //?Preparo Stringa SQL.
212000110309       //--------------------------------------------------------------
212100110309       BEGSR PrepSql;
212200110309
212300110309       //?Imposto la stringa per SQL
212400110601         IF  V01osf = 'R';
212500110601           wSQL = 'select * from TNCPO00F ';
212600110601         ENDIF;
212700110601         IF  V01osf = 'I';
212800110601           wSQL = 'select tncpo00f.*, -
212900110601                   case cpoftr when '' '' then ''0'' -
213000110601                   else ORD end as CPOord -
213100110601                   from tncpo00f left join -
213200110601                  (select tblkey, substr(tbluni, 31, 1) -
213300110601                   as ORD from tabel00f -
213400110601                   where tblcod = ''IC'') as TAB -
213500110601                   on cpoftr = tblkey ';
213600110601         ENDIF;
213700110601
213800110309         SELECT;
213900110309
214000110309         //?Selezione per categoria merceologica
214100110309           WHEN  $Ord = 3;
214200110309             yy = 0;
214300110309             wSQL += 'WHERE CPOsct in (';
214400110309
214500110615             IF V01sct1 <> *blanks and V01sct1 <> *zeros;
214600110309               IF yy > 0;
214700110309                 wSQL += ', ';
214800110309               ELSE;
214900110309                 yy = 1;
215000110309               ENDIF;
215100110309               wSQL += V01sct1;
215200110309             ENDIF;
215300110615             IF V01sct2 <> *blanks and V01sct2 <> *zeros;
215400110309               IF yy > 0;
215500110309                 wSQL += ', ';
215600110309               ELSE;
215700110309                 yy = 1;
215800110309               ENDIF;
215900110309               wSQL += V01sct2;
216000110309             ENDIF;
216100110615             IF V01sct3 <> *blanks and V01sct3 <> *zeros;
216200110309               IF yy > 0;
216300110309                 wSQL += ', ';
216400110309               ELSE;
216500110309                 yy = 1;
216600110309               ENDIF;
216700110309               wSQL += V01sct3;
216800110309             ENDIF;
216900110309
217000110309           wSQL += ')';
217100110309
217200110309         //?Selezione per provincia (solo in caso di ordinamento per importanza cliente)
217300110309           WHEN  $Ord = 4;
217400110309             wSQL += 'WHERE CPOprv = ';
217500110309             wSQL += '''';
217600110309             wSQL += V01prv;
217700110309             wSQL += '''';
217800110309
217900110309         //?Selezione per codice fiscale
218000110309           WHEN  $Ord = 5;
218100110309             wSQL += 'WHERE CPOcdf = ';
218200110309             wSQL += '''';
218300110309             wSQL += V01cdf;
218400110309             wSQL += '''';
218500110309
218600110309         //?Selezione per partita IVA
218700110309           WHEN  $Ord = 6;
218800110309             wSQL += 'WHERE CPOpiv = ';
218900110309             wSQL += '''';
219000110309             wSQL += V01piv;
219100110309             wSQL += '''';
219200110309
219300110309         //?Selezione per Importanza cliente
219400110309           WHEN  $Ord = 7;
219500110309             yy = 0;
219600110309             wSQL += 'WHERE CPOftr in (';
219700110309             IF V01cic1 <> *blanks;
219800110309               IF yy > 0;
219900110309                 wSQL += ', ';
220000110309               ELSE;
220100110309                 yy = 1;
220200110309               ENDIF;
220300110309               wSQL += '''';
220400110309               wSQL += V01cic1;
220500110309               wSQL += '''';
220600110309             ENDIF;
220700110309             IF V01cic2 <> *blanks;
220800110309               IF yy > 0;
220900110309                 wSQL += ', ';
221000110309               ELSE;
221100110309                 yy = 1;
221200110309               ENDIF;
221300110309               wSQL += '''';
221400110309               wSQL += V01cic2;
221500110309               wSQL += '''';
221600110309             ENDIF;
221700110309             IF V01cic3 <> *blanks;
221800110309               IF yy > 0;
221900110309                 wSQL += ', ';
222000110309               ELSE;
222100110309                 yy = 1;
222200110309               ENDIF;
222300110309               wSQL += '''';
222400110309               wSQL += V01cic3;
222500110309               wSQL += '''';
222600110309             ENDIF;
222700110309             IF V01cic4 <> *blanks;
222800110309               IF yy > 0;
222900110309                 wSQL += ', ';
223000110309               ELSE;
223100110309                 yy = 1;
223200110309               ENDIF;
223300110309               wSQL += '''';
223400110309               wSQL += V01cic4;
223500110309               wSQL += '''';
223600110309             ENDIF;
223700110309             IF V01cic5 <> *blanks;
223800110309               IF yy > 0;
223900110309                 wSQL += ', ';
224000110309               ELSE;
224100110309                 yy = 1;
224200110309               ENDIF;
224300110309               wSQL += '''';
224400110309               wSQL += V01cic5;
224500110309               wSQL += '''';
224600110309             ENDIF;
224700110309
224800110309           wSQL += ')';
224900110309
225000110309         //?Selezione per Filiale/Area
225100110309           WHEN  $Ord = 8 and V01flt > 0;
225200110309             wSQL += 'WHERE CPOflt = ';
225300110309             wSQL += %editc(V01flt:'X');
225400110309
225500110309           WHEN  $Ord = 8 and V01car <> *blanks;
225600110309             clear $Fil1;
225700110309             yy = 0;
225800110309             xx = 1;
225900110309             FOR xx by 1 to %elem($Car);
226000110309               IF %editc($Car(xx):'X') = V01car;
226100110309                 yy += 1;
226200110309                 $Fil1(yy) = $Fil(xx);
226300110309               ENDIF;
226400110309             ENDFOR;
226500110309             yy = 0;
226600110309             xx = 1;
226700110309             wSQL += 'WHERE CPOflt in(';
226800110309             FOR xx by 1 to %elem($Fil1);
226900110309               IF $Fil1(xx) <> *zeros;
227000110309                 IF yy > 0;
227100110309                   wSQL += ', ';
227200110309                 ELSE;
227300110309                   yy = 1;
227400110309                 ENDIF;
227500110309                 wSQL += %editc($Fil1(xx):'X');
227600110309               ENDIF;
227700110309             ENDFOR;
227800110309           wSQL += ')';
227900110309
228000110309         //?Selezione per Categoria potenziale
228100110309           WHEN  $Ord = 9;
228200110407             yy = 0;
228300110407             wSQL += 'WHERE CPOfls in (';
228400110407             IF V01fls1 <> *blanks;
228500110407               IF yy > 0;
228600110407                 wSQL += ', ';
228700110407               ELSE;
228800110407                 yy = 1;
228900110407               ENDIF;
229000110407               wSQL += '''';
229100110407               wSQL += V01fls1;
229200110407               wSQL += '''';
229300110407             ENDIF;
229400110407             IF V01fls2 <> *blanks;
229500110407               IF yy > 0;
229600110407                 wSQL += ', ';
229700110407               ELSE;
229800110407                 yy = 1;
229900110407               ENDIF;
230000110407               wSQL += '''';
230100110407               wSQL += V01fls2;
230200110407               wSQL += '''';
230300110407             ENDIF;
230400110407             IF V01fls3 <> *blanks;
230500110407               IF yy > 0;
230600110407                 wSQL += ', ';
230700110407               ELSE;
230800110407                 yy = 1;
230900110407               ENDIF;
231000110407               wSQL += '''';
231100110407               wSQL += V01fls3;
231200110407               wSQL += '''';
231300110407             ENDIF;
231400110407             IF V01fls4 <> *blanks;
231500110407               IF yy > 0;
231600110407                 wSQL += ', ';
231700110407               ELSE;
231800110407                 yy = 1;
231900110407               ENDIF;
232000110407               wSQL += '''';
232100110407               wSQL += V01fls4;
232200110407               wSQL += '''';
232300110407             ENDIF;
232400110407             IF V01fls5 <> *blanks;
232500110407               IF yy > 0;
232600110407                 wSQL += ', ';
232700110407               ELSE;
232800110407                 yy = 1;
232900110407               ENDIF;
233000110407               wSQL += '''';
233100110407               wSQL += V01fls5;
233200110407               wSQL += '''';
233300110407             ENDIF;
233400110407
233500110407           wSQL += ')';
233600110309
233700140910         //?Selezione per % fatturato export
233800140910           WHEN  $Ord = 11;
233900140910             wSQL += 'WHERE CPOpexp between ';
234000140910             wSQL += %editc(V01pexpda:'X');
234100140910             wSQL += ' and ';
234200140910             wSQL += %editc(V01pexpa:'X');
234300140910
234400140910         //?Selezione per Anno Inizio Attività
234500140910           WHEN  $Ord = 12;
234600140910             wSQL += 'WHERE substr(CPOtel, 1, 4) between ';
234700140910             wSQL += %editc(V01annoda:'X');
234800140910             wSQL += ' and ';
234900140910             wSQL += %editc(V01annoa:'X');
235000140910
235100110309         ENDSL;
235200110309
235300110309         IF  V01osf = 'R';
235400110309           wSQL += ' order by CPOrag';
235500110309         ENDIF;
235600110309         IF  V01osf = 'I';
235700110601           //wSQL += ' order by CPOftr';
235800110601           wSQL += ' order by CPOord desc';
235900110309         ENDIF;
236000110309
236100110309         wSQL += ' for fetch only';
236200110309
236300110309         //?Dichiarazione cursore
236400110309         exec sql
236500110309           prepare S1 from :wSQL;
236600110309         exec sql
236700110309           declare CPO cursor for S1;
236800110309
236900110309         //?Apertura del cursore
237000110309         exec sql
237100110309           open CPO;
237200110309
237300110309         IF sqlcode < 0;
237400110309           $Eof = *on;
237500110309         ENDIF;
237600110309
237700110309       ENDSR;
237800080207
237900080207       //--------------------------------------------------------------
238000080207       //?Caricamento singola pagina S01
238100080207       //--------------------------------------------------------------
238200080207       BEGSR RollUpS01;
238300080207
238400080207         S01nrr    = (W_SflPag * C_SflPag);
238500080207         W_SflPag += 1;
238600080207         SflNxtChg = *off;
238700080207
238800080207         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
238900080208         DOW  $EoF   = *off                   and
239000080208              S01nrr < (W_SflPag * C_SflPag)  and
239100080208              S01nrr < *hival;
239200080207
239300080207         // - Caricamento dati nel record del subfile
239400080213           exsr  RieS01;
239500080213           S01nrr += 1;
239600091027           write MK01S01;
239700080207
239800080207         // - Lettura record successivo nell'archivio
239900080213           exsr sr_ReadRec;
240000080207
240100080207         ENDDO;
240200080207
240300080207         // Visualizzazione del SFL (se ci sono dati)
240400080207         SflDsp_N = (S01nrr <= *zeros);
240500080207
240600080207         // Attivazione (eventuale) del SFLEND
240700080207         SflEnd   = ($EoF = *on);
240800080207
240900080207         // Posizionamento cursore al 1° rcd della pagina
241000080207         if  S01nrr > *zero;
241100080207           wPag   = S01nrr / C_SflPag;
241200080207           wRig   = S01nrr - (C_SflPag * wPag);
241300080207           C01rcd = wPag * C_SflPag;
241400080207           if  wRig > *zeros;
241500080207             C01rcd = C01rcd + 1;
241600080207           else;
241700080207             C01rcd = C01rcd - C_SflPag + 1;
241800080207           endif;
241900080207         else;
242000080207           clear  C01rcd;
242100080207         endif;
242200080207
242300080207         C01csr = C01rcd;
242400080207
242500080207       ENDSR;
242600080213
242700080213       //--------------------------------------------------------------
242800080213       //?Lettura record successivo in base all'ordinamento
242900080213       //--------------------------------------------------------------
243000080213       BEGSR sr_ReadRec;
243100080213
243200080213         $RecOK  = *off;
243300080213
243400080213         DOW  $EoF   = *off   and
243500080213              $RecOK = *off;
243600080213
243700080213           select;
243800080213
243900080213           // (se non rilevato stop record elaborabili in selezione)
244000080213             when  $EoF = *on;
244100080213
244200080213           // - per Ragione sociale
244300110520             when  $Ord = 2 and V01osf = 'R';
244400080213               read  TNCPO002;
244500080213               $EoF  = (%eof(TNCPO02L));
244600110309
244700110309           // - per Provincia in ordine di ragione sociale
244800110309             when  $Ord = 4 and V01osf = 'R';
244900110309               reade  V01prv  TNCPO004;
245000110309               $EoF  = (%eof(TNCPO04L));
245100110309
245200110309           // - per tutto il resto
245300110309             other;
245400110309               exec sql
245500110309               fetch next from CPO into :TNCPOds;
245600110309               IF sqlcod = 100 or sqlcod < 0;
245700110309                 $Eof = *on;
245800110309               ENDIF;
245900080213
246000080213           endsl;
246100080213
246200080213           // Selezione record in base alle parzializzazioni
246300080213           if  $EoF  = *off;
246400080213             exsr sr_SelRec;
246500080213           endif;
246600080213
246700080213         ENDDO;
246800080213
246900080213       ENDSR;
247000080207
247100080207       //--------------------------------------------------------------
247200080207       //?Selezione record in base alle parzializzazioni
247300080207       //--------------------------------------------------------------
247400080207       BEGSR sr_SelRec;
247500080207
247600080213         $RecOK  = *off;
247700120914         dcpo01=cporst   ;
247800140910         dcpo02 = CPOtel;
247900080207
248000080207         // Parzializzazione per ragione sociale
248100110309         if  V01rag <> *blank;
248200110309           xx    = %checkr(' ' : V01rag);
248300110309           if   %subst(V01rag : 1 : xx) <> %subst(CPOrag : 1 : xx);
248400080211             if  $Ord = 2;
248500080211               $EoF = *on;
248600080211             endif;
248700080207             leavesr;
248800080207           endif;
248900080207         endif;
249000110309
249100110309         // Parzializzazione per mai contattati
249200110310         IF  V01pst <> *blanks;
249300120914           IF  (V01pst = 'N' and §CPOdtpcon <> *blanks and
249400120914                                 §CPOdtpcon <> *zeros) or
249500120914               (V01pst = 'S' and (§CPOdtpcon = *blanks or
249600120914                                  §CPOdtpcon = *zeros));
249700110314             leavesr;
249800110314           ENDIF;
249900110309         ENDIF;
250000080207
250100080207         // Altre parzializzazioni:
250200080207         select;
250300080207
250400080207           // Parzializzazione per provincia
250500110309           when  $Ord <> 4 and
250600110309                 V01prv <> *blank   and
250700110309                 V01prv <> CPOprv;
250800080207             leavesr;
250900080207
251000080207           // Parzializzazione per c.a.p.
251100110309           when  V01cap <> *blank   and
251200110309                 V01cap <> CPOcap;
251300080207             leavesr;
251400080207
251500080207           // Parzializzazione per località
251600110309           when  V01loc <> *blank   and
251700110309                 %subst( V01loc : 1 : %checkr(' ':V01loc) ) <>
251800110309                 %subst( CPOcit : 1 : %checkr(' ':V01loc) );
251900080207             leavesr;
252000080207
252100080207           // Parzializzazione per filiale di appartenenza
252200110309           when  ($Ord <> 8 or V01car > *zeros) and
252300110309                  V01flt <> *zero    and
252400110309                  V01flt <> CPOflt;
252500080207             leavesr;
252600080207
252700080207           // Parzializzazione per categoria merceologica
252800110309           when  $Ord <> 3 and
252900110615                 (V01sct1 <> *blank and V01sct1 <> *zeros and
253000110615                  %int(V01sct1) <> CPOsct) or
253100110615                 (V01sct2 <> *blank and V01sct2 <> *zeros and
253200110615                  %int(V01sct2) <> CPOsct) or
253300110615                 (V01sct3 <> *blank and V01sct3 <> *zeros and
253400110309                  %int(V01sct3) <> CPOsct);
253500080207             leavesr;
253600080207
253700080207           // Parzializzazione per area
253800110309           when  ($Ord <> 8 or V01flt > 0 ) and
253900110309                 V01car <> *blank   and
254000110309                 V01car <> *zero    and
254100110309                 V01car <> %editc( $Car( %lookup( CPOflt : $Fil ) ) : 'X');
254200080207             leavesr;
254300080207
254400080207           // Parzializzazione per partita iva
254500110309           when  $Ord <> 6 and
254600110309                 V01piv <> *blank   and
254700110309                 V01piv <> CPOpiv;
254800080207             leavesr;
254900080207
255000080207           // Parzializzazione per codice fiscale
255100110309           when  $Ord <> 5 and
255200110309                 V01cdf <> *blank   and
255300110309                 V01cdf <> CPOcdf;
255400080207             leavesr;
255500110309
255600110309           // Parzializzazione Categoria potenziale
255700110407           when  $ord <> 9 and V01fls <> *blanks and
255800110407                 CPOfls <> *blanks and %lookup(CPOfls:$fls) = 0;
255900110309             leavesr;
256000110309
256100110309           // Parzializzazione Importanza cliente
256200110322           when  $Ord <> 7 and V01cic <> *blanks and
256300110322                 CPOftr = *blanks;
256400110309             leavesr;
256500110322           when  $Ord <> 7 and V01cic <> *blanks and
256600110322                 CPOftr <> *blanks and %lookup(CPOftr:$cic) = 0;
256700110322             leavesr;
256800140910
256900140910           // Parzializzazione % Fatturato Export
257000140910           when  $Ord <> 11 and V01pexp > *zeros and
257100140910                 CPOpexp = *zeros;
257200140910             leavesr;
257300140910           when  $Ord <> 11 and V01pexp > *zeros and
257400140910                 (CPOpexp < V01pexpda or CPOpexp > V01pexpa);
257500140910             leavesr;
257600140910
257700140910           // Parzializzazione Anno Inizio Attività
257800140910           when  $Ord <> 12 and V01anno > *zeros and
257900140910                 (§CPOannoia = *zeros or §CPOannoia = *blanks);
258000140910             leavesr;
258100140910           when  $Ord <> 12 and V01anno > *zeros and
258200140915                 (§CPOannoia < %editc(V01annoda:'X') or
258300140915                  §CPOannoia > %editc(V01annoa :'X'));
258400140910             leavesr;
258500080207
258600080207         endsl;
258700110310
258800110310       //?Parzializzazioni INFO commerciali
258900140910       //?Non affidato a BRT Italia Parcel
259000140910         IF  V02nrfda > 0;
259100140910           chain (CPOcpo:'NRF') TICPI01L;
259200140910         //?Non trovo dati relativi al peso escludo il potenziale
259300140910           IF  not %found(TICPI01L);
259400140910             leavesr;
259500140910           ENDIF;
259600140910           IF  CPIpft < V02nrfda or CPIpft > V02nrfa;
259700140910             leavesr;
259800140910           ENDIF;
259900140910         ENDIF;
260000140910
260100140910       //?Non affidato a BRT Italia Messaggeria
260200140910         IF  V02nroda > 0;
260300140910           chain (CPOcpo:'NRO') TICPI01L;
260400140910         //?Non trovo dati relativi al peso escludo il potenziale
260500140910           IF  not %found(TICPI01L);
260600140910             leavesr;
260700140910           ENDIF;
260800140910           IF  CPIpft < V02nroda or CPIpft > V02nroa;
260900140910             leavesr;
261000140910           ENDIF;
261100140910         ENDIF;
261200140910
261300140910       //?Non affidato a BRT Estero Via Aerea
261400140910         IF  V02nerda > 0;
261500140910           chain (CPOcpo:'NER') TICPI01L;
261600140910         //?Non trovo dati relativi al peso escludo il potenziale
261700140910           IF  not %found(TICPI01L);
261800140910             leavesr;
261900140910           ENDIF;
262000140910           IF  CPIpft < V02nerda or CPIpft > V02nera;
262100140910             leavesr;
262200140910           ENDIF;
262300140910         ENDIF;
262400140910
262500140910       //?Non affidato a BRT Estero Serv. Camionistico
262600140910         IF  V02naeda > 0;
262700140910           chain (CPOcpo:'NAE') TICPI01L;
262800140910         //?Non trovo dati relativi al peso escludo il potenziale
262900140910           IF  not %found(TICPI01L);
263000140910             leavesr;
263100140910           ENDIF;
263200140910           IF  CPIpft < V02naeda or CPIpft > V02naea;
263300140910             leavesr;
263400140910           ENDIF;
263500140910         ENDIF;
263600140910
263700140910       //?Non affidato a BRT Altro (es. Completi, ADR)
263800140910         IF  V02ntrda > 0;
263900140910           chain (CPOcpo:'NTR') TICPI01L;
264000140910         //?Non trovo dati relativi al peso escludo il potenziale
264100140910           IF  not %found(TICPI01L);
264200140910             leavesr;
264300140910           ENDIF;
264400140910           IF  CPIpft < V02ntrda or CPIpft > V02ntra;
264500140910             leavesr;
264600140910           ENDIF;
264700140910         ENDIF;
264800140910
264900140910       //?Paesi Estero
265000140910         IF  V02paesi <> *blanks;
265100140910           $Paesi = *off;
265200140910           IF  V02paesi1 <> *blanks;
265300140910             chain (CPOcpo:'50':V02paesi1) TICPI01L;
265400140910             IF  %found(TICPI01L) and CPIval > 0;
265500140910               $Paesi = *on;
265600140910             ENDIF;
265700140910           ENDIF;
265800140910           IF  V02paesi2 <> *blanks;
265900140910             chain (CPOcpo:'50':V02paesi2) TICPI01L;
266000140910             IF  %found(TICPI01L) and CPIval > 0;
266100140910               $Paesi = *on;
266200140910             ENDIF;
266300140910           ENDIF;
266400140910           IF  V02paesi3 <> *blanks;
266500140910             chain (CPOcpo:'50':V02paesi3) TICPI01L;
266600140910             IF  %found(TICPI01L) and CPIval > 0;
266700140910               $Paesi = *on;
266800140910             ENDIF;
266900140910           ENDIF;
267000140910           IF  not $Paesi;
267100140910             leavesr;
267200140910           ENDIF;
267300140910         ENDIF;
267400140910
267500110310       //?Peso medio a spedizione
267600140910         IF  V02pkgda > 0;
267700110310           chain (CPOcpo:'KMS') TICPI01L;
267800110310         //?Non trovo dati relativi al peso escludo il potenziale
267900110310           IF  not %found(TICPI01L);
268000110310             leavesr;
268100110310           ENDIF;
268200140910           IF  CPIval < V02pkgda or CPIval > V02pkga;
268300110310             leavesr;
268400110310           ENDIF;
268500110310         ENDIF;
268600110310
268700110310       //?Spedizioni 10:30-PRIORITY
268800140910         IF  V02she <> *blanks;
268900110310           chain (CPOcpo:'HPR') TICPI01L;
269000110310         //?Non trovo dati relativi al tipo spedizione escludo il potenziale
269100110310           IF  not %found(TICPI01L);
269200110310             leavesr;
269300110310           ENDIF;
269400110310           IF  CPIval = 0;
269500110310             leavesr;
269600110310           ENDIF;
269700110310         ENDIF;
269800140910
269900140910       //?Spedizioni E-COMMERCE
270000140910         IF  V02secm <> *blanks;
270100140910           chain (CPOcpo:'ECM') TICPI01L;
270200140910         //?Non trovo dati escludo il potenziale
270300140910           IF  not %found(TICPI01L);
270400140910             leavesr;
270500140910           ENDIF;
270600140910           IF  CPIval = 0;
270700140910             leavesr;
270800140910           ENDIF;
270900140910         ENDIF;
271000110601
271100110601       //?Spedizioni SECURE BOX
271200140910         IF  V02sbx <> *blanks;
271300110601           chain (CPOcpo:'SBX') TICPI01L;
271400110601         //?Non trovo dati relativi al tipo spedizione escludo il potenziale
271500110601           IF  not %found(TICPI01L);
271600110601             leavesr;
271700110601           ENDIF;
271800110601           IF  CPIfsn <> 'S';
271900110601             leavesr;
272000110601           ENDIF;
272100110601         ENDIF;
272200110310
272300110310       //?Concorrenti
272400140910         IF  V02conc <> *blanks;
272500110310           $Conc = *off;
272600140910           IF  V02conc1 <> *blanks;
272700140910             chain (CPOcpo:'10':V02conc1) TICPI01L;
272800110310             IF  %found(TICPI01L) and CPIval > 0;
272900110310               $Conc = *on;
273000110310             ENDIF;
273100110310           ENDIF;
273200140910           IF  V02conc2 <> *blanks;
273300140910             chain (CPOcpo:'10':V02conc2) TICPI01L;
273400110310             IF  %found(TICPI01L) and CPIval > 0;
273500110310               $Conc = *on;
273600110310             ENDIF;
273700110310           ENDIF;
273800140910           IF  V02conc3 <> *blanks;
273900140910             chain (CPOcpo:'10':V02conc3) TICPI01L;
274000110310             IF  %found(TICPI01L) and CPIval > 0;
274100110310               $Conc = *on;
274200110310             ENDIF;
274300110310           ENDIF;
274400110310           IF  not $Conc;
274500110310             leavesr;
274600110310           ENDIF;
274700110310         ENDIF;
274800110601
274900110601       //?Controllo trattative in corso
275000110601         $EndTtr = *off;
275100110601         IF  V01ttr <> *blanks;
275200110601           wTtr = 0;
275300110601           setll (CPOcpo) TIVIS01L;
275400110601           DOW  not $EndTtr;
275500110601             reade (CPOcpo) TIVIS01L;
275600110601             IF  %eof(TIVIS01L);
275700110601               $EndTtr = *on;
275800110601               leave;
275900110601             ENDIF;
276000110606             IF  VISffz = *blanks and VISdch = 0;
276100110606               wTtr += 1;
276200110606               $EndTtr = *on;
276300110606               leave;
276400110606             ENDIF;
276500110601           ENDDO;
276600110606           IF (V01ttr = 'S' and wTtr = 0) or
276700110606              (V01ttr = 'N' and wTtr > 0);
276800110601             leavesr;
276900110601           ENDIF;
277000110601         ENDIF;
277100080207
277200131125       //?Logistica  INTERESSATI S/N
277300131125         IF  V02int <> *blank ;
277400131125
277500131125           chain (CPOcpo:'LOG') TICPI01L;
277600131125           IF  not %found(TICPI01L);
277700131125                  leavesr ;
277800131125           ENDIF ;
277900131125           IF  %found(TICPI01L);
278000131125               If CPIfsn <> V02int ;
278100131125                  leavesr ;
278200131125               ENDIF ;
278300131125       //?verifico le data di inserimento delle INFO
278400131125               IF (Wdil1 > 0 or Wdil2 > 0) and
278500131125                  (CPIdim < Wdil1 or Cpidim > Wdil2) ;
278600131125                  leavesr ;
278700131125               ENDIF ;
278800131125           ENDIF;
278900131125
279000131125         ENDIF;
279100131125
279200131125       //?Logistica  OUTSOURCING INTERESSATI S/N
279300131125         IF  V02los <> *blank ;
279400131125
279500131125           chain (CPOcpo:'OUT') TICPI01L;
279600131125           IF  not %found(TICPI01L);
279700131125                  leavesr ;
279800131125           ENDIF ;
279900131125           IF  %found(TICPI01L);
280000131125               If CPIfsn <> V02los ;
280100131125                  leavesr ;
280200131125               ENDIF ;
280300131125           ENDIF;
280400131125
280500131125         ENDIF;
280600131125
280700131125       //?Concorrenti  LOGISTICA
280800131125         IF  V02conl <> *blanks;
280900131125           $Conclog = *off;
281000131125           IF  V02concl1 <> *blanks;
281100131125             chain (CPOcpo:'40':V02concl1) TICPI01L;
281200131125             IF  %found(TICPI01L) ;
281300131125               $Conclog = *on;
281400131125             ENDIF;
281500131125           ENDIF;
281600131125           IF  V02concl2 <> *blanks;
281700131125             chain (CPOcpo:'40':V02concl2) TICPI01L;
281800131125             IF  %found(TICPI01L) ;
281900131125               $Conclog = *on;
282000131125             ENDIF;
282100131125           ENDIF;
282200131125           IF  V02concl3 <> *blanks;
282300131125             chain (CPOcpo:'40':V02concl3) TICPI01L;
282400131125             IF  %found(TICPI01L) and CPIval > 0;
282500131125               $Conclog = *on;
282600131125             ENDIF;
282700131125           ENDIF;
282800131125           IF  not $Conclog ;
282900131125             leavesr;
283000131125           ENDIF;
283100131125         ENDIF;
283200131125
283300131125                  $RecOK  = *on;
283400131125
283500080207       ENDSR;
283600110601
283700080207       //--------------------------------------------------------------
283800080207       //?Caricamento singolo record nel SubFile S01
283900080207       //--------------------------------------------------------------
284000080208       BEGSR RieS01;
284100080207
284200091027         clear  MK01S01;
284300080211
284400080211         // Decodifica categoria merceologica
284500080211         clear  ds1L;
284600080211         if  CPOsct <> *zero;
284700080211           TBLcod = '1L';
284800080211           TBLkey = %editc(CPOsct : 'X');
284900080211           chain (TBLkut : TBLcod : TBLkey) TABEL;
285000080211           if  %found(TABEL00F)   and
285100080211               TBLflg = *blank;
285200080211             ds1L    = TBLuni;
285300080211           endif;
285400080211         endif;
285500080207
285600080214         // Parametri di visualizzazione (default)
285700080214         P01da1 = C_dspatr_Norm;
285800080214         P01da2 = C_dspatr_Norm;
285900080214
286000080207         // Campi hidden
286100080208         H01sct  = CPOsct;
286200081020         dcpo01=cporst   ;
286300081020         h01ifotot=§cpoifotot   ;
286400080208
286500080207         // Campi di solo output
286600081017         if cpofsf ='S'      ;
286700081017         s01sfs  ='Sd'   ;
286800081017         else  ;
286900081017         s01sfs  ='Fi'   ;
287000081017         endif   ;
287100081017
287200080207         S01rag  = %subst(CPOrag : 1 : %len(S01rag));
287300081021         P01da1 = C_dspatr_HI  ;
287400080207         S01loc  = %subst(CPOcit : 1 : %len(S01loc));
287500080208         S01prv  = CPOprv;
287600080208         S01flt  = CPOflt;
287700080211
287800080211         S01cpo  = CPOcpo;
287900080211         S01cap  = CPOcap;
288000080211         S01sctD = %subst(§1Ldes : 1 : %len(S01sctD));
288100080211         if  CPOatb <> *blank;
288200080214           P01da1 = C_dspatr_BL;
288300080214           P01da2 = C_dspatr_RI;
288400080211           S01ann = 'A';
288500080211         endif;
288600110309
288700110309        //?memorizzo importanza cliente
288800110309         S01cic = CPOftr;
288900110209
289000110209        //?memorizzo categoria potenziale
289100110309         S01fls = CPOfls;
289200110309
289300110309        //?Cerco prox attività da fare
289400110523         $EndAtt = *off;
289500110523         exec sql
289600110523         declare ATT cursor for select ATCcad from TIATC00F
289700110523         where ATCcpo = :CPOcpo and atcdco = 0
289800110523         order by ATCdad, ATChda;
289900110523         exec sql
290000110523         open ATT;
290100110523         IF sqlcode < 0;
290200110523           $EndAtt = *on;
290300110523         ENDIF;
290400110523         DOW not $EndAtt;
290500110523           exec sql
290600110523           fetch next from ATT into :ATCcad;
290700110523           IF sqlcod = 100 or sqlcod < 0;
290800110523             $EndAtt = *on;
290900110523             leave;
291000110523           ENDIF;
291100110523           S01cad = ATCcad;
291200110523           leave;
291300110523         ENDDO;
291400110523         exec sql close ATT;
291500110309
291600110309        //?Cerco se ci sono trattative aperte
291700110309         $EndTtr = *off;
291800110601         setll (CPOcpo:0) TIVIS01L;
291900110601         DOW  not $EndTtr;
292000110601           reade (CPOcpo:0) TIVIS01L;
292100110601           IF  %eof(TIVIS01L);
292200110601             $EndTtr = *on;
292300110601             leave;
292400110601           ENDIF;
292500110601           IF  VISffz <> *blanks;
292600110601             iter;
292700110601           ENDIF;
292800110601           S01ttr = 'S';
292900110601           $EndTtr = *on;
293000110601         ENDDO;
293100080207
293200080207       ENDSR;
293300080207
293400080207       //--------------------------------------------------------------
293500080207       //?Gestione tasto funzionale F12 da videata S01
293600080207       //?F12=Ritorno
293700080207       //--------------------------------------------------------------
293800080207       BEGSR F12S01;
293900080207
294000080207         // Ritorno alla videata D01
294100080207         $Video = 'D1';
294200110309
294300110309         // Chiudo il cursore
294400110309         exec sql
294500110309           close CPO;
294600110309
294700080207       ENDSR;
294800080207
294900080626       //--------------------------------------------------------------
295000080626       //?Gestione tasto funzionale F14 da videata S01
295100080626       //?F14=riconciliazione clienti potenziali
295200080626       //--------------------------------------------------------------
295300080626       BEGSR F14S01;
295400080626       savkpjbu=kpjbu  ;
295500080626       kpjbu='N'       ;
295600080626       TRMK07R (kpjba) ;
295700080626       kpjbu=savkpjbu  ;
295800080626
295900080626       ENDSR;
296000080207       //--------------------------------------------------------------
296100080207       //?Gestione opzioni subfile
296200080207       //--------------------------------------------------------------
296300080207       BEGSR OpzS01;
296400080207
296500091027         readc MK01S01;
296600080207
296700091027         DOW  NOT  %eof(TRMK01D);
296800080207
296900080207           SflNxtChg = *off;
297000080207           WindDspF  = IndDspF;
297100080207           reset WindDspFb;
297200080207           IndDspF   = WindDspF;
297300080207           C01rcd    = S01nrr;
297400080207
297500080207           SELECT;
297600080207
297700080207             // - Nessuna opzione
297800080208             WHEN  S01opz  = *blank;
297900080208
298000080208             // - Precedente segnalazione di errore
298100080208             WHEN  S01opz  = 'E';
298200080208               clear S01opz;
298300080207
298400080207             // - 1 = Selezione
298500080208             WHEN  S01opz  = '1';
298600091028               // Se pgm richiamato con Mk01k11='2' o '4'
298700080207               // => imposta ds e poi esce
298800091028               if  Mk01k11 = '2'or Mk01k11='4';
298900080207                 exsr Return_Dati;
299000080207                 eval $Fine = *on;
299100080207                 leavesr;
299200080207               endif;
299300110209               //?Se richiamato da gestione attività
299400110209               IF w_daAtti = 'S';
299500110209               //?Controllo se potenziale codificato, in questo caso no attività
299600110601               //?per ora non lo facciamo
299700110309                 IF  S01fls = 'C';
299800110518                  // errMessage  = *on;
299900110518                  // errGenerico = *on;
300000110518                  // PosCurOpz   = *on;
300100110518                  // V1Dmsg = 'Scelta non possibile potenziale CODIFICATO';
300200110209                 ENDIF;
300300110209               ENDIF;
300400081020
300500081020               if ErrGenerico=*off   ;
300600080207               exsr Call_TRMK10;
300700081020               endif                 ;
300800080207
300900081020             // - G/S = gestione o stampa info comm
301000081020             WHEN  S01opz  = 'G' or s01opz='S';
301100081020             // Solo se non richiamato
301200081020 1           if PGMrichia=*on  ;
301300081020               errMessage  = *on;
301400081020               errGenerico = *on;
301500081020               PosCurOpz   = *on;
301600081020               V1Dmsg = $Msg(11);
301700081020 x1          else ;
301800110601               IF  %lookup(%editc(S01flt:'X'):pog) = 0;
301900110601                 errMessage  = *on;
302000110601                 errGenerico = *on;
302100110601                 PosCurOpz   = *on;
302200110601                 V1Dmsg = $Msg(15);
302300110601               ENDIF;
302400110601
302500081020 2             if ErrGenerico=*off   ;
302600081020                    clear trmk50ds    ;
302700081020                    i50cpo=s01cpo     ;
302800081020 4                  if Gesclipot=*on  ;
302900081020                      i50mod='G'        ;
303000081020                      else              ;
303100081020                      i50mod='I'        ;
303200081020 4                  endif             ;
303300081020                    i50rag=s01rag     ;
303400091027                    i50pgm='TRMK01R'  ;
303500081020                    i50ifotot=h01ifotot;
303600081022 3                if s01opz='G'     ;
303700081020                    callp TRMK50R (kpjba:trmk50ds)  ;
303800081020 3                endif     ;
303900081022
304000081022 3                if s01opz='S'     ;
304100081022                  kpjbu=trmk50ds    ;
304200081022                    callp TRMK51R (kpjba)           ;
304300081022 3                endif     ;
304400081020 2             endif     ;
304500081020 1             endif     ;
304600081020
304700080207             // - ? = Opzione NON valida
304800080207             OTHER;
304900080207               errMessage  = *on;
305000080207               errGenerico = *on;
305100080207               PosCurOpz   = *on;
305200080208               V1Dmsg = $Msg(11);
305300080207
305400080207           ENDSL;
305500080207
305600080207           // Aggiornamento sfl
305700080207           select;
305800080207             when  errMessage  = *on;
305900080207               SflNxtChg = *on;
306000080207               C01csr    = C01rcd;
306100080207             when  errGenerico = *on;
306200080208               C01csr    = C01rcd;
306300080208               clear  S01opz;
306400080207             other;
306500080207               C01csr    = C01rcd;
306600080208               clear  S01opz;
306700080207           endsl;
306800080207
306900091027           update MK01S01;
307000080207
307100080208           if  errMessage = *on  or  errGenerico = *on;
307200080207             leave;
307300080207           endif;
307400080207
307500091027           readc MK01S01;
307600080207
307700080207         ENDDO;
307800080207
307900080207       ENDSR;
308000080207
308100080207       //--------------------------------------------------------------
308200080207       //?Chiamata pgm "Gestione Clienti Potenziali" in interrogazione
308300080207       //--------------------------------------------------------------
308400080206       BEGSR Call_TRMK10;
308500080207
308600080207           if GESclipot = *off;
308700091028             Mk01k10 = 'N';
308800080207           else;
308900091028             Mk01k10 = 'R';
309000080207           endif;
309100080207
309200080207         exsr Return_Dati;
309300080207
309400100202         kpjbu  = w_damenu;
309500131126         %subst(kpjbu : 2: 3) = $Logistica;
309600100408
309700100408        // se vengo dal programma di gestione attività passo anche la ds del TRMK19
309800100408         If W_daatti <> 'S' ;
309900100408            trmk02r ( kpjba : trmk01ds );
310000100408         else ;
310100100408            trmk02r ( kpjba : trmk01ds : trmk19ds );
310200100408         endif ;
310300080207
310400080207         SELECT;
310500080207
310600100311         // Se questo pgm. risulta richiamato => esce se fatta una scelta
310700100311           WHEN  Mk01k11 = '1' and MK01f06 <> *blanks;
310800100311              Mk01ent = *on;
310900100311              $Fine    = *on;
311000100311              errGenerico = *on;
311100100311              leavesr;
311200080207
311300091028         // Se premuto F3 da TRMK02R => esce
311400091028           WHEN  Mk01f03 = *on;
311500080207             $Fine       = *on;
311600080207             errGenerico = *on;
311700080207             leavesr;
311800080207
311900091028         // Se errore restituito da TRMK02R => esce
312000091028           WHEN  Mk01err = 'E';
312100091028             V1Dmsg   = Mk01m10;
312200080207             errMessage  = *on;
312300080207             errGenerico = *on;
312400080207             if  $Video  = 'WW';
312500080207               $Video    = 'D1';
312600080207               PosCurCP1 = *on;
312700080207             else;
312800080207               PosCurOpz   = *on;
312900080207               S01opz      = 'E';
313000080207             endif;
313100080207
313200080207         // Altrimenti: se specificato un solo potenziale
313300080207         // => si torna alla 1ª videata (di selezione)
313400080207           OTHER;
313500080207             if  $Video  = 'WW';
313600080207               $Video = 'D1';
313700080207             endif;
313800080207
313900080207         ENDSL;
314000080207
314100080206       ENDSR;
314200080207
314300080207       //--------------------------------------------------------------
314400080207       //?Impostazione campi di output nella struttura dati
314500080207       //--------------------------------------------------------------
314600080207       BEGSR Return_Dati;
314700080207
314800080213         if  $Video = 'D1'   or
314900080213             $Video = 'WW';
315000091028           Mk01cpo = CPOcpo;
315100091028           Mk01rag = CPOrag;
315200091028           Mk01prv = CPOprv;
315300110308           Mk01pst = V01pst;
315400091028           Mk01flt = CPOflt;
315500091028           Mk01cap = CPOcap;
315600091028           Mk01cit = CPOcit;
315700091028           Mk01sct = %editc( CPOsct : 'X' );
315800080208         else;
315900091028           Mk01cpo = S01cpo;
316000091028           Mk01rag = S01rag;
316100091028           Mk01Prv = S01prv;
316200110308           Mk01pst = V01pst;
316300091028           Mk01flt = S01flt;
316400091028           Mk01cap = S01cap;
316500091028           Mk01cit = S01loc;
316600091028           Mk01sct = %editc( H01sct : 'X' );
316700080208         endif;
316800080207
316900080207       ENDSR;
317000080206
317100080206       //--------------------------------------------------------------
317200080206       //?Operazioni finali.
317300080206       //--------------------------------------------------------------
317400080206       BEGSR RoutEnd;
317500090702
317600090702         //
317700090702         clear tntaa1ds;
317800090702         itaa1TLA='C' ;
317900090702         clear kpjbu   ;
318000090702         kpjbu=tntaa1ds ;
318100090703         TNTAA1C     (kpjba);
318200080206
318300091027         kpjbu = TRMK01ds;
318400080206         *inLR = *on;
318500080206         return;
318600080206
318700080206       ENDSR;
318800080206
318900080206      /end-free
319000080206
319100080206       //--------------------------------------------------------------
319200080206       //?Schiere a tempo di compilazione.
319300080206       //--------------------------------------------------------------
319400080206
319500080206** - $MSG -------------------------------------------------------------------*
319600110308Utente non abilitato alla Funzione.                                           01
319700110518Inserire o Cod.Potenziale o Cliente Unificante no tutti e 2 assieme.          02
319800110308Se parzializzazione per cap o località  immettere la provincia                03
319900110308Se parzializzazione per Info Commerciale immettere almeno 1 dato anagrafico.  04
320000110308Categoria merceologica errata.                                                05
320100110308Codice area errato                                                            06
320200110308Ordinamento errato                                                            07
320300110308Potenziale inesistente                                                        08
320400110308Non è stato trovato nessun potenziale                                         09
320500110308Raggiunta l'ampiezza massima del subfile                                      10
320600110308Opzione errata                                                                11
320700110601Se richiesto 'Trattative in corso' immettere almeno 1 dato anagrafico.        12
320800140910Immettere Anno Inizio Attività                                                13
320900140910Anno Inizio Attivita "DA" incongruente con Anno Inizio Attività "A"           14
321000110601Utente non abilitato al potenziale.                                           15
321100110308Filiale di appartenenza errata.                                               16
321200110309Importanza cliente errata.                                                    17
321300110309Ordinamento non valido se immessa ragione sociale.                            18
321400140910Immettere la % Fatturato Export                                               19
321500140910% Fatturato Export "DA" incongruente con % Fatturato Export "A"               20
321600110310Immettere Peso medio a Spedizione                                             21
321700110310Peso Medio Spedizione "DA" incongruente con Peso Medio Spedizione "A"         22
321800110310Concorrente errato                                                            23
321900110314Categoria Potenziale errata.                                                  24
322000131122Se non richiesti POTENZIALI interessati a LOGISTICA non immettere le date     25
322100131122Data errata. Inserire almeno un periodo                                       26
322200131122Data errata                                                                   27
322300131122Data "DAL" incongruente con data "AL"                                         28
322400131125Indicare se Logistica in Outsourcing                                          29
322500131125Immettere i Concorrenti SOLO se selezionata Logistica in outsourcing          30
322600131125Concorrente Logistica errato                                                  31
322700140910Immettere Valore                                                              32
322800140910Valore "DA" incongruente con Valore "A"                                       33
322900140910Paese estero errato                                                           34
